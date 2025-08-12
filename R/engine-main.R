#' Core Bayesian Assurance / Power Simulation
#'
#' Runs Monte Carlo simulations for Bayesian assurance / power based on
#' a \pkg{brms}-style model formula with an \pkg{INLA} backend.
#'
#' Supports metrics:
#' \itemize{
#' \item Posterior probability of correct direction
#' \item Posterior probability exceeding a threshold
#' \item Posterior probability inside a ROPE
#' \item Precision assurance (credible interval width <= target)
#' \item Bayes factor (Savage–Dickey method)
#' }
#'
#' @param formula Model formula.
#' @param family brms GLM family (e.g., \code{gaussian()}, \code{binomial()}).
#' @param family_control Optional list passed to INLA's \code{control.family}.
#' @param Ntrials,E,scale Optional vectors for binomial trials, Poisson exposure, family-specific scale.
#' @param priors Optional \code{brms::prior()} specification.
#' @param data_generator Optional function \code{function(n, effect)} returning a dataset.
#' @param effect_name Fixed effect name to assess.
#' @param effect_grid Vector of true effect values to simulate.
#' @param sample_sizes Vector of sample sizes.
#' @param nsims Simulations per (n, effect) cell.
#' @param power_threshold Decision probability threshold for summary.
#' @param precision_target Optional CI width target for precision assurance.
#' @param prob_threshold Threshold for decision metrics ("direction", "threshold", "rope").
#' @param effect_threshold Effect-size threshold for threshold-based metrics.
#' @param credible_level Credible interval level (default 0.95).
#' @param rope_bounds Optional length-2 numeric vector for ROPE.
#' @param error_sd Gaussian-family residual SD.
#' @param group_sd Random-effect SD.
#' @param obs_per_group Obs. per group.
#' @param predictor_means,predictor_sds Optional named lists for predictors.
#' @param seed Random seed.
#' @param inla_hyper Optional INLA hyperparameters.
#' @param compute_bayes_factor Logical; if TRUE, compute BF10 for \code{effect_name}.
#' @param progress "auto", "text", or "none".
#' @param family_args List of family-specific args passed to the generator.
#'
#' @return List with:
#' \describe{
#' \item{results}{All per-simulation replicate results.}
#' \item{summary}{Per-cell aggregated metrics.}
#' \item{settings}{List of settings and metadata.}
#' }
#' @export
brms_inla_power <- function(
    formula,
    family = gaussian(),
    family_control = NULL,
    Ntrials = NULL,
    E = NULL,
    scale = NULL,
    priors = NULL,
    data_generator = NULL,
    effect_name,
    effect_grid = 0.5,
    sample_sizes = c(50, 100, 200, 400),
    nsims = 200,
    power_threshold = 0.8,
    precision_target = NULL,
    prob_threshold = 0.95,
    effect_threshold = 0,
    credible_level = 0.95,
    rope_bounds = NULL,
    error_sd = 1,
    group_sd = 0.5,
    obs_per_group = 10,
    predictor_means = NULL,
    predictor_sds = NULL,
    seed = 123,
    inla_hyper = NULL,
    compute_bayes_factor = FALSE,
    progress = c("auto", "text", "none"),
    family_args = list()
) {
  stopifnot(is.character(effect_name), nchar(effect_name) > 0)
  set.seed(seed)
  progress <- match.arg(progress)

  fam_map   <- .to_inla_family(family)
  fam_inla  <- fam_map$inla
  needs_Ntrials <- fam_inla %in% c("binomial", "betabinomial")
  needs_E       <- fam_inla %in% c("poisson")

  # Set up data generator
  if (is.null(data_generator)) {
    data_generator <- .auto_data_generator(
      formula = formula,
      effect_name = effect_name,
      family = family,
      family_args = family_args,
      error_sd = error_sd,
      group_sd = group_sd,
      obs_per_group = obs_per_group,
      predictor_means = predictor_means,
      predictor_sds = predictor_sds
    )
  } else {
    stopifnot(is.function(data_generator))
  }

  tf_alt           <- .brms_to_inla_formula2(formula)
  inla_formula_alt <- tf_alt$inla_formula
  re_specs         <- tf_alt$re_specs
  prior_map        <- .map_brms_priors_to_inla(priors)

  prior_mean <- NA_real_
  prior_sd   <- NA_real_
  if (!is.null(prior_map$control_fixed$mean) &&
      !is.null(prior_map$control_fixed$prec) &&
      !is.null(prior_map$control_fixed$mean[[effect_name]]) &&
      !is.null(prior_map$control_fixed$prec[[effect_name]])) {
    prior_mean <- as.numeric(prior_map$control_fixed$mean[[effect_name]])
    prior_sd   <- sqrt(1 / as.numeric(prior_map$control_fixed$prec[[effect_name]]))
  }

  # --- simple built-in ASCII progress bar ---
  .simple_progress_bar <- function(step, total, width = 30) {
    done <- round(width * step / total)
    bar  <- paste0(rep("=", done), collapse = "")
    space <- paste0(rep(" ", width - done), collapse = "")
    pct <- round(100 * step / total)
    cat(sprintf("\r[%s%s] %3d%%", bar, space, pct))
    if (step == total) cat("\n")
    flush.console()
  }

  total_steps   <- length(sample_sizes) * length(effect_grid) * nsims
  show_progress <- progress %in% c("auto", "text") && interactive()
  step          <- 0L

  # Main loop
  res_list <- vector("list", length(sample_sizes) * length(effect_grid))
  idx <- 0L

  for (n in sample_sizes) {
    for (eff in effect_grid) {
      sim_rows <- vector("list", nsims)
      for (s in seq_len(nsims)) {
        dat <- data_generator(n, eff)
        # Add RE indices if needed
        if (length(re_specs) > 0L) {
          for (re in re_specs) {
            gid <- as.integer(as.factor(dat[[re$group]]))
            if (isTRUE(re$has_intercept) && is.null(dat[[re$id_intercept]]))
              dat[[re$id_intercept]] <- gid
            if (!is.null(re$slope) && is.null(dat[[re$id_slope]]))
              dat[[re$id_slope]] <- gid
          }
        }

        inla_args <- list(
          formula = inla_formula_alt,
          data = dat,
          family = fam_inla,
          control.fixed = prior_map$control_fixed %||% list(),
          control.predictor = list(link = 1),
          control.family = family_control %||% list(),
          verbose = FALSE
        )
        if (needs_Ntrials && !is.null(dat$.Ntrials)) inla_args$Ntrials <- dat$.Ntrials
        if (needs_E && !is.null(dat$.E)) inla_args$E <- dat$.E
        if (!is.null(dat$.scale)) inla_args$scale <- dat$.scale

        fit <- tryCatch(do.call(INLA::inla, inla_args), error = function(e) e)
        if (inherits(fit, "error") || is.null(fit$summary.fixed) ||
            !(effect_name %in% rownames(fit$summary.fixed))) {

          sim_rows[[s]] <- tibble::tibble(
            sim = s, n = n, true_effect = eff, ok = FALSE,
            post_prob_direction = NA_real_,
            post_prob_threshold = NA_real_,
            post_prob_rope      = NA_real_,
            ci_width = NA_real_, ci_lower = NA_real_, ci_upper = NA_real_,
            bf10 = NA_real_, log10_bf10 = NA_real_
          )

        } else {
          summ   <- fit$summary.fixed
          mean_b <- as.numeric(summ[effect_name, "mean"])
          sd_b   <- as.numeric(summ[effect_name, "sd"])

          if (all(c("0.025quant", "0.975quant") %in% colnames(summ))) {
            ci_lower <- as.numeric(summ[effect_name, "0.025quant"])
            ci_upper <- as.numeric(summ[effect_name, "0.975quant"])
          } else {
            ci_lower <- stats::qnorm((1 - credible_level) / 2, mean_b, sd_b)
            ci_upper <- stats::qnorm(1 - (1 - credible_level) / 2, mean_b, sd_b)
          }
          ci_width <- ci_upper - ci_lower

          dir_sign <- ifelse(eff >= 0, 1, -1)
          post_prob_direction <- if (dir_sign >= 0)
            1 - stats::pnorm(0, mean_b, sd_b) else stats::pnorm(0, mean_b, sd_b)
          thr <- effect_threshold
          post_prob_threshold <- if (dir_sign >= 0)
            1 - stats::pnorm(thr, mean_b, sd_b) else stats::pnorm(thr, mean_b, sd_b)
          post_prob_rope <- if (!is.null(rope_bounds) && length(rope_bounds) == 2L) {
            stats::pnorm(rope_bounds[2L], mean_b, sd_b) -
              stats::pnorm(rope_bounds[1L], mean_b, sd_b)
          } else NA_real_

          bf10 <- NA_real_
          log10_bf10 <- NA_real_
          if (isTRUE(compute_bayes_factor) && is.finite(prior_sd) && prior_sd > 0) {
            d_post0 <- stats::dnorm(0, mean_b, sd_b)
            d_pri0  <- stats::dnorm(0,
                                    mean = ifelse(is.finite(prior_mean), prior_mean, 0),
                                    sd   = prior_sd
            )
            if (is.finite(d_post0) && is.finite(d_pri0) && d_post0 > 0) {
              bf10       <- d_pri0 / d_post0
              log10_bf10 <- log10(bf10)
            }
          }

          sim_rows[[s]] <- tibble::tibble(
            sim = s, n = n, true_effect = eff, ok = TRUE,
            post_prob_direction = post_prob_direction,
            post_prob_threshold = post_prob_threshold,
            post_prob_rope      = post_prob_rope,
            ci_width = ci_width, ci_lower = ci_lower, ci_upper = ci_upper,
            bf10 = bf10, log10_bf10 = log10_bf10
          )
        }

        # Update progress
        step <- step + 1L
        if (show_progress) .simple_progress_bar(step, total_steps)
      }

      idx <- idx + 1L
      res_list[[idx]] <- dplyr::bind_rows(sim_rows)
    }
  }

  res <- dplyr::bind_rows(res_list)
  summ <- res %>%
    dplyr::filter(ok) %>%
    dplyr::group_by(n, true_effect) %>%
    dplyr::summarise(
      power_direction = mean(post_prob_direction >= prob_threshold, na.rm = TRUE),
      power_threshold = mean(post_prob_threshold >= prob_threshold, na.rm = TRUE),
      power_rope      = if (!is.null(rope_bounds))
        mean(post_prob_rope <= (1 - prob_threshold), na.rm = TRUE) else NA_real_,
      avg_ci_width    = mean(ci_width, na.rm = TRUE),
      ci_coverage     = if (!is.null(precision_target))
        mean(ci_width <= precision_target, na.rm = TRUE) else NA_real_,
      ciw_q05         = stats::quantile(ci_width, 0.05, na.rm = TRUE),
      ciw_q25         = stats::quantile(ci_width, 0.25, na.rm = TRUE),
      ciw_q50         = stats::quantile(ci_width, 0.50, na.rm = TRUE),
      ciw_q75         = stats::quantile(ci_width, 0.75, na.rm = TRUE),
      ciw_q95         = stats::quantile(ci_width, 0.95, na.rm = TRUE),
      avg_post_prob_direction = mean(post_prob_direction, na.rm = TRUE),
      avg_post_prob_threshold = mean(post_prob_threshold, na.rm = TRUE),
      avg_post_prob_rope      = mean(post_prob_rope, na.rm = TRUE),
      bf_hit_3       = mean(bf10 >= 3, na.rm = TRUE),
      bf_hit_10      = mean(bf10 >= 10, na.rm = TRUE),
      mean_log10_bf  = mean(log10_bf10, na.rm = TRUE),
      nsims_ok       = sum(ok, na.rm = TRUE),
      .groups = "drop"
    )

  list(
    results = res,
    summary = summ,
    settings = list(
      formula = formula,
      inla_formula = inla_formula_alt,
      inla_family = fam_inla,
      effect_name = effect_name,
      effect_grid = effect_grid,
      sample_sizes = sample_sizes,
      nsims = nsims,
      prob_threshold = prob_threshold,
      effect_threshold = effect_threshold,
      credible_level = credible_level,
      rope_bounds = rope_bounds,
      power_threshold = power_threshold,
      precision_target = precision_target,
      compute_bayes_factor = compute_bayes_factor,
      prior_for_effect = list(mean = prior_mean, sd = prior_sd)
    )
  )
}

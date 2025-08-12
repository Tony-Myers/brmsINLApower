#' Wilson CI-Based Stopping for a Binomial Proportion
#'
#' Computes the Wilson confidence interval for a binomial proportion and decides
#' whether to stop early for success or futility in a sequential simulation.
#'
#' @param hits Number of successes.
#' @param trials Number of trials.
#' @param target Target proportion for success.
#' @param margin Tolerance margin above/below target (default 0.02).
#' @param conf Confidence level for Wilson interval (default 0.95).
#'
#' @return List with elements:
#' \describe{
#'   \item{stop}{Logical; whether to stop early.}
#'   \item{low}{Lower bound of Wilson CI.}
#'   \item{high}{Upper bound of Wilson CI.}
#' }
#' @keywords internal
.should_stop_binom <- function(hits, trials, target, margin = 0.02, conf = 0.95) {
  if (trials <= 0) return(list(stop = FALSE, low = NA_real_, high = NA_real_))
  p_hat <- hits / trials
  z <- stats::qnorm(1 - (1 - conf) / 2)
  denom <- 1 + z^2 / trials
  centre <- p_hat + z^2 / (2 * trials)
  rad <- z * sqrt(p_hat * (1 - p_hat) / trials + z^2 / (4 * trials^2))
  low <- (centre - rad) / denom
  high <- (centre + rad) / denom
  stop <- (low > target + margin) || (high < target - margin)
  list(stop = stop, low = low, high = high)
}

#' Sequential Bayesian Assurance Simulation Engine
#'
#' Simulates assurance for \code{metric} in batches, stopping each cell early if the
#' Wilson binomial CI for the current pass rate is wholly above/below \code{target +/- margin}.
#'
#' @param formula Model formula for \pkg{brms}.
#' @param family GLM family (e.g. \code{gaussian()}, \code{binomial()}).
#' @param family_control Optional list for INLA's \code{control.family}.
#' @param Ntrials,E,scale Optional values for specific families (can be generated per-row by data generator).
#' @param priors Optional \pkg{brms} prior specification.
#' @param data_generator Optional function(n, effect) to generate data; defaults to auto-generator.
#' @param effect_name Fixed effect to assess.
#' @param effect_grid Vector of true effect values.
#' @param sample_sizes Vector of sample sizes.
#' @param metric One of "direction", "threshold", "rope", "bf".
#' @param target Target assurance value for stopping rule.
#' @param prob_threshold Posterior probability threshold for decision rules.
#' @param effect_threshold Effect size threshold for "threshold" rules.
#' @param rope_bounds Optional length-2 numeric vector for ROPE.
#' @param credible_level Credible interval coverage (default 0.95).
#' @param compute_bayes_factor Logical; if TRUE and metric=="bf", compute BFs.
#' @param error_sd Residual SD (Gaussian).
#' @param group_sd Random-effect SD.
#' @param obs_per_group Obs per grouping level.
#' @param predictor_means,predictor_sds Optional named lists for predictors.
#' @param seed Random seed.
#' @param batch_size Sims per sequential look.
#' @param min_sims Minimum sims before early stop.
#' @param max_sims Maximum sims per cell.
#' @param ci_conf Confidence level for Wilson CI in stop rule.
#' @param margin Margin for target (default 0.02).
#' @param num_threads INLA threads.
#' @param family_args Named list of family-specific params for data generator.
#' @param progress Logical; show progress.
#'
#' @return List with:
#' \describe{
#'   \item{results}{NULL (no per-replicate storage).}
#'   \item{summary}{Aggregated metrics in standard format.}
#'   \item{settings}{Simulation settings.}
#' }
#' @export
brms_inla_power_sequential <- function(
    formula,
    family = gaussian(),
    family_control = NULL,
    Ntrials = NULL,
    E = NULL,
    scale = NULL,
    priors = NULL,
    data_generator = NULL,
    effect_name,
    effect_grid,
    sample_sizes,
    metric = c("direction","threshold","rope","bf"),
    target = 0.8,
    prob_threshold = 0.95,
    effect_threshold = 0,
    rope_bounds = NULL,
    credible_level = 0.95,
    compute_bayes_factor = FALSE,
    error_sd = 1,
    group_sd = 0.5,
    obs_per_group = 10,
    predictor_means = NULL,
    predictor_sds = NULL,
    seed = 1,
    batch_size = 20,
    min_sims = 40,
    max_sims = 600,
    ci_conf = 0.95,
    margin = 0.02,
    num_threads = "1:1",
    family_args = list(),
    progress = TRUE
) {
  set.seed(seed)
  metric <- match.arg(metric)

  if (is.null(data_generator)) {
    data_generator <- .auto_data_generator(
      formula = formula, effect_name = effect_name,
      family = family, family_args = family_args,
      error_sd = error_sd, group_sd = group_sd, obs_per_group = obs_per_group,
      predictor_means = predictor_means, predictor_sds = predictor_sds
    )
  } else stopifnot(is.function(data_generator))

  tf         <- .brms_to_inla_formula2(formula)
  f_inla     <- tf$inla_formula
  re_specs   <- tf$re_specs
  fam_inla   <- .to_inla_family(family)$inla
  needs_N    <- fam_inla %in% c("binomial","betabinomial")
  needs_E    <- fam_inla %in% c("poisson")
  prior_map  <- .map_brms_priors_to_inla(priors)
  prior_mean <- NA_real_
  prior_sd   <- NA_real_
  if (isTRUE(compute_bayes_factor) && metric == "bf" &&
      !is.null(prior_map$control_fixed$mean) &&
      !is.null(prior_map$control_fixed$prec) &&
      !is.null(prior_map$control_fixed$mean[[effect_name]]) &&
      !is.null(prior_map$control_fixed$prec[[effect_name]])) {
    prior_mean <- as.numeric(prior_map$control_fixed$mean[[effect_name]])
    prior_sd   <- sqrt(1 / as.numeric(prior_map$control_fixed$prec[[effect_name]]))
  }

  total_cells <- length(sample_sizes) * length(effect_grid)
  if (progress) message("Sequential assurance over ", total_cells, " cells (\u2026)")

  out <- vector("list", total_cells)
  idx <- 0L

  for (n in sample_sizes) {
    for (eff in effect_grid) {
      hits <- 0L; trials <- 0L; stopped <- FALSE
      dir_sign <- ifelse(eff >= 0, 1, -1)

      while (!stopped && trials < max_sims) {
        b <- min(batch_size, max_sims - trials)
        for (s in seq_len(b)) {
          dat <- data_generator(n, eff)
          # Ensure RE index columns
          if (length(re_specs) > 0L) {
            for (re in re_specs) {
              gid <- as.integer(as.factor(dat[[re$group]]))
              if (isTRUE(re$has_intercept) && is.null(dat[[re$id_intercept]])) dat[[re$id_intercept]] <- gid
              if (!is.null(re$slope) && is.null(dat[[re$id_slope]])) dat[[re$id_slope]] <- gid
            }
          }
          Ntrials_vec <- if (!is.null(dat$.Ntrials)) dat$.Ntrials else NULL
          E_vec       <- if (!is.null(dat$.E)) dat$.E else NULL
          scale_vec   <- if (!is.null(dat$.scale)) dat$.scale else NULL

          if (length(Ntrials_vec) == 1L) Ntrials_vec <- rep(Ntrials_vec, n)
          if (length(E_vec) == 1L)       E_vec <- rep(E_vec, n)
          if (length(scale_vec) == 1L)   scale_vec <- rep(scale_vec, n)

          fit <- tryCatch({
            inla_args <- list(
              formula = f_inla,
              data = dat,
              family = fam_inla,
              control.fixed = prior_map$control_fixed %||% list(),
              control.family = family_control %||% list(),
              control.predictor = list(link = 1),
              verbose = FALSE,
              num.threads = num_threads
            )
            if (needs_N && !is.null(Ntrials_vec)) inla_args$Ntrials <- Ntrials_vec
            if (needs_E && !is.null(E_vec))       inla_args$E <- E_vec
            if (!is.null(scale_vec))              inla_args$scale <- scale_vec
            do.call(INLA::inla, inla_args)
          }, error = function(e) e)

          if (inherits(fit, "error")) next
          summ <- fit$summary.fixed
          if (is.null(summ) || !(effect_name %in% rownames(summ))) next
          mean_b <- as.numeric(summ[effect_name, "mean"])
          sd_b   <- as.numeric(summ[effect_name, "sd"])

          success <- switch(metric,
                            "direction" = {
                              if (dir_sign >= 0) 1 - stats::pnorm(0, mean_b, sd_b) >= prob_threshold
                              else stats::pnorm(0, mean_b, sd_b) >= prob_threshold
                            },
                            "threshold" = {
                              thr <- effect_threshold
                              if (dir_sign >= 0) 1 - stats::pnorm(thr, mean_b, sd_b) >= prob_threshold
                              else stats::pnorm(thr, mean_b, sd_b) >= prob_threshold
                            },
                            "rope" = {
                              if (is.null(rope_bounds) || length(rope_bounds) != 2L) FALSE else {
                                p_in <- stats::pnorm(rope_bounds[2L], mean_b, sd_b) -
                                  stats::pnorm(rope_bounds[1L], mean_b, sd_b)
                                (1 - p_in) >= prob_threshold
                              }
                            },
                            "bf" = {
                              if (!is.finite(prior_sd) || prior_sd <= 0) FALSE else {
                                d_post0 <- stats::dnorm(0, mean_b, sd_b)
                                d_pri0  <- stats::dnorm(0, mean = ifelse(is.finite(prior_mean), prior_mean, 0), sd = prior_sd)
                                bf10 <- d_pri0 / d_post0
                                is.finite(bf10) && (bf10 >= prob_threshold)
                              }
                            }
          )
          hits <- hits + as.integer(success)
          trials <- trials + 1L
        }

        if (trials >= min_sims) {
          dec <- .should_stop_binom(hits, trials, target = target, margin = margin, conf = ci_conf)
          if (dec$stop) stopped <- TRUE
        }
      }

      idx <- idx + 1L
      out[[idx]] <- tibble::tibble(
        n = n, true_effect = eff,
        assurance = if (trials > 0) hits / trials else NA_real_,
        sims_used = trials
      )
    }
  }

  summ <- dplyr::bind_rows(out)
  if (metric == "direction") summ$power_direction <- summ$assurance
  if (metric == "threshold") summ$power_threshold <- summ$assurance
  if (metric == "rope")      summ$power_rope <- summ$assurance
  if (metric == "bf")        summ$bf_hit_10 <- summ$assurance

  # Populate missing summary columns for consistency
  add_if_missing <- function(df, nm, val) { if (!nm %in% names(df)) df[[nm]] <- val; df }
  for (nm in c("power_direction","power_threshold","power_rope","avg_ci_width","ci_coverage",
               "ciw_q05","ciw_q25","ciw_q50","ciw_q75","ciw_q95",
               "avg_post_prob_direction","avg_post_prob_threshold","avg_post_prob_rope",
               "bf_hit_3","bf_hit_10","mean_log10_bf","nsims_ok")) {
    summ <- add_if_missing(summ, nm, NA_real_)
  }
  summ$nsims_ok <- summ$sims_used

  list(
    results = NULL,
    summary = summ %>%
      dplyr::select(
        n, true_effect,
        power_direction, power_threshold, power_rope,
        avg_ci_width, ci_coverage,
        ciw_q05, ciw_q25, ciw_q50, ciw_q75, ciw_q95,
        avg_post_prob_direction, avg_post_prob_threshold, avg_post_prob_rope,
        bf_hit_3, bf_hit_10, mean_log10_bf, nsims_ok
      ),
    settings = list(
      formula = formula,
      inla_family = fam_inla,
      effect_name = effect_name,
      effect_grid = effect_grid,
      sample_sizes = sample_sizes,
      metric = metric,
      target = target,
      prob_threshold = prob_threshold,
      effect_threshold = effect_threshold,
      rope_bounds = rope_bounds,
      credible_level = credible_level,
      compute_bayes_factor = compute_bayes_factor
    )
  )
}

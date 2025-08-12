#' Two-Stage Bayesian Assurance Simulation
#'
#' Runs a two-stage assurance / power simulation using \code{\link{brms_inla_power}}.
#' Stage 1 does a coarse grid search over \code{effect_range} and \code{n_range}.
#' Stage 2 refines the search near the target assurance band.
#'
#' @param formula Model formula.
#' @param effect_name Name of fixed effect to assess.
#' @param effect_range Numeric length-2 vector for effect size range.
#' @param n_range Numeric length-2 vector for sample size range.
#' @param stage1_k_effects Effects in Stage 1 grid (default 7).
#' @param stage1_k_n Sample sizes in Stage 1 grid (default 8).
#' @param stage1_nsims Sims per cell in Stage 1 (default 100).
#' @param stage2_nsims Sims per cell in Stage 2 (default 400).
#' @param refine_metric Metric to refine on: "direction", "threshold", or "rope".
#' @param refine_target Target assurance value to refine around (default 0.8).
#' @param prob_threshold Posterior prob. threshold for decisions.
#' @param effect_threshold Effect size threshold for "threshold" rules.
#' @param obs_per_group Observations per group for generator.
#' @param error_sd Residual SD for Gaussian-like families.
#' @param group_sd SD for random effects in generator.
#' @param band Bandwidth around target for candidate cells (default 0.06).
#' @param expand Expand radius in grid steps (default 1).
#' @param ... Additional arguments passed to \code{\link{brms_inla_power}}.
#'
#' @return List with elements:
#' \describe{
#'   \item{results}{Combined Stage 1 and Stage 2 simulation results.}
#'   \item{summary}{Aggregated metrics per cell.}
#'   \item{stage1}{Stage 1 grid info.}
#'   \item{stage2}{Stage 2 grid info.}
#'   \item{params}{Refinement parameters.}
#' }
#' @export
brms_inla_power_two_stage <- function(formula,
                                      effect_name,
                                      effect_range,
                                      n_range,
                                      stage1_k_effects = 7,
                                      stage1_k_n = 8,
                                      stage1_nsims = 100,
                                      stage2_nsims = 400,
                                      refine_metric = c("direction","threshold","rope"),
                                      refine_target = 0.80,
                                      prob_threshold = 0.95,
                                      effect_threshold = 0.0,
                                      obs_per_group = NULL,
                                      error_sd = NULL,
                                      group_sd = 0.5,
                                      band = 0.06,
                                      expand = 1L,
                                      ...) {
  refine_metric <- match.arg(refine_metric)

  stopifnot(length(effect_range) == 2L, length(n_range) == 2L)

  effect_range <- sort(effect_range)
  n_range      <- sort(n_range)

  eff1 <- seq(effect_range[1], effect_range[2], length.out = stage1_k_effects)
  n1   <- unique(round(seq(n_range[1], n_range[2], length.out = stage1_k_n)))

  stage1 <- brms_inla_power(
    formula          = formula,
    effect_name      = effect_name,
    effect_grid      = eff1,
    sample_sizes     = n1,
    prob_threshold   = prob_threshold,
    effect_threshold = effect_threshold,
    obs_per_group    = obs_per_group,
    error_sd         = error_sd,
    group_sd         = group_sd,
    nsims            = stage1_nsims,
    ...
  )

  ass1 <- .compute_assurance(stage1$results, refine_metric, prob_threshold)

  lower <- refine_target - band
  upper <- refine_target + band
  cand  <- dplyr::filter(ass1, assurance >= lower, assurance <= upper)
  if (nrow(cand) == 0L) {
    cand <- ass1 %>%
      dplyr::arrange(abs(assurance - refine_target)) %>%
      dplyr::slice(1:min(8L, nrow(.)))
  }

  idx_n   <- match(cand$n, n1)
  idx_eff <- match(cand$true_effect, eff1)
  expand_idx <- function(idx, maxlen) {
    as.integer(sort(unique(
      pmin(pmax(rep(idx, each = 2L * expand + 1L) + (-expand:expand), 1L), maxlen)
    )))
  }
  n2   <- sort(unique(n1[unique(unlist(lapply(idx_n,   expand_idx, maxlen = length(n1))))]))
  eff2 <- sort(unique(eff1[unique(unlist(lapply(idx_eff, expand_idx, maxlen = length(eff1))))]))

  if (length(n2) == 0L) n2 <- n1
  if (length(eff2) == 0L) eff2 <- eff1

  stage2 <- brms_inla_power(
    formula          = formula,
    effect_name      = effect_name,
    effect_grid      = eff2,
    sample_sizes     = n2,
    prob_threshold   = prob_threshold,
    effect_threshold = effect_threshold,
    obs_per_group    = obs_per_group,
    error_sd         = error_sd,
    group_sd         = group_sd,
    nsims            = stage2_nsims,
    ...
  )

  results_combined <- dplyr::bind_rows(stage1$results, stage2$results)

  rope_bounds      <- list(...)$rope_bounds
  precision_target <- list(...)$precision_target
  compute_bf       <- isTRUE(list(...)$compute_bayes_factor)

  summ <- results_combined %>%
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
      bf_hit_3  = if (compute_bf) mean(bf10 >= 3, na.rm = TRUE)  else NA_real_,
      bf_hit_10 = if (compute_bf) mean(bf10 >= 10, na.rm = TRUE) else NA_real_,
      mean_log10_bf = if (compute_bf) mean(log10_bf10, na.rm = TRUE) else NA_real_,
      nsims_ok = sum(ok, na.rm = TRUE),
      .groups  = "drop"
    )

  out <- list(
    results = results_combined,
    summary = summ,
    stage1  = list(grid_effects = eff1, grid_n = n1, nsims = stage1_nsims),
    stage2  = list(grid_effects = eff2, grid_n = n2, nsims = stage2_nsims),
    params  = list(
      refine_metric    = refine_metric,
      refine_target    = refine_target,
      prob_threshold   = prob_threshold,
      effect_threshold = effect_threshold
    )
  )
  class(out) <- "brms_inla_power"
  out
}

#' Fast Two-Stage Assurance Simulation
#'
#' A convenience wrapper for \code{\link{brms_inla_power_two_stage}} with sensible defaults.
#'
#' @inheritParams brms_inla_power_two_stage
#' @return See \code{\link{brms_inla_power_two_stage}}.
#' @export
brms_inla_power_fast <- function(formula,
                                 effect_name,
                                 effect_range,
                                 n_range,
                                 prob_threshold   = 0.95,
                                 refine_metric    = c("direction","threshold","rope"),
                                 refine_target    = 0.80,
                                 effect_threshold = 0.0,
                                 obs_per_group    = NULL,
                                 error_sd         = NULL,
                                 stage1_k_effects = 7,
                                 stage1_k_n       = 8,
                                 stage1_nsims     = 100,
                                 stage2_nsims     = 400,
                                 band             = 0.06,
                                 expand           = 1L,
                                 ...) {
  refine_metric <- match.arg(refine_metric)
  brms_inla_power_two_stage(
    formula, effect_name, effect_range, n_range,
    stage1_k_effects = stage1_k_effects,
    stage1_k_n       = stage1_k_n,
    stage1_nsims     = stage1_nsims,
    stage2_nsims     = stage2_nsims,
    refine_metric    = refine_metric,
    refine_target    = refine_target,
    prob_threshold   = prob_threshold,
    effect_threshold = effect_threshold,
    obs_per_group    = obs_per_group,
    error_sd         = error_sd,
    band             = band,
    expand           = expand,
    ...
  )
}

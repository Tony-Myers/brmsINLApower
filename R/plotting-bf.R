#' Plot Bayes Factor Assurance Curve
#'
#' Plots the proportion of simulations in which BF10 meets or exceeds
#' a given threshold, against sample size.
#'
#' @param power_results Simulation results from a `brms_inla_power*` function
#'   with `compute_bayes_factor = TRUE`.
#' @param bf_threshold Numeric; BF10 threshold to count as a "success" (default 3).
#' @param effect_weights Optional named numeric vector of weights for each `true_effect`.
#' @param title,subtitle Optional ggplot title/subtitle.
#'
#' @return A ggplot object.
#' @export
plot_bf_assurance_curve <- function(power_results,
                                    bf_threshold = 3,
                                    effect_weights = NULL,
                                    title = NULL,
                                    subtitle = NULL) {
  stopifnot(isTRUE(power_results$settings$compute_bayes_factor))
  df <- dplyr::filter(power_results$results, ok)
  tmp <- df %>%
    dplyr::group_by(n, true_effect) %>%
    dplyr::summarise(
      assurance = mean(bf10 >= bf_threshold, na.rm = TRUE),
      .groups = "drop"
    )
  agg <- .aggregate_over_effects(tmp, effect_weights, value_col = "assurance")
  ggplot2::ggplot(agg, ggplot2::aes(x = n, y = assurance)) +
    .geom_line_lw(width = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 1),
                                labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = "Sample size",
      y = paste0("Assurance: P{ BF10 \u2264 ", bf_threshold, " }"),
      title = title %||% "Bayes-factor Assurance Curve",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()
}

#' Plot Expected Evidence (mean log10 BF10)
#'
#' Plots the average log10 BF10 against true effect size,
#' with separate curves for each sample size.
#'
#' @inheritParams plot_bf_assurance_curve
#' @return A ggplot object.
#' @export
plot_bf_expected_evidence <- function(power_results,
                                      title = NULL,
                                      subtitle = NULL) {
  stopifnot(isTRUE(power_results$settings$compute_bayes_factor))
  s <- power_results$summary
  ggplot2::ggplot(s, ggplot2::aes(x = true_effect,
                                  y = mean_log10_bf,
                                  group = n,
                                  colour = factor(n))) +
    .geom_line_lw(width = 1) +
    ggplot2::labs(
      x = "True effect size",
      y = "E[log10 BF10]",
      colour = "n",
      title = title %||% "Expected Evidence Accumulation",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()
}

#' Plot Bayes Factor Heatmap (mean log10 BF10)
#'
#' Filled contour plot of mean log10 BF10 as a function of
#' sample size and true effect size.
#'
#' @inheritParams plot_bf_assurance_curve
#' @return A ggplot object.
#' @export
plot_bf_heatmap <- function(power_results,
                            title = NULL,
                            subtitle = NULL) {
  stopifnot(isTRUE(power_results$settings$compute_bayes_factor))
  s <- power_results$summary
  ggplot2::ggplot(s, ggplot2::aes(x = true_effect, y = n, z = mean_log10_bf)) +
    ggplot2::geom_contour_filled(bins = 12, alpha = 0.9) +
    .add_contour_lines(colour = "white", alpha = 0.3, width = 0.2) +
    .scale_fill_viridis_discrete(name = "E[log10 BF10]") +
    ggplot2::labs(
      x = "True effect size",
      y = "Sample size",
      title = title %||% "Expected Evidence Heatmap",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()
}

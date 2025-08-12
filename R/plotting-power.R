#' Plot Bayesian Power / Assurance Contour
#'
#' Draw a filled contour plot of assurance for a chosen metric,
#' over true effect size and sample size.
#'
#' @param power_results Output from a `brms_inla_power` function.
#' @param power_metric Which metric to plot: `"direction"`, `"threshold"`, or `"rope"`.
#' @param power_threshold Optional contour line for assurance (default 0.8).
#' @param show_threshold_line Logical; add a red contour at \code{power_threshold}.
#' @param title,subtitle Optional plot labels.
#' @export
plot_power_contour <- function(power_results,
                               power_metric = c("direction", "threshold", "rope"),
                               power_threshold = 0.8,
                               show_threshold_line = TRUE,
                               title = NULL,
                               subtitle = NULL) {
  stopifnot("summary" %in% names(power_results))
  power_metric <- match.arg(power_metric)
  power_col <- switch(power_metric,
                      direction = "power_direction",
                      threshold = "power_threshold",
                      rope      = "power_rope")
  summ_data <- power_results$summary
  if (!(power_col %in% names(summ_data)) || all(is.na(summ_data[[power_col]]))) {
    stop("Selected power metric not available in results.")
  }
  effect_name <- power_results$settings$effect_name %||% "Effect"

  p <- ggplot2::ggplot(summ_data,
                       ggplot2::aes(x = true_effect,
                                    y = n,
                                    z = !!rlang::sym(power_col))) +
    ggplot2::geom_contour_filled(bins = 12, alpha = 0.9) +
    .add_contour_lines(colour = "white", alpha = 0.3, width = 0.2) +
    .scale_fill_viridis_discrete(name = "Assurance") +
    ggplot2::labs(
      x = paste("True effect size (", effect_name, ")", sep = ""),
      y = "Sample size",
      title = `%||%`(title, paste("Assurance contour for", power_metric)),
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()

  if (show_threshold_line && !is.null(power_threshold)) {
    p <- p + .add_contour_lines(
      mapping = ggplot2::aes(z = !!rlang::sym(power_col)),
      breaks = power_threshold,
      colour = "red", width = 0.8
    )
  }
  p
}

#' Plot Bayesian Power / Assurance Heatmap
#'
#' Heatmap of assurance for a chosen metric across
#' true effect sizes and sample sizes.
#'
#' @inheritParams plot_power_contour
#' @export
plot_power_heatmap <- function(power_results,
                               power_metric = c("direction", "threshold", "rope"),
                               title = NULL,
                               subtitle = NULL) {
  stopifnot("summary" %in% names(power_results))
  power_metric <- match.arg(power_metric)
  power_col <- switch(power_metric,
                      direction = "power_direction",
                      threshold = "power_threshold",
                      rope      = "power_rope")
  summ_data <- power_results$summary
  if (!(power_col %in% names(summ_data)) || all(is.na(summ_data[[power_col]]))) {
    stop("Selected power metric not available in results.")
  }
  effect_name <- power_results$settings$effect_name %||% "Effect"

  ggplot2::ggplot(summ_data,
                  ggplot2::aes(x = true_effect,
                               y = n,
                               fill = !!rlang::sym(power_col))) +
    ggplot2::geom_tile() +
    .scale_fill_viridis_continuous(name = "Assurance",
                                   limits = c(0, 1),
                                   breaks = seq(0, 1, 0.2),
                                   labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = paste("True effect size (", effect_name, ")", sep = ""),
      y = "Sample size",
      title = `%||%`(title, paste("Assurance heatmap for", power_metric)),
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()
}

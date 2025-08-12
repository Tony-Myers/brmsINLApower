#' Aggregate Assurance Over Effects with Optional Weights
#'
#' Aggregates assurance or other metrics over differing true effect values,
#' optionally using user-supplied weights per effect.
#' @importFrom stats weighted.mean quantile
#' @param df Data frame containing columns at least \code{n}, \code{true_effect}, and a metric column.
#' @param weights Named numeric vector of weights keyed by \code{true_effect}. If \code{NULL}, unweighted mean is computed.
#' @param value_col Character, name of the metric column to aggregate (default "assurance").
#'
#' @return A tibble grouped by \code{n} with weighted or unweighted mean of the metric.
#' @keywords internal
.aggregate_over_effects <- function(df, weights = NULL, value_col = "assurance") {
  if (is.null(weights)) {
    df %>%
      dplyr::group_by(n) %>%
      dplyr::summarise(!!rlang::sym(value_col) := mean(.data[[value_col]], na.rm = TRUE), .groups = "drop")
  } else {
    w <- tibble::tibble(true_effect = as.numeric(names(weights)), w = as.numeric(weights))
    df %>%
      dplyr::inner_join(w, by = "true_effect") %>%
      dplyr::group_by(n) %>%
      dplyr::summarise(!!rlang::sym(value_col) := stats::weighted.mean(.data[[value_col]], w, na.rm = TRUE),
                       .groups = "drop")
  }
}

#' Plot Precision Assurance Curve
#'
#' Plots assurance curve showing the probability that credible interval width is
#' below a specified precision target as a function of sample size.
#'
#' @param power_results Simulation output containing \code{results} and \code{settings}.
#' @param precision_target Numeric; target credible interval width to consider success.
#'   If missing or NULL, taken from \code{power_results$settings$precision_target}.
#' @param effect_weights Named vector of weights for multiple true effect values (optional).
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#'
#' @return A ggplot2 object displaying the precision assurance curve.
#' @export
plot_precision_assurance_curve <- function(power_results,
                                           precision_target = NULL,
                                           effect_weights = NULL,
                                           title = NULL,
                                           subtitle = NULL) {
  stopifnot("results" %in% names(power_results))
  if (is.null(precision_target)) {
    precision_target <- power_results$settings$precision_target
  }
  if (is.null(precision_target)) stop("Please supply a precision_target or ensure it is set in settings.")

  df <- power_results$results %>%
    dplyr::filter(ok) %>%
    dplyr::group_by(n, true_effect) %>%
    dplyr::summarise(assurance = mean(ci_width <= precision_target, na.rm = TRUE), .groups = "drop")

  agg <- .aggregate_over_effects(df, effect_weights, value_col = "assurance")

  ggplot2::ggplot(agg, ggplot2::aes(x = n, y = assurance)) +
    .geom_line_lw(width = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(limits = c(0, 1), labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(
      x = "Sample size",
      y = paste0("Assurance: P(CI width \u2264 ", precision_target, ")"),
      title = title %||% "Precision Assurance Curve",
      subtitle = subtitle
    ) +
    ggplot2::theme_minimal()
}

#' Plot Precision Fan Chart
#'
#' Plots credible interval width quantiles across sample sizes to display the
#' distribution of precision in simulations as fan charts with 90% and 50% bands.
#' Optionally supports weighted quantiles if the \pkg{Hmisc} package is available.
#'
#' @param power_results Simulation output containing \code{results} and \code{settings}.
#' @param effect_weights Named vector of weights for multiple true effect values (optional).
#' @param title Optional plot title.
#' @param subtitle Optional plot subtitle.
#'
#' @return A ggplot2 object showing fan charts of credible interval widths.
#' @export
plot_precision_fanchart <- function(power_results,
                                    effect_weights = NULL,
                                    title = NULL,
                                    subtitle = NULL) {
  stopifnot("results" %in% names(power_results))

  # Unweighted quantiles
  df_unw <- power_results$results %>%
    dplyr::filter(ok) %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(
      q05 = stats::quantile(ci_width, 0.05, na.rm = TRUE),
      q25 = stats::quantile(ci_width, 0.25, na.rm = TRUE),
      q50 = stats::quantile(ci_width, 0.50, na.rm = TRUE),
      q75 = stats::quantile(ci_width, 0.75, na.rm = TRUE),
      q95 = stats::quantile(ci_width, 0.95, na.rm = TRUE),
      .groups = "drop"
    )

  # Weighted quantiles if weights provided and Hmisc installed
  if (!is.null(effect_weights) && requireNamespace("Hmisc", quietly = TRUE)) {
    dfw <- power_results$results %>%
      dplyr::filter(ok) %>%
      dplyr::select(n, true_effect, ci_width)
    w <- tibble::tibble(true_effect = as.numeric(names(effect_weights)), w = as.numeric(effect_weights))
    dfw <- dplyr::inner_join(dfw, w, by = "true_effect")
    wq <- function(x, w, p) as.numeric(Hmisc::wtd.quantile(x, weights = w, probs = p, na.rm = TRUE, normwt = TRUE))
    df <- dfw %>%
      dplyr::group_by(n) %>%
      dplyr::summarise(
        q05 = wq(ci_width, w, 0.05),
        q25 = wq(ci_width, w, 0.25),
        q50 = wq(ci_width, w, 0.50),
        q75 = wq(ci_width, w, 0.75),
        q95 = wq(ci_width, w, 0.95),
        .groups = "drop"
      )
  } else {
    if (!is.null(effect_weights)) {
      warning("Package 'Hmisc' not installed; falling back to unweighted fan chart.")
    }
    df <- df_unw
  }

  ggplot2::ggplot(df, ggplot2::aes(x = n)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q05, ymax = q95), alpha = 0.15) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = q25, ymax = q75), alpha = 0.30) +
    .geom_line_lw(mapping = ggplot2::aes(y = q50), width = 1) +
    ggplot2::labs(
      x = "Sample size",
      y = "Credible Interval Width",
      title = title %||% "Precision Fan Chart",
      subtitle = subtitle %||% "Bands represent 90% (light) and 50% (dark) quantiles"
    ) +
    ggplot2::theme_minimal()
}

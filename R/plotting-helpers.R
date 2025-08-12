
# Internal helper functions for plotting assurance, Bayes factors, precision, and decision metrics.
# These functions are intended for internal package use.

# Define `%||%` operator here just for internal use without documentation or export
`%||%` <- rlang::`%||%`

#' Compute Mean Assurance for a Given Metric
#'
#' Given simulation results, compute the mean assurance for a specified decision rule metric.
#'
#' @param df Data frame containing simulation results with columns `n`, `true_effect`, `ok`, and metric columns.
#' @param metric Character string; one of `"direction"`, `"threshold"`, or `"rope"`.
#' @param prob_threshold Numeric; probability threshold for `"threshold"` and `"rope"` metrics.
#' @param rope_rule Character vector; reserved for future use, default is `"≥"`.
#' @param direction_p Numeric; probability cutoff for `"direction"` metric, default 0.5.
#' @return A tibble grouped by `n` and `true_effect` with column `assurance`, the mean of passing runs.
#' @keywords internal
.compute_assurance <- function(df, metric, prob_threshold, rope_rule = c("\u2265"), direction_p = 0.5) {
  metric <- match.arg(metric, c("direction", "threshold", "rope"))

  colname <- switch(metric,
                    direction = "post_prob_direction",
                    threshold = "post_prob_threshold",
                    rope = "post_prob_rope")

  if (!all(c("n", "true_effect", colname, "ok") %in% names(df))) {
    stop("Expected columns n, true_effect, ok and ", colname, " in results.")
  }

  df <- df[df$ok, , drop = FALSE]

  if (metric == "direction") {
    df$pass <- df[[colname]] >= direction_p
  } else {
    df$pass <- df[[colname]] >= prob_threshold
  }

  dplyr::summarise(dplyr::group_by(df, n, true_effect),
                   assurance = mean(pass, na.rm = TRUE),
                   .groups = "drop")
}

#' Determine ggplot2 Line Width Argument Name by Version
#'
#' Returns the correct argument name for line width in ggplot2,
#' depending on package version ("linewidth" for ≥ 3.4.0, else "size").
#'
#' @return Character string of argument name.
#' @keywords internal
.gg_line_arg <- function() {
  if (utils::packageVersion("ggplot2") >= "3.4.0") "linewidth" else "size"
}

#' Add Contour Lines to a ggplot2 Plot
#'
#' Wrapper around `geom_contour` with preset defaults for color, alpha, width.
#' Uses the correct linewidth/size argument depending on ggplot2 version.
#'
#' @param mapping Mapping aesthetic.
#' @param data Data frame.
#' @param breaks Break points for contours.
#' @param colour Color of contour lines.
#' @param alpha Transparency level.
#' @param width Line width.
#' @param bins Number of bins for contour fill.
#' @return A ggplot2 layer adding contour lines.
#' @keywords internal
.add_contour_lines <- function(mapping = NULL, data = NULL,
                               breaks = NULL, colour = "white",
                               alpha = 0.3, width = 0.2, bins = NULL) {
  arg <- .gg_line_arg()
  args <- list(mapping = mapping,
               data = data,
               breaks = breaks,
               colour = colour,
               alpha = alpha,
               bins = bins)
  args[[arg]] <- width
  do.call(ggplot2::geom_contour, args)
}

#' Create a ggplot2 Point Layer with Version-Compatible Width
#'
#' Creates a `geom_point` with a width argument adapted to ggplot2 version.
#'
#' @param mapping Mapping aesthetic.
#' @param data Data frame.
#' @param ... Additional parameters passed to `geom_point`.
#' @param width Numeric line width for points, default 1.5.
#' @return ggplot2 layer for points.
#' @keywords internal
.geom_point_lw <- function(mapping = NULL, data = NULL, ..., width = 1.5) {
  arg <- .gg_line_arg()
  args <- c(list(mapping = mapping, data = data, ...), setNames(list(width), arg))
  do.call(ggplot2::geom_point, args)
}

#' Create a ggplot2 Line Layer with Version-Compatible Width
#'
#' Creates a `geom_line` with a width argument adapted to ggplot2 version.
#'
#' @param mapping Mapping aesthetic.
#' @param data Data frame.
#' @param ... Additional parameters passed to `geom_line`.
#' @param width Numeric line width for lines, default 1.
#' @return ggplot2 layer for lines.
#' @keywords internal
.geom_line_lw <- function(mapping = NULL, data = NULL, ..., width = 1) {
  arg <- .gg_line_arg()
  args <- c(list(mapping = mapping, data = data, ...), setNames(list(width), arg))
  do.call(ggplot2::geom_line, args)
}

#' Scale Fill for Viridis Discrete Data
#'
#' Provides a fill scale for discrete data using viridis colors.
#' Falls back to `scale_fill_stepsn` if `scale_fill_viridis_d` is not available.
#'
#' @param name Character; legend title, default "Assurance".
#' @return ggplot2 fill scale object.
#' @keywords internal
.scale_fill_viridis_discrete <- function(name = "Assurance") {
  if ("scale_fill_viridis_d" %in% getNamespaceExports("ggplot2")) {
    ggplot2::scale_fill_viridis_d(name = name)
  } else {
    ggplot2::scale_fill_stepsn(colours = viridisLite::viridis(12), name = name)
  }
}

#' Scale Fill for Viridis Continuous Data
#'
#' Provides a fill scale for continuous data using viridis colors.
#' Falls back to `scale_fill_gradientn` if `scale_fill_viridis_c` is not available.
#'
#' @param name Legend title.
#' @param limits Numeric vector of length 2 for scale limits.
#' @param breaks Numeric vector for break points.
#' @param labels Label formatting function or vector.
#' @return ggplot2 fill scale object.
#' @keywords internal
.scale_fill_viridis_continuous <- function(
    name = "Assurance",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)
) {
  if ("scale_fill_viridis_c" %in% getNamespaceExports("ggplot2")) {
    ggplot2::scale_fill_viridis_c(name = name, limits = limits,
                                  breaks = breaks, labels = labels)
  } else {
    ggplot2::scale_fill_gradientn(
      colours = viridisLite::viridis(256),
      name = name, limits = limits,
      breaks = breaks, labels = labels
    )
  }
}


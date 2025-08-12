#' Internal Coalesce Operator
#'
#' Returns the left-hand side if it is not \code{NULL}, otherwise the right-hand side.
#'
#' @param x Left-hand value.
#' @param y Right-hand value.
#' @return If \code{x} is not NULL, returns \code{x}, else \code{y}.
#' @keywords internal
#'
#' @name or_or
#' @export
`%||%` <- rlang::`%||%`

#' Compute Mean Assurance for a Given Metric
#'
#' Internal function to summarise simulation results and compute the
#' proportion of passes for a given decision rule metric.
#'
#' @param df Data frame containing simulation results with columns \code{n},
#'   \code{true_effect}, \code{ok}, and a metric column.
#' @param metric Character; one of "direction", "threshold", "rope".
#' @param prob_threshold Numeric; probability threshold for \code{"threshold"} and \code{"rope"} metrics.
#' @param rope_rule Reserved for future use (e.g. >= vs <= inside-ROPE rules).
#' @param direction_p Probability cutoff for "direction" metric (default 0.5).
#' @return A tibble grouped by \code{n} and \code{true_effect} with column \code{assurance}.
#' @keywords internal
.compute_assurance <- function(df, metric, prob_threshold, rope_rule = c("\u2265"), direction_p = 0.5) {
  metric <- match.arg(metric, c("direction", "threshold", "rope"))
  colname <- switch(metric,
                    "direction" = "post_prob_direction",
                    "threshold" = "post_prob_threshold",
                    "rope"      = "post_prob_rope"
  )
  if (!all(c("n", "true_effect", colname, "ok") %in% names(df))) {
    stop("Expected columns n, true_effect, ok and ", colname, " in results.")
  }
  df <- df[df$ok, , drop = FALSE]
  if (metric == "direction") {
    df$pass <- df[[colname]] >= direction_p
  } else {
    df$pass <- df[[colname]] >= prob_threshold
  }
  dplyr::summarise(
    dplyr::group_by(df, n, true_effect),
    assurance = mean(pass, na.rm = TRUE),
    .groups = "drop"
  )
}

#' Determine ggplot2 Line Width Argument Name by Version
#' @return "linewidth" if ggplot2 >= 3.4.0, else "size".
#' @keywords internal
.gg_line_arg <- function() {
  if (utils::packageVersion("ggplot2") >= "3.4.0") "linewidth" else "size"
}

#' Add Contour Lines to a ggplot2 Plot
#'
#' Wrapper around \code{geom_contour} with preset defaults for appearance,
#' using correct width arg for ggplot2 version.
#'
#' @keywords internal
.add_contour_lines <- function(mapping = NULL, data = NULL, breaks = NULL,
                               colour = "white", alpha = 0.3, width = 0.2, bins = NULL) {
  arg <- .gg_line_arg()
  args <- list(mapping = mapping, data = data, breaks = breaks,
               colour = colour, alpha = alpha, bins = bins)
  args[[arg]] <- width
  do.call(ggplot2::geom_contour, args)
}

#' Create a ggplot2 Point Layer with Version-Compatible Width
#' @keywords internal
.geom_point_lw <- function(mapping = NULL, data = NULL, ..., width = 1.5) {
  arg <- .gg_line_arg()
  args <- c(list(mapping = mapping, data = data, ...), setNames(list(width), arg))
  do.call(ggplot2::geom_point, args)
}

#' Create a ggplot2 Line Layer with Version-Compatible Width
#' @keywords internal
.geom_line_lw <- function(mapping = NULL, data = NULL, ..., width = 1) {
  arg <- .gg_line_arg()
  args <- c(list(mapping = mapping, data = data, ...), setNames(list(width), arg))
  do.call(ggplot2::geom_line, args)
}

#' Scale Fill for Viridis Discrete Data
#' @param name Legend title (default "Assurance").
#' @return A ggplot2 scale_fill object.
#' @keywords internal
.scale_fill_viridis_discrete <- function(name = "Assurance") {
  if ("scale_fill_viridis_d" %in% getNamespaceExports("ggplot2")) {
    ggplot2::scale_fill_viridis_d(name = name)
  } else {
    ggplot2::scale_fill_stepsn(colours = viridisLite::viridis(12), name = name)
  }
}

#' Scale Fill for Viridis Continuous Data
#' @param name Legend title.
#' @param limits Numeric vector length 2 for limits.
#' @param breaks Vector of breaks.
#' @param labels Function or vector for labels.
#' @return A ggplot2 scale_fill object.
#' @keywords internal
.scale_fill_viridis_continuous <- function(
    name = "Assurance",
    limits = c(0, 1),
    breaks = seq(0, 1, 0.2),
    labels = scales::percent_format(accuracy = 1)) {
  if ("scale_fill_viridis_c" %in% getNamespaceExports("ggplot2")) {
    ggplot2::scale_fill_viridis_c(name = name, limits = limits, breaks = breaks, labels = labels)
  } else {
    ggplot2::scale_fill_gradientn(
      colours = viridisLite::viridis(256), name = name,
      limits = limits, breaks = breaks, labels = labels
    )
  }
}

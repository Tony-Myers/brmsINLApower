#' Plot Decision Assurance Curve
#'
#' Plots the assurance (proportion of simulation runs meeting a
#' posterior probability decision rule) versus `n` for a given
#' metric (`direction`, `threshold`, or `rope`) at a fixed
#' decision probability threshold `p_star`.
#'
#' @param power_results A list returned by `brms_inla_power*`.
#' @param metric Decision metric: `"direction"`, `"threshold"`, or `"rope"`.
#' @param p_star Numeric decision threshold in (0,1).
#' @param effect_weights Optional named numeric vector of weights for each true_effect value.
#' @param title,subtitle Optional plot labels.
#'
#' @return A ggplot object.
#' @export
plot_decision_assurance_curve <- function(power_results,
                                          metric = c("direction","threshold","rope"),
                                          p_star = 0.95,
                                          effect_weights = NULL,
                                          title = NULL,
                                          subtitle = NULL) {
  metric <- match.arg(metric)
  colname <- switch(metric,
                    direction = "post_prob_direction",
                    threshold = "post_prob_threshold",
                    rope      = "post_prob_rope")
  df <- dplyr::filter(power_results$results, ok)
  tmp <- df %>%
    dplyr::group_by(n, true_effect) %>%
    dplyr::summarise(
      assurance = mean(.data[[colname]] >= p_star, na.rm = TRUE),
      .groups = "drop"
    )
  agg <- .aggregate_over_effects(tmp, effect_weights, value_col = "assurance")
  ggplot2::ggplot(agg, ggplot2::aes(x = n, y = assurance)) +
    .geom_line_lw(width = 1) +
    ggplot2::geom_point(size = 2) +
    ggplot2::scale_y_continuous(limits = c(0,1),
                                labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(x = "Sample size",
                  y = paste0("Assurance: P{ post-prob \u2264 ", p_star, " }"),
                  title = `%||%`(title, paste("Decision assurance (", metric, ")", sep = "")),
                  subtitle = subtitle) +
    ggplot2::theme_minimal()
}

#' Plot Decision Threshold Contour
#'
#' Shows assurance as a function of decision threshold \code{p*} and
#' sample size, optionally averaged across effects or weighted.
#'
#' @param power_results List from simulation.
#' @param metric One of "direction", "threshold", "rope".
#' @param p_star_grid Numeric vector of decision thresholds to consider.
#' @param effect_value Optional single effect value to condition on.
#' @param effect_weights Optional named numeric vector for averaging over effects.
#' @param title,subtitle Optional labels.
#'
#' @return ggplot2 object.
#' @export
plot_decision_threshold_contour <- function(power_results,
                                            metric = c("direction","threshold","rope"),
                                            p_star_grid = seq(0.5, 0.99, by = 0.01),
                                            effect_value = NULL,
                                            effect_weights = NULL,
                                            title = NULL,
                                            subtitle = NULL) {
  metric <- match.arg(metric)
  colname <- switch(metric,
                    direction = "post_prob_direction",
                    threshold = "post_prob_threshold",
                    rope      = "post_prob_rope")

  stopifnot("results" %in% names(power_results))
  df <- dplyr::filter(power_results$results, ok)

  # Optionally filter for one effect value
  if (!is.null(effect_value)) {
    ef_vals <- sort(unique(df$true_effect))
    effect_sel <- ef_vals[which.min(abs(ef_vals - effect_value))]
    df <- dplyr::filter(df, true_effect == effect_sel)
  }

  # Build grid over p* thresholds
  grid <- lapply(p_star_grid, function(ps) {
    tmp <- df %>%
      dplyr::group_by(n, true_effect) %>%
      dplyr::summarise(
        assurance = mean(.data[[colname]] >= ps, na.rm = TRUE),
        .groups = "drop"
      )

    if (is.null(effect_value)) {
      if (is.null(effect_weights)) {
        tmp <- tmp %>%
          dplyr::group_by(n) %>%
          dplyr::summarise(assurance = mean(assurance, na.rm = TRUE), .groups = "drop")
      } else {
        w <- tibble::tibble(true_effect = as.numeric(names(effect_weights)),
                            w = as.numeric(effect_weights))
        tmp <- tmp %>%
          dplyr::inner_join(w, by = "true_effect") %>%
          dplyr::group_by(n) %>%
          dplyr::summarise(
            assurance = stats::weighted.mean(assurance, w, na.rm = TRUE),
            .groups = "drop"
          )
      }
    }
    dplyr::mutate(tmp, p_star = ps)
  }) %>% dplyr::bind_rows()

  ggplot2::ggplot(grid, ggplot2::aes(x = p_star, y = n, z = assurance)) +
    ggplot2::geom_contour_filled(bins = 12, alpha = 0.9) +
    .add_contour_lines(colour = "white", alpha = 0.3, width = 0.2) +
    .scale_fill_viridis_discrete(name = "Assurance") +
    ggplot2::labs(x = "Decision threshold p*",
                  y = "Sample size",
                  title = `%||%`(title, paste("Assurance contour for", metric)),
                  subtitle = subtitle) +
    ggplot2::theme_minimal()
}

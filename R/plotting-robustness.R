#' Plot Assurance with Robustness Ribbon Across Scenarios
#'
#' Compares assurance results from multiple scenarios by showing the range ("ribbon")
#' of values across scenarios for each sample size. Optionally overlays the
#' individual scenario curves.
#'
#' @param power_results_list Named list of results objects from `brms_inla_power`
#'   (or sequential/two-stage variants). Must have length >= 2.
#' @param metric Which assurance metric to compute: `"precision"`, `"direction"`,
#'   `"threshold"`, or `"bf"`.
#' @param precision_target CI width target if `metric="precision"`. If not supplied,
#'   taken from each result's `settings$precision_target`.
#' @param p_star Posterior probability threshold for `"direction"` / `"threshold"`.
#' @param bf_threshold BF10 threshold for `"bf"`.
#' @param effect_weights Optional named numeric vector of weights to average over true effects.
#' @param show_individual_scenarios Logical; if `TRUE`, overlay each scenario's curve.
#' @param title,subtitle Optional plot title/subtitle.
#'
#' @return A ggplot object showing the robustness ribbon (and optional individual curves).
#' @export
plot_assurance_with_robustness <- function(power_results_list,
                                           metric = c("precision","direction","threshold","bf"),
                                           precision_target = NULL,
                                           p_star = 0.95,
                                           bf_threshold = 10,
                                           effect_weights = NULL,
                                           show_individual_scenarios = FALSE,
                                           title = NULL,
                                           subtitle = NULL) {
  stopifnot(is.list(power_results_list), length(power_results_list) >= 2)
  metric <- match.arg(metric)

  scenario_names <- names(power_results_list)
  if (is.null(scenario_names) || any(scenario_names == "")) {
    scenario_names <- paste0("Scenario ", seq_along(power_results_list))
  }

  # Compute assurance per scenario
  per_scen <- lapply(seq_along(power_results_list), function(i) {
    pr <- power_results_list[[i]]
    df <- pr$results %>%
      dplyr::filter(ok) %>%
      dplyr::group_by(n, true_effect)

    tmp <- switch(metric,
                  "precision" = {
                    pt <- precision_target %||% pr$settings$precision_target
                    if (is.null(pt)) stop("precision_target must be supplied for metric='precision'.")
                    df %>%
                      dplyr::summarise(assurance = mean(ci_width <= pt, na.rm = TRUE), .groups = "drop")
                  },
                  "direction" = df %>%
                    dplyr::summarise(assurance = mean(post_prob_direction >= p_star, na.rm = TRUE), .groups = "drop"),
                  "threshold" = df %>%
                    dplyr::summarise(assurance = mean(post_prob_threshold >= p_star, na.rm = TRUE), .groups = "drop"),
                  "bf" = df %>%
                    dplyr::summarise(assurance = mean(bf10 >= bf_threshold, na.rm = TRUE), .groups = "drop")
    )

    agg <- .aggregate_over_effects(tmp, effect_weights, value_col = "assurance")
    agg$scenario <- scenario_names[i]
    agg
  }) %>% dplyr::bind_rows()

  # Ribbon spanning min-max assurance across scenarios
  ribbon <- per_scen %>%
    dplyr::group_by(n) %>%
    dplyr::summarise(
      ymin = min(assurance, na.rm = TRUE),
      ymax = max(assurance, na.rm = TRUE),
      ymid = mean(assurance, na.rm = TRUE),
      .groups = "drop"
    )

  p <- ggplot2::ggplot(ribbon, ggplot2::aes(x = n)) +
    ggplot2::geom_ribbon(ggplot2::aes(ymin = ymin, ymax = ymax), alpha = 0.15) +
    .geom_line_lw(mapping = ggplot2::aes(y = ymid), width = 1)

  if (isTRUE(show_individual_scenarios)) {
    p <- p +
      .geom_line_lw(data = per_scen,
                    mapping = ggplot2::aes(y = assurance, colour = scenario),
                    width = 0.8) +
      if ("scale_colour_viridis_d" %in% getNamespaceExports("ggplot2")) {
        ggplot2::scale_colour_viridis_d(name = "Scenario", option = "plasma")
      } else {
        ggplot2::scale_colour_manual(values = viridisLite::viridis(length(unique(per_scen$scenario))))
      }
  }

  y_lab <- switch(metric,
                  "precision" = "Assurance P(width \u2264 target)",
                  "direction" = paste0("Assurance P{ post-prob \u2265 ", p_star, " }"),
                  "threshold" = paste0("Assurance P{ post-prob \u2265 ", p_star, " }"),
                  "bf" = paste0("Assurance P{ BF10 \u2265 ", bf_threshold, " }"))

  p +
    ggplot2::scale_y_continuous(limits = c(0,1), labels = scales::percent_format(accuracy = 1)) +
    ggplot2::labs(x = "Sample size", y = y_lab,
                  title = title %||% "Assurance with robustness ribbon across scenarios",
                  subtitle = subtitle) +
    ggplot2::theme_minimal()
}

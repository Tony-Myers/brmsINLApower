test_that("plotting functions return ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  # Minimal fake brms_inla_power() output
  fake_results <- list(
    summary = data.frame(
      n = rep(c(10, 20), each = 2),
      true_effect = rep(c(0.5, 1.0), times = 2),
      power_direction = runif(4),
      power_threshold = runif(4),
      power_rope = runif(4)
    ),
    settings = list(effect_name = "x"),
    results  = data.frame(
      n = rep(c(10, 20), each = 2),
      true_effect = rep(c(0.5, 1.0), times = 2),
      post_prob_direction = runif(4),
      post_prob_threshold = runif(4),
      post_prob_rope = runif(4),
      ok = TRUE
    )
  )
  
  expect_s3_class(brmsINLApower::plot_power_contour(fake_results), "ggplot")
  expect_s3_class(brmsINLApower::plot_power_heatmap(fake_results), "ggplot")
})


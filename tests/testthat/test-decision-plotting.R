test_that("decision-related plotting functions return ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  # Fake minimal results object for testing
  fake_results <- list(
    results = data.frame(
      n = rep(c(10, 20), each = 2),
      true_effect = rep(c(0.5, 1.0), times = 2),
      post_prob_direction = runif(4),
      post_prob_threshold = runif(4),
      post_prob_rope = runif(4),
      ok = TRUE
    ),
    settings = list(effect_name = "x")
  )
  
  expect_s3_class(
    brmsINLApower::plot_decision_assurance_curve(fake_results, metric = "direction", p_star = 0.9),
    "ggplot"
  )
  
  expect_s3_class(
    brmsINLApower::plot_decision_threshold_contour(fake_results, metric = "threshold"),
    "ggplot"
  )
})

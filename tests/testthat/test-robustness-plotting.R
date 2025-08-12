test_that("robustness plotting functions run and return ggplot", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  # Create two fake scenarios for input
  fake_results1 <- list(
    results = data.frame(
      n = c(10, 20),
      true_effect = c(0.5, 1.0),
      post_prob_direction = runif(2),
      ci_width = runif(2),
      bf10 = runif(2),
      ok = TRUE
    ),
    settings = list(precision_target = 0.1, compute_bayes_factor = TRUE)
  )
  fake_results2 <- fake_results1  # duplicate for simplicity, could vary if desired
  fake_list <- list(Scenario1 = fake_results1, Scenario2 = fake_results2)
  
  expect_s3_class(
    brmsINLApower::plot_assurance_with_robustness(
      fake_list, metric = "direction", show_individual_scenarios = TRUE
    ),
    "ggplot"
  )
  expect_s3_class(
    brmsINLApower::plot_assurance_with_robustness(
      fake_list, metric = "precision"
    ),
    "ggplot"
  )
  expect_s3_class(
    brmsINLApower::plot_assurance_with_robustness(
      fake_list, metric = "bf", bf_threshold = 3
    ),
    "ggplot"
  )
})

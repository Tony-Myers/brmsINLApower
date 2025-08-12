test_that("Bayes factor plotting functions return ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  fake_results <- list(
    results = data.frame(
      n = rep(c(10, 20), each = 3),
      true_effect = rep(c(0.5, 1.0), times = 3),
      bf10 = runif(6, 0, 10),
      ok = TRUE
    ),
    settings = list(
      effect_name = "x",
      compute_bayes_factor = TRUE
    ),
    summary = data.frame(
      n = c(10, 20),
      true_effect = c(0.5, 1.0),
      mean_log10_bf = runif(2)
    )
  )
  
  expect_s3_class(
    brmsINLApower::plot_bf_assurance_curve(fake_results),
    "ggplot"
  )
  expect_s3_class(
    brmsINLApower::plot_bf_expected_evidence(fake_results),
    "ggplot"
  )
  expect_s3_class(
    brmsINLApower::plot_bf_heatmap(fake_results),
    "ggplot"
  )
})

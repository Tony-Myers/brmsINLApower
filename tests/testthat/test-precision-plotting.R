test_that("precision plotting functions return ggplot objects", {
  skip_on_cran()
  skip_if_not_installed("ggplot2")
  
  fake_results <- list(
    results = data.frame(
      n = rep(c(10, 20), each = 3),
      true_effect = rep(c(0.5, 1.0), times = 3),
      ci_width = runif(6),
      ok = TRUE
    ),
    settings = list(
      effect_name = "x",
      precision_target = 0.1
    ),
    summary = data.frame(
      n = c(10, 20),
      true_effect = c(0.5, 1.0),
      power_direction = c(0.7, 0.9)
    )
  )
  
  expect_s3_class(
    brmsINLApower::plot_precision_assurance_curve(fake_results),
    "ggplot"
  )
  
  expect_s3_class(
    brmsINLApower::plot_precision_fanchart(fake_results),
    "ggplot"
  )
})

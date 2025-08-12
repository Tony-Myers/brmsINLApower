test_that("brms_inla_power basic run returns list with expected components", {
  skip_on_cran()

  set.seed(1)

  # Minimal reproducible example:
  test_data <- data.frame(
    x = rnorm(30),
    y = rnorm(30)
  )

  fmla <- y ~ x
  effect <- "x"

  result <- brmsINLApower::brms_inla_power(
    formula = fmla,
    family = gaussian(),
    effect_name = effect,
    effect_grid = 0.5,
    sample_sizes = c(10),
    nsims = 2,  # very low for quick test
    data_generator = function(...) test_data
  )

  # Check high-level structure
  expect_type(result, "list")
  expect_true(all(c("results", "summary", "settings") %in% names(result)))

  # Check that results and summary are data frames with expected structure
  expect_s3_class(result$results, "data.frame")
  expect_true(all(c("n", "true_effect", "ok") %in% colnames(result$results)))

  expect_s3_class(result$summary, "data.frame")
  expect_true(any(grepl("power_", colnames(result$summary), fixed = TRUE)))

  # Settings should be a list
  expect_type(result$settings, "list")
})

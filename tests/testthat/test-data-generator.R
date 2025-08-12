test_that("internal .auto_data_generator produces valid data", {
  skip_on_cran()
  
  fmla <- y ~ x
  gen_fun <- brmsINLApower:::`.auto_data_generator`(
    formula = fmla,
    effect_name = "x",
    family = gaussian(),
    error_sd = 1
  )
  
  dat <- gen_fun(n = 10, effect = 0.5)
  
  expect_s3_class(dat, "data.frame")
  expect_true(all(c("y", "x") %in% names(dat)))
  expect_equal(nrow(dat), 10)
})

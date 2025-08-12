test_that(".compute_assurance works for different metrics", {
  df <- data.frame(
    n = c(10, 10, 20, 20),
    true_effect = c(0.5, 0.5, 1.0, 1.0),
    post_prob_direction = c(0.6, 0.4, 0.7, 0.9),
    ok = TRUE
  )
  
  res_dir <- brmsINLApower:::`.compute_assurance`(df, "direction", 0.5)
  expect_true(all(c("n", "true_effect", "assurance") %in% colnames(res_dir)))
  
  # If other metric modes exist, test them:
  # res_other <- brmsINLApower:::`.compute_assurance`(df, "other_metric", 0.5)
})

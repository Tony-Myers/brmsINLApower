test_that(".compute_assurance works", {
  df <- data.frame(
    n = c(10, 10, 20, 20),
    true_effect = c(0.5, 0.5, 1, 1),
    post_prob_direction = c(0.6, 0.4, 0.8, 0.9),
    post_prob_threshold = c(0.7, 0.2, 0.9, 0.95),
    post_prob_rope = c(0.1, 0.2, 0.3, 0.4),
    ok = TRUE
  )
  
  res <- brmsINLApower:::`.compute_assurance`(df, "direction", prob_threshold = 0.5)
  expect_true(all(c("n", "true_effect", "assurance") %in% names(res)))
})

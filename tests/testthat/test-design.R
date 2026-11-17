test_that("sample size matches the textbook formula", {
  # Normal approximation: (z_a + z_b)^2 * (p1 q1 + p2 q2) / d^2
  p1 <- 0.03
  p2 <- 0.035
  approx <- (qnorm(0.975) + qnorm(0.8))^2 * (p1 * (1 - p1) + p2 * (1 - p2)) / (p2 - p1)^2
  expect_equal(required_sample_size(p1, 0.005), ceiling(approx), tolerance = 0.01)
})

test_that("minimum detectable effect inverts the sample size", {
  n <- required_sample_size(0.03, 0.004)
  expect_equal(minimum_detectable_effect(0.03, n), 0.004, tolerance = 1e-4)
})

test_that("SRM check passes a fair split and flags a broken one", {
  set.seed(1)
  fair <- sample(c("A", "B"), 50000, replace = TRUE)
  expect_false(srm_check(fair)$mismatch)
  broken <- c(rep("A", 25600), rep("B", 24400))
  expect_true(srm_check(broken)$mismatch)
})

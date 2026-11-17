test_that("two-proportion test agrees with prop.test without continuity correction", {
  res <- two_proportion_test(300, 10000, 360, 10000)
  ref <- prop.test(c(360, 300), c(10000, 10000), correct = FALSE)
  expect_equal(res$diff, 0.006)
  expect_equal(res$p_value, ref$p.value, tolerance = 1e-8)
  expect_lt(res$ci_low, res$diff)
  expect_gt(res$ci_high, res$diff)
})

test_that("mean difference is B minus A", {
  y <- c(1, 2, 3, 11, 12, 13)
  g <- c("A", "A", "A", "B", "B", "B")
  res <- mean_difference(y, g)
  expect_equal(res$diff, 10)
  expect_lt(res$ci_low, 10)
})

test_that("CUPED keeps the difference and reduces the variance", {
  set.seed(5)
  x <- rnorm(20000, 50, 10)
  g <- rep(c("A", "B"), each = 10000)
  y <- 0.8 * x + rnorm(20000) + (g == "B") * 1
  adjusted <- cuped_adjust(y, x)
  expect_equal(mean_difference(adjusted, g)$diff, mean_difference(y, g)$diff, tolerance = 0.05)
  expect_lt(var(adjusted), var(y) / 10)
})

test_that("Bayesian probability is near 0.5 for identical arms and near 1 for a clear win", {
  even <- beta_binomial(300, 10000, 300, 10000)
  expect_equal(even$prob_b_better, 0.5, tolerance = 0.02)
  clear <- beta_binomial(300, 10000, 450, 10000)
  expect_gt(clear$prob_b_better, 0.999)
  expect_lt(clear$expected_loss_b, 1e-5)
})

test_that("daily peeking inflates false positives above alpha", {
  res <- peeking_simulation(n_sims = 400, seed = 2)
  expect_equal(res$summary$false_positive_rate[1], 0.05, tolerance = 0.03)
  expect_gt(res$summary$false_positive_rate[2], 0.1)
  expect_true(all(diff(res$by_day$false_positive_rate) >= 0))
})

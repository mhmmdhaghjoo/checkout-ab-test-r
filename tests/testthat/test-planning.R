test_that("days_needed turns a sample size into whole weeks", {
  # 10,000 per arm at 2,000 visitors a day: 10 days, rounded up to 14
  expect_equal(days_needed(10000, 2000, whole_weeks = FALSE), 10)
  expect_equal(days_needed(10000, 2000), 14)
  # only half of the traffic in the test doubles the time
  expect_equal(days_needed(10000, 2000, share_in_test = 0.5, whole_weeks = FALSE), 20)
  expect_error(days_needed(10000, 2000, share_in_test = 1.5))
})

test_that("segment_table recovers per-segment rates", {
  d <- data.frame(
    variant = c("A", "A", "B", "B", "A", "B"),
    device = c("mobile", "mobile", "mobile", "mobile", "desktop", "desktop"),
    converted = c(0, 1, 1, 1, 0, 1)
  )
  out <- segment_table(d, "device")
  expect_equal(out$segment, c("desktop", "mobile"))
  expect_equal(out$visitors, c(2, 4))
  expect_equal(out$rate_a[out$segment == "mobile"], 0.5)
  expect_equal(out$diff[out$segment == "mobile"], 0.5)
})

test_that("segment_table finds the simulated mobile-only effect", {
  d <- simulate_experiment(n_visitors = 200000, seed = 7)
  out <- segment_table(d, "device")
  mobile <- out[out$segment == "mobile", ]
  desktop <- out[out$segment == "desktop", ]
  expect_gt(mobile$ci_low, 0)
  expect_lt(desktop$ci_low, 0)
  expect_gt(desktop$ci_high, 0)
})

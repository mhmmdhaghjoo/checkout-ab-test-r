test_that("simulation is reproducible and has the expected shape", {
  a <- simulate_experiment(n_visitors = 5000, seed = 7)
  b <- simulate_experiment(n_visitors = 5000, seed = 7)
  expect_identical(a, b)
  expect_equal(nrow(a), 5000)
  expect_setequal(unique(a$variant), c("A", "B"))
  expect_true(all(a$revenue[a$converted == 0] == 0))
  expect_true(all(a$revenue[a$converted == 1] > 0))
  expect_true(all(a$pre_spend[a$customer == "new"] == 0))
})

test_that("baseline conversion matches the requested rate", {
  d <- simulate_experiment(n_visitors = 200000, lift_mobile = 0, lift_desktop = 0, seed = 3)
  expect_equal(mean(d$converted), 0.03, tolerance = 0.002)
})

test_that("the planted effect is in the data", {
  d <- simulate_experiment(n_visitors = 400000, seed = 11)
  mobile <- d[d$device == "mobile", ]
  desktop <- d[d$device == "desktop", ]
  lift <- function(x) mean(x$converted[x$variant == "B"]) - mean(x$converted[x$variant == "A"])
  expect_gt(lift(mobile), 0.003)
  expect_lt(abs(lift(desktop)), 0.002)
})

test_that("true_segment_lift matches the lift the simulation produces", {
  d <- simulate_experiment(n_visitors = 400000, seed = 11, lift_mobile = 0.01, lift_desktop = 0)
  truth <- true_segment_lift(d, lift_mobile = 0.01, lift_desktop = 0)
  est <- segment_table(d, "device")
  both <- merge(truth, est, by = "segment")
  expect_equal(both$true_lift[both$segment == "desktop"], 0)
  # the estimate's interval covers the recomputed truth in both segments
  expect_true(all(both$ci_low <= both$true_lift & both$true_lift <= both$ci_high))
  # and on mobile the truth is not the raw argument, because of the scaling
  expect_false(isTRUE(all.equal(both$true_lift[both$segment == "mobile"], 0.01)))
})

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

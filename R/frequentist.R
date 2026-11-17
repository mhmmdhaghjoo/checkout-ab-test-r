# Frequentist estimates for the primary and secondary metrics.

# Difference in conversion rates (B minus A): Wald interval for the
# difference, p-value from the pooled two-proportion z-test.
two_proportion_test <- function(x_a, n_a, x_b, n_b, conf = 0.95) {
  p_a <- x_a / n_a
  p_b <- x_b / n_b
  diff <- p_b - p_a
  se <- sqrt(p_a * (1 - p_a) / n_a + p_b * (1 - p_b) / n_b)
  z <- stats::qnorm(1 - (1 - conf) / 2)
  pooled <- (x_a + x_b) / (n_a + n_b)
  se_null <- sqrt(pooled * (1 - pooled) * (1 / n_a + 1 / n_b))
  data.frame(
    rate_a = p_a, rate_b = p_b, diff = diff,
    ci_low = diff - z * se, ci_high = diff + z * se,
    relative_lift = diff / p_a,
    p_value = 2 * stats::pnorm(-abs(diff / se_null))
  )
}

# Difference in means (B minus A) with Welch's t-test, for skewed metrics such
# as revenue per visitor. With tens of thousands of visitors per arm the
# sampling distribution of the mean is close to normal even when y is not.
mean_difference <- function(y, variant, conf = 0.95) {
  a <- y[variant == "A"]
  b <- y[variant == "B"]
  test <- stats::t.test(b, a, conf.level = conf)
  data.frame(
    mean_a = mean(a), mean_b = mean(b), diff = mean(b) - mean(a),
    ci_low = test$conf.int[1], ci_high = test$conf.int[2],
    p_value = test$p.value
  )
}

# CUPED: remove the part of the outcome that a pre-experiment covariate
# already predicts. The adjusted metric has the same expected difference
# between arms but less variance, so the interval gets narrower.
cuped_adjust <- function(y, x) {
  theta <- stats::cov(y, x) / stats::var(x)
  y - theta * (x - mean(x))
}

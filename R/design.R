# Experiment design: how many visitors are needed, and what can they detect?

# Visitors per arm to detect an absolute lift `mde` from `base_rate`.
required_sample_size <- function(base_rate, mde, power = 0.8, alpha = 0.05) {
  test <- stats::power.prop.test(p1 = base_rate, p2 = base_rate + mde,
                                 power = power, sig.level = alpha)
  ceiling(test$n)
}

# Smallest absolute lift detectable with `n_per_arm` visitors per arm.
minimum_detectable_effect <- function(base_rate, n_per_arm, power = 0.8, alpha = 0.05) {
  gap <- function(d) {
    stats::power.prop.test(n = n_per_arm, p1 = base_rate, p2 = base_rate + d,
                           sig.level = alpha)$power - power
  }
  stats::uniroot(gap, c(1e-6, 1 - base_rate - 1e-6))$root
}

# Sample ratio mismatch: did randomisation deliver the planned split?
# A tiny p-value means the assignment or the logging is broken, and the
# experiment should not be analysed until that is fixed.
srm_check <- function(variant, expected = c(A = 0.5, B = 0.5), threshold = 0.001) {
  counts <- table(factor(variant, levels = names(expected)))
  test <- stats::chisq.test(as.numeric(counts), p = unname(expected))
  list(counts = as.integer(counts), p_value = test$p.value, mismatch = test$p.value < threshold)
}

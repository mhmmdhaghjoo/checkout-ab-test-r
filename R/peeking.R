# Why the test runs for its planned length: the cost of peeking.
#
# Simulates A/A tests (no true difference) and compares two habits: looking
# once at the planned end, and looking every day and stopping as soon as
# p < alpha. The second habit declares far more false winners.
peeking_simulation <- function(n_sims = 2000, visitors_per_day = 5714, days = 14,
                               rate = 0.03, alpha = 0.05, seed = 1) {
  set.seed(seed)
  per_arm <- visitors_per_day / 2
  n <- round(seq_len(days) * per_arm)
  first_hit <- rep(NA_integer_, n_sims)
  final_hit <- logical(n_sims)
  for (i in seq_len(n_sims)) {
    a <- cumsum(stats::rbinom(days, round(per_arm), rate))
    b <- cumsum(stats::rbinom(days, round(per_arm), rate))
    p <- two_proportion_test(a, n, b, n)$p_value
    hits <- which(p < alpha)
    if (length(hits) > 0) first_hit[i] <- hits[1]
    final_hit[i] <- p[days] < alpha
  }
  cumulative <- vapply(seq_len(days), function(d) mean(!is.na(first_hit) & first_hit <= d),
                       numeric(1))
  list(
    summary = data.frame(
      strategy = c("Look once, at the planned end", "Look daily, stop at first p < 0.05"),
      false_positive_rate = c(mean(final_hit), mean(!is.na(first_hit)))
    ),
    by_day = data.frame(day = seq_len(days), false_positive_rate = cumulative)
  )
}

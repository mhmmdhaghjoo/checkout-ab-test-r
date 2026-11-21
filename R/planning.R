# Turning a sample size into a calendar: how long must the test run?

# Days needed so that each arm reaches `n_per_arm` visitors, when
# `visitors_per_day` reach the checkout and `share_in_test` of them are
# randomised. Rounded up to whole weeks by default, because conversion
# differs between weekdays and weekends and a test should cover full weeks.
days_needed <- function(n_per_arm, visitors_per_day, share_in_test = 1, whole_weeks = TRUE) {
  stopifnot(n_per_arm > 0, visitors_per_day > 0, share_in_test > 0, share_in_test <= 1)
  days <- ceiling(2 * n_per_arm / (visitors_per_day * share_in_test))
  if (whole_weeks) days <- 7 * ceiling(days / 7)
  days
}

# Conversion difference (B minus A) per segment with a Wald interval.
# Returns one row per level of `segment`.
segment_table <- function(data, segment = "device", conf = 0.95) {
  levels <- sort(unique(data[[segment]]))
  rows <- lapply(levels, function(level) {
    d <- data[data[[segment]] == level, ]
    a <- d$variant == "A"
    b <- d$variant == "B"
    cbind(segment = level, visitors = nrow(d),
          two_proportion_test(sum(d$converted[a]), sum(a), sum(d$converted[b]), sum(b), conf))
  })
  do.call(rbind, rows)
}

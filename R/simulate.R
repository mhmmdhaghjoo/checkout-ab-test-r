# Simulate the one-page checkout experiment.
#
# Every visitor is randomised 50/50 to the current checkout (A) or the new
# one-page checkout (B). The true effects are set by the arguments, so the
# analysis can be checked against a known answer. With the defaults the new
# checkout helps on mobile, does nothing on desktop, and lowers the average
# order value a little, because the extra buyers it wins have smaller baskets.
simulate_experiment <- function(n_visitors = 80000, days = 14, seed = 2026,
                                base_rate = 0.03, lift_mobile = 0.007, lift_desktop = 0,
                                aov_mean = 62, aov_change = -2.5) {
  set.seed(seed)
  device <- sample(c("mobile", "desktop"), n_visitors, replace = TRUE, prob = c(0.6, 0.4))
  returning <- stats::runif(n_visitors) < 0.35
  pre_spend <- ifelse(returning, stats::rgamma(n_visitors, shape = 1.2, scale = 55), 0)
  variant <- ifelse(stats::runif(n_visitors) < 0.5, "A", "B")
  day <- sample.int(days, n_visitors, replace = TRUE)

  # Baseline conversion: higher on desktop and for customers who spent more
  # in the 30 days before the test. Rescaled so the average is base_rate.
  device_factor <- ifelse(device == "desktop", 1.3, 0.8)
  score <- device_factor * ifelse(returning, 1 + pre_spend / 40, 0.6)
  p_base <- base_rate * score / mean(score)
  lift <- ifelse(device == "mobile", lift_mobile, lift_desktop) * p_base / base_rate
  p <- pmin(p_base + (variant == "B") * lift, 0.95)
  converted <- as.integer(stats::runif(n_visitors) < p)

  basket_mean <- aov_mean + (variant == "B") * aov_change + 0.15 * (pre_spend - mean(pre_spend))
  value <- stats::rgamma(n_visitors, shape = 4, scale = pmax(basket_mean, 5) / 4)

  data.frame(
    visitor_id = seq_len(n_visitors),
    day = day,
    variant = variant,
    device = device,
    customer = ifelse(returning, "returning", "new"),
    pre_spend = round(pre_spend, 2),
    converted = converted,
    revenue = round(converted * value, 2),
    stringsAsFactors = FALSE
  )
}

# The true average lift in each segment of a simulated experiment.
# simulate_experiment() scales the lift by each visitor's baseline
# conversion, so the lift a segment really gets differs from the
# lift_mobile / lift_desktop arguments. This recomputes it from the same
# formula, which is what an estimate should be compared with.
true_segment_lift <- function(data, segment = "device", base_rate = 0.03,
                              lift_mobile = 0.007, lift_desktop = 0) {
  returning <- data$customer == "returning"
  device_factor <- ifelse(data$device == "desktop", 1.3, 0.8)
  score <- device_factor * ifelse(returning, 1 + data$pre_spend / 40, 0.6)
  p_base <- base_rate * score / mean(score)
  lift <- ifelse(data$device == "mobile", lift_mobile, lift_desktop) * p_base / base_rate
  lift <- pmin(p_base + lift, 0.95) - p_base
  out <- stats::aggregate(lift, list(segment = data[[segment]]), mean)
  names(out)[2] <- "true_lift"
  out
}

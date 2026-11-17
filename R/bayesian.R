# Bayesian estimates, answering the questions a product manager asks:
# how likely is B better, and how much do we lose if we pick wrongly?

# Beta-binomial model for conversion with a flat Beta(1, 1) prior by default.
beta_binomial <- function(x_a, n_a, x_b, n_b, prior = c(1, 1), draws = 100000, seed = 1) {
  set.seed(seed)
  p_a <- stats::rbeta(draws, prior[1] + x_a, prior[2] + n_a - x_a)
  p_b <- stats::rbeta(draws, prior[1] + x_b, prior[2] + n_b - x_b)
  diff <- p_b - p_a
  data.frame(
    prob_b_better = mean(diff > 0),
    expected_loss_b = mean(pmax(p_a - p_b, 0)),
    expected_loss_a = mean(pmax(p_b - p_a, 0)),
    diff_median = stats::median(diff),
    diff_low = unname(stats::quantile(diff, 0.025)),
    diff_high = unname(stats::quantile(diff, 0.975))
  )
}

# Effect of B by segment from one Bayesian logistic regression (rstanarm).
# The data are aggregated to one row per variant and segment first, which
# gives the same posterior as the visitor-level model in a fraction of the time.
segment_effects <- function(data, segment = "device", chains = 2, iter = 2000, seed = 1) {
  data$segment <- data[[segment]]
  data$one <- 1
  agg <- stats::aggregate(cbind(conversions = converted, visitors = one) ~ variant + segment,
                          data = data, FUN = sum)
  fit <- rstanarm::stan_glm(
    cbind(conversions, visitors - conversions) ~ variant * segment,
    data = agg, family = stats::binomial(),
    prior = rstanarm::normal(0, 1), prior_intercept = rstanarm::normal(-3.5, 1),
    chains = chains, iter = iter, seed = seed, refresh = 0
  )
  levels <- sort(unique(agg$segment))
  grid <- expand.grid(variant = c("A", "B"), segment = levels, stringsAsFactors = FALSE)
  grid$conversions <- 0
  grid$visitors <- 1
  prob <- stats::plogis(rstanarm::posterior_linpred(fit, newdata = grid))
  rows <- lapply(levels, function(level) {
    diff <- prob[, grid$segment == level & grid$variant == "B"] -
      prob[, grid$segment == level & grid$variant == "A"]
    data.frame(
      segment = level,
      diff_median = stats::median(diff),
      diff_low = unname(stats::quantile(diff, 0.025)),
      diff_high = unname(stats::quantile(diff, 0.975)),
      prob_positive = mean(diff > 0)
    )
  })
  do.call(rbind, rows)
}

# Run the whole experiment analysis and write results and figures to reports/.
# Usage (from the repository root): Rscript analysis/run_analysis.R
suppressPackageStartupMessages(library(ggplot2))
for (file in list.files("R", pattern = "\\.R$", full.names = TRUE)) source(file)

dir.create("reports/figures", recursive = TRUE, showWarnings = FALSE)
dir.create("data", showWarnings = FALSE)
theme_set(theme_minimal(base_size = 12) +
            theme(plot.title.position = "plot", plot.title = element_text(face = "bold"),
                  panel.grid.minor = element_blank()))
blue <- "#2a78d6"
orange <- "#eb6834"
muted <- "#6b6a65"
save_plot <- function(plot, name, height = 4) {
  ggsave(file.path("reports/figures", name), plot, width = 8, height = height, dpi = 150, bg = "white")
}

# 1. Design, decided before the data come in -----------------------------------
base_rate <- 0.03
planned_mde <- 0.0035
n_per_arm <- required_sample_size(base_rate, planned_mde)
design <- list(base_rate = base_rate, mde = planned_mde, n_per_arm = n_per_arm,
               visitors_per_day = 5714, days = ceiling(2 * n_per_arm / 5714))

# 2. Data -----------------------------------------------------------------------
data <- simulate_experiment(n_visitors = 80000, days = 14, seed = 2026)
write.csv(data, "data/experiment.csv", row.names = FALSE)
data$one <- 1

# 3. Trust checks -----------------------------------------------------------------
srm <- srm_check(data$variant)
balance <- aggregate(cbind(mobile = device == "mobile", returning = customer == "returning",
                           pre_spend = pre_spend) ~ variant, data = data, FUN = mean)

# 4. Primary metric: conversion ----------------------------------------------------
counts <- aggregate(cbind(conversions = converted, visitors = one) ~ variant, data = data, FUN = sum)
x <- setNames(counts$conversions, counts$variant)
n <- setNames(counts$visitors, counts$variant)
primary <- two_proportion_test(x[["A"]], n[["A"]], x[["B"]], n[["B"]])
bayes <- beta_binomial(x[["A"]], n[["A"]], x[["B"]], n[["B"]])

# 5. Secondary and guardrail metrics -------------------------------------------------
rpv <- mean_difference(data$revenue, data$variant)
rpv_cuped <- mean_difference(cuped_adjust(data$revenue, data$pre_spend), data$variant)
buyers <- data[data$converted == 1, ]
aov <- mean_difference(buyers$revenue, buyers$variant)
variance_reduction <- 1 - ((rpv_cuped$ci_high - rpv_cuped$ci_low) / (rpv$ci_high - rpv$ci_low))^2

# 6. Where does the effect come from? -----------------------------------------------
segments <- segment_effects(data, "device")
segments_customer <- segment_effects(data, "customer")

# 7. What peeking would have done ---------------------------------------------------
peeking <- peeking_simulation(n_sims = 2000)

# 8. Decision in money ----------------------------------------------------------------
monthly_visitors <- 5714 * 30
mobile_share <- mean(data$device == "mobile")
mobile_rpv <- with(data[data$device == "mobile", ], mean_difference(revenue, variant))

results <- list(design = design, srm = srm, balance = balance, primary = primary, bayes = bayes,
                rpv = rpv, rpv_cuped = rpv_cuped, aov = aov, variance_reduction = variance_reduction,
                segments = segments, segments_customer = segments_customer, peeking = peeking,
                monthly_visitors = monthly_visitors, mobile_share = mobile_share,
                mobile_rpv = mobile_rpv, n_visitors = nrow(data))
saveRDS(results, "reports/results.rds")
write.csv(rbind(cbind(metric = "conversion", primary[, c("diff", "ci_low", "ci_high", "p_value")]),
                cbind(metric = "revenue per visitor", rpv[, c("diff", "ci_low", "ci_high", "p_value")]),
                cbind(metric = "revenue per visitor (CUPED)",
                      rpv_cuped[, c("diff", "ci_low", "ci_high", "p_value")]),
                cbind(metric = "average order value", aov[, c("diff", "ci_low", "ci_high", "p_value")])),
          "reports/metrics.csv", row.names = FALSE)
write.csv(rbind(cbind(by = "device", segments), cbind(by = "customer", segments_customer)),
          "reports/segments.csv", row.names = FALSE)
write.csv(peeking$summary, "reports/peeking.csv", row.names = FALSE)

# Figures ----------------------------------------------------------------------------
pct <- function(v) paste0(format(round(100 * v, 1), nsmall = 1), "%")

# Cumulative conversion by day: the lines separate early and stay apart.
daily <- aggregate(cbind(conversions = converted, visitors = one) ~ variant + day, data = data, FUN = sum)
daily <- daily[order(daily$variant, daily$day), ]
daily$cum_rate <- ave(daily$conversions, daily$variant, FUN = cumsum) /
  ave(daily$visitors, daily$variant, FUN = cumsum)
daily$label <- ifelse(daily$variant == "A", "A: current checkout", "B: one-page checkout")
end <- daily[daily$day == max(daily$day), ]
p1 <- ggplot(daily, aes(day, cum_rate, colour = label)) +
  geom_line(linewidth = 1) +
  geom_text(data = end, aes(label = pct(cum_rate)), hjust = -0.2, show.legend = FALSE) +
  scale_colour_manual(values = c(muted, blue), name = NULL) +
  scale_y_continuous(labels = pct) +
  scale_x_continuous(breaks = seq(2, 14, 2), limits = c(1, 15)) +
  labs(title = "Cumulative conversion rate by day of the test", x = "Day", y = NULL) +
  theme(legend.position = "top", legend.justification = "left")
save_plot(p1, "cumulative_conversion.png")

seg <- rbind(cbind(group = "Device", segments), cbind(group = "Customer", segments_customer))
seg$segment <- factor(seg$segment, levels = rev(seg$segment))
p2 <- ggplot(seg, aes(diff_median, segment)) +
  geom_vline(xintercept = 0, colour = muted, linetype = "dashed") +
  geom_errorbar(aes(xmin = diff_low, xmax = diff_high), width = 0.15, colour = blue) +
  geom_point(size = 3, colour = blue) +
  facet_grid(group ~ ., scales = "free_y", space = "free_y") +
  scale_x_continuous(labels = function(v) paste0(round(100 * v, 2), " pp")) +
  labs(title = "Effect of the one-page checkout on conversion, by segment",
       subtitle = "Posterior median and 95% interval, Bayesian logistic regression (rstanarm)",
       x = "Change in conversion rate (percentage points)", y = NULL)
save_plot(p2, "segment_effects.png", height = 3.6)

p3 <- ggplot(peeking$by_day, aes(day, false_positive_rate)) +
  geom_hline(yintercept = 0.05, colour = orange, linetype = "dashed") +
  annotate("text", x = 14, y = 0.035, label = "5% promised by the test", hjust = 1, colour = orange) +
  geom_line(colour = blue, linewidth = 1) +
  geom_point(colour = blue) +
  scale_y_continuous(labels = pct, limits = c(0, NA)) +
  scale_x_continuous(breaks = seq(2, 14, 2)) +
  labs(title = "Peeking daily: share of A/A tests declared a winner by each day",
       subtitle = "2,000 simulated tests with no true difference",
       x = "Day", y = NULL)
save_plot(p3, "peeking.png")

ci <- rbind(cbind(metric = "Without CUPED", rpv[, c("diff", "ci_low", "ci_high")]),
            cbind(metric = "With CUPED", rpv_cuped[, c("diff", "ci_low", "ci_high")]))
ci$metric <- factor(ci$metric, levels = c("With CUPED", "Without CUPED"))
p4 <- ggplot(ci, aes(diff, metric)) +
  geom_vline(xintercept = 0, colour = muted, linetype = "dashed") +
  geom_errorbar(aes(xmin = ci_low, xmax = ci_high), width = 0.12, colour = blue) +
  geom_point(size = 3, colour = blue) +
  scale_x_continuous(labels = function(v) paste0("€", format(round(v, 2), nsmall = 2))) +
  labs(title = "Revenue per visitor, B minus A, with 95% interval",
       subtitle = "CUPED uses each visitor's spend in the 30 days before the test",
       x = "Difference in revenue per visitor", y = NULL)
save_plot(p4, "cuped.png", height = 2.8)

# Report ------------------------------------------------------------------------------
rmarkdown::render("analysis/report.Rmd",
                  output_format = rmarkdown::github_document(html_preview = FALSE),
                  output_file = "report.md", output_dir = "reports",
                  knit_root_dir = getwd(), quiet = TRUE)
message("Done: reports/report.md")

# Experiment planner and reader for the one-page checkout test.
#
# Run from the repository root:  shiny::runApp("app")
#
# The visitors are simulated with effects you choose, so you can see whether
# the analysis recovers them. Nothing here describes a real shop.

library(shiny)
library(ggplot2)

for (file in list.files(file.path("..", "R"), pattern = "\\.R$", full.names = TRUE)) {
  if (basename(file) != "bayesian.R") source(file) # rstanarm is not needed in the app
}

pct <- function(x, digits = 2) sprintf(paste0("%.", digits, "f %%"), 100 * x)
pp <- function(x, digits = 2) sprintf(paste0("%+.", digits, "f pp"), 100 * x)
interval <- function(est, low, high, fmt = pp) {
  sprintf("%s (%s to %s)", fmt(est), fmt(low), fmt(high))
}
eur <- function(x) sprintf("%+.2f", x)
blue <- "#2b6cb0"
red <- "#c53030"

plan_panel <- sidebarLayout(
  sidebarPanel(
    numericInput("base_rate", "Current conversion rate (%)", 3, 0.1, 50, 0.1),
    numericInput("mde", "Smallest lift worth detecting (pp)", 0.5, 0.05, 10, 0.05),
    sliderInput("power", "Power", 0.5, 0.95, 0.8, 0.05),
    sliderInput("alpha", "Significance level", 0.01, 0.10, 0.05, 0.01),
    numericInput("traffic", "Checkout visitors per day", 5714, 100, 1e6, 100),
    sliderInput("share", "Share of traffic in the test", 0.1, 1, 1, 0.1)
  ),
  mainPanel(
    h4(textOutput("plan_n")),
    h4(textOutput("plan_days")),
    p("Fix this before the data arrive. Changing the sample size after looking at",
      "results is a form of peeking (tab 3)."),
    plotOutput("plan_curve", height = 320)
  )
)

run_panel <- sidebarLayout(
  sidebarPanel(
    numericInput("n_visitors", "Visitors", 80000, 2000, 400000, 2000),
    numericInput("lift_mobile", "Lift setting, mobile (pp)", 0.7, -2, 5, 0.1),
    numericInput("lift_desktop", "Lift setting, desktop (pp)", 0, -2, 5, 0.1),
    numericInput("aov_change", "True change in order value (EUR)", -2.5, -20, 20, 0.5),
    numericInput("seed", "Random seed", 2026, 1, 1e6, 1),
    actionButton("run", "Run the experiment")
  ),
  mainPanel(
    uiOutput("srm"),
    tableOutput("overall"),
    h4("By device"),
    tableOutput("segments"),
    plotOutput("segment_plot", height = 260),
    p("The red cross is the true average lift in each segment. The simulator scales the",
      "lift you enter by each visitor's baseline conversion, so it differs from the input.",
      "With 80,000 visitors the desktop interval is wide: 'no significant effect' is not",
      "the same as 'no effect'.")
  )
)

peek_panel <- sidebarLayout(
  sidebarPanel(
    sliderInput("sims", "Simulated A/A tests", 200, 2000, 500, 100),
    sliderInput("days", "Test length (days)", 7, 28, 14, 7),
    actionButton("peek", "Simulate")
  ),
  mainPanel(
    tableOutput("peek_summary"),
    plotOutput("peek_plot", height = 300),
    p("In an A/A test both arms are identical, so every 'winner' is a false positive.",
      "Stopping at the first p < 0.05 inflates the error well above 5 %.")
  )
)

check_panel <- tagList(
  h4("What I would verify before trusting this analysis, mine or an AI assistant's"),
  tags$ol(
    tags$li("Sample ratio first: if the split is off, stop; logging or assignment is broken."),
    tags$li("Recompute the headline by hand: conversions / visitors per arm, then the difference."),
    tags$li("Check that the sample size was fixed in advance and the test ran full weeks."),
    tags$li("Look at the guardrail (order value) as well as the primary metric."),
    tags$li("Treat segment results found after the fact as hypotheses for a new test."),
    tags$li("A person decides the rollout; the test reduces uncertainty, it does not remove it.")
  )
)

ui <- fluidPage(
  titlePanel("Checkout experiment: plan it, read it, don't peek"),
  p("Simulated online shop. Set the true effect, run the experiment and check whether",
    "the analysis finds it. The functions come from the tested R/ folder of this repository."),
  tabsetPanel(
    tabPanel("1 Plan", plan_panel),
    tabPanel("2 Run and read", run_panel),
    tabPanel("3 The cost of peeking", peek_panel),
    tabPanel("4 How to check this", check_panel)
  )
)

server <- function(input, output, session) {
  plan <- reactive({
    base <- input$base_rate / 100
    mde <- input$mde / 100
    validate(need(base + mde < 1, "Base rate plus lift must stay below 100 %."))
    n <- required_sample_size(base, mde, input$power, input$alpha)
    list(n = n, days = days_needed(n, input$traffic, input$share))
  })
  output$plan_n <- renderText(
    sprintf("Visitors needed: %s per arm", format(plan()$n, big.mark = ","))
  )
  output$plan_days <- renderText(
    sprintf("Test length: %d days (rounded up to whole weeks)", plan()$days)
  )
  output$plan_curve <- renderPlot({
    base <- input$base_rate / 100
    grid <- seq(max(0.1, input$mde / 4), input$mde * 3, length.out = 30) / 100
    grid <- grid[base + grid < 1]
    n <- vapply(grid, function(m) {
      required_sample_size(base, m, input$power, input$alpha)
    }, numeric(1))
    ggplot(data.frame(mde = grid * 100, n = n), aes(mde, n)) +
      geom_line(colour = blue, linewidth = 1) +
      geom_vline(xintercept = input$mde, linetype = 2) +
      scale_y_log10(labels = scales::comma) +
      labs(x = "Smallest lift worth detecting (pp)", y = "Visitors per arm (log scale)") +
      theme_minimal(base_size = 13)
  })

  experiment <- eventReactive(input$run, {
    simulate_experiment(
      n_visitors = input$n_visitors, seed = input$seed,
      lift_mobile = input$lift_mobile / 100, lift_desktop = input$lift_desktop / 100,
      aov_change = input$aov_change
    )
  }, ignoreNULL = FALSE)

  output$srm <- renderUI({
    s <- srm_check(experiment()$variant)
    msg <- sprintf(
      "Sample ratio check: A %s, B %s visitors, p = %.3f.",
      format(s$counts[1], big.mark = ","), format(s$counts[2], big.mark = ","), s$p_value
    )
    if (s$mismatch) {
      div(class = "alert alert-danger", msg, " Stop: do not read the results.")
    } else {
      div(class = "alert alert-success", msg, " The split looks as planned.")
    }
  })
  output$overall <- renderTable({
    d <- experiment()
    a <- d$variant == "A"
    conv <- two_proportion_test(sum(d$converted[a]), sum(a), sum(d$converted[!a]), sum(!a))
    rev <- mean_difference(d$revenue, d$variant)
    buyers <- d$converted == 1
    aov <- mean_difference(d$revenue[buyers], d$variant[buyers])
    data.frame(
      Metric = c("Conversion (primary)", "Revenue per visitor", "Order value (guardrail)"),
      A = c(pct(conv$rate_a), sprintf("EUR %.2f", rev$mean_a), sprintf("EUR %.2f", aov$mean_a)),
      B = c(pct(conv$rate_b), sprintf("EUR %.2f", rev$mean_b), sprintf("EUR %.2f", aov$mean_b)),
      `B minus A (95 % interval)` = c(
        paste0(interval(conv$diff, conv$ci_low, conv$ci_high), sprintf(", p = %.3g", conv$p_value)),
        interval(rev$diff, rev$ci_low, rev$ci_high, eur),
        interval(aov$diff, aov$ci_low, aov$ci_high, eur)
      ),
      check.names = FALSE
    )
  })
  segments <- reactive(segment_table(experiment(), "device"))
  output$segments <- renderTable({
    s <- segments()
    data.frame(
      Device = s$segment, Visitors = format(s$visitors, big.mark = ","),
      A = pct(s$rate_a), B = pct(s$rate_b),
      `B minus A (95 % interval)` = interval(s$diff, s$ci_low, s$ci_high),
      check.names = FALSE
    )
  })
  output$segment_plot <- renderPlot({
    s <- segments()
    truth <- true_segment_lift(
      experiment(), lift_mobile = input$lift_mobile / 100, lift_desktop = input$lift_desktop / 100
    )
    truth$lift <- 100 * truth$true_lift
    ggplot(s, aes(x = 100 * diff, y = segment)) +
      geom_vline(xintercept = 0, colour = "grey60") +
      geom_errorbarh(aes(xmin = 100 * ci_low, xmax = 100 * ci_high), height = 0.15, colour = blue) +
      geom_point(size = 3, colour = blue) +
      geom_point(data = truth, aes(x = lift, y = segment), shape = 4, size = 4, colour = red) +
      labs(x = "Conversion lift, pp (dot and 95 % interval; red cross = true lift)", y = NULL) +
      theme_minimal(base_size = 13)
  })

  peeking <- eventReactive(input$peek, {
    peeking_simulation(n_sims = input$sims, days = input$days, seed = 1)
  }, ignoreNULL = FALSE)
  output$peek_summary <- renderTable({
    s <- peeking()$summary
    data.frame(
      Habit = s$strategy, `False winners` = pct(s$false_positive_rate, 1),
      check.names = FALSE
    )
  })
  output$peek_plot <- renderPlot({
    ggplot(peeking()$by_day, aes(day, false_positive_rate)) +
      geom_hline(yintercept = 0.05, linetype = 2) +
      geom_line(colour = red, linewidth = 1) +
      scale_y_continuous(labels = scales::percent) +
      labs(x = "Day of the test", y = "Share of A/A tests declared a winner so far") +
      theme_minimal(base_size = 13)
  })
}

shinyApp(ui, server)

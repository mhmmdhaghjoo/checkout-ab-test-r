test_that("the Shiny app computes a plan and reads a simulated experiment", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("ggplot2")
  app_dir <- test_path("..", "..", "app")
  app <- shiny::shinyAppDir(app_dir)
  shiny::testServer(app, {
    session$setInputs(base_rate = 3, mde = 0.5, power = 0.8, alpha = 0.05,
                      traffic = 5714, share = 1)
    expect_equal(plan()$n, required_sample_size(0.03, 0.005))
    expect_equal(plan()$days %% 7, 0)
    session$setInputs(n_visitors = 20000, lift_mobile = 0.7, lift_desktop = 0,
                      aov_change = -2.5, seed = 1, run = 1)
    expect_equal(nrow(experiment()), 20000)
    expect_setequal(segments()$segment, c("desktop", "mobile"))
  })
})

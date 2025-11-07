readRenviron(".Renviron")
shiny::runApp(
  appDir = "frontend/app.R",
  port = 3838,
  host = "0.0.0.0",
  launch.browser = TRUE,
)
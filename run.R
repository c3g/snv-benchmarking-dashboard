readRenviron(".Renviron")
shiny::runApp(
  appDir = "frontend",
  port = 3838,
  host = "0.0.0.0",
  launch.browser = TRUE,
)
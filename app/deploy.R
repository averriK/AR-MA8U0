  # deploy.R
  APP_ID <- Sys.getenv("APP_ID")
  APP_NAME <- Sys.getenv("APP_NAME")
  rsconnect::deployApp(
    appDir = ".",
    appName = Sys.getenv("APP_ID"),
    appTitle = Sys.getenv("APP_NAME"),
    forceUpdate = TRUE,
    account = "averri",
    server = "shinyapps.io",
    appMode = "shiny",
    logLevel="verbose"
  ) 

# app.R
Sys.setenv(APP_ID = "ARMA8U0") # gmdb, kashima

APP_ID <- Sys.getenv("APP_ID")
Sys.setenv(APP_NAME = "ProjectID: AR-MA8U0")
Sys.setenv(EMAIL = "argentina@srk.com.ar")
Sys.setenv(ORGANIZATION = "SRK Consulting (Argentina)")
Sys.setenv(WEBSITE = "https://www.srk.com")
Sys.setenv(LOGO = "logo_srk_long.png")
Sys.setenv(LOGO_SIZE = "35px")
Sys.setenv(VERSION = "Version v0.025")
Sys.setenv(AUTHOR = "Authors: A. Verri, F. Garate, C. De Los Hoyos")


source("global.R")
source("setup.R")
source("app/dataset.R")
source("app/ui.R")
source("app/server.R")

# Run the selected Shiny app
shinyApp(ui = ui, server = server)

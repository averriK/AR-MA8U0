# credits.R

APP_NAME <- Sys.getenv("APP_NAME")
EMAIL <- Sys.getenv("EMAIL")
AUTHOR <- Sys.getenv("AUTHOR")
VERSION <- Sys.getenv("VERSION")
ORGANIZATION <- Sys.getenv("ORGANIZATION")
WEBSITE <- Sys.getenv("WEBSITE")
LOGO <-  Sys.getenv("LOGO")#"logo_srk_long.png"
LOGO_SIZE <-  Sys.getenv("LOGO_SIZE")#"35px"

CREDITS <- div(
  style = "padding: 10px; text-align: left; color: grey; font-size: small;",
  HTML(paste0(
    "<span class='app-name-id'>", APP_NAME, "</span>", "<br>",VERSION, " © 2024 ", "<br>",
    AUTHOR, " - ","<a href='mailto:", EMAIL, "'><i class='fas fa-envelope'></i></a>", "<br>",
    ORGANIZATION, " - ","<a href=",WEBSITE," target='_blank'><i class='fas fa-globe'></i></a><br>"  # Link to the website with globe icon
    
  ))
)


LOGO <- div(
  style = "padding: 10px; text-align: left; color: grey; font-size: small;",
  HTML(paste0(
    "<img src='", LOGO, "' alt='Logo' style='height:", LOGO_SIZE, "; margin-top: 5px;'>" # Logo added here
  ))
)

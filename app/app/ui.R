
source("R/footer.R")
source("R/header.R")
source("R/credits.R")

# global vars ----
BOOTSWATCH_THEME <- bslib::bs_theme(
  version = 5,
  bootswatch = "cyborg",    # Choose a valid Bootswatch theme
  base_font = c("Arial", "sans-serif")
) #"cyborg" #"darkly"  # spacelab, superhero,cosmo,simplex,flatly,cerulean,slate,yeti,lumen,sandstone,united,cyborg,darkly,journal
BUTTON_STYLE <- "stretch" #simple, bordered, minimal, stretch, jelly, gradient, fill, material-circle, material-flat, pill, float, united

# UI ----
ui <- bslib::page_navbar(
  
  id = "navbar",  # ID for capturing the active navigation panel
  theme = BOOTSWATCH_THEME,
  # Sidebar content
  sidebar = bslib::sidebar(
    width=400,
    
    open = "desktop",
    m1.sidebar("m1"),
    # uiOutput("sidebarUI"),
    div(style = "flex-grow: 1;"), # Spacer to push footer to the bottom
    LOGO,
    CREDITS,
    ),
  # Header
  header = HEADER,
  
  # ),
  m1.ui(id = "m1"),
 
  # Footer
  footer = FOOTER
  
)



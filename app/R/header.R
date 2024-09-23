# header.R
CONSOLE_FONT <- "Fira Code"
HEADER <- tagList(
  tags$head(
    # Include the Google Fonts link for Fira Code
    tags$link(rel = "stylesheet", href = "https://fonts.googleapis.com/css2?family=Fira+Code:wght@400;500;700&display=swap"),
    
    tags$style(HTML("
        .header-custom {
          padding-top: 0; /* Remove padding at the top */
          margin-top: 0;  /* Remove margin at the top */
        }
        .custom-render-print {
         font-family: 'Fira Code', monospace !important;
          font-size: 16px; /* Example font size */
          color: lightgreen; /* Example text color */
        }
      "))
  )
)
# font-family: 'Courier New', Courier, monospace; /* Example font */
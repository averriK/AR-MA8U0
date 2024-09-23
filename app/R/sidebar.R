
RSN.bar <- function(id) {
  ns <- NS(id)
  tagList(
    fluidRow(
      column(
        width = 4,
        actionBttn(
          inputId= ns("include"),
          label = "Include",
          icon = NULL,
          style = BUTTON_STYLE,
          color = "success",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        )
      ),#Column
      column(
        width = 4,
        actionBttn(
          inputId= ns("exclude"),
          label = "Remove",
          icon = NULL,
          style = BUTTON_STYLE,
          color = "primary",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        )
      ),#Column
      
      column(
        width = 4,
        actionBttn(
          inputId= ns("reset"),
          label = "Reset",
          icon = NULL,
          style = BUTTON_STYLE,
          color = "royal",
          size = "sm",
          block = FALSE,
          no_outline = TRUE
        )
      )#Column
      
    ),#fluidRow
  )
}
















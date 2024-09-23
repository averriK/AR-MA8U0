

server <- function(input, output, session) {
  source("R/sidebar.R")
  
  # output$sidebarUI <- renderUI({
  #   
  #   accordion(
  #     id="accordion",
  #     multiple = FALSE,
  #     open=FALSE,
  #     
  #     accordion_panel(
  #       title="WQ",
  #       value = "WQ",
  #       m1.sidebar("m1")
  #     )
  #     
  #     
  #     
  #   )
  # })

  observe( {
    # browser()
    
    if (input$navbar== "WQ") {
      accordion_panel_open(session=session, id="accordion",values ="WQ")
    }
    
  }) |> bindEvent(input$navbar)
  
  #
  m1.server("m1")
  
}



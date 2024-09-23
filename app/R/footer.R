

FOOTER <- tagList(
  tags$head(
    tags$style(HTML("
      .footer-custom {
        position: fixed;
        bottom: 0;
        right: 0;
        width: calc(100% - 400px); 
        padding: 0px;
        background-color: rgba(0, 0, 0, 0.5); 
        display: flex;
        justify-content: flex-end; /* Align logos/images to the right */
        align-items: center;
        font-size: small;
        color: grey;
      }
      
    "))
  )
)


library(shiny)
source("funcs.R")

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h1 { color: #ff0000; }
      body { background-color: black; }
      .checkbox label {
        color: #ff0000;
        font-size: 16px;
      }
    "))
  ),
  htmlOutput("curr_word")
)

server <- function(input, output, session) {
  
  # Session-specifik tilstand
  state <- reactiveValues(
    ord = NULL,
    target_ord = NULL
  )
  
  # Initialiser tilstanden for denne session
  set_target_ord(state, "TVANGSHYGGE")
  gen_start_ord(state, "TVANGSHYGGE")
  
  output$curr_word <- renderUI({
    update_ord(state)
    invalidateLater(400, session)
    HTML(paste0(
      '<h1 style="color: #ff0000; font-family: monospace;">',
      saml_ord(state),
      '</h1>'
    ))
  })
}

shinyApp(ui = ui, server = server)

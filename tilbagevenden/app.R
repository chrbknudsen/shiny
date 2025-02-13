#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
source("funcs.R")


# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h1 { color: #ff0000; }
      body { background-color: black; }
      .checkbox label {
        color: #ff0000;
        font-size: 16px;
      }
    "))),
  
  
  # Application title
  
  htmlOutput("curr_word")
  
  
)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
  set_target_ord("TVANGSHYGGE")
  gen_start_ord()
    output$curr_word <- renderUI({
      update_ord()
      invalidateLater(400, session)
      HTML(paste('<h1 style="color: ##ff0000; font-family: monospace;">',saml_ord()),'</h2>')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

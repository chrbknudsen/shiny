#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Old Faithful Geyser Data - eller, nej..."),
    textOutput("curr_down")
    

)

# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$curr_down <- renderText({
      invalidateLater(1000, session)
      paste("The current time is", Sys.time())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

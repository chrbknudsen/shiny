#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



library(shiny)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel(title = "The Peoples Roman Numeral Converter",
               windowTitle = "Roman Converter"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("roman",
                        "Roman Numeral:",
                        value = "MM")
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("arabic")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$arabic <- renderText({
      as.numeric(as.roman(input$roman))        

    })

}

# Run the application 
shinyApp(ui = ui, server = server)

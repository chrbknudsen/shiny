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
   

        # Show a plot of the generated distribution
        mainPanel(
          textInput("roman",
                    "Roman Numeral:",
                    value = "MM"),
           textOutput("arabic")
        )
    )


# Define server logic required to draw a histogram
server <- function(input, output) {
 
    output$arabic <- renderText({
      res <- gsub(" ", "", input$roman)
      res <- as.roman(res)
      if(is.na(res)){
        res <- "Invalid input. This app can only handle roman numerals between 1 and 3899"
      } 
      res        

    })

}

# Run the application 
shinyApp(ui = ui, server = server)

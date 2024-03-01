#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    https://shiny.posit.co/
#

library(shiny)
library(lubridate)

to_binary_string <- function(number) {
  if(number == 0) {
    return('0')
  }
  
  binary_string <- ""
  
  while(number > 0) {
    binary_string <- paste0(number %% 2, binary_string)
    number <- number %/% 2
  }
  
  return(binary_string)
}


# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("leaving ground"),
    textOutput("curr_down")
    

)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$curr_down <- renderText({
      invalidateLater(1000, session)
      curr_time <- with_tz(now("UTC"), tzone = "Europe/Copenhagen")
      target_time <-  as.POSIXct("2038-06-30 23:59:59", tz = "Europe/Copenhagen")
      diff <- floor(as.numeric(difftime(target_time, curr_time, units = "secs")))
      if(diff>0){
        paste(toBinaryString(diff))
      }else{
        paste("Done!")
      }
      
      
      
      
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

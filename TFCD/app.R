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
library(KnudsenKit)


# skaber svg'er af 0 og 1

svg0 <- segment7(0)
svg1 <- segment7(1)

start <- '<style>
  .svg-container {
    display: flex;
  }

svg {
  width: 100px;
  height: auto;
  margin: 0px;
  padding: 0px;
}

.svg-container svg:last-child {
  margin-right: 0;
}
</style>
  
  <div class="svg-container">'

slut <- '</div>'

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

to_svgs <- function(number){
  temp <- to_binary_string(number)
  temp <- unlist(strsplit(temp, ""))
  temp <- paste0(ifelse(temp == 0, svg0, svg1), collapse = " ")
}

# Define UI for application that draws a histogram
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h2 { color: #00FF00; }
      body { background-color: black; }
    ")),
    # favicons
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
    ),
    
  
    # Application title
    titlePanel("leaving ground"),
    htmlOutput("curr_down")
    

)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
    output$curr_down <- renderUI({
      invalidateLater(1000, session)
      curr_time <- with_tz(now("UTC"), tzone = "Europe/Copenhagen")
      target_time <-  as.POSIXct("2038-06-30 23:59:59", tz = "Europe/Copenhagen")
      diff <- floor(as.numeric(difftime(target_time, curr_time, units = "secs")))
      if(diff>0){
        HTML(paste0(start, to_svgs(diff), slut, collapse = " "))
      }else{
        HTML(paste("Done!"))
      }
      
      
      
      
    })
}








# Run the application 
shinyApp(ui = ui, server = server)

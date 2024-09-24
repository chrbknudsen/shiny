

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

# Funktion til justering af enhed
unit_label <- function(value, singular, plural) {
  if (value == 1) {
    return(sprintf("%d %s", value, singular))
  } else {
    return(sprintf("%d %s", value, plural))
  }
}

to_text_string <- function(duration){
  aar <- floor(as.numeric(as.period(duration)@year))
  mon <- floor(as.numeric(as.period(duration)@month))
  day <- floor(as.numeric(as.period(duration)@day))
  hou <- floor(as.numeric(as.period(duration)@hour))
  min <- floor(as.numeric(as.period(duration)@minute))
  sec <- floor(as.numeric(as.period(duration)@.Data))
  result <- paste(
    if (aar > 0) unit_label(aar, "år", "år") else "",
    if (mon > 0) unit_label(mon, "måned", "måneder") else "",
    if (day > 0) unit_label(day, "dag", "dage") else "",
    if (hou > 0) unit_label(hou, "time", "timer") else "",
    if (min > 0) unit_label(min, "minut", "minutter") else "",
    if (sec > 0) unit_label(sec, "sekund", "sekunder") else "",
    sep = " "
  )
  result <- gsub(", ,", ",", result)
  result <- gsub("^, |, $", "", result)
  result
}

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

# Define UI for application 
ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h2 { color: #00FF00; }
      body { background-color: black; }
      .checkbox label {
        color: #00FF00;
        font-size: 16px;
      }
      .checkbox input[type='checkbox'] {
        color: #00FF00;
        
      }
    ")),
    # favicons
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png"),
    ),
    
  
    # Application title
    titlePanel("leaving ground"),
    checkboxInput("toggle", 'Binær', value = TRUE),
    htmlOutput("curr_down")
    

)



# Define server logic
server <- function(input, output, session) {
    output$curr_down <- renderUI({
      invalidateLater(1000, session)
      curr_time <- with_tz(now("UTC"), tzone = "Europe/Copenhagen")
      target_time <-  as.POSIXct("2038-06-30 23:59:59", tz = "Europe/Copenhagen")
      duration <- interval(curr_time, target_time)
      sekunder <- floor(as.numeric(duration, "seconds"))
      if(sekunder>0 & input$toggle){
        HTML(paste0(start, to_svgs(sekunder), slut, collapse = " "))
      }else if(sekunder>0 & !input$toggle){
        HTML(paste('<h2 style="color: #00FF00;">',to_text_string(duration),'</h2>'))
      }else{
        HTML(paste('<h2 style="color: #00FF00;">Done!</h2>'))
      }
      
      
      
      
    })
}








# Run the application 
shinyApp(ui = ui, server = server)

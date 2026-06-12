library(tidyverse)




library(shiny)
library(lubridate)

source("data.R")


server <- function(input, output, session) {
  output$curr_down <- renderText({
    invalidateLater(1000, session)
    
    curr_time <- with_tz(now("UTC"), tzone = "Europe/Copenhagen")
    curr_time <- paste0(hour(curr_time), minute(curr_time))
    data[data$tid == curr_time,]$begivenhed
    })
}


library(tidyverse)
data <- read_csv("OTD/data.csv")


today()

noget <- function(date, data){
  data %>% 
    filter(day == as.integer(day(date)),
           month == as.integer(month(date)))
}


noget(today(), data)

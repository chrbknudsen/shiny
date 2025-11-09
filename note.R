library(tidyverse)
library(httr)
library(leaflet)

svar <- GET("https://letparkeringapi.azurewebsites.net/api/ParkingSpotOverview")
svar <- svar |> content() |> tibble() 
svar <- svar  |>  unnest_wider(`content(svar)`) 

svar <- svar %>% mutate(latitude = as.numeric(latitude), longitude = as.numeric(longitude))


pal <- colorFactor(
  palette = c("green", "red", "blue"),  # du kan ændre farverne
  domain = svar$status
)
svar %>% 
leaflet() |>
  addTiles() |>
  addCircleMarkers(
    lng = ~longitude,
    lat = ~latitude,
    color = ~pal(status),
    fillOpacity = 0.8,
    radius = 6,
    popup = ~paste("Status:", status)
  )

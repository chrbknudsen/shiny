library(shiny)
library(tidyverse)
library(httr)
library(leaflet)

ui <- fluidPage(
  titlePanel("P-pladser i Vejle"),
  uiOutput("subtitle"),  # her viser vi tidspunktet
  leafletOutput("map", height = 600)
)

server <- function(input, output, session) {
  # Hent data
  svar <- GET("https://letparkeringapi.azurewebsites.net/api/ParkingSpotOverview")
  tid  <- svar$headers$date |> as.character()   # gem tidspunkt som tekst
  svar <- svar |> content() |> tibble()
  svar <- svar |> unnest_wider(`content(svar)`)

  # Sørg for numeriske coords
  places <- svar |> mutate(
    latitude  = as.numeric(latitude),
    longitude = as.numeric(longitude)
  ) |> filter(!is.na(latitude), !is.na(longitude))

  # Farveskala på status
  pal <- colorFactor(
    palette = c("green", "red"),
    domain  = places$status
  )

  # Underskrift med tidspunkt
  output$subtitle <- renderUI({
    tags$p(
      paste("Sidst opdateret:", tid),
      style = "margin-top:-10px; color:#555; font-style:italic;"
    )
  })

  # Leafletkort
  output$map <- renderLeaflet({
    m <- leaflet(places) |>
      addTiles() |>
      addCircleMarkers(
        lng = ~longitude,
        lat = ~latitude,
        color = ~pal(status),
        fillOpacity = 0.8,
        radius = 6,
        popup = ~paste("Status:", status)
      )

    # Zoom til data hvis muligt, ellers til Vejle
    if (nrow(places) > 0) {
      m |> fitBounds(
        lng1 = min(places$longitude, na.rm = TRUE),
        lat1 = min(places$latitude,  na.rm = TRUE),
        lng2 = max(places$longitude, na.rm = TRUE),
        lat2 = max(places$latitude,  na.rm = TRUE)
      )
    } else {
      m |> setView(lng = 9.535, lat = 55.711, zoom = 13)
    }
  })
}

shinyApp(ui, server)

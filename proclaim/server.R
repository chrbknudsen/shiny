library(shiny)
library(leaflet)

shinyServer(function(input, output) {
  
  output$lat <- renderPrint({
    input$lat
  })
  
  output$long <- renderPrint({
    input$long
  })
  
  output$geolocation <- renderPrint({
    input$geolocation
  })
  
  output$mymap <- renderLeaflet({
    leaflet() %>%
      addTiles() %>%
      setView(lng = -84.3880, lat = 33.7490, zoom = 5) %>%  # Eksempel koordinater for Atlanta, GA
      addCircles(lng = -84.3880, lat = 33.7490, radius = 804672, # 500 miles i meter
                 color = "red", fillOpacity = 0.5, weight = 1) %>%
      addCircles(lng = -84.3880, lat = 33.7490, radius = 1609344, # 1000 miles i meter
                 color = "blue", fillOpacity = 0.5, weight = 1)
  })
  
})
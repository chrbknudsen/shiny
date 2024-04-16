library(shiny)
library(leaflet)

def_lat <- 55.69748
def_long <- 12.56017

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
    if(is.null(input$lat)){lat <- def_lat} else {lat <- input$lat}
    if(is.null(input$long)){long <- def_long} else {long <- input$long}
    leaflet() %>%
      addTiles() %>%
      setView(lng = long, lat = lat, zoom = 3) %>%  # Eksempel koordinater for Atlanta, GA
      addCircles(lng = long, lat = lat, radius = 804672, # 500 miles i meter
                 color = "red", fillOpacity = 0.5, weight = 1) %>%
      addCircles(lng = long, lat = lat, radius = 1609344, # 1000 miles i meter
                 color = "blue", fillOpacity = 0.5, weight = 1) %>%
      addLegend("bottomright", colors = c("red", "blue"), labels = c("500 Miles", "500 more"),
              title = "I would walk")
  })


})
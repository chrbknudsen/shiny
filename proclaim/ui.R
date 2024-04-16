library(shiny)
library(leaflet)

shinyUI(fluidPage(
  
  titlePanel("I would walk 804 kilometers and 672 meters.",
             windowTitle = "Proclaim!"),
  
  tags$script('
      $(document).ready(function () {
        navigator.geolocation.getCurrentPosition(onSuccess, onError);
              
        function onError (err) {
          Shiny.onInputChange("geolocation", false);
        }
              
        function onSuccess (position) {
          setTimeout(function () {
            var coords = position.coords;
            console.log(coords.latitude + ", " + coords.longitude);
            Shiny.onInputChange("geolocation", true);
            Shiny.onInputChange("lat", coords.latitude);
            Shiny.onInputChange("long", coords.longitude);
          }, 1100)
        }
      });
              '),
  
  # Show a plot of the generated distribution
  leafletOutput("mymap")
)
)
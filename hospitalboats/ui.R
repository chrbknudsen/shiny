library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("USNS Comfort & Mercy"),

  # 🔹 Tekstblok under titel
  fluidRow(
    column(
      width = 12,
      textOutput("min_distance_text"),
      br()
    )
  ),

  fluidRow(
    column(
      width = 8,
      leafletOutput("map", height = "750px")
    ),
    column(
      width = 4,
      h4("Distance to Greenland coast (km)"),
      tableOutput("dist_table"),

      hr(),

      sliderInput(
        "refresh_sec",
        "Auto refresh (seconds)",
        min = 30, max = 900, value = 300, step = 30
      ),

      actionButton("refresh_now", "Refresh now")
    )
  )
)
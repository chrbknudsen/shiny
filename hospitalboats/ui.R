library(shiny)
library(leaflet)

ui <- fluidPage(

  titlePanel("USNS Comfort & Mercy"),

  # 🔹 Lille kodeblok under titel
  fluidRow(
    column(
      width = 12,
      tags$div(
        style = "
          background-color:#f5f5f5;
          border:1px solid #ddd;
          border-radius:6px;
          padding:10px;
          font-family:monospace;
          margin-bottom:15px;
        ",
        verbatimTextOutput("min_distance_block")
      )
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
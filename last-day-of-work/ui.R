library(shiny)
library(shinyjs)
fluidPage(
  useShinyjs(),
  titlePanel("6 outputs med automatisk funktion på step 4"),
  sidebarLayout(
    sidebarPanel(
      h3("Trin vis eksekvering"),
      p("Her vil vi gerne prøve at lave en app"),
      p("hvor vi klikker os gennem beregningerne."),
      p("Normalt ville vi lave et markdown eller quarto"),
      p("dokument, men her klikker vi os gennem."),
      actionButton("knap", "Næste trin"),
    ),
    mainPanel(
      uiOutput("output_tekst"),
      plotOutput("output_plot"),
      tableOutput("output_tabel"),
      uiOutput("output_billede")
    )
  )
)
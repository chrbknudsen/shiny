library(shiny)
"<h4>Her er inline math: \\( \\mu = 5 \\)</h4>"

ui <- fluidPage(

  
  
  tags$head(tags$script(src = "https://cdn.jsdelivr.net/npm/mathjax@3/es5/tex-mml-chtml.js")),
  withMathJax(),
  uiOutput("latex")
)

server <- function(input, output, session) {
  output$latex <- renderUI({
    withMathJax(HTML("<h4>Her er inline math: \\( \\mu = 5 \\)</h4>"))
  })
}

shinyApp(ui, server)

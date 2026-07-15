library(shiny)

library(stringr)
library(rvest)


server <- function(input, output, session){
  SR <- "https://kub.kb.dk/systematiske-reviews" |> 
rvest::read_html() |> 
  rvest::html_text() |> 
  stringr::str_detect("KUB Systematic")
  
  
datalab <- "https://kub.kb.dk/datalab" |> 
rvest::read_html() |> 
  rvest::html_text() |> 
  stringr::str_detect("KUB")


datalab <- if_else(datalab, "Datalab: Ja", "Datalab: Nej")
SR <- if_else(SR, "Systematic Review Service: Ja", "Systematic Review Service: Nej")


  output$datalab <- renderText({datalab})
  output$SR <- renderText({SR})
  output$test <- renderUI({HTML("<span style='color: blue; font-size: 1.5em;'>Tekst i farver!</span>")})
}
  
  

ui <- fluidPage(
  titlePanel("Baseret på evidens, må det hedde 'KUB'?"),
  textOutput("datalab"),
  textOutput("SR")
  
)

shinyApp(ui, server)
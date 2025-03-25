library(shiny)
library(readr)
library(dplyr)

# LÃ¦s data
df <- read_csv("example.csv") |> 
  mutate(across(everything(), ~replace_na(., "")))

ui <- fluidPage(
  titlePanel("BeslutningsstÃ¸tte"),
  uiOutput("question_ui"),
  br(),
  fluidRow(
    column(6, actionButton("back", "â¬ GÃ¥ tilbage")),
    column(6, actionButton("reset", "ð Start forfra"))
  ),
  br(),
  textOutput("recommendation")
)

server <- function(input, output, session) {
  current_id <- reactiveVal("Q1")          # hvor vi er nu
  history <- reactiveVal(character())      # gemmer tidligere noder
  
  observeEvent(input$reset, {
    current_id("Q1")
    history(character())
    output$recommendation <- renderText({ "" })
  })
  
  observeEvent(input$back, {
    h <- history()
    if (length(h) >= 1) {
      current_id(tail(h, 1))
      history(head(h, -1))
      output$recommendation <- renderText({ "" })
    }
  })
  
  output$question_ui <- renderUI({
    q_rows <- df %>% filter(id == current_id())
    if (nrow(q_rows) == 0) return(NULL)
    
    tagList(
      h4(q_rows$spørgsmål[1]),
      lapply(seq_len(nrow(q_rows)), function(i) {
        ans_id <- paste0("answer_", current_id(), "_", i)
        actionButton(ans_id, q_rows$svar[i], class = "btn-primary m-1")
      })
    )
  })
  
  # Dynamisk observer for hver mulig svar-knap
  observe({
    q_rows <- df %>% filter(id == current_id())
    lapply(seq_len(nrow(q_rows)), function(i) {
      ans_id <- paste0("answer_", current_id(), "_", i)
      observeEvent(input[[ans_id]], {
        chosen <- q_rows[i, ]
        
        # opdater historik
        history(c(history(), current_id()))
        
        if (chosen$anbefaling != "") {
          output$recommendation <- renderText({ chosen$anbefaling })
        } else {
          output$recommendation <- renderText({ "" })
          current_id(chosen$next_id)
        }
      })
    })
  })
}

shinyApp(ui, server)


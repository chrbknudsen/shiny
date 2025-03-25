library(shiny)
library(readr)
library(dplyr)
library(readxl)
# Læs data
df <-read_excel("data.xlsx") |> 
  mutate(across(everything(), ~replace_na(., "")))

ui <- fluidPage(
  titlePanel("Beslutningsstøtte"),
  uiOutput("question_ui"),
  br(),
  fluidRow(
    column(6, actionButton("back", "⬅ Gå tilbage")),
    column(6, actionButton("reset", "🔄 Start forfra"))
  ),
  br(),
  textOutput("recommendation")
)

server <- function(input, output, session) {
  current_id <- reactiveVal("Q1")
  history <- reactiveVal(character())
  
  # Håndtér start forfra
  observeEvent(input$reset, {
    current_id("Q1")
    history(character())
    output$recommendation <- renderText({ "" })
  }, ignoreInit = TRUE)
  
  # Håndtér tilbage
  observeEvent(input$back, {
    h <- history()
    if (length(h) >= 1) {
      current_id(tail(h, 1))
      history(head(h, -1))
      output$recommendation <- renderText({ "" })
    }
  }, ignoreInit = TRUE)
  
  # Dynamisk spørgsmål + knapper
  output$question_ui <- renderUI({
    q_rows <- df %>% filter(id == current_id())
    if (nrow(q_rows) == 0) return(NULL)
    
    tagList(
      h4(q_rows$question[1]),
      lapply(seq_len(nrow(q_rows)), function(i) {
        ans_id <- paste0("answer_", current_id(), "_", i)
        actionButton(ans_id, q_rows$svar[i], class = "btn-primary m-1")
      })
    )
  })
  
  # Dynamisk observering af alle mulige knapper i det aktuelle spørgsmål
  observe({
    q_rows <- df %>% filter(id == current_id())
    lapply(seq_len(nrow(q_rows)), function(i) {
      ans_id <- paste0("answer_", current_id(), "_", i)
      observeEvent(input[[ans_id]], {
        chosen <- q_rows[i, ]
        history(c(history(), current_id()))
        
        if (chosen$anbefaling != "") {
          output$recommendation <- renderText({ chosen$anbefaling })
        } else {
          output$recommendation <- renderText({ "" })
          current_id(chosen$next_id)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
}

shinyApp(ui, server)

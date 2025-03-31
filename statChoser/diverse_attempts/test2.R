library(shiny)
library(readr)
library(dplyr)
library(readxl)

# Læs data
df <- read_excel("data.xlsx") |> 
  mutate(across(everything(), ~replace_na(., "")))

ui <- fluidPage(
  titlePanel("Beslutningsstøtte til valg af statistisk test/metode"),
  
  uiOutput("step_info"),
  uiOutput("question_ui"),
  br(),
  
  fluidRow(
    column(4, actionButton("back", "⬅ Gå tilbage")),
    column(4, actionButton("reset", "🔄 Start forfra"))
  ),
  br(),
  
  h4("Valgt rute indtil nu:"),
  uiOutput("breadcrumb"),
  
  br(),
  h3("Anbefaling:"),
  textOutput("recommendation"),
  p(em("baseret på fundamentals of biostatistics, 7th edition af Bernard Rosner")),
  p(em("bruges på eget ansvar")),
  p(em("sidetal henviser til denne udgave - bemærk at der er en del fejl i sidetallene i den")),
  uiOutput("final_route")
)

server <- function(input, output, session) {
  current_id <- reactiveVal("Q1")
  history <- reactiveVal(character())
  valg_rute <- reactiveVal(data.frame(id = character(), question = character(), svar = character()))
  
  observeEvent(input$reset, {
    current_id("Q1")
    history(character())
    valg_rute(data.frame(id = character(), question = character(), svar = character()))
    output$recommendation <- renderText({ "" })
    output$final_route <- renderUI({ NULL })
  }, ignoreInit = TRUE)
  
  observeEvent(input$back, {
    h <- history()
    if (length(h) >= 1) {
      current_id(tail(h, 1))
      history(head(h, -1))
      rute <- valg_rute()
      if (nrow(rute) > 0) valg_rute(rute[-nrow(rute), ])
      output$recommendation <- renderText({ "" })
      output$final_route <- renderUI({ NULL })
    }
  }, ignoreInit = TRUE)
  
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
  
  output$step_info <- renderUI({
    trin <- nrow(valg_rute()) + 1
    h4(paste0("Trin ", trin))
  })
  
  output$breadcrumb <- renderUI({
    rute <- valg_rute()
    if (nrow(rute) == 0) return("Ingen valg endnu.")
    HTML(paste0(
      apply(rute, 1, function(row) {
        paste0("<b>", row["question"], ":</b> ", row["svar"])
      }),
      collapse = "<br>"
    ))
  })
  
  observe({
    q_rows <- df %>% filter(id == current_id())
    lapply(seq_len(nrow(q_rows)), function(i) {
      ans_id <- paste0("answer_", current_id(), "_", i)
      observeEvent(input[[ans_id]], {
        chosen <- q_rows[i, ]
        
        # Opdater historik og rute
        history(c(history(), current_id()))
        valg_rute(rbind(valg_rute(), data.frame(
          id = chosen$id,
          question = chosen$question,
          svar = chosen$svar
        )))
        
        if (chosen$anbefaling != "") {
          output$recommendation <- renderText({ chosen$anbefaling })
          
          output$final_route <- renderUI({
            HTML(paste0("<hr><h4>Din fulde rute:</h4><ul>",
                        paste0(
                          apply(valg_rute(), 1, function(row) {
                            paste0("<li><b>", row["question"], ":</b> ", row["svar"], "</li>")
                          }),
                          collapse = ""
                        ),
                        "</ul>"
            ))
          })
        } else {
          output$recommendation <- renderText({ "" })
          output$final_route <- renderUI({ NULL })
          current_id(chosen$next_id)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
}

shinyApp(ui, server)


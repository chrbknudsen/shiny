library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# === Brug debug mode til fejlfinding ===
debug <- TRUE

# Læs data
df <- read_excel("data.xlsx") |> 
  mutate(across(everything(), ~replace_na(., ""))) 

ui <- fluidPage(
  titlePanel("Beslutningsstøtte"),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sprog", "Sprog / Language", choices = c("Dansk" = "da", "English" = "en")),
      br(),
      h4("Valgt rute:"),
      uiOutput("breadcrumb")
    ),
    
    mainPanel(
      uiOutput("step_info"),
      uiOutput("question_ui"),
      br(),
      fluidRow(
        column(4, actionButton("back", "⬅ Gå tilbage")),
        column(4, actionButton("reset", "🔄 Start forfra"))
      ),
      br(),
      h3("Anbefaling:"),
      textOutput("recommendation"),
      uiOutput("recommendation_link"),
      p(em("hej")),
      uiOutput("final_route"),
      tags$script(HTML("
        Shiny.addCustomMessageHandler('reset_ui', function(msg) {
          Shiny.unbindAll();
          Shiny.bindAll();
        });
      "))
    )
  )
)

server <- function(input, output, session) {
  current_id <- reactiveVal("Q1")
  history <- reactiveVal(character())
  valg_rute <- reactiveVal(data.frame(id = character(), question = character(), svar = character()))
  
  get_col <- function(base) paste0(base, "_", input$sprog)
  
  observeEvent(input$reset, {
    current_id("Q1")
    history(character())
    valg_rute(data.frame(id = character(), question = character(), svar = character()))
    output$recommendation <- renderText({ "" })
    output$recommendation_link <- renderUI({ NULL })
    output$final_route <- renderUI({ NULL })
  }, ignoreInit = TRUE)
  
  observeEvent(input$back, {
    h <- history()
    if (length(h) >= 1) {
      current_id(h[length(h)])
      history(h[-length(h)])
      
      rute <- valg_rute()
      if (nrow(rute) > 0) valg_rute(rute[-nrow(rute), ])
      
      output$recommendation <- renderText({ "" })
      output$recommendation_link <- renderUI({ NULL })
      output$final_route <- renderUI({ NULL })
      
      session$sendCustomMessage(type = "reset_ui", message = list())
    }
  }, ignoreInit = TRUE)
  
  output$step_info <- renderUI({
    trin <- nrow(valg_rute()) + 1
    h4(paste0("Trin ", trin))
  })
  
  output$question_ui <- renderUI({
    q_rows <- df %>% filter(id == current_id())
    if (nrow(q_rows) == 0) return(NULL)
    
    question_col <- get_col("question")
    svar_col <- get_col("svar")
    
    label_text <- if (debug) {
      paste0("[", q_rows$id[1], "] ", q_rows[[question_col]][1])
    } else {
      q_rows[[question_col]][1]
    }
    
    tagList(
      h4(label_text),
      lapply(seq_len(nrow(q_rows)), function(i) {
        ans_id <- paste0("answer_", current_id(), "_", i)
        actionButton(ans_id, q_rows[[svar_col]][i], class = "btn-primary m-1")
      })
    )
  })
  
  output$breadcrumb <- renderUI({
    rute <- valg_rute()
    if (nrow(rute) == 0) return("Ingen valg endnu.")
    HTML(paste0(
      apply(rute, 1, function(row) {
        paste0("<b>[", row["id"], "] ", row["question"], ":</b> ", row["svar"])
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
        history(c(history(), current_id()))
        
        question_col <- get_col("question")
        svar_col <- get_col("svar")
        anbefaling_col <- get_col("anbefaling")
        
        rute <- valg_rute()
        if (nrow(rute) > 0 && rute[nrow(rute), "id"] == chosen$id) {
          rute[nrow(rute), ] <- list(chosen$id, chosen[[question_col]], chosen[[svar_col]])
        } else {
          rute <- rbind(rute, data.frame(
            id = chosen$id,
            question = chosen[[question_col]],
            svar = chosen[[svar_col]]
          ))
        }
        
        valg_rute(rute)
        
        if (chosen[[anbefaling_col]] != "") {
          output$recommendation <- renderText({ chosen[[anbefaling_col]] })
          
          output$recommendation_link <- renderUI({
            if (chosen$link != "") tags$a(href = chosen$link, target = "_blank", "Link til materiale")
            else NULL
          })
          
          output$final_route <- renderUI({
            HTML(paste0("<hr><h4>Din fulde rute:</h4><ul>",
                        paste0(
                          apply(valg_rute(), 1, function(row) {
                            paste0("<li><b>[", row["id"], "] ", row["question"], ":</b> ", row["svar"], "</li>")
                          }),
                          collapse = ""
                        ),
                        "</ul>"
            ))
          })
        } else {
          output$recommendation <- renderText({ "" })
          output$recommendation_link <- renderUI({ NULL })
          output$final_route <- renderUI({ NULL })
          current_id(chosen$next_id)
        }
      }, ignoreInit = TRUE)
    })
  })
}

shinyApp(ui, server)

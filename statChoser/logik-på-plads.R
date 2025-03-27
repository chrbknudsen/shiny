library(shiny)
library(readr)
library(tidyverse)
library(dplyr)
library(readxl)

# === Brug debug mode til fejlfinding ===
debug <- TRUE

# Indlæs data fra Excel og erstat NA'er med tom tekst
df <- read_excel("data.xlsx") |> 
  mutate(across(everything(), ~replace_na(., "")))

# === UI ===
ui <- fluidPage(
  titlePanel("Beslutningsstøtte"),
  
  # Vis trin-indikator (Trin 1, Trin 2 osv.)
  uiOutput("step_info"),
  
  # Det aktuelle spørgsmål og svarmuligheder
  uiOutput("question_ui"),
  br(),
  
  # Tilbage- og reset-knapper
  fluidRow(
    column(4, actionButton("back", "⬅ Gå tilbage")),
    column(4, actionButton("reset", "🔄 Start forfra"))
  ),
  br(),
  
  # Breadcrumb: oversigt over de valg brugeren har truffet
  h4("Valgt rute indtil nu:"),
  uiOutput("breadcrumb"),
  
  br(),
  # Visning af anbefaling
  h3("Anbefaling:"),
  textOutput("recommendation"),
  p(em("Decision tree from Rosner.")),  # Ekstra note – kan ændres
  p(em("Follow recommendations at your own risk")),
  
  # Fuld opsummering af valgte rute til sidst
  uiOutput("final_route"),
  
  # JavaScript: bruges til at tvinge UI til at opdatere sig selv
  tags$script(HTML("
    Shiny.addCustomMessageHandler('reset_ui', function(msg) {
      Shiny.unbindAll();
      Shiny.bindAll();
    });
  "))
)

# === SERVER ===
server <- function(input, output, session) {
  # Reaktivt ID for hvor vi er i beslutningstræet
  current_id <- reactiveVal("Q1")
  
  # Historik af ID'er – bruges til "gå tilbage"-funktion
  history <- reactiveVal(character())
  
  # Valgt rute – liste over spørgsmål og svar brugeren har truffet
  valg_rute <- reactiveVal(data.frame(
    id = character(), question = character(), svar = character()
  ))
  
  # Start forfra-knappen nulstiller alt
  observeEvent(input$reset, {
    current_id("Q1")
    history(character())
    valg_rute(data.frame(id = character(), question = character(), svar = character()))
    output$recommendation <- renderText({ "" })
    output$final_route <- renderUI({ NULL })
  }, ignoreInit = TRUE)
  
  # "Gå tilbage"-knappen
  # observeEvent(input$back, {
  #   h <- history()
  #   if (length(h) >= 1) {
  #     current_id(tail(h, 1))             # hop tilbage til sidste spørgsmål
  #     history(head(h, -1))               # fjern det sidste element i historikken
  #     
  #     rute <- valg_rute()
  #     if (nrow(rute) > 0) valg_rute(rute[-nrow(rute), ])  # fjern sidste svar
  #     
  #     # ryd anbefaling og slutvisning
  #     output$recommendation <- renderText({ "" })
  #     output$final_route <- renderUI({ NULL })
  #     
  #     # tving UI til at reaktivere knapper og UI
  #     session$sendCustomMessage(type = "reset_ui", message = list())
  #   }
  # }, ignoreInit = TRUE)
  # andet forsøg
  # observeEvent(input$back, {
  #   h <- history()
  #   if (length(h) >= 1) {
  #     new_id <- if (length(h) >= 2) h[length(h) - 1] else "Q1"
  #     current_id(new_id)
  #     history(h[-length(h)])
  #     
  #     rute <- valg_rute()
  #     if (nrow(rute) > 0) valg_rute(rute[-nrow(rute), ])
  #     
  #     output$recommendation <- renderText({ "" })
  #     output$final_route <- renderUI({ NULL })
  #     
  #     session$sendCustomMessage(type = "reset_ui", message = list())
  #   }
  # }, ignoreInit = TRUE)
  observeEvent(input$back, {
    h <- history()
    if (length(h) >= 1) {
      current_id(h[length(h)])        # <- Korrekt: gå tilbage til sidste besøgte
      history(h[-length(h)])          # <- Fjern den fra historikken
      
      rute <- valg_rute()
      if (nrow(rute) > 0) valg_rute(rute[-nrow(rute), ])
      
      output$recommendation <- renderText({ "" })
      output$final_route <- renderUI({ NULL })
      
      session$sendCustomMessage(type = "reset_ui", message = list())
    }
  }, ignoreInit = TRUE)
  
  
  # UI-rendering af spørgsmål og svar-knapper
  output$question_ui <- renderUI({
    q_rows <- df %>% filter(id == current_id())
    if (nrow(q_rows) == 0) return(NULL)
    
    # Vis spørgsmål med ID hvis debug er slået til
    label_text <- if (debug) {
      paste0("[", q_rows$id[1], "] ", q_rows$question[1])
    } else {
      q_rows$question[1]
    }
    
    # Lav knapper til hvert muligt svar
    tagList(
      h4(label_text),
      lapply(seq_len(nrow(q_rows)), function(i) {
        ans_id <- paste0("answer_", current_id(), "_", i)
        actionButton(ans_id, q_rows$svar[i], class = "btn-primary m-1")
      })
    )
  })
  
  # Vis nuværende trinnummer
  output$step_info <- renderUI({
    trin <- nrow(valg_rute()) + 1
    h4(paste0("Trin ", trin))
  })
  
  # Breadcrumb-visning: den valgte rute indtil nu
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
  
  # Dynamisk observer for hver svar-knap på det aktuelle spørgsmål
  observe({
    q_rows <- df %>% filter(id == current_id())
    lapply(seq_len(nrow(q_rows)), function(i) {
      ans_id <- paste0("answer_", current_id(), "_", i)
      
      observeEvent(input[[ans_id]], {
        chosen <- q_rows[i, ]
        
        # Tilføj nuværende spørgsmål til historikken
        history(c(history(), current_id()))
        
        # Opdater valgt rute (overskriv hvis vi er på samme id som før)
        rute <- valg_rute()
        if (nrow(rute) > 0 && rute[nrow(rute), "id"] == chosen$id) {
          rute[nrow(rute), ] <- list(chosen$id, chosen$question, chosen$svar)
        } else {
          rute <- rbind(rute, data.frame(
            id = chosen$id,
            question = chosen$question,
            svar = chosen$svar
          ))
        }
        valg_rute(rute)
        
        # Hvis vi er nået til en anbefaling, vis den og opsummering
        if (chosen$anbefaling != "") {
          output$recommendation <- renderText({ chosen$anbefaling })
          
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
          
          # Ellers hop videre til næste spørgsmål
        } else {
          output$recommendation <- renderText({ "" })
          output$final_route <- renderUI({ NULL })
          current_id(chosen$next_id)
        }
      }, ignoreInit = TRUE)
    })
  })
}

# Kør appen
shinyApp(ui, server)

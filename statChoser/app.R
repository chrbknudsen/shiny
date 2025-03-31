library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# === Brug debug mode til fejlfinding ===
debug <- TRUE

# Læs data fra Excel og rens for backslashes (MathJax)
df <- read_excel("data.xlsx") |>
  mutate(across(everything(), ~replace_na(., ""))) |>
  mutate(across(everything(), ~gsub("\\\\", "\\", ., fixed = TRUE)))

ui <- fluidPage(
  div(
    style = "background-color:#f8f9fa; padding: 20px; border-radius: 10px; margin-bottom: 20px; box-shadow: 0 2px 4px rgba(0,0,0,0.1);",
    fluidRow(
      column(2, tags$img(src = "logo.png", height = "80px", style = "margin-top: 5px;")),
      column(10,
             h1("Beslutningsstøtte", style = "margin-top: 0;"),
             uiOutput("intro_text")
      )
    )
  ),

  sidebarLayout(
    sidebarPanel(
      selectInput("sprog", "Sprog / Language", choices = c("Dansk" = "da", "English" = "en")),
      br(),
      h4("Valgt rute:"),
      uiOutput("breadcrumb")
    ),
    
    mainPanel(
      withMathJax(),
      uiOutput("step_info"),
      uiOutput("question_ui"),
      br(),
      # fluidRow(
      #   column(4, actionButton("back", "⬅ Gå tilbage")),
      #   column(4, actionButton("reset", "🔄 Start forfra"))
      # ),
      div(
        style = "margin-top: 10px;",
        actionButton("back", "⬅ Gå tilbage", class = "btn btn-secondary me-2"),
        actionButton("reset", "🔄 Start forfra", class = "btn btn-outline-secondary")
      ),
      br(),
      uiOutput("recommendation_box"),
      
      
      uiOutput("recommendation_link"),
      p(em("Decision tree from Rosner 7ed.")),
      p(em("Use recommendations on you own SOMETHING")),
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
  
  output$intro_text <- renderUI({
    if (input$sprog == "da") {
      div(class = "lead", HTML("
      <p>Dette værktøj hjælper dig med at vælge en passende statistisk test
      baseret på dine data og analyseformål.</p>
      <p>Besvar spørgsmålene trin for trin. Når du når frem til en anbefaling,
      vises den nederst sammen med en eventuel henvisning.</p>
      <p>Du kan gå ét trin tilbage eller starte forfra når som helst.</p>
      <p>Beslutningstræet er hentet fra 'Fundamentals of Biostatistics' 7. udgave af Bernard Rosner</p>
      <p>Brug anbefalingerne på eget ansvar, og kontroller altid, at de er relevante for din specifikke situation.</p>
    "))
    } else {
      div(class = "lead", HTML("
      <p>This tool helps you choose an appropriate statistical test
      based on your data and analytical goals.</p>
      <p>Answer the questions step by step. When you reach a recommendation,
      it will be displayed below along with a reference if available.</p>
      <p>You can go one step back or start over at any time.</p>
      <p>The decision tree is adapted from 'Fundamentals of Biostatistics' 7. edition by Bernard Rosner</p>
      <p>Use the recommendations at your own risk, and always verify that they are applicable to your specific situation.</p>
    "))
    }
  })
  
  observeEvent(input$reset, {
    current_id("Q1")
    history(character())
    valg_rute(data.frame(id = character(), question = character(), svar = character()))
    output$recommendation <- renderUI({ NULL })
    output$recommendation_box <- renderUI({ NULL })
    
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
      
      output$recommendation_box <- renderUI({ NULL })
      output$recommendation_link <- renderUI({ NULL })
      output$final_route <- renderUI({ NULL })
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
    anbefaling_col <- get_col("anbefaling")
    header_col <- get_col("header")
    
    label_text <- if (debug) {
      paste0("[", q_rows$id[1], "] ", q_rows[[question_col]][1])
    } else {
      q_rows[[question_col]][1]
    }
    
    header_text <- q_rows[[header_col]][1]
    
    sidste <- tail(valg_rute(), 1)
    er_ved_anbefaling <- (
      nrow(sidste) > 0 &&
        sidste$id == current_id() &&
        q_rows[[anbefaling_col]][1] != ""
    )
    
    tagList(
      if (header_text != "") h4(header_text),
      withMathJax(HTML(paste0("<p><strong>", label_text, "</strong></p>"))),
      if (!er_ved_anbefaling) {
        lapply(seq_len(nrow(q_rows)), function(i) {
          ans_id <- paste0("answer_", current_id(), "_", i)
          withMathJax(actionButton(ans_id, q_rows[[svar_col]][i], class = "btn btn-primary m-1"))
        })
      }
    )
  })
  
  
  
  output$breadcrumb <- renderUI({
    rute <- valg_rute()
    if (nrow(rute) == 0) return("Ingen valg endnu.")
    withMathJax(HTML(paste0(
      apply(rute, 1, function(row) {
        paste0("<b>[", row["id"], "] ", row["question"], ":</b> ", row["svar"])
      }),
      collapse = "<br>"
    )))
  })
  
  # 🔁 OBSERVER for spørgsmål og svar bindes når current_id() ændres
  observeEvent(current_id(), {
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
        
        # Vis anbefaling hvis vi er fremme
        if (chosen[[anbefaling_col]] != "") {
          output$recommendation_box <- renderUI({
            link <- q_rows$link[1]  # Brug q_rows, ikke chosen!
            vis_link <- !is.null(link) && !is.na(link) && nzchar(link)
            div(
              style = "background-color:#e9f9ee; border-left: 5px solid #28a745; padding: 20px; margin-top: 20px; border-radius: 5px;",
              h4(if (input$sprog == "da") "✅ Anbefaling" else "✅ Recommendation"),
              withMathJax(HTML(paste0("<p>", chosen[[anbefaling_col]], "</p>"))),
              if (vis_link) {
                tags$p(
                  tags$a(
                    href = link, target = "_blank",
                    if (input$sprog == "da") "Link til materiale" else "Link to material"
                  )
                )
              }
            )
          })
          
          # # Link og rute vises stadig separat (hvis relevant)
          # output$recommendation_link <- renderUI({
          #   if (chosen$link != "") tags$a(href = chosen$link, target = "_blank", "Link til materiale")
          #   else NULL
          # })
          
          output$final_route <- renderUI({
            withMathJax(HTML(paste0("<hr><h4>", if (input$sprog == "da") "Din fulde rute:" else "Your full path:", "</h4><ul>",
                                    paste0(
                                      apply(valg_rute(), 1, function(row) {
                                        paste0("<li><b>[", row["id"], "] ", row["question"], ":</b> ", row["svar"], "</li>")
                                      }),
                                      collapse = ""
                                    ),
                                    "</ul>"
            )))
          })
        } else {
          # Hvis ikke anbefaling endnu – ryd
          output$recommendation_box <- renderUI({ NULL })
          output$recommendation_link <- renderUI({ NULL })
          output$final_route <- renderUI({ NULL })
          current_id(chosen$next_id)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
  
}

shinyApp(ui, server)

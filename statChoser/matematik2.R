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
  titlePanel("Beslutningsstøtte"),
  uiOutput("intro_text"),
  
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
      fluidRow(
        column(4, actionButton("back", "⬅ Gå tilbage")),
        column(4, actionButton("reset", "🔄 Start forfra"))
      ),
      br(),
      h3("Anbefaling:"),
      uiOutput("recommendation"),
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
    "))
    } else {
      div(class = "lead", HTML("
      <p>This tool helps you choose an appropriate statistical test
      based on your data and analytical goals.</p>
      <p>Answer the questions step by step. When you reach a recommendation,
      it will be displayed below along with a reference if available.</p>
      <p>You can go one step back or start over at any time.</p>
    "))
    }
  })
  
  observeEvent(input$reset, {
    current_id("Q1")
    history(character())
    valg_rute(data.frame(id = character(), question = character(), svar = character()))
    output$recommendation <- renderUI({ NULL })
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
      
      output$recommendation <- renderUI({ NULL })
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
    
    label_text <- if (debug) {
      paste0("[", q_rows$id[1], "] ", q_rows[[question_col]][1])
    } else {
      q_rows[[question_col]][1]
    }
    
    # Tjek: hvis vi er fremme ved en anbefaling der allerede er givet
    sidste <- tail(valg_rute(), 1)
    if (nrow(sidste) > 0 && sidste$id == current_id() && q_rows[[anbefaling_col]][1] != "") {
      return(withMathJax(HTML(paste0("<h4>", label_text, "</h4>"))))
    }
    
    # Ellers vis spørgsmål + knapper
    tagList(
      withMathJax(HTML(paste0("<h4>", label_text, "</h4>"))),
      lapply(seq_len(nrow(q_rows)), function(i) {
        ans_id <- paste0("answer_", current_id(), "_", i)
        withMathJax(actionButton(ans_id, q_rows[[svar_col]][i], class = "btn-primary m-1"))
      })
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
        
        if (chosen[[anbefaling_col]] != "") {
          output$recommendation <- renderUI({
            withMathJax(HTML(paste0("<p>", chosen[[anbefaling_col]], "</p>")))
          })
          
          output$recommendation_link <- renderUI({
            if (chosen$link != "") tags$a(href = chosen$link, target = "_blank", "Link til materiale")
            else NULL
          })
          
          output$final_route <- renderUI({
            withMathJax(HTML(paste0("<hr><h4>Din fulde rute:</h4><ul>",
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
          output$recommendation <- renderUI({ NULL })
          output$recommendation_link <- renderUI({ NULL })
          output$final_route <- renderUI({ NULL })
          current_id(chosen$next_id)
        }
      }, ignoreInit = TRUE, once = TRUE)
    })
  })
}

shinyApp(ui, server)

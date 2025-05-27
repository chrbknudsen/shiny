library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# Læs data og rens backslashes (MathJax)
df <- read_excel("data.xlsx") %>%
  mutate(across(everything(), ~replace_na(., ""))) %>%
  mutate(across(everything(), ~gsub("\\\\", "\\", ., fixed = TRUE)))

ui <- fluidPage(
  
  # Header med logo og intro
  div(
    style = "background-color:#f8f9fa; padding:20px; border-radius:8px; margin-bottom:20px; box-shadow:0 2px 4px rgba(0,0,0,0.1);",
    fluidRow(
      column(2, tags$img(src = "logo.png", height = "80px", style = "margin-top:5px;")),
      column(10,
             h1("Beslutningsstøtte", style = "margin-top:0;"),
             uiOutput("intro_text")
      )
    )
  ),
  
  sidebarLayout(
    sidebarPanel(
      selectInput("sprog", "Sprog / Language",
                  choices = c("Dansk" = "da", "English" = "en")),
      actionButton("back", "<< Tilbage / Back"),
      actionButton("reset", "Genstart / Restart")
    ),
    
    mainPanel(
      uiOutput("question_ui"),
      uiOutput("recommendation_box"),
      uiOutput("final_route")
    )
  )
)

server <- function(input, output, session) {
  
  # --- Reaktive værdier ---
  current_id <- reactiveVal("Q1")
  history    <- reactiveVal(character())
  valg_rute  <- reactiveVal(
    data.frame(id = character(),
               question = character(),
               svar     = character(),
               stringsAsFactors = FALSE)
  )
  
  get_col <- function(base) paste0(base, "_", input$sprog)
  
  # --- Intro-tekst ---
  output$intro_text <- renderUI({
    if (input$sprog == "da") {
      div(class = "lead", HTML("
        <p>Dette værktøj hjælper dig med at vælge en passende statistisk test
        baseret på dine data og analyseformål.</p>
        <p>Besvar spørgsmålene trin for trin. Når du når frem til en anbefaling,
        vises den nederst sammen med en eventuel henvisning.</p>
        <p>Du kan gå ét trin tilbage eller starte forfra når som helst.</p>
        <p>Beslutningstræet er hentet fra 'Fundamentals of Biostatistics' 7. udgave
        af Bernard Rosner</p>
      "))
    } else {
      div(class = "lead", HTML("
        <p>This tool helps you choose an appropriate statistical test
        based on your data and analysis objectives.</p>
        <p>Answer the questions step by step. When you reach a recommendation,
        it will appear below along with any reference material.</p>
        <p>You can go back one step or restart at any time.</p>
        <p>The decision tree is taken from Bernard Rosner's
        'Fundamentals of Biostatistics', 7th edition.</p>
      "))
    }
  })
  
  # --- Spørgsmål + svar-knapper ---
  output$question_ui <- renderUI({
    q_rows      <- df %>% filter(id == current_id())
    header_col  <- get_col("header")
    question_col<- get_col("question")
    svar_col    <- get_col("svar")
    
    header_text <- q_rows[[header_col]][1]
    ui_list <- list()
    
    # 1) Håndterer NA korrekt
    if (!is.na(header_text) && nzchar(header_text)) {
      ui_list <- c(ui_list, h4(header_text))
    }
    
    # selve spørgsmålet
    ui_list <- c(ui_list,
                 withMathJax(HTML(paste0("<p>", q_rows[[question_col]][1], "</p>")))
    )
    
    # én knap pr. svar-række
    for (i in seq_len(nrow(q_rows))) {
      ans_id <- paste0("answer_", current_id(), "_", i)
      label  <- q_rows[[svar_col]][i]
      ui_list <- c(ui_list,
                   actionButton(ans_id, label, style = "margin:5px;")
      )
    }
    
    do.call(tagList, ui_list)
  })
  
  # --- Tilbage og restart knapper ---
  observeEvent(input$back, {
    h <- history()
    if (length(h) > 0) {
      prev <- tail(h, 1)
      history(head(h, -1))
      current_id(prev)
      output$recommendation_box <- renderUI(NULL)
      output$final_route      <- renderUI(NULL)
    }
  })
  
  observeEvent(input$reset, {
    history(character())
    valg_rute(
      data.frame(id = character(),
                 question = character(),
                 svar     = character(),
                 stringsAsFactors = FALSE)
    )
    current_id("Q1")
    output$recommendation_box <- renderUI(NULL)
    output$final_route      <- renderUI(NULL)
  })
  
  # --- 2) Dynamiske observeEvent én gang per svar-knap ---
  df_list <- split(df, df$id)
  for (qid in names(df_list)) {
    rows <- df_list[[qid]]
    for (i in seq_len(nrow(rows))) {
      ans_id <- paste0("answer_", qid, "_", i)
      local({
        this_qid  <- qid
        this_i    <- i
        this_rows <- rows
        
        observeEvent(input[[ans_id]], {
          chosen <- this_rows[this_i, ]
          
          # opdater historik & rute
          history(c(history(), current_id()))
          question_col  <- get_col("question")
          svar_col      <- get_col("svar")
          anbefaling_col<- get_col("anbefaling")
          
          r <- valg_rute()
          new_row <- data.frame(
            id       = chosen$id,
            question = chosen[[question_col]],
            svar     = chosen[[svar_col]],
            stringsAsFactors = FALSE
          )
          if (nrow(r) > 0 && r$id[nrow(r)] == chosen$id) {
            r[nrow(r), ] <- new_row
          } else {
            r <- rbind(r, new_row)
          }
          valg_rute(r)
          
          # 3) Brug chosen$link i stedet for q_rows$link[1]
          if (!is.na(chosen[[anbefaling_col]]) && nzchar(chosen[[anbefaling_col]])) {
            output$recommendation_box <- renderUI({
              link    <- chosen$link
              vis_link<- !is.null(link) && nzchar(link)
              div(
                style = "background-color:#e9f9ee; border-left:5px solid #28a745;
                         padding:20px; margin-top:20px; border-radius:5px;",
                h4(if (input$sprog == "da") "✅ Anbefaling" else "✅ Recommendation"),
                withMathJax(HTML(paste0("<p>", chosen[[anbefaling_col]], "</p>"))),
                if (vis_link) {
                  tags$p(
                    tags$a(href = link, target = "_blank",
                           if (input$sprog == "da") "Link til materiale" else "Link to material")
                  )
                }
              )
            })
            
            output$final_route <- renderUI({
              fr <- valg_rute()
              items <- apply(fr, 1, function(row) {
                paste0("<li><b>[", row["id"], "] ", row["question"], ":</b> ", row["svar"], "</li>")
              })
              withMathJax(HTML(paste0(
                "<hr><h4>", if (input$sprog == "da") "Din fulde rute:" else "Your full path:", "</h4><ul>",
                paste(items, collapse = ""), "</ul>"
              )))
            })
            
          } else {
            # Ingen anbefaling endnu – gå til næste spørgsmål
            output$recommendation_box <- renderUI(NULL)
            output$final_route      <- renderUI(NULL)
            current_id(chosen$next_id)
          }
          
        }, ignoreInit = TRUE)
      })
    }
  }
}

shinyApp(ui, server)

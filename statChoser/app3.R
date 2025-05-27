library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)

# --- Indlæs og forbered data ---
df <- read_excel("data.xlsx") %>%
  # Erstat NA med tom streng
  mutate(across(everything(), ~ replace_na(., ""))) %>%
  # Fjern escape-tegn til MathJax, hvis nødvendigt
  mutate(across(everything(), ~ gsub("\\\\", "\\", ., fixed = TRUE)))

# Hjælpefunktion til at vælge sprog-kolonner
get_col <- function(prefix, lang) paste0(prefix, "_", lang)

ui <- fluidPage(
  tags$head(tags$style(HTML("
    .btn-answer { margin: 5px; }
    .recommendation { 
      background-color: #e9f9ee; 
      border-left: 5px solid #28a745; 
      padding: 20px; 
      margin-top: 20px; 
      border-radius: 5px;
    }
  "))),
  
  # Header med logo (antag at logo.png kommer senere)
  fluidRow(
    column(2, tags$img(src = "logo.png", height = "60px")),
    column(10, uiOutput("intro_text"))
  ),
  hr(),
  
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
  # State
  current_id <- reactiveVal("Q1")
  history    <- reactiveVal(character())
  path_tbl   <- reactiveVal(
    tibble(id = character(), question = character(), svar = character())
  )
  
  # Intro-tekst
  output$intro_text <- renderUI({
    if (input$sprog == "da") {
      tagList(
        h2("Beslutningsstøtte forsøg 3"),
        p("Dette værktøj hjælper dig med at vælge en passende statistisk test..."),
        p("Besvar trin for trin. Du kan gå tilbage eller genstarte når som helst.")
      )
    } else {
      tagList(
        h2("Decision Support"),
        p("This tool helps you choose an appropriate statistical test..."),
        p("Answer step by step. You can go back or restart at any time.")
      )
    }
  })
  
  # Spørgsmål + knapper
  output$question_ui <- renderUI({
    req(current_id())
    q <- df %>% filter(id == current_id())
    header <- q[[ get_col("header", input$sprog) ]][1]
    question <- q[[ get_col("question", input$sprog) ]][1]
    svar_vec  <- q[[ get_col("svar", input$sprog) ]]
    
    ui <- tagList()
    # #1: tjek for NA og tom streng
    if (!is.na(header) && nzchar(header)) {
      ui <- tagList(ui, h4(header))
    }
    ui <- tagList(ui, p(HTML(question)))
    
    # knapper
    btns <- lapply(seq_along(svar_vec), function(i) {
      actionButton(
        inputId = paste0("answer_", current_id(), "_", i),
        label   = svar_vec[i],
        class   = "btn-answer"
      )
    })
    ui <- tagList(ui, div(btns))
    ui
  })
  
  # Back og reset
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
    path_tbl(tibble(id=character(), question=character(), svar=character()))
    current_id("Q1")
    output$recommendation_box <- renderUI(NULL)
    output$final_route      <- renderUI(NULL)
  })
  
  # #2: registrer alle knappers observeEvent én gang
  df_split <- split(df, df$id)
  for (qid in names(df_split)) {
    rows <- df_split[[qid]]
    for (i in seq_len(nrow(rows))) {
      local({
        my_qid <- qid
        my_i   <- i
        my_row <- rows[my_i, ]
        btn_id <- paste0("answer_", my_qid, "_", my_i)
        
        observeEvent(input[[btn_id]], ignoreInit = TRUE, {
          # gem historik
          history(c(history(), my_qid))
          
          # opdater path
          question_txt <- my_row[[ get_col("question", input$sprog) ]]
          svar_txt     <- my_row[[ get_col("svar", input$sprog) ]]
          path_tbl(bind_rows(path_tbl(),
                             tibble(id=my_qid, question=question_txt, svar=svar_txt)))
          
          # #3: hent anbefalingstekst og link fra chosen-row
          anbef_txt <- my_row[[ get_col("anbefaling", input$sprog) ]]
          link      <- my_row$link
          
          if (nzchar(anbef_txt)) {
            # vis anbefaling
            output$recommendation_box <- renderUI({
              div(
                class = "recommendation",
                h4(if (input$sprog=="da") "✅ Anbefaling" else "✅ Recommendation"),
                p(HTML(anbef_txt)),
                if (nzchar(link)) {
                  tags$p(tags$a(href=link, target="_blank",
                                if (input$sprog=="da") "Link til materiale" else "Link to material"))
                }
              )
            })
            # vis fuld rute
            output$final_route <- renderUI({
              rows <- path_tbl()
              items <- purrr::map_chr(seq_len(nrow(rows)), function(j) {
                row <- rows[j,]
                paste0("<li><b>[", row$id, "]</b> ", row$question, ": ", row$svar, "</li>")
              })
              HTML(paste0("<h4>", if(input$sprog=="da") "Din fulde rute:" else "Your full path:", "</h4>",
                          "<ul>", paste(items, collapse=""), "</ul>"))
            })
            
          } else {
            # ingen anbefaling endnu → næste spørgsmål
            output$recommendation_box <- renderUI(NULL)
            output$final_route      <- renderUI(NULL)
            current_id(my_row$next_id)
          }
        })
      })
    }
  }
}

shinyApp(ui, server)

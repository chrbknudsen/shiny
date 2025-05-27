library(shiny)
library(readxl)
library(dplyr)
library(tidyr)
library(stringr)
library(purrr)

# --- Indlæs og forbered data ---
df <- read_excel("data.xlsx") %>%
  mutate(across(everything(), ~ replace_na(., ""))) %>%
  mutate(across(everything(), ~ gsub("\\\\", "\\", ., fixed = TRUE)))

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
  # Reactive values til status
  current_id <- reactiveVal("Q1")
  history    <- reactiveVal(character())
  path_tbl   <- reactiveVal(tibble(id=character(), question=character(), svar=character()))
  show_q     <- reactiveVal(TRUE)
  
  # Hjælpefunktion til at bygge spørgsmål-UI
  render_question_ui_fn <- function() {
    req(current_id(), show_q())
    q        <- df %>% filter(id == current_id())
    header   <- q[[ get_col("header", input$sprog) ]][1]
    question <- q[[ get_col("question", input$sprog) ]][1]
    svar_vec <- q[[ get_col("svar", input$sprog) ]]
    
    ui <- tagList()
    if (nzchar(header)) {
      ui <- tagList(ui, h4(header))
    }
    ui <- tagList(ui, p(HTML(question)))
    
    btns <- lapply(seq_along(svar_vec), function(i) {
      actionButton(
        inputId = paste0("answer_", current_id(), "_", i),
        label   = svar_vec[i],
        class   = "btn-answer"
      )
    })
    tagList(ui, div(btns))
  }
  
  # Intro-tekst
  output$intro_text <- renderUI({
    if (input$sprog == "da") {
      tagList(
        h2("Beslutningsstøtte"),
        p("Dette værktøj hjælper dig med at vælge en passende statistisk test baseret på dine data og analyseformål."),
        p("Besvar spørgsmålene trin for trin. Når du når frem til en anbefaling, vises den nederst sammen med en eventuel henvisning."),
        p("Du kan gå ét trin tilbage eller starte forfra når som helst."),
        p("Beslutningstræet er hentet fra 'Fundamentals of Biostatistics' 7. udgave af Bernard Rosner"),
        p("Det er også her sidehenvisningerne er til"),
        p("Brug anbefalingerne på eget ansvar, og kontroller altid, at de er relevante for din specifikke situation."),
        p("Hvis anbefalingslinket fører til musikvideoer - så har vi ikke fået lavet en side endnu.")
      )
    } else {
      tagList(
        h2("Decision Support"),
        p("This tool helps you choose an appropriate statistical test based on your data and analytical goals."),
        p("Answer the questions step by step. When you reach a recommendation, it will be displayed below along with a reference if available."),
        p("You can go one step back or start over at any time."),
        p("The decision tree is adapted from 'Fundamentals of Biostatistics' 7. edition by Bernard Rosner."),
        p("Use the recommendations at your own risk, and always verify that they are applicable to your specific situation.")
        
      )
    }
  })
  
  # Spørgsmål + knapper
  output$question_ui <- renderUI({
    render_question_ui_fn()
  })
  
  # Render den løbende rute
  render_path_ui <- function() {
    rows <- path_tbl()
    items <- map_chr(seq_len(nrow(rows)), function(j) {
      row <- rows[j, ]
      paste0("<li><b>[", row$id, "]</b> ", row$question, ": ", row$svar, "</li>")
    })
    HTML(paste0(
      "<h4>", if (input$sprog=="da") "Din løbende rute:" else "Your current path:", "</h4>",
      "<ul>", paste(items, collapse=""), "</ul>"
    ))
  }
  
  # Håndter svar-knapper
  df_split <- split(df, df$id)
  for (qid in names(df_split)) {
    rows <- df_split[[qid]]
    for (i in seq_len(nrow(rows))) {
      local({
        my_qid <- qid
        my_row <- rows[i, ]
        btn_id <- paste0("answer_", my_qid, "_", i)
        
        observeEvent(input[[btn_id]], ignoreInit = TRUE, {
          # 1) Opdater historik og rute
          history(c(history(), my_qid))
          question_txt <- my_row[[ get_col("question", input$sprog) ]]
          svar_txt     <- my_row[[ get_col("svar", input$sprog) ]]
          path_tbl(bind_rows(path_tbl(),
                             tibble(id       = my_qid,
                                    question = question_txt,
                                    svar     = svar_txt)))
          
          # 2) Hent anbefalingstekst og evt. link
          anbef_txt <- my_row[[ get_col("anbefaling", input$sprog) ]]
          link      <- my_row$link
          
          if (nzchar(anbef_txt)) {
            # Vis anbefaling og gem ikke spørgsmål
            show_q(FALSE)
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
          } else {
            # Gå til næste spørgsmål
            show_q(TRUE)
            output$recommendation_box <- renderUI(NULL)
            current_id(my_row$next_id)
          }
          
          # 3) Opdater den løbende rute
          output$final_route <- renderUI(render_path_ui())
        })
      })
    }
  }
  
  # Tilbage-knap
  observeEvent(input$back, {
    h <- history()
    if (length(h) >= 1) {
      # Pop sidste ID
      last_id <- tail(h, 1)
      history(head(h, -1))
      
      # Fjern sidste række i path_tbl
      pt <- path_tbl()
      if (nrow(pt) > 0) {
        path_tbl(pt[ seq_len(nrow(pt) - 1), ])
      }
      
      # Vis spørgsmål igen og ryd anbefaling
      show_q(TRUE)
      output$recommendation_box <- renderUI(NULL)
      
      # Sæt current_id til det poppede spørgsmål
      current_id(last_id)
      
      # Rerender stien
      output$final_route <- renderUI(render_path_ui())
    }
  })
  
  # Genstart-knap
  observeEvent(input$reset, {
    show_q(TRUE)
    history(character())
    path_tbl(tibble(id=character(), question=character(), svar=character()))
    current_id("Q1")
    output$recommendation_box <- renderUI(NULL)
    output$final_route       <- renderUI(NULL)
  })
}

shinyApp(ui, server)

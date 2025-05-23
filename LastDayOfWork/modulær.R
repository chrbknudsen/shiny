library(shiny)
library(shinyjs)
library(tidyverse)
library(httr)
library(jsonlite)
library(lubridate)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Mændenes sidste arbejdsdag - Modulær version"),
  mainPanel(
    uiOutput("stepUI"),
    br(),
    # Vi placerer knappen i en div, som vi kan skjule når det er sidste trin
    div(id = "nextBtnDiv", actionButton("nextBtn", "Videre"))
  )
)

server <- function(input, output, session) {
  currentStep <- reactiveVal(1)
  
  # Reactive værdier til at gemme API-resultater og beregnede data
  values <- reactiveValues(
    metadata = NULL,
    data_raw = NULL,
    data_df = NULL,
    sidst_dag_aar = NULL
  )
  
  # Listen med alle 18 trin – nextEnabled kan være et fast logisk eller en funktion, der evaluerer en betingelse
  steps <- list(
    list(
      title = "Trin 1: Indlæsning af biblioteker",
      content = tagList(
        p("Dette trin viser koden, som indlæser de nødvendige biblioteker:"),
        pre("library(shiny)
library(shinyjs)
library(future)
library(promises)
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)")
      ),
      nextEnabled = TRUE
    ),
    list(
      title = "Trin 2: Opsætning af API-oplysninger for metadata",
      content = tagList(
        pre("endpoint <- 'http://api.statbank.dk/v1/tableinfo'
our_body <- list(lang = 'da', table = 'AKU410A')"),
        p("Når du trykker 'Videre', kan du i næste trin hente metadata.")
      ),
      nextEnabled = TRUE
    ),
    list(
      title = "Trin 3: Hent metadata",
      content = tagList(
        p("Klik på knappen for at udføre API-kaldet og hente metadata:"),
        actionButton("getMeta", "Hent Metadata"),
        verbatimTextOutput("metaOutput")
      ),
      nextEnabled = function() { !is.null(values$metadata) }
    ),
    list(
      title = "Trin 4: Visning af metadata",
      content = tagList(
        verbatimTextOutput("metaOutput")
      ),
      nextEnabled = TRUE
    ),
    list(
      title = "Trin 5: Opsætning af variabler for datahentning",
      content = tagList(
        pre("variables <- list(
  list(code = 'ARBEJDSTID', values = I('022')),
  list(code = 'ALDER', values = I('*')),
  list(code = 'KOEN', values = c('M', 'K')),
  list(code = 'Tid', values = I('*'))
)"),
        p("Dette definerer de variable, vi ønsker fra API'et.")
      ),
      nextEnabled = TRUE
    ),
    list(
      title = "Trin 6: Opsætning af API-kald for data",
      content = tagList(
        pre("data_body <- list(
  table = 'AKU410A', 
  lang = 'da', 
  format = 'CSV', 
  variables = variables
)
endpoint <- 'http://api.statbank.dk/v1/data'"),
      p("Når du trykker 'Videre', vil næste trin hente data.")
      ),
    nextEnabled = TRUE
    ),
  list(
    title = "Trin 7: Hent data",
    content = tagList(
      actionButton("getData", "Hent Data"),
      verbatimTextOutput("dataStatus")
    ),
    nextEnabled = function() { !is.null(values$data_df) }
  ),
  list(
    title = "Trin 8: Konverter data til data frame",
    content = tagList(
      verbatimTextOutput("dataHead")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 9: Fjern kolonnen ARBEJDSTID",
    content = tagList(
      verbatimTextOutput("dataHead2")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 10: Plot data for arbejdstid",
    content = tagList(
      plotOutput("plot1")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 11: Pivotering og beregning af forskel",
    content = tagList(
      verbatimTextOutput("pivotOutput")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 12: Plot forskel over tid",
    content = tagList(
      plotOutput("plot2")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 13: Filter for 'Alder i alt' og plot",
    content = tagList(
      plotOutput("plot3")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 14: Lineær model og beregning af ligestillingstidspunkt",
    content = tagList(
      verbatimTextOutput("modelOutput")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 15: Beregn mændenes sidste arbejdsdag",
    content = tagList(
      pre("sidste_dag <- 31/35*365"),
      verbatimTextOutput("lastWorkDayCalc")
    ),
    nextEnabled = TRUE
  ),
  list(
    title = "Trin 16: Beregn sidste arbejdsdag for hvert år",
    content = tagList(
      verbatimTextOutput("sidsteDagAarOutput")
    ),
    nextEnabled = function() { !is.null(values$data_df) }
  ),
  list(
    title = "Trin 17: Beregn og vis sidste arbejdsdag som dato",
    content = tagList(
      verbatimTextOutput("sidsteDagAarOutput2")
    ),
    nextEnabled = function() { !is.null(values$sidst_dag_aar) }
  ),
  list(
    title = "Trin 18: Vis kalender med markeret sidste arbejdsdag",
    content = tagList(
      plotOutput("calendarPlot")
    ),
    # Det sidste trin skal ikke have en 'Videre'-knap
    nextEnabled = FALSE
  )
  )

# Dynamisk rendering af UI baseret på det aktuelle trin
output$stepUI <- renderUI({
  s <- currentStep()
  if (s <= length(steps)) {
    tagList(
      h3(steps[[s]]$title),
      steps[[s]]$content
    )
  } else {
    h3("Alle trin er gennemgået!")
  }
})

# Evaluerer nextEnabled for det aktuelle trin – hvis nextEnabled er en funktion, kaldes den
isNextEnabled <- reactive({
  s <- currentStep()
  if (s <= length(steps)) {
    ne <- steps[[s]]$nextEnabled
    if (is.function(ne)) {
      return(ne())
    } else {
      return(ne)
    }
  }
  return(FALSE)
})

# Observer der styrer synligheden af "Videre"-knappen: skjul den, hvis det er sidste trin
observe({
  if (currentStep() >= length(steps)) {
    shinyjs::hide("nextBtnDiv")
  } else {
    shinyjs::show("nextBtnDiv")
  }
})

# Aktiver/deaktiver "Videre"-knappen baseret på isNextEnabled
observe({
  shinyjs::toggleState("nextBtn", condition = isNextEnabled())
})

# Opdater det aktuelle trin, når brugeren trykker "Videre"
observeEvent(input$nextBtn, {
  s <- currentStep()
  if (s < length(steps)) {
    currentStep(s + 1)
  }
})

# --- Trin 3: Hent metadata ---
observeEvent(input$getMeta, {
  endpoint <- "http://api.statbank.dk/v1/tableinfo"
  our_body <- list(lang = "da", table = "AKU410A")
  res <- POST(endpoint, body = our_body, encode = "json")
  values$metadata <- content(res) %>% fromJSON()
})

output$metaOutput <- renderPrint({
  if (is.null(values$metadata)) {
    "Metadata ikke hentet endnu."
  } else {
    values$metadata
  }
})

# --- Trin 7: Hent data ---
observeEvent(input$getData, {
  variables <- list(
    list(code = "ARBEJDSTID", values = I("022")),
    list(code = "ALDER", values = I("*")),
    list(code = "KOEN", values = c("M", "K")),
    list(code = "Tid", values = I("*"))
  )
  data_body <- list(
    table = "AKU410A", 
    lang = "da", 
    format = "CSV", 
    variables = variables
  )
  endpoint <- "http://api.statbank.dk/v1/data"
  res <- POST(endpoint, body = data_body, encode = "json")
  values$data_raw <- res
  data_text <- content(res, type = "text")
  values$data_df <- read_csv2(data_text)
})

output$dataStatus <- renderPrint({
  if (is.null(values$data_raw)) {
    "Data ikke hentet endnu."
  } else {
    paste("Status kode:", values$data_raw$status_code)
  }
})

output$dataHead <- renderPrint({
  if (is.null(values$data_df)) {
    "Dataframe ikke oprettet endnu."
  } else {
    head(values$data_df)
  }
})

output$dataHead2 <- renderPrint({
  if (is.null(values$data_df)) {
    "Dataframe ikke oprettet endnu."
  } else {
    head(values$data_df %>% select(-ARBEJDSTID))
  }
})

output$plot1 <- renderPlot({
  req(values$data_df)
  df <- values$data_df
  ggplot(df, aes(x = TID, y = INDHOLD, colour = ALDER)) +
    geom_line() +
    facet_wrap(~KOEN)
})

output$pivotOutput <- renderPrint({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(dif = Mænd - Kvinder)
  head(df)
})

output$plot2 <- renderPlot({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(dif = Mænd - Kvinder)
  ggplot(df, aes(x = TID, y = dif, colour = ALDER)) +
    geom_line()
})

output$plot3 <- renderPlot({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(dif = Mænd - Kvinder) %>% 
    filter(ALDER == "Alder i alt")
  ggplot(df, aes(x = TID, y = dif)) +
    geom_line()
})

output$modelOutput <- renderPrint({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(dif = Mænd - Kvinder) %>% 
    filter(ALDER == "Alder i alt")
  model <- lm(dif ~ TID, data = df)
  coef_model <- coef(model)
  i_maal <- round(-coef_model[1] / coef_model[2])
  paste("Ligestilling opnås ca. i år:", i_maal)
})

output$lastWorkDayCalc <- renderPrint({
  calc <- 31/35*365
  paste("Beregnet antal dage:", calc)
})

output$sidsteDagAarOutput <- renderPrint({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(brøk = Kvinder / Mænd) %>% 
    select(TID, brøk)
  values$sidst_dag_aar <- df
  head(df)
})

frac_til_dato <- function(fraction, aar) {
  dag_nummer <- round(fraction * (365 + leap_year(aar)))
  as.Date(paste0(aar, "-01-01")) + (dag_nummer - 1)
}

output$sidsteDagAarOutput2 <- renderPrint({
  req(values$sidst_dag_aar)
  df <- values$sidst_dag_aar %>% 
    mutate(sidste_arbejdsdag = map2(brøk, TID, frac_til_dato)) %>% 
    unnest(sidste_arbejdsdag)
  df
})

output$calendarPlot <- renderPlot({
  req(values$sidst_dag_aar)
  df <- values$sidst_dag_aar %>% 
    mutate(sidste_arbejdsdag = map2(brøk, TID, frac_til_dato)) %>% 
    unnest(sidste_arbejdsdag)
  special_date <- df %>% slice_max(TID, n = 1, with_ties = FALSE) %>% 
    pull(sidste_arbejdsdag) %>% as.Date()
  year_val <- lubridate::year(special_date)
  month_val <- lubridate::month(special_date)
  
  first_date <- as.Date(paste(year_val, month_val, "1", sep = "-"))
  last_date <- ceiling_date(first_date, "month") - days(1)
  dates <- seq(first_date, last_date, by = "day")
  calendar_df <- data.frame(date = dates)
  calendar_df$day <- lubridate::day(calendar_df$date)
  calendar_df$wday <- lubridate::wday(calendar_df$date, week_start = 1)
  first_wday <- lubridate::wday(first_date, week_start = 1)
  calendar_df$week <- ceiling((calendar_df$day + first_wday - 1) / 7)
  
  p <- ggplot(calendar_df, aes(x = wday, y = -week)) + 
    geom_tile(fill = "white", color = "black") +
    geom_text(aes(label = day), size = 5) +
    scale_x_continuous(breaks = 1:7, labels = c("Man", "Tir", "Ons", "Tor", "Fre", "Lør", "Søn")) +
    theme_minimal() +
    labs(title = paste(month.name[month_val], year_val)) +
    theme(axis.title = element_blank(), 
          axis.text.y = element_blank(), 
          axis.ticks = element_blank())
  
  if (special_date >= first_date && special_date <= last_date) {
    special_day <- lubridate::day(special_date)
    special_wday <- lubridate::wday(special_date, week_start = 1)
    special_week <- ceiling((special_day + first_wday - 1) / 7)
    
    p <- p + annotate("text", 
                      x = special_wday, 
                      y = -special_week, 
                      label = "×", 
                      color = "red", 
                      size = 10, 
                      fontface = "bold")
  }
  p
})
}

shinyApp(ui, server)

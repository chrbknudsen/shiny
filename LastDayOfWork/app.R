library(shiny)
library(shinyjs)
library(future)
library(promises)
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)
library(lubridate)

ui <- fluidPage(
  shinyjs::useShinyjs(),
  titlePanel("Mændenes sidste arbejdsdag"),
  mainPanel(
    uiOutput("stepUI"),
    br(),
    # Knappen er fast placeret, men vi kan skjule den med shinyjs, når der ikke er flere trin.
    div(id = "nextBtnDiv", actionButton("nextBtn", "Videre"))
  )
)

server <- function(input, output, session) {
  
  # Holder styr på hvilket trin, vi er i
  step <- reactiveVal(1)
  
  # ReactiveValues til at lagre resultaterne fra de enkelte trin
  values <- reactiveValues(
    metadata = NULL,
    data_raw = NULL,
    data_df = NULL,
    sidst_dag_aar = NULL
  )
  
  # Dynamisk UI: Hvert trin vises forskelligt
  output$stepUI <- renderUI({
    s <- step()
    if(s == 1) {
      tagList(
        h3("Hvornår kan vi holde fri?"),
        p("Lige resultater er vigtige. Og en oplagt parameter at måle på er, hvor mange
          timer man lægger på arbejdsmarkedet. De timer man bruger dér kan jo ikke
          bruges på det der giver mening i livet. Så hvornår har mænd lagt lige så
          mange timer på jobbet som kvinder? Og hvornår kan de så holde fri?"),
        p("Den slags holder Danmarks Statistik styr på, og vi kan trække data fra
          dem, der kan give os svaret på hvornår mænd kan holde deres sidste arbejdsdag."),
        p("Det gør vi ved hjælp af R, og vi skal starte med at indlæse en række biblioteker:"),
        pre("library(shiny)
library(shinyjs)
library(future)
library(promises)
library(tidyverse)
library(httr)
library(jsonlite)
library(magrittr)")
      )
    } else if(s == 2) {
      tagList(
        h3("Opsætning af API-oplysninger for metadata"),
        p("Vi har kigget på deres hjemmeside, og fundet ud af at det er tabellen 'AKU410A',
          der indeholder de relevante data. Så vi definerer det endpoint vi skal tale med 
          hos Danmarks Statistik, og det 'body', vi skal sende til API'en for at få 
          information om tabellen returneret:"),
        pre("endpoint <- 'http://api.statbank.dk/v1/tableinfo'
our_body <- list(lang = 'da', table = 'AKU410A')"),
        p("Når du trykker 'Videre', henter vi metadata på tabellen, som vi skal bruge til
          at hente de egentlige data.")
      )
    } else if(s == 3) {
      tagList(
        h3("Hent metadata"),
        p("Klik på knappen for at udføre API-kaldet og hente metadata på tabellen:"),
        actionButton("getMeta", "Hent Metadata"),
        verbatimTextOutput("metaOutput")
      )
    } else if(s == 4) {
      tagList(
        h3("Hvad betyder de metadata?"),
        p("Når vi nærlæser outputtet, kan vi se, at det er den værdi af 'ARBEJDSTID', der
          er '022' vi er på jagt efter. Det er 'Faktisk ugentlig arbejdstid, 1-95 timer (mindst en times arbejde i referenceugen)'. 
          Vi vil gerne have det for alle aldersgrupper, så vi skal bede om '*' fra
          variablen 'ALDER'. Vi er interesseret i begge køn, men har ikke brug 
          for at Danmarks Statistik lægger tallene sammen for os, så i variablen 
          'KOEN', skal vi have værdierne 'M' og 'K'."),
        p("Hvis vi vil se på om vi kommer tættere på ligestilling, skal vi også
          have værdierne for forskellige år, så vi kan sammenligne. Det er også
          '*'."),
        p("Da R er glad for at parse input, og API'en er glad for input der 
          ikke er parset - pakker vi de værdier hvor det kan gå galt, ind i 
          funktionen 'I' som beskytter værdierne mod R:"),
        verbatimTextOutput("metaOutput")
      )
    } else if(s == 5) {
      tagList(
        h3("Trin 5: Opsætning af variabler for datahentning"),
        pre("variables <- list(
  list(code = 'ARBEJDSTID', values = I('022')),
  list(code = 'ALDER', values = I('*')),
  list(code = 'KOEN', values = c('M', 'K')),
  list(code = 'Tid', values = I('*'))
)")
      )
    } else if(s == 6) {
      tagList(
        h3("Opsætning af API-kald for data"),
        p("Nu har vi det vi skal bruge for at hente data. Vi beder om at få svar
          på dansk, at det godt må formatteres som csv (for JSON er noget bøvl). Og
          så skal vi huske at ændre på endpointet til API'en, for nu er det data
          vi skal hente:"),
        pre("data_body <- list(
  table = 'AKU410A', 
  lang = 'da', 
  format = 'CSV', 
  variables = variables
)
endpoint <- 'http://api.statbank.dk/v1/data'"),
      p("Når du trykker 'Videre', vil næste trin hente data.")
      )
    } else if(s == 7) {
      tagList(
        h3("Trin 7: Hent data"),
        actionButton("getData", "Hent Data"),
        verbatimTextOutput("dataStatus")
      )
    } else if(s == 8) {
      tagList(
        h3("Trin 8: Konverter data til data frame"),
        verbatimTextOutput("dataHead")
      )
    } else if(s == 9) {
      tagList(
        h3("Trin 9: Fjern kolonnen ARBEJDSTID"),
        verbatimTextOutput("dataHead2")
      )
    } else if(s == 10) {
      tagList(
        h3("Trin 10: Plot data for arbejdstid"),
        plotOutput("plot1")
      )
    } else if(s == 11) {
      tagList(
        h3("Trin 11: Pivotering og beregning af forskel"),
        verbatimTextOutput("pivotOutput")
      )
    } else if(s == 12) {
      tagList(
        h3("Trin 12: Plot forskel over tid"),
        plotOutput("plot2")
      )
    } else if(s == 13) {
      tagList(
        h3("Trin 13: Filter for 'Alder i alt' og plot"),
        plotOutput("plot3")
      )
    } else if(s == 14) {
      tagList(
        h3("Trin 14: Lineær model og beregning af ligestillingstidspunkt"),
        verbatimTextOutput("modelOutput")
      )
    } else if(s == 15) {
      tagList(
        h3("Trin 15: Beregn mændenes sidste arbejdsdag"),
        pre("sidste_dag <- 31/35*365"),
        verbatimTextOutput("lastWorkDayCalc")
      )
    } else if(s == 16) {
      tagList(
        h3("Trin 16: Beregn sidste arbejdsdag for hvert år"),
        verbatimTextOutput("sidsteDagAarOutput")
      )
    } else if(s == 17) {
      tagList(
        h3("Trin 17: Beregn og vis sidste arbejdsdag som dato"),
        verbatimTextOutput("sidsteDagAarOutput2")
      )
    } else if(s == 18) {
      tagList(
        h3("Trin 18: Vis kalender med markeret sidste arbejdsdag"),
        plotOutput("calendarPlot")
      )
    } else {
      h3("Alle trin er gennemgået!")
    }
  })

# Observer, som aktiverer/deaktiverer "Videre"-knappen, afhængig af trinnet
observe({
  s <- step()
  canProceed <- TRUE
  if (s == 3) {
    canProceed <- !is.null(values$metadata)
  } else if (s == 7) {
    canProceed <- !is.null(values$data_df)
  } else if (s == 16) {
    canProceed <- !is.null(values$data_df)
  } else if (s == 17) {
    canProceed <- !is.null(values$sidst_dag_aar)
  }
  shinyjs::toggleState("nextBtn", condition = canProceed)
})

# Skjul "Videre"-knappen, hvis der ikke er flere trin
observe({
  if (step() >= 18) {
    shinyjs::hide("nextBtnDiv")
  } else {
    shinyjs::show("nextBtnDiv")
  }
})

# Navigation: Når "Videre" trykkes, opdateres til næste trin
observeEvent(input$nextBtn, {
  step(step() + 1)
  # Hvis det nye trin kræver, at en beregning skal udføres (og knappen derfor skal deaktiveres)
  if (step() %in% c(3, 7, 16, 17)) {
    shinyjs::disable("nextBtn")
  }
})

# --- Trin 3: Hent metadata ---
observeEvent(input$getMeta, {
  endpoint <- "http://api.statbank.dk/v1/tableinfo"
  our_body <- list(lang = "da", table = "AKU410A")
  res <- POST(endpoint, body = our_body, encode = "json")
  values$metadata <- res %>% content() %>% fromJSON()
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

# --- Trin 10: Plot data ---
output$plot1 <- renderPlot({
  req(values$data_df)
  df <- values$data_df
  ggplot(df, aes(x = TID, y = INDHOLD, colour = ALDER)) +
    geom_line() +
    facet_wrap(~KOEN)
})

# --- Trin 11: Pivotering og beregning af forskel ---
output$pivotOutput <- renderPrint({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(dif = Mænd - Kvinder)
  head(df)
})

# --- Trin 12: Plot forskel over tid ---
output$plot2 <- renderPlot({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(dif = Mænd - Kvinder)
  ggplot(df, aes(x = TID, y = dif, colour = ALDER)) +
    geom_line()
})

# --- Trin 13: Filter for "Alder i alt" og plot ---
output$plot3 <- renderPlot({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(dif = Mænd - Kvinder) %>% 
    filter(ALDER == "Alder i alt")
  ggplot(df, aes(x = TID, y = dif)) +
    geom_line()
})

# --- Trin 14: Lineær model og beregning af ligestillingstidspunkt ---
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

# --- Trin 15: Beregn mændenes sidste arbejdsdag ---
output$lastWorkDayCalc <- renderPrint({
  calc <- 31/35*365
  paste("Beregnet antal dage:", calc)
})

# --- Trin 16: Beregn sidste arbejdsdag for hvert år ---
output$sidsteDagAarOutput <- renderPrint({
  req(values$data_df)
  df <- values$data_df %>% select(-ARBEJDSTID) %>% 
    pivot_wider(names_from = KOEN, values_from = INDHOLD) %>% 
    mutate(brøk = Kvinder / Mænd) %>% 
    select(TID, brøk)
  values$sidst_dag_aar <- df
  head(df)
})

# Funktion til at konvertere en brøk til en dato (tager højde for skudår)
frac_til_dato <- function(fraction, aar) {
  dag_nummer <- round(fraction * (365 + leap_year(aar)))
  as.Date(paste0(aar, "-01-01")) + (dag_nummer - 1)
}

# --- Trin 17: Beregn og vis sidste arbejdsdag som dato ---
output$sidsteDagAarOutput2 <- renderPrint({
  req(values$sidst_dag_aar)
  df <- values$sidst_dag_aar %>% 
    mutate(sidste_arbejdsdag = map2(brøk, TID, frac_til_dato)) %>% 
    unnest(sidste_arbejdsdag)
  df
})

# --- Trin 18: Vis kalender med markeret sidste arbejdsdag ---
output$calendarPlot <- renderPlot({
  req(values$sidst_dag_aar)
  df <- values$sidst_dag_aar %>% 
    mutate(sidste_arbejdsdag = map2(brøk, TID, frac_til_dato)) %>% 
    unnest(sidste_arbejdsdag)
  special_date <- df %>% slice_max(TID, n = 1, with_ties = FALSE) %>% 
    pull(sidste_arbejdsdag) %>% as.Date()
  year_val <- year(special_date)
  month_val <- month(special_date)
  
  first_date <- as.Date(paste(year_val, month_val, "1", sep = "-"))
  last_date <- ceiling_date(first_date, "month") - days(1)
  dates <- seq(first_date, last_date, by = "day")
  calendar_df <- data.frame(date = dates)
  calendar_df$day <- day(calendar_df$date)
  calendar_df$wday <- wday(calendar_df$date, week_start = 1)
  first_wday <- wday(first_date, week_start = 1)
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
    special_day <- day(special_date)
    special_wday <- wday(special_date, week_start = 1)
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

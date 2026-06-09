library(shiny)
library(lubridate)
library(KnudsenKit)

svg0 <- segment7(0)
svg1 <- segment7(1)

start <- '<style>
  .svg-container {
    display: flex;
  }

  svg {
    width: 100px;
    height: auto;
    margin: 0px;
    padding: 0px;
  }

  .svg-container svg:last-child {
    margin-right: 0;
  }
</style>

<div class="svg-container">'

slut <- '</div>'

unit_label <- function(value, singular, plural) {
  if (value == 1) {
    sprintf("%d %s", value, singular)
  } else {
    sprintf("%d %s", value, plural)
  }
}

to_text_string <- function(duration) {
  aar <- floor(as.numeric(as.period(duration)@year))
  mon <- floor(as.numeric(as.period(duration)@month))
  day <- floor(as.numeric(as.period(duration)@day))
  hou <- floor(as.numeric(as.period(duration)@hour))
  min <- floor(as.numeric(as.period(duration)@minute))
  sec <- floor(as.numeric(as.period(duration)@.Data))
  
  result <- paste(
    if (aar > 0) unit_label(aar, "år", "år") else "",
    if (mon > 0) unit_label(mon, "måned", "måneder") else "",
    if (day > 0) unit_label(day, "dag", "dage") else "",
    if (hou > 0) unit_label(hou, "time", "timer") else "",
    if (min > 0) unit_label(min, "minut", "minutter") else "",
    if (sec > 0) unit_label(sec, "sekund", "sekunder") else "",
    sep = " "
  )
  
  trimws(result)
}

to_binary_string <- function(number) {
  if (number == 0) return("0")
  
  binary_string <- ""
  
  while (number > 0) {
    binary_string <- paste0(number %% 2, binary_string)
    number <- number %/% 2
  }
  
  binary_string
}

to_svgs <- function(number) {
  temp <- to_binary_string(number)
  temp <- unlist(strsplit(temp, ""))
  paste0(ifelse(temp == 0, svg0, svg1), collapse = " ")
}

ui <- fluidPage(
  tags$head(
    tags$style(HTML("
      h2 {
        color: #00FF00;
      }

      body {
        background-color: black;
      }

      .checkbox label {
        color: #00FF00;
        font-size: 16px;
      }

      .controls-row {
        display: flex;
        align-items: center;
        gap: 10px;
        width: fit-content;
      }

      .binary-control,
      .binary-control .form-group,
      .binary-control .checkbox {
        width: auto !important;
        display: inline-block !important;
        margin: 0 !important;
        padding: 0 !important;
      }

      .age-select-inline {
        display: flex;
        align-items: center;
        gap: 6px;
        margin: 0 !important;
        padding: 0 !important;
      }

      .age-select-inline label {
        color: #00FF00;
        margin: 0 !important;
        font-weight: normal;
      }

      .age-select-inline .form-group {
        margin: 0 !important;
        padding: 0 !important;
      }

      .age-select-inline .selectize-control {
        width: 60px !important;
      }

      .age-select-inline .selectize-input {
        min-height: 24px !important;
        height: 24px !important;
        padding: 2px 6px !important;
        background-color: black !important;
        color: #00FF00 !important;
        border-color: #00FF00 !important;
        line-height: 18px !important;
      }

      .age-select-inline .selectize-input input {
        color: #00FF00 !important;
      }

      .age-select-inline .selectize-dropdown {
        background-color: black !important;
        color: #00FF00 !important;
        border-color: #00FF00 !important;
      }

      .age-select-inline .selectize-dropdown-content .option {
        color: #00FF00 !important;
        background-color: black !important;
      }
    ")),
    
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.ico"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "/favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "/favicon-16x16.png")
  ),
  
  titlePanel("leaving ground"),
  
  div(
    class = "controls-row",
    
    div(
      class = "binary-control",
      checkboxInput("toggle", "Binær", value = TRUE)
    ),
    
    div(
      class = "age-select-inline",
      tags$label("Vælg pensionsalder", `for` = "alder"),
      selectInput(
        inputId = "alder",
        label = NULL,
        choices = c("62" = 62, "63" = 63, "64" = 64),
        selected = 64,
        width = "60px"
      )
    )
  ),
  
  htmlOutput("curr_down")
)

server <- function(input, output, session) {
  output$curr_down <- renderUI({
    invalidateLater(1000, session)
    
    curr_time <- with_tz(now("UTC"), tzone = "Europe/Copenhagen")
    
    target_time <- as.POSIXct(
      "1974-06-30 23:59:59",
      tz = "Europe/Copenhagen"
    ) %m+% years(as.numeric(input$alder))
    
    duration <- interval(curr_time, target_time)
    sekunder <- floor(as.numeric(duration, "seconds"))
    
    if (sekunder > 0 & input$toggle) {
      HTML(paste0(start, to_svgs(sekunder), slut))
    } else if (sekunder > 0 & !input$toggle) {
      HTML(paste('<h2 style="color: #00FF00;">', to_text_string(duration), '</h2>'))
    } else {
      HTML('<h2 style="color: #00FF00;">Done!</h2>')
    }
  })
}

shinyApp(ui = ui, server = server)
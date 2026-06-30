# app.R
# install.packages(c("shiny", "bslib", "tidyverse", "rvest", "lubridate", "DT"))

library(shiny)
library(bslib)
library(tidyverse)
library(rvest)
library(lubridate)
library(DT)

program_url <- "https://events.digital-research.academy/event/109/contributions/"
event_tz <- "Europe/Warsaw"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || all(is.na(x))) y else x
}

duration_to_minutes <- function(type) {
  type <- type %||% ""

  h <- str_match(type, "(\\d+)\\s*hours?")[, 2] |> as.numeric()
  m <- str_match(type, "(\\d+)\\s*minutes?")[, 2] |> as.numeric()

  h[is.na(h)] <- 0
  m[is.na(m)] <- 0

  out <- h * 60 + m

  case_when(
    out > 0 ~ out,
    str_detect(type, "Keynote") ~ 60,
    str_detect(type, "Poster") ~ 60,
    str_detect(type, "Talks") ~ 20,
    TRUE ~ 20
  )
}

parse_contributions <- function(url = program_url) {
  page <- read_html(url)

  lines <- page |>
    html_element("body") |>
    html_text2() |>
    str_split("\n") |>
    pluck(1) |>
    str_squish()

  lines <- lines[lines != ""]

  starts <- which(str_detect(lines, "^\\d+\\.\\s+"))

  blocks <- map2(
    starts,
    c(starts[-1] - 1, length(lines)),
    ~ lines[.x:.y]
  )

  map_dfr(blocks, function(block) {
    block <- block[!str_detect(block, "^Go to contribution page$")]

    date_i <- which(str_detect(block, "^\\d{2}/\\d{2}/\\d{4},\\s*\\d{2}:\\d{2}$"))[1]

    if (is.na(date_i)) {
      return(tibble())
    }

    contribution_id <- str_extract(block[1], "^\\d+")
    title <- str_remove(block[1], "^\\d+\\.\\s+")

    speaker <- block[2:(date_i - 1)] |>
      paste(collapse = " ") |>
      str_squish()

    datetime_txt <- block[date_i]
    start_dt <- dmy_hm(datetime_txt, tz = event_tz)

    after <- block[(date_i + 1):length(block)]

    type_i <- which(str_detect(
      after,
      "^(Tutorial|Talks|Lightning Talk|Poster|Keynote|Sponsor Session)"
    ))[1]

    if (is.na(type_i)) {
      type_i <- 1
    }

    track <- if (type_i > 1) {
      paste(after[1:(type_i - 1)], collapse = " ") |> str_squish()
    } else {
      NA_character_
    }

    type <- after[type_i] %||% NA_character_

    possible_session <- after[type_i + 1] %||% NA_character_

    session <- if (
      !is.na(possible_session) &&
        nchar(possible_session) < 80 &&
        !str_detect(possible_session, "^Abstract")
    ) {
      possible_session
    } else {
      NA_character_
    }

    desc_start <- type_i + ifelse(is.na(session), 1, 2)

    description <- if (desc_start <= length(after)) {
      after[desc_start:length(after)] |>
        paste(collapse = " ") |>
        str_squish()
    } else {
      NA_character_
    }

    duration_min <- duration_to_minutes(type)
    end_dt <- start_dt + minutes(duration_min)

    tibble(
      row_id = contribution_id,
      contribution_id = contribution_id,
      title = title,
      speaker = speaker,
      datetime_start = start_dt,
      datetime_end = end_dt,
      date = as.Date(start_dt),
      start = format(start_dt, "%H:%M"),
      end = format(end_dt, "%H:%M"),
      track = track,
      type = type,
      session = session,
      description = description,
      url = paste0(
        "https://events.digital-research.academy/event/109/contributions/",
        contribution_id,
        "/"
      )
    )
  }) |>
    mutate(
      text = str_c(title, speaker, track, type, session, description, sep = " ")
    ) |>
    arrange(datetime_start, track, title)
}

ics_escape <- function(x) {
  x |>
    replace_na("") |>
    str_replace_all("\\\\", "\\\\\\\\") |>
    str_replace_all("\n", "\\\\n") |>
    str_replace_all(",", "\\\\,") |>
    str_replace_all(";", "\\\\;")
}

make_ics <- function(df) {
  if (nrow(df) == 0) {
    return(paste(
      "BEGIN:VCALENDAR",
      "VERSION:2.0",
      "PRODID:-//user2026-plan//shiny//EN",
      "END:VCALENDAR",
      sep = "\r\n"
    ))
  }

  one_event <- function(row) {
    c(
      "BEGIN:VEVENT",
      paste0("UID:user2026-", row$row_id, "@local"),
      paste0("DTSTAMP:", format(with_tz(now(), "UTC"), "%Y%m%dT%H%M%SZ")),
      paste0("DTSTART:", format(with_tz(row$datetime_start, "UTC"), "%Y%m%dT%H%M%SZ")),
      paste0("DTEND:", format(with_tz(row$datetime_end, "UTC"), "%Y%m%dT%H%M%SZ")),
      paste0("SUMMARY:", ics_escape(row$title)),
      paste0("DESCRIPTION:", ics_escape(paste(
        na.omit(c(row$track, row$type, row$session, row$speaker, row$url)),
        collapse = " — "
      ))),
      paste0("URL:", row$url),
      "END:VEVENT"
    )
  }

  c(
    "BEGIN:VCALENDAR",
    "VERSION:2.0",
    "PRODID:-//user2026-plan//shiny//EN",
    unlist(map(split(df, seq_len(nrow(df))), one_event)),
    "END:VCALENDAR"
  ) |>
    paste(collapse = "\r\n")
}

ui <- page_sidebar(
  title = "Min useR! 2026-plan",

  sidebar = sidebar(
    actionButton("reload", "Hent program igen"),
    selectInput("day", "Dag", choices = "Alle"),
    selectInput("track", "Track", choices = "Alle"),
    selectInput("type", "Type", choices = "Alle"),
    selectInput("session_filter", "Session", choices = "Alle"),
    textInput("q", "Søg", placeholder = "fx econometrics, shiny, ggplot2"),
    hr(),
    actionButton("add", "Tilføj markerede"),
    actionButton("remove", "Fjern markerede"),
    hr(),
    downloadButton("csv", "Download CSV"),
    downloadButton("ics", "Download kalender")
  ),

  layout_columns(
    card(card_header("Program"), DTOutput("program")),
    card(card_header("Valgt plan"), DTOutput("chosen"), textOutput("conflicts")),
    col_widths = c(7, 5)
  )
)

server <- function(input, output, session) {
  program <- reactiveVal(parse_contributions())
  chosen_ids <- reactiveVal(character())

  observeEvent(input$reload, {
    program(parse_contributions())
  })

  observe({
    df <- program()

    updateSelectInput(session, "day",
      choices = c("Alle", sort(unique(as.character(df$date))))
    )

    updateSelectInput(session, "track",
      choices = c("Alle", sort(unique(na.omit(df$track))))
    )

    updateSelectInput(session, "type",
      choices = c("Alle", sort(unique(na.omit(df$type))))
    )

    updateSelectInput(session, "session_filter",
      choices = c("Alle", sort(unique(na.omit(df$session))))
    )
  })

  filtered <- reactive({
    df <- program()

    if (input$day != "Alle") df <- filter(df, as.character(date) == input$day)
    if (input$track != "Alle") df <- filter(df, track == input$track)
    if (input$type != "Alle") df <- filter(df, type == input$type)
    if (input$session_filter != "Alle") df <- filter(df, session == input$session_filter)

    if (nzchar(input$q)) {
      df <- filter(df, str_detect(str_to_lower(text), fixed(str_to_lower(input$q))))
    }

    df
  })

  chosen <- reactive({
    program() |>
      filter(row_id %in% chosen_ids()) |>
      arrange(datetime_start)
  })

  observeEvent(input$add, {
    rows <- input$program_rows_selected
    ids <- filtered()$row_id[rows]
    chosen_ids(sort(unique(c(chosen_ids(), ids))))
  })

  observeEvent(input$remove, {
    rows <- input$chosen_rows_selected
    ids <- chosen()$row_id[rows]
    chosen_ids(setdiff(chosen_ids(), ids))
  })

  output$program <- renderDT({
    filtered() |>
      transmute(
        row_id,
        dato = date,
        tid = paste(start, end, sep = "–"),
        track,
        type,
        session,
        titel = title,
        speaker,
        abstract = description,
        link = url
      ) |>
      datatable(
        selection = "multiple",
        rownames = FALSE,
        escape = TRUE,
        options = list(
          pageLength = 12,
          scrollX = TRUE,
          columnDefs = list(list(visible = FALSE, targets = 0))
        )
      )
  })

  output$chosen <- renderDT({
    chosen() |>
      transmute(
        row_id,
        dato = date,
        tid = paste(start, end, sep = "–"),
        track,
        type,
        titel = title,
        speaker,
        link = url
      ) |>
      datatable(
        selection = "multiple",
        rownames = FALSE,
        escape = TRUE,
        options = list(
          pageLength = 12,
          scrollX = TRUE,
          columnDefs = list(list(visible = FALSE, targets = 0))
        )
      )
  })

  output$conflicts <- renderText({
    df <- chosen()

    if (nrow(df) < 2) return("Ingen overlap i dine valg.")

    clashes <- crossing(df, df, suffix = c("_a", "_b")) |>
      filter(row_id_a < row_id_b) |>
      filter(
        datetime_start_a < datetime_end_b,
        datetime_start_b < datetime_end_a
      )

    if (nrow(clashes) == 0) {
      "Ingen overlap i dine valg."
    } else {
      paste(
        "Overlap:",
        paste(unique(paste(clashes$date_a, clashes$start_a)), collapse = ", ")
      )
    }
  })

  output$csv <- downloadHandler(
    filename = function() "user2026_plan.csv",
    content = function(file) write_csv(chosen(), file)
  )

  output$ics <- downloadHandler(
    filename = function() "user2026_plan.ics",
    content = function(file) writeLines(make_ics(chosen()), file, useBytes = TRUE)
  )
}

shinyApp(ui, server)
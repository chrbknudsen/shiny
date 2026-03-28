library(shiny)
library(jsonlite)
library(dplyr)
library(purrr)
library(httr2)
library(htmltools)
library(tibble)

repo_api <- "https://api.github.com/repos/famelack/famelack-data/contents/tv/raw/countries"
raw_base <- "https://raw.githubusercontent.com/famelack/famelack-data/refs/heads/main/tv/raw/countries"

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) {
    y
  } else {
    x
  }
}

get_country_codes <- function() {
  resp <- request(repo_api) |>
    req_user_agent("shiny-tv-wall") |>
    req_perform()

  txt <- resp_body_string(resp)

  fromJSON(txt) |>
    as_tibble() |>
    filter(type == "file", grepl("\\.json$", name)) |>
    mutate(code = sub("\\.json$", "", name)) |>
    pull(code) |>
    sort()
}

parse_channel_item <- function(item, country_code) {
  stream_url <- if (!is.null(item$stream_urls) && length(item$stream_urls) > 0) {
    item$stream_urls[[1]]
  } else {
    NA_character_
  }

  youtube_url <- if (!is.null(item$youtube_urls) && length(item$youtube_urls) > 0) {
    item$youtube_urls[[1]]
  } else {
    NA_character_
  }

  type <- case_when(
    !is.na(stream_url) ~ "hls",
    is.na(stream_url) & !is.na(youtube_url) ~ "youtube",
    TRUE ~ NA_character_
  )

  tibble(
    nanoid = item$nanoid %||% NA_character_,
    name = item$name %||% NA_character_,
    stream_url = stream_url,
    youtube_url = youtube_url,
    type = type,
    url = coalesce(stream_url, youtube_url),
    country = item$country %||% country_code,
    isGeoBlocked = isTRUE(item$isGeoBlocked)
  )
}

get_channels_for_country <- function(country_code) {
  url <- sprintf("%s/%s.json", raw_base, country_code)

  resp <- request(url) |>
    req_user_agent("shiny-tv-wall") |>
    req_perform()

  txt <- resp_body_string(resp)

  items <- fromJSON(txt, simplifyVector = FALSE)

  map_dfr(items, parse_channel_item, country_code = country_code) |>
    filter(!is.na(url), nzchar(url)) |>
    distinct(url, .keep_all = TRUE)
}

looks_like_live_stream <- function(type, url, timeout_sec = 5) {
  if (is.na(url) || !nzchar(url)) {
    return(FALSE)
  }

  if (identical(type, "youtube")) {
    return(grepl("^https://www\\.youtube-nocookie\\.com/embed/", url))
  }

  if (!identical(type, "hls")) {
    return(FALSE)
  }

  if (!grepl("\\.m3u8($|\\?)", url, ignore.case = TRUE)) {
    return(FALSE)
  }

  out <- tryCatch(
    {
      resp <- request(url) |>
        req_method("GET") |>
        req_options(timeout = timeout_sec) |>
        req_headers(
          `User-Agent` = "Mozilla/5.0",
          Accept = "application/vnd.apple.mpegurl, application/x-mpegURL, */*"
        ) |>
        req_perform()

      status <- resp_status(resp)
      body <- tryCatch(resp_body_string(resp), error = function(e) "")

      status < 400 && grepl("#EXTM3U", body, fixed = TRUE)
    },
    error = function(e) FALSE
  )

  isTRUE(out)
}

prefilter_streams <- function(channels, max_check = 20) {
  if (nrow(channels) == 0) {
    return(channels)
  }

  youtube_ok <- channels |>
    filter(type == "youtube") |>
    mutate(live_ok = TRUE)

  hls_candidates <- channels |>
    filter(type == "hls")

  if (nrow(hls_candidates) > max_check) {
    hls_candidates <- hls_candidates |>
      slice_sample(n = max_check)
  }

  hls_checked <- hls_candidates |>
    mutate(live_ok = map2_lgl(type, url, looks_like_live_stream))

  bind_rows(youtube_ok, hls_checked) |>
    filter(live_ok)
}

pick_four <- function(channels) {
  n_pick <- min(4, nrow(channels))

  if (n_pick == 0) {
    return(channels[0, , drop = FALSE])
  }

  channels |>
    slice_sample(n = n_pick)
}

make_player <- function(id, name, type, url) {
  if (identical(type, "youtube")) {
    src <- paste0(
      url,
      if (grepl("\\?", url)) "&" else "?",
      "autoplay=1&mute=1&playsinline=1&controls=1"
    )

    return(
      div(
        class = "tv-box",
        div(class = "tv-title", name),
        tags$iframe(
          src = src,
          allow = "autoplay; encrypted-media; picture-in-picture",
          allowfullscreen = NA,
          referrerpolicy = "strict-origin-when-cross-origin",
          style = "width:100%; height:100%; border:0;"
        )
      )
    )
  }

  video_id <- paste0("video_", id)

  div(
    class = "tv-box",
    div(class = "tv-title", name),
    tags$video(
      id = video_id,
      muted = NA,
      autoplay = NA,
      playsinline = NA,
      controls = NA,
      style = "width:100%; height:100%; background:black;"
    ),
    tags$script(HTML(sprintf(
      "
      (function() {
        var video = document.getElementById('%s');
        var src = %s;
        if (!video) return;

        if (video.canPlayType('application/vnd.apple.mpegurl')) {
          video.src = src;
        } else if (window.Hls && Hls.isSupported()) {
          var hls = new Hls({
            enableWorker: true,
            lowLatencyMode: true
          });
          hls.loadSource(src);
          hls.attachMedia(video);
        } else {
          video.outerHTML =
            '<div style=\"color:white;padding:1rem;\">HLS understøttes ikke i denne browser.</div>';
          return;
        }

        video.muted = true;
        video.volume = 0;

        var p = video.play();
        if (p && typeof p.catch === 'function') {
          p.catch(function(e) {
            console.log('Autoplay failed', e);
          });
        }
      })();
      ",
      video_id,
      toJSON(url, auto_unbox = TRUE)
    )))
  )
}

ui <- fluidPage(
  tags$head(
    tags$script(src = "https://cdn.jsdelivr.net/npm/hls.js@latest"),
    tags$style(HTML("
      html, body {
        margin: 0;
        padding: 0;
        height: 100%;
        background: #111;
        color: white;
        overflow: hidden;
      }
      .topbar {
        min-height: 88px;
        padding: 10px 14px;
        box-sizing: border-box;
        background: #1b1b1b;
        border-bottom: 1px solid #333;
      }
      .toprow {
        display: flex;
        gap: 10px;
        align-items: center;
        flex-wrap: wrap;
      }
      .statusline {
        margin-top: 8px;
        font-size: 13px;
        color: #ccc;
        white-space: pre-wrap;
      }
      .grid-wrap {
        height: calc(100vh - 104px);
        padding: 10px;
        box-sizing: border-box;
      }
      .tv-grid {
        display: grid;
        grid-template-columns: 1fr 1fr;
        grid-template-rows: 1fr 1fr;
        gap: 10px;
        width: 100%;
        height: 100%;
      }
      .tv-box {
        position: relative;
        background: black;
        border: 1px solid #333;
        overflow: hidden;
      }
      .tv-title {
        position: absolute;
        top: 0;
        left: 0;
        z-index: 10;
        background: rgba(0, 0, 0, 0.65);
        padding: 6px 10px;
        font-size: 14px;
      }
      .tv-box iframe,
      .tv-box video {
        display: block;
        width: 100%;
        height: 100%;
      }
      .empty-box {
        display: flex;
        align-items: center;
        justify-content: center;
        border: 1px dashed #555;
        background: #181818;
        color: #bbb;
        font-size: 16px;
      }
    "))
  ),
  div(
    class = "topbar",
    div(
      class = "toprow",
      uiOutput("country_ui"),
      actionButton("reload_countries", "Genindlæs lande"),
      actionButton("load_four", "Vælg 4 nye"),
      checkboxInput("prefer_live", "Forsøg at vælge streams der ser levende ud", value = TRUE)
    ),
    div(class = "statusline", textOutput("status_text"))
  ),
  div(
    class = "grid-wrap",
    uiOutput("grid")
  )
)

server <- function(input, output, session) {
  country_codes <- reactiveVal(character())
  current_channels <- reactiveVal(tibble())
  selected_four <- reactiveVal(tibble())

  load_country_codes <- function(selected = "ar") {
    codes <- tryCatch(
      get_country_codes(),
      error = function(e) character()
    )

    country_codes(codes)

    if (length(codes) > 0) {
      updateSelectInput(
        session = session,
        inputId = "country_code",
        choices = codes,
        selected = if (selected %in% codes) selected else codes[[1]]
      )
    }
  }

  load_and_pick <- function(country_code, prefer_live = TRUE) {
    dat <- tryCatch(
      get_channels_for_country(country_code),
      error = function(e) tibble()
    )

    current_channels(dat)

    pool <- dat

    if (prefer_live && nrow(dat) > 0) {
      checked <- prefilter_streams(dat)

      if (nrow(checked) >= 4) {
        pool <- checked
      } else if (nrow(checked) > 0) {
        pool <- bind_rows(
          checked,
          anti_join(dat, checked, by = names(dat))
        )
      }
    }

    selected_four(pick_four(pool))
  }

  output$country_ui <- renderUI({
    selectInput(
      inputId = "country_code",
      label = "Landekode",
      choices = country_codes(),
      selected = if ("ar" %in% country_codes()) "ar" else NULL,
      width = "180px"
    )
  })

  observe({
    load_country_codes("ar")
  })

  observeEvent(input$reload_countries, {
    load_country_codes(isolate(input$country_code))
  })

  observeEvent(input$country_code, {
    req(input$country_code)
    load_and_pick(input$country_code, isTRUE(input$prefer_live))
  }, ignoreInit = FALSE)

  observeEvent(input$load_four, {
    req(input$country_code)
    load_and_pick(input$country_code, isTRUE(input$prefer_live))
  })

  output$status_text <- renderText({
    total <- nrow(current_channels())
    shown <- nrow(selected_four())
    cc <- input$country_code %||% ""
    example <- if (total > 0) current_channels()$name[[1]] else "ingen"

    paste0(
      "Land: ", cc,
      " | kandidater: ", total,
      " | vist: ", shown,
      " | første kanal: ", example
    )
  })

  output$grid <- renderUI({
    dat <- selected_four()

    if (nrow(dat) == 0) {
      return(
        div(
          class = "tv-grid",
          div(class = "empty-box", "Ingen streams fundet"),
          div(class = "empty-box", ""),
          div(class = "empty-box", ""),
          div(class = "empty-box", "")
        )
      )
    }

    boxes <- lapply(seq_len(min(4, nrow(dat))), function(i) {
      make_player(
        id = i,
        name = dat$name[[i]],
        type = dat$type[[i]],
        url = dat$url[[i]]
      )
    })

    while (length(boxes) < 4) {
      boxes[[length(boxes) + 1]] <- div(class = "empty-box", "Ingen ekstra stream")
    }

    div(class = "tv-grid", tagList(boxes))
  })
}

shinyApp(ui, server)

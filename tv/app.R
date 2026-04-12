library(shiny)
library(shinyWidgets)
library(dplyr)
library(readr)
library(purrr)
library(tibble)
library(htmltools)

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

source("tv/get_channel_data.R")

channels <- get_famelack_channels(
  cache_dir = "data-cache/famelack/countries",
  update = FALSE,
  verbose = TRUE
)

channel_categories <- get_famelack_channel_categories(
  cache_dir = "data-cache/famelack/categories",
  update = FALSE,
  verbose = TRUE
)

category_labels <- channel_categories |>
  distinct(nanoid, category) |>
  group_by(nanoid) |>
  summarise(
    categories = paste(sort(unique(category)), collapse = ", "),
    .groups = "drop"
  )

channels_meta <- channels |>
  left_join(category_labels, by = "nanoid") |>
  mutate(
    categories = coalesce(categories, ""),
    label = if_else(
      nzchar(categories),
      paste0(name, " (", toupper(country), ") — ", categories),
      paste0(name, " (", toupper(country), ")")
    )
  ) |>
  distinct(nanoid, .keep_all = TRUE)

## youtube_embed_url funktion ----
youtube_embed_url <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }

  x <- trimws(x)

  if (grepl("youtube\\.com/watch\\?", x)) {
    m <- regexec("[?&]v=([^&]+)", x)
    reg <- regmatches(x, m)[[1]]
    if (length(reg) >= 2) {
      return(sprintf(
        "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
        reg[2]
      ))
    }
  }

  if (grepl("youtu\\.be/", x)) {
    id <- sub("^.*youtu\\.be/([^?&/]+).*$", "\\1", x)
    return(sprintf(
      "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
      id
    ))
  }

  if (grepl("youtube\\.com/embed/", x)) {
    id <- sub("^.*youtube\\.com/embed/([^?&/]+).*$", "\\1", x)
    return(sprintf(
      "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
      id
    ))
  }

  if (grepl("youtube\\.com/live/", x)) {
    id <- sub("^.*youtube\\.com/live/([^?&/]+).*$", "\\1", x)
    return(sprintf(
      "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
      id
    ))
  }

  NA_character_
}

## make_player_tage funktion ----
youtube_embed_url <- function(x) {
  if (length(x) == 0 || is.na(x) || !nzchar(x)) {
    return(NA_character_)
  }

  x <- trimws(x)

  if (grepl("youtube\\.com/watch\\?", x)) {
    m <- regexec("[?&]v=([^&]+)", x)
    reg <- regmatches(x, m)[[1]]
    if (length(reg) >= 2) {
      return(sprintf(
        "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
        reg[2]
      ))
    }
  }

  if (grepl("youtu\\.be/", x)) {
    id <- sub("^.*youtu\\.be/([^?&/]+).*$", "\\1", x)
    return(sprintf(
      "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
      id
    ))
  }

  if (grepl("youtube\\.com/embed/", x)) {
    id <- sub("^.*youtube\\.com/embed/([^?&/]+).*$", "\\1", x)
    return(sprintf(
      "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
      id
    ))
  }

  if (grepl("youtube-nocookie\\.com/embed/", x)) {
    id <- sub("^.*youtube-nocookie\\.com/embed/([^?&/]+).*$", "\\1", x)
    return(sprintf(
      "https://www.youtube-nocookie.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
      id
    ))
  }

  if (grepl("youtube\\.com/live/", x)) {
    id <- sub("^.*youtube\\.com/live/([^?&/]+).*$", "\\1", x)
    return(sprintf(
      "https://www.youtube.com/embed/%s?autoplay=1&mute=1&playsinline=1&rel=0",
      id
    ))
  }

  NA_character_
}

channel_tile_ui <- function(row) {
  tags$div(
    class = "channel-tile",
    `data-nanoid` = row$nanoid,
    tags$div(
      class = "channel-tile-header",
      tags$div(
        class = "channel-title-wrap",
        tags$div(class = "channel-title", row$name),
        tags$div(
          class = "channel-subtitle",
          paste0(
            toupper(row$country),
            if (!is.na(row$categories) && nzchar(row$categories)) {
              paste0(" · ", row$categories)
            } else {
              ""
            }
          )
        )
      ),
      tags$button(
        class = "close-tile-btn",
        title = "Luk kanal",
        onclick = sprintf(
          "Shiny.setInputValue('remove_channel', '%s', {priority: 'event'})",
          row$nanoid
        ),
        HTML("&times;")
      )
    ),
    tags$div(
      class = "channel-tile-body",
      make_player_tag(
        type = row$type,
        url = row$url,
        nanoid = row$nanoid
      )
    )
  )
}

ui <- fluidPage(
  tags$head(
    tags$meta(name = "viewport", content = "width=device-width, initial-scale=1"),
    tags$script(src = "https://cdn.jsdelivr.net/npm/hls.js@latest"),
    tags$style(HTML("
      html, body {
        height: 100%;
        margin: 0;
        padding: 0;
        overflow: hidden;
        background: #111;
      }

      body > .container-fluid {
        height: 100vh;
        padding: 0;
      }

      .app-shell {
        height: 100vh;
        display: flex;
        flex-direction: column;
        background: #111;
        color: #f5f5f5;
      }

      .control-bar {
        flex: 0 0 auto;
        padding: 8px 10px;
        background: #1a1a1a;
        border-bottom: 1px solid #2b2b2b;
        z-index: 10;
      }

      .control-grid {
        display: grid;
        grid-template-columns: 1.1fr 1.1fr 2fr auto;
        gap: 10px;
        align-items: start;
      }

      .control-label {
        font-size: 12px;
        font-weight: 600;
        margin-bottom: 4px;
        color: #d9d9d9;
      }

      .btn-toolbar-wrap {
        display: flex;
        align-items: end;
        gap: 8px;
        padding-top: 18px;
      }

      .statline {
        font-size: 12px;
        color: #cccccc;
        margin-top: 6px;
      }

      .viewer-wrap {
        flex: 1 1 auto;
        min-height: 0;
        min-width: 0;
        padding: 8px;
        background: #111;
        overflow: hidden;
      }

      .viewer-wrap > .shiny-html-output {
        width: 100%;
        height: 100%;
      }

      #channel-grid {
        --cols: 1;
        width: 100%;
        height: 100%;
        display: grid;
        grid-template-columns: repeat(var(--cols), minmax(0, 1fr));
        gap: 8px;
        align-content: center;
        justify-content: center;
        overflow: hidden;
      }

      .channel-tile {
        width: 100%;
        height: 100%;
        background: #000;
        border: 1px solid #252525;
        border-radius: 10px;
        overflow: hidden;
        display: flex;
        flex-direction: column;
        min-width: 0;
        min-height: 0;
      }

      .channel-tile-header {
        flex: 0 0 auto;
        display: flex;
        align-items: center;
        justify-content: space-between;
        gap: 8px;
        padding: 6px 8px;
        background: rgba(20,20,20,0.95);
        border-bottom: 1px solid #1f1f1f;
      }

      .channel-title-wrap {
        min-width: 0;
      }

      .channel-title {
        font-size: 13px;
        font-weight: 700;
        line-height: 1.2;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .channel-subtitle {
        font-size: 11px;
        color: #b8b8b8;
        white-space: nowrap;
        overflow: hidden;
        text-overflow: ellipsis;
      }

      .close-tile-btn {
        background: #2c2c2c;
        color: #fff;
        border: 0;
        border-radius: 6px;
        width: 28px;
        height: 28px;
        line-height: 1;
        font-size: 20px;
        cursor: pointer;
        flex: 0 0 auto;
      }

      .close-tile-btn:hover {
        background: #444;
      }

      .channel-tile-body {
        flex: 1 1 auto;
        min-height: 0;
        min-width: 0;
        background: #000;
        overflow: hidden;
        display: flex;
      }

      .channel-player,
      .channel-player-wrap {
        width: 100%;
        height: 100%;
        border: 0;
        display: block;
        background: #000;
      }

      .player-fallback,
      .empty-state {
        width: 100%;
        height: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        color: #cfcfcf;
        background: #181818;
        text-align: center;
        padding: 20px;
      }

      @media (max-width: 1100px) {
        .control-grid {
          grid-template-columns: 1fr;
        }

        .btn-toolbar-wrap {
          padding-top: 0;
        }
      }
    ")),
    tags$script(HTML("
      function bestColsForGrid() {
        const host = document.querySelector('.viewer-wrap');
        const grid = document.getElementById('channel-grid');
        if (!host || !grid) return 1;

        const tiles = Array.from(grid.querySelectorAll('.channel-tile'));
        const n = tiles.length;
        if (n <= 1) return 1;

        const gap = 8;
        const hostStyle = window.getComputedStyle(host);
        const padX =
          parseFloat(hostStyle.paddingLeft || 0) +
          parseFloat(hostStyle.paddingRight || 0);
        const padY =
          parseFloat(hostStyle.paddingTop || 0) +
          parseFloat(hostStyle.paddingBottom || 0);

        const W = host.clientWidth - padX;
        const H = host.clientHeight - padY;

        const headerHeight = 43;
        const ratio = 16 / 9;

        let bestCols = 1;
        let bestArea = 0;

        for (let cols = 1; cols <= n; cols++) {
          const rows = Math.ceil(n / cols);

          const tileW = (W - gap * (cols - 1)) / cols;
          const tileH = (H - gap * (rows - 1)) / rows;

          const videoH = tileH - headerHeight;
          const videoW = Math.min(tileW, videoH * ratio);
          const usableVideoH = videoW / ratio;

          if (videoW <= 0 || usableVideoH <= 0) continue;

          const totalArea = videoW * usableVideoH * n;

          if (totalArea > bestArea) {
            bestArea = totalArea;
            bestCols = cols;
          }
        }

        return bestCols;
      }

      function layoutChannelGrid() {
        const grid = document.getElementById('channel-grid');
        if (!grid) return;

        const tiles = Array.from(grid.querySelectorAll('.channel-tile'));
        const n = tiles.length;

        if (n === 0) {
          grid.style.setProperty('--cols', 1);
          return;
        }

        const cols = bestColsForGrid();
        grid.style.setProperty('--cols', cols);
      }

      function initHlsPlayers(scope) {
        const root = scope || document;
        const videos = root.querySelectorAll('video.hls-player');

        videos.forEach(function(video) {
          if (video.dataset.initialized === 'true') return;

          const src = video.dataset.src;
          if (!src) return;

          video.muted = true;
          video.autoplay = true;
          video.playsInline = true;

          if (video.canPlayType('application/vnd.apple.mpegurl')) {
            video.src = src;
            video.dataset.initialized = 'true';
            video.play().catch(() => {});
            return;
          }

          if (window.Hls && Hls.isSupported()) {
            const hls = new Hls({ autoStartLoad: true });
            hls.loadSource(src);
            hls.attachMedia(video);
            video.dataset.initialized = 'true';
            video._hlsInstance = hls;

            hls.on(Hls.Events.MANIFEST_PARSED, function() {
              video.play().catch(() => {});
            });
          }
        });
      }

      function refreshGrid() {
        layoutChannelGrid();
        initHlsPlayers(document);
      }

      function attachObservers() {
        const host = document.querySelector('.viewer-wrap');
        const grid = document.getElementById('channel-grid');

        if (!host || !grid) return;

        if (!window._channelGridResizeObserver) {
          window._channelGridResizeObserver = new ResizeObserver(function() {
            refreshGrid();
          });
          window._channelGridResizeObserver.observe(host);
        }

        if (!window._channelGridMutationObserver) {
          window._channelGridMutationObserver = new MutationObserver(function() {
            refreshGrid();
          });
          window._channelGridMutationObserver.observe(grid, {
            childList: true,
            subtree: true
          });
        }
      }

      document.addEventListener('DOMContentLoaded', function() {
        setTimeout(function() {
          attachObservers();
          refreshGrid();
        }, 50);
      });

      document.addEventListener('shiny:value', function() {
        setTimeout(function() {
          attachObservers();
          refreshGrid();
        }, 50);
      });

      window.addEventListener('resize', function() {
        refreshGrid();
      });
    "))
  ),

  div(
    class = "app-shell",
    div(
      class = "control-bar",
      div(
        class = "control-grid",
        div(
          div(class = "control-label", "Land"),
          pickerInput(
            inputId = "countries",
            label = NULL,
            choices = sort(unique(channels_meta$country)),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              noneSelectedText = "Alle lande",
              selectedTextFormat = "count > 3"
            )
          )
        ),
        div(
          div(class = "control-label", "Kategorier"),
          pickerInput(
            inputId = "categories",
            label = NULL,
            choices = sort(unique(channel_categories$category)),
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              noneSelectedText = "Alle kategorier",
              selectedTextFormat = "count > 3"
            )
          )
        ),
        div(
          div(class = "control-label", "Kanaler"),
          pickerInput(
            inputId = "selected_channels",
            label = NULL,
            choices = NULL,
            selected = NULL,
            multiple = TRUE,
            options = pickerOptions(
              actionsBox = TRUE,
              liveSearch = TRUE,
              noneSelectedText = "Vælg kanaler",
              selectedTextFormat = "count > 3"
            )
          )
        ),
        div(
          class = "btn-toolbar-wrap",
          actionButton("select_all_filtered", "Vælg alle filtrerede"),
          actionButton("clear_selection", "Ryd")
        )
      ),
      div(class = "statline", textOutput("status_line"))
    ),
    div(
      class = "viewer-wrap",
      uiOutput("channel_grid_ui", style = "height: 100%;")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    selected = character()
  )

  filtered_channel_ids <- reactive({
    out <- channels_meta

    if (!is.null(input$countries) && length(input$countries) > 0) {
      out <- out |>
        filter(country %in% input$countries)
    }

    if (!is.null(input$categories) && length(input$categories) > 0) {
      ids_in_categories <- channel_categories |>
        filter(category %in% input$categories) |>
        distinct(nanoid) |>
        pull(nanoid)

      out <- out |>
        filter(nanoid %in% ids_in_categories)
    }

    out |>
      distinct(nanoid) |>
      pull(nanoid)
  })

  filtered_channel_choices <- reactive({
    channels_meta |>
      filter(nanoid %in% filtered_channel_ids()) |>
      arrange(country, name) |>
      distinct(nanoid, .keep_all = TRUE)
  })

  observe({
    choices_tbl <- filtered_channel_choices()

    choices <- stats::setNames(
      choices_tbl$nanoid,
      choices_tbl$label
    )

    selected_valid <- intersect(rv$selected, choices_tbl$nanoid)

    updatePickerInput(
      session = session,
      inputId = "selected_channels",
      choices = choices,
      selected = selected_valid
    )

    rv$selected <- selected_valid
  })

  observeEvent(input$selected_channels, {
    rv$selected <- input$selected_channels %||% character()
  }, ignoreNULL = FALSE)

  observeEvent(input$select_all_filtered, {
    rv$selected <- filtered_channel_choices()$nanoid

    updatePickerInput(
      session = session,
      inputId = "selected_channels",
      selected = rv$selected
    )
  })

  observeEvent(input$clear_selection, {
    rv$selected <- character()

    updatePickerInput(
      session = session,
      inputId = "selected_channels",
      selected = character()
    )
  })

  observeEvent(input$remove_channel, {
    rv$selected <- setdiff(rv$selected, input$remove_channel)

    updatePickerInput(
      session = session,
      inputId = "selected_channels",
      selected = rv$selected
    )
  })

  selected_channel_data <- reactive({
    if (length(rv$selected) == 0) {
      return(channels_meta[0, ])
    }

    channels_meta |>
      filter(nanoid %in% rv$selected) |>
      mutate(order_index = match(nanoid, rv$selected)) |>
      arrange(order_index) |>
      select(-order_index)
  })

  output$status_line <- renderText({
    n_candidates <- nrow(filtered_channel_choices())
    n_selected <- length(rv$selected)

    paste0(
      "Filtrerede kanaler: ", n_candidates,
      " · Valgte kanaler: ", n_selected
    )
  })

  output$channel_grid_ui <- renderUI({
    dat <- selected_channel_data()

    if (nrow(dat) == 0) {
      return(
        div(
          id = "channel-grid",
          div(
            class = "empty-state",
            "Vælg en eller flere kanaler i dropdown-menuen."
          )
        )
      )
    }

    div(
      id = "channel-grid",
      tagList(
        lapply(seq_len(nrow(dat)), function(i) {
          channel_tile_ui(dat[i, ])
        })
      )
    )
  })
}

shinyApp(ui, server)

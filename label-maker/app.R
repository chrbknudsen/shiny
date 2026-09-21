required_packages <- c(
  "shiny",
  "DBI",
  "RSQLite",
  "magick",
  "rsvg",
  "uuid",
  "yaml",
  "sysfonts",
  "showtext"
)

missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0) {
  stop(
    paste0(
      "Følgende pakker mangler: ",
      paste(missing_packages, collapse = ", "),
      ". Se README.md."
    ),
    call. = FALSE
  )
}

library(shiny)

options(shiny.maxRequestSize = 10 * 1024^2)

r_files <- list.files(
  file.path(getwd(), "R"),
  pattern = "\\.[Rr]$",
  full.names = TRUE
)

invisible(
  lapply(
    r_files,
    source,
    local = FALSE,
    encoding = "UTF-8"
  )
)

image_library_paths <- initialize_image_library()
label_config <- read_label_config()
register_label_fonts(label_config)
admin_configuration <- admin_token_status()

if (identical(admin_configuration$reason, "too_short")) {
  warning(
    paste(
      "LABEL_ADMIN_TOKEN er kortere end 32 bytes.",
      "Biblioteket starter derfor i skrivebeskyttet tilstand."
    ),
    call. = FALSE
  )
}

label_sizes <- label_config$size_table
default_label_size <- label_sizes[1, , drop = FALSE]
thumbnail_resource_prefix <- paste0(
  "label-library-thumbnails-",
  uuid::UUIDgenerate()
)
addResourcePath(
  thumbnail_resource_prefix,
  image_library_paths$thumbnails
)

slugify <- function(text) {
  text <- iconv(text, from = "", to = "ASCII//TRANSLIT")
  text <- tolower(text)
  text <- gsub("[^a-z0-9]+", "-", text)
  text <- gsub("(^-+|-+$)", "", text)

  if (!nzchar(text)) {
    "etiket"
  } else {
    text
  }
}

ui <- fluidPage(
  tags$head(
    tags$meta(
      name = "viewport",
      content = "width=device-width, initial-scale=1"
    ),
    includeCSS(file.path("www", "app.css"))
  ),

  div(
    class = "app-header",
    h1("Nusse Label Maker"),
    p("Etiketter i konfigurerbare mål med genbrugelige motiver")
  ),

  sidebarLayout(
    sidebarPanel(
      width = 4,

      tabsetPanel(
        id = "control_tab",

        tabPanel(
          "Etiket",
          br(),
          textInput(
            "title",
            "Titel",
            value = "Lys jævner"
          ),
          textInput(
            "subtitle",
            "Undertekst",
            value = ""
          ),
          dateInput(
            "date",
            "Dato",
            value = Sys.Date(),
            format = "dd-mm-yyyy",
            language = "da"
          ),
          fluidRow(
            column(
              6,
              textInput(
                "batch",
                "Batch",
                value = "03"
              )
            ),
            column(
              6,
              textInput(
                "code",
                "Kode",
                value = "LA67F"
              )
            )
          ),
          textInput(
            "footer",
            "Bundtekst",
            value = "Nusse Mad Science Inc."
          ),
          fluidRow(
            column(
              6,
              selectInput(
                "display_font_id",
                "Skrift: titel",
                choices = label_config$fonts$choices,
                selected = label_config$fonts$default_display
              )
            ),
            column(
              6,
              selectInput(
                "typewriter_font_id",
                "Skrift: data og bundtekst",
                choices = label_config$fonts$choices,
                selected = label_config$fonts$default_typewriter
              )
            )
          ),
          helpText("Skriftvalgene defineres i config.yml."),
          selectInput(
            "paper_colour",
            "Papir",
            choices = paper_colours,
            selected = paper_colours[["Varmt papir"]]
          ),
          sliderInput(
            "aging",
            "Ældning",
            min = 0,
            max = 100,
            value = 35,
            step = 1
          ),
          selectInput(
            "size_preset",
            "Foruddefineret størrelse",
            choices = label_config$size_choices,
            selected = default_label_size$id[[1L]]
          ),
          helpText("Størrelserne redigeres i config.yml."),
          fluidRow(
            column(
              6,
              numericInput(
                "width_mm",
                "Bredde (mm)",
                value = default_label_size$width_mm[[1L]],
                min = 20,
                max = A4_WIDTH_MM - 2 * label_config$pdf$margin_mm,
                step = 1
              )
            ),
            column(
              6,
              numericInput(
                "height_mm",
                "Højde (mm)",
                value = default_label_size$height_mm[[1L]],
                min = 20,
                max = A4_HEIGHT_MM - 2 * label_config$pdf$margin_mm,
                step = 1
              )
            )
          )
        ),

        tabPanel(
          "Motiv",
          br(),
          radioButtons(
            "image_source",
            "Billedkilde",
            choices = c(
              "Tilfældigt motiv" = "generated",
              "Billedbibliotek" = "library"
            ),
            selected = "generated"
          ),

          conditionalPanel(
            condition = "input.image_source == 'generated'",
            selectInput(
              "motif_type",
              "Motivtype",
              choices = motif_choices,
              selected = "1"
            ),
            actionButton(
              "new_motif",
              "Nyt motiv",
              class = "btn-primary"
            ),
            div(
              class = "seed-line",
              "Seed: ",
              textOutput(
                "motif_seed",
                inline = TRUE
              )
            ),
            hr(),
            uiOutput("generated_write_controls")
          ),

          conditionalPanel(
            condition = "input.image_source == 'library'",
            textInput(
              "library_search",
              "Søg i biblioteket",
              value = ""
            ),
            selectInput(
              "library_image",
              "Billede",
              choices = character()
            ),
            uiOutput("library_gallery"),
            div(
              class = "library-meta",
              textOutput("library_metadata")
            ),
            selectInput(
              "image_fit",
              "Tilpasning",
              choices = c(
                "Hele billedet" = "contain",
                "Fyld og beskær" = "fill"
              ),
              selected = "contain"
            ),
            checkboxInput(
              "grayscale",
              "Vis som sort-hvid",
              value = TRUE
            )
          )
        ),

        tabPanel(
          "Upload",
          br(),
          p(
            class = "help-block",
            paste(
              "Uploadede billeder renses, konverteres til PNG",
              "og gemmes til senere brug."
            )
          ),
          uiOutput("upload_write_controls"),
          hr(),
          strong(textOutput("library_count"))
        ),

        tabPanel(
          "Adgang",
          br(),
          uiOutput("admin_access_panel")
        )
      ),

      hr(),

      selectInput(
        "png_dpi",
        "PNG-opløsning",
        choices = c(
          "300 dpi" = 300,
          "600 dpi" = 600
        ),
        selected = 300
      ),
      selectInput(
        "pdf_copies",
        "Etiketter på A4-arket",
        choices = c("1 etiket" = 1),
        selected = 1
      ),
      div(
        class = "pdf-capacity",
        textOutput("pdf_capacity")
      ),
      fluidRow(
        column(
          6,
          downloadButton(
            "download_png",
            "PNG",
            class = "download-button"
          )
        ),
        column(
          6,
          downloadButton(
            "download_pdf",
            "PDF",
            class = "download-button"
          )
        )
      )
    ),

    mainPanel(
      width = 8,
      div(
        class = "preview-shell",
        div(
          class = "preview-heading",
          span("Live preview"),
          span(
            class = "size-badge",
            textOutput(
              "physical_size",
              inline = TRUE
            )
          )
        ),
        div(
          class = "label-preview",
          uiOutput("label_preview_ui")
        )
      )
    )
  )
)

server <- function(input, output, session) {
  motif_seed <- reactiveVal(
    sample.int(.Machine$integer.max, 1)
  )
  library_revision <- reactiveVal(0L)
  pending_library_id <- reactiveVal(NULL)
  admin_authorized <- reactiveVal(FALSE)

  write_access_notice <- function() {
    if (isTRUE(admin_configuration$configured)) {
      paste(
        "Biblioteket er skrivebeskyttet.",
        "Lås op under fanen Adgang for at gemme billeder."
      )
    } else {
      paste(
        "Biblioteket er skrivebeskyttet på denne server.",
        "Administratornøglen er ikke konfigureret."
      )
    }
  }

  require_library_write_access <- function() {
    if (isTRUE(admin_authorized())) {
      return(TRUE)
    }

    showNotification(
      "Administratoradgang kræves for at ændre billedbiblioteket.",
      type = "error",
      duration = 6
    )
    FALSE
  }

  output$generated_write_controls <- renderUI({
    if (!isTRUE(admin_authorized())) {
      return(
        div(
          class = "write-access-note is-locked",
          tags$span(class = "glyphicon glyphicon-lock"),
          write_access_notice()
        )
      )
    }

    tagList(
      div(
        class = "write-access-note is-unlocked",
        tags$span(class = "glyphicon glyphicon-ok-circle"),
        "Administratoradgang er aktiv i denne session."
      ),
      textInput(
        "generated_name",
        "Navn i biblioteket",
        value = ""
      ),
      textInput(
        "generated_tags",
        "Mærkater",
        value = "genereret"
      ),
      actionButton(
        "save_generated",
        "Gem motiv i bibliotek"
      )
    )
  })

  output$upload_write_controls <- renderUI({
    if (!isTRUE(admin_authorized())) {
      return(
        div(
          class = "write-access-note is-locked",
          tags$span(class = "glyphicon glyphicon-lock"),
          write_access_notice()
        )
      )
    }

    tagList(
      div(
        class = "write-access-note is-unlocked",
        tags$span(class = "glyphicon glyphicon-ok-circle"),
        "Administratoradgang er aktiv i denne session."
      ),
      fileInput(
        "upload_image",
        "Vælg billede",
        accept = c(
          ".png",
          ".jpg",
          ".jpeg",
          ".webp",
          ".svg",
          "image/png",
          "image/jpeg",
          "image/webp",
          "image/svg+xml"
        )
      ),
      textInput(
        "upload_name",
        "Navn i biblioteket",
        value = ""
      ),
      textInput(
        "upload_tags",
        "Mærkater",
        value = ""
      ),
      actionButton(
        "upload_and_save",
        "Upload, gem og brug",
        class = "btn-primary"
      )
    )
  })

  output$admin_access_panel <- renderUI({
    if (isTRUE(admin_authorized())) {
      return(
        div(
          class = "admin-access-panel is-unlocked",
          h4("Skriveadgang er låst op"),
          p(
            paste(
              "Du kan nu uploade billeder og gemme genererede motiver",
              "i denne browsersession."
            )
          ),
          actionButton(
            "admin_lock",
            "Lås skriveadgang",
            icon = icon("lock")
          )
        )
      )
    }

    if (!isTRUE(admin_configuration$configured)) {
      reason <- if (identical(admin_configuration$reason, "too_short")) {
        "Nøglen skal være mindst 32 bytes lang."
      } else {
        "Miljøvariablen LABEL_ADMIN_TOKEN er ikke sat."
      }

      return(
        div(
          class = "admin-access-panel is-locked",
          h4("Skriveadgang er slået fra"),
          p(reason),
          p(
            class = "help-block",
            "Genstart appen efter at serverindstillingen er ændret."
          )
        )
      )
    }

    div(
      class = "admin-access-panel is-locked",
      h4("Administratoradgang"),
      p(
        paste(
          "Oplåsningen gælder kun denne browsersession.",
          "Brug altid HTTPS, når appen ligger på internettet."
        )
      ),
      passwordInput(
        "admin_token",
        "Administratornøgle",
        value = "",
        width = "100%",
        placeholder = "Indtast serverens administratornøgle"
      ),
      actionButton(
        "admin_unlock",
        "Lås op",
        class = "btn-primary",
        icon = icon("unlock")
      )
    )
  })

  observeEvent(
    input$admin_unlock,
    {
      supplied_token <- isolate(input$admin_token)
      updateTextInput(session, "admin_token", value = "")

      if (verify_admin_token(supplied_token)) {
        admin_authorized(TRUE)
        showNotification(
          "Skriveadgang er låst op i denne session.",
          type = "message"
        )
      } else {
        admin_authorized(FALSE)
        showNotification(
          "Administratornøglen blev ikke godkendt.",
          type = "error",
          duration = 6
        )
      }
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$admin_lock,
    {
      admin_authorized(FALSE)
      showNotification(
        "Skriveadgang er låst.",
        type = "message"
      )
    },
    ignoreInit = TRUE
  )

  observeEvent(
    input$new_motif,
    {
      motif_seed(sample.int(.Machine$integer.max, 1))
    }
  )

  observeEvent(
    input$size_preset,
    {
      selected_size <- label_sizes[
        label_sizes$id == input$size_preset,
        ,
        drop = FALSE
      ]
      req(nrow(selected_size) == 1L)

      updateNumericInput(
        session,
        "width_mm",
        value = selected_size$width_mm[[1L]]
      )
      updateNumericInput(
        session,
        "height_mm",
        value = selected_size$height_mm[[1L]]
      )
    },
    ignoreInit = TRUE
  )

  output$motif_seed <- renderText({
    format(motif_seed(), scientific = FALSE)
  })

  library_entries <- reactive({
    invalidateLater(3000, session)
    library_revision()

    search <- if (is.null(input$library_search)) {
      ""
    } else {
      input$library_search
    }

    library_list_images(
      image_library_paths,
      search = search
    )
  })

  observe({
    entries <- library_entries()
    current <- isolate(input$library_image)
    pending <- isolate(pending_library_id())

    if (nrow(entries) == 0) {
      updateSelectInput(
        session,
        "library_image",
        choices = character(),
        selected = character()
      )
      return()
    }

    choice_labels <- ifelse(
      nzchar(entries$tags),
      paste0(entries$display_name, " · ", entries$tags),
      entries$display_name
    )
    choices <- stats::setNames(entries$id, choice_labels)

    selected <- if (!is.null(pending) && pending %in% entries$id) {
      pending
    } else if (!is.null(current) && current %in% entries$id) {
      current
    } else {
      entries$id[1]
    }

    updateSelectInput(
      session,
      "library_image",
      choices = choices,
      selected = selected
    )

    if (!is.null(pending) && identical(selected, pending)) {
      pending_library_id(NULL)
    }
  })

  selected_library_image <- reactive({
    entries <- library_entries()
    id <- input$library_image

    if (
      is.null(id) ||
        length(id) != 1L ||
        is.na(id) ||
        !nzchar(id) ||
        !id %in% entries$id
    ) {
      return(NULL)
    }

    library_get_image(
      image_library_paths,
      id
    )
  })

  output$library_gallery <- renderUI({
    entries <- library_entries()

    if (nrow(entries) == 0L) {
      return(
        div(
          class = "library-gallery-empty",
          "Ingen billeder matcher søgningen."
        )
      )
    }

    selected_id <- input$library_image
    cards <- lapply(
      seq_len(nrow(entries)),
      function(index) {
        id <- entries$id[[index]]
        filename <- entries$thumbnail_filename[[index]]
        is_selected <- identical(id, selected_id)
        source_label <- if (entries$source[[index]] == "upload") {
          "Uploadet"
        } else {
          "Genereret"
        }

        tags$button(
          type = "button",
          class = paste(
            "library-card",
            if (is_selected) "is-selected" else ""
          ),
          title = paste("Vælg", entries$display_name[[index]]),
          onclick = sprintf(
            paste0(
              "Shiny.setInputValue('library_gallery_choice', ",
              "'%s', {priority: 'event'});"
            ),
            id
          ),
          tags$img(
            src = paste0(
              thumbnail_resource_prefix,
              "/",
              utils::URLencode(filename, reserved = TRUE)
            ),
            alt = entries$display_name[[index]]
          ),
          tags$span(
            class = "library-card-name",
            entries$display_name[[index]]
          ),
          tags$span(
            class = "library-card-source",
            source_label
          )
        )
      }
    )

    div(
      class = "library-gallery",
      do.call(tagList, cards)
    )
  })

  observeEvent(
    input$library_gallery_choice,
    {
      entries <- library_entries()
      selected_id <- input$library_gallery_choice

      if (
        length(selected_id) == 1L &&
          !is.na(selected_id) &&
          selected_id %in% entries$id
      ) {
        updateSelectInput(
          session,
          "library_image",
          selected = selected_id
        )
      }
    }
  )

  output$library_metadata <- renderText({
    entry <- selected_library_image()

    if (is.null(entry)) {
      return("Biblioteket er tomt.")
    }

    source_label <- if (entry$source[1] == "generated") {
      paste0(
        "Genereret motiv ",
        entry$motif_type[1],
        " · seed ",
        entry$seed[1]
      )
    } else {
      "Uploadet billede"
    }

    if (nzchar(entry$tags[1])) {
      paste0(source_label, " · ", entry$tags[1])
    } else {
      source_label
    }
  })

  output$library_count <- renderText({
    invalidateLater(3000, session)
    library_revision()

    count <- nrow(
      library_list_images(
        image_library_paths,
        search = ""
      )
    )

    paste0(
      count,
      if (count == 1) " billede i biblioteket" else " billeder i biblioteket"
    )
  })

  observeEvent(
    input$save_generated,
    {
      if (!require_library_write_access()) {
        return()
      }

      type <- as_motif_type(input$motif_type)
      seed <- as.integer(motif_seed())
      default_name <- paste0(
        motif_names[[as.character(type)]],
        " ",
        seed
      )
      display_name <- trimws(input$generated_name)

      if (!nzchar(display_name)) {
        display_name <- default_name
      }

      temporary_png <- tempfile(fileext = ".png")
      on.exit(unlink(temporary_png, force = TRUE), add = TRUE)

      tryCatch(
        {
          render_motif_png(
            temporary_png,
            type = type,
            seed = seed
          )

          id <- library_add_image(
            image_library_paths,
            source_path = temporary_png,
            original_name = "generated.png",
            display_name = display_name,
            source = "generated",
            motif_type = type,
            seed = seed,
            tags = input$generated_tags
          )

          pending_library_id(id)
          updateTextInput(
            session,
            "library_search",
            value = ""
          )
          library_revision(library_revision() + 1L)
          updateRadioButtons(
            session,
            "image_source",
            selected = "library"
          )
          showNotification(
            "Motivet er gemt i billedbiblioteket.",
            type = "message"
          )
        },
        error = function(error) {
          showNotification(
            conditionMessage(error),
            type = "error",
            duration = 8
          )
        }
      )
    }
  )

  observeEvent(
    input$upload_and_save,
    {
      if (!require_library_write_access()) {
        return()
      }

      req(input$upload_image)

      if (input$upload_image$size > 10 * 1024^2) {
        showNotification(
          "Filen er større end grænsen på 10 MB.",
          type = "error"
        )
        return()
      }

      display_name <- trimws(input$upload_name)

      if (!nzchar(display_name)) {
        display_name <- tools::file_path_sans_ext(
          input$upload_image$name
        )
      }

      tryCatch(
        {
          id <- library_add_image(
            image_library_paths,
            source_path = input$upload_image$datapath,
            original_name = input$upload_image$name,
            display_name = display_name,
            source = "upload",
            tags = input$upload_tags
          )

          pending_library_id(id)
          updateTextInput(
            session,
            "library_search",
            value = ""
          )
          library_revision(library_revision() + 1L)
          updateRadioButtons(
            session,
            "image_source",
            selected = "library"
          )
          updateTabsetPanel(
            session,
            "control_tab",
            selected = "Motiv"
          )
          showNotification(
            "Billedet er gemt og valgt.",
            type = "message"
          )
        },
        error = function(error) {
          showNotification(
            conditionMessage(error),
            type = "error",
            duration = 8
          )
        }
      )
    }
  )

  pdf_capacity_info <- reactive({
    width_mm <- suppressWarnings(as.numeric(input$width_mm))
    height_mm <- suppressWarnings(as.numeric(input$height_mm))
    req(
      length(width_mm) == 1L,
      length(height_mm) == 1L,
      is.finite(width_mm),
      is.finite(height_mm),
      width_mm > 0,
      height_mm > 0
    )

    a4_label_capacity(
      width_mm,
      height_mm,
      margin_mm = label_config$pdf$margin_mm,
      gap_mm = label_config$pdf$gap_mm
    )
  })

  observe({
    capacity <- pdf_capacity_info()$max_copies
    current <- isolate(input$pdf_copies)

    if (capacity < 1L) {
      updateSelectInput(
        session,
        "pdf_copies",
        choices = c("Etiketten passer ikke på A4" = ""),
        selected = ""
      )
      return()
    }

    values <- as.character(seq_len(capacity))
    labels <- ifelse(
      values == "1",
      "1 etiket",
      paste(values, "etiketter")
    )
    selected <- if (
      !is.null(current) &&
        length(current) == 1L &&
        current %in% values
    ) {
      current
    } else {
      "1"
    }

    updateSelectInput(
      session,
      "pdf_copies",
      choices = stats::setNames(values, labels),
      selected = selected
    )
  })

  output$pdf_capacity <- renderText({
    capacity <- pdf_capacity_info()

    if (capacity$max_copies < 1L) {
      return("Etiketten er for stor til den valgte A4-margen.")
    }

    paste0(
      "Der kan højst være ",
      capacity$max_copies,
      " på arket (",
      capacity$columns,
      " x ",
      capacity$rows,
      ")."
    )
  })

  label_specification <- reactive({
    width_mm <- suppressWarnings(as.numeric(input$width_mm))
    height_mm <- suppressWarnings(as.numeric(input$height_mm))
    aging <- suppressWarnings(as.numeric(input$aging))
    req(
      length(width_mm) == 1L,
      length(height_mm) == 1L,
      length(aging) == 1L,
      is.finite(width_mm),
      is.finite(height_mm),
      is.finite(aging),
      width_mm > 0,
      height_mm > 0,
      !is.null(input$date),
      !is.null(input$paper_colour),
      !is.null(input$image_source),
      !is.null(input$motif_type),
      !is.null(input$display_font_id),
      !is.null(input$typewriter_font_id),
      input$display_font_id %in% names(label_config$fonts$families),
      input$typewriter_font_id %in% names(label_config$fonts$families)
    )
    selected <- selected_library_image()

    image_path <- if (
      is.null(selected) ||
        !file.exists(selected$image_path[1])
    ) {
      NULL
    } else {
      selected$image_path[1]
    }

    list(
      width_mm = width_mm,
      height_mm = height_mm,
      title = input$title,
      subtitle = input$subtitle,
      date = format(input$date, "%d-%m-%Y"),
      batch = input$batch,
      code = input$code,
      footer = input$footer,
      paper_colour = input$paper_colour,
      aging = aging,
      image_source = input$image_source,
      motif_type = as_motif_type(input$motif_type),
      seed = as.integer(motif_seed()),
      image_path = image_path,
      image_fit = input$image_fit,
      grayscale = isTRUE(input$grayscale),
      font_family = font_family_from_id(
        label_config,
        input$display_font_id
      ),
      typewriter_family = font_family_from_id(
        label_config,
        input$typewriter_font_id
      )
    )
  })

  output$physical_size <- renderText({
    paste0(
      input$width_mm,
      " × ",
      input$height_mm,
      " mm"
    )
  })

  output$label_preview_ui <- renderUI({
    width_mm <- suppressWarnings(as.numeric(input$width_mm))
    height_mm <- suppressWarnings(as.numeric(input$height_mm))
    req(
      length(width_mm) == 1L,
      length(height_mm) == 1L,
      is.finite(width_mm),
      is.finite(height_mm),
      width_mm > 0,
      height_mm > 0
    )

    div(
      class = "label-preview-size",
      style = paste0(
        "width: 580px; max-width: 100%; aspect-ratio: ",
        width_mm,
        " / ",
        height_mm,
        ";"
      ),
      plotOutput(
        "label_preview",
        width = "100%",
        height = "100%"
      )
    )
  })

  output$label_preview <- renderPlot(
    {
      old_showtext_options <- showtext::showtext_opts(dpi = 110)
      on.exit(
        showtext::showtext_opts(old_showtext_options),
        add = TRUE
      )
      draw_label(label_specification())
    },
    res = 110,
    bg = "transparent"
  )

  output$download_png <- downloadHandler(
    filename = function() {
      paste0(slugify(input$title), ".png")
    },
    content = function(file) {
      write_label_png(
        file,
        label_specification(),
        dpi = as.numeric(input$png_dpi)
      )
    },
    contentType = "image/png"
  )

  output$download_pdf <- downloadHandler(
    filename = function() {
      paste0(slugify(input$title), ".pdf")
    },
    content = function(file) {
      write_label_pdf(
        file,
        label_specification(),
        copies = as.integer(input$pdf_copies),
        margin_mm = label_config$pdf$margin_mm,
        gap_mm = label_config$pdf$gap_mm
      )
    },
    contentType = "application/pdf"
  )
}

shinyApp(ui, server)

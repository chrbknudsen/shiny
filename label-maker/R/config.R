default_label_config <- function() {
  list(
    sizes = list(
      standard = list(
        label = "Standard - 58 x 74 mm",
        width_mm = 58,
        height_mm = 74
      )
    ),
    fonts = list(
      directory = file.path(getwd(), "fonts"),
      default_display = "dejavu_serif",
      default_typewriter = "dejavu_mono",
      families = list(
        dejavu_serif = list(
          label = "DejaVu Serif",
          family = "label-dejavu-serif",
          regular = "DejaVuSerif.ttf",
          bold = "DejaVuSerif-Bold.ttf"
        ),
        dejavu_sans = list(
          label = "DejaVu Sans",
          family = "label-dejavu-sans",
          regular = "DejaVuSans.ttf",
          bold = "DejaVuSans-Bold.ttf"
        ),
        dejavu_mono = list(
          label = "DejaVu Sans Mono",
          family = "label-dejavu-mono",
          regular = "DejaVuSansMono.ttf",
          bold = "DejaVuSansMono-Bold.ttf"
        )
      )
    ),
    pdf = list(
      margin_mm = 10,
      gap_mm = 4
    )
  )
}

config_scalar_text <- function(value, default) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(as.character(value)))
  ) {
    return(default)
  }

  trimws(as.character(value))
}

config_scalar_number <- function(value, field, allow_zero = FALSE) {
  number <- suppressWarnings(as.numeric(value))

  invalid_sign <- if (allow_zero) number < 0 else number <= 0

  if (
    length(number) != 1L ||
      !is.finite(number) ||
      invalid_sign
  ) {
    stop(
      paste0("Ugyldig numerisk værdi for ", field, " i config.yml."),
      call. = FALSE
    )
  }

  number
}

is_absolute_font_path <- function(path) {
  grepl(
    "^(/|[A-Za-z]:[/\\\\]|\\\\\\\\)",
    path
  )
}

resolve_font_path <- function(value, font_directory, field) {
  if (
    is.null(value) ||
      length(value) != 1L ||
      is.na(value) ||
      !nzchar(trimws(as.character(value)))
  ) {
    return(NULL)
  }

  value <- path.expand(trimws(as.character(value)))
  path <- if (is_absolute_font_path(value)) {
    value
  } else {
    file.path(font_directory, value)
  }

  if (!file.exists(path)) {
    stop(
      paste0("Skriftfilen for ", field, " findes ikke: ", path),
      call. = FALSE
    )
  }

  extension <- tolower(tools::file_ext(path))

  if (!extension %in% c("ttf", "ttc", "otf")) {
    stop(
      paste0(
        "Skriftfilen for ",
        field,
        " skal være TTF, TTC eller OTF."
      ),
      call. = FALSE
    )
  }

  normalizePath(
    path,
    winslash = "/",
    mustWork = TRUE
  )
}

normalize_font_definition <- function(
    value,
    id,
    font_directory) {
  field <- paste0("fonts.families.", id)

  if (!is.list(value)) {
    stop(
      paste0(
        field,
        " i config.yml skal definere family og mindst filen regular."
      ),
      call. = FALSE
    )
  }

  label <- config_scalar_text(value$label, id)
  family <- config_scalar_text(value$family, paste0("label-", id))

  if (family %in% c("sans", "serif", "mono")) {
    stop(
      paste0(
        field,
        ".family skal være et eget navn og må ikke være sans, serif eller mono."
      ),
      call. = FALSE
    )
  }

  regular <- resolve_font_path(
    value$regular,
    font_directory,
    paste0(field, ".regular")
  )

  if (is.null(regular)) {
    stop(
      paste0(field, ".regular skal angive en skriftfil."),
      call. = FALSE
    )
  }

  list(
    id = id,
    label = label,
    family = family,
    regular = regular,
    bold = resolve_font_path(
      value$bold,
      font_directory,
      paste0(field, ".bold")
    ),
    italic = resolve_font_path(
      value$italic,
      font_directory,
      paste0(field, ".italic")
    ),
    bolditalic = resolve_font_path(
      value$bolditalic,
      font_directory,
      paste0(field, ".bolditalic")
    )
  )
}

register_label_fonts <- function(config) {
  definitions <- config$fonts$families

  for (definition in definitions) {
    if (definition$family %in% sysfonts::font_families()) {
      next
    }

    arguments <- list(
      family = definition$family,
      regular = definition$regular
    )

    for (face in c("bold", "italic", "bolditalic")) {
      if (!is.null(definition[[face]])) {
        arguments[[face]] <- definition[[face]]
      }
    }

    do.call(sysfonts::font_add, arguments)
  }

  showtext::showtext_auto(enable = TRUE)

  invisible(
    vapply(
      definitions,
      function(definition) definition$family,
      character(1)
    )
  )
}

font_family_from_id <- function(config, id) {
  if (
    is.null(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !id %in% names(config$fonts$families)
  ) {
    stop("Der er ikke valgt en gyldig skrifttype.", call. = FALSE)
  }

  config$fonts$families[[id]]$family
}

read_label_config <- function(
    path = Sys.getenv(
      "LABEL_CONFIG_FILE",
      unset = file.path(getwd(), "config.yml")
    )) {
  config <- default_label_config()

  if (file.exists(path)) {
    supplied <- yaml::read_yaml(path)

    if (!is.list(supplied)) {
      stop("config.yml skal indeholde en YAML-liste.", call. = FALSE)
    }

    if (!is.null(supplied$sizes)) {
      config$sizes <- supplied$sizes
    }

    for (section in c("fonts", "pdf")) {
      if (!is.null(supplied[[section]])) {
        if (!is.list(supplied[[section]])) {
          stop(
            paste0(section, " i config.yml skal være en YAML-liste."),
            call. = FALSE
          )
        }

        config[[section]] <- utils::modifyList(
          config[[section]],
          supplied[[section]]
        )

        # A supplied catalog replaces the default catalog. This lets the
        # configuration determine exactly which choices appear in the app.
        if (
          identical(section, "fonts") &&
            !is.null(supplied$fonts$families)
        ) {
          config$fonts$families <- supplied$fonts$families

          if (is.null(supplied$fonts$default_display)) {
            config$fonts$default_display <- NULL
          }

          if (is.null(supplied$fonts$default_typewriter)) {
            config$fonts$default_typewriter <- NULL
          }
        }
      }
    }
  }

  if (!is.list(config$sizes) || length(config$sizes) == 0L) {
    stop("config.yml skal indeholde mindst én etiketstørrelse.", call. = FALSE)
  }

  size_ids <- names(config$sizes)

  if (
    is.null(size_ids) ||
      anyNA(size_ids) ||
      any(!nzchar(size_ids)) ||
      anyDuplicated(size_ids)
  ) {
    stop("Alle størrelser i config.yml skal have et unikt id.", call. = FALSE)
  }

  size_rows <- lapply(
    seq_along(config$sizes),
    function(index) {
      size <- config$sizes[[index]]
      id <- size_ids[[index]]

      if (!is.list(size)) {
        stop(
          paste0("Størrelsen '", id, "' skal være en YAML-liste."),
          call. = FALSE
        )
      }

      width_mm <- config_scalar_number(
        size$width_mm,
        paste0("sizes.", id, ".width_mm")
      )
      height_mm <- config_scalar_number(
        size$height_mm,
        paste0("sizes.", id, ".height_mm")
      )

      data.frame(
        id = id,
        label = config_scalar_text(
          size$label,
          paste0(id, " - ", width_mm, " x ", height_mm, " mm")
        ),
        width_mm = width_mm,
        height_mm = height_mm,
        stringsAsFactors = FALSE
      )
    }
  )

  config$size_table <- do.call(rbind, size_rows)
  config$size_choices <- stats::setNames(
    config$size_table$id,
    config$size_table$label
  )

  config_path <- normalizePath(
    path,
    winslash = "/",
    mustWork = FALSE
  )
  config_directory <- dirname(config_path)
  font_directory_value <- config_scalar_text(
    config$fonts$directory,
    "fonts"
  )
  font_directory_value <- path.expand(font_directory_value)
  font_directory <- if (is_absolute_font_path(font_directory_value)) {
    font_directory_value
  } else {
    file.path(config_directory, font_directory_value)
  }
  font_directory <- normalizePath(
    font_directory,
    winslash = "/",
    mustWork = FALSE
  )

  font_definitions <- config$fonts$families
  font_ids <- names(font_definitions)

  if (
    !is.list(font_definitions) ||
      length(font_definitions) == 0L ||
      is.null(font_ids) ||
      anyNA(font_ids) ||
      any(!nzchar(font_ids)) ||
      anyDuplicated(font_ids)
  ) {
    stop(
      "fonts.families skal indeholde mindst én skrifttype med et unikt id.",
      call. = FALSE
    )
  }

  font_definitions <- lapply(
    font_ids,
    function(id) {
      normalize_font_definition(
        config$fonts$families[[id]],
        id,
        font_directory
      )
    }
  )
  names(font_definitions) <- font_ids
  font_labels <- vapply(
    font_definitions,
    function(definition) definition$label,
    character(1)
  )
  registered_family_names <- vapply(
    font_definitions,
    function(definition) definition$family,
    character(1)
  )

  if (anyDuplicated(font_labels)) {
    stop("Skriftnavnene i fonts.families skal være unikke.", call. = FALSE)
  }

  if (anyDuplicated(registered_family_names)) {
    stop("family-navnene i fonts.families skal være unikke.", call. = FALSE)
  }

  default_display <- config_scalar_text(
    config$fonts$default_display,
    font_ids[[1L]]
  )
  default_typewriter <- config_scalar_text(
    config$fonts$default_typewriter,
    font_ids[[1L]]
  )

  for (default_id in c(default_display, default_typewriter)) {
    if (!default_id %in% font_ids) {
      stop(
        paste0("Den valgte standardskrift findes ikke: ", default_id),
        call. = FALSE
      )
    }
  }

  config$fonts <- list(
    directory = font_directory,
    families = font_definitions,
    choices = stats::setNames(font_ids, font_labels),
    default_display = default_display,
    default_typewriter = default_typewriter
  )

  config$pdf <- list(
    margin_mm = config_scalar_number(
      config$pdf$margin_mm,
      "pdf.margin_mm",
      allow_zero = TRUE
    ),
    gap_mm = config_scalar_number(
      config$pdf$gap_mm,
      "pdf.gap_mm",
      allow_zero = TRUE
    )
  )

  if (config$pdf$margin_mm >= 105) {
    stop("pdf.margin_mm skal være mindre end 105 mm.", call. = FALSE)
  }

  config$config_path <- config_path
  config
}

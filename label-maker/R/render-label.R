paper_colours <- c(
  "Varmt papir" = "#efe3c5",
  "Falmet beige" = "#e4d2aa",
  "Blegt papir" = "#f4ecd8",
  "Næsten hvidt" = "#faf8ee"
)

motif_names <- c(
  "1" = "Suspekt segl",
  "2" = "Mekanisk øje",
  "3" = "Astrolabium",
  "4" = "Pseudovidenskabeligt diagram"
)

# selectInput() uses vector names as labels and vector values as the values
# returned to the server. Keep motif_names convenient for lookups by number,
# but reverse that mapping for the input control.
motif_choices <- stats::setNames(
  names(motif_names),
  unname(motif_names)
)

as_motif_type <- function(value) {
  if (is.null(value) || length(value) != 1L || is.na(value)) {
    stop("Der er ikke valgt en gyldig motivtype.", call. = FALSE)
  }

  value <- as.character(value)

  if (value %in% names(motif_names)) {
    return(as.integer(value))
  }

  # Also accept the labels used by the first app version. This makes the
  # server robust to a browser briefly retaining the old selectInput value.
  label_match <- match(value, unname(motif_names))

  if (!is.na(label_match)) {
    return(as.integer(names(motif_names)[label_match]))
  }

  stop("Der er ikke valgt en gyldig motivtype.", call. = FALSE)
}

draw_fitted_text <- function(
    x,
    y,
    label,
    max_width,
    max_height = Inf,
    cex,
    family = "serif",
    font = 1,
    ...) {
  if (is.null(label) || !nzchar(trimws(label))) {
    return(invisible(NULL))
  }

  measured_width <- strwidth(
    label,
    units = "user",
    cex = cex,
    family = family,
    font = font
  )

  measured_height <- strheight(
    label,
    units = "user",
    cex = cex,
    family = family,
    font = font
  )

  width_factor <- if (measured_width > 0) {
    max_width / measured_width
  } else {
    1
  }
  height_factor <- if (is.finite(max_height) && measured_height > 0) {
    max_height / measured_height
  } else {
    1
  }
  fitted_cex <- cex * min(1, width_factor, height_factor)

  text(
    x,
    y,
    labels = label,
    cex = fitted_cex,
    family = family,
    font = font,
    ...
  )

  invisible(fitted_cex)
}

draw_paper_texture <- function(width_mm, height_mm, amount) {
  amount <- max(0, min(100, amount))

  if (amount == 0) {
    return(invisible(NULL))
  }

  old_seed <- if (exists(".Random.seed", envir = .GlobalEnv)) {
    get(".Random.seed", envir = .GlobalEnv)
  } else {
    NULL
  }

  on.exit(
    {
      if (is.null(old_seed)) {
        if (exists(".Random.seed", envir = .GlobalEnv)) {
          rm(".Random.seed", envir = .GlobalEnv)
        }
      } else {
        assign(".Random.seed", old_seed, envir = .GlobalEnv)
      }
    },
    add = TRUE
  )

  set.seed(271828)
  stain_n <- round(15 + amount * 1.2)

  for (i in seq_len(stain_n)) {
    x <- runif(1, 1, width_mm - 1)
    y <- runif(1, 1, height_mm - 1)
    radius <- runif(1, 0.15, 1.6 + amount / 35)
    alpha <- runif(1, 0.006, 0.018 + amount / 5000)

    circle(
      x,
      y,
      radius,
      border = NA,
      col = grDevices::adjustcolor("#9a592f", alpha.f = alpha)
    )
  }

  invisible(NULL)
}

draw_generated_motif <- function(type, seed) {
  type <- as_motif_type(type)
  set.seed(as.integer(seed))

  switch(
    as.character(type),
    "1" = draw_suspicious_seal(),
    "2" = draw_mechanical_eye(),
    "3" = draw_astrolabe(),
    "4" = draw_pseudoscientific_diagram()
  )
}

draw_library_motif <- function(path, fit = "contain", grayscale = TRUE) {
  if (is.null(path) || !file.exists(path)) {
    text(
      50,
      50,
      labels = "Intet billede valgt",
      family = "sans",
      cex = 0.8,
      col = "#765f45"
    )
    return(invisible(NULL))
  }

  image <- tryCatch(
    magick::image_read(path, strip = TRUE),
    error = function(error) NULL
  )

  if (is.null(image)) {
    text(
      50,
      50,
      labels = "Billedet kunne ikke læses",
      family = "sans",
      cex = 0.8,
      col = "#765f45"
    )
    return(invisible(NULL))
  }

  if (length(image) > 1) {
    image <- image[1]
  }

  if (isTRUE(grayscale)) {
    image <- magick::image_convert(
      image,
      colorspace = "gray"
    )
  }

  information <- magick::image_info(image)
  image_ratio <- information$width / information$height

  if (identical(fit, "fill")) {
    side <- min(information$width, information$height)
    offset_x <- floor((information$width - side) / 2)
    offset_y <- floor((information$height - side) / 2)

    image <- magick::image_crop(
      image,
      geometry = sprintf(
        "%dx%d+%d+%d",
        side,
        side,
        offset_x,
        offset_y
      )
    )

    bounds <- c(
      xleft = 4,
      ybottom = 4,
      xright = 96,
      ytop = 96
    )
  } else if (image_ratio >= 1) {
    width <- 92
    height <- width / image_ratio
    bounds <- c(
      xleft = 4,
      ybottom = 50 - height / 2,
      xright = 96,
      ytop = 50 + height / 2
    )
  } else {
    height <- 92
    width <- height * image_ratio
    bounds <- c(
      xleft = 50 - width / 2,
      ybottom = 4,
      xright = 50 + width / 2,
      ytop = 96
    )
  }

  raster <- as.raster(image)
  rasterImage(
    raster,
    xleft = bounds[["xleft"]],
    ybottom = bounds[["ybottom"]],
    xright = bounds[["xright"]],
    ytop = bounds[["ytop"]],
    interpolate = TRUE
  )

  invisible(NULL)
}

draw_label <- function(specification) {
  width_mm <- specification$width_mm
  height_mm <- specification$height_mm
  paper_colour <- specification$paper_colour
  display_family <- if (
    is.null(specification$font_family) ||
      !nzchar(specification$font_family)
  ) {
    "serif"
  } else {
    specification$font_family
  }
  typewriter_family <- if (
    is.null(specification$typewriter_family) ||
      !nzchar(specification$typewriter_family)
  ) {
    "mono"
  } else {
    specification$typewriter_family
  }
  label_scale <- min(width_mm / 58, height_mm / 74)
  label_scale <- max(0.3, min(1.6, label_scale))

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)
  label_fig <- par("fig")

  par(
    mar = rep(0, 4),
    oma = rep(0, 4),
    xaxs = "i",
    yaxs = "i"
  )

  plot.new()
  plot.window(
    xlim = c(0, width_mm),
    ylim = c(0, height_mm),
    asp = 1
  )

  rect(
    0,
    0,
    width_mm,
    height_mm,
    border = NA,
    col = paper_colour
  )
  draw_paper_texture(
    width_mm,
    height_mm,
    specification$aging
  )

  # Motif viewport: a square that scales with non-standard label sizes.
  motif_size <- max(4, min(width_mm - 17, height_mm * 0.53))
  motif_left <- (width_mm - motif_size) / 2
  motif_bottom <- height_mm * 0.285
  motif_right <- motif_left + motif_size
  motif_top <- motif_bottom + motif_size

  label_fig_width <- label_fig[2] - label_fig[1]
  label_fig_height <- label_fig[4] - label_fig[3]
  motif_fig <- c(
    label_fig[1] + label_fig_width * motif_left / width_mm,
    label_fig[1] + label_fig_width * motif_right / width_mm,
    label_fig[3] + label_fig_height * motif_bottom / height_mm,
    label_fig[3] + label_fig_height * motif_top / height_mm
  )

  par(
    fig = motif_fig,
    mar = rep(0, 4),
    new = TRUE,
    xaxs = "i",
    yaxs = "i"
  )
  plot.new()
  plot.window(
    xlim = c(0, 100),
    ylim = c(0, 100),
    asp = 1
  )

  if (identical(specification$image_source, "generated")) {
    draw_generated_motif(
      specification$motif_type,
      specification$seed
    )
  } else {
    draw_library_motif(
      specification$image_path,
      fit = specification$image_fit,
      grayscale = specification$grayscale
    )
  }

  # Return to the full label for borders and text.
  par(
    fig = label_fig,
    mar = rep(0, 4),
    new = TRUE,
    xaxs = "i",
    yaxs = "i"
  )
  plot.new()
  plot.window(
    xlim = c(0, width_mm),
    ylim = c(0, height_mm),
    asp = 1
  )

  border_offset <- min(width_mm, height_mm) * 0.045
  inner_offset <- border_offset + 0.7
  vertical_padding <- max(0.25, 0.45 * label_scale)

  rect(
    border_offset,
    border_offset,
    width_mm - border_offset,
    height_mm - border_offset,
    border = "#44382c",
    lwd = 1.5
  )
  rect(
    inner_offset,
    inner_offset,
    width_mm - inner_offset,
    height_mm - inner_offset,
    border = "#44382c",
    lwd = 0.7
  )

  title_band_top <- height_mm - inner_offset - vertical_padding
  title_band_bottom <- motif_top + vertical_padding

  if (title_band_bottom >= title_band_top) {
    title_band_bottom <- title_band_top - max(0.8, height_mm * 0.03)
  }

  title_band_height <- max(0.5, title_band_top - title_band_bottom)
  has_subtitle <- !is.null(specification$subtitle) &&
    nzchar(trimws(specification$subtitle))

  if (has_subtitle) {
    subtitle_band_height <- title_band_height * 0.28
    title_area_bottom <- title_band_bottom + subtitle_band_height

    draw_fitted_text(
      width_mm / 2,
      (title_area_bottom + title_band_top) / 2,
      specification$title,
      max_width = max(1, width_mm - 2 * inner_offset - 3),
      max_height = max(0.5, title_band_top - title_area_bottom),
      cex = 2.15 * label_scale,
      family = display_family,
      font = 2,
      col = "#3d3329"
    )

    draw_fitted_text(
      width_mm / 2,
      title_band_bottom + subtitle_band_height / 2,
      specification$subtitle,
      max_width = max(1, width_mm - 2 * inner_offset - 5),
      max_height = max(0.4, subtitle_band_height),
      cex = 0.78 * label_scale,
      family = display_family,
      col = "#4d4034"
    )
  } else {
    draw_fitted_text(
      width_mm / 2,
      (title_band_bottom + title_band_top) / 2,
      specification$title,
      max_width = max(1, width_mm - 2 * inner_offset - 3),
      max_height = title_band_height,
      cex = 2.15 * label_scale,
      family = display_family,
      font = 2,
      col = "#3d3329"
    )
  }

  data_top <- height_mm * 0.245
  data_bottom <- height_mm * 0.13
  left <- border_offset + 1.3
  right <- width_mm - border_offset - 1.3
  divider_1 <- left + (right - left) * 0.38
  divider_2 <- left + (right - left) * 0.72

  segments(left, data_top, right, data_top, col = "#55483a")
  segments(left, data_bottom, right, data_bottom, col = "#55483a")
  segments(
    c(divider_1, divider_2),
    data_bottom,
    c(divider_1, divider_2),
    data_top,
    col = "#55483a"
  )

  field_y <- (data_top + data_bottom) / 2

  draw_fitted_text(
    (left + divider_1) / 2,
    field_y,
    specification$date,
    max_width = divider_1 - left - 2,
    max_height = max(0.5, data_top - data_bottom - 1),
    cex = 0.72 * label_scale,
    family = typewriter_family,
    col = "#4d4034"
  )
  draw_fitted_text(
    (divider_1 + divider_2) / 2,
    field_y,
    specification$batch,
    max_width = divider_2 - divider_1 - 2,
    max_height = max(0.5, data_top - data_bottom - 1),
    cex = 0.72 * label_scale,
    family = typewriter_family,
    col = "#4d4034"
  )
  draw_fitted_text(
    (divider_2 + right) / 2,
    field_y,
    specification$code,
    max_width = right - divider_2 - 2,
    max_height = max(0.5, data_top - data_bottom - 1),
    cex = 0.72 * label_scale,
    family = typewriter_family,
    col = "#4d4034"
  )

  footer_band_bottom <- inner_offset + vertical_padding
  footer_band_top <- data_bottom - vertical_padding

  if (footer_band_top > footer_band_bottom) {
    draw_fitted_text(
      width_mm / 2,
      (footer_band_bottom + footer_band_top) / 2,
      specification$footer,
      max_width = max(1, width_mm - 2 * inner_offset - 3),
      max_height = footer_band_top - footer_band_bottom,
      cex = 0.68 * label_scale,
      family = typewriter_family,
      col = "#4d4034"
    )
  }

  invisible(NULL)
}

render_motif_png <- function(
    path,
    type,
    seed,
    size_px = 1400,
    background = "transparent") {
  old_showtext_options <- showtext::showtext_opts(dpi = 160)
  on.exit(
    showtext::showtext_opts(old_showtext_options),
    add = TRUE
  )

  grDevices::png(
    filename = path,
    width = size_px,
    height = size_px,
    res = 160,
    bg = background
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  old_par <- par(no.readonly = TRUE)
  on.exit(par(old_par), add = TRUE)

  par(
    mar = rep(0.4, 4),
    xaxs = "i",
    yaxs = "i",
    bg = background
  )
  plot.new()
  plot.window(
    xlim = c(0, 100),
    ylim = c(0, 100),
    asp = 1
  )
  draw_generated_motif(type, seed)

  invisible(path)
}

write_label_png <- function(path, specification, dpi = 300) {
  old_showtext_options <- showtext::showtext_opts(dpi = dpi)
  on.exit(
    showtext::showtext_opts(old_showtext_options),
    add = TRUE
  )

  grDevices::png(
    filename = path,
    width = specification$width_mm / 25.4,
    height = specification$height_mm / 25.4,
    units = "in",
    res = dpi,
    bg = specification$paper_colour
  )
  on.exit(grDevices::dev.off(), add = TRUE)

  draw_label(specification)
  invisible(path)
}

A4_WIDTH_MM <- 210
A4_HEIGHT_MM <- 297

a4_label_capacity <- function(
    label_width_mm,
    label_height_mm,
    margin_mm = 10,
    gap_mm = 4) {
  values <- c(label_width_mm, label_height_mm, margin_mm, gap_mm)

  if (any(!is.finite(values)) || any(values < 0)) {
    stop("Mål, margen og afstand skal være endelige tal på mindst nul.", call. = FALSE)
  }

  if (label_width_mm <= 0 || label_height_mm <= 0) {
    stop("Etikettens mål skal være større end nul.", call. = FALSE)
  }

  usable_width <- A4_WIDTH_MM - 2 * margin_mm
  usable_height <- A4_HEIGHT_MM - 2 * margin_mm
  columns <- max(
    0L,
    floor((usable_width + gap_mm) / (label_width_mm + gap_mm))
  )
  rows <- max(
    0L,
    floor((usable_height + gap_mm) / (label_height_mm + gap_mm))
  )

  list(
    columns = as.integer(columns),
    rows = as.integer(rows),
    max_copies = as.integer(columns * rows)
  )
}

a4_label_layout <- function(
    label_width_mm,
    label_height_mm,
    copies = 1L,
    margin_mm = 10,
    gap_mm = 4) {
  capacity <- a4_label_capacity(
    label_width_mm,
    label_height_mm,
    margin_mm,
    gap_mm
  )
  copies <- suppressWarnings(as.integer(copies))

  if (capacity$max_copies < 1L) {
    stop(
      "Etiketten kan ikke være på A4-siden med den valgte margen.",
      call. = FALSE
    )
  }

  if (
    length(copies) != 1L ||
      is.na(copies) ||
      copies < 1L ||
      copies > capacity$max_copies
  ) {
    stop(
      paste0(
        "Antallet af etiketter skal være mellem 1 og ",
        capacity$max_copies,
        "."
      ),
      call. = FALSE
    )
  }

  candidate_columns <- seq_len(capacity$columns)
  candidate_rows <- ceiling(copies / candidate_columns)
  valid <- candidate_rows <= capacity$rows
  candidate_columns <- candidate_columns[valid]
  candidate_rows <- candidate_rows[valid]
  empty_cells <- candidate_columns * candidate_rows - copies
  grid_width <- candidate_columns * label_width_mm +
    pmax(0, candidate_columns - 1) * gap_mm
  grid_height <- candidate_rows * label_height_mm +
    pmax(0, candidate_rows - 1) * gap_mm
  aspect_difference <- abs(
    log((grid_width / grid_height) / (A4_WIDTH_MM / A4_HEIGHT_MM))
  )
  best <- which.min(empty_cells * 100 + aspect_difference)
  columns <- candidate_columns[[best]]
  rows <- candidate_rows[[best]]
  total_height <- rows * label_height_mm + (rows - 1) * gap_mm
  grid_bottom <- (A4_HEIGHT_MM - total_height) / 2

  positions <- vector("list", copies)
  position_index <- 1L

  for (row_from_top in seq.int(0L, rows - 1L)) {
    labels_before_row <- row_from_top * columns
    labels_in_row <- min(columns, copies - labels_before_row)

    if (labels_in_row <= 0L) {
      next
    }

    row_width <- labels_in_row * label_width_mm +
      (labels_in_row - 1) * gap_mm
    row_left <- (A4_WIDTH_MM - row_width) / 2
    y <- grid_bottom +
      (rows - 1L - row_from_top) * (label_height_mm + gap_mm)

    for (column in seq_len(labels_in_row)) {
      positions[[position_index]] <- data.frame(
        x_mm = row_left + (column - 1) * (label_width_mm + gap_mm),
        y_mm = y,
        width_mm = label_width_mm,
        height_mm = label_height_mm
      )
      position_index <- position_index + 1L
    }
  }

  do.call(rbind, positions)
}

write_label_pdf <- function(
    path,
    specification,
    copies = 1L,
    margin_mm = 10,
    gap_mm = 4) {
  layout <- a4_label_layout(
    specification$width_mm,
    specification$height_mm,
    copies,
    margin_mm,
    gap_mm
  )
  use_cairo <- capabilities("cairo")
  if (use_cairo) {
    grDevices::cairo_pdf(
      filename = path,
      width = A4_WIDTH_MM / 25.4,
      height = A4_HEIGHT_MM / 25.4,
      onefile = FALSE,
      family = "sans",
      bg = "white"
    )
  } else {
    grDevices::pdf(
      file = path,
      width = A4_WIDTH_MM / 25.4,
      height = A4_HEIGHT_MM / 25.4,
      paper = "special",
      onefile = FALSE,
      useDingbats = FALSE,
      bg = "white"
    )
  }
  on.exit(grDevices::dev.off(), add = TRUE)

  for (index in seq_len(nrow(layout))) {
    x_left <- layout$x_mm[[index]] / A4_WIDTH_MM
    x_right <- (layout$x_mm[[index]] + layout$width_mm[[index]]) /
      A4_WIDTH_MM
    y_bottom <- layout$y_mm[[index]] / A4_HEIGHT_MM
    y_top <- (layout$y_mm[[index]] + layout$height_mm[[index]]) /
      A4_HEIGHT_MM

    par(
      fig = c(x_left, x_right, y_bottom, y_top),
      mar = rep(0, 4),
      oma = rep(0, 4),
      new = index > 1L
    )
    draw_label(specification)
  }

  invisible(path)
}

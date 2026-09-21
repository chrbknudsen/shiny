required_packages <- c(
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
    paste(
      "Manglende pakker:",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

r_files <- list.files(
  "R",
  pattern = "\\.[Rr]$",
  full.names = TRUE
)
invisible(lapply(r_files, source, encoding = "UTF-8"))

label_config <- read_label_config("config.yml")
registered_fonts <- register_label_fonts(label_config)
configured_font_files <- unlist(
  lapply(
    label_config$fonts$families,
    function(definition) {
      unlist(
        definition[c("regular", "bold", "italic", "bolditalic")],
        use.names = FALSE
      )
    }
  ),
  use.names = FALSE
)

stopifnot(
  !admin_token_status("")$configured,
  !admin_token_status("for-kort")$configured,
  admin_token_status(paste(rep("a", 32), collapse = ""))$configured,
  secure_token_equal("samme-token", "samme-token"),
  !secure_token_equal("forkert-token", "samme-token"),
  !secure_token_equal("kort", "kortere"),
  !secure_token_equal("", ""),
  verify_admin_token(
    "korrekt-administratornoegle-123456",
    "korrekt-administratornoegle-123456"
  ),
  !verify_admin_token(
    "forkert-administratornoegle-123456",
    "korrekt-administratornoegle-123456"
  ),
  nrow(label_config$size_table) >= 1L,
  identical(label_config$size_table$width_mm[[1L]], 58),
  identical(label_config$size_table$height_mm[[1L]], 74),
  length(label_config$fonts$families) == 3L,
  identical(
    unname(label_config$fonts$choices),
    names(label_config$fonts$families)
  ),
  label_config$fonts$default_display %in%
    names(label_config$fonts$families),
  label_config$fonts$default_typewriter %in%
    names(label_config$fonts$families),
  all(file.exists(configured_font_files)),
  all(registered_fonts %in% sysfonts::font_families()),
  identical(
    font_family_from_id(label_config, "dejavu_serif"),
    "label-dejavu-serif"
  ),
  identical(unname(motif_choices), names(motif_names)),
  identical(as_motif_type("1"), 1L),
  identical(as_motif_type("Suspekt segl"), 1L),
  identical(as_motif_type("4"), 4L)
)

capacity <- a4_label_capacity(
  58,
  74,
  label_config$pdf$margin_mm,
  label_config$pdf$gap_mm
)
layout_four <- a4_label_layout(
  58,
  74,
  copies = 4,
  margin_mm = label_config$pdf$margin_mm,
  gap_mm = label_config$pdf$gap_mm
)

stopifnot(
  capacity$max_copies == 9L,
  nrow(layout_four) == 4L,
  length(unique(layout_four$x_mm)) == 2L,
  length(unique(layout_four$y_mm)) == 2L,
  all(layout_four$x_mm >= 0),
  all(layout_four$y_mm >= 0),
  all(layout_four$x_mm + layout_four$width_mm <= A4_WIDTH_MM),
  all(layout_four$y_mm + layout_four$height_mm <= A4_HEIGHT_MM),
  min(layout_four$x_mm) >= label_config$pdf$margin_mm,
  min(layout_four$y_mm) >= label_config$pdf$margin_mm,
  max(layout_four$x_mm + layout_four$width_mm) <=
    A4_WIDTH_MM - label_config$pdf$margin_mm,
  max(layout_four$y_mm + layout_four$height_mm) <=
    A4_HEIGHT_MM - label_config$pdf$margin_mm
)

test_root <- tempfile("label-maker-test-")
dir.create(test_root, recursive = TRUE)
on.exit(
  unlink(test_root, recursive = TRUE, force = TRUE),
  add = TRUE
)

paths <- initialize_image_library(test_root)

connection <- open_image_database(paths)
database_columns <- DBI::dbListFields(connection, "images")
DBI::dbDisconnect(connection)

stopifnot(
  "deleted_at" %in% database_columns,
  is.null(library_get_image(paths, NULL)),
  is.null(library_get_image(paths, character())),
  is.null(library_get_image(paths, NA_character_)),
  is.null(library_get_image(paths, ""))
)

motif_path <- file.path(test_root, "generated.png")
render_motif_png(
  motif_path,
  type = 1,
  seed = 12345
)

image_id <- library_add_image(
  paths,
  source_path = motif_path,
  original_name = "generated.png",
  display_name = "Testsegl",
  source = "generated",
  motif_type = 1,
  seed = 12345,
  tags = "test"
)

entries <- library_list_images(paths)
entry <- library_get_image(paths, image_id)

stopifnot(
  nrow(entries) == 1,
  nrow(entry) == 1,
  identical(entry$display_name[1], "Testsegl"),
  file.exists(entry$image_path[1]),
  file.exists(entry$thumbnail_path[1])
)

active_image_path <- entry$image_path[[1L]]
active_thumbnail_path <- entry$thumbnail_path[[1L]]
deleted_entry <- library_soft_delete_image(paths, image_id)
deleted_entries <- library_list_images(paths, state = "deleted")

stopifnot(
  nrow(library_list_images(paths)) == 0L,
  nrow(deleted_entries) == 1L,
  is.null(library_get_image(paths, image_id)),
  nrow(library_get_image(paths, image_id, include_deleted = TRUE)) == 1L,
  !is.na(deleted_entry$deleted_at[[1L]]),
  nzchar(deleted_entry$deleted_at[[1L]]),
  !file.exists(active_image_path),
  !file.exists(active_thumbnail_path),
  file.exists(deleted_entry$image_path[[1L]]),
  file.exists(deleted_entry$thumbnail_path[[1L]])
)

restored_entry <- library_restore_image(paths, image_id)

stopifnot(
  nrow(library_list_images(paths)) == 1L,
  nrow(library_list_images(paths, state = "deleted")) == 0L,
  is.na(restored_entry$deleted_at[[1L]]),
  file.exists(restored_entry$image_path[[1L]]),
  file.exists(restored_entry$thumbnail_path[[1L]])
)

legacy_root <- file.path(test_root, "legacy-library")
dir.create(legacy_root, recursive = TRUE)
legacy_paths <- label_data_paths(legacy_root)
legacy_connection <- DBI::dbConnect(
  RSQLite::SQLite(),
  dbname = legacy_paths$database
)
DBI::dbExecute(
  legacy_connection,
  paste(
    "CREATE TABLE images (",
    "id TEXT PRIMARY KEY,",
    "display_name TEXT NOT NULL,",
    "stored_filename TEXT NOT NULL UNIQUE,",
    "thumbnail_filename TEXT NOT NULL,",
    "source TEXT NOT NULL,",
    "motif_type INTEGER,",
    "seed INTEGER,",
    "tags TEXT NOT NULL DEFAULT '',",
    "created_at TEXT NOT NULL",
    ")"
  )
)
DBI::dbDisconnect(legacy_connection)
initialize_image_library(legacy_root)
legacy_connection <- open_image_database(legacy_paths)
legacy_columns <- DBI::dbListFields(legacy_connection, "images")
DBI::dbDisconnect(legacy_connection)

stopifnot("deleted_at" %in% legacy_columns)

specification <- list(
  width_mm = 58,
  height_mm = 74,
  title = "Lys jævner",
  subtitle = "",
  date = "28-07-2026",
  batch = "03",
  code = "LA67F",
  footer = "Nusse Mad Science Inc.",
  paper_colour = "#efe3c5",
  aging = 35,
  image_source = "generated",
  motif_type = 1,
  seed = 12345,
  image_path = NULL,
  image_fit = "contain",
  grayscale = TRUE,
  font_family = font_family_from_id(
    label_config,
    label_config$fonts$default_display
  ),
  typewriter_family = font_family_from_id(
    label_config,
    label_config$fonts$default_typewriter
  )
)

png_path <- file.path(test_root, "label.png")
low_png_path <- file.path(test_root, "label-low.png")
pdf_path <- file.path(test_root, "label.pdf")

write_label_png(
  png_path,
  specification,
  dpi = 300
)

low_specification <- specification
low_specification$width_mm <- 74
low_specification$height_mm <- 45
low_specification$title <- "Professorens reagens"
low_specification$footer <- "Nusse Mad Science Inc."

write_label_png(
  low_png_path,
  low_specification,
  dpi = 300
)
write_label_pdf(
  pdf_path,
  specification,
  copies = 4,
  margin_mm = label_config$pdf$margin_mm,
  gap_mm = label_config$pdf$gap_mm
)

stopifnot(
  file.exists(png_path),
  file.info(png_path)$size > 0,
  file.exists(low_png_path),
  file.info(low_png_path)$size > 0,
  file.exists(pdf_path),
  file.info(pdf_path)$size > 0
)

message("Smoke-test gennemført.")

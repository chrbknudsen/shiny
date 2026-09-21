label_data_paths <- function(data_dir = NULL) {
  if (is.null(data_dir) || !nzchar(data_dir)) {
    data_dir <- Sys.getenv(
      "LABEL_DATA_DIR",
      unset = file.path(getwd(), "label-data")
    )
  }

  data_dir <- path.expand(data_dir)

  list(
    root = data_dir,
    images = file.path(data_dir, "images"),
    thumbnails = file.path(data_dir, "thumbnails"),
    trash = file.path(data_dir, "trash"),
    trash_images = file.path(data_dir, "trash", "images"),
    trash_thumbnails = file.path(data_dir, "trash", "thumbnails"),
    database = file.path(data_dir, "library.sqlite")
  )
}

open_image_database <- function(paths) {
  connection <- DBI::dbConnect(
    RSQLite::SQLite(),
    dbname = paths$database
  )
  DBI::dbExecute(connection, "PRAGMA busy_timeout = 5000")
  connection
}

initialize_image_library <- function(data_dir = NULL) {
  paths <- label_data_paths(data_dir)

  dir.create(paths$root, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$images, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$thumbnails, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$trash_images, recursive = TRUE, showWarnings = FALSE)
  dir.create(paths$trash_thumbnails, recursive = TRUE, showWarnings = FALSE)

  connection <- open_image_database(paths)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  DBI::dbExecute(
    connection,
    paste(
      "CREATE TABLE IF NOT EXISTS images (",
      "id TEXT PRIMARY KEY,",
      "display_name TEXT NOT NULL,",
      "stored_filename TEXT NOT NULL UNIQUE,",
      "thumbnail_filename TEXT NOT NULL,",
      "source TEXT NOT NULL,",
      "motif_type INTEGER,",
      "seed INTEGER,",
      "tags TEXT NOT NULL DEFAULT '',",
      "created_at TEXT NOT NULL,",
      "deleted_at TEXT",
      ")"
    )
  )

  columns <- DBI::dbGetQuery(
    connection,
    "PRAGMA table_info(images)"
  )$name

  if (!"deleted_at" %in% columns) {
    DBI::dbExecute(
      connection,
      "ALTER TABLE images ADD COLUMN deleted_at TEXT"
    )
  }

  DBI::dbExecute(
    connection,
    paste(
      "CREATE INDEX IF NOT EXISTS idx_images_state_created_at",
      "ON images(deleted_at, created_at)"
    )
  )

  paths
}

read_uploaded_image <- function(path, original_name) {
  extension <- tolower(tools::file_ext(original_name))

  if (!extension %in% c("png", "jpg", "jpeg", "webp", "svg")) {
    stop(
      "Filtypen understøttes ikke. Brug PNG, JPEG, WebP eller SVG.",
      call. = FALSE
    )
  }

  image <- if (extension == "svg") {
    magick::image_read_svg(path, width = 1800, height = 1800)
  } else {
    magick::image_read(path, strip = TRUE)
  }

  if (length(image) > 1) {
    image <- image[1]
  }

  image <- magick::image_orient(image)
  image <- magick::image_strip(image)
  image <- magick::image_convert(
    image,
    format = "png",
    colorspace = "sRGB"
  )

  information <- magick::image_info(image)

  if (
    information$width > 12000 ||
      information$height > 12000 ||
      information$width * information$height > 50000000
  ) {
    stop(
      "Billedet er for stort. Maksimum er 12.000 pixels på en led og 50 megapixels.",
      call. = FALSE
    )
  }

  image
}

library_add_image <- function(
    paths,
    source_path,
    original_name,
    display_name,
    source = "upload",
    motif_type = NA_integer_,
    seed = NA_integer_,
    tags = "") {
  display_name <- trimws(display_name)
  tags <- trimws(tags)

  if (!nzchar(display_name)) {
    display_name <- tools::file_path_sans_ext(basename(original_name))
  }

  display_name <- substr(display_name, 1, 80)
  tags <- substr(tags, 1, 240)

  image <- read_uploaded_image(source_path, original_name)

  id <- uuid::UUIDgenerate()
  stored_filename <- paste0(id, ".png")
  thumbnail_filename <- paste0(id, ".png")
  stored_path <- file.path(paths$images, stored_filename)
  thumbnail_path <- file.path(paths$thumbnails, thumbnail_filename)

  magick::image_write(
    image,
    path = stored_path,
    format = "png"
  )

  thumbnail <- magick::image_resize(image, "320x320>")
  magick::image_write(
    thumbnail,
    path = thumbnail_path,
    format = "png"
  )

  connection <- open_image_database(paths)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  inserted <- FALSE
  on.exit(
    {
      if (!inserted) {
        unlink(c(stored_path, thumbnail_path), force = TRUE)
      }
    },
    add = TRUE
  )

  DBI::dbExecute(
    connection,
    paste(
      "INSERT INTO images",
      "(id, display_name, stored_filename, thumbnail_filename,",
      "source, motif_type, seed, tags, created_at)",
      "VALUES (?, ?, ?, ?, ?, ?, ?, ?, ?)"
    ),
    params = list(
      id,
      display_name,
      stored_filename,
      thumbnail_filename,
      source,
      if (is.na(motif_type)) NA else as.integer(motif_type),
      if (is.na(seed)) NA else as.integer(seed),
      tags,
      format(
        Sys.time(),
        "%Y-%m-%dT%H:%M:%OS3%z",
        tz = "UTC"
      )
    )
  )

  inserted <- TRUE
  id
}

library_list_images <- function(
    paths,
    search = "",
    state = c("active", "deleted", "all")) {
  state <- match.arg(state)
  connection <- open_image_database(paths)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  search <- trimws(search)
  conditions <- character()
  params <- list()

  if (state == "active") {
    conditions <- c(conditions, "deleted_at IS NULL")
  } else if (state == "deleted") {
    conditions <- c(conditions, "deleted_at IS NOT NULL")
  }

  if (nzchar(search)) {
    pattern <- paste0("%", search, "%")
    conditions <- c(
      conditions,
      "(display_name LIKE ? OR tags LIKE ?)"
    )
    params <- c(params, list(pattern, pattern))
  }

  where_clause <- if (length(conditions) > 0L) {
    paste("WHERE", paste(conditions, collapse = " AND "))
  } else {
    ""
  }

  query <- paste(
    "SELECT * FROM images",
    where_clause,
    "ORDER BY created_at DESC, display_name COLLATE NOCASE"
  )

  if (length(params) == 0L) {
    DBI::dbGetQuery(
      connection,
      query
    )
  } else {
    DBI::dbGetQuery(connection, query, params = params)
  }
}

add_library_entry_paths <- function(paths, result) {
  if (nrow(result) == 0) {
    return(NULL)
  }

  is_deleted <- !is.na(result$deleted_at) & nzchar(result$deleted_at)
  image_directories <- ifelse(
    is_deleted,
    paths$trash_images,
    paths$images
  )
  thumbnail_directories <- ifelse(
    is_deleted,
    paths$trash_thumbnails,
    paths$thumbnails
  )

  result$image_path <- file.path(
    image_directories,
    result$stored_filename
  )
  result$thumbnail_path <- file.path(
    thumbnail_directories,
    result$thumbnail_filename
  )
  result
}

library_get_image <- function(paths, id, include_deleted = FALSE) {
  if (
    is.null(id) ||
      length(id) != 1L ||
      is.na(id) ||
      !nzchar(id)
  ) {
    return(NULL)
  }

  connection <- open_image_database(paths)
  on.exit(DBI::dbDisconnect(connection), add = TRUE)

  query <- if (isTRUE(include_deleted)) {
    "SELECT * FROM images WHERE id = ?"
  } else {
    "SELECT * FROM images WHERE id = ? AND deleted_at IS NULL"
  }

  result <- DBI::dbGetQuery(connection, query, params = list(id))
  add_library_entry_paths(paths, result)
}

move_library_file <- function(source, destination) {
  if (!file.exists(source)) {
    return(FALSE)
  }

  if (file.exists(destination)) {
    stop(
      paste0("Målfilen findes allerede: ", destination),
      call. = FALSE
    )
  }

  if (!file.rename(source, destination)) {
    stop(
      paste0("Kunne ikke flytte filen: ", source),
      call. = FALSE
    )
  }

  TRUE
}

relocate_library_entry <- function(paths, entry, to_trash) {
  if (isTRUE(to_trash)) {
    sources <- c(
      file.path(paths$images, entry$stored_filename[[1L]]),
      file.path(paths$thumbnails, entry$thumbnail_filename[[1L]])
    )
    destinations <- c(
      file.path(paths$trash_images, entry$stored_filename[[1L]]),
      file.path(
        paths$trash_thumbnails,
        entry$thumbnail_filename[[1L]]
      )
    )
  } else {
    sources <- c(
      file.path(paths$trash_images, entry$stored_filename[[1L]]),
      file.path(
        paths$trash_thumbnails,
        entry$thumbnail_filename[[1L]]
      )
    )
    destinations <- c(
      file.path(paths$images, entry$stored_filename[[1L]]),
      file.path(paths$thumbnails, entry$thumbnail_filename[[1L]])
    )
  }

  moved <- logical(length(sources))

  tryCatch(
    {
      for (index in seq_along(sources)) {
        moved[[index]] <- move_library_file(
          sources[[index]],
          destinations[[index]]
        )
      }
    },
    error = function(error) {
      for (index in rev(which(moved))) {
        file.rename(destinations[[index]], sources[[index]])
      }
      stop(error)
    }
  )

  list(
    sources = sources,
    destinations = destinations,
    moved = moved
  )
}

rollback_library_relocation <- function(relocation) {
  for (index in rev(which(relocation$moved))) {
    file.rename(
      relocation$destinations[[index]],
      relocation$sources[[index]]
    )
  }
}

library_soft_delete_image <- function(paths, id) {
  connection <- open_image_database(paths)
  transaction_open <- FALSE
  on.exit(
    {
      if (transaction_open) {
        try(DBI::dbExecute(connection, "ROLLBACK"), silent = TRUE)
      }
      DBI::dbDisconnect(connection)
    },
    add = TRUE
  )
  DBI::dbExecute(connection, "BEGIN IMMEDIATE")
  transaction_open <- TRUE

  entry <- DBI::dbGetQuery(
    connection,
    "SELECT * FROM images WHERE id = ? AND deleted_at IS NULL",
    params = list(id)
  )
  entry <- add_library_entry_paths(paths, entry)

  if (is.null(entry)) {
    stop("Billedet findes ikke eller er allerede slettet.", call. = FALSE)
  }

  relocation <- relocate_library_entry(paths, entry, to_trash = TRUE)

  tryCatch(
    {
      changed <- DBI::dbExecute(
        connection,
        paste(
          "UPDATE images SET deleted_at = ?",
          "WHERE id = ? AND deleted_at IS NULL"
        ),
        params = list(
          format(Sys.time(), "%Y-%m-%dT%H:%M:%OS3%z", tz = "UTC"),
          id
        )
      )

      if (length(changed) != 1L || is.na(changed) || changed != 1L) {
        stop("Billedet kunne ikke markeres som slettet.", call. = FALSE)
      }

      DBI::dbExecute(connection, "COMMIT")
      transaction_open <- FALSE
    },
    error = function(error) {
      rollback_library_relocation(relocation)
      stop(error)
    }
  )

  library_get_image(paths, id, include_deleted = TRUE)
}

library_restore_image <- function(paths, id) {
  connection <- open_image_database(paths)
  transaction_open <- FALSE
  on.exit(
    {
      if (transaction_open) {
        try(DBI::dbExecute(connection, "ROLLBACK"), silent = TRUE)
      }
      DBI::dbDisconnect(connection)
    },
    add = TRUE
  )
  DBI::dbExecute(connection, "BEGIN IMMEDIATE")
  transaction_open <- TRUE

  entry <- DBI::dbGetQuery(
    connection,
    "SELECT * FROM images WHERE id = ?",
    params = list(id)
  )
  entry <- add_library_entry_paths(paths, entry)

  if (
    is.null(entry) ||
      is.na(entry$deleted_at[[1L]]) ||
      !nzchar(entry$deleted_at[[1L]])
  ) {
    stop("Billedet findes ikke i papirkurven.", call. = FALSE)
  }

  relocation <- relocate_library_entry(paths, entry, to_trash = FALSE)

  tryCatch(
    {
      changed <- DBI::dbExecute(
        connection,
        paste(
          "UPDATE images SET deleted_at = NULL",
          "WHERE id = ? AND deleted_at IS NOT NULL"
        ),
        params = list(id)
      )

      if (length(changed) != 1L || is.na(changed) || changed != 1L) {
        stop("Billedet kunne ikke gendannes.", call. = FALSE)
      }

      DBI::dbExecute(connection, "COMMIT")
      transaction_open <- FALSE
    },
    error = function(error) {
      rollback_library_relocation(relocation)
      stop(error)
    }
  )

  library_get_image(paths, id)
}

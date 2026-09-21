#!/usr/bin/env Rscript

required_packages <- c("DBI", "RSQLite")
missing_packages <- required_packages[
  !vapply(
    required_packages,
    requireNamespace,
    quietly = TRUE,
    FUN.VALUE = logical(1)
  )
]

if (length(missing_packages) > 0L) {
  stop(
    paste(
      "Manglende pakker:",
      paste(missing_packages, collapse = ", ")
    ),
    call. = FALSE
  )
}

script_argument <- grep(
  "^--file=",
  commandArgs(trailingOnly = FALSE),
  value = TRUE
)

if (length(script_argument) != 1L) {
  stop("Kør værktøjet med Rscript image-admin.R ...", call. = FALSE)
}

script_path <- sub("^--file=", "", script_argument[[1L]])
project_directory <- dirname(
  normalizePath(script_path, winslash = "/", mustWork = TRUE)
)

source(
  file.path(project_directory, "R", "storage.R"),
  encoding = "UTF-8"
)

usage <- function() {
  cat(
    paste0(
      "Administrér billedbiblioteket uden for Shiny-appen.\n\n",
      "Brug:\n",
      "  Rscript image-admin.R list\n",
      "  Rscript image-admin.R list --deleted\n",
      "  Rscript image-admin.R list --all\n",
      "  Rscript image-admin.R delete <id>\n",
      "  Rscript image-admin.R delete <id> --yes\n",
      "  Rscript image-admin.R restore <id>\n\n",
      "delete uden --yes viser det valgte billede uden at ændre noget.\n",
      "Sletning flytter filerne til papirkurven og kan fortrydes.\n",
      "Datamappen vælges med LABEL_DATA_DIR ligesom i appen.\n"
    )
  )
}

print_entries <- function(entries) {
  if (nrow(entries) == 0L) {
    cat("Ingen billeder fundet.\n")
    return(invisible(entries))
  }

  visible_columns <- c(
    "id",
    "display_name",
    "source",
    "tags",
    "created_at",
    "deleted_at"
  )
  print(
    entries[, visible_columns, drop = FALSE],
    row.names = FALSE,
    right = FALSE
  )
  invisible(entries)
}

fail <- function(message, show_usage = FALSE) {
  cat(paste0("Fejl: ", message, "\n"), file = stderr())
  if (isTRUE(show_usage)) {
    usage()
  }
  quit(save = "no", status = 2L)
}

arguments <- commandArgs(trailingOnly = TRUE)

if (length(arguments) == 0L || arguments[[1L]] %in% c("help", "--help", "-h")) {
  usage()
  quit(save = "no", status = 0L)
}

paths <- initialize_image_library()
command <- arguments[[1L]]

if (identical(command, "list")) {
  flags <- arguments[-1L]

  if (length(flags) > 1L || !all(flags %in% c("--deleted", "--all"))) {
    fail("Ugyldige argumenter til list.", show_usage = TRUE)
  }

  state <- if ("--deleted" %in% flags) {
    "deleted"
  } else if ("--all" %in% flags) {
    "all"
  } else {
    "active"
  }

  print_entries(library_list_images(paths, state = state))
  quit(save = "no", status = 0L)
}

if (identical(command, "delete")) {
  if (length(arguments) < 2L || length(arguments) > 3L) {
    fail("delete kræver ét fuldt billed-id.", show_usage = TRUE)
  }

  id <- arguments[[2L]]
  confirmation <- length(arguments) == 3L && identical(arguments[[3L]], "--yes")

  if (length(arguments) == 3L && !confirmation) {
    fail("Det eneste tilladte tredje argument er --yes.")
  }

  entry <- library_get_image(paths, id)

  if (is.null(entry)) {
    fail("Billedet findes ikke i det aktive bibliotek. Brug det fulde id fra list.")
  }

  cat("Valgt billede:\n")
  print_entries(entry)

  if (!confirmation) {
    cat(
      paste0(
        "\nIngen ændring foretaget. Gentag med --yes for at flytte ",
        "billedet til papirkurven.\n"
      )
    )
    quit(save = "no", status = 0L)
  }

  deleted <- tryCatch(
    library_soft_delete_image(paths, id),
    error = function(error) fail(conditionMessage(error))
  )
  cat(paste0("\nFlyttet til papirkurven: ", deleted$display_name[[1L]], "\n"))
  quit(save = "no", status = 0L)
}

if (identical(command, "restore")) {
  if (length(arguments) != 2L) {
    fail("restore kræver ét fuldt billed-id.", show_usage = TRUE)
  }

  restored <- tryCatch(
    library_restore_image(paths, arguments[[2L]]),
    error = function(error) fail(conditionMessage(error))
  )
  cat(paste0("Gendannet: ", restored$display_name[[1L]], "\n"))
  quit(save = "no", status = 0L)
}

fail(paste0("Ukendt kommando: ", command), show_usage = TRUE)

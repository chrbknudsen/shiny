library(jsonlite)
library(dplyr)
library(purrr)
library(tibble)
library(httr2)
library(readr)
library(fs)

# ------------------------------------------------------------
# Hjælpefunktion
# ------------------------------------------------------------

`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0 || (length(x) == 1 && is.na(x))) y else x
}

# ------------------------------------------------------------
# Tomme tabeller
# ------------------------------------------------------------

empty_channels_tbl <- function() {
  tibble(
    nanoid = character(),
    name = character(),
    stream_url = character(),
    youtube_url = character(),
    type = character(),
    url = character(),
    language = character(),
    country = character(),
    isGeoBlocked = logical()
  )
}

empty_channel_categories_tbl <- function() {
  tibble(
    nanoid = character(),
    category = character()
  )
}

empty_manifest_tbl <- function(id_col = "file_id") {
  out <- tibble(
    name = character(),
    path = character(),
    sha = character(),
    download_url = character()
  )

  out[[id_col]] <- character()

  out |>
    select(all_of(id_col), everything())
}

# ------------------------------------------------------------
# GitHub-manifest
# ------------------------------------------------------------

get_github_json_manifest <- function(
  owner = "famelack",
  repo = "famelack-data",
  dir_path,
  id_col = "file_id",
  user_agent = "famelack-cache-r"
) {
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s",
    owner, repo, dir_path
  )

  resp <- request(api_url) |>
    req_user_agent(user_agent) |>
    req_perform()

  txt <- resp_body_string(resp)

  manifest <- fromJSON(txt) |>
    as_tibble() |>
    filter(type == "file", grepl("\\.json$", name)) |>
    transmute(
      id = sub("\\.json$", "", name),
      name = name,
      path = path,
      sha = sha,
      download_url = download_url
    ) |>
    arrange(id)

  names(manifest)[names(manifest) == "id"] <- id_col
  manifest
}

# ------------------------------------------------------------
# Lokal manifest-fil
# ------------------------------------------------------------

read_local_manifest <- function(manifest_file, id_col = "file_id") {
  if (!file.exists(manifest_file)) {
    return(empty_manifest_tbl(id_col = id_col))
  }

  readr::read_csv(manifest_file, show_col_types = FALSE)
}

write_local_manifest <- function(manifest, manifest_file) {
  fs::dir_create(fs::path_dir(manifest_file))
  readr::write_csv(manifest, manifest_file)
}

# ------------------------------------------------------------
# Download én JSON-fil
# ------------------------------------------------------------

download_one_json_file <- function(
  download_url,
  dest_file,
  user_agent = "famelack-cache-r"
) {
  fs::dir_create(fs::path_dir(dest_file))

  resp <- request(download_url) |>
    req_user_agent(user_agent) |>
    req_perform()

  writeLines(resp_body_string(resp), dest_file, useBytes = TRUE)
  invisible(dest_file)
}

# ------------------------------------------------------------
# Generisk sync
# ------------------------------------------------------------

sync_famelack_json_files <- function(
  dir_path,
  cache_dir,
  manifest_name = "manifest.csv",
  id_col = "file_id",
  delete_removed = TRUE,
  verbose = TRUE,
  user_agent = "famelack-cache-r"
) {
  fs::dir_create(cache_dir)

  manifest_file <- fs::path(cache_dir, manifest_name)

  remote_manifest <- get_github_json_manifest(
    dir_path = dir_path,
    id_col = id_col,
    user_agent = user_agent
  )

  local_manifest <- read_local_manifest(
    manifest_file = manifest_file,
    id_col = id_col
  )

  comparison <- remote_manifest |>
    left_join(
      local_manifest |>
        select(all_of(id_col), local_sha = sha),
      by = id_col
    ) |>
    mutate(
      status = case_when(
        is.na(local_sha) ~ "new",
        sha != local_sha ~ "changed",
        TRUE ~ "unchanged"
      )
    )

  to_download <- comparison |>
    filter(status %in% c("new", "changed"))

  if (verbose) {
    message("Nye filer: ", sum(comparison$status == "new"))
    message("Ændrede filer: ", sum(comparison$status == "changed"))
    message("Uændrede filer: ", sum(comparison$status == "unchanged"))
  }

  if (nrow(to_download) > 0) {
    purrr::pwalk(
      list(
        download_url = to_download$download_url,
        dest_file = fs::path(cache_dir, to_download$name)
      ),
      ~ download_one_json_file(
        download_url = ..1,
        dest_file = ..2,
        user_agent = user_agent
      )
    )
  }

  if (delete_removed && nrow(local_manifest) > 0) {
    removed <- anti_join(
      local_manifest,
      remote_manifest,
      by = id_col
    )

    if (nrow(removed) > 0) {
      removed_files <- fs::path(cache_dir, removed$name)
      exists_vec <- file.exists(removed_files)

      if (any(exists_vec)) {
        file.remove(removed_files[exists_vec])
      }

      if (verbose) {
        message(
          "Fjernede lokale filer som ikke længere findes på GitHub: ",
          nrow(removed)
        )
      }
    }
  }

  write_local_manifest(remote_manifest, manifest_file)

  invisible(list(
    remote_manifest = remote_manifest,
    comparison = comparison,
    downloaded = to_download
  ))
}

# ------------------------------------------------------------
# Countries: parse og load
# ------------------------------------------------------------

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

  tibble(
    nanoid = item$nanoid %||% NA_character_,
    name = item$name %||% NA_character_,
    stream_url = stream_url,
    youtube_url = youtube_url,
    type = case_when(
      !is.na(stream_url) ~ "hls",
      is.na(stream_url) & !is.na(youtube_url) ~ "youtube",
      TRUE ~ NA_character_
    ),
    url = dplyr::coalesce(stream_url, youtube_url),
    language = paste(item$languages %||% character(), collapse = ","),
    country = item$country %||% country_code,
    isGeoBlocked = isTRUE(item$isGeoBlocked)
  )
}

read_country_json <- function(path) {
  country_code <- sub("\\.json$", "", basename(path))
  items <- jsonlite::fromJSON(path, simplifyVector = FALSE)
  purrr::map_dfr(items, parse_channel_item, country_code = country_code)
}

load_channels_from_local_json <- function(cache_dir) {
  json_files <- list.files(
    cache_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )

  if (length(json_files) == 0) {
    return(empty_channels_tbl())
  }

  purrr::map_dfr(json_files, read_country_json) |>
    filter(!is.na(url), nzchar(url)) |>
    distinct(country, url, .keep_all = TRUE)
}

sync_famelack_country_files <- function(
  cache_dir = "data-cache/famelack/countries",
  manifest_name = "manifest.csv",
  delete_removed = TRUE,
  verbose = TRUE,
  user_agent = "famelack-cache-r"
) {
  sync_famelack_json_files(
    dir_path = "tv/raw/countries",
    cache_dir = cache_dir,
    manifest_name = manifest_name,
    id_col = "country_code",
    delete_removed = delete_removed,
    verbose = verbose,
    user_agent = user_agent
  )
}

# ------------------------------------------------------------
# Categories: parse og load
# ------------------------------------------------------------

parse_category_item <- function(item, category) {
  tibble(
    nanoid = item$nanoid %||% NA_character_,
    category = category
  )
}

read_category_json <- function(path) {
  category <- sub("\\.json$", "", basename(path))
  items <- jsonlite::fromJSON(path, simplifyVector = FALSE)

  purrr::map_dfr(items, parse_category_item, category = category) |>
    filter(!is.na(nanoid), nzchar(nanoid)) |>
    distinct(nanoid, category)
}

load_channel_categories_from_local_json <- function(cache_dir) {
  json_files <- list.files(
    cache_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )

  if (length(json_files) == 0) {
    return(empty_channel_categories_tbl())
  }

  purrr::map_dfr(json_files, read_category_json) |>
    distinct(nanoid, category)
}

sync_famelack_category_files <- function(
  cache_dir = "data-cache/famelack/categories",
  manifest_name = "manifest.csv",
  delete_removed = TRUE,
  verbose = TRUE,
  user_agent = "famelack-cache-r"
) {
  sync_famelack_json_files(
    dir_path = "tv/raw/categories",
    cache_dir = cache_dir,
    manifest_name = manifest_name,
    id_col = "category",
    delete_removed = delete_removed,
    verbose = verbose,
    user_agent = user_agent
  )
}

# ------------------------------------------------------------
# Offentlig funktion: kanaler
# ------------------------------------------------------------

get_famelack_channels <- function(
  cache_dir = "data-cache/famelack/countries",
  rds_name = "channels.rds",
  update = FALSE,
  verbose = TRUE,
  user_agent = "famelack-cache-r"
) {
  fs::dir_create(cache_dir)

  rds_path <- fs::path(cache_dir, rds_name)

  if (!update && file.exists(rds_path)) {
    if (verbose) {
      message("Loader kanaler fra RDS-cache")
    }
    return(readr::read_rds(rds_path))
  }

  if (update) {
    if (verbose) {
      message("Opdaterer lokale country-filer fra GitHub")
    }

    sync_famelack_country_files(
      cache_dir = cache_dir,
      delete_removed = TRUE,
      verbose = verbose,
      user_agent = user_agent
    )

    channels <- load_channels_from_local_json(cache_dir)
    readr::write_rds(channels, rds_path)
    return(channels)
  }

  json_files <- list.files(
    cache_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )

  if (length(json_files) > 0) {
    if (verbose) {
      message("RDS mangler; genskaber kanaler fra lokale JSON-filer")
    }

    channels <- load_channels_from_local_json(cache_dir)
    readr::write_rds(channels, rds_path)
    return(channels)
  }

  if (verbose) {
    message("Ingen lokal country-cache fundet; henter filer fra GitHub")
  }

  sync_famelack_country_files(
    cache_dir = cache_dir,
    delete_removed = TRUE,
    verbose = verbose,
    user_agent = user_agent
  )

  channels <- load_channels_from_local_json(cache_dir)
  readr::write_rds(channels, rds_path)
  channels
}

# ------------------------------------------------------------
# Offentlig funktion: kanal-kategorier
# ------------------------------------------------------------

get_famelack_channel_categories <- function(
  cache_dir = "data-cache/famelack/categories",
  rds_name = "channel_categories.rds",
  update = FALSE,
  verbose = TRUE,
  user_agent = "famelack-cache-r"
) {
  fs::dir_create(cache_dir)

  rds_path <- fs::path(cache_dir, rds_name)

  if (!update && file.exists(rds_path)) {
    if (verbose) {
      message("Loader kanal-kategorier fra RDS-cache")
    }
    return(readr::read_rds(rds_path))
  }

  if (update) {
    if (verbose) {
      message("Opdaterer lokale category-filer fra GitHub")
    }

    sync_famelack_category_files(
      cache_dir = cache_dir,
      delete_removed = TRUE,
      verbose = verbose,
      user_agent = user_agent
    )

    channel_categories <- load_channel_categories_from_local_json(cache_dir)
    readr::write_rds(channel_categories, rds_path)
    return(channel_categories)
  }

  json_files <- list.files(
    cache_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )

  if (length(json_files) > 0) {
    if (verbose) {
      message("RDS mangler; genskaber kanal-kategorier fra lokale JSON-filer")
    }

    channel_categories <- load_channel_categories_from_local_json(cache_dir)
    readr::write_rds(channel_categories, rds_path)
    return(channel_categories)
  }

  if (verbose) {
    message("Ingen lokal category-cache fundet; henter filer fra GitHub")
  }

  sync_famelack_category_files(
    cache_dir = cache_dir,
    delete_removed = TRUE,
    verbose = verbose,
    user_agent = user_agent
  )

  channel_categories <- load_channel_categories_from_local_json(cache_dir)
  readr::write_rds(channel_categories, rds_path)
  channel_categories
}
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
# GitHub metadata for country-filer
# ------------------------------------------------------------

get_github_country_manifest <- function(
  owner = "famelack",
  repo = "famelack-data",
  dir_path = "tv/raw/countries"
) {
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s",
    owner, repo, dir_path
  )

  resp <- request(api_url) |>
    req_user_agent("famelack-cache-r") |>
    req_perform()

  txt <- resp_body_string(resp)

  fromJSON(txt) |>
    as_tibble() |>
    filter(type == "file", grepl("\\.json$", name)) |>
    transmute(
      country_code = sub("\\.json$", "", name),
      name = name,
      path = path,
      sha = sha,
      download_url = download_url
    ) |>
    arrange(country_code)
}

# ------------------------------------------------------------
# Lokal manifest-fil
# ------------------------------------------------------------

read_local_manifest <- function(manifest_file) {
  if (!file.exists(manifest_file)) {
    return(tibble(
      country_code = character(),
      name = character(),
      path = character(),
      sha = character(),
      download_url = character()
    ))
  }

  readr::read_csv(manifest_file, show_col_types = FALSE)
}

write_local_manifest <- function(manifest, manifest_file) {
  fs::dir_create(fs::path_dir(manifest_file))
  readr::write_csv(manifest, manifest_file)
}

# ------------------------------------------------------------
# Download én fil
# ------------------------------------------------------------

download_one_country_file <- function(download_url, dest_file) {
  fs::dir_create(fs::path_dir(dest_file))

  resp <- request(download_url) |>
    req_user_agent("famelack-cache-r") |>
    req_perform()

  writeLines(resp_body_string(resp), dest_file, useBytes = TRUE)
  invisible(dest_file)
}

# ------------------------------------------------------------
# Synkroniser lokale json-filer mod GitHub
# ------------------------------------------------------------

sync_famelack_country_files <- function(
  cache_dir = "data-cache/famelack",
  manifest_name = "manifest.csv",
  delete_removed = TRUE,
  verbose = TRUE
) {
  fs::dir_create(cache_dir)

  manifest_file <- fs::path(cache_dir, manifest_name)

  remote_manifest <- get_github_country_manifest()
  local_manifest  <- read_local_manifest(manifest_file)

  comparison <- remote_manifest |>
    left_join(
      local_manifest |>
        select(country_code, local_sha = sha),
      by = "country_code"
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
    pwalk(
      list(
        download_url = to_download$download_url,
        dest_file = fs::path(cache_dir, to_download$name)
      ),
      download_one_country_file
    )
  }

  if (delete_removed && nrow(local_manifest) > 0) {
    removed <- anti_join(
      local_manifest,
      remote_manifest,
      by = "country_code"
    )

    if (nrow(removed) > 0) {
      removed_files <- fs::path(cache_dir, removed$name)
      file.exists_vec <- file.exists(removed_files)

      if (any(file.exists_vec)) {
        file.remove(removed_files[file.exists_vec])
      }

      if (verbose) {
        message("Fjernede lokale filer som ikke længere findes på GitHub: ", nrow(removed))
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
# Parse én landefil
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
    url = coalesce(stream_url, youtube_url),
    language = paste(item$languages %||% character(), collapse = ","),
    country = item$country %||% country_code,
    isGeoBlocked = isTRUE(item$isGeoBlocked)
  )
}

read_country_json <- function(path) {
  country_code <- sub("\\.json$", "", basename(path))

  items <- fromJSON(path, simplifyVector = FALSE)

  map_dfr(items, parse_channel_item, country_code = country_code)
}

# ------------------------------------------------------------
# Hent alle kanaler fra lokale filer
# ------------------------------------------------------------

load_all_famelack_channels_from_cache <- function(
  cache_dir = "data-cache/famelack",
  sync_first = TRUE,
  verbose = TRUE
) {
  if (sync_first) {
    sync_famelack_country_files(
      cache_dir = cache_dir,
      verbose = verbose
    )
  }

  json_files <- list.files(
    cache_dir,
    pattern = "\\.json$",
    full.names = TRUE
  )

  if (length(json_files) == 0) {
    return(tibble(
      nanoid = character(),
      name = character(),
      stream_url = character(),
      youtube_url = character(),
      type = character(),
      url = character(),
      language = character(),
      country = character(),
      isGeoBlocked = logical()
    ))
  }

  map_dfr(json_files, read_country_json) |>
    filter(!is.na(url), nzchar(url)) |>
    distinct(country, url, .keep_all = TRUE)
}
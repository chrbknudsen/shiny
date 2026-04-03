library(shiny)
library(httr2)
library(jsonlite)
library(dplyr)
library(tibble)
library(purrr)
library(readr)
library(fs)

CACHE_DIR <- "data-cache/famelack"
MANIFEST_FILE <- fs::path(CACHE_DIR, "manifest.csv")

REPO_OWNER <- "famelack"
REPO_NAME  <- "famelack-data"
REPO_PATH  <- "tv/raw/countries"

# ------------------------------------------------------------
# GitHub metadata
# ------------------------------------------------------------

get_github_country_manifest <- function() {
  api_url <- sprintf(
    "https://api.github.com/repos/%s/%s/contents/%s",
    REPO_OWNER, REPO_NAME, REPO_PATH
  )

  resp <- request(api_url) |>
    req_user_agent("famelack-cache-shiny") |>
    req_headers(Accept = "application/vnd.github+json") |>
    req_options(timeout = 10) |>
    req_perform()

  txt <- resp_body_string(resp)

  fromJSON(txt, simplifyVector = TRUE) |>
    as_tibble() |>
    filter(type == "file", grepl("\\.json$", name)) |>
    transmute(
      country_code = sub("\\.json$", "", name),
      name,
      path,
      sha,
      download_url
    ) |>
    arrange(country_code)
}

# ------------------------------------------------------------
# Manifest
# ------------------------------------------------------------

read_local_manifest <- function() {
  if (!file.exists(MANIFEST_FILE)) {
    return(tibble(
      country_code = character(),
      name = character(),
      path = character(),
      sha = character(),
      download_url = character()
    ))
  }

  read_csv(MANIFEST_FILE, show_col_types = FALSE)
}

write_local_manifest <- function(manifest) {
  fs::dir_create(fs::path_dir(MANIFEST_FILE))
  write_csv(manifest, MANIFEST_FILE)
}

# ------------------------------------------------------------
# Download
# ------------------------------------------------------------

download_one_country_file <- function(download_url, dest_file) {
  resp <- request(download_url) |>
    req_user_agent("famelack-cache-shiny") |>
    req_options(timeout = 15) |>
    req_perform()

  raw <- resp_body_raw(resp)

  fs::dir_create(fs::path_dir(dest_file))
  writeBin(raw, dest_file)

  TRUE
}

safe_download <- purrr::safely(download_one_country_file)

# ------------------------------------------------------------
# Check
# ------------------------------------------------------------

check_country_files <- function() {
  fs::dir_create(CACHE_DIR)

  remote <- get_github_country_manifest()
  local  <- read_local_manifest()

  remote |>
    left_join(
      local |> select(country_code, local_sha = sha),
      by = "country_code"
    ) |>
    mutate(
      status = case_when(
        is.na(local_sha) ~ "new",
        sha != local_sha ~ "changed",
        TRUE ~ "unchanged"
      )
    ) |>
    arrange(desc(status), country_code)
}

# ------------------------------------------------------------
# Download updates
# ------------------------------------------------------------

download_updates <- function(comparison) {
  to_download <- comparison |>
    filter(status %in% c("new", "changed"))

  results <- purrr::pmap(
    list(
      url = to_download$download_url,
      file = fs::path(CACHE_DIR, to_download$name)
    ),
    function(url, file) {
      res <- safe_download(url, file)

      tibble(
        file = basename(file),
        success = is.null(res$error),
        error = if (!is.null(res$error)) res$error$message else NA_character_
      )
    }
  ) |> bind_rows()

  # opdater manifest uanset hvad
  new_manifest <- comparison |>
    select(country_code, name, path, sha, download_url)

  write_local_manifest(new_manifest)

  list(
    downloaded = to_download,
    results = results
  )
}

# ------------------------------------------------------------
# UI
# ------------------------------------------------------------

ui <- fluidPage(
  titlePanel("Famelack cache-opdatering"),

  actionButton("run", "Tjek og hent ændringer"),

  br(), br(),

  verbatimTextOutput("summary"),

  h4("Ændringer"),
  tableOutput("changes"),

  h4("Download status"),
  tableOutput("download_status")
)

# ------------------------------------------------------------
# Server
# ------------------------------------------------------------

server <- function(input, output, session) {

  result <- reactiveVal(NULL)

  observeEvent(input$run, {

    comparison <- check_country_files()

    dl <- download_updates(comparison)

    result(list(
      comparison = comparison,
      downloaded = dl$downloaded,
      status = dl$results
    ))
  })

  output$summary <- renderText({
    x <- result()
    if (is.null(x)) return("Klik på knappen")

    comp <- x$comparison

    paste(
      "Nye:", sum(comp$status == "new"),
      "\nÆndrede:", sum(comp$status == "changed"),
      "\nUændrede:", sum(comp$status == "unchanged")
    )
  })

  output$changes <- renderTable({
    x <- result()
    if (is.null(x)) return(NULL)

    x$comparison |>
      filter(status != "unchanged") |>
      select(country_code, name, status)
  })

  output$download_status <- renderTable({
    x <- result()
    if (is.null(x)) return(NULL)

    x$status
  })
}

shinyApp(ui, server)

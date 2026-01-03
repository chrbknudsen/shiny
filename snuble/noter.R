# 'https://snublesten.dk/wp-json/wp/v2/snublesten?per_page=100&_fields=id,slug,title,meta' \
# 'Accept: application/json'


# Det står klart at der ikke er koordinater på stenene - links, der i øvrigt
# ikke virker specielt godt, linker til adressen. Så der skal geokodes på 
# adresserne.

# vi kan heller ikke trække data hver gang appen loades. Så der skal findes
# noget "data er x-dage ældre end i dag, indlæs data"

library(httr2)
library(jsonlite)

# --- lille hjælpefunktion: robust JSON parsing ---
resp_json <- function(resp) {
  txt <- resp_body_string(resp)
  fromJSON(txt, simplifyVector = FALSE)
}

# --- finder lat/lng dybt inde i en liste (meta kan være nested) ---
find_lat_lng <- function(x) {
  hits <- list()

  walk <- function(obj, path = character()) {
    if (is.list(obj)) {
      # hvis der findes lat/lng i samme "node"
      nms <- names(obj)
      if (!is.null(nms)) {
        has_lat <- any(tolower(nms) %in% c("lat", "latitude"))
        has_lng <- any(tolower(nms) %in% c("lng", "lon", "long", "longitude"))
        if (has_lat && has_lng) {
          lat_name <- nms[tolower(nms) %in% c("lat", "latitude")][1]
          lng_name <- nms[tolower(nms) %in% c("lng", "lon", "long", "longitude")][1]
          hits[[length(hits) + 1]] <<- list(
            path = paste(path, collapse = "$"),
            lat  = suppressWarnings(as.numeric(obj[[lat_name]])),
            lng  = suppressWarnings(as.numeric(obj[[lng_name]]))
          )
        }
      }

      # fortsæt rekursivt
      for (i in seq_along(obj)) {
        nm <- names(obj)[i]
        if (is.null(nm) || nm == "") nm <- as.character(i)
        walk(obj[[i]], c(path, nm))
      }
    }
  }

  walk(x)
  # filtrér åbenlyst ikke-numeriske/NA
  hits <- Filter(function(h) is.finite(h$lat) && is.finite(h$lng), hits)
  hits
}

# --- generisk pager til WP endpoints ---
wp_get_all_pages <- function(url, per_page = 100, verbose = TRUE) {
  out <- list()
  page <- 1

  repeat {
    req <- request(url) |>
      req_url_query(per_page = per_page, page = page) |>
      req_headers(Accept = "application/json")

    resp <- tryCatch(req_perform(req), error = function(e) e)

    if (inherits(resp, "error")) {
      stop("Request fejlede på page=", page, ": ", conditionMessage(resp))
    }

    # WP kan returnere 400 når page er for høj
    if (resp_status(resp) == 400) break
    if (resp_status(resp) >= 400) {
      stop("HTTP ", resp_status(resp), " ved ", url, " (page=", page, "):\n",
           resp_body_string(resp))
    }

    dat <- resp_json(resp)
    if (length(dat) == 0) break

    out <- c(out, dat)

    if (verbose) message("Hentede page=", page, " (", length(dat), " items)")
    if (length(dat) < per_page) break

    page <- page + 1
  }

  out
}

# --- hovedfunktion: hent alle snublesten + forsøg at udlede koordinater ---
get_snublesten <- function(base = "https://snublesten.dk", per_page = 100) {
  # primær + fallback (nogle WP sites kræver rest_route)
  candidates <- c(
    paste0(base, "/wp-json/wp/v2/snublesten"),
    paste0(base, "/?rest_route=/wp/v2/snublesten")
  )

  data <- NULL
  last_err <- NULL

  for (u in candidates) {
    message("Prøver: ", u)
    res <- tryCatch(wp_get_all_pages(u, per_page = per_page, verbose = FALSE),
                    error = function(e) e)
    if (!inherits(res, "error")) {
      data <- res
      break
    } else {
      last_err <- res
    }
  }

  if (is.null(data)) {
    stop("Kunne ikke hente snublesten via REST. Sidste fejl:\n",
         conditionMessage(last_err))
  }

  # udtræk (mulige) koordinater fra meta
  out <- lapply(data, function(item) {
    meta <- item$meta %||% list()
    coords <- find_lat_lng(meta)

    list(
      id    = item$id %||% NA,
      slug  = item$slug %||% NA,
      title = tryCatch(item$title$rendered %||% NA, error = function(e) NA),
      coords = coords
    )
  })

  out
}

`%||%` <- function(a, b) if (is.null(a)) b else a

# --- kør ---
stones <- get_snublesten()

# eksempel: print dem, der faktisk havde coords fundet i meta
with_coords <- Filter(function(x) length(x$coords) > 0, stones)

length(stones)
length(with_coords)

# kig på de første 3 med coords
utils::str(with_coords[1:3], max.level = 4)

stones

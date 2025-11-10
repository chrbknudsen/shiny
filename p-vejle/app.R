library(shiny)
library(httr)
library(jsonlite)
library(dplyr)
library(leaflet)
library(stringr)
library(purrr)

# Hjælper: find kolonner for latitude/longitude uanset navngivning
guess_coords <- function(df) {
  nms <- names(df)
  lat_idx <- which(str_detect(tolower(nms), "\\blat|latitude"))
  lon_idx <- which(str_detect(tolower(nms), "\\blon|\\blng|longitude"))
  if (!length(lat_idx) || !length(lon_idx)) return(df)

  lat_col <- nms[lat_idx[1]]
  lon_col <- nms[lon_idx[1]]

  df %>%
    mutate(
      latitude  = suppressWarnings(as.numeric(.data[[lat_col]])),
      longitude = suppressWarnings(as.numeric(.data[[lon_col]]))
    )
}

# Hjælper: vælg label-kolonne hvis muligt
guess_label <- function(df) {
  candidates <- c("name","parkingSpotName","title","status","id")
  hit <- candidates[candidates %in% names(df)]
  if (!length(hit)) return(rep(NA_character_, nrow(df)))
  out <- as.character(df[[hit[1]]])
  if (length(hit) > 1) {
    for (v in hit[-1]) out <- coalesce(out, as.character(df[[v]]))
  }
  out
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "shortcut icon", href = "favicon.ico"),
    tags$link(rel = "apple-touch-icon", sizes = "180x180", href = "apple-touch-icon.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "32x32", href = "favicon-32x32.png"),
    tags$link(rel = "icon", type = "image/png", sizes = "16x16", href = "favicon-16x16.png")
  ),
  titlePanel("P-pladser i Vejle"),
  leafletOutput("map", height = 600)
)

server <- function(input, output, session) {
  # Hent og parse data
  resp <- GET(
    "https://letparkeringapi.azurewebsites.net/api/ParkingSpotOverview",
    user_agent("VejleParkingShiny/1.0"),
    accept_json(),
    timeout(10)
  )

  if (http_error(resp)) {
    output$map <- renderLeaflet({
      validate(need(FALSE, paste("Fejl fra server:", status_code(resp))))
      leaflet() # ikke vist pga. validate
    })
    return(invisible())
  }

  raw_json <- content(resp, as = "text", encoding = "UTF-8")
  dat <- fromJSON(raw_json, flatten = TRUE)

  # Hvis API’et returnerer en liste af lister, lav det til en tibble
  if (is.list(dat) && !is.data.frame(dat)) {
    # prøv at binde elementer som rækker
    dat <- tryCatch(
      bind_rows(dat),
      error = function(e) {
        # sidste udvej: unnest dybt
        as_tibble(dat)
      }
    )
  }

  places <- as_tibble(dat) %>%
    guess_coords() %>%
    mutate(label = guess_label(.)) %>%
    filter(!is.na(latitude), !is.na(longitude))

  output$map <- renderLeaflet({
    validate(need(nrow(places) > 0, "Ingen punkter at vise (tjek at API’et returnerer koordinater)."))

    m <- leaflet(places) |>
      addTiles(group = "OSM") |>
      addProviderTiles(providers$CartoDB.Positron, group = "Carto") |>
      addProviderTiles(providers$Stamen.TonerLite, group = "Toner") |>
      addMarkers(~longitude, ~latitude, label = ~label,
                 clusterOptions = markerClusterOptions(),
                 group = "Steder") |>
      addLayersControl(
        baseGroups = c("OSM", "Carto", "Toner"),
        overlayGroups = c("Steder"),
        options = layersControlOptions(collapsed = FALSE)
      ) |>
      addMeasure(
        primaryLengthUnit = "meters", secondaryLengthUnit = "kilometers",
        primaryAreaUnit = "sqmeters", position = "topleft"
      )

    # Zoom til data
    bb <- sf::st_as_sf(places, coords = c("longitude","latitude"), crs = 4326) |>
      sf::st_bbox()
    m |> fitBounds(lng1 = bb["xmin"], lat1 = bb["ymin"], lng2 = bb["xmax"], lat2 = bb["ymax"])
  })
}

shinyApp(ui, server)

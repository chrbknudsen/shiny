library(shiny)
library(leaflet)

library(httr2)
library(rvest)
library(stringr)
library(tibble)
library(tidyverse)

library(sf)

# -----------------------------
# Scraper
# -----------------------------
get_myshiptracking_coords <- function(url) {
  html <- request(url) |>
    req_user_agent("R (httr2; simple scraper for personal use)") |>
    req_perform() |>
    resp_body_html()

  txt <- html |>
    html_elements("body") |>
    html_text2()

  m_coords <- str_match(
    txt,
    "coordinates\\s+([+-]?[0-9]+\\.[0-9]+)°\\s*/\\s*([+-]?[0-9]+\\.[0-9]+)°"
  )

  if (all(is.na(m_coords))) {
    stop("Kunne ikke finde koordinater i siden.")
  }

  lat <- as.numeric(m_coords[2])
  lon <- as.numeric(m_coords[3])

  m_time <- str_match(
    txt,
    "as reported on\\s+([0-9]{4}-[0-9]{2}-[0-9]{2}\\s+[0-9]{2}:[0-9]{2})"
  )
  reported_utc <- if (!is.na(m_time[2])) m_time[2] else NA_character_

  tibble(
    latitude = lat,
    longitude = lon,
    reported_utc = reported_utc
  )
}

comfort_url <- "https://www.myshiptracking.com/vessels/usns-comfort-mmsi-368817000-imo-0"
mercy_url   <- "https://www.myshiptracking.com/vessels/mercys-vessel-mmsi-368106090-imo-0"

# -----------------------------
# Load Greenland + coast
# -----------------------------
greenland <- st_read("greenland.geojson", quiet = TRUE)
if (is.na(st_crs(greenland))) st_crs(greenland) <- 4326
greenland <- st_transform(greenland, 4326)

coast_3413 <- readRDS("greenland_coast_3413.rds")
coast_3413_sfc <- if (inherits(coast_3413, "sf")) st_geometry(coast_3413) else coast_3413

# -----------------------------
# Distance calculation
# -----------------------------
distance_to_greenland_coast <- function(df_lonlat, coast_sfc_3413) {

  pts_wgs84 <- st_as_sf(df_lonlat,
                        coords = c("longitude", "latitude"),
                        crs = 4326,
                        remove = FALSE)

  pts_3413 <- st_transform(pts_wgs84, st_crs(coast_sfc_3413))

  lines_3413 <- st_nearest_points(pts_3413, coast_sfc_3413)

  nearest_xy <- lapply(seq_along(lines_3413), function(i) {
    xy <- st_coordinates(lines_3413[i])
    xy[nrow(xy), 1:2]
  })
  nearest_xy <- do.call(rbind, nearest_xy)
  colnames(nearest_xy) <- c("x", "y")

  dist_m <- as.numeric(st_distance(pts_3413, coast_sfc_3413))

  nearest_pts <- st_as_sf(
    as.data.frame(nearest_xy),
    coords = c("x", "y"),
    crs = st_crs(coast_sfc_3413)
  ) |> st_transform(4326)

  nearest_lonlat <- st_coordinates(nearest_pts) |>
    as.data.frame() |>
    setNames(c("nearest_lon", "nearest_lat"))

  df_lonlat |>
    mutate(distance_km = dist_m / 1000) |>
    bind_cols(nearest_lonlat)
}

pad_bounds <- function(lng1, lat1, lng2, lat2, pad = 0.08) {
  dlng <- lng2 - lng1
  dlat <- lat2 - lat1
  if (dlng == 0) dlng <- 1
  if (dlat == 0) dlat <- 1

  list(
    lng1 = lng1 - dlng * pad,
    lng2 = lng2 + dlng * pad,
    lat1 = lat1 - dlat * pad,
    lat2 = lat2 + dlat * pad
  )
}

# -----------------------------
# Server
# -----------------------------
server <- function(input, output, session) {

  ship_cache <- reactiveVal(NULL)

  fetch_ships <- function() {
    comfort <- get_myshiptracking_coords(comfort_url) |> mutate(skib = "Comfort")
    mercy   <- get_myshiptracking_coords(mercy_url)   |> mutate(skib = "Mercy")

    bind_rows(comfort, mercy) |>
      distance_to_greenland_coast(coast_3413_sfc)
  }

  ship_cache(fetch_ships())

  observe({
    invalidateLater(as.integer(input$refresh_sec) * 1000, session)
    ship_cache(fetch_ships())
  })

  observeEvent(input$refresh_now, {
    ship_cache(fetch_ships())
  })

  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$OpenStreetMap, group = "OSM") |>
      addProviderTiles(providers$Esri.WorldImagery, group = "Satellite") |>
      addLayersControl(
        baseGroups = c("OSM", "Satellite"),
        overlayGroups = c("Comfort", "Mercy", "Nearest coast"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  observeEvent(ship_cache(), {

    d <- ship_cache()

    proxy <- leafletProxy("map") |>
      clearGroup("Comfort") |>
      clearGroup("Mercy") |>
      clearGroup("Nearest coast")

    proxy |>
      addCircleMarkers(
        data = d |> filter(skib == "Comfort"),
        lng = ~longitude, lat = ~latitude,
        radius = 8, color = "red",
        group = "Comfort"
      ) |>
      addCircleMarkers(
        data = d |> filter(skib == "Mercy"),
        lng = ~longitude, lat = ~latitude,
        radius = 8, color = "blue",
        group = "Mercy"
      ) |>
      addCircleMarkers(
        data = d,
        lng = ~nearest_lon, lat = ~nearest_lat,
        radius = 4,
        group = "Nearest coast"
      )

    # 🔥 DEFAULT ZOOM = Grønland + skibe
    bbox_gl <- st_bbox(greenland)

    all_lon <- c(d$longitude,
                 bbox_gl["xmin"], bbox_gl["xmax"])
    all_lat <- c(d$latitude,
                 bbox_gl["ymin"], bbox_gl["ymax"])

    b <- pad_bounds(
      min(all_lon, na.rm = TRUE),
      min(all_lat, na.rm = TRUE),
      max(all_lon, na.rm = TRUE),
      max(all_lat, na.rm = TRUE)
    )

    proxy |> fitBounds(b$lng1, b$lat1, b$lng2, b$lat2)
  })

  output$dist_table <- renderTable({
    ship_cache() |>
      select(skib, latitude, longitude, distance_km)
  })
  output$min_distance_block <- renderText({
  d <- ship_cache()
  req(d)

  min_dist <- min(d$distance_km, na.rm = TRUE)

  paste0(
    "min_distance_km = ", round(min_dist, 2), "\n",
    "nej"
  )
})
}
# ==== Building the OCCT stop list ====
# Find BC Transit stops at the same places as OCCT timepoints,
# so their locations can be reused.

library(tidyverse)
library(tidytransit)

gtfs <- read_rds("data/processed/gtfs_prepared.rds")

# BC Transit stops whose names mention OCCT timepoint streets/places
gtfs$stops |>
  filter(str_detect(stop_name, regex(
    "floral|murray|leroy|columbus|conklin|tompkins|hawley|lehigh|union|mohawk|junction|plaza|town sq|oakdale|wegmans|aldi|shoppes",
    ignore_case = TRUE))) |>
  select(stop_id, stop_name, stop_lat, stop_lon) |>
  arrange(stop_name) |>
  as.data.frame()

# ---- Check: plot the OCCT stop list ----
library(leaflet)

occt <- read_csv("data/manual/occt_stops.csv")

leaflet(occt) |>
  addTiles() |>
  addCircleMarkers(lng = ~stop_lon,
                   lat = ~stop_lat,
                   radius = 7,
                   label = ~paste0(stop_name, " [", source, "]"))
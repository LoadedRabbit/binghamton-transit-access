# 04_download_context.R
# Download neighborhood data (Census) and supermarkets (OpenStreetMap).
# These change slowly (Census yearly), so this doesn't need to run as
# often as the bus feed. Needs a Census API key (stored in .Renviron).
# (Investigation and checks: scratch/phase3_people.R)

library(tidyverse)
library(tidycensus)
library(sf)
library(osmdata)

# ---- Census block groups in Broome County ----
# ACS 5-year estimates. Variables:
#   B01003_001 = total population
#   B25044_001 = total households
#   B25044_003 = owner households with no vehicle
#   B25044_010 = renter households with no vehicle
acs_year <- 2024

bg <- get_acs(geography = "block group",
              state = "NY",
              county = "Broome",
              variables = c(pop = "B01003_001",
                            hh = "B25044_001",
                            no_car_own = "B25044_003",
                            no_car_rent = "B25044_010"),
              year = acs_year,
              geometry = TRUE,
              output = "wide")

# ---- Supermarkets (OpenStreetMap shop=supermarket) ----
# Excludes convenience stores. List spot-checked against store
# directories in Oct 2026 (see scratch/phase3_people.R).
bbox <- st_bbox(st_transform(bg, 4326))

osm <- opq(bbox) |>
  add_osm_feature(key = "shop", value = "supermarket") |>
  osmdata_sf()

# Stores are mapped as points or building outlines: one point per store
store_points <- osm$osm_points |>
  filter(!is.na(shop)) |>
  select(name)

store_buildings <- osm$osm_polygons |>
  st_centroid() |>
  select(name)

groceries <- bind_rows(store_points, store_buildings)

# ---- Save ----
write_rds(bg, "data/raw/census_block_groups.rds")
write_rds(groceries, "data/raw/supermarkets.rds")
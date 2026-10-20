# 05_neighborhoods.R
# Neighborhood results: campus access (3 scenarios), grocery access,
# and the last bus leaving campus. Saves everything the website reads.
# (Investigation and checks: scratch/phase3_people.R)

library(tidyverse)
library(tidytransit)
library(sf)
source("R/functions.R")  # dist_m(), transit_times(), neighborhood_times()

gtfs <- read_rds("data/processed/gtfs_prepared.rds")
campus_stops <- read_rds("data/processed/campus_stops.rds")
bg <- read_rds("data/raw/census_block_groups.rds")
groceries <- read_rds("data/raw/supermarkets.rds")

# ---- Settings ----
weekday  <- pick_date(gtfs, day = 3, prefer = "BU")  # next semester Tuesday
saturday <- pick_date(gtfs, day = 7)                 # next Saturday

# Saved so the website can say which dates the map is based on
analysis_dates <- tibble(weekday = weekday, saturday = saturday)
# ---- Neighborhoods ----
# No-car households = owners + renters.
# Margin of error for a sum: sqrt(M1^2 + M2^2) (Census Bureau formula)
bg <- bg |>
  mutate(no_carE = no_car_ownE + no_car_rentE,
         no_carM = sqrt(no_car_ownM^2 + no_car_rentM^2),
         pct_no_car = no_carE / hhE)

# One point per neighborhood, guaranteed inside its boundary
pts <- st_point_on_surface(bg)
coords <- st_coordinates(pts)
bg_pts <- tibble(GEOID = pts$GEOID,
                 bg_lon = coords[, "X"],
                 bg_lat = coords[, "Y"])

# ---- Campus scenarios ----
tt_8am  <- transit_times(gtfs, campus_stops, weekday, 8 * 3600, "to")
tt_10pm <- transit_times(gtfs, campus_stops, weekday, 22 * 3600, "from")
tt_sat  <- transit_times(gtfs, campus_stops, saturday, 12 * 3600, "to")

campus_8am <- neighborhood_times(bg_pts, tt_8am) |>
  select(GEOID, campus_8am_min = total_min, campus_8am_stop = best_stop)
campus_10pm <- neighborhood_times(bg_pts, tt_10pm) |>
  select(GEOID, campus_10pm_min = total_min)
campus_sat <- neighborhood_times(bg_pts, tt_sat) |>
  select(GEOID, campus_sat_min = total_min)

# ---- Groceries (Saturday, arrive by noon) ----
coords_g <- st_coordinates(groceries)
groc_xy <- tibble(groc_name = groceries$name,
                  groc_lon = coords_g[, "X"],
                  groc_lat = coords_g[, "Y"])

# "Grocery stops": bus stops within 400 m (~5 min walk) of a supermarket
grocery_stops <- gtfs$stops |>
  select(stop_name, stop_lat, stop_lon) |>
  cross_join(groc_xy) |>
  mutate(d = dist_m(stop_lat, stop_lon, groc_lat, groc_lon)) |>
  filter(d <= 400) |>
  pull(stop_name) |>
  unique()

tt_grocery <- transit_times(gtfs, grocery_stops, saturday, 12 * 3600, "to")

# Option 1: bus. (Ends at the grocery stop: excludes a final walk <= 5 min.)
by_bus <- neighborhood_times(bg_pts, tt_grocery) |>
  select(GEOID, bus_min = total_min)

# Option 2: walk straight to a supermarket (up to 800 m)
by_walk <- bg_pts |>
  cross_join(groc_xy) |>
  mutate(walk_m = dist_m(bg_lat, bg_lon, groc_lat, groc_lon)) |>
  filter(walk_m <= 800) |>
  group_by(GEOID) |>
  summarise(walk_only_min = min(walk_m) / 1.2 / 60)

# Faster of the two (full_join keeps neighborhoods with only one option)
grocery <- full_join(by_bus, by_walk, by = "GEOID") |>
  mutate(grocery_min = pmin(bus_min, walk_only_min, na.rm = TRUE)) |>
  select(GEOID, grocery_min)

# ---- All results, one row per neighborhood ----
bg_results <- bg |>
  left_join(campus_8am, by = "GEOID") |>
  left_join(campus_10pm, by = "GEOID") |>
  left_join(campus_sat, by = "GEOID") |>
  left_join(grocery, by = "GEOID")

# ---- Summary table ----
access_summary <- function(minutes, label) {
  has <- !is.na(minutes)
  tibble(scenario = label,
         neighborhoods = sum(has),
         pct_people = sum(bg_results$popE[has]) / sum(bg_results$popE),
         pct_car_free = sum(bg_results$no_carE[has]) / sum(bg_results$no_carE),
         pct_car_free_30min = sum(bg_results$no_carE[which(minutes <= 30)]) /
           sum(bg_results$no_carE),
         median_min = median(minutes, na.rm = TRUE))
}

scenario_summary <- bind_rows(
  access_summary(bg_results$campus_8am_min,  "Campus: weekday, arrive by 8am"),
  access_summary(bg_results$campus_10pm_min, "Campus: weekday, leave after 10pm"),
  access_summary(bg_results$campus_sat_min,  "Campus: Saturday, arrive by noon"),
  access_summary(bg_results$grocery_min,     "Supermarket: Saturday, by noon")
)

# ---- Last bus actually leaving campus on a weekday ----
# Counts only if the trip visits an OFF-campus stop afterward.
campus_stop_ids <- gtfs$stops |>
  filter(stop_name %in% campus_stops) |>
  pull(stop_id)

st_day <- filter_stop_times(gtfs, extract_date = weekday)

last_offcampus <- st_day |>
  filter(!(stop_id %in% campus_stop_ids)) |>
  group_by(trip_id) |>
  summarise(last_off_seq = max(stop_sequence))

last_bus <- st_day |>
  filter(stop_id %in% campus_stop_ids) |>
  inner_join(last_offcampus, by = "trip_id") |>
  filter(stop_sequence < last_off_seq) |>
  summarise(last_departure = max(departure_time)) |>
  pull(last_departure)

# ---- Save ----
write_rds(bg_results, "data/processed/neighborhoods.rds")
write_rds(scenario_summary, "data/processed/scenario_summary.rds")
write_rds(last_bus, "data/processed/last_bus_from_campus.rds")
write_rds(analysis_dates, "data/processed/analysis_dates.rds")
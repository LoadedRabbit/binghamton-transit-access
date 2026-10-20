# 03_travel_times.R
# Travel times from every stop to BU's main campus. Saves the results.
# (Decisions and checks: scratch/phase2_travel_times.R)

library(tidyverse)
library(tidytransit)
source("R/functions.R")  # loads dist_m() and transit_times()

gtfs <- read_rds("data/processed/gtfs_prepared.rds")

# ---- Settings ----
analysis_date <- "2026-10-20"  # TODO before Phase 5: pick automatically
arrive_by     <- 8 * 3600      # arrive on campus by 8:00am

# ---- Campus stops: name starts with "BU" AND on the main campus ----
# (Name alone isn't enough: "BU Pharmacy School" is in Johnson City.)
campus_stops <- gtfs$stops |>
  filter(str_detect(stop_name, "^BU"),
         between(stop_lat, 42.08, 42.10),
         between(stop_lon, -75.98, -75.96)) |>
  pull(stop_name) |>
  unique()

# ---- Travel times to campus ----
tt <- transit_times(gtfs, campus_stops, analysis_date, arrive_by,
                    direction = "to")

# ---- Unreachable: served that day, but can't arrive in time ----
stops_served_day <- filter_stop_times(gtfs, extract_date = analysis_date) |>
  pull(stop_id) |>
  unique()

unreachable <- gtfs$stops |>
  filter(is.na(location_type) | location_type == 0) |>
  filter(!(stop_name %in% tt$stop_name)) |>
  filter(stop_id %in% stops_served_day)

# ---- Save ----
write_rds(campus_stops, "data/processed/campus_stops.rds")
write_rds(tt, "data/processed/travel_times.rds")
write_rds(unreachable, "data/processed/unreachable.rds")
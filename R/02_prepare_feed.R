# 02_prepare_feed.R
# Load the BC Transit feed and fix it for routing. Saves the result.
# (Investigation behind each fix: scratch/phase2_travel_times.R)

library(tidyverse)
library(tidytransit)
source("R/functions.R")  # loads dist_m()

gtfs <- read_gtfs("data/raw/bct_gtfs.zip")

# Fix 1: fill in blank stop times between timepoints, spaced evenly.
# Without this, stops with blank times look unserved.
# (use_shape_dist = FALSE: the feed's distance column isn't usable.)
gtfs <- interpolate_stop_times(gtfs, use_shape_dist = FALSE)

# Fix 2: walking transfers between stops within 250 m (1.2 m/s).
# The feed's own transfer rules don't let riders walk between nearby
# stops at the start of a trip (e.g. between BC Junction bays).


stops_xy <- gtfs$stops |>
  select(stop_id, stop_lat, stop_lon)

walk_transfers <- stops_xy |>
  cross_join(stops_xy, suffix = c("_from", "_to")) |>
  filter(stop_id_from != stop_id_to) |>
  mutate(dist = dist_m(stop_lat_from, stop_lon_from,
                       stop_lat_to, stop_lon_to)) |>
  filter(dist <= 250) |>
  transmute(from_stop_id = stop_id_from,
            to_stop_id = stop_id_to,
            transfer_type = 2L,
            min_transfer_time = as.integer(ceiling(dist / 1.2)))

gtfs$transfers <- bind_rows(gtfs$transfers, walk_transfers)

write_rds(gtfs, "data/processed/gtfs_prepared.rds")

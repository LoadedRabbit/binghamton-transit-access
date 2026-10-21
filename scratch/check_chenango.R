# ==== Check: 66 Chenango St (downtown) ====
# Google Maps: 40-50 min to campus, arriving by 8am.
# Model: BC Junction stops ~14-19 min. Which is right, and why?

library(tidyverse)
library(tidytransit)
source("R/functions.R")

gtfs <- read_rds("data/processed/gtfs_prepared.rds")
campus_stops <- read_rds("data/processed/campus_stops.rds")
tt <- read_rds("data/processed/travel_times.rds")
dates <- read_rds("data/processed/analysis_dates.rds")

# ---- 1. Stops within 300 m of BC Junction (stop "1") ----
# 66 Chenango St is right by the transportation center
bcj <- gtfs$stops |> filter(stop_id == "1")

nearby <- gtfs$stops |>
  mutate(dist = dist_m(stop_lat, stop_lon, bcj$stop_lat, bcj$stop_lon)) |>
  filter(dist <= 300) |>
  select(stop_id, stop_name, dist) |>
  arrange(dist)

nearby

# ---- 2. What the model says for those stops ----
tt |>
  filter(stop_name %in% nearby$stop_name) |>
  select(stop_name, travel_min, transfers,
         journey_departure_time, journey_arrival_time) |>
  arrange(travel_min) |>
  as.data.frame()

# ---- 3. Ground truth: direct buses in the schedule ----
# Every bus that stops near BC Junction and LATER at a campus stop,
# reaching campus between 6 and 8am on the analysis Tuesday.
# No routing algorithm: just the timetable.
campus_ids <- gtfs$stops |>
  filter(stop_name %in% campus_stops) |>
  pull(stop_id)

st_day <- filter_stop_times(gtfs, extract_date = dates$weekday)

boards <- st_day |>
  filter(stop_id %in% nearby$stop_id) |>
  select(trip_id, board_stop = stop_id, board_seq = stop_sequence,
         board_time = departure_time)

alights <- st_day |>
  filter(stop_id %in% campus_ids) |>
  select(trip_id, campus_stop = stop_id, campus_seq = stop_sequence,
         campus_time = arrival_time)

direct <- inner_join(boards, alights, by = "trip_id",
                     relationship = "many-to-many") |>
  filter(campus_seq > board_seq,
         campus_time >= hms::hms(hours = 6),
         campus_time <= hms::hms(hours = 8)) |>
  mutate(ride_min = as.numeric(campus_time - board_time) / 60) |>
  group_by(trip_id) |>
  slice_min(ride_min, n = 1, with_ties = FALSE) |>
  ungroup() |>
  left_join(select(gtfs$trips, trip_id, route_id), by = "trip_id") |>
  arrange(campus_time) |>
  select(route_id, trip_id, board_time, campus_time, ride_min)

direct |> as.data.frame()

# RESULT: the model is right. All downtown stops use Route 57
# (Shoppers Special): leaves BC Junction 7:10, campus ~7:23-7:25.
# BC Transit's published Route 57 timetable: BC Junction 7:10 ->
# Digman (campus) 7:25 = 15 min. Other options: Route 15 (27 min),
# Route 5 (30 min).
# Google's 40-50 min: likely door-to-door walking at both ends, a
# slower route, and/or "arrive by" picking a later trip.
# LIMITATION found: the map shows the FASTEST trip in the window
# (best case). Downtown depends on one fast bus (7:10 Route 57).
# Future improvement: also measure the typical trip across the hour.
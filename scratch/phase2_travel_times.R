# ==== Phase 2: First map ====
# Goal: on a weekday morning, how long does it take to get
# from every BC Transit stop to campus?

library(tidyverse)
library(tidytransit)
library(leaflet)

gtfs <- read_gtfs("data/raw/bct_gtfs.zip")

# How many stop times are blank (no time listed)?
sum(is.na(gtfs$stop_times$arrival_time))

# Trips with fewer than 2 listed times can't be interpolated. Which are they?
bad_trips <- gtfs$stop_times |>
  group_by(trip_id) |>
  summarise(n_arr = sum(!is.na(arrival_time)),
            n_dep = sum(!is.na(departure_time))) |>
  filter(n_arr < 2 | n_dep < 2)

nrow(bad_trips)

gtfs$trips |>
  filter(trip_id %in% bad_trips$trip_id) |>
  count(route_id, service_id)

# Fill in blank times between timepoints, spaced evenly between the
# timed stops. (use_shape_dist = FALSE because the feed's distance
# column isn't usable for this.) Without this, stops with blank
# times look unserved.
gtfs <- interpolate_stop_times(gtfs, use_shape_dist = FALSE)

# Check: should now be 0
sum(is.na(gtfs$stop_times$arrival_time))

# ---- Walking transfers ----
# The feed's transfer rules are "timed" (type 1), which tidytransit won't
# use for walking at the start of a trip (e.g. between bays at BC Junction).
# Add our own: any two stops within 250 m are connected by a walk.
# Assumption: walking speed 1.2 m/s (a standard planning value).

# Distance in meters between two points on the globe (haversine formula)
dist_m <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  a <- sin((lat2 - lat1) * to_rad / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) *
    sin((lon2 - lon1) * to_rad / 2)^2
  6371000 * 2 * asin(sqrt(a))
}

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

nrow(walk_transfers)

gtfs$transfers <- bind_rows(gtfs$transfers, walk_transfers)

# ---- find the campus stops ----


# Found: campus stop names start with "BU"

# Campus stops = names starting with "BU" AND inside the main Vestal campus.
# (Name alone isn't enough: "BU Pharmacy School" is in Johnson City.)
campus_stops <- gtfs$stops |>
  filter(str_detect(stop_name, "^BU"),
         between(stop_lat, 42.08, 42.10),
         between(stop_lon, -75.98, -75.96)) |>
  pull(stop_name) |>
  unique()

campus_stops

# Where is every "BU" stop? (some BU buildings are off the main campus)

gtfs$stops |>
  filter(str_detect(stop_name, "^BU")) |>
  select(stop_name, stop_lat, stop_lon) |>
  as.data.frame()

#checking just in case
gtfs$stops |>
  filter(between(stop_lat, 42.08, 42.10),
         between(stop_lon, -75.98, -75.96)) |>
  filter(!(stop_name %in% campus_stops)) |>
  pull(stop_name)

# Check result: the 10 stops it found are on the Vestal Pkwy (NY 434),
# a medical site, a plaza, and nearby roads, not on campus.
# Conclusion: the "^BU" list covers the campus stops.
# Why unique(): some stops share a name (one per side of the street),
# e.g. "Vestal/Old Lane" appears twice.
# Limitation: if BC Transit ever adds a campus stop without "BU"
# in the name, this list would miss it. Rerun this check
# when a new feed comes out.

# ---- Step 3: design decisions ----
# Date: Tuesday 2026-10-20
#   (inside the feed's range, before BU trips end Dec 8,
#    a normal weekday with no special Wednesday school trips)
# Time window: ARRIVALS at campus by 8:00am
#   (a student with an 8am class cares about arriving on time,
#    so use arrival = TRUE in travel_times())
# Long-run note: this date is hard-coded for now. Before Phase 5,
# the code must pick a date inside the feed's range on its own.

#step 4
# Cut the schedule down to Tue Oct 20, trips between 6:00 and 8:00am
# (times in seconds after midnight: 6 * 3600 = 6:00am, 8 * 3600 = 8:00am)
stop_times_tue <- filter_stop_times(gtfs,
                                    extract_date = "2026-10-20",
                                    min_departure_time = 6 * 3600,
                                    max_arrival_time = 8 * 3600)

stop_group_distances(gtfs$stops) |>
  filter(dist_max > 300)

# Result: only "Broome Corporate..." (8 stops, avg ~750 m apart) breaks
# the 300 m rule. Not a campus stop. Safe to set stop_dist_check = FALSE.
# Its travel time is approximate (8 stops treated as one place).
# Long-run: rerun this check when a new feed comes out.

tt <- travel_times(stop_times_tue,
                   stop_name = campus_stops,
                   time_range = 3600,
                   arrival = TRUE,
                   stop_dist_check = FALSE,
                   return_coords = TRUE)


glimpse(tt)

#converting from seconds to minutes
tt<-tt|>
  mutate(travel_min = travel_time/60)

#longest trips to campus first
tt|>
  arrange(desc(travel_min)) |>
  select(from_stop_name, travel_min, transfers, journey_departure_time) |>
  head(10)

# After interpolation: recheck stops from before
tt |>
  filter(str_detect(from_stop_name, "Main /Avenue B|Glendale|BC Junction 12")) |>
  select(from_stop_name, travel_min, transfers) |>
  as.data.frame()

# After interpolation: Main/Avenue B 80 -> 38 min, Glendale ~58 -> ~53,
# BC Junction 12 now 14 min with 0 transfers (the earlier "1 transfer"
# was caused by blank times, not a counting quirk; resolved).
# Google Maps check for Main/Avenue B: 25 min

# Google says 25 min (arrive ~7:46). Mine says 38. Compare the trips.
tt |>
  filter(from_stop_name == "Main /Avenue B") |>
  select(from_stop_name, travel_min, transfers,
         journey_departure_time, journey_arrival_time) |>
  as.data.frame()

# Explained: Google's 25 min includes ~14 min walking (~1 km) to a
# Route 15 stop. Mine is boarding AT Main/Avenue B, with walks capped
# at 250 m. Different questions, both correct. The Route 15 stops
# Google uses have their own (short) times on the map.
# Phase 3 improvement: neighborhood-level times that let people walk
# to the fastest nearby stop.

# Did any Main St stops become unreachable instead?
unreachable_tue |>
  filter(str_detect(stop_name, "Main")) |>
  pull(stop_name)

# After walking transfers: BC Junction should now be a short trip
tt |>
  filter(str_detect(from_stop_name, "BC Junction")) |>
  select(from_stop_name, travel_min, transfers) |>
  arrange(travel_min)

# To verify later: how is the "transfers" column counted?
# (Shows 1 even from the Route 15 bay.)

# Red flag: BC Junction stops show 77-97 min to campus.
# Suspect: model can't walk between bays (transfer rules were ignored).

# 1. What transfer rules does the feed have?
gtfs$transfers |>
  as.data.frame() |>
  head(20)

# 2. How is BC Junction split up?
gtfs$stops |>
  filter(str_detect(stop_name, "BC Junction")) |>
  select(stop_id, stop_name) |>
  as.data.frame()

nrow(walk_transfers)


tt |>
  arrange(desc(travel_min)) |>
  select(from_stop_name, travel_min, transfers, journey_departure_time) |>
  head(10)


# Check a surprising result: far away but under 60 min?
tt |>
  filter(str_detect(from_stop_name, "Glendale")) |>
  select(from_stop_name, travel_min, transfers,
         journey_departure_time, journey_arrival_time) |>
  as.data.frame()

#step 6
unreachable <- gtfs$stops |>
  filter(is.na(location_type) | location_type == 0) |>
  filter(!(stop_name %in% tt$from_stop_name))

nrow(unreachable)

# How many distinct places (names), not stop IDs?
n_distinct(unreachable$stop_name)

# Do these stops get ANY bus on Tuesday, at any time of day?
stops_served_tue <- filter_stop_times(gtfs, extract_date = "2026-10-20") |>
  pull(stop_id) |>
  unique()

unreachable |>
  mutate(served_tuesday = stop_id %in% stops_served_tue) |>
  count(served_tuesday)

# Of the stops with no Tuesday bus: are they used on ANY day?
unreachable |>
  mutate(served_tuesday = stop_id %in% stops_served_tue,
         in_any_schedule = stop_id %in% gtfs$stop_times$stop_id) |>
  count(served_tuesday, in_any_schedule)

# Which routes/service days serve the stops with no Tuesday bus?
unreachable |>
  filter(!(stop_id %in% stops_served_tue)) |>
  select(stop_id) |>
  inner_join(gtfs$stop_times, by = "stop_id") |>
  inner_join(gtfs$trips, by = "trip_id") |>
  distinct(stop_id, route_id, service_id) |>
  count(route_id, service_id, sort = TRUE)


# Result: after interpolating, 73 stops can't reach campus by 8am.
#   59 are served Tuesday, just not in time -> show as hollow circles.
#   14 are Route 47 Saturday-only -> leave off the weekday map.
unreachable_tue <- unreachable |>
  filter(stop_id %in% stops_served_tue)

nrow(unreachable_tue)

# ---- Step 7: map ----
# Color scale: viridis (color-blind friendly).
# Bright yellow = short trip, dark purple = long trip.
pal <- colorNumeric("viridis", domain = tt$travel_min, reverse = TRUE)

leaflet() |>
  addTiles() |>
  # Hollow gray circles: served Tuesday, but can't arrive by 8am
  addCircleMarkers(data = unreachable_tue,
                   lng = ~stop_lon,
                   lat = ~stop_lat,
                   radius = 5,
                   color = "gray",
                   weight = 2,
                   fill = FALSE,
                   label = ~paste0(stop_name, ": can't arrive by 8am")) |>
  # Filled circles: travel time to campus
  addCircleMarkers(data = tt,
                   lng = ~from_stop_lon,
                   lat = ~from_stop_lat,
                   radius = 5,
                   stroke = FALSE,
                   fillOpacity = 0.8,
                   color = ~pal(travel_min),
                   label = ~paste0(from_stop_name, ": ",
                                   round(travel_min), " min")) |>
  addLegend(pal = pal,
            values = tt$travel_min,
            title = "Minutes to campus<br>(arrive by 8am Tue)") |>
  addLegend(colors = "gray",
            labels = "Hollow circle: can't arrive by 8am")
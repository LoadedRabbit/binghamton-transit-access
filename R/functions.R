# functions.R
# Helper functions shared across scripts. Load with source("R/functions.R").

# Distance in meters between two points on the globe (haversine formula)
dist_m <- function(lat1, lon1, lat2, lon2) {
  to_rad <- pi / 180
  a <- sin((lat2 - lat1) * to_rad / 2)^2 +
    cos(lat1 * to_rad) * cos(lat2 * to_rad) *
    sin((lon2 - lon1) * to_rad / 2)^2
  6371000 * 2 * asin(sqrt(a))
}

# Fastest scheduled bus trip between every stop and a set of destination
# stops (e.g. campus), on one date.
#   direction = "to":   arriving at dest_stops within 1 hour before `time`
#   direction = "from": leaving dest_stops within 1 hour after `time`
# `time` is in seconds after midnight (e.g. 8 * 3600 = 8:00am).
# Returns one row per reachable stop. Needs tidyverse and tidytransit.
transit_times <- function(gtfs, dest_stops, date, time, direction = "to") {
  arriving <- direction == "to"
  
  # Allow up to 2 hours of travel before (or after) the target time
  stop_times_day <- filter_stop_times(
    gtfs,
    extract_date = date,
    min_departure_time = if (arriving) time - 2 * 3600 else time,
    max_arrival_time   = if (arriving) time else time + 2 * 3600
  )
  
  # stop_dist_check = FALSE: only "Broome Corporate..." breaks the
  # 300 m rule, and it's never a destination.
  # Expected warning: "unsupported route-to-route transfers". Harmless:
  # the walking transfers from 02_prepare_feed.R cover these.
  tt <- travel_times(stop_times_day,
                     stop_name = dest_stops,
                     time_range = 3600,
                     arrival = arriving,
                     stop_dist_check = FALSE,
                     return_coords = TRUE)
  
  # Keep the "other end" of each trip: where the rider starts ("to"),
  # or where they end up ("from"). Same column names either way.
  if (arriving) {
    tt <- rename(tt, stop_name = from_stop_name,
                 stop_lat = from_stop_lat, stop_lon = from_stop_lon)
  } else {
    tt <- rename(tt, stop_name = to_stop_name,
                 stop_lat = to_stop_lat, stop_lon = to_stop_lon)
  }
  
  tt |>
    mutate(travel_min = travel_time / 60) |>
    select(stop_name, stop_lat, stop_lon, travel_min, transfers,
           journey_departure_time, journey_arrival_time)
}

# Best total trip time for each neighborhood: walk to (or from) a nearby
# stop + bus time. Riders walk up to max_walk meters, at 1.2 m/s.
#   bg_pts: one row per neighborhood with GEOID, bg_lat, bg_lon
#   tt:     output of transit_times()
neighborhood_times <- function(bg_pts, tt, max_walk = 800) {
  bg_pts |>
    cross_join(select(tt, stop_name, stop_lat, stop_lon, travel_min)) |>
    mutate(walk_m = dist_m(bg_lat, bg_lon, stop_lat, stop_lon)) |>
    filter(walk_m <= max_walk) |>
    mutate(walk_min = walk_m / 1.2 / 60,
           total_min = walk_min + travel_min) |>
    group_by(GEOID) |>
    slice_min(total_min, n = 1, with_ties = FALSE) |>
    ungroup() |>
    select(GEOID, best_stop = stop_name, walk_min, travel_min, total_min)
}

# Pick the next date (from today) when the feed has service on a given
# day of the week. day: 1 = Sunday, 2 = Monday, ..., 7 = Saturday.
# prefer: optional pattern of service_ids to prefer, e.g. "BU" for days
# when Binghamton University trips run (during the semester).
pick_date <- function(gtfs, day, prefer = NULL) {
  candidates <- gtfs$.$dates_services |>
    filter(date >= Sys.Date(), lubridate::wday(date) == day)
  
  if (!is.null(prefer)) {
    preferred <- candidates |> filter(str_detect(service_id, prefer))
    if (nrow(preferred) > 0) candidates <- preferred
  }
  
  if (nrow(candidates) == 0) {
    stop("The feed has no upcoming service on day ", day,
         ". Is the feed out of date?")
  }
  
  as.character(min(candidates$date))
}
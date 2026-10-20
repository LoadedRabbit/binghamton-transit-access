library(tidyverse)
library(tidytransit)

gtfs <- read_gtfs("data/raw/bct_gtfs.zip")

# ---- Phase 1 gate ----
# A route has many trips, each trip has many stop times,
# and each stop time points to a stop.
# Links: routes -> trips by route_id,
#        trips -> stop_times by trip_id,
#        stop_times -> stops by stop_id.

# ---- Is the feed current? ----
select(gtfs$calendar, service_id, sunday, start_date, end_date)

# Feed covers: (2026-08-20) to (2027-12-31)
# Does it include today (Oct 18, 2026)? Yes
# BU trips end Dec 8, 2026 because: winter break
# This means the map will need: the map must
# download it, rerun, and republish automatically (Phase 5).

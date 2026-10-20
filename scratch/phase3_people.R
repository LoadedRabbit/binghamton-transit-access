# ==== Phase 3: People and places ====

library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(tidytransit)
source("R/functions.R")  # loads dist_m(), transit_times(), neighborhood_times()
tt <- read_rds("data/processed/travel_times.rds")
gtfs <- read_rds("data/processed/gtfs_prepared.rds")
campus_stops <- read_rds("data/processed/campus_stops.rds")

# ---- Census block groups in Broome County ----
# ACS 5-year estimates. Variables:
#   B01003_001 = total population
#   B25044_001 = total households
#   B25044_003 = owner households with no vehicle
#   B25044_010 = renter households with no vehicle
bg <- get_acs(geography = "block group",
              state = "NY",
              county = "Broome",
              variables = c(pop = "B01003_001",
                            hh = "B25044_001",
                            no_car_own = "B25044_003",
                            no_car_rent = "B25044_010"),
              year = 2024,
              geometry = TRUE,
              output = "wide")

glimpse(bg)

# ---- Households without a car ----
# Total = owners + renters with no vehicle.
# Margin of error for a sum: sqrt(M1^2 + M2^2) (Census Bureau's formula)
bg <- bg |>
  mutate(no_carE = no_car_ownE + no_car_rentE,
         no_carM = sqrt(no_car_ownM^2 + no_car_rentM^2),
         pct_no_car = no_carE / hhE)

# Neighborhoods with the most car-free households
bg |>
  st_drop_geometry() |>
  select(NAME, popE, hhE, no_carE, no_carM, pct_no_car) |>
  arrange(desc(no_carE)) |>
  head(10)

# Sanity check: county totals
sum(bg$popE)
sum(bg$hhE)
sum(bg$no_carE)

# County total 197,378 (checks out). ~11,100 of 82,000 households
# (~13.5%) have no car. Top neighborhoods: 50-70% car-free.
# Margins of error are large: present these as approximate.v

# ---- Step 4: neighborhood travel times ----
# One point per neighborhood, guaranteed inside its boundary.
# Max walk to a stop: 800 m (~11 min at 1.2 m/s).
pts <- st_point_on_surface(bg)
coords <- st_coordinates(pts)
bg_pts <- tibble(GEOID = pts$GEOID,
                 bg_lon = coords[, "X"],
                 bg_lat = coords[, "Y"])

# Every stop within 800 m of each neighborhood: walk + bus time
access <- bg_pts |>
  cross_join(tt |> select(stop_name, stop_lat,
                          stop_lon, travel_min)) |>
  mutate(walk_m = dist_m(bg_lat, bg_lon, stop_lat, stop_lon)) |>
  filter(walk_m <= 800) |>
  mutate(walk_min = walk_m / 1.2 / 60,
         total_min = walk_min + travel_min)

# Fastest option for each neighborhood
bg_time <- access |>
  group_by(GEOID) |>
  slice_min(total_min, n = 1, with_ties = FALSE) |>
  ungroup() |>
  select(GEOID, best_stop = stop_name, walk_min, travel_min, total_min)

# Attach to the neighborhoods (keeps all 197, even without a stop)
bg <- bg |>
  left_join(bg_time, by = "GEOID")

# Neighborhoods with no stop within 800 m that reaches campus by 8am
sum(is.na(bg$total_min))

bg |>
  st_drop_geometry() |>
  filter(is.na(total_min)) |>
  summarise(people = sum(popE), car_free_households = sum(no_carE))

# Result: 80 of 197 neighborhoods (~87,000 people, ~44%) have no stop
# within 800 m reaching campus by 8am. But only ~2,000 of ~11,000
# car-free households live there (~18%): most car-free households are
# in the urban core, where buses run.

# Check: the most car-free neighborhoods should all have access
bg |>
  st_drop_geometry() |>
  arrange(desc(no_carE)) |>
  select(NAME, no_carE, pct_no_car, best_stop, walk_min, total_min) |>
  head(10) |>
  as.data.frame()

# All 10 most car-free neighborhoods have access (mostly 15-30 min total).
# Check the fastest: Floral/Cook, under 4 min on the bus. Is the stop
# close enough to campus for that to be realistic?
tt |>
  filter(stop_name == "Floral/Cook") |>
  mutate(km_to_campus = dist_m(stop_lat, stop_lon,
                               42.08705, -75.96706) / 1000) |>
  select(stop_name, travel_min, transfers, km_to_campus,
         journey_departure_time, journey_arrival_time) |>
  as.data.frame()

# Floral/Cook: 2.25 km from campus in 3.8 min (~36 km/h). Realistic.
# Its odd departure time (7:43:44) shows it's an interpolated stop time:
# estimated times can be off by a minute or two. Mention in write-up.

# ---- Step 5: neighborhood map ----
# leaflet needs standard GPS coordinates, so convert first
bg_map <- st_transform(bg, 4326)

# Viridis (color-blind friendly): bright = quick, dark = long.
# Neighborhoods with no access: no fill, outline only.
pal_bg <- colorNumeric("viridis", domain = bg_map$total_min,
                       reverse = TRUE, na.color = "transparent")

leaflet(bg_map) |>
  addTiles() |>
  addPolygons(fillColor = ~pal_bg(total_min),
              fillOpacity = 0.7,
              color = "gray30",
              weight = 0.5,
              label = ~ifelse(is.na(total_min),
                              "No stop within 800 m that reaches campus by 8am",
                              paste0(round(total_min), " min to campus (",
                                     round(walk_min), " min walk to ",
                                     best_stop, ")")),
              popup = ~paste0("<b>", NAME, "</b><br>",
                              "Households: ", hhE, "<br>",
                              "Car-free households: about ", no_carE,
                              " (\u00b1", round(no_carM), ")")) |>
  addLegend(pal = pal_bg,
            values = ~total_min,
            title = "Minutes to campus,<br>incl. walk to stop<br>(arrive by 8am Tue)",
            na.label = "No access (outline only)")

# Neighborhood map: pattern looks right (urban core filled, rural
# areas mostly outline-only). Sharp color jumps between neighbors are
# real effects (being near a direct route vs. needing a transfer,
# rivers), but also exaggerated by measuring each neighborhood from
# one point. Mention as a limitation in the write-up.|

# ---- Step 6: times of day ----
# Tue Oct 20 and Sat Oct 24, 2026: both inside the feed's date range,
# during the semester (BU trips running).
tt_weekday_8am  <- transit_times(gtfs, campus_stops, "2026-10-20",
                                 8 * 3600, direction = "to")
tt_weekday_10pm <- transit_times(gtfs, campus_stops, "2026-10-20",
                                 22 * 3600, direction = "from")
tt_saturday     <- transit_times(gtfs, campus_stops, "2026-10-24",
                                 12 * 3600, direction = "to")

# Summarize one scenario: who has access, and how long trips take
summarise_access <- function(tt, label) {
  times <- neighborhood_times(bg_pts, tt)
  
  bg |>
    st_drop_geometry() |>
    select(GEOID, popE, no_carE) |>
    left_join(times, by = "GEOID") |>
    summarise(scenario = label,
              neighborhoods = sum(!is.na(total_min)),
              pct_people = sum(popE[!is.na(total_min)]) / sum(popE),
              pct_car_free = sum(no_carE[!is.na(total_min)]) / sum(no_carE),
              median_min = median(total_min, na.rm = TRUE))
}

bind_rows(
  summarise_access(tt_weekday_8am,  "Weekday: arrive by 8am"),
  summarise_access(tt_weekday_10pm, "Weekday: leave campus after 10pm"),
  summarise_access(tt_saturday,     "Saturday: arrive by noon")
)

# 10pm result: only 1 neighborhood. Real, or a bug?
# Check: last BC Transit departure from campus on Tuesday
campus_stop_ids <- gtfs$stops |>
  filter(stop_name %in% campus_stops) |>
  pull(stop_id)

filter_stop_times(gtfs, extract_date = "2026-10-20") |>
  filter(stop_id %in% campus_stop_ids) |>
  summarise(first_departure = min(departure_time),
            last_departure = max(departure_time))

# Which neighborhood still has access at 10pm?
neighborhood_times(bg_pts, tt_weekday_10pm)

# Last campus departure is 10:15pm, yet no trips home after 10pm.
# Check: are the late buses ending their trips at campus?
late <- filter_stop_times(gtfs, extract_date = "2026-10-20") |>
  filter(stop_id %in% campus_stop_ids,
         departure_time >= 21 * 3600) |>
  select(trip_id, stop_id, stop_sequence, departure_time)

late

# Last stop number on each of those trips
gtfs$stop_times |>
  filter(trip_id %in% late$trip_id) |>
  group_by(trip_id) |>
  summarise(last_stop_sequence = max(stop_sequence))

# Confirmed: every bus at campus after 9pm ENDS its trip there.
# Last bus actually LEAVING campus on weekdays: 8:31pm (Route 17,
# matches Google Maps). After that, no BC Transit rides home.
# Students rely on OCCT late at night (not in this data).

# When does the last bus actually LEAVE campus?
# A campus departure only counts if the trip visits an OFF-campus
# stop afterward. (A first version counted a bus moving between two
# campus stops at 9:23pm. Google Maps caught it: last trip 8:31pm.)
st_day <- filter_stop_times(gtfs, extract_date = "2026-10-20")

# For each trip, the last stop number that's off campus
last_offcampus <- st_day |>
  filter(!(stop_id %in% campus_stop_ids)) |>
  group_by(trip_id) |>
  summarise(last_off_seq = max(stop_sequence))

st_day |>
  filter(stop_id %in% campus_stop_ids) |>
  inner_join(last_offcampus, by = "trip_id") |>
  filter(stop_sequence < last_off_seq) |>
  summarise(last_departure = max(departure_time))
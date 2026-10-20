# ==== Phase 3: People and places ====

library(tidyverse)
library(tidycensus)
library(sf)
library(leaflet)
library(tidytransit)
library(osmdata)
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

# ---- Step 7: grocery access ----
# Grocery = supermarket (OpenStreetMap shop=supermarket). Excludes
# convenience stores: you can't do a full week's shopping there.

# Download supermarkets in a box around Broome County
bbox <- st_bbox(st_transform(bg, 4326))

osm <- opq(bbox) |>
  add_osm_feature(key = "shop", value = "supermarket") |>
  osmdata_sf()

# Stores are mapped as single points or as building outlines.
# Use one point per store either way.
store_points <- osm$osm_points |>
  filter(!is.na(shop)) |>
  select(name)

store_buildings <- osm$osm_polygons |>
  st_centroid() |>
  select(name)

groceries <- bind_rows(store_points, store_buildings)

nrow(groceries)
groceries |> st_drop_geometry() |> count(name, sort = TRUE) |> as.data.frame()

# Store list checked against store directories and local news (Oct 2026):
# no major chain missing (no Market 32 or Tops in Broome County).
# Weis 6, Price Chopper 5, ALDI 3, Walmart 2, Wegmans 1 are consistent.
# Kept small independent markets: they sell real groceries.
# Limitation: OpenStreetMap is volunteer-built; spot-checked, not complete.
# Validation for later: BU says the North Side of Binghamton has no major
# grocery within walking distance. Results should agree.

# ---- Step 7b: grocery trip times (Saturday, arrive by noon) ----
# Supermarket locations as a plain table
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

length(grocery_stops)

# Bus trips from every stop to the nearest grocery stop
tt_grocery <- transit_times(gtfs, grocery_stops, "2026-10-24",
                            12 * 3600, direction = "to")

# Option 1: walk to a bus stop + ride to a grocery stop
by_bus <- neighborhood_times(bg_pts, tt_grocery) |>
  select(GEOID, bus_min = total_min)

# Option 2: walk straight to a supermarket (up to 800 m)
by_walk <- bg_pts |>
  cross_join(groc_xy) |>
  mutate(walk_m = dist_m(bg_lat, bg_lon, groc_lat, groc_lon)) |>
  filter(walk_m <= 800) |>
  group_by(GEOID) |>
  summarise(walk_only_min = min(walk_m) / 1.2 / 60)

# Each neighborhood's faster option
grocery_access <- bg |>
  st_drop_geometry() |>
  select(GEOID, NAME, popE, no_carE, pct_no_car) |>
  left_join(by_bus, by = "GEOID") |>
  left_join(by_walk, by = "GEOID") |>
  mutate(grocery_min = pmin(bus_min, walk_only_min, na.rm = TRUE))

# Share of car-free households within 30 min of a supermarket
grocery_access |>
  summarise(
    pct_car_free_30min = sum(no_carE[which(grocery_min <= 30)]) / sum(no_carE),
    pct_car_free_no_access = sum(no_carE[is.na(grocery_min)]) / sum(no_carE),
    median_min = median(grocery_min, na.rm = TRUE))

# Neighborhoods with the most car-free households beyond 30 min
grocery_access |>
  filter(is.na(grocery_min) | grocery_min > 30) |>
  arrange(desc(no_carE)) |>
  select(NAME, no_carE, pct_no_car, grocery_min) |>
  head(10) |>
  as.data.frame()

# Check: neighborhoods with no grocery access on Saturday.
# Can they reach CAMPUS by bus on Saturday? If many can, something's off.
sat_campus <- neighborhood_times(bg_pts, tt_saturday) |>
  select(GEOID, campus_min = total_min)

grocery_access |>
  filter(is.na(grocery_min)) |>
  left_join(sat_campus, by = "GEOID") |>
  count(can_reach_campus_saturday = !is.na(campus_min))

# Check passed: all 78 neighborhoods with no Saturday grocery access
# also can't reach campus by bus on Saturday. No usable Saturday bus
# nearby, so the result is consistent, not a grocery-specific bug.
# Grocery results (Saturday, arrive by noon):
#   ~82% of car-free households reach a supermarket within 30 min
#   (bus or walk); ~16% have no access; median ~7 min.
#   Bus times end at the grocery stop (excludes a final walk of up to 5 min).
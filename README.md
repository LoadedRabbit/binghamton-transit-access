# Binghamton Transit Access

How easy is it to get around Broome County, New York, without a car? This
project maps travel times by bus (BC Transit) from every neighborhood to
Binghamton University's main campus and to supermarkets, at different
times of day, and updates itself every month as schedules change.

**Live map:** https://loadedrabbit.github.io/binghamton-transit-access/

## Key findings (as of October 2026)

See the live map for current numbers.

- **Evenings are the biggest gap.** BC Transit's last bus out of campus
  on weekdays leaves at 8:31 pm. After that, only OCCT (the student bus
  system) runs: to the West Side and Downtown until 11:45 pm, but to the
  Southside and Johnson City's Main Street only until 9:35 pm.
- **Weekday mornings work for most car-free households.** About 82% live
  within a short walk of a bus that reaches campus by 8 am. The typical
  trip takes about 28 minutes, including the walk to the stop.
- **Weekends are slower.** On Saturdays, similar coverage, but the typical
  trip takes about 38 minutes.
- **Groceries:** about 82% of car-free households can reach a supermarket
  within 30 minutes on a Saturday, by bus or on foot.

## How it works

The pipeline is a series of R scripts, run in order:

| Script | What it does |
|---|---|
| `R/01_download.R` | Downloads BC Transit's public schedule data (GTFS) |
| `R/02_prepare_feed.R` | Fills in estimated stop times between scheduled timepoints, and adds walking transfers between stops within 250 m |
| `R/03_travel_times.R` | Calculates the fastest scheduled trip from every stop to campus |
| `R/04_download_context.R` | Downloads Census neighborhood data and supermarket locations (run about once a year) |
| `R/05_neighborhoods.R` | Combines everything into neighborhood results for four scenarios |

Shared functions live in `R/functions.R`. The website (`index.qmd`,
`methods.qmd`) is built with Quarto and published with GitHub Pages from
the `docs` folder.

**Automation:** on the 3rd of every month, a GitHub Actions workflow
(`.github/workflows/monthly-update.yml`) downloads the newest schedule,
reruns the pipeline, rebuilds the site, and publishes it. The analysis
dates are chosen automatically: the next Tuesday when Binghamton
University trips run, and the next Saturday.

## Data sources

- **Bus schedules:** BC Transit's public GTFS feed (Broome County)
- **Neighborhoods:** U.S. Census Bureau, American Community Survey 5-year
  estimates (2020-2024), by block group
- **Supermarkets:** OpenStreetMap (`shop=supermarket`), spot-checked
  against store directories
- **OCCT evening times:** OCCT's published Fall 2026 schedule (headline
  only, not part of the map)

## Assumptions and limitations

- Riders walk up to 800 m to a stop, at 1.2 m/s.
- Each neighborhood is measured from one point inside it, so large rural
  areas are approximate.
- Times are based on schedules, not real-time bus locations. Some stop
  times are estimated between scheduled timepoints.
- **OCCT is not on the map**, since its schedules aren't in BC Transit's
  data. Students near OCCT routes have more options than shown.
- Census numbers are survey estimates with margins of error.

## Running it yourself

You'll need R, Quarto, and (for `04_download_context.R`) a free Census API
key from https://api.census.gov/data/key_signup.html.

```r
# Install the exact package versions this project uses
renv::restore()

# Run the pipeline
source("R/01_download.R")
source("R/02_prepare_feed.R")
source("R/03_travel_times.R")
source("R/05_neighborhoods.R")

# Build the website into docs/
quarto::quarto_render()
```

## Project structure

```
R/          pipeline scripts and shared functions
data/raw/   downloaded data, as received
data/processed/  results the website reads
scratch/    exploration and checks behind each decision
docs/       the built website
log.md      project log: decisions, problems found, and fixes
```

## About

Built by [Your Name], a student at Binghamton University, using R,
tidytransit, sf, leaflet, and Quarto.
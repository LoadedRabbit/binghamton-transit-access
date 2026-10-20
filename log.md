10/18/2026 set up project

changed domain url and  downloaded to data/raw
includes today

## 2026-10-19
- Phase 2: first map of travel times to campus (Tue 8am arrival).
- Fixed 3 data problems: off-campus "BU" stop (Pharmacy School),
  no walking between nearby stops (BC Junction bays), blank stop
  times (interpolated). Unreachable stops dropped 291 -> 59.
- Verified against Google Maps. Differences come from walking:
  Google allows long walks; my map is stop-to-stop, 250 m max.
- Phase 2 gate passed.

## 2026-10-19 (evening)
- Phase 3 done. Census block groups + neighborhood travel times.
- Built transit_times() and neighborhood_times() in R/functions.R.
- Times of day: last bus leaves campus 8:31pm weekdays (matches Google).
  Caught a bug where a bus moving between two campus stops counted as leaving.
- Groceries: 24 supermarkets from OpenStreetMap, spot-checked against
  store directories. ~82% of car-free households within 30 min on Saturday.
  
- Phase 3 pipeline: R/04_download_context.R (Census + supermarkets) and
  R/05_neighborhoods.R (all scenarios, groceries, last bus). Verified
  from a blank slate: matches scratch results exactly.
- NEXT: Phase 0 conversations before building the website.

- Phase 4 started: Quarto website skeleton (_quarto.yml, index.qmd,
  methods.qmd). Builds to docs/. Draft numbers fill in automatically.
  
## Phase 0 (research + proxy answers)
- Research: ~7,000 students live off campus, most on the West Side.
  Students ride OCCT and BC Transit free. OCCT Late Nite: Fri/Sat only.
  OCCT doesn't run on summer weekends.
- My answers (as a typical student): ride both buses about equally;
  would use the map when choosing housing, or when OCCT isn't running
  (Google Maps wins for planning one trip). Top problems: getting home
  at night, groceries (nearly tied).
- Design: headline = night gap; map views = groceries, weekday,
  Saturday, night. Still to confirm with 2 real students: which bus
  they ride, and whether the map makes sense to them.
  
- OCCT Fall 2026 schedule: last weekday buses leave campus at 11:45pm
  to the West Side/Downtown (WS, DCL), but 9:35pm to Southside (DS)
  and Main St/Johnson City (MS). Late Nite + Downtown Express: Fri/Sat
  only. Shopping shuttles to Wegmans/Aldi and Target/Walmart until ~9pm.
- Night headline (accurate for students): BC Transit's last bus leaves
  campus 8:31pm; after that only OCCT, until 11:45pm (West Side) or
  9:35pm (Southside, Main St).
  
  
- Website v1: night headline (BC Transit + OCCT context), map with 4
  views (groceries, weekday 8am, Saturday, weeknight 10pm), shared color
  scale, dashed outlines for no-access areas. OCCT times typed by hand:
  recheck each semester.
  
- Project on GitHub: github.com/LoadedRabbit/binghamton-transit-access
  (branch is "master", not "main": matters for Phase 5).
- Website live: https://loadedrabbit.github.io/binghamton-transit-access/
  (published from /docs via GitHub Pages).
- To update the site: quarto::quarto_render(as_job = FALSE), then git_add, git_commit,
  git_push.
- NEXT: 2 friend check-ins with the live link (Phase 4 gate), then
  Phase 5 automation.
  
## Phase 5: automation
- Step 1: pick_date() in R/functions.R picks the next semester Tuesday
  ("BU" service) and next Saturday the feed covers. Errors loudly if
  the feed has no upcoming dates. Replaced hard-coded dates in 03 and
  05. Website shows which dates it's based on. Verified: same results.
- Decision: 04_download_context.R (Census, supermarkets) runs by hand
  about once a year, not monthly.
- Step 2: renv locks package versions (renv.lock). Confirmed the
  pipeline runs and gives the same results inside renv.
  
- Step 3: .github/workflows/monthly-update.yml runs 01, 02, 03, 05,
  rebuilds the site, and publishes on the 3rd of each month (10:17 UTC).
  Writes data/processed/last_run.txt every run, so there's always a
  commit (GitHub disables scheduled workflows after 60 days of no
  activity). Can also run by hand: GitHub, Actions tab, Run workflow.
  
- Step 4: first run by hand succeeded. The bot ran the pipeline,
  rebuilt the site, and published it. Phase 5 done.
- Habit: run gert::git_pull() at the start of every session.
- Recurring by-hand tasks:
  - Each semester: recheck OCCT times typed into index.qmd
    (11:45pm, 9:35pm) against OCCT's new schedule.
  - Once a year: rerun R/04_download_context.R (new Census data,
    supermarket list), and bump acs_year when a new ACS release is out.
    
- TODO before launch: phone test (headline readable, menu works, map
  not covered by view box/legend, tapping shows labels, can scroll
  past the map).
  
- Added "Data last checked for updates" to the site, from
  data/processed/last_run.txt (UTC, converted to New York time).
  Shows "not yet" if the file doesn't exist. Built the sentence in the
  code chunk (schedule_note) instead of inline: copying inline code
  from chat added stray backslashes twice.
# Chennai northeast monsoon: wet and dry spells

An R Shiny app for the Statistics with R group project "Wet and Dry Spell Dynamics of
Chennai's Northeast Monsoon: A Markov Chain Analysis with Extreme-Rainfall Risk".
It is deliverable 4 of the one-page plan, and every number in it comes from the same
R functions you can run on their own for the report.

The same app runs two ways: in R on your computer, and as a website on Vercel, where
Shinylive runs R inside the visitor's browser (no R server needed).

## Run it

1. Install R 4.3 or newer (built and tested with R 4.6.1).
2. Install the packages once:

   ```r
   install.packages(c("shiny", "bslib", "ggplot2", "ggiraph", "withr", "jsonlite", "shinylive"))
   ```

3. From this folder:

   ```r
   shiny::runApp("app")
   ```

   In RStudio you can also open `app/app.R` and press Run App. The app needs no
   internet connection: the data and the DM Sans font are inside `app/`.

The slow simulations (1,000 simulated records for every month and decade, the
season bootstrap and 20,000 simulated seasons for the buffer days) are run once by
`prepare/precompute.R` and stored in `app/data/precomputed.rds`, so the app opens in
a few seconds. Run that script again after changing the data or anything in `app/R`.

## Host it on Vercel

Vercel only serves static files and has no R, so the app is exported with
[Shinylive](https://posit-dev.github.io/r-shinylive/): R itself (compiled to
WebAssembly) runs in the browser, with the app and its packages.

1. Build the site (needs `install.packages("shinylive")` once):

   ```r
   # from this folder, in R or with Rscript
   source("deploy/build_site.R")
   ```

   This writes `site/`. Commit it: Vercel publishes it exactly as it is and runs no
   build of its own.
2. Push this folder to a GitHub repository and import it in Vercel (Add New, Project).
   `vercel.json` already sets the framework to Other and the output folder to `site`,
   so the defaults can stay as they are.
3. Every push to the main branch deploys again. After changing the app, rebuild
   `site/` (step 1) before you commit.

The first visit downloads R and the packages (about 40 MB), so the page takes
10 to 30 seconds to start; browsers keep them for later visits. Only `site/` is
published: the raw data, the scripts and this README stay in the repository.

## What is on each page

| Page | Plan item |
|---|---|
| Wet and dry days | Pick months and decade: transition matrix with 95% intervals, long-run wet share, typical spells, H1, H2, H3 (1,000 simulated records against the real spells) and one simulated season |
| Decades | Rain per season, H4 (homogeneity of transition counts) and H5 (heavy-rain share and ANOVA) |
| Extremes | Lognormal, gamma and Weibull fits by AIC, upper-tail QQ check, return periods with season-bootstrap intervals |
| Decision | Cost-loss flood alert rule by state, and reservoir buffer days for a chosen dry-spell risk |
| Data | The audit: rules fixed before modelling, every day of every season, and what the checks found |

## Folders

```
app/                 the Shiny app
  app.R              layout and server
  R/                 analysis functions, loaded automatically by Shiny
    data.R           loading data, choosing months and decades
    markov.R         transition counts, H1, H2, H4, spells, simulation, H3
    extremes.R       truncated fits, return periods, bootstrap, H5
    decision.R       cost-loss alert rule, longest dry spell, buffer days
    plots.R          chart style and charts
  data/              clean data written by prepare/prepare_data.R
  www/               styles, favicon and the DM Sans font (SIL Open Font License)
prepare/
  download_data.R    downloads the three raw files into data/
  prepare_data.R     builds app/data from the raw downloads (run once)
  precompute.R       runs the slow simulations into app/data/precomputed.rds
deploy/
  build_site.R       exports the app with Shinylive into site/
site/                the static website Vercel publishes (built, do not edit)
vercel.json          Vercel settings: no build, publish site/
analysis/
  results.R          prints every result for the written report
  results.txt        its output
data/                raw downloads, kept exactly as downloaded (not in git)
```

## Data and audit

Raw files (`Rscript prepare/download_data.R` fetches all three):

- OpenCity dataset page: https://data.opencity.in/dataset/chennai-rainfall-data
- "Chennai Daily Rainfall Data from 1991-2023":
  https://data.opencity.in/dataset/256d1a20-adf5-4e3a-ae18-27664339117a/resource/3086f865-a04c-431e-815d-105ae658871f/download/e5c275eb-a4f2-4412-9677-73654e8f5f4d.csv
- "Tamil Nadu Daily Rainfall from 1991 to 2023" (same dataset):
  https://data.opencity.in/dataset/256d1a20-adf5-4e3a-ae18-27664339117a/resource/fd176db8-caca-42e5-b5a0-9dc78a3d73a9/download/4804c971-4f38-40b5-b203-aba08e19b937.csv
- ERA5 hourly rain at Nungambakkam from Open-Meteo, for the cross-check:
  https://archive-api.open-meteo.com/v1/archive?latitude=13.0674&longitude=80.2376&start_date=1990-12-31&end_date=2023-12-31&hourly=precipitation&timezone=GMT&models=era5

Save them as `data/raw/chennai_daily_1991_2023.csv`,
`data/raw-tn/tamilnadu_daily_1991_2023.csv` and
`data/raw-openmeteo/era5_hourly_nungambakkam_1990_2023.json`, then run
`Rscript prepare/prepare_data.R` from this folder.

What the audit found:

- The Chennai file starts on 3 Mar 1993 despite its title. The Tamil Nadu file in the
  same dataset has Nungambakkam from 3 Jun 1991 and matches the Chennai file on all
  2,709 shared days, so the station series is taken from it.
- The files list rainy days only (no zeros in 650,617 rows). A day not listed is read
  as dry (0 mm), except where the checks below say it is missing.
- 1,500 exact duplicate rows in the Chennai district are removed. Three days have two
  different values for Nungambakkam and are set to missing.
- Dec 1992, Dec 1995 and Oct 1996 have no listing at any Chennai gauge, so those months
  are missing. ERA5 shows 17 wet days in Oct 1996, so that one is clearly a gap.
- 19 days are not listed at Nungambakkam while at least two other Chennai gauges had
  10 mm or more (or one had 25 mm and ERA5 had 10 mm). They are set to missing.
- No sign of filled-in values at Nungambakkam (no amount repeats for more than 2 days).
  Sholinganallur repeats Nungambakkam's value on 49% of shared days, so it is not an
  independent gauge.
- Against ERA5, with rain days running 08:30 to 08:30 IST as IMD reports them, wet and
  dry days agree on 82.5% of days, and the dates line up (correlation 0.68 on the same
  day against 0.35 and 0.45 a day off).

Rules fixed before modelling: station Nungambakkam; season 1 October to 31 December;
wet day 2.5 mm or more; no interpolation, so pairs of days that include a missing day
are left out; a season with more than 9 missing days is not used (1992, 1995 and 1996),
which leaves 30 seasons: 8 in 1991 to 2001 and 11 in each later decade.

## Method notes

- Transition probabilities carry Wilson 95% intervals. H1 and H2 are Pearson
  chi-square tests; H2 compares the day before yesterday only within the same
  yesterday state (2 degrees of freedom).
- H3 simulates 1,000 records, each with as many seasons as the selection, from the
  fitted first-order chain (first day drawn from the steady state). The 90% range for
  each spell length comes from those records, and the Monte Carlo p-value from a
  chi-square distance on binned spell lengths.
- Wet-day amounts are fitted by maximum likelihood with the 2.5 mm cut-off built in
  (left-truncated fits). Return periods use T(x) = 1 / (lambda [1 - F(x)]), with
  lambda the mean number of wet days per season, and 90% intervals from 400 resamples
  of whole seasons. Beyond about 30 years they are extrapolations.
- The flood alert uses P(wet tomorrow | last two days) times P(amount above the
  full-record 95th percentile | wet). C and L are assumptions; the app shows which
  states trigger an alert at any C/L.

## Rebuilding everything from scratch

```
Rscript prepare/download_data.R
Rscript prepare/prepare_data.R
Rscript prepare/precompute.R
Rscript deploy/build_site.R
```

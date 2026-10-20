# Loading the prepared data and choosing the days a view asks for.
# Shiny sources every file in R/ automatically, before app.R runs.

WET_MM <- 2.5   # IMD wet-day threshold: a day is wet with 2.5 mm or more

DECADES <- c("1991 to 2001", "2002 to 2012", "2013 to 2023")

MONTHS <- list(
  "Oct to Dec" = 10:12,
  "October"    = 10,
  "November"   = 11,
  "December"   = 12
)

decade_of <- function(season) {
  cut(season, breaks = c(1990, 2001, 2012, 2023), labels = DECADES)
}

# One row per October to December day. rain_mm is NA on missing days.
load_days <- function(dir = "data") {
  d <- read.csv(file.path(dir, "nungambakkam_ond.csv"), stringsAsFactors = FALSE)
  s <- read.csv(file.path(dir, "audit_seasons.csv"))
  d$date   <- as.Date(d$date)
  d        <- d[order(d$date), ]
  d$used   <- d$season %in% s$season[s$used]
  d$wet    <- d$rain_mm >= WET_MM
  d$decade <- decade_of(d$season)
  # Yesterday and the day before, inside the same season (NA on 1 and 2 October)
  lag_in_season <- function(x, k) ave(x, d$season, FUN = function(v) c(rep(NA, k), head(v, -k)))
  d$prev  <- lag_in_season(d$wet, 1)
  d$prev2 <- lag_in_season(d$wet, 2)
  d
}

load_audit <- function(dir = "data") {
  facts <- read.csv(file.path(dir, "audit_facts.csv"), stringsAsFactors = FALSE)
  list(
    seasons = read.csv(file.path(dir, "audit_seasons.csv")),
    facts   = setNames(as.list(facts$value), facts$key)
  )
}

# Name under which a month and decade choice is stored in precomputed.rds
spell_key <- function(months, decade) paste(months, decade, sep = " / ")

# Results of the slow simulations, made by prepare/precompute.R
load_precomputed <- function(dir = "data") readRDS(file.path(dir, "precomputed.rds"))

# Days of the used seasons, for some months and one decade (or all years)
select_days <- function(d, months = 10:12, decade = "All years") {
  keep <- d$used & d$month %in% months
  if (decade != "All years") keep <- keep & d$decade == decade
  d[keep, ]
}

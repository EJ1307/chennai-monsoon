# Builds the clean Nungambakkam October to December series that the app reads.
#
# Run once from the project folder:
#   Rscript prepare/prepare_data.R
#
# Inputs (downloaded as is, never edited):
#   data/raw-tn/tamilnadu_daily_1991_2023.csv   OpenCity "Tamil Nadu Daily Rainfall from 1991 to 2023"
#   data/raw/chennai_daily_1991_2023.csv        OpenCity "Chennai Daily Rainfall Data from 1991-2023"
#   data/raw-openmeteo/era5_hourly_nungambakkam_1990_2023.json   Open-Meteo ERA5, hourly, for the cross-check
#
# Outputs (app/data):
#   nungambakkam_ond.csv   one row per October to December day, 1991 to 2023
#   audit_seasons.csv      one row per monsoon season
#   audit_facts.csv        the numbers quoted on the Data page
#
# Rules, fixed before any modelling:
#   1. Station: Chennai Nungambakkam, the IMD city observatory and the only station
#      in the file that covers every year from 1991 to 2023.
#   2. The file lists rainy days only (no zeros anywhere). A day that is not listed
#      is read as dry, except in the three cases below, which are set to missing.
#   3. Missing: a month with no listing at Nungambakkam (the whole city network was
#      silent in each case, so a gap cannot be told apart from a dry month).
#   4. Missing: a day not listed at Nungambakkam while at least two other Chennai
#      gauges had 10 mm or more, or one had 25 mm or more and ERA5 had 10 mm or more.
#   5. Missing: a day with two different values for Nungambakkam.
#   6. No interpolation. A season with more than 9 missing days (10 percent) is not used.

tn_file   <- "data/raw-tn/tamilnadu_daily_1991_2023.csv"
city_file <- "data/raw/chennai_daily_1991_2023.csv"
era5_file <- "data/raw-openmeteo/era5_hourly_nungambakkam_1990_2023.json"
out_dir   <- "app/data"

WET_MM      <- 2.5   # IMD wet-day threshold
MAX_MISSING <- 9     # seasons with more missing days than this are not used
STATION     <- "Chennai nungambakkam"

dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
facts <- list()
fact <- function(key, value) facts[[key]] <<- value

# 1. Read the Tamil Nadu file and keep Chennai district --------------------------
tn <- read.csv(tn_file, stringsAsFactors = FALSE)
tn$date <- as.Date(tn$date)
ch <- tn[tn$dist == "Chennai", c("station", "value", "date")]

fact("tn_rows_chennai", nrow(ch))
fact("tn_exact_duplicates", sum(duplicated(ch)))
fact("zeros_in_file", sum(tn$value == 0))
fact("negative_values", sum(tn$value < 0))
fact("stations_chennai", length(unique(ch$station)))

ch <- unique(ch)

# 2. The Nungambakkam series -----------------------------------------------------
nu <- ch[ch$station == STATION, c("date", "value")]
conflict_dates <- unique(nu$date[duplicated(nu$date)])
fact("first_listed_day", format(min(nu$date)))
fact("conflict_days", length(conflict_dates))

# The Chennai-only file in the same dataset starts later than its title says.
# Check that it agrees with the Tamil Nadu file where both have data.
city <- read.csv(city_file, stringsAsFactors = FALSE)
city$Date <- as.Date(city$Date, format = "%d-%m-%Y")
fact("city_file_first_day", format(min(city$Date)))
city_nu <- unique(city[city$Station == STATION, c("Date", "Rainfall")])
both <- merge(nu[!nu$date %in% conflict_dates, ], city_nu, by.x = "date", by.y = "Date")
fact("city_file_days_compared", nrow(both))
fact("city_file_days_equal", sum(abs(both$value - both$Rainfall) < 1e-9))

# Other gauges that repeat Nungambakkam's value on the same day suggest copied
# (filled-in) records, so they cannot serve as independent checks.
others <- ch[ch$station != STATION, ]
copy <- merge(others, nu, by = "date", suffixes = c("", "_nu"))
copy_share <- tapply(abs(copy$value - copy$value_nu) < 1e-9, copy$station, mean)
copy_n     <- tapply(copy$value, copy$station, length)
top_copy <- names(which.max(copy_share[copy_n >= 100]))
fact("most_copied_station", top_copy)
fact("most_copied_share", round(copy_share[[top_copy]], 2))

# Long runs of the same non-zero amount on consecutive days would hint at fill-in.
nu_sorted <- nu[order(nu$date), ]
same_next <- diff(nu_sorted$date) == 1 & diff(nu_sorted$value) == 0
runs <- rle(same_next)
fact("repeat_pairs", sum(same_next))
fact("longest_repeat_run_days", if (any(runs$values)) max(runs$lengths[runs$values]) + 1 else 1)

# 3. ERA5 in the IMD rain day (08:30 IST to 08:30 IST, labelled by its end) -------
era5 <- jsonlite::fromJSON(era5_file)
hourly <- data.frame(
  time = as.POSIXct(era5$hourly$time, format = "%Y-%m-%dT%H:%M", tz = "UTC"),
  mm   = era5$hourly$precipitation
)
# An hourly value covers the hour that ends at its time stamp. 08:30 IST is 03:00 UTC,
# so rain day D collects the hours ending 04:00 UTC on D-1 up to 03:00 UTC on D.
hourly$imd_day <- as.Date(hourly$time - 3 * 3600 - 1, tz = "UTC") + 1
era5_day <- aggregate(mm ~ imd_day, hourly, sum)
names(era5_day) <- c("date", "era5_mm")

# 4. One row per October to December day ------------------------------------------
days <- data.frame(date = seq(as.Date("1991-10-01"), as.Date("2023-12-31"), by = "day"))
days$month <- as.integer(format(days$date, "%m"))
days <- days[days$month >= 10, ]
days$season <- as.integer(format(days$date, "%Y"))
days$dos <- as.integer(days$date - as.Date(paste0(days$season, "-10-01"))) + 1

listed <- nu[!nu$date %in% conflict_dates, ]
days <- merge(days, listed, by = "date", all.x = TRUE)
days <- merge(days, era5_day, by = "date", all.x = TRUE)
days$era5_mm <- round(days$era5_mm, 1)
days$status <- ifelse(is.na(days$value), "dry, not listed", "listed")
days$rain_mm <- ifelse(is.na(days$value), 0, days$value)

# Rule 3: months with no listing at all
ym <- format(days$date, "%Y-%m")
listed_in_month <- tapply(days$status == "listed", ym, sum)
silent <- names(listed_in_month)[listed_in_month == 0]
days$status[ym %in% silent] <- "missing, month not reported"

# Rule 4: neighbours wet while Nungambakkam is silent
oth <- aggregate(value ~ station + date, others, max)
n10 <- tapply(oth$value >= 10, oth$date, sum)
mx  <- tapply(oth$value, oth$date, max)
key <- format(days$date)
days$others_10mm <- as.integer(ifelse(is.na(n10[key]), 0, n10[key]))
days$others_max  <- ifelse(is.na(mx[key]), 0, mx[key])
suspect <- days$status == "dry, not listed" &
  (days$others_10mm >= 2 | (days$others_max >= 25 & !is.na(days$era5_mm) & days$era5_mm >= 10))
days$status[suspect] <- "missing, neighbours wet"

# Rule 5: conflicting values
days$status[days$date %in% conflict_dates] <- "missing, two values"

days$rain_mm[startsWith(days$status, "missing")] <- NA
days <- days[order(days$date), ]

# 5. Season audit ---------------------------------------------------------------
seasons <- do.call(rbind, lapply(split(days, days$season), function(s) {
  ok <- !is.na(s$rain_mm)
  data.frame(
    season         = s$season[1],
    listed_days    = sum(s$status == "listed"),
    wet_days       = sum(s$rain_mm >= WET_MM, na.rm = TRUE),
    missing_days   = sum(!ok),
    total_mm       = round(sum(s$rain_mm, na.rm = TRUE)),
    era5_total_mm  = round(sum(s$era5_mm[ok])),
    era5_wet_days  = sum(s$era5_mm[ok] >= WET_MM),
    agree_share    = round(mean((s$rain_mm[ok] >= WET_MM) == (s$era5_mm[ok] >= WET_MM)), 3),
    used           = sum(!ok) <= MAX_MISSING
  )
}))

used_days <- days[days$season %in% seasons$season[seasons$used] & !is.na(days$rain_mm), ]
fact("seasons_total", nrow(seasons))
fact("seasons_used", sum(seasons$used))
fact("seasons_dropped", paste(seasons$season[!seasons$used], collapse = ", "))
fact("missing_months", paste(format(as.Date(paste0(silent[silent >= "1991-10"], "-01")), "%b %Y"), collapse = ", "))
fact("missing_days_neighbours", sum(days$status == "missing, neighbours wet"))
fact("missing_days_two_values", sum(days$status == "missing, two values"))
fact("max_ond_mm", max(days$rain_mm, na.rm = TRUE))
fact("max_ond_date", format(days$date[which.max(days$rain_mm)]))
fact("era5_agreement", round(mean((used_days$rain_mm >= WET_MM) == (used_days$era5_mm >= WET_MM)), 3))
fact("era5_wet_share", round(mean(used_days$era5_mm >= WET_MM), 3))
fact("station_wet_share", round(mean(used_days$rain_mm >= WET_MM), 3))

# Date check: rain recorded for day D should line up best with ERA5 for the same D.
lagged_cor <- function(k) {
  x <- used_days$rain_mm
  y <- era5_day$era5_mm[match(used_days$date + k, era5_day$date)]
  round(cor(x, y, use = "complete.obs"), 2)
}
fact("era5_cor_day_before", lagged_cor(-1))
fact("era5_cor_same_day", lagged_cor(0))
fact("era5_cor_day_after", lagged_cor(1))

# 6. Write ----------------------------------------------------------------------
out <- days[, c("date", "season", "month", "dos", "rain_mm", "status", "era5_mm")]
write.csv(out, file.path(out_dir, "nungambakkam_ond.csv"), row.names = FALSE)
write.csv(seasons, file.path(out_dir, "audit_seasons.csv"), row.names = FALSE)
write.csv(data.frame(key = names(facts), value = unlist(lapply(facts, as.character))),
          file.path(out_dir, "audit_facts.csv"), row.names = FALSE)

cat("Wrote", nrow(out), "days and", nrow(seasons), "seasons to", out_dir, "\n")
str(facts)

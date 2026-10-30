# Writes every result the website shows into web/data/, so R does all the statistics
# here, once, and the website only reads and draws them.
#
# Run from the project folder, after prepare_data.R and precompute.R:
#   Rscript prepare/export_web.R
#
# Output:
#   web/data/results.json   all results: every month and decade choice, decades, extremes,
#                           decision and audit
#   web/data/days.json      the daily series, used by the calendar and by the browser's own
#                           calculations for anything new

for (f in list.files("app/R", full.names = TRUE)) source(f)

days  <- load_days("app/data")
audit <- load_audit("app/data")
pre   <- load_precomputed("app/data")
ond   <- select_days(days, 10:12, "All years")
record_years <- sum(!is.na(ond$wet)) / 92

dir.create("web/data", recursive = TRUE, showWarnings = FALSE)

spell_state <- function(r) {
  list(curve = r$curve, n_spells = r$n_spells, x2 = r$x2, p_mc = r$p_mc,
       long = r$long, long_obs = r$long_obs, long_n = r$long_n, long_sim = r$long_sim,
       long_band = unname(r$long_band), longest_obs = r$longest_obs,
       worst_bin = r$worst_bin, worst_obs = r$worst_obs, worst_exp = r$worst_exp)
}

# 1. Every month and decade choice ----------------------------------------------------
selections <- list()
for (m in names(MONTHS)) for (dc in c("All years", DECADES)) {
  sel <- select_days(days, MONTHS[[m]], dc)
  ch  <- fit_chain(sel)
  h1  <- test_h1(ch)
  h2  <- test_h2(sel)
  sp  <- pre$spells[[spell_key(m, dc)]]
  selections[[spell_key(m, dc)]] <- list(
    months = m, decade = dc,
    seasons = sort(unique(sel$season)),
    dos = sort(unique(sel$dos)),
    counts = list(dd = ch$counts["Dry", "Dry"], dw = ch$counts["Dry", "Wet"],
                  wd = ch$counts["Wet", "Dry"], ww = ch$counts["Wet", "Wet"]),
    n_pairs = ch$n_pairs,
    p01 = unname(ch$p01), p11 = unname(ch$p11),
    pi_wet = ch$pi_wet, mean_wet_spell = ch$mean_wet_spell, mean_dry_spell = ch$mean_dry_spell,
    h1 = h1,
    h2 = list(stat = h2$stat, df = h2$df, p = h2$p, min_expected = h2$min_expected,
              p_wet = as.list(h2$p_wet), n = as.list(h2$n)),
    h3 = list(n_sets = sp$n_sets, n_seasons = sp$n_seasons, n_days = sp$n_days,
              wet = spell_state(sp$wet), dry = spell_state(sp$dry))
  )
}

# 2. Decades (H4, H5) and rain per season -----------------------------------------------
h4 <- test_h4(ond)
h5 <- test_h5(ond)

# 3. Extremes -------------------------------------------------------------------------
fits <- pre$fits
natural <- function(f) {
  th <- f$theta
  switch(f$dist,
         lognormal = list(meanlog = th[1], sdlog = exp(th[2])),
         gamma     = list(shape = exp(th[1]), scale = exp(th[2])),
         weibull   = list(shape = exp(th[1]), scale = exp(th[2])))
}
qq_upper <- fits$qq[fits$qq$p > 0.5, ]
qq <- list(observed = qq_upper$observed[qq_upper$dist == fits$best_aic],
           p = qq_upper$p[qq_upper$dist == fits$best_aic])
for (dn in names(DIST_NAMES)) qq[[dn]] <- qq_upper$fitted[qq_upper$dist == dn]

wet <- ond[!is.na(ond$wet) & ond$wet, ]
top <- wet[order(-wet$rain_mm), ][1:25, ]
top_days <- data.frame(date = format(top$date), rain_mm = top$rain_mm,
                       T = record_years / seq_len(25))

# 4. Decision -----------------------------------------------------------------------------
chain_all <- fit_chain(ond)
h2_all    <- test_h2(ond)
best_fit  <- fits$fits[[fits$best_aic]]
alert_tab <- alert_states(ond, chain_all, h2_all, best_fit, h5$q95, h2_all$p < 0.05)
# the plain chain fails H3 for dry spells, so buffer days also come from the seasonal chain
dry_max   <- pre$dry_max$curve
dry_max$seasonal <- pre$outlook$dry_exceed$p[match(dry_max$k, pre$outlook$dry_exceed$k)]

# 5. Predict ---------------------------------------------------------------------------------
models    <- choose_models(model_rows(days))
forecasts <- forecast_table(days, models)

# 6. Audit ----------------------------------------------------------------------------------
missing_by_decade <- tapply(is.na(days$rain_mm), days$decade, sum)

results <- list(
  meta = list(
    made = format(Sys.time(), "%Y-%m-%d %H:%M"),
    wet_mm = WET_MM, months = names(MONTHS), decades = DECADES,
    n_used = length(unique(ond$season)), record_years = record_years
  ),
  selections = selections,
  decades = list(
    seasons = audit$seasons,
    h4 = list(stat = h4$stat, df = h4$df, p = h4$p, p_from_dry = h4$from_dry$p,
              p_from_wet = h4$from_wet$p, per_decade = h4$per_decade),
    h5 = list(q95 = h5$q95, share = h5$share, stat = h5$stat, df = h5$df, p = h5$p,
              anova_f = h5$anova_f, anova_df = h5$anova_df, anova_p = h5$anova_p)
  ),
  extremes = list(
    table = fits$table[, c("dist", "name", "params", "loglik", "aic", "delta_aic", "tail_error")],
    best_aic = fits$best_aic, best_tail = fits$best_tail, n = fits$n,
    params = lapply(fits$fits, natural), cut = WET_MM,
    qq = qq,
    returns = pre$returns,
    return_t = RETURN_T,
    top_days = top_days,
    lambda = wet_rate(ond)
  ),
  decision = list(
    states = alert_tab[, c("state", "after", "p_wet", "p_heavy", "observed", "n")],
    p_heavy_if_wet = alert_tab$p_heavy_if_wet[1],
    heavy_mm = h5$q95,
    second_order = h2_all$p < 0.05,
    dry_max = dry_max,
    dry_fit = pre$spells[[spell_key("Oct to Dec", "All years")]]$dry$p_mc >= 0.05,
    dry_in_range = pre$outlook$table$obs_in_range[pre$outlook$table$key == "dry"]
  ),
  predict = list(
    forecasts = forecasts, by_day = observed_by_day(days),
    k = models$k, best_chain = models$best_chain, n_rows = nrow(model_rows(days)),
    models = models$table[, c("key", "name", "rhs", "df", "loglik", "aic", "delta_aic", "lrt_p")],
    trend_season = models$trend_season, trend_chain = models$trend_chain,
    peak_dos = forecasts$dos[forecasts$history == "any"][which.max(forecasts$p[forecasts$history == "any"])],
    heavy_mm = h5$q95, p_heavy_if_wet = wet_exceed(best_fit, h5$q95),
    histories = as.list(HISTORY_LABELS), after = as.list(HISTORY_AFTER),
    outlook = pre$outlook$table, n_sim = pre$outlook$n_sim,
    checks = pre$checks
  ),
  audit = list(
    facts = audit$facts,
    missing_by_decade = as.list(missing_by_decade)
  )
)

jsonlite::write_json(results, "web/data/results.json", auto_unbox = TRUE,
                     digits = I(6), dataframe = "rows", na = "null", null = "null")

# The daily series: one entry per October to December day (rain null when missing)
status_code <- c("dry, not listed" = 0L, "listed" = 0L, "missing, month not reported" = 1L,
                 "missing, neighbours wet" = 2L, "missing, two values" = 3L)
d <- days[order(days$date), ]
daily <- list(
  first_season = min(d$season), last_season = max(d$season),
  used_seasons = sort(unique(d$season[d$used])),
  status_labels = c("", "month not reported", "neighbours wet", "two values"),
  season = d$season, month = d$month, dos = d$dos,
  rain = d$rain_mm,
  status = unname(status_code[d$status])
)
jsonlite::write_json(daily, "web/data/days.json", auto_unbox = TRUE, digits = I(6), na = "null")

# The clean daily data, for the Download button on the Data page
invisible(file.copy("app/data/nungambakkam_ond.csv", "web/data/nungambakkam_oct_dec_1991_2023.csv",
          overwrite = TRUE))

for (f in c("web/data/results.json", "web/data/days.json")) {
  cat(sprintf("%-26s %6.1f KB\n", f, file.size(f) / 1024))
}

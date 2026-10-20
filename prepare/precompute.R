# Runs the slow simulations once and stores the results in app/data/precomputed.rds,
# so the app opens quickly, also when it runs inside a web browser (Shinylive).
#
# Run from the project folder after prepare_data.R, and again whenever the data or
# the analysis functions in app/R change:
#   Rscript prepare/precompute.R

for (f in list.files("app/R", full.names = TRUE)) source(f)

days <- load_days("app/data")
ond  <- select_days(days, 10:12, "All years")

t0 <- Sys.time()

# H3 for every month and decade the app offers: 1,000 simulated records each
spells <- list()
for (m in names(MONTHS)) for (dc in c("All years", DECADES)) {
  sel <- select_days(days, MONTHS[[m]], dc)
  spells[[spell_key(m, dc)]] <- compare_spells(sel, fit_chain(sel))
}

fits <- compare_fits(ond)

# Return levels with season-bootstrap intervals, for each distribution
returns <- lapply(setNames(names(DIST_NAMES), names(DIST_NAMES)),
                  function(dn) bootstrap_returns(ond, dn, T = T_GRID))

# Longest dry spell per season, record against 20,000 simulated seasons
dry_max <- longest_dry(ond, fit_chain(ond))

saveRDS(list(spells = spells, fits = fits, returns = returns, dry_max = dry_max,
             made = Sys.time()),
        "app/data/precomputed.rds")
cat("Saved app/data/precomputed.rds in", format(round(Sys.time() - t0, 1)), "\n")

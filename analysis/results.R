# Prints every result the app shows, for the written report.
# Run from the project folder:  Rscript analysis/results.R

for (f in list.files("app/R", full.names = TRUE)) source(f)

days <- load_days("app/data")
ond  <- select_days(days, 10:12, "All years")
fmt  <- function(x, d = 3) formatC(x, format = "f", digits = d)
pval <- function(p) if (p < 0.001) "< 0.001" else fmt(p, 3)
line <- function(...) cat(..., "\n", sep = "")

line("Seasons used: ", length(unique(ond$season)), " (", sum(!is.na(ond$wet)), " days with data)")

line("\n== Chain by month and decade (wet = 2.5 mm or more) ==")
for (m in names(MONTHS)) for (dc in c("All years", DECADES)) {
  ch <- fit_chain(select_days(days, MONTHS[[m]], dc))
  line(sprintf("%-10s %-13s p(D>W) %s [%s, %s]  p(W>W) %s [%s, %s]  wet share %s  spells wet %s dry %s",
               m, dc, fmt(ch$p01[1]), fmt(ch$p01[2]), fmt(ch$p01[3]),
               fmt(ch$p11[1]), fmt(ch$p11[2]), fmt(ch$p11[3]),
               fmt(ch$pi_wet), fmt(ch$mean_wet_spell, 1), fmt(ch$mean_dry_spell, 1)))
}

chain <- fit_chain(ond)
h1 <- test_h1(chain)
line("\n== H1 independence ==  X2 = ", fmt(h1$stat, 1), ", df 1, p ", pval(h1$p),
     ", p(W>W) - p(D>W) = ", fmt(h1$diff))
h2 <- test_h2(ond)
line("== H2 second order ==  X2 = ", fmt(h2$stat, 2), ", df 2, p ", pval(h2$p),
     ", min expected ", fmt(h2$min_expected, 1))
print(round(cbind(n = h2$n, p_wet = h2$p_wet), 3))

t0 <- Sys.time()
h3 <- compare_spells(ond, chain)
line("== H3 spells (", h3$n_sets, " simulated records of ", h3$n_seasons, " seasons, ",
     format(round(Sys.time() - t0, 1)), ") ==")
for (s in c("wet", "dry")) {
  r <- h3[[s]]
  line(sprintf("%s: %d spells, X2 %.1f, Monte Carlo p %s; %d+ days observed %s (n %d), chain %s [%s, %s]; longest %d",
               s, r$n_spells, r$x2, pval(r$p_mc), r$long, fmt(r$long_obs), r$long_n,
               fmt(r$long_sim), fmt(r$long_band[1]), fmt(r$long_band[2]), r$longest_obs))
}

h4 <- test_h4(ond)
line("\n== H4 decades ==  X2 = ", fmt(h4$stat, 2), ", df ", h4$df, ", p ", pval(h4$p),
     "  (from dry p ", pval(h4$from_dry$p), ", from wet p ", pval(h4$from_wet$p), ")")
print(format(h4$per_decade, digits = 3))

h5 <- test_h5(ond)
line("\n== H5 heavy days ==  95th percentile ", fmt(h5$q95, 1), " mm; X2 = ", fmt(h5$stat, 2),
     ", df ", h5$df, ", p ", pval(h5$p), "; ANOVA F = ", fmt(h5$anova_f, 2), ", p ", pval(h5$anova_p))
print(format(h5$share, digits = 3))

t0 <- Sys.time()
fits <- compare_fits(ond)
line("\n== Amount fits (", fits$n, " wet days) ==  best AIC: ", fits$best_aic, ", best tail: ", fits$best_tail)
print(format(fits$table, digits = 4))
lambda <- wet_rate(ond)
line("lambda (wet days per season) = ", fmt(lambda, 1))
for (dn in names(DIST_NAMES)) {
  rl <- bootstrap_returns(ond, dn)
  line(DIST_NAMES[[dn]], " return levels (mm, 90% interval):")
  print(format(rl, digits = 4))
}
line("(fits and bootstrap took ", format(round(Sys.time() - t0, 1)), ")")
obs_top <- sort(wet_amounts(ond), decreasing = TRUE)[1:5]
line("largest observed wet days: ", paste(obs_top, collapse = ", "))

line("\n== Decision ==")
best <- fits$fits[[fits$best_aic]]
st <- alert_states(ond, chain, h2, best, h5$q95, second_order = h2$p < 0.05)
print(format(st, digits = 3))
ld <- longest_dry(ond, chain)
for (risk in c(0.05, 0.1, 0.2)) {
  line(sprintf("buffer at risk %.2f: chain %d days, observed %d days", risk,
               buffer_days(ld$curve, "chain", risk), buffer_days(ld$curve, "observed", risk)))
}

# Forecasts for any October to December day.
#
# Two logistic regressions on the daily record:
#   - the seasonal model gives the chance that a given date is wet, knowing nothing
#     else (what can be said today about a day in 2027);
#   - the seasonal chain adds the last two days, a Markov chain whose transition
#     chances rise and fall with the season (Stern and Coe 1984).
# Both are chosen by AIC and checked by leaving out one season at a time.

HISTORIES <- c("DD", "WD", "DW", "WW")   # day before yesterday, then yesterday
HISTORY_LABELS <- c(DD = "Two dry days", WD = "Wet, then dry",
                    DW = "Dry, then wet", WW = "Two wet days")
HISTORY_AFTER <- c(DD = "two dry days", WD = "a wet day and then a dry one",
                   DW = "a dry day and then a wet one", WW = "two wet days")
MONTH_DAYS <- c(Oct = 31, Nov = 30, Dec = 31)

# Day of the season to a date in a given year (1 = 1 October)
dos_date <- function(dos, year) as.Date(sprintf("%d-10-01", year)) + dos - 1

# Harmonics of the day of the year: the slow rise and fall of the monsoon.
# 1 October is day 274, so dos + 273 is the day of the year.
add_season_terms <- function(d) {
  w <- 2 * pi * (d$dos + 273) / 365.25
  for (j in 1:3) {
    d[[paste0("s", j)]] <- sin(j * w)
    d[[paste0("c", j)]] <- cos(j * w)
  }
  d
}
season_rhs <- function(k) {
  if (k == 0) return("1")
  paste(paste0(c("s", "c"), rep(seq_len(k), each = 2)), collapse = " + ")
}

# Rows for model fitting: used seasons, with today and the last two days known
model_rows <- function(days) {
  d <- add_season_terms(days[days$used, ])
  d <- d[!is.na(d$wet) & !is.na(d$prev) & !is.na(d$prev2), ]
  d$yest  <- as.integer(d$prev)
  d$yest2 <- as.integer(d$prev2)
  d$year  <- d$season - 2007   # centred, for the trend check
  d
}

# The candidate models, each adding one thing to the one before. All are fitted to
# the same rows so their AIC values compare.
CANDIDATES <- function(k) {
  s <- season_rhs(k)
  list(
    list(key = "flat",     name = "Same chance every day",       rhs = "1"),
    list(key = "season",   name = "Season only",                  rhs = s),
    list(key = "chain1",   name = "Season and yesterday",         rhs = paste(s, "+ yest")),
    list(key = "chain2",   name = "Season and the last two days", rhs = paste(s, "+ yest * yest2")),
    list(key = "chain2s",  name = "Memory that changes with the season",
         rhs = paste(s, "+ yest * yest2 + yest:(s1 + c1)"))
  )
}

fit_glm <- function(rhs, rows) {
  suppressWarnings(glm(as.formula(paste("wet ~", rhs)), family = binomial, data = rows))
}

# Pick the number of harmonics for the season, then compare the candidates
choose_models <- function(rows) {
  harm <- do.call(rbind, lapply(1:3, function(k) {
    m <- fit_glm(season_rhs(k), rows)
    data.frame(k = k, aic = AIC(m))
  }))
  k <- harm$k[which.min(harm$aic)]
  cand <- CANDIDATES(k)
  fits <- lapply(cand, function(c) fit_glm(c$rhs, rows))
  names(fits) <- vapply(cand, `[[`, "", "key")
  table <- data.frame(
    key  = names(fits),
    name = vapply(cand, `[[`, "", "name"),
    rhs  = vapply(cand, `[[`, "", "rhs"),
    df   = vapply(fits, function(m) length(coef(m)), numeric(1)),
    loglik = vapply(fits, function(m) as.numeric(logLik(m)), numeric(1)),
    aic  = vapply(fits, AIC, numeric(1))
  )
  table$delta_aic <- table$aic - min(table$aic)
  # likelihood ratio test of each model against the one above it
  table$lrt_p <- c(NA, vapply(2:length(fits), function(i) {
    anova(fits[[i - 1]], fits[[i]], test = "LRT")[2, "Pr(>Chi)"]
  }, numeric(1)))
  chain_keys <- c("chain1", "chain2", "chain2s")
  best_chain <- chain_keys[which.min(table$aic[match(chain_keys, table$key)])]
  # Is there a trend over the years? Added to the season model and to the chosen chain.
  trend <- function(key) {
    base <- fits[[key]]
    with_year <- fit_glm(paste(table$rhs[table$key == key], "+ year"), rows)
    list(slope = unname(coef(with_year)["year"]),
         p = anova(base, with_year, test = "LRT")[2, "Pr(>Chi)"])
  }
  list(k = k, harmonics = harm, table = table, fits = fits,
       best_chain = best_chain, rhs_season = table$rhs[table$key == "season"],
       rhs_chain = table$rhs[table$key == best_chain],
       trend_season = trend("season"), trend_chain = trend(best_chain))
}

# Chance of a wet day for every day of the season: knowing nothing ("any") and
# for each history of the last two days. 95% intervals from the model's standard
# errors on the logit scale.
forecast_table <- function(days, models) {
  # the season model is refitted on every day with data, not only the chain's rows
  all_rows <- add_season_terms(days[days$used & !is.na(days$wet), ])
  season_fit <- fit_glm(models$rhs_season, all_rows)
  chain_fit  <- models$fits[[models$best_chain]]
  grid <- add_season_terms(data.frame(dos = 1:92))
  band <- function(m, newdata) {
    pr <- predict(m, newdata, type = "link", se.fit = TRUE)
    data.frame(p = plogis(pr$fit), lo = plogis(pr$fit - 1.96 * pr$se.fit),
               hi = plogis(pr$fit + 1.96 * pr$se.fit))
  }
  out <- list(cbind(dos = 1:92, history = "any", band(season_fit, grid)))
  for (h in HISTORIES) {
    g <- grid
    g$yest2 <- as.integer(substr(h, 1, 1) == "W")
    g$yest  <- as.integer(substr(h, 2, 2) == "W")
    out[[h]] <- cbind(dos = 1:92, history = h, band(chain_fit, g))
  }
  res <- do.call(rbind, out)
  rownames(res) <- NULL
  res
}

# What the record itself says about each day of the season: share of the used
# seasons that were wet on that date, and the wet-day amounts within ten days of it
observed_by_day <- function(days, window = 10) {
  d <- days[days$used, ]
  do.call(rbind, lapply(1:92, function(t) {
    on   <- d[d$dos == t & !is.na(d$wet), ]
    near <- d[abs(d$dos - t) <= window & !is.na(d$wet) & d$wet, ]
    data.frame(dos = t, n = nrow(on), wet = sum(on$wet),
               share = if (nrow(on)) mean(on$wet) else NA_real_,
               near_wet = nrow(near),
               near_median = median(near$rain_mm),
               near_q90 = unname(quantile(near$rain_mm, 0.9)))
  }))
}

# Chance of wet today for every day and every history, from the chosen chain
chain_matrix <- function(models) {
  m <- models$fits[[models$best_chain]]
  grid <- add_season_terms(data.frame(dos = 1:92))
  sapply(HISTORIES, function(h) {
    g <- grid
    g$yest2 <- as.integer(substr(h, 1, 1) == "W")
    g$yest  <- as.integer(substr(h, 2, 2) == "W")
    predict(m, g, type = "response")
  })
}

# Whole seasons simulated from the seasonal chain: wet days per month, rain in the
# season and the longest dry spell, against the real seasons. The first two days
# are drawn from the first two days of the real seasons.
season_outlook <- function(days, models, amount_fit, n_sim = 20000, seed = 2027) {
  P <- chain_matrix(models)
  d <- days[days$used, ]
  start <- d[d$dos %in% 1:2, c("season", "dos", "wet")]
  start <- reshape(start, idvar = "season", timevar = "dos", direction = "wide")
  start <- start[complete.cases(start), ]
  sim <- withr::with_seed(seed, {
    s <- matrix(FALSE, n_sim, 92)
    pick <- sample.int(nrow(start), n_sim, replace = TRUE)
    s[, 1] <- start$wet.1[pick]
    s[, 2] <- start$wet.2[pick]
    for (t in 3:92) {
      h <- 1 + s[, t - 2] + 2 * s[, t - 1]   # DD = 1, WD = 2, DW = 3, WW = 4
      s[, t] <- runif(n_sim) < P[cbind(t, h)]
    }
    rain <- matrix(0, n_sim, 92)
    rain[s] <- r_amount(sum(s), amount_fit)
    list(s = s, total = rowSums(rain))
  })
  month_of <- rep(names(MONTH_DAYS), MONTH_DAYS)
  runs <- runs_of(as.vector(t(sim$s)), rep(seq_len(n_sim), each = 92))
  sim_dry <- tapply(runs$length[!runs$wet], factor(runs$block[!runs$wet], levels = seq_len(n_sim)), max)
  sim_dry[is.na(sim_dry)] <- 0

  # real seasons: wet days scaled up for missing days, so seasons compare
  obs_wet <- function(rows) {
    vapply(split(rows, rows$season), function(s) sum(s$wet, na.rm = TRUE) * nrow(s) / sum(!is.na(s$wet)), numeric(1))
  }
  obs_spells <- observed_spells(d)
  obs_dry <- tapply(obs_spells$length[!obs_spells$wet], obs_spells$block[!obs_spells$wet], max)
  obs_total <- vapply(split(d, d$season), function(s) sum(s$rain_mm, na.rm = TRUE), numeric(1))

  summarise <- function(sim_x, obs_x, name) {
    data.frame(what = name, mean = mean(sim_x),
               lo = unname(quantile(sim_x, 0.05)), hi = unname(quantile(sim_x, 0.95)),
               obs_mean = mean(obs_x), obs_min = min(obs_x), obs_max = max(obs_x),
               obs_in_range = mean(obs_x >= quantile(sim_x, 0.05) & obs_x <= quantile(sim_x, 0.95)))
  }
  rows <- list(summarise(rowSums(sim$s), obs_wet(d), "Wet days, season"))
  for (m in names(MONTH_DAYS)) {
    cols <- which(month_of == m)
    rows[[m]] <- summarise(rowSums(sim$s[, cols]), obs_wet(d[d$dos %in% cols, ]), paste("Wet days,", m))
  }
  rows$dry   <- summarise(sim_dry, obs_dry, "Longest dry spell")
  rows$total <- summarise(sim$total, obs_total, "Rain in the season")
  out <- do.call(rbind, rows)
  rownames(out) <- NULL
  out$key <- c("wet", "oct", "nov", "dec", "dry", "total")

  # distribution of wet days in a season, simulated, and the real seasons
  wet_sim <- rowSums(sim$s)
  k <- 0:92
  list(table = out,
       wet_hist = data.frame(k = k, sim = tabulate(wet_sim + 1, 93) / n_sim),
       wet_obs = unname(round(obs_wet(d), 1)),
       share_by_day = data.frame(dos = 1:92, sim = colMeans(sim$s)),
       n_sim = n_sim)
}

# Leave one season out: refit both models without it, then forecast every day of
# it. The same rows are scored for all three forecasts (days with the last two
# days known), against the flat chance from the other seasons.
backtest <- function(days, models) {
  rows <- model_rows(days)
  seasons <- sort(unique(rows$season))
  out <- do.call(rbind, lapply(seasons, function(s) {
    train <- rows[rows$season != s, ]
    test  <- rows[rows$season == s, ]
    data.frame(season = s, dos = test$dos, wet = test$wet,
               flat   = mean(train$wet),
               season_p = predict(fit_glm(models$rhs_season, train), test, type = "response"),
               chain_p  = predict(fit_glm(models$rhs_chain, train), test, type = "response"))
  }))
  out
}

FORECASTS <- c(flat = "Same chance every day", season = "Long range (season only)",
               chain = "Next day (season and last two days)")

# Scores of the backtest: Brier score and skill against the flat chance, the area
# under the ROC curve, hits and false alarms at every threshold, and reliability.
score_backtest <- function(bt) {
  probs <- list(flat = bt$flat, season = bt$season_p, chain = bt$chain_p)
  y <- bt$wet
  brier_flat <- mean((bt$flat - y)^2)
  auc <- function(p) {
    r <- rank(p)
    n1 <- sum(y)
    n0 <- sum(!y)
    (sum(r[y]) - n1 * (n1 + 1) / 2) / (n1 * n0)
  }
  scores <- do.call(rbind, lapply(names(probs), function(k) {
    p <- probs[[k]]
    # the flat chance does not rank days, so it has no ROC area
    data.frame(key = k, name = FORECASTS[[k]], brier = mean((p - y)^2),
               skill = 1 - mean((p - y)^2) / brier_flat,
               auc = if (k == "flat") NA_real_ else auc(p))
  }))
  thresholds <- seq(0.05, 0.95, by = 0.01)
  grid <- do.call(rbind, lapply(c("season", "chain"), function(k) {
    p <- probs[[k]]
    do.call(rbind, lapply(thresholds, function(th) {
      call <- p > th
      data.frame(key = k, threshold = th,
                 hits = sum(call & y), misses = sum(!call & y),
                 false_alarms = sum(call & !y), correct_dry = sum(!call & !y))
    }))
  }))
  bins <- seq(0, 1, by = 0.1)
  reliability <- do.call(rbind, lapply(c("season", "chain"), function(k) {
    p <- probs[[k]]
    b <- cut(p, bins, include.lowest = TRUE)
    do.call(rbind, lapply(split(seq_along(p), b, drop = TRUE), function(i) {
      w <- wilson(sum(y[i]), length(i))
      data.frame(key = k, predicted = mean(p[i]), observed = w[["est"]],
                 lo = w[["lo"]], hi = w[["hi"]], n = length(i))
    }))
  }))
  rownames(reliability) <- NULL
  list(scores = scores, grid = grid, reliability = reliability,
       n_days = length(y), n_wet = sum(y), n_seasons = length(unique(bt$season)))
}

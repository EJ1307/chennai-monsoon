# Two-state Markov chain for wet (W) and dry (D) days: fit, tests and simulation.

# Wilson score interval for a proportion x / n
wilson <- function(x, n, level = 0.95) {
  if (n == 0) return(c(est = NA_real_, lo = NA_real_, hi = NA_real_))
  z <- qnorm(1 - (1 - level) / 2)
  p <- x / n
  centre <- (p + z^2 / (2 * n)) / (1 + z^2 / n)
  half   <- z * sqrt(p * (1 - p) / n + z^2 / (4 * n^2)) / (1 + z^2 / n)
  c(est = p, lo = centre - half, hi = centre + half)
}

state_factor <- function(x) factor(x, levels = c(FALSE, TRUE), labels = c("Dry", "Wet"))

# Yesterday by today counts. Pairs that include a missing day are left out.
transition_counts <- function(days) {
  ok <- !is.na(days$wet) & !is.na(days$prev)
  table(yesterday = state_factor(days$prev[ok]), today = state_factor(days$wet[ok]))
}

fit_chain <- function(days) {
  n   <- transition_counts(days)
  p01 <- wilson(n["Dry", "Wet"], sum(n["Dry", ]))   # P(wet today | dry yesterday)
  p11 <- wilson(n["Wet", "Wet"], sum(n["Wet", ]))   # P(wet today | wet yesterday)
  a <- p01[["est"]]
  b <- p11[["est"]]
  list(
    counts         = n,
    p01            = p01,
    p11            = p11,
    pi_wet         = a / (a + 1 - b),   # long-run share of wet days (steady state)
    mean_wet_spell = 1 / (1 - b),
    mean_dry_spell = 1 / a,
    n_pairs        = sum(n)
  )
}

# H1: does today depend on yesterday? Chi-square test of independence, 2 by 2 table.
test_h1 <- function(chain) {
  ct <- suppressWarnings(chisq.test(chain$counts, correct = FALSE))
  list(
    stat         = unname(ct$statistic),
    df           = 1L,
    p            = ct$p.value,
    min_expected = min(ct$expected),
    diff         = chain$p11[["est"]] - chain$p01[["est"]]
  )
}

# H2: is first order enough? The last two days (DD, WD, DW, WW) against today.
# Histories are compared only within the same yesterday state: DD with WD
# (yesterday dry) and DW with WW (yesterday wet), one degree of freedom each.
# History "WD" means wet the day before yesterday, dry yesterday.
test_h2 <- function(days) {
  ok <- !is.na(days$wet) & !is.na(days$prev) & !is.na(days$prev2)
  d  <- days[ok, ]
  history <- paste0(ifelse(d$prev2, "W", "D"), ifelse(d$prev, "W", "D"))
  tab <- table(history = factor(history, c("DD", "WD", "DW", "WW")), today = state_factor(d$wet))
  part <- function(rows) {
    t <- tab[rows, ]
    if (any(rowSums(t) == 0) || any(colSums(t) == 0)) return(c(stat = 0, min_e = NA))
    ct <- suppressWarnings(chisq.test(t, correct = FALSE))
    c(stat = unname(ct$statistic), min_e = min(ct$expected))
  }
  dry_yesterday <- part(c("DD", "WD"))
  wet_yesterday <- part(c("DW", "WW"))
  stat <- dry_yesterday[["stat"]] + wet_yesterday[["stat"]]
  list(
    stat         = stat,
    df           = 2L,
    p            = pchisq(stat, 2, lower.tail = FALSE),
    min_expected = suppressWarnings(min(dry_yesterday[["min_e"]], wet_yesterday[["min_e"]], na.rm = TRUE)),
    table        = tab,
    p_wet        = tab[, "Wet"] / rowSums(tab),
    n            = rowSums(tab)
  )
}

# H4: has the chain changed across decades? Homogeneity of the transition counts:
# one decade by today table for each yesterday state, 2 degrees of freedom each.
test_h4 <- function(days) {
  ok <- !is.na(days$wet) & !is.na(days$prev)
  d  <- days[ok, ]
  d$decade <- droplevels(d$decade)
  part <- function(yesterday_wet) {
    s  <- d[d$prev == yesterday_wet, ]
    tb <- table(s$decade, state_factor(s$wet))
    ct <- suppressWarnings(chisq.test(tb, correct = FALSE))
    list(stat = unname(ct$statistic), df = unname(ct$parameter), p = ct$p.value, table = tb)
  }
  from_dry <- part(FALSE)
  from_wet <- part(TRUE)
  per_decade <- do.call(rbind, lapply(levels(d$decade), function(dc) {
    a <- from_dry$table[dc, ]
    b <- from_wet$table[dc, ]
    p01 <- wilson(a[["Wet"]], sum(a))
    p11 <- wilson(b[["Wet"]], sum(b))
    data.frame(decade = dc,
               p01 = p01[["est"]], p01_lo = p01[["lo"]], p01_hi = p01[["hi"]],
               p11 = p11[["est"]], p11_lo = p11[["lo"]], p11_hi = p11[["hi"]])
  }))
  stat <- from_dry$stat + from_wet$stat
  df   <- from_dry$df + from_wet$df
  list(stat = stat, df = df, p = pchisq(stat, df, lower.tail = FALSE),
       from_dry = from_dry, from_wet = from_wet, per_decade = per_decade)
}

# Runs of wet or dry days. `code` is TRUE (wet), FALSE (dry) or NA (missing) and
# `block` marks the season, so a run never crosses into the next season. A run that
# touches a missing day is dropped, because its true length is unknown. Runs cut by
# the start or end of the period are kept, in the observed and simulated data alike.
runs_of <- function(code, block) {
  x <- ifelse(is.na(code), 2L, as.integer(code))
  n <- length(x)
  start <- c(TRUE, x[-1] != x[-n] | block[-1] != block[-n])
  len <- tabulate(cumsum(start))
  val <- x[start]
  blk <- block[start]
  m <- length(val)
  same_next <- c(blk[-1] == blk[-m], FALSE)
  same_prev <- c(FALSE, blk[-1] == blk[-m])
  next_missing <- c(val[-1] == 2L, FALSE) & same_next
  prev_missing <- c(FALSE, val[-m] == 2L) & same_prev
  keep <- val != 2L & !next_missing & !prev_missing
  data.frame(wet = val[keep] == 1L, length = len[keep], block = blk[keep])
}

observed_spells <- function(days) {
  days <- days[order(days$season, days$dos), ]
  runs_of(days$wet, days$season)
}

# Simulated wet (TRUE) and dry (FALSE) days: one row per season, one column per day
simulate_states <- function(p01, p11, n_days, n_seasons, p_start) {
  s <- matrix(FALSE, n_seasons, n_days)
  s[, 1] <- runif(n_seasons) < p_start
  for (t in seq_len(n_days)[-1]) {
    s[, t] <- runif(n_seasons) < ifelse(s[, t - 1], p11, p01)
  }
  s
}

# Bins for the goodness-of-fit statistic: wet spells 1, 2, 3, 4, 5+ days and
# dry spells 1, 2, 3-4, 5-7, 8-14, 15+ days. Long spells are 5+ wet or 15+ dry.
SPELL_BINS <- list(wet = c(1, 2, 3, 4, 5), dry = c(1, 2, 3, 5, 8, 15))
SPELL_BIN_NAMES <- list(
  wet = c("1 day", "2 days", "3 days", "4 days", "5 days or more"),
  dry = c("1 day", "2 days", "3 to 4 days", "5 to 7 days", "8 to 14 days", "15 days or more")
)
LONG_SPELL <- c(wet = 5, dry = 15)

# H3: does the fitted chain reproduce the observed spells? The chain simulates
# n_sets records, each with as many seasons as the observed record. The spread of
# those records gives a 90 percent range for every spell length, and a Monte Carlo
# p-value for a chi-square distance between observed and expected spell lengths.
compare_spells <- function(days, chain, n_sets = 1000, seed = 2026) {
  obs <- observed_spells(days)
  n_seasons <- length(unique(days$season))
  n_days    <- length(unique(days$dos))
  # a fixed seed keeps results stable, without resetting the session's own random numbers
  sim <- withr::with_seed(seed, simulate_states(chain$p01[["est"]], chain$p11[["est"]],
                                                n_days, n_seasons * n_sets, chain$pi_wet))
  runs <- runs_of(as.vector(t(sim)), rep(seq_len(n_seasons * n_sets), each = n_days))
  runs$set <- (runs$block - 1L) %/% n_seasons + 1L

  one_state <- function(state) {
    is_wet  <- state == "wet"
    o_len   <- obs$length[obs$wet == is_wet]
    s       <- runs[runs$wet == is_wet, ]
    k_max   <- min(n_days, max(c(o_len, quantile(s$length, 0.999))))
    # spells of each length (capped at k_max) in every simulated record
    idx     <- (s$set - 1L) * k_max + pmin(s$length, k_max)
    counts  <- matrix(tabulate(idx, nbins = n_sets * k_max), nrow = n_sets, byrow = TRUE)
    at_least <- t(apply(counts, 1, function(r) rev(cumsum(rev(r))))) / rowSums(counts)
    band    <- apply(at_least, 2, quantile, probs = c(0.05, 0.95), na.rm = TRUE)
    k       <- seq_len(k_max)
    curve <- data.frame(
      k        = k,
      observed = vapply(k, function(i) mean(o_len >= i), numeric(1)),
      n_obs    = vapply(k, function(i) sum(o_len >= i), numeric(1)),
      chain    = vapply(k, function(i) mean(s$length >= i), numeric(1)),
      lo       = band[1, ],
      hi       = band[2, ]
    )
    # Monte Carlo goodness of fit on binned spell lengths
    breaks   <- SPELL_BINS[[state]]
    nb       <- length(breaks)
    bin_of   <- function(len) findInterval(len, breaks)
    expected <- tabulate(bin_of(s$length), nbins = nb) / nrow(s)
    x2 <- function(o) {
      e <- sum(o) * expected
      sum((o - e)^2 / e)
    }
    o_bins  <- tabulate(bin_of(o_len), nbins = nb)
    x2_obs  <- x2(o_bins)
    sim_bins <- matrix(tabulate((s$set - 1L) * nb + bin_of(s$length), nbins = n_sets * nb),
                       nrow = n_sets, byrow = TRUE)
    x2_sim  <- apply(sim_bins, 1, x2)
    # the spell-length bin that contributes most to the distance
    e_bins  <- sum(o_bins) * expected
    worst   <- which.max((o_bins - e_bins)^2 / e_bins)
    long    <- LONG_SPELL[[state]]
    long_sets <- if (long <= k_max) at_least[, long] else rep(0, n_sets)
    list(
      curve     = curve,
      n_spells  = length(o_len),
      x2        = x2_obs,
      p_mc      = mean(x2_sim >= x2_obs),
      long      = long,
      long_obs  = mean(o_len >= long),
      long_n    = sum(o_len >= long),
      long_sim  = mean(s$length >= long),
      long_band = quantile(long_sets, c(0.05, 0.95), na.rm = TRUE),
      longest_obs = max(o_len),
      worst_bin = SPELL_BIN_NAMES[[state]][worst],
      worst_obs = o_bins[worst] / sum(o_bins),
      worst_exp = expected[worst]
    )
  }
  list(wet = one_state("wet"), dry = one_state("dry"),
       n_seasons = n_seasons, n_days = n_days, n_sets = n_sets)
}

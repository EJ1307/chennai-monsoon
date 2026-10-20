# The closing decision: a flood alert rule from the cost-loss model, and the
# alternative, reservoir buffer days for a chosen dry-spell risk.

# Chance of a heavy-rain day tomorrow for each current state:
# P(wet tomorrow | state) from the chain, times P(amount above the cut-off | wet)
# from the amount fit. Second-order states are used when H2 rejects first order.
alert_states <- function(days, chain, h2, fit, heavy_mm, second_order = FALSE) {
  p_heavy_if_wet <- wet_exceed(fit, heavy_mm)
  ok <- !is.na(days$wet) & !is.na(days$prev)
  heavy <- days$rain_mm > heavy_mm
  if (!second_order) {
    st <- data.frame(
      state = c("After a dry day", "After a wet day"),
      after = c("after a dry day", "after a wet day"),
      p_wet = c(chain$p01[["est"]], chain$p11[["est"]])
    )
    groups <- list(ok & !days$prev, ok & days$prev)
  } else {
    ok2 <- ok & !is.na(days$prev2)
    st <- data.frame(
      state = c("Two dry days", "Wet, then dry", "Dry, then wet", "Two wet days"),
      after = c("after two dry days", "after a wet day and then a dry one",
                "after a dry day and then a wet one", "after two wet days"),
      p_wet = unname(h2$p_wet[c("DD", "WD", "DW", "WW")])
    )
    groups <- list(ok2 & !days$prev2 & !days$prev, ok2 & days$prev2 & !days$prev,
                   ok2 & !days$prev2 & days$prev, ok2 & days$prev2 & days$prev)
  }
  st$p_heavy  <- st$p_wet * p_heavy_if_wet
  st$observed <- vapply(groups, function(g) mean(heavy[g]), numeric(1))
  st$n        <- vapply(groups, sum, numeric(1))
  st$p_heavy_if_wet <- p_heavy_if_wet
  st
}

# Longest dry spell in each season, observed and simulated from the chain
longest_dry <- function(days, chain, n_sim = 20000, seed = 5) {
  obs <- observed_spells(days)
  obs_max <- tapply(obs$length[!obs$wet], obs$block[!obs$wet], max)
  n_days <- length(unique(days$dos))
  sim <- withr::with_seed(seed, simulate_states(chain$p01[["est"]], chain$p11[["est"]],
                                                n_days, n_sim, chain$pi_wet))
  runs <- runs_of(as.vector(t(sim)), rep(seq_len(n_sim), each = n_days))
  sim_max <- tapply(runs$length[!runs$wet], runs$block[!runs$wet], max)
  sim_max <- c(sim_max, rep(0, n_sim - length(sim_max)))   # seasons with no dry day
  k <- 0:max(c(obs_max, sim_max))
  list(
    curve = data.frame(
      k        = k,
      chain    = vapply(k, function(i) mean(sim_max > i), numeric(1)),
      observed = vapply(k, function(i) mean(obs_max > i), numeric(1))
    ),
    obs_max = obs_max
  )
}

# Smallest k with P(longest dry spell > k) at or below the risk level
buffer_days <- function(curve, column, risk) {
  min(curve$k[curve[[column]] <= risk])
}

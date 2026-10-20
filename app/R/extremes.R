# Wet-day amounts: lognormal, gamma and Weibull fits, return periods, and the
# heavy-rain comparison across decades (H5).
#
# Only amounts of 2.5 mm or more are ever wet-day amounts, so each distribution is
# fitted by maximum likelihood with that cut-off built in (a left-truncated fit).
# Parameters are estimated on the log scale where they must be positive.

DIST_NAMES <- c(gamma = "Gamma", lognormal = "Lognormal", weibull = "Weibull")

dist_fun <- function(dist) {
  switch(dist,
    lognormal = list(
      d = function(x, th, log = FALSE) dlnorm(x, th[1], exp(th[2]), log = log),
      p = function(x, th, ...) plnorm(x, th[1], exp(th[2]), ...),
      q = function(u, th, ...) qlnorm(u, th[1], exp(th[2]), ...),
      start = function(x) c(mean(log(x)), log(sd(log(x)))),
      show  = function(th) sprintf("meanlog %.2f, sdlog %.2f", th[1], exp(th[2]))
    ),
    gamma = list(
      d = function(x, th, log = FALSE) dgamma(x, shape = exp(th[1]), scale = exp(th[2]), log = log),
      p = function(x, th, ...) pgamma(x, shape = exp(th[1]), scale = exp(th[2]), ...),
      q = function(u, th, ...) qgamma(u, shape = exp(th[1]), scale = exp(th[2]), ...),
      start = function(x) c(log(mean(x)^2 / var(x)), log(var(x) / mean(x))),
      show  = function(th) sprintf("shape %.2f, scale %.1f mm", exp(th[1]), exp(th[2]))
    ),
    weibull = list(
      d = function(x, th, log = FALSE) dweibull(x, shape = exp(th[1]), scale = exp(th[2]), log = log),
      p = function(x, th, ...) pweibull(x, shape = exp(th[1]), scale = exp(th[2]), ...),
      q = function(u, th, ...) qweibull(u, shape = exp(th[1]), scale = exp(th[2]), ...),
      start = function(x) c(log(0.9), log(mean(x))),
      show  = function(th) sprintf("shape %.2f, scale %.1f mm", exp(th[1]), exp(th[2]))
    )
  )
}

fit_amounts <- function(x, dist, cut = WET_MM, start = NULL, reltol = 1e-12) {
  f <- dist_fun(dist)
  nll <- function(th) {
    ll <- sum(f$d(x, th, log = TRUE)) - length(x) * f$p(cut, th, lower.tail = FALSE, log.p = TRUE)
    if (!is.finite(ll)) return(1e10)
    -ll
  }
  o <- optim(if (is.null(start)) f$start(x) else start, nll,
             control = list(maxit = 5000, reltol = reltol))
  list(dist = dist, theta = o$par, loglik = -o$value, aic = 2 * o$value + 2 * length(o$par),
       n = length(x), cut = cut, converged = o$convergence == 0)
}

# P(amount > x) on a wet day
wet_exceed <- function(fit, x) {
  f <- dist_fun(fit$dist)
  f$p(x, fit$theta, lower.tail = FALSE) / f$p(fit$cut, fit$theta, lower.tail = FALSE)
}

# The wet-day amount exceeded with probability `tail` (worked from the upper tail)
wet_quantile_upper <- function(fit, tail) {
  f <- dist_fun(fit$dist)
  f$q(tail * f$p(fit$cut, fit$theta, lower.tail = FALSE), fit$theta, lower.tail = FALSE)
}

r_amount <- function(n, fit) wet_quantile_upper(fit, runif(n))

# lambda: expected wet days in a 92-day season, allowing for missing days
wet_rate <- function(days) {
  ok <- !is.na(days$wet)
  92 * mean(days$wet[ok])
}

# T(x) is about 1 / (lambda [1 - F(x)]) years, so the T-year daily amount is the
# wet-day amount exceeded with probability 1 / (lambda T)
return_level  <- function(fit, lambda, T) wet_quantile_upper(fit, 1 / (lambda * T))
return_period <- function(fit, lambda, x) 1 / (lambda * wet_exceed(fit, x))

wet_amounts <- function(days) days$rain_mm[!is.na(days$wet) & days$wet]

# All three fits, ranked by AIC, with a check of the upper tail: the average
# distance (on the log scale) between fitted and observed amounts in the top 5 percent.
compare_fits <- function(days) {
  x <- sort(wet_amounts(days))
  n <- length(x)
  pp <- (seq_len(n) - 0.5) / n
  fits <- lapply(names(DIST_NAMES), function(dn) fit_amounts(x, dn))
  names(fits) <- names(DIST_NAMES)
  top <- pp > 0.95
  qq <- do.call(rbind, lapply(fits, function(f) {
    data.frame(dist = f$dist, observed = x, fitted = wet_quantile_upper(f, 1 - pp), p = pp)
  }))
  table <- do.call(rbind, lapply(fits, function(f) {
    fitted_top <- wet_quantile_upper(f, 1 - pp[top])
    data.frame(dist = f$dist, name = DIST_NAMES[[f$dist]],
               params = dist_fun(f$dist)$show(f$theta),
               loglik = f$loglik, aic = f$aic,
               tail_error = mean(abs(log(fitted_top / x[top]))))
  }))
  table$delta_aic <- table$aic - min(table$aic)
  table <- table[order(table$aic), ]
  list(fits = fits, table = table, qq = qq, n = n,
       best_aic = table$dist[1], best_tail = table$dist[which.min(table$tail_error)])
}

# Return levels with intervals from resampling whole seasons (keeps the
# dependence between days of the same season). T_GRID draws the curve.
RETURN_T <- c(2, 5, 10, 25, 50, 100)
T_GRID   <- sort(unique(c(exp(seq(log(1.2), log(200), length.out = 40)), RETURN_T)))

bootstrap_returns <- function(days, dist, T = RETURN_T, B = 400, seed = 11) {
  by_season <- split(days, days$season)
  amounts <- lapply(by_season, wet_amounts)
  wet_n <- vapply(by_season, function(s) sum(s$wet, na.rm = TRUE), numeric(1))
  ok_n  <- vapply(by_season, function(s) sum(!is.na(s$wet)), numeric(1))
  full  <- fit_amounts(unlist(amounts), dist)
  n <- length(by_season)
  levels <- matrix(NA_real_, B, length(T))
  withr::with_seed(seed, {
    for (b in seq_len(B)) {
      pick <- sample.int(n, n, replace = TRUE)
      fit  <- fit_amounts(unlist(amounts[pick]), dist, start = full$theta, reltol = 1e-8)
      lam  <- 92 * sum(wet_n[pick]) / sum(ok_n[pick])
      levels[b, ] <- return_level(fit, lam, T)
    }
  })
  lambda <- wet_rate(days)
  data.frame(
    T     = T,
    level = return_level(full, lambda, T),
    lo    = apply(levels, 2, quantile, 0.05),
    hi    = apply(levels, 2, quantile, 0.95)
  )
}

# H5: has the extreme tail changed? Share of wet days above the full-record 95th
# percentile by decade (chi-square), and ANOVA on log wet-day amounts.
test_h5 <- function(days) {
  w <- days[!is.na(days$wet) & days$wet, ]
  w$decade <- droplevels(w$decade)
  q95   <- unname(quantile(w$rain_mm, 0.95))
  heavy <- w$rain_mm > q95
  tb <- table(w$decade, heavy)
  ct <- suppressWarnings(chisq.test(tb, correct = FALSE))
  share <- do.call(rbind, lapply(levels(w$decade), function(dc) {
    s <- wilson(sum(heavy[w$decade == dc]), sum(w$decade == dc))
    data.frame(decade = dc, share = s[["est"]], lo = s[["lo"]], hi = s[["hi"]],
               heavy = sum(heavy[w$decade == dc]), wet = sum(w$decade == dc),
               geo_mean = exp(mean(log(w$rain_mm[w$decade == dc]))))
  }))
  av <- summary(aov(log(rain_mm) ~ decade, data = w))[[1]]
  list(q95 = q95, share = share, stat = unname(ct$statistic), df = unname(ct$parameter),
       p = ct$p.value, anova_f = av[["F value"]][1], anova_df = av[["Df"]],
       anova_p = av[["Pr(>F)"]][1])
}

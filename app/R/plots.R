# Chart style and the charts. Charts are ggplot2 drawn as interactive SVG by
# ggiraph, so every mark has a hover note.

INK     <- "#0b0b0b"
INK_2   <- "#52514e"
MUTED   <- "#898781"
GRID    <- "#ebeae4"
AXIS    <- "#c3c2b7"
WET     <- "#2a78d6"
WET_LT  <- "#b7d3f6"
DRY     <- "#eb6834"
DRY_LT  <- "#f0efec"
MISSING <- "#a9a79f"
FONT    <- "DM Sans"

register_chart_font <- function(dir = "www/fonts") {
  systemfonts::register_font(
    name = FONT,
    plain = file.path(dir, "DMSans-400.ttf"),
    bold  = file.path(dir, "DMSans-600.ttf")
  )
}

# Number formatting; `d` (decimal places) may differ from value to value
num <- function(x, d = 2) {
  d <- rep_len(d, length(x))
  vapply(seq_along(x), function(i) {
    if (is.na(x[i])) return(NA_character_)
    formatC(x[i], format = "f", digits = d[i], big.mark = ",")
  }, character(1))
}
pct <- function(x, d = 0) ifelse(is.na(x), NA_character_, paste0(num(100 * x, d), "%"))
fmt_date <- function(x, with_year = TRUE) {
  paste(as.integer(format(x, "%d")), format(x, if (with_year) "%b %Y" else "%b"))
}

theme_monsoon <- function(base_size = 10.5) {
  theme_minimal(base_size = base_size, base_family = FONT) +
    theme(
      panel.grid.major.x = element_blank(),
      panel.grid.minor   = element_blank(),
      panel.grid.major.y = element_line(colour = GRID, linewidth = 0.35),
      axis.text          = element_text(colour = MUTED, size = rel(0.85)),
      axis.title         = element_text(colour = INK_2, size = rel(0.85)),
      axis.title.x       = element_text(margin = margin(t = 6)),
      axis.title.y       = element_text(margin = margin(r = 6)),
      axis.ticks         = element_blank(),
      legend.position    = "none",
      strip.text         = element_text(colour = INK_2, hjust = 0, size = rel(0.9),
                                        margin = margin(b = 4)),
      plot.margin        = margin(6, 10, 4, 4)
    )
}

# Charts are drawn at the pixel size of their box (96 px to the inch), so text
# keeps the same size on a phone and on a wide screen.
as_girafe <- function(p, width_px = 700, height_px = 320) {
  girafe(
    ggobj = p, width_svg = width_px / 96, height_svg = height_px / 96,
    fonts = list(sans = FONT),
    options = list(
      opts_tooltip(
        css = paste0("background:", INK, ";color:#fff;padding:7px 10px;border-radius:8px;",
                     "font-family:'DM Sans',sans-serif;font-size:12px;line-height:1.4;"),
        opacity = 0.96, offx = 14, offy = 14, use_fill = FALSE
      ),
      opts_hover(css = "opacity:1;"),
      opts_hover_inv(css = "opacity:0.45;"),
      opts_selection(type = "none"),
      opts_toolbar(saveaspng = FALSE, hidden = c("selection", "zoom", "misc")),
      opts_sizing(rescale = TRUE, width = 1)
    )
  )
}

# Spell lengths: share of spells lasting at least k days, observed against the chain
plot_spells <- function(res, state) {
  col <- if (state == "wet") WET else DRY
  d <- res$curve
  floor_y <- 0.001
  d <- d[d$chain >= floor_y | d$observed > 0, ]
  d$lo <- pmax(d$lo, floor_y)
  d$hi <- pmax(d$hi, floor_y)
  d$tip <- sprintf(
    "<b>%d %s days or longer</b><br>Observed: %s of spells (%d)<br>Chain: %s, 90%% range %s to %s",
    d$k, state, pct(d$observed, 1), as.integer(d$n_obs), pct(d$chain, 1), pct(d$lo, 1), pct(d$hi, 1)
  )
  pts <- d[d$observed > 0, ]
  ggplot(d, aes(k)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = INK, alpha = 0.07) +
    geom_line(aes(y = chain), colour = INK_2, linewidth = 0.55) +
    geom_point_interactive(data = pts, aes(y = observed, tooltip = tip, data_id = k),
                           colour = col, size = 2.3) +
    scale_y_log10(labels = function(b) pct(b, ifelse(b < 0.01, 1, 0)),
                  breaks = c(1, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001)) +
    scale_x_continuous(breaks = function(l) unique(round(pretty(l, 6)))) +
    coord_cartesian(ylim = c(floor_y, 1)) +
    labs(x = sprintf("%s spell length, days or longer", tools::toTitleCase(state)),
         y = "Share of spells (log scale)") +
    theme_monsoon()
}

# One real season next to one season simulated from the chain
plot_season <- function(obs, sim, obs_label) {
  obs$panel <- obs_label
  sim$panel <- "Simulated from the chain"
  d <- rbind(obs, sim)
  d$panel <- factor(d$panel, c(obs_label, "Simulated from the chain"))
  d$kind <- ifelse(d$rain_mm >= WET_MM, "wet", "light")
  d <- d[d$rain_mm > 0, ]
  d$tip <- sprintf("<b>%s</b><br>%s mm%s", fmt_date(d$date, FALSE), num(d$rain_mm, 1),
                   ifelse(d$kind == "light", " (dry day: under 2.5 mm)", ""))
  month_starts <- c("1 Oct" = 1, "1 Nov" = 32, "1 Dec" = 62)
  ggplot(d, aes(dos, rain_mm)) +
    geom_col_interactive(aes(fill = kind, tooltip = tip, data_id = paste(panel, dos)),
                         width = 0.72) +
    facet_wrap(~panel, ncol = 1) +
    scale_fill_manual(values = c(wet = WET, light = WET_LT)) +
    scale_y_sqrt(breaks = c(10, 50, 100, 200, 300), expand = expansion(mult = c(0, 0.05))) +
    scale_x_continuous(breaks = month_starts, labels = names(month_starts),
                       limits = range(c(obs$dos, sim$dos)) + c(-0.6, 0.6),
                       expand = expansion(0)) +
    labs(x = NULL, y = "Rain, mm (square-root scale)") +
    theme_monsoon() +
    theme(panel.spacing.y = unit(10, "pt"))
}

# Rainfall in each October to December season
plot_season_totals <- function(seasons) {
  s <- seasons
  s$decade <- decade_of(s$season)
  s$tip <- ifelse(
    s$used,
    sprintf("<b>%d</b><br>%s mm, %d wet days<br>ERA5 agrees on %s of days", s$season,
            num(s$total_mm, 0), s$wet_days, pct(s$agree_share)),
    sprintf("<b>%d</b><br>Not used: %d days missing", s$season, s$missing_days)
  )
  ggplot(s, aes(season, total_mm)) +
    geom_col_interactive(aes(fill = used, tooltip = tip, data_id = season), width = 0.72) +
    geom_vline(xintercept = c(2001.5, 2012.5), colour = AXIS, linewidth = 0.35,
               linetype = "22") +
    annotate("text", x = c(1996, 2007, 2018), y = max(s$total_mm) * 1.07, label = DECADES,
             family = FONT, colour = MUTED, size = 3.1) +
    scale_fill_manual(values = c(`TRUE` = WET, `FALSE` = MISSING)) +
    scale_y_continuous(labels = function(b) num(b, 0), expand = expansion(mult = c(0, 0.1))) +
    scale_x_continuous(breaks = seq(1991, 2023, 4), expand = expansion(add = 0.6)) +
    labs(x = NULL, y = "Rain, Oct to Dec (mm)") +
    theme_monsoon()
}

# p(wet after wet) and p(wet after dry) by decade, with 95% intervals
plot_decade_chain <- function(per_decade) {
  d <- rbind(
    data.frame(decade = per_decade$decade, what = "Wet after wet",
               est = per_decade$p11, lo = per_decade$p11_lo, hi = per_decade$p11_hi),
    data.frame(decade = per_decade$decade, what = "Wet after dry",
               est = per_decade$p01, lo = per_decade$p01_lo, hi = per_decade$p01_hi)
  )
  d$decade <- factor(d$decade, DECADES)
  d$tip <- sprintf("<b>%s, %s</b><br>%s (95%% interval %s to %s)", d$what, d$decade,
                   num(d$est), num(d$lo), num(d$hi))
  lab <- d[d$decade == DECADES[3], ]
  ggplot(d, aes(decade, est, colour = what)) +
    geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 0.9, alpha = 0.35) +
    geom_point_interactive(aes(tooltip = tip, data_id = paste(what, decade)), size = 2.8) +
    geom_text(data = lab, aes(label = what), hjust = 0, nudge_x = 0.14,
              family = FONT, size = 3.2) +
    scale_colour_manual(values = c("Wet after wet" = WET, "Wet after dry" = DRY)) +
    scale_y_continuous(limits = c(0, 0.8), breaks = seq(0, 0.8, 0.2)) +
    scale_x_discrete(expand = expansion(add = c(0.5, 1.25))) +
    labs(x = NULL, y = "Probability") +
    theme_monsoon()
}

# Share of wet days above the full-record 95th percentile, by decade
plot_heavy_share <- function(h5) {
  d <- h5$share
  d$decade <- factor(d$decade, DECADES)
  d$tip <- sprintf("<b>%s</b><br>%d of %d wet days above %s mm<br>%s (95%% interval %s to %s)",
                   d$decade, d$heavy, d$wet, num(h5$q95, 0), pct(d$share, 1),
                   pct(d$lo, 1), pct(d$hi, 1))
  ggplot(d, aes(decade, share)) +
    geom_hline(yintercept = 0.05, colour = AXIS, linewidth = 0.35, linetype = "22") +
    geom_linerange(aes(ymin = lo, ymax = hi), colour = WET, linewidth = 0.9, alpha = 0.35) +
    geom_point_interactive(aes(tooltip = tip, data_id = decade), colour = WET, size = 2.8) +
    annotate("text", x = 3.45, y = 0.05, label = "5% overall", hjust = 0, vjust = -0.6,
             family = FONT, colour = MUTED, size = 3) +
    scale_y_continuous(limits = c(0, 0.12), breaks = seq(0, 0.12, 0.02),
                       labels = function(b) pct(b)) +
    scale_x_discrete(expand = expansion(add = c(0.5, 1.1))) +
    labs(x = NULL, y = "Wet days that were heavy") +
    theme_monsoon()
}

# Upper half of the QQ plot for the three fits, chosen fit highlighted
plot_qq <- function(fits, chosen) {
  d <- fits$qq[fits$qq$p > 0.5, ]
  d$name <- DIST_NAMES[d$dist]
  d$chosen <- d$dist == chosen
  d$tip <- sprintf("<b>%s</b><br>Observed %s mm<br>Fit says %s mm", d$name,
                   num(d$observed, 1), num(d$fitted, 1))
  d <- d[order(d$chosen), ]
  ggplot(d, aes(fitted, observed)) +
    geom_abline(slope = 1, intercept = 0, colour = AXIS, linewidth = 0.35, linetype = "22") +
    geom_point_interactive(aes(colour = chosen, size = chosen, tooltip = tip,
                               data_id = paste(dist, p))) +
    scale_colour_manual(values = c(`TRUE` = WET, `FALSE` = "#bdbcb5")) +
    scale_size_manual(values = c(`TRUE` = 1.6, `FALSE` = 1.1)) +
    scale_x_log10(breaks = c(10, 30, 100, 300, 1000)) +
    scale_y_log10(breaks = c(10, 30, 100, 300)) +
    labs(x = "Fitted wet-day amount, mm (log scale)", y = "Observed, mm (log scale)") +
    theme_monsoon() +
    theme(panel.grid.major.x = element_line(colour = GRID, linewidth = 0.35))
}

# Return level curve with its 90% band and the largest observed days
plot_returns <- function(curve, observed, record_years, fit_name) {
  curve$tip <- sprintf("<b>%s-year day</b><br>%s mm (90%% range %s to %s)<br>%s fit",
                       num(curve$T, ifelse(curve$T < 10, 1, 0)), num(curve$level, 0),
                       num(curve$lo, 0), num(curve$hi, 0), fit_name)
  observed$tip <- sprintf("<b>%s</b><br>%s mm, about once in %s years in this record",
                          fmt_date(observed$date), num(observed$rain_mm, 1),
                          num(observed$T, ifelse(observed$T < 10, 1, 0)))
  top <- max(c(curve$hi, observed$rain_mm))
  ggplot(curve, aes(T)) +
    annotate("rect", xmin = record_years, xmax = Inf, ymin = -Inf, ymax = Inf,
             fill = INK, alpha = 0.035) +
    annotate("text", x = max(curve$T), y = top * 0.03, hjust = 1, vjust = 0,
             label = "Beyond\nthe record", family = FONT, colour = MUTED,
             size = 3, lineheight = 0.95) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = WET, alpha = 0.14) +
    geom_line(aes(y = level), colour = WET, linewidth = 0.7) +
    geom_point_interactive(aes(y = level, tooltip = tip, data_id = T), colour = WET,
                           size = 4, alpha = 0.01) +
    geom_point_interactive(data = observed, aes(T, rain_mm, tooltip = tip, data_id = date),
                           colour = INK, size = 1.9) +
    scale_x_log10(breaks = c(1, 2, 5, 10, 25, 50, 100, 200)) +
    scale_y_continuous(limits = c(0, top * 1.04), expand = expansion(0)) +
    labs(x = "Return period, years (log scale)", y = "Daily rain, mm") +
    theme_monsoon() +
    theme(panel.grid.major.x = element_line(colour = GRID, linewidth = 0.35))
}

# Chance of a heavy-rain day tomorrow by state, against the cost-loss ratio
plot_alert <- function(states, cl) {
  d <- states
  d$state <- factor(d$state, rev(d$state))
  d$alert <- d$p_heavy > cl
  d$tip <- sprintf("<b>%s</b><br>Model: %s chance of a heavy day tomorrow<br>Record: %s of %d days",
                   d$state, pct(d$p_heavy, 1), pct(d$observed, 1), d$n)
  top <- max(c(d$p_heavy, d$observed, cl)) * 1.28
  ggplot(d, aes(p_heavy, state)) +
    geom_col_interactive(aes(fill = alert, tooltip = tip, data_id = state), width = 0.56) +
    geom_point(aes(x = observed), shape = 124, size = 5.5, colour = INK) +
    geom_vline(xintercept = cl, colour = INK, linewidth = 0.5) +
    geom_text(aes(label = ifelse(alert, "Alert", "No alert"), x = top,
                  colour = alert),
              hjust = 1, family = FONT, size = 3.2) +
    scale_colour_manual(values = c(`TRUE` = INK, `FALSE` = MUTED)) +
    scale_fill_manual(values = c(`TRUE` = WET, `FALSE` = WET_LT)) +
    scale_x_continuous(limits = c(0, top), labels = function(b) pct(b, 1),
                       expand = expansion(0)) +
    labs(x = "Chance of a heavy-rain day tomorrow", y = NULL) +
    theme_monsoon() +
    theme(panel.grid.major.y = element_blank(),
          panel.grid.major.x = element_line(colour = GRID, linewidth = 0.35),
          axis.text.y = element_text(colour = INK_2, size = rel(0.95)))
}

# P(longest dry spell in a season > k), chain against the record
plot_buffer <- function(ld, risk, k_chain) {
  d <- ld$curve
  d$tip <- sprintf("<b>Longest dry spell over %d days</b><br>Chain: %s of seasons<br>Record: %s of seasons",
                   d$k, pct(d$chain, 1), pct(d$observed, 0))
  ggplot(d, aes(k)) +
    geom_hline(yintercept = risk, colour = INK, linewidth = 0.4, linetype = "22") +
    geom_step(aes(y = observed), colour = DRY, linewidth = 0.6, direction = "hv") +
    geom_line(aes(y = chain), colour = INK_2, linewidth = 0.6) +
    geom_point_interactive(aes(y = chain, tooltip = tip, data_id = k), size = 3.2,
                           colour = INK_2, alpha = 0.01) +
    annotate("point", x = k_chain, y = risk, colour = INK, size = 2.6) +
    annotate("text", x = k_chain, y = risk, label = sprintf("%d days", k_chain),
             hjust = -0.2, vjust = -0.7, family = FONT, size = 3.3, colour = INK) +
    scale_y_continuous(limits = c(0, 1), labels = function(b) pct(b)) +
    scale_x_continuous(breaks = seq(0, 100, 10)) +
    labs(x = "Buffer, days without useful rain", y = "Seasons with a longer dry spell") +
    theme_monsoon()
}

# Chance of a wet day through the season: the season model with its 95% band, the
# share of real seasons wet on each date, the chosen history, and the chosen day
plot_forecast <- function(ft, obs, dos, history, year) {
  any <- ft[ft$history == "any", ]
  any$date <- dos_date(any$dos, year)
  any$tip <- sprintf("<b>%s</b><br>Chance of a wet day: %s<br>Record: %d of %d seasons wet",
                     fmt_date(any$date, FALSE), pct(any$p), obs$wet, obs$n)
  pick <- ft[ft$history == history & ft$dos == dos, ]
  month_starts <- c("1 Oct" = 1, "1 Nov" = 32, "1 Dec" = 62)
  p <- ggplot(any, aes(dos)) +
    geom_ribbon(aes(ymin = lo, ymax = hi), fill = WET_LT, alpha = 0.55) +
    geom_point(data = obs, aes(dos, share), colour = MUTED, size = 1.1) +
    geom_line(aes(y = p), colour = WET, linewidth = 0.8)
  if (history != "any") {
    h <- ft[ft$history == history, ]
    p <- p + geom_line(data = h, aes(dos, p), colour = INK, linewidth = 0.6, linetype = "22")
  }
  p +
    geom_point_interactive(aes(y = p, tooltip = tip, data_id = dos), size = 3.5,
                           colour = WET, alpha = 0.01) +
    geom_vline(xintercept = dos, colour = INK, linewidth = 0.35) +
    annotate("point", x = dos, y = pick$p, colour = INK, size = 2.8) +
    annotate("text", x = dos, y = pick$p, label = pct(pick$p),
             hjust = if (dos > 70) 1.25 else -0.25, vjust = -0.6, family = FONT, size = 3.4,
             colour = INK) +
    scale_y_continuous(limits = c(0, max(1, max(ft$hi))), labels = function(b) pct(b),
                       breaks = seq(0, 1, 0.2)) +
    scale_x_continuous(breaks = month_starts, labels = names(month_starts),
                       expand = expansion(add = 1)) +
    labs(x = NULL, y = "Chance of a wet day") +
    theme_monsoon()
}

# Reliability of the backtest forecasts: when they said p, how often it rained
plot_reliability <- function(rel) {
  d <- rel
  d$name <- ifelse(d$key == "season", "Long range", "Next day")
  d$tip <- sprintf("<b>%s</b><br>Said %s on average<br>Wet on %s of %d days",
                   d$name, pct(d$predicted), pct(d$observed), d$n)
  ggplot(d, aes(predicted, observed, colour = key)) +
    geom_abline(slope = 1, intercept = 0, colour = AXIS, linetype = "22") +
    geom_linerange(aes(ymin = lo, ymax = hi), linewidth = 0.5, alpha = 0.5) +
    geom_line(linewidth = 0.6) +
    geom_point_interactive(aes(tooltip = tip, data_id = paste(key, predicted), size = n)) +
    scale_colour_manual(values = c(season = DRY, chain = WET)) +
    scale_size_area(max_size = 4.5) +
    scale_x_continuous(limits = c(0, 1), labels = function(b) pct(b)) +
    scale_y_continuous(limits = c(0, 1), labels = function(b) pct(b)) +
    coord_equal() +
    labs(x = "Forecast chance", y = "Days that were wet") +
    theme_monsoon()
}

# Every October to December day of every season
plot_calendar <- function(days) {
  d <- days
  d$kind <- ifelse(is.na(d$rain_mm), "Missing",
                   ifelse(d$rain_mm >= WET_MM, "Wet",
                          ifelse(d$rain_mm > 0, "Light rain", "Dry")))
  d$tip <- sprintf("<b>%s</b><br>%s", fmt_date(d$date),
                   ifelse(d$kind == "Missing", sub("missing, ", "Missing: ", d$status),
                          paste0(num(d$rain_mm, 1), " mm")))
  d$row <- factor(d$season, rev(sort(unique(d$season))))
  month_starts <- c("1 Oct" = 1, "1 Nov" = 32, "1 Dec" = 62)
  ggplot(d, aes(dos, row)) +
    geom_tile_interactive(aes(fill = kind, tooltip = tip, data_id = date),
                          colour = "#ffffff", linewidth = 0.25) +
    scale_fill_manual(values = c(Wet = WET, `Light rain` = WET_LT, Dry = DRY_LT,
                                 Missing = MISSING)) +
    scale_x_continuous(breaks = month_starts, labels = names(month_starts),
                       expand = expansion(0), position = "top") +
    scale_y_discrete(breaks = as.character(seq(1991, 2023, 2))) +
    labs(x = NULL, y = NULL) +
    theme_monsoon() +
    theme(panel.grid.major.y = element_blank())
}

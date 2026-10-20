# Chennai northeast monsoon: wet and dry spells.
# Run with shiny::runApp("app") from the project folder, or open this file in
# RStudio and press Run App. Files in R/ are loaded automatically first.
# The same app runs in a web browser through Shinylive: see deploy/build_site.R.

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(ggplot2)
  library(ggiraph)
})

# Chart text uses the bundled DM Sans; without it, charts fall back to the system sans
try(register_chart_font("www/fonts"), silent = TRUE)

# Data and the full-record results used on several pages --------------------------
days    <- load_days("data")
audit   <- load_audit("data")
pre     <- load_precomputed("data")   # slow simulations, made by prepare/precompute.R
ond     <- select_days(days, 10:12, "All years")
n_used  <- length(unique(ond$season))
record_years <- sum(!is.na(ond$wet)) / 92

chain_all  <- fit_chain(ond)
h2_all     <- test_h2(ond)
h4         <- test_h4(ond)
h5         <- test_h5(ond)
fits       <- pre$fits
lambda     <- wet_rate(ond)
best_fit   <- fits$fits[[fits$best_aic]]
dry_max    <- pre$dry_max
spells_all <- pre$spells[[spell_key("Oct to Dec", "All years")]]
use_second_order <- h2_all$p < 0.05
alert_tab  <- alert_states(ond, chain_all, h2_all, best_fit, h5$q95, use_second_order)

get_returns <- function(dist) pre$returns[[dist]]

# Small text helpers ---------------------------------------------------------------
p_text <- function(p) if (p < 0.001) "p < 0.001" else paste0("p = ", num(p, 3))
chi_text <- function(stat, df, p) {
  HTML(sprintf("&chi;&sup2; = %s, %d df, %s", num(stat, 1), as.integer(df), p_text(p)))
}
join_and <- function(x) {
  n <- length(x)
  if (n == 1) x else paste0(paste(x[-n], collapse = ", "), ", and ", x[n])
}
plural <- function(n, word) paste(n, if (n == 1) word else paste0(word, "s"))
months_label <- function(m) if (m == "Oct to Dec") "October to December" else m

test_block <- function(tag, question, answer, stats, note = NULL) {
  div(class = "test",
      div(class = "test-q", span(class = "test-tag", tag), question),
      p(class = "test-a", answer),
      p(class = "test-s", stats),
      if (!is.null(note)) p(class = "test-s", note))
}
fact_item <- function(value, label) {
  div(class = "fact", div(class = "fact-v", value), div(class = "fact-l", label))
}
spec_item <- function(label, value) {
  div(class = "spec", div(class = "spec-l", label), div(class = "spec-v", value))
}
legend_item <- function(key, label) span(class = "li", span(class = paste("key", key)), label)
page_head <- function(title, lede_id = NULL, lede = NULL) {
  div(class = "page-head", h1(title),
      if (!is.null(lede_id)) uiOutput(lede_id, class = "lede") else p(class = "lede", lede))
}
caption <- function(...) p(class = "caption", ...)

# UI ---------------------------------------------------------------------------------
# The stock Bootstrap 5 theme comes precompiled, so nothing is compiled at start-up
# (slow inside a browser). Fonts, colours and shapes are set in www/styles.css.
fit_choices <- setNames(fits$table$dist, fits$table$name)

ui <- page_navbar(
  title = span(class = "brand", span(class = "brand-dot"), "Chennai monsoon spells"),
  theme = bs_theme(version = 5),
  fillable = FALSE,
  navbar_options = navbar_options(underline = FALSE),
  header = tags$head(tags$link(rel = "stylesheet", href = "styles.css"),
                     tags$link(rel = "icon", type = "image/svg+xml", href = "favicon.svg")),

  nav_panel(
    "Wet and dry days",
    layout_sidebar(
      fillable = FALSE,
      sidebar = sidebar(
        width = 230, class = "picker",
        open = list(desktop = "open", mobile = "always-above"),
        radioButtons("months", "Months", names(MONTHS)),
        radioButtons("decade", "Decade", c("All years", DECADES)),
        p(class = "side-note", "Nungambakkam (IMD). A day is wet with 2.5 mm of rain or more.")
      ),
      page_head("How rain follows rain", "lede_main"),
      layout_columns(
        col_widths = breakpoints(sm = 12, lg = c(5, 7)),
        card(card_header("Transition probabilities"), uiOutput("matrix")),
        card(card_header("What the chain says"), uiOutput("chain_facts"))
      ),
      card(
        card_header("Spell lengths: real seasons against the chain"),
        div(class = "legend",
            span(class = "li", span(class = "key key-dot wet"), span(class = "key key-dot dry"), "Observed"),
            legend_item("key-line", "Chain, with its 90% range")),
        layout_columns(
          col_widths = breakpoints(sm = 12, md = c(6, 6)),
          girafeOutput("spell_wet"), girafeOutput("spell_dry")
        ),
        uiOutput("spell_verdict")
      ),
      card(
        card_header(class = "with-action", span("One season, real and simulated"),
                    actionButton("resim", "Simulate another", class = "btn-sm btn-primary")),
        girafeOutput("season_plot"),
        uiOutput("season_caption")
      )
    )
  ),

  nav_panel(
    "Decades",
    div(class = "page",
        page_head("Has the monsoon changed?", "lede_decades"),
        card(card_header("Rain in each season"), girafeOutput("season_totals"),
             caption("October to December totals at Nungambakkam. Grey seasons have a missing month and are not used.")),
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(6, 6)),
          card(card_header("Persistence by decade (H4)"), girafeOutput("decade_chain"),
               uiOutput("h4_text")),
          card(card_header("Heavy-rain days by decade (H5)"), girafeOutput("heavy_share"),
               uiOutput("h5_text"))
        )
    )
  ),

  nav_panel(
    "Extremes",
    div(class = "page",
        page_head("How heavy can one day get?", "lede_extremes"),
        card(
          card_header(class = "with-action", span("Which distribution fits"),
                      radioButtons("fit", NULL, fit_choices, selected = fits$best_aic, inline = TRUE)),
          layout_columns(
            col_widths = breakpoints(sm = 12, lg = c(5, 7)),
            div(uiOutput("fit_table"),
                caption("Fitted to the amounts of all wet days (2.5 mm or more), October to December, with the 2.5 mm cut-off built into the likelihood.")),
            div(uiOutput("qq_legend"), girafeOutput("qq_plot"),
                caption("Upper half of the wet days. On the dashed line, fitted and observed amounts agree."))
          )
        ),
        card(
          card_header("Return periods"),
          layout_columns(
            col_widths = breakpoints(sm = 12, lg = c(8, 4)),
            girafeOutput("return_plot"),
            uiOutput("return_table")
          ),
          caption("T(x) is about 1 / (λ [1 − F(x)]) years, where λ is the mean number of wet days per season. Ranges come from resampling whole seasons 400 times.")
        )
    )
  ),

  nav_panel(
    "Decision",
    div(class = "page",
        page_head("When should a flood alert go out?", "lede_decision"),
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(7, 5)),
          card(card_header("Flood alert by the last two days"), girafeOutput("alert_plot"),
               uiOutput("alert_caption")),
          card(card_header("The alert rule"),
               sliderInput("cl", "Cost-loss ratio C/L", min = 0.002, max = 0.06,
                           value = 0.02, step = 0.001, ticks = FALSE, width = "100%"),
               p(class = "side-note", "C is the cost of acting on an alert. L is the loss when heavy rain arrives without one. Alert when the chance of heavy rain is above C/L."),
               uiOutput("alert_rule"))
        ),
        layout_columns(
          col_widths = breakpoints(sm = 12, lg = c(7, 5)),
          card(card_header("Dry-spell buffer for a reservoir"), girafeOutput("buffer_plot"),
               caption(sprintf("Longest run of dry days in a season. Grey line: 20,000 seasons simulated from the chain. Orange steps: the %d real seasons.", n_used))),
          card(card_header("Buffer days"),
               sliderInput("risk", "Accepted risk of a longer dry spell", min = 0.01,
                           max = 0.5, value = 0.1, step = 0.01, ticks = FALSE, width = "100%"),
               uiOutput("buffer_rule"))
        )
    )
  ),

  nav_panel(
    "Data",
    div(class = "page",
        page_head("Where the numbers come from", lede = paste(
          "Daily rain at Nungambakkam from the OpenCity Chennai rainfall dataset",
          "(state gauge records credited to TNDMA, dataset credited to IMD).",
          "The file lists rainy days only, so every gap was checked before any modelling.")),
        card(card_header(class = "with-action", span("Rules fixed before modelling"),
                         downloadButton("download", "Download the clean data",
                                        class = "btn-sm btn-outline-primary")),
             uiOutput("audit_rules")),
        card(card_header("Every October to December day"),
             div(class = "legend",
                 legend_item("key-sq wet", "Wet"),
                 legend_item("key-sq light", "Light rain, under 2.5 mm"),
                 legend_item("key-sq dry", "Dry"),
                 legend_item("key-sq missing", "Missing")),
             girafeOutput("calendar")),
        card(card_header("What the audit found"), uiOutput("audit_found"))
    )
  ),

  nav_spacer(),
  nav_item(span(class = "nav-meta", "Nungambakkam · 1991 to 2023"))
)

# Server ---------------------------------------------------------------------------
server <- function(input, output, session) {

  # Each chart is drawn at the width of its box; phones get a slightly shorter chart
  draw <- function(id, plot, height, phone_height = height) {
    w <- session$clientData[[paste0("output_", id, "_width")]]
    if (is.null(w) || w < 100) w <- 700
    as_girafe(plot, w, if (w < 560) phone_height else height)
  }

  # Wet and dry days -----------------------------------------------------------------
  sel    <- reactive(select_days(days, MONTHS[[input$months]], input$decade))
  chain  <- reactive(fit_chain(sel()))
  h1     <- reactive(test_h1(chain()))
  h2     <- reactive(test_h2(sel()))
  spells <- reactive(pre$spells[[spell_key(input$months, input$decade)]])
  n_sel  <- reactive(length(unique(sel()$season)))

  output$lede_main <- renderUI({
    ch <- chain()
    years <- if (input$decade == "All years") "1991 to 2023" else input$decade
    p(sprintf("%s, %s (%s). After a wet day the next day is wet %s of the time; after a dry day, %s.",
              months_label(input$months), years, plural(n_sel(), "season"),
              pct(ch$p11[["est"]]), pct(ch$p01[["est"]])))
  })

  output$matrix <- renderUI({
    ch <- chain()
    tile <- function(est, lo, hi, state) {
      div(class = paste("tile", state), style = sprintf("--a:%.3f", est),
          div(class = "tile-p", num(est)),
          div(class = "tile-ci", paste(num(lo), "to", num(hi))))
    }
    a <- ch$p01
    b <- ch$p11
    tagList(
      div(class = "matrix",
          div(), div(class = "m-head", "Dry today"), div(class = "m-head", "Wet today"),
          div(class = "m-row", "Dry yesterday"),
          tile(1 - a[["est"]], 1 - a[["hi"]], 1 - a[["lo"]], "dry"),
          tile(a[["est"]], a[["lo"]], a[["hi"]], "wet"),
          div(class = "m-row", "Wet yesterday"),
          tile(1 - b[["est"]], 1 - b[["hi"]], 1 - b[["lo"]], "dry"),
          tile(b[["est"]], b[["lo"]], b[["hi"]], "wet")),
      caption(sprintf("From %s pairs of days with data. Ranges are 95%% intervals. Wet: 2.5 mm or more.",
                      num(ch$n_pairs, 0)))
    )
  })

  output$chain_facts <- renderUI({
    ch <- chain()
    t1 <- h1()
    t2 <- h2()
    small <- if (min(t2$min_expected, t1$min_expected) < 5) "Some expected counts are under 5: read with care." else NULL
    pw <- t2$p_wet
    h2_answer <- if (t2$p < 0.05) {
      sprintf("No. The day before yesterday still matters: after two dry days rain follows %s of the time, after a wet day and then a dry one %s.",
              pct(pw[["DD"]]), pct(pw[["WD"]]))
    } else {
      "Yes. Knowing the day before yesterday does not change the odds in a way the data can detect."
    }
    tagList(
      div(class = "facts",
          fact_item(pct(ch$pi_wet), "of days wet in the long run"),
          fact_item(paste(num(ch$mean_wet_spell, 1), "days"), "typical wet spell"),
          fact_item(paste(num(ch$mean_dry_spell, 1), "days"), "typical dry spell")),
      test_block("H1", "Does today depend on yesterday?",
                 if (t1$p < 0.05) sprintf("Yes. A wet yesterday raises the chance of rain today by %s points.", num(100 * t1$diff, 0))
                 else "Not detectably in this selection.",
                 chi_text(t1$stat, t1$df, t1$p)),
      test_block("H2", "Is one day of memory enough?", h2_answer,
                 chi_text(t2$stat, t2$df, t2$p), small)
    )
  })

  output$spell_wet <- renderGirafe(draw("spell_wet", plot_spells(spells()$wet, "wet"), 300, 260))
  output$spell_dry <- renderGirafe(draw("spell_dry", plot_spells(spells()$dry, "dry"), 300, 260))

  output$spell_verdict <- renderUI({
    s <- spells()
    line <- function(r, state) {
      name <- tools::toTitleCase(state)
      head <- if (r$p_mc >= 0.05) {
        sprintf("%s spells match the chain (Monte Carlo %s).", name, p_text(r$p_mc))
      } else {
        sprintf("%s spells do not match the chain (Monte Carlo %s): real seasons have %s %s spells of %s than it expects (%s against %s).",
                name, p_text(r$p_mc), if (r$worst_obs > r$worst_exp) "more" else "fewer", state,
                r$worst_bin, pct(r$worst_obs), pct(r$worst_exp))
      }
      tail <- sprintf("Spells of %d days or more: %s observed, %s simulated (90%% range %s to %s).",
                      r$long, pct(r$long_obs, 1), pct(r$long_sim, 1), pct(r$long_band[1], 1),
                      pct(r$long_band[2], 1))
      div(class = "verdict", span(class = paste("key key-dot", state)), span(head, tail))
    }
    tagList(
      line(s$wet, "wet"), line(s$dry, "dry"),
      caption(sprintf("H3. %s simulated records of %s each, against the %s real ones. Spells touching a missing day are left out.",
                      num(s$n_sets, 0), plural(s$n_seasons, "season"), num(s$n_seasons, 0)))
    )
  })

  one_season <- eventReactive(list(input$resim, sel()), {
    d  <- sel()
    ch <- chain()
    pick <- sample(unique(d$season), 1)
    obs <- d[d$season == pick, c("date", "dos", "rain_mm")]
    obs$rain_mm[is.na(obs$rain_mm)] <- 0
    dos <- sort(unique(d$dos))
    wet <- simulate_states(ch$p01[["est"]], ch$p11[["est"]], length(dos), 1, ch$pi_wet)[1, ]
    sim <- data.frame(date = as.Date("2001-10-01") + dos - 1, dos = dos, rain_mm = 0)
    sim$rain_mm[wet] <- round(r_amount(sum(wet), best_fit), 1)
    list(obs = obs, sim = sim, year = pick)
  })

  output$season_plot <- renderGirafe({
    s <- one_season()
    draw("season_plot", plot_season(s$obs, s$sim, paste0(s$year, ", observed")), 330, 300)
  })

  output$season_caption <- renderUI({
    s <- one_season()
    caption(sprintf("The real season is drawn at random from your selection. Simulated wet days take their amounts from the %s fit on the Extremes page. %d wet days observed, %d simulated.",
                    DIST_NAMES[[fits$best_aic]], sum(s$obs$rain_mm >= WET_MM), sum(s$sim$rain_mm >= WET_MM)))
  })

  # Decades ----------------------------------------------------------------------
  output$lede_decades <- renderUI({
    p(sprintf("Across the three decades the chain shows no clear change (%s), and neither does the share of heavy-rain days (%s). With about 900 days per decade, only large shifts would show.",
              p_text(h4$p), p_text(h5$p)))
  })
  output$season_totals <- renderGirafe(draw("season_totals", plot_season_totals(audit$seasons), 300, 260))
  output$decade_chain  <- renderGirafe(draw("decade_chain", plot_decade_chain(h4$per_decade), 300, 270))
  output$heavy_share   <- renderGirafe(draw("heavy_share", plot_heavy_share(h5), 300, 270))

  output$h4_text <- renderUI({
    tagList(
      test_block("H4", "Has the chain changed across decades?",
                 if (h4$p < 0.05) "Yes, the transition counts differ between decades." else "No clear change. The intervals overlap in every decade.",
                 chi_text(h4$stat, h4$df, h4$p),
                 sprintf("From a dry day %s; from a wet day %s.", p_text(h4$from_dry$p), p_text(h4$from_wet$p))),
      caption("Chi-square test of homogeneity on the transition counts, October to December. Bars are 95% intervals.")
    )
  })

  output$h5_text <- renderUI({
    tagList(
      test_block("H5", "Has the heavy tail changed?",
                 if (h5$p < 0.05) "Yes, the share of heavy days differs between decades."
                 else sprintf("No clear change in days above %s mm, the full-record 95th percentile of wet days.", num(h5$q95, 0)),
                 chi_text(h5$stat, h5$df, h5$p),
                 sprintf("ANOVA on log wet-day amounts: F = %s, %s.", num(h5$anova_f, 2), p_text(h5$anova_p))),
      caption("Wet days only (2.5 mm or more). Bars are 95% intervals.")
    )
  })

  # Extremes ---------------------------------------------------------------------
  returns <- reactive(get_returns(input$fit))

  output$lede_extremes <- renderUI({
    r <- get_returns(fits$best_aic)
    r10 <- r[which.min(abs(r$T - 10)), ]
    top <- ond[which.max(ond$rain_mm), ]
    p(sprintf("A 10-year day at Nungambakkam brings about %s mm of rain (%s fit, 90%% range %s to %s mm). The wettest day in the record gave %s mm, on %s.",
              num(r10$level, 0), DIST_NAMES[[fits$best_aic]], num(r10$lo, 0), num(r10$hi, 0),
              num(top$rain_mm, 1), fmt_date(top$date)))
  })

  output$fit_table <- renderUI({
    t <- fits$table
    rows <- lapply(seq_len(nrow(t)), function(i) {
      tags$tr(class = if (t$dist[i] == input$fit) "best" else NULL,
              tags$td(t$name[i], tags$div(class = "sub", t$params[i])),
              tags$td(num(t$aic[i], 0)),
              tags$td(if (t$delta_aic[i] < 0.05) "lowest" else paste0("+", num(t$delta_aic[i], 1))),
              tags$td(num(t$tail_error[i], 3)))
    })
    tagList(
      tags$table(class = "tbl",
                 tags$thead(tags$tr(tags$th("Fit"), tags$th("AIC"), tags$th("ΔAIC"),
                                    tags$th("Tail error"))),
                 tags$tbody(rows)),
      p(class = "note", if (fits$best_aic == fits$best_tail) {
        sprintf("%s has the lowest AIC and also sits closest to the wettest 5%% of days (tail error: mean log distance between fitted and observed). %s wet days.",
                DIST_NAMES[[fits$best_aic]], num(fits$n, 0))
      } else {
        sprintf("%s has the lowest AIC, but %s sits closest to the wettest 5%% of days (tail error: mean log distance between fitted and observed). %s wet days.",
                DIST_NAMES[[fits$best_aic]], DIST_NAMES[[fits$best_tail]], num(fits$n, 0))
      })
    )
  })

  output$qq_legend <- renderUI({
    div(class = "legend",
        legend_item("key-dot wet", DIST_NAMES[[input$fit]]),
        legend_item("key-dot grey", paste(DIST_NAMES[names(DIST_NAMES) != input$fit], collapse = " and ")))
  })

  output$qq_plot <- renderGirafe(draw("qq_plot", plot_qq(fits, input$fit), 300, 280))

  output$return_plot <- renderGirafe({
    r <- returns()
    w <- ond[!is.na(ond$wet) & ond$wet, ]
    w <- w[order(-w$rain_mm), ][1:25, ]
    w$T <- record_years / seq_len(25)
    draw("return_plot", plot_returns(r, w, record_years, DIST_NAMES[[input$fit]]), 340, 300)
  })

  output$return_table <- renderUI({
    r <- returns()
    r <- r[r$T %in% RETURN_T, ]
    rows <- lapply(seq_len(nrow(r)), function(i) {
      tags$tr(class = if (r$T[i] > record_years) "extrap" else NULL,
              tags$td(paste(r$T[i], "years")),
              tags$td(paste(num(r$level[i], 0), "mm")),
              tags$td(paste(num(r$lo[i], 0), "to", num(r$hi[i], 0))))
    })
    tagList(
      tags$table(class = "tbl",
                 tags$thead(tags$tr(tags$th("Every"), tags$th("One day of"), tags$th("90% range"))),
                 tags$tbody(rows)),
      p(class = "note", sprintf("%s fit. The record has %d usable seasons, so the 50 and 100-year rows are extrapolations that lean on the shape of the fitted tail.",
                                DIST_NAMES[[input$fit]], n_used))
    )
  })

  # Decision -----------------------------------------------------------------------
  output$lede_decision <- renderUI({
    lo <- which.min(alert_tab$p_heavy)
    hi <- which.max(alert_tab$p_heavy)
    p(sprintf("Heavy rain here means a day above %s mm, the wettest 5%% of wet days. The chance of one tomorrow runs from %s %s to %s %s.",
              num(h5$q95, 0), pct(alert_tab$p_heavy[lo], 1), alert_tab$after[lo],
              pct(alert_tab$p_heavy[hi], 1), alert_tab$after[hi]))
  })

  output$alert_plot <- renderGirafe(draw("alert_plot", plot_alert(alert_tab, input$cl), 300, 260))

  output$alert_caption <- renderUI({
    caption(sprintf("Bars: chance of a wet day from the chain%s, times the %s chance that a wet day passes %s mm (%s fit). Black ticks: how often it happened in the record. Line: C/L.",
                    if (use_second_order) " (second order, since H2 rejects first order)" else "",
                    pct(alert_tab$p_heavy_if_wet[1], 1), num(h5$q95, 0), DIST_NAMES[[fits$best_aic]]))
  })

  output$alert_rule <- renderUI({
    on <- alert_tab$p_heavy > input$cl
    rule <- if (!any(on)) {
      "At this ratio no state calls for an alert: acting costs too much for how rare heavy rain is."
    } else if (all(on)) {
      "At this ratio every state calls for an alert: acting is cheap next to the loss."
    } else {
      paste0("Issue an alert ", join_and(alert_tab$after[on]), ".")
    }
    thresholds <- lapply(seq_len(nrow(alert_tab)), function(i) {
      tags$tr(class = if (on[i]) "on" else NULL,
              tags$td(alert_tab$state[i]), tags$td(paste("C/L below", num(alert_tab$p_heavy[i], 3))))
    })
    tagList(
      p(class = "rule", rule),
      tags$table(class = "tbl compact", tags$tbody(thresholds)),
      p(class = "note", "C and L are assumptions, not measured costs. This is a framework, not a calibrated warning system.")
    )
  })

  buffer_k <- reactive(list(
    chain = buffer_days(dry_max$curve, "chain", input$risk),
    observed = buffer_days(dry_max$curve, "observed", input$risk)
  ))

  output$buffer_plot <- renderGirafe({
    draw("buffer_plot", plot_buffer(dry_max, input$risk, buffer_k()$chain), 300, 260)
  })

  output$buffer_rule <- renderUI({
    k <- buffer_k()
    dry_fit <- spells_all$dry$p_mc >= 0.05
    levels <- c(0.05, 0.1, 0.2, 0.5)
    rows <- lapply(levels, function(r) {
      tags$tr(tags$td(paste(pct(r), "risk")),
              tags$td(plural(buffer_days(dry_max$curve, "chain", r), "day")),
              tags$td(plural(buffer_days(dry_max$curve, "observed", r), "day")))
    })
    tagList(
      p(class = "rule", sprintf("Hold %s of supply to keep the chance of a longer dry spell at or below %s in a season.",
                                plural(k$chain, "day"), pct(input$risk))),
      tags$table(class = "tbl compact",
                 tags$thead(tags$tr(tags$th("Risk"), tags$th("Chain"), tags$th("Record"))),
                 tags$tbody(rows)),
      p(class = "note", sprintf("The %d real seasons alone give %s at your risk level. %s", n_used,
                                plural(k$observed, "day"),
                                if (dry_fit) "The chain matches observed dry spells (H3), so the simulated figure can be used."
                                else "The chain does not match observed dry spells (H3), so read the simulated figure next to the record."))
    )
  })

  # Data -------------------------------------------------------------------------
  output$calendar <- renderGirafe(draw("calendar", plot_calendar(days), 440, 420))

  output$audit_rules <- renderUI({
    f <- audit$facts
    div(class = "specs",
        spec_item("Station", "Nungambakkam, the IMD city observatory and the only gauge covering 1991 to 2023"),
        spec_item("Season", "1 October to 31 December, 92 days"),
        spec_item("Wet day", "2.5 mm of rain or more, the IMD threshold"),
        spec_item("Gaps", "No interpolation. Pairs of days that include a missing day are left out"),
        spec_item("Seasons", sprintf("%s of %s used. More than 9 missing days drops a season: %s",
                                     f$seasons_used, f$seasons_total, f$seasons_dropped)))
  })

  output$audit_found <- renderUI({
    f <- audit$facts
    miss <- tapply(is.na(days$rain_mm), days$decade, sum)
    items <- list(
      sprintf("The Chennai file starts on %s, not in 1991. The Tamil Nadu file in the same dataset has Nungambakkam from %s and matches the Chennai file on all %s shared days, so it is used.",
              fmt_date(as.Date(f$city_file_first_day)), fmt_date(as.Date(f$first_listed_day)),
              num(as.numeric(f$city_file_days_compared), 0)),
      sprintf("No zeros in %s rows: only rainy days are listed. A day that is not listed is read as dry.",
              num(as.numeric(f$tn_rows_chennai), 0)),
      sprintf("%s exact duplicate rows removed. %s days carry two different values and are set to missing.",
              num(as.numeric(f$tn_exact_duplicates), 0), f$conflict_days),
      sprintf("Three months are silent at every Chennai gauge: %s. They are set to missing. ERA5 has 17 wet days in October 1996, so that month is clearly a gap.",
              f$missing_months),
      sprintf("%s more days are not listed although two or more nearby gauges had 10 mm or more. They are set to missing.",
              f$missing_days_neighbours),
      sprintf("Missing days by decade: %s.", paste(names(miss), miss, sep = ": ", collapse = "; ")),
      sprintf("No sign of filled-in values: the same amount never repeats for more than %s days running. %s repeats Nungambakkam's value on %s of shared days, so it is not an independent check.",
              f$longest_repeat_run_days, f$most_copied_station, pct(as.numeric(f$most_copied_share))),
      sprintf("Against ERA5 (Open-Meteo, rain days from 08:30 to 08:30 IST), wet and dry days agree on %s of days. The match is best on the same date (r = %s, against %s and %s a day off), so the dates line up.",
              pct(as.numeric(f$era5_agreement), 1), f$era5_cor_same_day, f$era5_cor_day_before, f$era5_cor_day_after),
      sprintf("No negative values. The wettest October to December day is %s mm, on %s.",
              num(as.numeric(f$max_ond_mm), 1), fmt_date(as.Date(f$max_ond_date)))
    )
    tags$ol(class = "audit", lapply(items, tags$li))
  })

  output$download <- downloadHandler(
    filename = "nungambakkam_oct_dec_1991_2023.csv",
    content = function(file) file.copy("data/nungambakkam_ond.csv", file)
  )
}

shinyApp(ui, server)

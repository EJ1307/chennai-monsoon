// Page logic. Results come from R (data/results.json); anything not in there is
// calculated in the browser with Stats (js/stats.js).

(() => {
  const $ = (id) => document.getElementById(id);
  const esc = (s) => String(s).replace(/[&<>"]/g, (c) => ({ "&": "&amp;", "<": "&lt;", ">": "&gt;", '"': "&quot;" }[c]));
  const chi = (stat, df, p) => `&chi;&sup2; = ${Fmt.num(stat, 1)}, ${df} df, ${Fmt.pText(p)}`;
  const test = (tag, q, a, stats, note) => `<div class="test"><div class="test-q"><span class="test-tag">${tag}</span>${q}</div>
    <p class="test-a">${a}</p><p class="test-s">${stats}</p>${note ? `<p class="test-s">${note}</p>` : ""}</div>`;

  let R = null;          // results from R
  let DAYS = null;       // daily series (browser calculations, calendar)
  const browserCache = {};
  const state = { months: "Oct to Dec", decade: "All years", fit: null, cl: 0.02, risk: 0.1 };
  const rand = Stats.rng((Date.now() ^ (Math.random() * 1e9)) >>> 0);

  // A month and decade result: from R when it exists, otherwise worked out here
  function selection(months, decade) {
    const key = `${months} / ${decade}`;
    if (R.selections[key]) return R.selections[key];
    if (!browserCache[key]) browserCache[key] = Stats.computeSelection(DAYS, months, decade);
    return browserCache[key];
  }

  // Charts redraw at the width of their box; remember what is on screen. A chart marked
  // "fill" also grows to the height its card has next to a taller card in the same row.
  const drawn = {};
  function draw(id, make) {
    const el = $(id);
    const w = Math.max(240, Math.floor(el.clientWidth));
    el.replaceChildren(make(w));
    if (el.classList.contains("fill")) {
      const svg = el.querySelector("svg");
      const used = svg ? Math.ceil(svg.getBoundingClientRect().height) : 0;
      const avail = Math.floor(el.clientHeight);
      if (avail - used > 8) el.replaceChildren(make(w, avail));
    }
    drawn[id] = { make, w };
  }
  let resizeTimer = null;
  window.addEventListener("resize", () => {
    clearTimeout(resizeTimer);
    resizeTimer = setTimeout(() => {
      for (const [id, c] of Object.entries(drawn)) {
        const el = $(id);
        if (!el.offsetParent) continue;
        if (Math.abs(Math.floor(el.clientWidth) - c.w) > 4) draw(id, c.make);
      }
    }, 150);
  });

  // Wet and dry days ---------------------------------------------------------------------
  function renderWet() {
    const s = selection(state.months, state.decade);
    const years = state.decade === "All years" ? "1991 to 2023" : state.decade;
    const months = state.months === "Oct to Dec" ? "October to December" : state.months;
    $("lede-wet").textContent = `${months}, ${years} (${Fmt.plural(s.seasons.length, "season")}). After a wet day the next day is wet ${Fmt.pct(s.p11[0])} of the time; after a dry day, ${Fmt.pct(s.p01[0])}.`;

    const tile = (est, lo, hi, kind) => `<div class="tile ${kind}" style="--a:${est.toFixed(3)}"><div class="tile-p">${Fmt.num(est)}</div><div class="tile-ci">${Fmt.num(lo)} to ${Fmt.num(hi)}</div></div>`;
    const [a, alo, ahi] = s.p01, [b, blo, bhi] = s.p11;
    $("matrix").innerHTML = `<div class="matrix">
      <div></div><div class="m-head">Dry today</div><div class="m-head">Wet today</div>
      <div class="m-row">Dry yesterday</div>${tile(1 - a, 1 - ahi, 1 - alo, "dry")}${tile(a, alo, ahi, "wet")}
      <div class="m-row">Wet yesterday</div>${tile(1 - b, 1 - bhi, 1 - blo, "dry")}${tile(b, blo, bhi, "wet")}
    </div>
    <p class="caption">From ${Fmt.num(s.n_pairs, 0)} pairs of days with data. Ranges are 95% intervals. Wet: 2.5 mm or more.</p>
    ${s.computed_in_browser ? '<p class="browser-note">This choice was calculated in your browser.</p>' : ""}`;

    const h1 = s.h1, h2 = s.h2, pw = h2.p_wet;
    const small = Math.min(h1.min_expected, h2.min_expected) < 5 ? "Some expected counts are under 5: read with care." : null;
    $("chain-facts").innerHTML = `<div class="facts">
        <div><div class="fact-v">${Fmt.pct(s.pi_wet)}</div><div class="fact-l">of days wet in the long run</div></div>
        <div><div class="fact-v">${Fmt.num(s.mean_wet_spell, 1)} days</div><div class="fact-l">typical wet spell</div></div>
        <div><div class="fact-v">${Fmt.num(s.mean_dry_spell, 1)} days</div><div class="fact-l">typical dry spell</div></div>
      </div>
      ${test("H1", "Does today depend on yesterday?",
        h1.p < 0.05 ? `Yes. A wet yesterday raises the chance of rain today by ${Fmt.num(100 * h1.diff, 0)} points.` : "Not detectably in this selection.",
        chi(h1.stat, h1.df, h1.p))}
      ${test("H2", "Is one day of memory enough?",
        h2.p < 0.05 ? `No. The day before yesterday still matters: after two dry days rain follows ${Fmt.pct(pw.DD)} of the time, after a wet day and then a dry one ${Fmt.pct(pw.WD)}.`
          : "Yes. Knowing the day before yesterday does not change the odds in a way the data can detect.",
        chi(h2.stat, h2.df, h2.p), small)}`;

    draw("chart-spell-wet", (w) => Charts.spells(w, s.h3.wet, "wet"));
    draw("chart-spell-dry", (w) => Charts.spells(w, s.h3.dry, "dry"));
    const line = (r, st) => {
      const name = Fmt.title(st);
      const head = r.p_mc >= 0.05 ? `${name} spells match the chain (Monte Carlo ${Fmt.pText(r.p_mc)}).`
        : `${name} spells do not match the chain (Monte Carlo ${Fmt.pText(r.p_mc)}): real seasons have ${r.worst_obs > r.worst_exp ? "more" : "fewer"} ${st} spells of ${r.worst_bin} than it expects (${Fmt.pct(r.worst_obs)} against ${Fmt.pct(r.worst_exp)}).`;
      const tail = `Spells of ${r.long} days or more: ${Fmt.pct(r.long_obs, 1)} observed, ${Fmt.pct(r.long_sim, 1)} simulated (90% range ${Fmt.pct(r.long_band[0], 1)} to ${Fmt.pct(r.long_band[1], 1)}).`;
      return `<div class="verdict"><span class="key key-dot ${st}"></span><span>${head} ${tail}</span></div>`;
    };
    const h3 = s.h3;
    $("spell-verdict").innerHTML = line(h3.wet, "wet") + line(h3.dry, "dry") +
      `<p class="caption">H3. ${Fmt.num(h3.n_sets, 0)} simulated records of ${Fmt.plural(h3.n_seasons, "season")} each, against the ${h3.n_seasons} real ones. Spells touching a missing day are left out.</p>`;
    renderSeason();
  }

  // One real season next to one simulated in the browser from the chain
  function renderSeason() {
    const s = selection(state.months, state.decade);
    const year = s.seasons[Math.floor(rand() * s.seasons.length)];
    const dosSet = new Set(s.dos);
    const obs = DAYS.filter((d) => d.season === year && dosSet.has(d.dos)).map((d) => ({ dos: d.dos, rain: d.rain === null ? 0 : d.rain }));
    const best = R.extremes.best_aic;
    const sim = Stats.simulateSeason(s, best, R.extremes.params[best], R.extremes.cut, rand);
    draw("chart-season", (w) => Charts.season(w, obs, sim, `${year}, observed`));
    const wetCount = (x) => x.filter((r) => r.rain >= 2.5).length;
    $("season-caption").textContent = `The real season is drawn at random from your selection. The simulated one is made in your browser from the chain, with wet-day amounts from the ${fitName(best)} fit on the Extremes page. ${wetCount(obs)} wet days observed, ${wetCount(sim)} simulated.`;
  }

  // Decades ---------------------------------------------------------------------------
  function renderDecades() {
    const { h4, h5, seasons } = R.decades;
    const change4 = h4.p < 0.05, change5 = h5.p < 0.05;
    $("lede-decades").textContent = !change4 && !change5
      ? `Across the three decades the chain shows no clear change (${Fmt.pText(h4.p)}), and neither does the share of heavy-rain days (${Fmt.pText(h5.p)}). With about 900 days per decade, only large shifts would show.`
      : `The chain ${change4 ? "changes" : "shows no clear change"} across the three decades (${Fmt.pText(h4.p)}), and the share of heavy-rain days ${change5 ? "changes" : "shows no clear change"} (${Fmt.pText(h5.p)}).`;
    draw("chart-totals", (w) => Charts.totals(w, seasons, R.meta.decades));
    draw("chart-decade-chain", (w) => Charts.decadeChain(w, h4.per_decade, R.meta.decades));
    draw("chart-heavy", (w) => Charts.heavyShare(w, h5, R.meta.decades));
    $("h4-text").innerHTML = test("H4", "Has the chain changed across decades?",
      change4 ? "Yes, the transition counts differ between decades." : "No clear change. The intervals overlap in every decade.",
      chi(h4.stat, h4.df, h4.p), `From a dry day ${Fmt.pText(h4.p_from_dry)}; from a wet day ${Fmt.pText(h4.p_from_wet)}.`) +
      '<p class="caption">Chi-square test of homogeneity on the transition counts, October to December. Bars are 95% intervals.</p>';
    $("h5-text").innerHTML = test("H5", "Has the heavy tail changed?",
      change5 ? "Yes, the share of heavy days differs between decades." : `No clear change in days above ${Fmt.num(h5.q95, 0)} mm, the full-record 95th percentile of wet days.`,
      chi(h5.stat, h5.df, h5.p), `ANOVA on log wet-day amounts: F = ${Fmt.num(h5.anova_f)}, ${Fmt.pText(h5.anova_p)}.`) +
      '<p class="caption">Wet days only (2.5 mm or more). Bars are 95% intervals.</p>';
  }

  // Extremes --------------------------------------------------------------------------
  const fitName = (dist) => R.extremes.table.find((t) => t.dist === dist).name;
  function renderExtremes() {
    const E = R.extremes;
    const best = E.returns[E.best_aic];
    const r10 = best.reduce((a, r) => (Math.abs(r.T - 10) < Math.abs(a.T - 10) ? r : a));
    const wettest = E.top_days[0];
    $("lede-extremes").textContent = `A 10-year day at Nungambakkam brings about ${Fmt.num(r10.level, 0)} mm of rain (${fitName(E.best_aic)} fit, 90% range ${Fmt.num(r10.lo, 0)} to ${Fmt.num(r10.hi, 0)} mm). The wettest day in the record gave ${Fmt.num(wettest.rain_mm, 1)} mm, on ${Fmt.date(wettest.date)}.`;
    renderFit();
  }
  function renderFit() {
    const E = R.extremes, f = state.fit;
    const rows = E.table.map((t) => `<tr class="${t.dist === f ? "best" : ""}"><td>${t.name}<div class="sub">${esc(t.params)}</div></td>
      <td>${Fmt.num(t.aic, 0)}</td><td>${t.delta_aic < 0.05 ? "lowest" : "+" + Fmt.num(t.delta_aic, 1)}</td><td>${Fmt.num(t.tail_error, 3)}</td></tr>`).join("");
    const note = E.best_aic === E.best_tail
      ? `${fitName(E.best_aic)} has the lowest AIC and also sits closest to the wettest 5% of days (tail error: mean log distance between fitted and observed). ${Fmt.num(E.n, 0)} wet days.`
      : `${fitName(E.best_aic)} has the lowest AIC, but ${fitName(E.best_tail)} sits closest to the wettest 5% of days (tail error: mean log distance between fitted and observed). ${Fmt.num(E.n, 0)} wet days.`;
    $("fit-table").innerHTML = `<table class="tbl"><thead><tr><th>Fit</th><th>AIC</th><th>&Delta;AIC</th><th>Tail error</th></tr></thead><tbody>${rows}</tbody></table><p class="note">${note}</p>`;
    const names = Object.fromEntries(E.table.map((t) => [t.dist, t.name]));
    $("qq-legend").innerHTML = `<span class="li"><span class="key key-dot wet"></span>${fitName(f)}</span>
      <span class="li"><span class="key key-dot grey"></span>${E.table.filter((t) => t.dist !== f).map((t) => t.name).join(" and ")}</span>`;
    draw("chart-qq", (w) => Charts.qq(w, E.qq, names, f));
    const curve = E.returns[f];
    draw("chart-returns", (w) => Charts.returns(w, curve, E.top_days, R.meta.record_years, fitName(f)));
    const trs = curve.filter((r) => E.return_t.some((t) => Math.abs(t - r.T) < 1e-6)).map((r) =>
      `<tr class="${r.T > R.meta.record_years ? "extrap" : ""}"><td>${Fmt.num(r.T, 0)} years</td><td>${Fmt.num(r.level, 0)} mm</td><td>${Fmt.num(r.lo, 0)} to ${Fmt.num(r.hi, 0)}</td></tr>`).join("");
    $("return-table").innerHTML = `<table class="tbl"><thead><tr><th>Every</th><th>One day of</th><th>90% range</th></tr></thead><tbody>${trs}</tbody></table>
      <p class="note">${fitName(f)} fit. The record has ${R.meta.n_used} usable seasons, so the 50 and 100-year rows are extrapolations that lean on the shape of the fitted tail.</p>`;
  }

  // Decision --------------------------------------------------------------------------
  function renderDecision() {
    const D = R.decision, st = D.states;
    const lo = st.reduce((a, s) => (s.p_heavy < a.p_heavy ? s : a)), hi = st.reduce((a, s) => (s.p_heavy > a.p_heavy ? s : a));
    $("lede-decision").textContent = `Heavy rain here means a day above ${Fmt.num(D.heavy_mm, 0)} mm, the wettest 5% of wet days. The chance of one tomorrow runs from ${Fmt.pct(lo.p_heavy, 1)} ${lo.after} to ${Fmt.pct(hi.p_heavy, 1)} ${hi.after}.`;
    $("alert-caption").textContent = `Bars: chance of a wet day from the chain${D.second_order ? " (second order, since H2 rejects first order)" : ""}, times the ${Fmt.pct(D.p_heavy_if_wet, 1)} chance that a wet day passes ${Fmt.num(D.heavy_mm, 0)} mm (${fitName(R.extremes.best_aic)} fit). Black ticks: how often it happened in the record. Line: C/L.`;
    $("buffer-caption").textContent = `Longest run of dry days in a season. Grey line: 20,000 seasons simulated from the chain. Orange steps: the ${R.meta.n_used} real seasons.`;
    renderAlert();
    renderBuffer();
  }
  // Worked out in the browser for whatever ratio the slider is on
  function renderAlert() {
    const D = R.decision, cl = state.cl;
    $("cl-out").textContent = Fmt.num(cl, 3);
    const on = D.states.filter((s) => s.p_heavy > cl);
    const rule = !on.length ? "At this ratio no state calls for an alert: acting costs too much for how rare heavy rain is."
      : on.length === D.states.length ? "At this ratio every state calls for an alert: acting is cheap next to the loss."
        : `Issue an alert ${Fmt.joinAnd(on.map((s) => s.after))}.`;
    $("alert-rule").innerHTML = `<p class="rule">${rule}</p><table class="tbl compact"><tbody>${D.states.map((s) =>
      `<tr class="${s.p_heavy > cl ? "on" : ""}"><td>${s.state}</td><td>C/L below ${Fmt.num(s.p_heavy, 3)}</td></tr>`).join("")}</tbody></table>
      <p class="note">C and L are assumptions, not measured costs. This is a framework, not a calibrated warning system.</p>`;
    // the chart last, so it can grow to the height the rule card gives the row
    draw("chart-alert", (w, h) => Charts.alert(w, D.states, cl, h));
  }
  const bufferDays = (curve, col, risk) => Math.min(...curve.filter((r) => r[col] <= risk + 1e-12).map((r) => r.k));
  function renderBuffer() {
    const D = R.decision, risk = state.risk;
    $("risk-out").textContent = Fmt.pct(risk);
    const kc = bufferDays(D.dry_max, "chain", risk), ko = bufferDays(D.dry_max, "observed", risk);
    const rows = [0.05, 0.1, 0.2, 0.5].map((r) => `<tr><td>${Fmt.pct(r)} risk</td><td>${Fmt.plural(bufferDays(D.dry_max, "chain", r), "day")}</td><td>${Fmt.plural(bufferDays(D.dry_max, "observed", r), "day")}</td></tr>`).join("");
    $("buffer-rule").innerHTML = `<p class="rule">Hold ${Fmt.plural(kc, "day")} of supply to keep the chance of a longer dry spell at or below ${Fmt.pct(risk)} in a season.</p>
      <table class="tbl compact"><thead><tr><th>Risk</th><th>Chain</th><th>Record</th></tr></thead><tbody>${rows}</tbody></table>
      <p class="note">The ${R.meta.n_used} real seasons alone give ${Fmt.plural(ko, "day")} at your risk level. ${D.dry_fit
        ? "The chain matches observed dry spells (H3), so the simulated figure can be used."
        : "The chain does not match observed dry spells (H3), so read the simulated figure next to the record."}</p>`;
    draw("chart-buffer", (w, h) => Charts.buffer(w, D.dry_max, risk, kc, h));
  }

  // Data ------------------------------------------------------------------------------
  function renderData() {
    const f = R.audit.facts;
    const spec = (l, v) => `<div class="spec"><div class="spec-l">${l}</div><div class="spec-v">${v}</div></div>`;
    $("audit-rules").innerHTML = `<div class="specs">
      ${spec("Station", "Nungambakkam, the IMD city observatory and the only gauge covering 1991 to 2023")}
      ${spec("Season", "1 October to 31 December, 92 days")}
      ${spec("Wet day", "2.5 mm of rain or more, the IMD threshold")}
      ${spec("Gaps", "No interpolation. Pairs of days that include a missing day are left out")}
      ${spec("Seasons", `${f.seasons_used} of ${f.seasons_total} used. More than 9 missing days drops a season: ${f.seasons_dropped}`)}</div>`;
    draw("chart-calendar", (w) => Charts.calendar(w, DAYS, DAYS_LABELS));
    const miss = Object.entries(R.audit.missing_by_decade).map(([k, v]) => `${k}: ${v}`).join("; ");
    const items = [
      `The Chennai file starts on ${Fmt.date(f.city_file_first_day)}, not in 1991. The Tamil Nadu file in the same dataset has Nungambakkam from ${Fmt.date(f.first_listed_day)} and matches the Chennai file on all ${Fmt.num(+f.city_file_days_compared, 0)} shared days, so it is used.`,
      `No zeros in ${Fmt.num(+f.tn_rows_chennai, 0)} rows: only rainy days are listed. A day that is not listed is read as dry.`,
      `${Fmt.num(+f.tn_exact_duplicates, 0)} exact duplicate rows removed. ${f.conflict_days} days carry two different values and are set to missing.`,
      `Three months are silent at every Chennai gauge: ${f.missing_months}. They are set to missing. ERA5 has 17 wet days in October 1996, so that month is clearly a gap.`,
      `${f.missing_days_neighbours} more days are not listed although two or more nearby gauges had 10 mm or more. They are set to missing.`,
      `Missing days by decade: ${miss}.`,
      `No sign of filled-in values: the same amount never repeats for more than ${f.longest_repeat_run_days} days running. ${f.most_copied_station} repeats Nungambakkam's value on ${Fmt.pct(+f.most_copied_share)} of shared days, so it is not an independent check.`,
      `Against ERA5 (Open-Meteo, rain days from 08:30 to 08:30 IST), wet and dry days agree on ${Fmt.pct(+f.era5_agreement, 1)} of days. The match is best on the same date (r = ${f.era5_cor_same_day}, against ${f.era5_cor_day_before} and ${f.era5_cor_day_after} a day off), so the dates line up.`,
      `No negative values. The wettest October to December day is ${Fmt.num(+f.max_ond_mm, 1)} mm, on ${Fmt.date(f.max_ond_date)}.`,
    ];
    $("audit-found").innerHTML = `<ol class="audit">${items.map((t) => `<li>${t}</li>`).join("")}</ol>`;
  }
  let DAYS_LABELS = [];

  // Pages -----------------------------------------------------------------------------
  const PAGES = {
    "wet-and-dry-days": { id: "page-wet", render: renderWet },
    decades: { id: "page-decades", render: renderDecades },
    extremes: { id: "page-extremes", render: renderExtremes },
    decision: { id: "page-decision", render: renderDecision },
    data: { id: "page-data", render: renderData },
  };
  const rendered = new Set();
  function show() {
    const name = PAGES[location.hash.slice(1)] ? location.hash.slice(1) : "wet-and-dry-days";
    for (const [key, p] of Object.entries(PAGES)) $(p.id).hidden = key !== name;
    document.querySelectorAll(".nav a").forEach((a) => {
      if (a.getAttribute("href") === "#" + name) a.setAttribute("aria-current", "page");
      else a.removeAttribute("aria-current");
    });
    closeMenu();
    if (!rendered.has(name)) { PAGES[name].render(); rendered.add(name); }
    else {
      // a page drawn while the window had another size: redraw its charts
      for (const [id, c] of Object.entries(drawn)) {
        const el = $(id);
        if (el.offsetParent && Math.abs(Math.floor(el.clientWidth) - c.w) > 4) draw(id, c.make);
      }
    }
  }
  const menuBtn = $("menu-btn"), nav = $("nav");
  function closeMenu() { nav.classList.remove("open"); menuBtn.setAttribute("aria-expanded", "false"); }
  menuBtn.addEventListener("click", () => {
    const open = !nav.classList.contains("open");
    nav.classList.toggle("open", open);
    menuBtn.setAttribute("aria-expanded", String(open));
  });

  // Controls --------------------------------------------------------------------------
  function radios(container, name, values, current, onChange) {
    const box = document.createElement("div");
    box.className = "options";
    for (const v of values) {
      const label = document.createElement("label");
      const input = Object.assign(document.createElement("input"), { type: "radio", name, value: v, checked: v === current });
      input.addEventListener("change", () => onChange(v));
      label.append(input, document.createTextNode(typeof v === "string" ? v : String(v)));
      box.append(label);
    }
    container.append(box);
  }
  function segmented(container, name, options, current, onChange) {
    for (const [value, text] of options) {
      const label = document.createElement("label");
      const input = Object.assign(document.createElement("input"), { type: "radio", name, value, checked: value === current });
      input.addEventListener("change", () => onChange(value));
      label.append(input, document.createTextNode(text));
      container.append(label);
    }
  }

  // Start -----------------------------------------------------------------------------
  const started = performance.now();
  function done() {
    const wait = Math.max(0, 700 - (performance.now() - started));
    setTimeout(() => $("loading").classList.add("done"), wait);
  }
  const fontsReady = document.fonts ? Promise.race([document.fonts.ready, new Promise((r) => setTimeout(r, 1500))]) : Promise.resolve();
  Promise.all([
    fetch("data/results.json").then((r) => r.json()),
    fetch("data/days.json").then((r) => r.json()),
    fontsReady,
  ]).then(([results, days]) => {
    R = results;
    DAYS = Stats.prepareDays(days);
    DAYS_LABELS = days.status_labels;
    state.fit = R.extremes.best_aic;

    radios($("pick-months"), "months", R.meta.months, state.months, (v) => { state.months = v; renderWet(); });
    radios($("pick-decade"), "decade", ["All years", ...R.meta.decades], state.decade, (v) => { state.decade = v; renderWet(); });
    segmented($("pick-fit"), "fit", R.extremes.table.map((t) => [t.dist, t.name]), state.fit, (v) => { state.fit = v; renderFit(); });
    $("resim").addEventListener("click", renderSeason);
    $("cl").addEventListener("input", (e) => { state.cl = +e.target.value; renderAlert(); });
    $("risk").addEventListener("input", (e) => { state.risk = +e.target.value; renderBuffer(); });
    $("download").href = "data/nungambakkam_oct_dec_1991_2023.csv";

    window.addEventListener("hashchange", show);
    show();
    done();
  }).catch((err) => {
    console.error(err);
    $("loading-note").textContent = "The results could not be loaded. Please reload the page.";
  });
})();

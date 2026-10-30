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
  const state = { months: "Oct to Dec", decade: "All years", fit: null, cl: 0.02, risk: 0.1, day: "2027-10-15", history: "any", cut: 0.3 };
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
    $("lede-wet").textContent = `${months}, ${years}. After a wet day, the next is wet ${Fmt.pct(s.p11[0])} of the time. After a dry day, ${Fmt.pct(s.p01[0])}.`;

    const tile = (est, lo, hi, kind) => `<div class="tile ${kind}" style="--a:${est.toFixed(3)}"><div class="tile-p">${Fmt.num(est)}</div><div class="tile-ci">${Fmt.num(lo)} to ${Fmt.num(hi)}</div></div>`;
    const [a, alo, ahi] = s.p01, [b, blo, bhi] = s.p11;
    $("matrix").innerHTML = `<div class="matrix">
      <div></div><div class="m-head">Dry today</div><div class="m-head">Wet today</div>
      <div class="m-row">Dry yesterday</div>${tile(1 - a, 1 - ahi, 1 - alo, "dry")}${tile(a, alo, ahi, "wet")}
      <div class="m-row">Wet yesterday</div>${tile(1 - b, 1 - bhi, 1 - blo, "dry")}${tile(b, blo, bhi, "wet")}
    </div>
    <p class="caption">${Fmt.num(s.n_pairs, 0)} pairs of days, 95% ranges.</p>
    ${s.computed_in_browser ? '<p class="browser-note">Calculated in your browser.</p>' : ""}`;

    const h1 = s.h1, h2 = s.h2, pw = h2.p_wet;
    const small = Math.min(h1.min_expected, h2.min_expected) < 5 ? "Some expected counts are under 5." : null;
    $("chain-facts").innerHTML = `<div class="facts">
        <div><div class="fact-v">${Fmt.pct(s.pi_wet)}</div><div class="fact-l">of days wet in the long run</div></div>
        <div><div class="fact-v">${Fmt.num(s.mean_wet_spell, 1)} days</div><div class="fact-l">typical wet spell</div></div>
        <div><div class="fact-v">${Fmt.num(s.mean_dry_spell, 1)} days</div><div class="fact-l">typical dry spell</div></div>
      </div>
      ${test("H1", "Does today depend on yesterday?",
        h1.p < 0.05 ? `Yes. A wet yesterday adds ${Fmt.num(100 * h1.diff, 0)} points.` : "Not detectably here.",
        chi(h1.stat, h1.df, h1.p))}
      ${test("H2", "Is one day of memory enough?",
        h2.p < 0.05 ? `No. Rain follows two dry days ${Fmt.pct(pw.DD)} of the time, wet then dry ${Fmt.pct(pw.WD)}.`
          : "Yes. The day before yesterday adds nothing.",
        chi(h2.stat, h2.df, h2.p), small)}`;

    draw("chart-spell-wet", (w) => Charts.spells(w, s.h3.wet, "wet"));
    draw("chart-spell-dry", (w) => Charts.spells(w, s.h3.dry, "dry"));
    const line = (r, st) => {
      const name = Fmt.title(st);
      const head = r.p_mc >= 0.05 ? `${name} spells fit (${Fmt.pText(r.p_mc)}).`
        : `${name} spells do not fit (${Fmt.pText(r.p_mc)}). ${Fmt.title(r.worst_obs > r.worst_exp ? "more" : "fewer")} spells of ${r.worst_bin} than expected.`;
      const tail = `${r.long}+ days: ${Fmt.pct(r.long_obs, 1)} real, ${Fmt.pct(r.long_sim, 1)} simulated.`;
      return `<div class="verdict"><span class="key key-dot ${st}"></span><span>${head} ${tail}</span></div>`;
    };
    const h3 = s.h3;
    $("spell-verdict").innerHTML = line(h3.wet, "wet") + line(h3.dry, "dry") +
      `<p class="caption">H3, Monte Carlo: ${Fmt.num(h3.n_sets, 0)} simulated sets of ${h3.n_seasons} seasons.</p>`;
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
    $("season-caption").textContent = `Wet days: ${wetCount(obs)} real, ${wetCount(sim)} simulated.`;
  }

  // Predict ---------------------------------------------------------------------------
  // Everything here comes from R; the page only looks up the chosen day.
  const MONTHS_FULL = ["January", "February", "March", "April", "May", "June", "July", "August", "September", "October", "November", "December"];
  const WEEKDAYS = ["Sun", "Mon", "Tue", "Wed", "Thu", "Fri", "Sat"];
  const dayMonth = (t) => `${t.getUTCDate()} ${MONTHS_FULL[t.getUTCMonth()]}`;
  let FC = null;   // forecasts by history, each 92 rows in day order
  function pickDay() {
    const m = /^(\d{4})-(\d{2})-(\d{2})$/.exec(state.day || "");
    if (!m || +m[2] < 10) return null;
    const year = +m[1], t = new Date(Date.UTC(year, +m[2] - 1, +m[3]));
    return { t, year, dos: Math.round((t - Date.UTC(year, 9, 1)) / 86400000) + 1 };
  }
  function renderPredict() {
    const P = R.predict, any = FC.any, peak = any[P.peak_dos - 1];
    $("lede-predict").textContent = `The chance of a wet day peaks at ${Fmt.pct(peak.p)} around ${dayMonth(new Date(Date.UTC(2027, 9, P.peak_dos)))}. Knowing the last two days moves it most.`;
    $("forecast-caption").textContent = `Logistic regression, ${P.k} harmonics, 95% band.`;
    renderDay();
    renderChecks();
    const tr = P.models.map((m) => `<tr class="${m.key === "season" || m.key === P.best_chain ? "best" : ""}"><td>${m.name}<div class="sub">${m.rhs}</div></td>
      <td>${m.df}</td><td>${Fmt.num(m.aic, 0)}</td><td>${m.delta_aic < 0.05 ? "lowest" : "+" + Fmt.num(m.delta_aic, 1)}</td><td>${m.lrt_p === null ? "" : Fmt.pText(m.lrt_p)}</td></tr>`).join("");
    $("model-table").innerHTML = `<table class="tbl"><thead><tr><th>Model</th><th>Terms</th><th>AIC</th><th>&Delta;AIC</th><th>Against the row above</th></tr></thead><tbody>${tr}</tbody></table>
      <p class="note">Fitted to ${Fmt.num(P.n_rows, 0)} days. s, c: ${P.k} sine and cosine pairs. yest, yest2: the last two days. Adding a trend: ${Fmt.pText(P.trend_season.p)}.</p>`;
  }
  // The forecast, its chart and the season outlook for the chosen day and history
  function renderDay() {
    const P = R.predict, k = pickDay(), h = state.history;
    const hist = h === "any" ? null : FC[h];
    $("forecast-legend").innerHTML = `<span class="li"><span class="key key-line wet"></span>Model</span>
      ${hist ? `<span class="li"><span class="key key-dash"></span>After ${P.after[h]}</span>` : ""}
      <span class="li"><span class="key key-dot grey"></span>Real seasons</span>`;
    renderOutlook(k ? k.year : 2027);
    // the chart last, so it can grow to the height the forecast card gives the row
    const chart = () => draw("chart-forecast", (w, ht) => Charts.forecast(w, FC.any, hist, P.by_day, k ? k.dos : P.peak_dos, ht));
    if (!k) { $("forecast-box").innerHTML = '<p class="rule">Pick a day from 1 October to 31 December.</p>'; chart(); return; }
    const f = FC[h][k.dos - 1], a = FC.any[k.dos - 1], o = P.by_day[k.dos - 1];
    const phase = Math.abs(k.dos - P.peak_dos) <= 7 ? "The wettest stretch of the season."
      : k.dos < P.peak_dos ? "Before the peak, chance rising." : "After the peak, chance falling.";
    const memory = h === "any"
      ? `After two dry days ${Fmt.pct(FC.DD[k.dos - 1].p)}, two wet days ${Fmt.pct(FC.WW[k.dos - 1].p)}.`
      : `After ${P.after[h]}: ${Fmt.pct(f.p)}, ${f.p < a.p ? "down" : "up"} from ${Fmt.pct(a.p)}.`;
    const reasons = [
      phase,
      `Wet on this date in ${o.wet} of ${o.n} seasons.`,
      memory,
      `If wet, about ${Fmt.num(o.near_median, 0)} mm; 1 in 10 over ${Fmt.num(o.near_q90, 0)} mm.`,
      `No trend since 1991 (${Fmt.pText(P.trend_season.p)}).`,
    ];
    $("forecast-box").innerHTML = `<p class="forecast-date">${WEEKDAYS[k.t.getUTCDay()]} ${k.t.getUTCDate()} ${Fmt.dosDate(k.dos, k.year).split(" ").slice(1).join(" ")}</p>
      <div class="facts two">
        <div><div class="fact-v">${Fmt.pct(f.p)}</div><div class="fact-l">chance of a wet day</div></div>
        <div><div class="fact-v">${Fmt.pct(f.p * P.p_heavy_if_wet, 1)}</div><div class="fact-l">chance of over ${Fmt.num(P.heavy_mm, 0)}&nbsp;mm</div></div>
      </div>
      <p class="test-s">95% range ${Fmt.pct(f.lo)} to ${Fmt.pct(f.hi)}</p>
      <p class="why-head">Why</p>
      <ul class="why">${reasons.map((r) => `<li>${r}</li>`).join("")}</ul>`;
    chart();
  }
  function renderOutlook(year) {
    const P = R.predict, t = P.outlook;
    const label = { wet: "Wet days in the season", oct: "Wet days in October", nov: "Wet days in November", dec: "Wet days in December", dry: "Longest dry spell", total: "Rain in the season" };
    const unit = (key) => (key === "total" ? " mm" : " days");
    const row = (key) => t.find((r) => r.key === key);
    const w = row("wet"), d = row("dry"), tot = row("total");
    const trs = t.map((r) => `<tr><td>${label[r.key]}</td>
      <td>${Fmt.num(r.mean, 0)}${unit(r.key)}<div class="sub">${Fmt.num(r.lo, 0)} to ${Fmt.num(r.hi, 0)}</div></td>
      <td>${Fmt.num(r.obs_mean, 0)}${unit(r.key)}<div class="sub">${Fmt.num(r.obs_min, 0)} to ${Fmt.num(r.obs_max, 0)}</div></td>
      <td>${Fmt.pct(r.obs_in_range)}</td></tr>`).join("");
    $("outlook").innerHTML = `<p class="rule">${year}: about ${Fmt.num(w.mean, 0)} wet days (${Fmt.num(w.lo, 0)} to ${Fmt.num(w.hi, 0)}), longest dry spell about ${Fmt.plural(Math.round(d.mean), "day")}.</p>
      <table class="tbl"><thead><tr><th></th><th>Simulated</th><th>Real seasons</th><th>Real inside the range</th></tr></thead><tbody>${trs}</tbody></table>
      <p class="note">${Fmt.num(P.n_sim, 0)} simulated seasons, 90% ranges. Real: lowest to highest.${tot.obs_in_range < 0.85 ? " Real season totals vary more than the model allows." : ""}</p>`;
  }
  // Hits and false alarms at the slider's threshold, from R's grid
  function renderChecks() {
    const K = R.predict.checks, cut = state.cut;
    $("cut-out").textContent = Fmt.pct(cut);
    const one = (key) => {
      const r = K.grid.find((g) => g.key === key && Math.abs(g.threshold - cut) < 1e-6);
      const wetDays = r.hits + r.misses, calls = r.hits + r.false_alarms;
      return {
        caught: `${Fmt.pct(r.hits / wetDays)} of ${Fmt.num(wetDays, 0)}`,
        fa: calls === 0 ? "no wet calls" : `${Fmt.pct(r.false_alarms / calls)} of ${Fmt.num(calls, 0)}`,
        right: Fmt.pct((r.hits + r.correct_dry) / K.n_days),
        pod: r.hits / wetDays, far: calls === 0 ? null : r.false_alarms / calls,
      };
    };
    const ls = one("season"), nd = one("chain");
    const sc = (key) => K.scores.find((s) => s.key === key);
    $("check-table").innerHTML = `<p class="rule">Next day: ${Fmt.pct(nd.pod)} of wet days caught, ${nd.far === null ? "no wet calls" : `${Fmt.pct(nd.far)} false alarms`}.</p>
      <table class="tbl compact"><thead><tr><th></th><th>Long range</th><th>Next day</th></tr></thead><tbody>
        <tr><td>Wet days caught</td><td>${ls.caught}</td><td>${nd.caught}</td></tr>
        <tr><td>False alarms</td><td>${ls.fa}</td><td>${nd.fa}</td></tr>
        <tr><td>Right on the day</td><td>${ls.right}</td><td>${nd.right}</td></tr>
        <tr><td>Better than a flat guess</td><td>${Fmt.pct(sc("season").skill)}</td><td>${Fmt.pct(sc("chain").skill)}</td></tr>
        <tr><td>ROC area</td><td>${Fmt.num(sc("season").auc, 2)}</td><td>${Fmt.num(sc("chain").auc, 2)}</td></tr>
      </tbody></table>
      <p class="note">Each season hidden in turn and forecast from the other ${K.n_seasons - 1}. False alarm: a wet call that stayed dry. Flat guess: Brier skill.</p>`;
    if (!drawn["chart-reliability"]) draw("chart-reliability", (w) => Charts.reliability(w, K.reliability));
  }

  // Decades ---------------------------------------------------------------------------
  function renderDecades() {
    const { h4, h5, seasons } = R.decades;
    const change4 = h4.p < 0.05, change5 = h5.p < 0.05;
    $("lede-decades").textContent = !change4 && !change5
      ? `No clear change across three decades, in rain following rain (${Fmt.pText(h4.p)}) or in heavy days (${Fmt.pText(h5.p)}).`
      : `Rain following rain: ${change4 ? "changed" : "no clear change"} (${Fmt.pText(h4.p)}). Heavy days: ${change5 ? "changed" : "no clear change"} (${Fmt.pText(h5.p)}).`;
    draw("chart-totals", (w) => Charts.totals(w, seasons, R.meta.decades));
    draw("chart-decade-chain", (w) => Charts.decadeChain(w, h4.per_decade, R.meta.decades));
    draw("chart-heavy", (w) => Charts.heavyShare(w, h5, R.meta.decades));
    $("h4-text").innerHTML = test("H4", "Has the chain changed across decades?",
      change4 ? "Yes." : "No clear change.",
      chi(h4.stat, h4.df, h4.p), `From dry ${Fmt.pText(h4.p_from_dry)}, from wet ${Fmt.pText(h4.p_from_wet)}.`);
    $("h5-text").innerHTML = test("H5", "Has the heavy tail changed?",
      change5 ? "Yes." : `No clear change in days over ${Fmt.num(h5.q95, 0)} mm.`,
      chi(h5.stat, h5.df, h5.p), `ANOVA on log amounts: F = ${Fmt.num(h5.anova_f)}, ${Fmt.pText(h5.anova_p)}.`);
  }

  // Extremes --------------------------------------------------------------------------
  const fitName = (dist) => R.extremes.table.find((t) => t.dist === dist).name;
  function renderExtremes() {
    const E = R.extremes;
    const best = E.returns[E.best_aic];
    const r10 = best.reduce((a, r) => (Math.abs(r.T - 10) < Math.abs(a.T - 10) ? r : a));
    const wettest = E.top_days[0];
    $("lede-extremes").textContent = `A 1 in 10 year day brings about ${Fmt.num(r10.level, 0)} mm. The record is ${Fmt.num(wettest.rain_mm, 0)} mm, on ${Fmt.date(wettest.date)}.`;
    renderFit();
  }
  function renderFit() {
    const E = R.extremes, f = state.fit;
    const rows = E.table.map((t) => `<tr class="${t.dist === f ? "best" : ""}"><td>${t.name}<div class="sub">${esc(t.params)}</div></td>
      <td>${Fmt.num(t.aic, 0)}</td><td>${t.delta_aic < 0.05 ? "lowest" : "+" + Fmt.num(t.delta_aic, 1)}</td><td>${Fmt.num(t.tail_error, 3)}</td></tr>`).join("");
    const note = E.best_aic === E.best_tail
      ? `${fitName(E.best_aic)} wins on AIC and on the wettest 5%. ${Fmt.num(E.n, 0)} wet days.`
      : `${fitName(E.best_aic)} wins on AIC, ${fitName(E.best_tail)} on the wettest 5%. ${Fmt.num(E.n, 0)} wet days.`;
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
      <p class="note">${fitName(f)} fit. Grey rows go past the ${R.meta.n_used}-season record.</p>`;
  }

  // Decision --------------------------------------------------------------------------
  function renderDecision() {
    const D = R.decision, st = D.states;
    const lo = st.reduce((a, s) => (s.p_heavy < a.p_heavy ? s : a)), hi = st.reduce((a, s) => (s.p_heavy > a.p_heavy ? s : a));
    $("lede-decision").textContent = `Chance of over ${Fmt.num(D.heavy_mm, 0)} mm tomorrow: ${Fmt.pct(lo.p_heavy, 1)} ${lo.after}, ${Fmt.pct(hi.p_heavy, 1)} ${hi.after}.`;
    $("alert-caption").textContent = "Bars: model. Ticks: record. Line: C/L.";
    $("buffer-caption").textContent = `Grey: 20,000 simulated seasons. Orange: the ${R.meta.n_used} real ones.`;
    renderAlert();
    renderBuffer();
  }
  // Worked out in the browser for whatever ratio the slider is on
  function renderAlert() {
    const D = R.decision, cl = state.cl;
    $("cl-out").textContent = Fmt.num(cl, 3);
    const on = D.states.filter((s) => s.p_heavy > cl);
    const rule = !on.length ? "No alert at this ratio." : on.length === D.states.length ? "Always alert at this ratio."
      : `Alert ${Fmt.joinAnd(on.map((s) => s.after))}.`;
    $("alert-rule").innerHTML = `<p class="rule">${rule}</p><table class="tbl compact"><tbody>${D.states.map((s) =>
      `<tr class="${s.p_heavy > cl ? "on" : ""}"><td>${s.state}</td><td>C/L below ${Fmt.num(s.p_heavy, 3)}</td></tr>`).join("")}</tbody></table>
      <p class="note">C and L are assumed, not measured.</p>`;
    // the chart last, so it can grow to the height the rule card gives the row
    draw("chart-alert", (w, h) => Charts.alert(w, D.states, cl, h));
  }
  const bufferDays = (curve, col, risk) => Math.min(...curve.filter((r) => r[col] <= risk + 1e-12).map((r) => r.k));
  function renderBuffer() {
    const D = R.decision, risk = state.risk;
    $("risk-out").textContent = Fmt.pct(risk);
    const kc = bufferDays(D.dry_max, "chain", risk), ko = bufferDays(D.dry_max, "observed", risk);
    const rows = [0.05, 0.1, 0.2, 0.5].map((r) => `<tr><td>${Fmt.pct(r)} risk</td><td>${Fmt.plural(bufferDays(D.dry_max, "chain", r), "day")}</td><td>${Fmt.plural(bufferDays(D.dry_max, "observed", r), "day")}</td></tr>`).join("");
    $("buffer-rule").innerHTML = `<p class="rule">Hold ${Fmt.plural(kc, "day")} of supply.</p>
      <table class="tbl compact"><thead><tr><th>Risk</th><th>Chain</th><th>Record</th></tr></thead><tbody>${rows}</tbody></table>
      <p class="note">Record alone: ${Fmt.plural(ko, "day")}. ${D.dry_fit ? "The chain fits dry spells (H3)." : "The chain misses long dry spells (H3), so check the record column."}</p>`;
    draw("chart-buffer", (w, h) => Charts.buffer(w, D.dry_max, risk, kc, h));
  }

  // Data ------------------------------------------------------------------------------
  function renderData() {
    const f = R.audit.facts;
    const spec = (l, v) => `<div class="spec"><div class="spec-l">${l}</div><div class="spec-v">${v}</div></div>`;
    $("audit-rules").innerHTML = `<div class="specs">
      ${spec("Station", "Nungambakkam (IMD)")}
      ${spec("Season", "1 October to 31 December")}
      ${spec("Wet day", "2.5 mm or more")}
      ${spec("Gaps", "Never filled in")}
      ${spec("Seasons", `${f.seasons_used} of ${f.seasons_total} used. Dropped: ${f.seasons_dropped}`)}</div>`;
    draw("chart-calendar", (w) => Charts.calendar(w, DAYS, DAYS_LABELS));
    const miss = Object.entries(R.audit.missing_by_decade).map(([k, v]) => `${k}: ${v}`).join("; ");
    const items = [
      `The Tamil Nadu file has Nungambakkam from ${Fmt.date(f.first_listed_day)} and matches the Chennai file on all ${Fmt.num(+f.city_file_days_compared, 0)} shared days, so it is used.`,
      `Only rainy days are listed, so an unlisted day is read as dry.`,
      `${Fmt.num(+f.tn_exact_duplicates, 0)} duplicate rows removed. ${f.conflict_days} days with two values set to missing.`,
      `Silent at every gauge, set to missing: ${f.missing_months}.`,
      `${f.missing_days_neighbours} unlisted days where nearby gauges had 10 mm or more, set to missing.`,
      `Missing days by decade: ${miss}.`,
      `No filled-in values: no amount repeats for more than ${f.longest_repeat_run_days} days.`,
      `ERA5 agrees on wet or dry for ${Fmt.pct(+f.era5_agreement, 1)} of days, best on the same date (r = ${f.era5_cor_same_day}).`,
    ];
    $("audit-found").innerHTML = `<ol class="audit">${items.map((t) => `<li>${t}</li>`).join("")}</ol>`;
  }
  let DAYS_LABELS = [];

  // Pages -----------------------------------------------------------------------------
  const PAGES = {
    "wet-and-dry-days": { id: "page-wet", render: renderWet },
    predict: { id: "page-predict", render: renderPredict },
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
    FC = {};
    for (const r of R.predict.forecasts) (FC[r.history] ||= []).push(r);

    radios($("pick-months"), "months", R.meta.months, state.months, (v) => { state.months = v; renderWet(); });
    radios($("pick-decade"), "decade", ["All years", ...R.meta.decades], state.decade, (v) => { state.decade = v; renderWet(); });
    segmented($("pick-fit"), "fit", R.extremes.table.map((t) => [t.dist, t.name]), state.fit, (v) => { state.fit = v; renderFit(); });
    $("resim").addEventListener("click", renderSeason);
    $("cl").addEventListener("input", (e) => { state.cl = +e.target.value; renderAlert(); });
    $("risk").addEventListener("input", (e) => { state.risk = +e.target.value; renderBuffer(); });
    radios($("pick-history"), "history", ["any", ...Object.keys(R.predict.histories)], state.history, (v) => { state.history = v; renderDay(); });
    $("pick-history").querySelectorAll("label").forEach((l) => {
      const v = l.querySelector("input").value;
      l.lastChild.textContent = v === "any" ? "Not known yet" : R.predict.histories[v];
    });
    // the browser may restore a day or slider value from before a reload
    state.day = $("day").value || state.day;
    state.cut = +$("cut").value || state.cut;
    $("day").addEventListener("change", (e) => { state.day = e.target.value; renderDay(); });
    $("cut").addEventListener("input", (e) => { state.cut = +e.target.value; renderChecks(); });
    $("download").href = "data/nungambakkam_oct_dec_1991_2023.csv";

    window.addEventListener("hashchange", show);
    show();
    done();
  }).catch((err) => {
    console.error(err);
    $("loading-note").textContent = "The results could not be loaded. Please reload the page.";
  });
})();

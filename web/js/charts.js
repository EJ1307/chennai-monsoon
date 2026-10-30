// Charts, drawn with Observable Plot at the width of their box, in the same style as
// the R Shiny app. Each function returns an element to put on the page.

const Fmt = (() => {
  const num = (x, d = 2) => (x === null || x === undefined || Number.isNaN(x) ? "" :
    Number(x).toLocaleString("en-US", { minimumFractionDigits: d, maximumFractionDigits: d }));
  const pct = (x, d = 0) => num(100 * x, d) + "%";
  const pText = (p) => (p < 0.001 ? "p < 0.001" : "p = " + num(p, 3));
  const MONTH_NAMES = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];
  const date = (iso, withYear = true) => {
    const [y, m, d] = iso.split("-").map(Number);
    return `${d} ${MONTH_NAMES[m - 1]}${withYear ? " " + y : ""}`;
  };
  // day of season (1 = 1 October) to a date label
  const dosDate = (dos, year) => {
    const t = new Date(Date.UTC(year || 2001, 9, 1) + (dos - 1) * 86400000);
    return `${t.getUTCDate()} ${MONTH_NAMES[t.getUTCMonth()]}${year ? " " + year : ""}`;
  };
  const plural = (n, word) => `${n} ${n === 1 ? word : word + "s"}`;
  const joinAnd = (x) => (x.length === 1 ? x[0] : x.slice(0, -1).join(", ") + ", and " + x[x.length - 1]);
  const title = (s) => s.charAt(0).toUpperCase() + s.slice(1);
  return { num, pct, pText, date, dosDate, plural, joinAnd, title };
})();

const Charts = (() => {
  const C = {
    ink: "#0b0b0b", ink2: "#52514e", muted: "#898781", grid: "#ebeae4", axis: "#c3c2b7",
    wet: "#2a78d6", wetLt: "#b7d3f6", dry: "#eb6834", dryLt: "#f0efec", missing: "#a9a79f", other: "#bdbcb5",
  };
  const FONT = '"DM Sans", system-ui, -apple-system, "Segoe UI", sans-serif';
  const TIP = { fill: "#ffffff", stroke: "#d9d7d0", fontSize: 12, textPadding: 9, lineHeight: 1.35 };
  const phone = (w) => w < 560;

  const base = (w, h, extra = {}) => ({
    width: w, height: h, marginTop: 12, marginRight: 14, marginBottom: 44, marginLeft: 60,
    style: { fontFamily: FONT, fontSize: "11.5px", color: C.ink, background: "transparent", overflow: "visible" },
    ...extra,
  });
  const axisX = (ticks, o = {}) => Plot.axisX(ticks, { tickSize: 0, tickPadding: 6, color: C.muted, labelArrow: "none", labelAnchor: "center", labelOffset: 34, ...o });
  const axisY = (ticks, o = {}) => Plot.axisY(ticks, { tickSize: 0, tickPadding: 6, color: C.muted, labelArrow: "none", labelAnchor: "center", labelOffset: 48, ...o });
  const gridY = (ticks) => Plot.gridY(ticks, { stroke: C.grid, strokeOpacity: 1 });
  const gridX = (ticks) => Plot.gridX(ticks, { stroke: C.grid, strokeOpacity: 1 });

  // Spell lengths: share of spells lasting at least k days, observed against the chain
  function spells(w, res, state) {
    const col = state === "wet" ? C.wet : C.dry;
    // Shares are drawn on a log10 axis (a plain linear scale of log10 values keeps every
    // tick label: 100%, 30%, 10% and so on)
    const floor = 0.001;
    const L = (v) => Math.log10(Math.max(v, floor));
    const d = res.curve.filter((r) => r.chain >= floor || r.observed > 0)
      .map((r) => ({ ...r, loL: L(r.lo), hiL: L(r.hi), chainL: L(r.chain), obsL: L(r.observed) }));
    const pts = d.filter((r) => r.observed > 0);
    const kMax = d[d.length - 1].k;
    const yTicks = [1, 0.3, 0.1, 0.03, 0.01, 0.003, 0.001];
    return Plot.plot({
      ...base(w, phone(w) ? 250 : 290),
      x: { domain: [1, kMax], inset: 6 },
      y: { domain: [-3, 0] },
      marks: [
        gridY(yTicks.map(L)),
        axisY(yTicks.map(L), { tickFormat: (v) => { const s = 10 ** v; return Fmt.pct(s, s < 0.0095 ? 1 : 0); }, label: "Share of spells (log scale)" }),
        axisX(undefined, { label: `${Fmt.title(state)} spell length, days or longer`, ticks: phone(w) ? 5 : 8, tickFormat: (v) => (Number.isInteger(v) ? v : "") }),
        Plot.areaY(d, { x: "k", y1: "loL", y2: "hiL", fill: C.ink, fillOpacity: 0.07 }),
        Plot.lineY(d, { x: "k", y: "chainL", stroke: C.ink2, strokeWidth: 1.4 }),
        Plot.dot(pts, {
          x: "k", y: "obsL", r: 3.6, fill: col,
          title: (r) => `${r.k} ${state} days or longer\nObserved: ${Fmt.pct(r.observed, 1)} of spells (${r.n_obs})\nChain: ${Fmt.pct(r.chain, 1)}, 90% range ${Fmt.pct(r.lo, 1)} to ${Fmt.pct(r.hi, 1)}`,
          tip: TIP,
        }),
      ],
    });
  }

  // One real season above one simulated season, daily rain on a square-root scale
  function season(w, obs, sim, obsLabel) {
    const all = obs.concat(sim);
    const dosMin = Math.min(...all.map((r) => r.dos)), dosMax = Math.max(...all.map((r) => r.dos));
    const yMax = Math.max(50, ...all.map((r) => r.rain));
    const yTicks = [10, 50, 100, 200, 300].filter((t) => t <= yMax * 1.05);
    const starts = { 1: "1 Oct", 32: "1 Nov", 62: "1 Dec" };
    const xTicks = [1, 32, 62].filter((t) => t >= dosMin && t <= dosMax);
    const h = phone(w) ? 128 : 140;
    const panel = (data, label, withAxis, year) => {
      const d = data.filter((r) => r.rain > 0);
      const wrap = document.createElement("div");
      const head = document.createElement("p");
      head.className = "panel-label";
      head.textContent = label;
      wrap.append(head, Plot.plot({
        ...base(w, withAxis ? h + 26 : h, { marginTop: 8, marginBottom: withAxis ? 26 : 6, marginLeft: 44 }),
        x: { domain: [dosMin - 0.6, dosMax + 0.6], axis: null },
        y: { type: "sqrt", domain: [0, yMax * 1.05] },
        marks: [
          gridY(yTicks),
          axisY(yTicks, { label: null }),
          withAxis ? axisX(xTicks, { tickFormat: (t) => starts[t], label: null }) : null,
          Plot.rect(d, {
            x1: (r) => r.dos - 0.36, x2: (r) => r.dos + 0.36, y1: 0, y2: "rain",
            fill: (r) => (r.rain >= 2.5 ? C.wet : C.wetLt),
            title: (r) => `${Fmt.dosDate(r.dos)}\n${Fmt.num(r.rain, 1)} mm${r.rain < 2.5 ? " (dry day: under 2.5 mm)" : ""}`,
            tip: TIP,
          }),
        ],
      }));
      return wrap;
    };
    const box = document.createElement("div");
    box.append(panel(obs, obsLabel, false), panel(sim, "Simulated from the chain", true));
    return box;
  }

  // Rain in each October to December season
  function totals(w, seasons, decades) {
    const top = Math.max(...seasons.map((s) => s.total_mm));
    const yTicks = [0, 500, 1000, 1500, 2000].filter((t) => t <= top * 1.1);
    return Plot.plot({
      ...base(w, phone(w) ? 230 : 270, { marginTop: 26, marginBottom: 26 }),
      x: { domain: [1990.4, 2023.6] },
      y: { domain: [0, top * 1.08] },
      marks: [
        gridY(yTicks),
        axisY(yTicks, { tickFormat: (v) => Fmt.num(v, 0), label: "Rain, Oct to Dec (mm)" }),
        axisX(phone(w) ? [1991, 1999, 2007, 2015, 2023] : [1991, 1995, 1999, 2003, 2007, 2011, 2015, 2019, 2023], { tickFormat: (v) => String(v), label: null }),
        Plot.ruleX([2001.5, 2012.5], { stroke: C.axis, strokeDasharray: "3,3" }),
        Plot.text(decades, { x: (_, i) => [1996, 2007, 2018][i], y: top * 1.08, text: (d) => d, fill: C.muted, dy: -14, fontSize: 11 }),
        Plot.rect(seasons, {
          x1: (s) => s.season - 0.36, x2: (s) => s.season + 0.36, y1: 0, y2: "total_mm",
          fill: (s) => (s.used ? C.wet : C.missing),
          title: (s) => (s.used ? `${s.season}\n${Fmt.num(s.total_mm, 0)} mm, ${s.wet_days} wet days\nERA5 agrees on ${Fmt.pct(s.agree_share)} of days`
            : `${s.season}\nNot used: ${s.missing_days} days missing`),
          tip: TIP,
        }),
      ],
    });
  }

  // p(wet after wet) and p(wet after dry) by decade, with 95% intervals
  function decadeChain(w, perDecade, decades) {
    const d = perDecade.flatMap((r) => [
      { decade: r.decade, what: "Wet after wet", est: r.p11, lo: r.p11_lo, hi: r.p11_hi, col: C.wet },
      { decade: r.decade, what: "Wet after dry", est: r.p01, lo: r.p01_lo, hi: r.p01_hi, col: C.dry },
    ]);
    const last = d.filter((r) => r.decade === decades[decades.length - 1]);
    const yTicks = [0, 0.2, 0.4, 0.6, 0.8];
    return Plot.plot({
      ...base(w, phone(w) ? 240 : 270, { marginRight: phone(w) ? 84 : 96, marginBottom: 28 }),
      x: { type: "point", domain: decades, padding: 0.45 },
      y: { domain: [0, 0.8] },
      marks: [
        gridY(yTicks),
        axisY(yTicks, { tickFormat: (v) => Fmt.num(v, 1), label: "Probability" }),
        axisX(decades, { label: null }),
        Plot.ruleX(d, { x: "decade", y1: "lo", y2: "hi", stroke: "col", strokeWidth: 3, strokeOpacity: 0.3 }),
        Plot.dot(d, {
          x: "decade", y: "est", r: 4.6, fill: "col",
          title: (r) => `${r.what}, ${r.decade}\n${Fmt.num(r.est)} (95% interval ${Fmt.num(r.lo)} to ${Fmt.num(r.hi)})`,
          tip: TIP,
        }),
        Plot.text(last, { x: "decade", y: "est", text: "what", fill: "col", textAnchor: "start", dx: 12, fontSize: 11.5 }),
      ],
    });
  }

  // Share of wet days above the full-record 95th percentile, by decade
  function heavyShare(w, h5, decades) {
    const yTicks = [0, 0.02, 0.04, 0.06, 0.08, 0.1, 0.12];
    return Plot.plot({
      ...base(w, phone(w) ? 240 : 270, { marginRight: 84, marginBottom: 28 }),
      x: { type: "point", domain: decades, padding: 0.45 },
      y: { domain: [0, 0.12] },
      marks: [
        gridY(yTicks),
        axisY(yTicks, { tickFormat: (v) => Fmt.pct(v), label: "Wet days that were heavy" }),
        axisX(decades, { label: null }),
        Plot.ruleY([0.05], { stroke: C.axis, strokeDasharray: "3,3" }),
        Plot.text(["5% overall"], { x: () => decades[decades.length - 1], y: 0.05, dx: 16, dy: -8, textAnchor: "start", fill: C.muted, fontSize: 11 }),
        Plot.ruleX(h5.share, { x: "decade", y1: "lo", y2: "hi", stroke: C.wet, strokeWidth: 3, strokeOpacity: 0.3 }),
        Plot.dot(h5.share, {
          x: "decade", y: "share", r: 4.6, fill: C.wet,
          title: (r) => `${r.decade}\n${r.heavy} of ${r.wet} wet days above ${Fmt.num(h5.q95, 0)} mm\n${Fmt.pct(r.share, 1)} (95% interval ${Fmt.pct(r.lo, 1)} to ${Fmt.pct(r.hi, 1)})`,
          tip: TIP,
        }),
      ],
    });
  }

  // Upper half of the QQ plot for the three fits, the chosen fit highlighted
  function qq(w, q, names, chosen) {
    const rows = [];
    for (const dist of Object.keys(names)) {
      q[dist].forEach((f, i) => rows.push({ dist, fitted: f, observed: q.observed[i], chosen: dist === chosen }));
    }
    rows.sort((a, b) => a.chosen - b.chosen);
    const lo = Math.min(...rows.map((r) => Math.min(r.fitted, r.observed)));
    const hi = Math.max(...rows.map((r) => Math.max(r.fitted, r.observed)));
    const ticks = [10, 30, 100, 300, 1000].filter((t) => t >= lo * 0.9 && t <= hi * 1.1);
    return Plot.plot({
      ...base(w, phone(w) ? 260 : 290),
      x: { type: "log", domain: [lo * 0.95, hi * 1.05] },
      y: { type: "log", domain: [lo * 0.95, Math.max(...rows.map((r) => r.observed)) * 1.05] },
      marks: [
        gridY(ticks), gridX(ticks),
        axisY(ticks, { label: "Observed, mm (log scale)" }),
        axisX(ticks, { label: "Fitted wet-day amount, mm (log scale)" }),
        Plot.line([[lo, lo], [hi, hi]], { stroke: C.axis, strokeDasharray: "3,3", clip: true }),
        ...[false, true].map((isChosen) => Plot.dot(rows.filter((r) => r.chosen === isChosen), {
          x: "fitted", y: "observed", r: isChosen ? 2.6 : 1.9, fill: isChosen ? C.wet : C.other,
          title: (r) => `${names[r.dist]}\nObserved ${Fmt.num(r.observed, 1)} mm\nFit says ${Fmt.num(r.fitted, 1)} mm`,
          tip: TIP,
        })),
      ],
    });
  }

  // Return level curve with its 90% band and the largest observed days
  function returns(w, curve, topDays, recordYears, fitName) {
    const top = Math.max(...curve.map((r) => r.hi), ...topDays.map((r) => r.rain_mm));
    const xTicks = phone(w) ? [1, 2, 5, 10, 25, 50, 100, 200] : [1, 2, 5, 10, 25, 50, 100, 200];
    const yTicks = d3.ticks(0, top, 5);
    return Plot.plot({
      ...base(w, phone(w) ? 270 : 330),
      x: { type: "log", domain: [1, 220] },
      y: { domain: [0, top * 1.04] },
      marks: [
        Plot.rect([{}], { x1: recordYears, x2: 220, y1: 0, y2: top * 1.04, fill: C.ink, fillOpacity: 0.035 }),
        gridY(yTicks), gridX(xTicks),
        axisY(yTicks, { label: "Daily rain, mm" }),
        axisX(xTicks, { label: "Return period, years (log scale)" }),
        Plot.text(["Beyond\nthe record"], { x: 200, y: top * 0.03, textAnchor: "end", lineAnchor: "bottom", fill: C.muted, fontSize: 11, lineHeight: 1.1 }),
        Plot.areaY(curve, { x: "T", y1: "lo", y2: "hi", fill: C.wet, fillOpacity: 0.14 }),
        Plot.lineY(curve, { x: "T", y: "level", stroke: C.wet, strokeWidth: 1.8 }),
        Plot.dot(topDays, {
          x: "T", y: "rain_mm", r: 3, fill: C.ink,
          title: (r) => `${Fmt.date(r.date)}\n${Fmt.num(r.rain_mm, 1)} mm, about once in ${Fmt.num(r.T, r.T < 10 ? 1 : 0)} years in this record`,
          tip: TIP,
        }),
        Plot.tip(curve, Plot.pointerX({
          x: "T", y: "level",
          title: (r) => `${Fmt.num(r.T, r.T < 10 ? 1 : 0)}-year day\n${Fmt.num(r.level, 0)} mm (90% range ${Fmt.num(r.lo, 0)} to ${Fmt.num(r.hi, 0)})\n${fitName} fit`,
          ...TIP,
        })),
      ],
    });
  }

  // Chance of a heavy-rain day tomorrow by state, against the cost-loss ratio
  function alert(w, states, cl, h) {
    const top = Math.max(...states.map((s) => Math.max(s.p_heavy, s.observed)), cl) * 1.28;
    const d = states.map((s) => ({ ...s, on: s.p_heavy > cl }));
    const xTicks = d3.ticks(0, top, phone(w) ? 3 : 4);
    return Plot.plot({
      ...base(w, h || (phone(w) ? 230 : 260), { marginLeft: phone(w) ? 96 : 110, marginRight: 12 }),
      x: { domain: [0, top] },
      y: { type: "band", domain: d.map((s) => s.state), padding: 0.36 },
      marks: [
        gridX(xTicks),
        axisX(xTicks, { tickFormat: (v) => Fmt.pct(v, 1), label: "Chance of a heavy-rain day tomorrow" }),
        Plot.axisY({ tickSize: 0, tickPadding: 8, color: C.ink2, label: null, fontSize: 12 }),
        Plot.barX(d, {
          x: "p_heavy", y: "state", fill: (s) => (s.on ? C.wet : C.wetLt),
          title: (s) => `${s.state}\nModel: ${Fmt.pct(s.p_heavy, 1)} chance of a heavy day tomorrow\nRecord: ${Fmt.pct(s.observed, 1)} of ${s.n} days`,
          tip: TIP,
        }),
        Plot.tickX(d, { x: "observed", y: "state", stroke: C.ink, strokeWidth: 2 }),
        Plot.ruleX([cl], { stroke: C.ink, strokeWidth: 1.5 }),
        Plot.text(d, { x: top, y: "state", text: (s) => (s.on ? "Alert" : "No alert"), textAnchor: "end", fill: (s) => (s.on ? C.ink : C.muted), fontSize: 11.5 }),
      ],
    });
  }

  // P(longest dry spell in a season > k): the model against the record
  function buffer(w, curve, risk, kChain, h, col = "chain") {
    const maxK = Math.max(...curve.map((r) => r.k));
    const yTicks = [0, 0.25, 0.5, 0.75, 1];
    return Plot.plot({
      ...base(w, h || (phone(w) ? 240 : 280)),
      x: { domain: [0, maxK] },
      y: { domain: [0, 1] },
      marks: [
        gridY(yTicks),
        axisY(yTicks, { tickFormat: (v) => Fmt.pct(v), label: "Seasons with a longer dry spell" }),
        axisX(undefined, { label: "Buffer, days without useful rain", ticks: phone(w) ? 5 : 8 }),
        Plot.ruleY([risk], { stroke: C.ink, strokeDasharray: "3,3" }),
        Plot.line(curve, { x: "k", y: "observed", stroke: C.dry, strokeWidth: 1.6, curve: "step-after" }),
        Plot.line(curve, { x: "k", y: col, stroke: C.ink2, strokeWidth: 1.6 }),
        Plot.dot([{ k: kChain, y: risk }], { x: "k", y: "y", r: 4, fill: C.ink }),
        Plot.text([{ k: kChain, y: risk }], { x: "k", y: "y", text: () => Fmt.plural(kChain, "day"), dx: 8, dy: -10, textAnchor: "start", fill: C.ink, fontSize: 12 }),
        Plot.tip(curve, Plot.pointerX({
          x: "k", y: col,
          title: (r) => `Longest dry spell over ${r.k} days\nModel: ${Fmt.pct(r[col], 1)} of seasons\nRecord: ${Fmt.pct(r.observed)} of seasons`,
          ...TIP,
        })),
      ],
    });
  }

  // Chance of a wet day through the season: the season model with its 95% band, the
  // share of real seasons wet on each date, the chosen history, and the chosen day
  function forecast(w, any, hist, byDay, dos, h) {
    const yTicks = [0, 0.2, 0.4, 0.6, 0.8, 1];
    const starts = { 1: "1 Oct", 32: "1 Nov", 62: "1 Dec" };
    const pick = (hist || any).find((r) => r.dos === dos);
    const right = dos > 70;
    return Plot.plot({
      ...base(w, h || (phone(w) ? 250 : 290), { marginBottom: 28 }),
      x: { domain: [0, 93] },
      y: { domain: [0, 1] },
      marks: [
        gridY(yTicks),
        axisY(yTicks, { tickFormat: (v) => Fmt.pct(v), label: "Chance of a wet day" }),
        axisX([1, 32, 62], { tickFormat: (t) => starts[t], label: null }),
        Plot.areaY(any, { x: "dos", y1: "lo", y2: "hi", fill: C.wetLt, fillOpacity: 0.55 }),
        Plot.dot(byDay, { x: "dos", y: "share", r: 1.8, fill: C.muted }),
        Plot.lineY(any, { x: "dos", y: "p", stroke: C.wet, strokeWidth: 2 }),
        hist ? Plot.lineY(hist, { x: "dos", y: "p", stroke: C.ink, strokeWidth: 1.4, strokeDasharray: "4,3" }) : null,
        Plot.ruleX([dos], { stroke: C.ink, strokeWidth: 0.8 }),
        Plot.dot([pick], { x: "dos", y: "p", r: 4.2, fill: C.ink }),
        Plot.text([pick], { x: "dos", y: "p", text: (r) => Fmt.pct(r.p), dx: right ? -8 : 8, dy: -11, textAnchor: right ? "end" : "start", fill: C.ink, fontSize: 12 }),
        Plot.tip(any, Plot.pointerX({
          x: "dos", y: "p",
          title: (r) => { const o = byDay[r.dos - 1]; return `${Fmt.dosDate(r.dos)}\nChance of a wet day: ${Fmt.pct(r.p)}\nRecord: ${o.wet} of ${o.n} seasons wet`; },
          ...TIP,
        })),
      ],
    });
  }

  // Reliability of the backtest forecasts: when they said p, how often it rained
  function reliability(w, rel) {
    const ticks = [0, 0.25, 0.5, 0.75, 1];
    const col = (r) => (r.key === "season" ? C.dry : C.wet);
    const name = (r) => (r.key === "season" ? "Long range" : "Next day");
    return Plot.plot({
      ...base(w, Math.min(w, phone(w) ? 280 : 310)),
      x: { domain: [0, 1] },
      y: { domain: [0, 1] },
      r: { range: [2, 6.5] },
      marks: [
        gridY(ticks), gridX(ticks),
        axisY(ticks, { tickFormat: (v) => Fmt.pct(v), label: "Days that were wet" }),
        axisX(ticks, { tickFormat: (v) => Fmt.pct(v), label: "Forecast chance" }),
        Plot.line([[0, 0], [1, 1]], { stroke: C.axis, strokeDasharray: "3,3" }),
        Plot.ruleX(rel, { x: "predicted", y1: "lo", y2: "hi", stroke: col, strokeWidth: 2, strokeOpacity: 0.35 }),
        Plot.line(rel, { x: "predicted", y: "observed", z: "key", stroke: col, strokeWidth: 1.4 }),
        Plot.dot(rel, {
          x: "predicted", y: "observed", r: "n", fill: col,
          title: (r) => `${name(r)}\nSaid ${Fmt.pct(r.predicted)} on average\nWet on ${Fmt.pct(r.observed)} of ${r.n} days`,
          tip: TIP,
        }),
      ],
    });
  }

  // Every October to December day of every season
  function calendar(w, days, labels) {
    const seasons = [...new Set(days.map((d) => d.season))].sort((a, b) => a - b);
    const cell = phone(w) ? 9 : 12;
    const kind = (d) => (d.rain === null ? C.missing : d.rain >= 2.5 ? C.wet : d.rain > 0 ? C.wetLt : C.dryLt);
    const starts = { 1: "1 Oct", 32: "1 Nov", 62: "1 Dec" };
    return Plot.plot({
      ...base(w, seasons.length * cell + 30, { marginTop: 26, marginBottom: 4, marginLeft: 44, marginRight: 4 }),
      x: { type: "band", domain: d3.range(1, 93), padding: 0 },
      y: { type: "band", domain: seasons, padding: 0 },
      marks: [
        axisX([1, 32, 62], { anchor: "top", tickFormat: (t) => starts[t], label: null, textAnchor: "start", tickPadding: 4 }),
        axisY(seasons.filter((s) => s % 2 === 1), { label: null, tickFormat: (s) => String(s) }),
        Plot.cell(days, {
          x: "dos", y: "season", fill: kind, inset: phone(w) ? 0.25 : 0.5,
          title: (d) => `${Fmt.dosDate(d.dos, d.season)}\n${d.rain === null ? "Missing: " + labels[d.status] : Fmt.num(d.rain, 1) + " mm"}`,
          tip: TIP,
        }),
      ],
    });
  }

  return { C, spells, season, totals, decadeChain, heavyShare, qq, returns, alert, buffer, forecast, reliability, calendar };
})();

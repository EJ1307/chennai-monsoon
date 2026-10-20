// Browser maths. R works out every standard result in advance (prepare/export_web.R);
// this file covers what cannot be prepared ahead: a new random season, slider values,
// and any month or decade choice that is not in results.json. It follows the R code
// in app/R step by step, so a choice computed here matches what R would give.

const Stats = (() => {
  const WET_MM = 2.5;
  const DECADES = ["1991 to 2001", "2002 to 2012", "2013 to 2023"];
  const MONTHS = { "Oct to Dec": [10, 11, 12], October: [10], November: [11], December: [12] };

  // Random numbers with a seed, so simulations can be repeated (mulberry32)
  function rng(seed) {
    let a = seed >>> 0;
    return () => {
      a = (a + 0x6d2b79f5) >>> 0;
      let t = a;
      t = Math.imul(t ^ (t >>> 15), t | 1);
      t ^= t + Math.imul(t ^ (t >>> 7), t | 61);
      return ((t ^ (t >>> 14)) >>> 0) / 4294967296;
    };
  }

  // log Gamma (Lanczos) and the regularized upper incomplete gamma, for chi-square p-values
  function lgamma(x) {
    const c = [0.99999999999980993, 676.5203681218851, -1259.1392167224028, 771.32342877765313,
      -176.61502916214059, 12.507343278686905, -0.13857109526572012, 9.9843695780195716e-6,
      1.5056327351493116e-7];
    if (x < 0.5) return Math.log(Math.PI / Math.sin(Math.PI * x)) - lgamma(1 - x);
    x -= 1;
    let a = c[0];
    const t = x + 7.5;
    for (let i = 1; i < 9; i++) a += c[i] / (x + i);
    return 0.5 * Math.log(2 * Math.PI) + (x + 0.5) * Math.log(t) - t + Math.log(a);
  }
  function gammaQ(a, x) {
    if (x <= 0) return 1;
    const front = Math.exp(-x + a * Math.log(x) - lgamma(a));
    if (x < a + 1) {
      let sum = 1 / a, del = sum, ap = a;
      for (let n = 0; n < 1000; n++) {
        ap += 1; del *= x / ap; sum += del;
        if (Math.abs(del) < Math.abs(sum) * 1e-16) break;
      }
      return Math.max(0, 1 - sum * front);
    }
    const tiny = 1e-300;
    let b = x + 1 - a, c = 1 / tiny, d = 1 / b, h = d;
    for (let i = 1; i < 1000; i++) {
      const an = -i * (i - a);
      b += 2;
      d = an * d + b; if (Math.abs(d) < tiny) d = tiny;
      c = b + an / c; if (Math.abs(c) < tiny) c = tiny;
      d = 1 / d;
      const del = d * c; h *= del;
      if (Math.abs(del - 1) < 1e-16) break;
    }
    return front * h;
  }
  const pchisqUpper = (x, df) => gammaQ(df / 2, x / 2);

  // Normal quantile (Acklam), used to sample lognormal amounts
  function qnorm(p) {
    const a = [-3.969683028665376e1, 2.209460984245205e2, -2.759285104469687e2, 1.38357751867269e2, -3.066479806614716e1, 2.506628277459239];
    const b = [-5.447609879822406e1, 1.615858368580409e2, -1.556989798598866e2, 6.680131188771972e1, -1.328068155288572e1];
    const c = [-7.784894002430293e-3, -3.223964580411365e-1, -2.400758277161838, -2.549732539343734, 4.374664141464968, 2.938163982698783];
    const d = [7.784695709041462e-3, 3.224671290700398e-1, 2.445134137142996, 3.754408661907416];
    const pl = 0.02425;
    if (p < pl) {
      const q = Math.sqrt(-2 * Math.log(p));
      return (((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    }
    if (p > 1 - pl) {
      const q = Math.sqrt(-2 * Math.log(1 - p));
      return -(((((c[0] * q + c[1]) * q + c[2]) * q + c[3]) * q + c[4]) * q + c[5]) / ((((d[0] * q + d[1]) * q + d[2]) * q + d[3]) * q + 1);
    }
    const q = p - 0.5, r = q * q;
    return (((((a[0] * r + a[1]) * r + a[2]) * r + a[3]) * r + a[4]) * r + a[5]) * q / (((((b[0] * r + b[1]) * r + b[2]) * r + b[3]) * r + b[4]) * r + 1);
  }
  function pnorm(z) {
    // complementary error function (Numerical Recipes erfcc), accurate to about 1e-7
    const x = -z / Math.SQRT2, t = 1 / (1 + 0.5 * Math.abs(x));
    const y = t * Math.exp(-x * x - 1.26551223 + t * (1.00002368 + t * (0.37409196 + t * (0.09678418 + t * (-0.18628806 + t * (0.27886807 + t * (-1.13520398 + t * (1.48851587 + t * (-0.82215223 + t * 0.17087277)))))))));
    return 0.5 * (x >= 0 ? y : 2 - y);
  }

  // R's default quantile (type 7) of an already sorted array
  function quantile7(sorted, p) {
    const n = sorted.length;
    if (!n) return NaN;
    const h = (n - 1) * p, lo = Math.floor(h), hi = Math.min(lo + 1, n - 1);
    return sorted[lo] + (h - lo) * (sorted[hi] - sorted[lo]);
  }

  // Wilson score interval
  function wilson(x, n, level = 0.95) {
    if (!n) return [NaN, NaN, NaN];
    const z = 1.959963984540054, p = x / n;
    const centre = (p + z * z / (2 * n)) / (1 + z * z / n);
    const half = z * Math.sqrt(p * (1 - p) / n + z * z / (4 * n * n)) / (1 + z * z / n);
    return [p, centre - half, centre + half];
  }

  // Pearson chi-square (no continuity correction) for a table given as rows
  function chisq(rows) {
    const rs = rows.map((r) => r.reduce((s, v) => s + v, 0));
    const cs = rows[0].map((_, j) => rows.reduce((s, r) => s + r[j], 0));
    const N = rs.reduce((s, v) => s + v, 0);
    let stat = 0, minE = Infinity;
    rows.forEach((r, i) => r.forEach((o, j) => {
      const e = rs[i] * cs[j] / N;
      minE = Math.min(minE, e);
      stat += (o - e) * (o - e) / e;
    }));
    return { stat, minE, df: (rows.length - 1) * (rows[0].length - 1) };
  }

  // The daily series as objects, with yesterday and the day before inside each season
  function prepareDays(raw) {
    const used = new Set(raw.used_seasons);
    const out = raw.rain.map((rain, i) => ({
      season: raw.season[i], month: raw.month[i], dos: raw.dos[i], rain,
      status: raw.status[i], used: used.has(raw.season[i]),
      wet: rain === null ? null : rain >= WET_MM,
    }));
    out.forEach((d, i) => {
      const p1 = out[i - 1], p2 = out[i - 2];
      d.prev = p1 && p1.season === d.season ? p1.wet : null;
      d.prev2 = p2 && p2.season === d.season ? p2.wet : null;
    });
    return out;
  }
  const decadeOf = (s) => (s <= 2001 ? DECADES[0] : s <= 2012 ? DECADES[1] : DECADES[2]);
  function selectDays(days, months, decade) {
    const m = MONTHS[months];
    return days.filter((d) => d.used && m.includes(d.month) && (decade === "All years" || decadeOf(d.season) === decade));
  }

  function fitChain(sel) {
    const n = { dd: 0, dw: 0, wd: 0, ww: 0 };
    for (const d of sel) {
      if (d.wet === null || d.prev === null) continue;
      n[(d.prev ? "w" : "d") + (d.wet ? "w" : "d")]++;
    }
    const p01 = wilson(n.dw, n.dd + n.dw), p11 = wilson(n.ww, n.wd + n.ww);
    return {
      counts: n, n_pairs: n.dd + n.dw + n.wd + n.ww, p01, p11,
      pi_wet: p01[0] / (p01[0] + 1 - p11[0]),
      mean_wet_spell: 1 / (1 - p11[0]), mean_dry_spell: 1 / p01[0],
    };
  }

  function testH1(ch) {
    const t = chisq([[ch.counts.dd, ch.counts.dw], [ch.counts.wd, ch.counts.ww]]);
    return { stat: t.stat, df: 1, p: pchisqUpper(t.stat, 1), min_expected: t.minE, diff: ch.p11[0] - ch.p01[0] };
  }

  // H2: the day before yesterday, compared only within the same yesterday state
  function testH2(sel) {
    const tab = { DD: [0, 0], WD: [0, 0], DW: [0, 0], WW: [0, 0] };
    for (const d of sel) {
      if (d.wet === null || d.prev === null || d.prev2 === null) continue;
      tab[(d.prev2 ? "W" : "D") + (d.prev ? "W" : "D")][d.wet ? 1 : 0]++;
    }
    const part = (a, b) => {
      const rows = [tab[a], tab[b]];
      const ok = rows.every((r) => r[0] + r[1] > 0) && [0, 1].every((j) => rows[0][j] + rows[1][j] > 0);
      return ok ? chisq(rows) : { stat: 0, minE: NaN };
    };
    const x = part("DD", "WD"), y = part("DW", "WW");
    const stat = x.stat + y.stat;
    const p_wet = {}, n = {};
    for (const k of Object.keys(tab)) { n[k] = tab[k][0] + tab[k][1]; p_wet[k] = tab[k][1] / n[k]; }
    const minE = [x.minE, y.minE].filter((v) => !Number.isNaN(v));
    return { stat, df: 2, p: pchisqUpper(stat, 2), min_expected: minE.length ? Math.min(...minE) : NaN, p_wet, n };
  }

  // Runs of wet (1) or dry (0) days; 2 marks a missing day. A run next to a missing day
  // is dropped; runs never cross from one season (block) into the next.
  function runsOf(codes, blocks) {
    const vals = [], lens = [], blks = [];
    for (let i = 0; i < codes.length; i++) {
      if (i > 0 && codes[i] === codes[i - 1] && blocks[i] === blocks[i - 1]) lens[lens.length - 1]++;
      else { vals.push(codes[i]); lens.push(1); blks.push(blocks[i]); }
    }
    const out = { wet: [], dry: [] };
    for (let r = 0; r < vals.length; r++) {
      if (vals[r] === 2) continue;
      const prevMissing = r > 0 && vals[r - 1] === 2 && blks[r - 1] === blks[r];
      const nextMissing = r < vals.length - 1 && vals[r + 1] === 2 && blks[r + 1] === blks[r];
      if (prevMissing || nextMissing) continue;
      (vals[r] === 1 ? out.wet : out.dry).push(lens[r]);
    }
    return out;
  }

  const SPELL_BINS = { wet: [1, 2, 3, 4, 5], dry: [1, 2, 3, 5, 8, 15] };
  const SPELL_BIN_NAMES = {
    wet: ["1 day", "2 days", "3 days", "4 days", "5 days or more"],
    dry: ["1 day", "2 days", "3 to 4 days", "5 to 7 days", "8 to 14 days", "15 days or more"],
  };
  const LONG_SPELL = { wet: 5, dry: 15 };
  const binOf = (len, breaks) => { let b = 0; while (b + 1 < breaks.length && len >= breaks[b + 1]) b++; return b; };

  // H3: simulate nSets records, each as long as the observed record, from the fitted chain
  function compareSpells(sel, ch, nSets = 1000, seed = 2026) {
    const ordered = [...sel].sort((a, b) => a.season - b.season || a.dos - b.dos);
    const obs = runsOf(ordered.map((d) => (d.wet === null ? 2 : d.wet ? 1 : 0)), ordered.map((d) => d.season));
    const seasons = [...new Set(sel.map((d) => d.season))];
    const nSeasons = seasons.length, nDays = new Set(sel.map((d) => d.dos)).size;
    const rand = rng(seed);
    const p01 = ch.p01[0], p11 = ch.p11[0], pStart = ch.pi_wet;
    // simulated runs per state, with the record (set) they belong to
    const sim = { wet: { len: [], set: [] }, dry: { len: [], set: [] } };
    for (let s = 0; s < nSets * nSeasons; s++) {
      const set = Math.floor(s / nSeasons);
      let state = rand() < pStart, run = 1;
      for (let t = 1; t < nDays; t++) {
        const next = rand() < (state ? p11 : p01);
        if (next === state) run++;
        else { const k = state ? "wet" : "dry"; sim[k].len.push(run); sim[k].set.push(set); state = next; run = 1; }
      }
      const k = state ? "wet" : "dry"; sim[k].len.push(run); sim[k].set.push(set);
    }
    const oneState = (state) => {
      const oLen = obs[state], s = sim[state];
      const sortedSim = Float64Array.from(s.len).sort();
      const kMax = Math.floor(Math.min(nDays, Math.max(Math.max(...oLen), quantile7(sortedSim, 0.999))));
      const counts = Array.from({ length: nSets }, () => new Float64Array(kMax));
      for (let i = 0; i < s.len.length; i++) counts[s.set[i]][Math.min(s.len[i], kMax) - 1]++;
      const atLeast = counts.map((row) => {
        const tot = row.reduce((a, v) => a + v, 0), out = new Float64Array(kMax);
        let acc = 0;
        for (let k = kMax - 1; k >= 0; k--) { acc += row[k]; out[k] = tot ? acc / tot : NaN; }
        return out;
      });
      const curve = [];
      for (let k = 1; k <= kMax; k++) {
        const col = atLeast.map((r) => r[k - 1]).filter((v) => !Number.isNaN(v)).sort((a, b) => a - b);
        const nObs = oLen.filter((v) => v >= k).length;
        let simAtLeast = 0;
        for (const v of s.len) if (v >= k) simAtLeast++;
        curve.push({ k, observed: nObs / oLen.length, n_obs: nObs, chain: simAtLeast / s.len.length,
          lo: quantile7(col, 0.05), hi: quantile7(col, 0.95) });
      }
      const breaks = SPELL_BINS[state], nb = breaks.length;
      const expected = new Float64Array(nb);
      for (const v of s.len) expected[binOf(v, breaks)]++;
      for (let b = 0; b < nb; b++) expected[b] /= s.len.length;
      const x2 = (o) => { const tot = o.reduce((a, v) => a + v, 0); return o.reduce((a, v, b) => a + (v - tot * expected[b]) ** 2 / (tot * expected[b]), 0); };
      const oBins = new Array(nb).fill(0);
      for (const v of oLen) oBins[binOf(v, breaks)]++;
      const x2Obs = x2(oBins);
      const simBins = Array.from({ length: nSets }, () => new Array(nb).fill(0));
      for (let i = 0; i < s.len.length; i++) simBins[s.set[i]][binOf(s.len[i], breaks)]++;
      const pMc = simBins.filter((row) => x2(row) >= x2Obs).length / nSets;
      const totO = oBins.reduce((a, v) => a + v, 0);
      let worst = 0, worstVal = -1;
      oBins.forEach((o, b) => { const e = totO * expected[b]; const v = (o - e) ** 2 / e; if (v > worstVal) { worstVal = v; worst = b; } });
      const long = LONG_SPELL[state];
      const longSets = (long <= kMax ? atLeast.map((r) => r[long - 1]) : atLeast.map(() => 0)).filter((v) => !Number.isNaN(v)).sort((a, b) => a - b);
      let simLong = 0;
      for (const v of s.len) if (v >= long) simLong++;
      return {
        curve, n_spells: oLen.length, x2: x2Obs, p_mc: pMc, long,
        long_obs: oLen.filter((v) => v >= long).length / oLen.length,
        long_n: oLen.filter((v) => v >= long).length,
        long_sim: simLong / s.len.length,
        long_band: [quantile7(longSets, 0.05), quantile7(longSets, 0.95)],
        longest_obs: Math.max(...oLen),
        worst_bin: SPELL_BIN_NAMES[state][worst], worst_obs: oBins[worst] / totO, worst_exp: expected[worst],
      };
    };
    return { n_sets: nSets, n_seasons: nSeasons, n_days: nDays, wet: oneState("wet"), dry: oneState("dry") };
  }

  // A full month and decade result in the same shape R writes to results.json
  function computeSelection(days, months, decade) {
    const sel = selectDays(days, months, decade);
    const ch = fitChain(sel);
    return {
      months, decade, computed_in_browser: true,
      seasons: [...new Set(sel.map((d) => d.season))].sort((a, b) => a - b),
      dos: [...new Set(sel.map((d) => d.dos))].sort((a, b) => a - b),
      counts: ch.counts, n_pairs: ch.n_pairs, p01: ch.p01, p11: ch.p11, pi_wet: ch.pi_wet,
      mean_wet_spell: ch.mean_wet_spell, mean_dry_spell: ch.mean_dry_spell,
      h1: testH1(ch), h2: testH2(sel), h3: compareSpells(sel, ch),
    };
  }

  // Wet-day amounts from a fit, with the 2.5 mm cut-off (left-truncated)
  function gammaSample(shape, rand) {
    if (shape < 1) return gammaSample(shape + 1, rand) * Math.pow(rand(), 1 / shape);
    const d = shape - 1 / 3, c = 1 / Math.sqrt(9 * d);
    for (;;) {
      let x, v;
      do {
        const u1 = rand() || 1e-12, u2 = rand();
        x = Math.sqrt(-2 * Math.log(u1)) * Math.cos(2 * Math.PI * u2);
        v = 1 + c * x;
      } while (v <= 0);
      v = v * v * v;
      const u = rand();
      if (u < 1 - 0.0331 * x ** 4 || Math.log(u) < 0.5 * x * x + d * (1 - v + Math.log(v))) return d * v;
    }
  }
  function sampleAmount(dist, par, cut, rand) {
    if (dist === "weibull") {
      return par.scale * Math.pow(Math.pow(cut / par.scale, par.shape) - Math.log(1 - rand()), 1 / par.shape);
    }
    if (dist === "lognormal") {
      const p0 = pnorm((Math.log(cut) - par.meanlog) / par.sdlog);
      return Math.exp(par.meanlog + par.sdlog * qnorm(p0 + rand() * (1 - p0)));
    }
    for (;;) { const x = gammaSample(par.shape, rand) * par.scale; if (x >= cut) return x; }
  }

  // One season simulated from the chain, with amounts on wet days
  function simulateSeason(sel, dist, par, cut, rand) {
    const n = sel.dos.length;
    const out = [];
    let wet = rand() < sel.pi_wet;
    for (let i = 0; i < n; i++) {
      if (i > 0) wet = rand() < (wet ? sel.p11[0] : sel.p01[0]);
      out.push({ dos: sel.dos[i], rain: wet ? Math.round(sampleAmount(dist, par, cut, rand) * 10) / 10 : 0 });
    }
    return out;
  }

  return { WET_MM, DECADES, MONTHS, rng, pchisqUpper, wilson, prepareDays, selectDays, decadeOf,
    fitChain, testH1, testH2, compareSpells, computeSelection, simulateSeason, quantile7 };
})();

if (typeof module !== "undefined") module.exports = Stats;

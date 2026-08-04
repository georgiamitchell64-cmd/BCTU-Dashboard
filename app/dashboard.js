// ============================================================================
// TONIC Dashboard — rendering
// ============================================================================

// ── Derived figures ─────────────────────────────────────────────────────────
const cumActual = [];
const cumTarget = [];
MONTHS.reduce((a, m) => { a += m.randomised; cumActual.push(a); return a; }, 0);
MONTHS.reduce((a, m) => { a += m.target; cumTarget.push(a); return a; }, 0);

const totalRand = cumActual.at(-1);
const targetToDate = cumTarget.at(-1);
const pctOfTarget = Math.round((totalRand / targetToDate) * 100);
const pctOfTotal = (totalRand / TRIAL.target) * 100;

const last3 = MONTHS.slice(-3).reduce((s, m) => s + m.randomised, 0);
const rateRecent = last3 / 3;
const rateAll = totalRand / MONTHS.length;
const remaining = TRIAL.target - totalRand;

const recruitingSites = SITES.filter((s) => s.n > 0);
const openSites = SITES.filter((s) => s.status === "Recruiting" || s.status === "Open");

// A site is dormant when it has opened but not randomised in 60 days. The
// reference point follows the declared data cut, so an import moves it.
const CUT = (() => {
  const d = new Date(TRIAL.dataCut);
  return isNaN(d) ? new Date() : d;
})();
function daysSince(d) {
  if (!d) return Infinity;
  return Math.round((CUT - new Date(d)) / 86400000);
}
const dormantSites = recruitingSites.filter((s) => daysSince(s.lastRand) > 60);

const fuComplete = FOLLOWUP.reduce((s, f) => s + f.complete, 0);
const fuExpected = FOLLOWUP.reduce((s, f) => s + f.expected, 0);
const fuRate = Math.round((fuComplete / fuExpected) * 100);

// Month at which a flat rate would reach target, counted from the last month
// that has data rather than a fixed point.
function projectedClose(rate) {
  if (rate <= 0) return "—";
  const d = new Date(CUT.getFullYear(), CUT.getMonth(), 1);
  d.setMonth(d.getMonth() + Math.ceil(remaining / rate));
  return d.toLocaleDateString("en-GB", { month: "short", year: "numeric" });
}

// ── Tooltip ─────────────────────────────────────────────────────────────────
const tipEl = document.getElementById("tip");

function showTip(html, x, y) {
  tipEl.innerHTML = html;
  tipEl.classList.add("show");
  const r = tipEl.getBoundingClientRect();
  let left = x + 15, top = y + 15;
  if (left + r.width > innerWidth - 10) left = x - r.width - 15;
  if (top + r.height > innerHeight - 10) top = y - r.height - 15;
  tipEl.style.left = left + "px";
  tipEl.style.top = top + "px";
}
const hideTip = () => tipEl.classList.remove("show");

function row(label, value) {
  return `<div class="tip-row"><span>${label}</span><b>${value}</b></div>`;
}
function variance(actual, target) {
  const d = actual - target;
  if (d === 0) return `<div class="tip-row"><span>Variance</span><span class="tip-good">on target</span></div>`;
  const cls = d > 0 ? "tip-good" : "tip-bad";
  return `<div class="tip-row"><span>Variance</span><span class="${cls}">${Math.abs(d)} ${d > 0 ? "ahead" : "behind"}</span></div>`;
}

// Attach a hover tooltip to any DOM element.
function hoverTip(el, html) {
  el.addEventListener("mousemove", (e) => showTip(html, e.clientX, e.clientY));
  el.addEventListener("mouseleave", hideTip);
}

// ── Chart defaults ──────────────────────────────────────────────────────────
Chart.defaults.font.family = "'IBM Plex Sans', system-ui, sans-serif";
Chart.defaults.font.size = 11.5;
Chart.defaults.color = "#93A09C";

const GRID = "#EDEBE4";
const MONO = "'IBM Plex Mono', monospace";

// Shared axis styling so every chart reads as one family.
function axes(opts = {}) {
  return {
    x: {
      grid: { display: false },
      border: { color: GRID },
      ticks: { font: { family: MONO, size: 10.5 } },
      ...(opts.x || {}),
    },
    y: {
      beginAtZero: true,
      grid: { color: GRID },
      border: { display: false },
      ticks: { font: { family: MONO, size: 10.5 }, precision: 0 },
      ...(opts.y || {}),
    },
  };
}

// Render tooltips through our own element rather than Chart.js's canvas one.
function external(build) {
  return (ctx) => {
    const t = ctx.tooltip;
    if (!t || t.opacity === 0) return hideTip();
    const box = ctx.chart.canvas.getBoundingClientRect();
    showTip(build(t.dataPoints[0].dataIndex), box.left + t.caretX, box.top + t.caretY);
  };
}

const noLegend = { legend: { display: false } };

// ── KPI cards ───────────────────────────────────────────────────────────────
function kpiCard({ label, value, unit, pct, colour, note, tip }) {
  const el = document.createElement("div");
  el.className = "kpi";
  el.innerHTML = `
    <div class="kicker">${label}</div>
    <div class="kpi-val">
      <span class="kpi-num">${value}</span>
      ${unit ? `<span class="kpi-unit">${unit}</span>` : ""}
    </div>
    <div class="kpi-bar"><div style="width:${Math.min(pct, 100)}%;background:${colour}"></div></div>
    <div class="kpi-note">${note}</div>
  `;
  if (tip) hoverTip(el, tip);
  return el;
}

function fillKpis(id, cards) {
  const host = document.getElementById(id);
  host.innerHTML = "";
  cards.forEach((c) => host.appendChild(kpiCard(c)));
}

// ── Completion ring ─────────────────────────────────────────────────────────
function ringCard(f) {
  const pct = Math.round((f.complete / f.expected) * 100);
  const r = 36, circ = 2 * Math.PI * r;
  const el = document.createElement("div");
  el.className = "ring-card";
  el.innerHTML = `
    <div class="ring-name">${f.name}</div>
    <div class="ring-wrap">
      <svg width="84" height="84" viewBox="0 0 84 84">
        <circle cx="42" cy="42" r="${r}" fill="none" stroke="${GRID}" stroke-width="6"/>
        <circle cx="42" cy="42" r="${r}" fill="none" stroke="${f.colour}" stroke-width="6"
                stroke-dasharray="${circ}" stroke-dashoffset="${circ - (circ * pct) / 100}"
                stroke-linecap="round"/>
      </svg>
      <div class="ring-pct">${pct}%</div>
    </div>
    <div class="ring-detail">${f.complete} / ${f.expected} forms</div>
  `;
  hoverTip(el, `
    <div class="tip-title">${f.name}</div>
    ${row("Received", f.complete)}
    ${row("Due", f.expected)}
    ${row("Outstanding", f.expected - f.complete)}
    ${row("Rate", pct + "%")}
    <div class="tip-note">${f.event}</div>
  `);
  return el;
}

function fillRings(id, list) {
  const host = document.getElementById(id);
  host.innerHTML = "";
  list.forEach((f) => host.appendChild(ringCard(f)));
}

// ── Table helper ────────────────────────────────────────────────────────────
function buildTable(el, cols, rows, opts = {}) {
  const { sort, onSort } = opts;

  const head = cols
    .map((c, i) => {
      const classes = [c.num ? "num" : "", c.sortKey ? "sortable" : ""];
      let arrow = "";
      if (c.sortKey && sort && sort.col === i) {
        classes.push("sorted");
        arrow = `<span class="arrow">${sort.dir === "asc" ? "↑" : "↓"}</span>`;
      } else if (c.sortKey) {
        arrow = `<span class="arrow">↓</span>`;
      }
      return `<th class="${classes.join(" ")}" data-col="${i}">${c.label}${arrow}</th>`;
    })
    .join("");

  const body = rows
    .map((r) => {
      const tds = cols
        .map((c) => `<td class="${c.num ? "num" : ""} ${c.cls || ""}">${c.get(r)}</td>`)
        .join("");
      return `<tr>${tds}</tr>`;
    })
    .join("");

  el.innerHTML = `<thead><tr>${head}</tr></thead><tbody>${body}</tbody>`;

  if (onSort) {
    el.querySelectorAll("th.sortable").forEach((th) =>
      th.addEventListener("click", () => onSort(Number(th.dataset.col)))
    );
  }

  if (opts.tip) {
    [...el.querySelectorAll("tbody tr")].forEach((tr, i) =>
      hoverTip(tr, opts.tip(rows[i], i))
    );
  }
}

// Sort a copy of `rows` by a column's sortKey, nulls always last.
function sortRows(rows, key, dir) {
  const val = (r) => (typeof key === "function" ? key(r) : r[key]);
  return [...rows].sort((a, b) => {
    const x = val(a), y = val(b);
    if (x == null && y == null) return 0;
    if (x == null) return 1;
    if (y == null) return -1;
    const c = typeof x === "string" ? x.localeCompare(y) : x - y;
    return dir === "asc" ? c : -c;
  });
}

const statusPill = (s) => {
  const cls = { Recruiting: "pill-green", Open: "pill-amber", "Set-up": "pill-grey", Identified: "pill-grey" }[s];
  return `<span class="pill ${cls}">${s}</span>`;
};

// ══ HEADER ═════════════════════════════════════════════════════════════════
document.getElementById("side-sub").textContent = TRIAL.subtitle;
document.getElementById("side-cut").textContent = TRIAL.dataCut;

// Say plainly whether the figures on screen are imported or the shipped demo
// set, so nobody mistakes one for the other.
(function showProvenance() {
  const meta = window.__IMPORT_META;
  const note = document.getElementById("side-note");
  if (!meta) return;
  const when = new Date(meta.at);
  note.classList.add("imported");
  note.innerHTML =
    "Imported " + when.toLocaleDateString("en-GB", { day: "2-digit", month: "short", year: "numeric" }) +
    "<br>" + (meta.files || []).join("<br>");
})();
document.getElementById("head-close").textContent = projectedClose(rateRecent);

const statusEl = document.getElementById("head-status");
statusEl.textContent = `${pctOfTarget}% of time-adjusted target`;
statusEl.style.color = pctOfTarget >= 95 ? "#0E6B5E" : pctOfTarget >= 80 ? "#B67A16" : "#A63D2F";

const PAGES = {
  overview: {
    label: "Overview",
    meta: `${totalRand}/${TRIAL.target}`,
    kicker: "Live status",
    title: "Trial overview",
    blurb: "Recruitment, site activity and follow-up at a glance for the coordinating centre stand-up.",
  },
  recruitment: {
    label: "Recruitment",
    meta: `${rateRecent.toFixed(1)}/mo`,
    kicker: "Accrual",
    title: "Recruitment",
    blurb: "Monthly accrual against the protocol schedule, and what the current rate implies for the recruitment window.",
  },
  sites: {
    label: "Sites",
    meta: `${recruitingSites.length}/${TRIAL.targetSites}`,
    kicker: "Network",
    title: "Sites",
    blurb: "Where participants are coming from, which sites have opened, and which have gone quiet.",
  },
  randomisations: {
    label: "Randomisations",
    meta: `${totalRand}`,
    kicker: "Participants",
    title: "Randomisations",
    blurb: "Every participant entered to date, with the baseline profile of the recruited population.",
  },
  followup: {
    label: "Follow-up",
    meta: `${fuRate}%`,
    kicker: "Data return",
    title: "Follow-up",
    blurb: "Questionnaire return across the baseline, discharge, day 30 and day 90 windows.",
  },
};

// ══ NAVIGATION ═════════════════════════════════════════════════════════════
let currentPage = "overview";
const navHost = document.getElementById("side-nav");

Object.entries(PAGES).forEach(([key, p]) => {
  const b = document.createElement("button");
  b.className = "nav-item" + (key === currentPage ? " on" : "");
  b.dataset.page = key;
  b.innerHTML = `<span>${p.label}</span><span class="nav-meta">${p.meta}</span>`;
  b.addEventListener("click", () => goTo(key));
  navHost.appendChild(b);
});

function goTo(key) {
  currentPage = key;
  hideTip();

  document.querySelectorAll(".nav-item").forEach((b) =>
    b.classList.toggle("on", b.dataset.page === key)
  );
  document.querySelectorAll("[data-page]").forEach((s) => {
    if (s.tagName === "SECTION") s.hidden = s.dataset.page !== key;
  });

  const p = PAGES[key];
  document.getElementById("page-kicker").textContent = p.kicker;
  document.getElementById("page-title").textContent = p.title;
  document.getElementById("page-blurb").textContent = p.blurb;

  // Charts built inside a hidden section start at zero size; Chart.js picks
  // the real dimensions up through its ResizeObserver once the section shows.
}

document.querySelectorAll("[data-goto]").forEach((b) =>
  b.addEventListener("click", () => goTo(b.dataset.goto))
);

// ══ OVERVIEW ═══════════════════════════════════════════════════════════════
fillKpis("kpis", [
  {
    label: "Randomised", value: totalRand, unit: `of ${TRIAL.target}`,
    pct: pctOfTotal, colour: "#0E6B5E",
    note: `${pctOfTotal.toFixed(1)}% of the recruitment target; ${remaining} still to randomise.`,
    tip: `<div class="tip-title">Randomised</div>${row("To date", totalRand)}${row("Target", TRIAL.target)}${row("Remaining", remaining)}${row("Since", TRIAL.firstRandomisation)}`,
  },
  {
    label: "Last 3 months", value: last3, unit: "randomised",
    pct: (rateRecent / 20) * 100, colour: "#3FA593",
    note: `${rateRecent.toFixed(1)}/month against ${(MONTHS.slice(-3).reduce((s, m) => s + m.target, 0) / 3).toFixed(1)} scheduled.`,
    tip: `<div class="tip-title">Recent accrual</div>${MONTHS.slice(-3).map((m) => row(m.label, m.randomised)).join("")}${row("Mean", rateRecent.toFixed(1) + "/mo")}`,
  },
  {
    label: "Sites recruiting", value: recruitingSites.length, unit: `of ${TRIAL.targetSites}`,
    pct: (recruitingSites.length / TRIAL.targetSites) * 100, colour: "#7FA79F",
    note: `${openSites.length} open; ${dormantSites.length} with no randomisation in 60 days.`,
    tip: `<div class="tip-title">Site network</div>${row("Recruiting", recruitingSites.length)}${row("Open, none yet", openSites.length - recruitingSites.length)}${row("In set-up", SITES.filter((s) => s.status === "Set-up").length)}${row("Target", TRIAL.targetSites)}`,
  },
  {
    label: "Follow-up return", value: fuRate + "%", unit: "",
    pct: fuRate, colour: fuRate >= 90 ? "#0E6B5E" : "#B67A16",
    note: `${fuComplete} of ${fuExpected} forms received across all windows.`,
    tip: `<div class="tip-title">Follow-up</div>${row("Received", fuComplete)}${row("Due", fuExpected)}${row("Outstanding", fuExpected - fuComplete)}`,
  },
]);

document.getElementById("cum-sub").textContent =
  `First randomisation ${TRIAL.firstRandomisation} · target n=${TRIAL.target} by ${TRIAL.targetClose}`;

fillRings("rings-mini", FOLLOWUP);

// Cumulative / monthly recruitment chart
let cumView = "cumulative";
let showTarget = true;

function cumTip(i) {
  const a = cumView === "cumulative" ? cumActual[i] : MONTHS[i].randomised;
  const t = cumView === "cumulative" ? cumTarget[i] : MONTHS[i].target;
  return `
    <div class="tip-title">${MONTHS[i].label}</div>
    ${row("Randomised", a)}
    ${showTarget ? row("Target", t) + variance(a, t) + row("Of target", Math.round((a / t) * 100) + "%") : ""}
  `;
}

function cumSets() {
  const line = cumView === "cumulative";
  const actual = line ? cumActual : MONTHS.map((m) => m.randomised);
  const target = line ? cumTarget : MONTHS.map((m) => m.target);
  const sets = [];
  if (showTarget) {
    sets.push({
      label: "Target", data: target,
      borderColor: "#B0AEA4", backgroundColor: line ? "transparent" : "#EDEBE4",
      borderWidth: line ? 1.6 : 1, borderDash: line ? [5, 5] : undefined,
      pointRadius: 0, pointHoverRadius: 0, fill: false, tension: .25,
      borderRadius: line ? undefined : 3,
    });
  }
  sets.push({
    label: "Actual", data: actual,
    borderColor: "#0E6B5E", backgroundColor: line ? "rgba(221,234,231,.75)" : "#0E6B5E",
    borderWidth: line ? 2.6 : 1, pointRadius: line ? 0 : 0,
    pointHoverRadius: line ? 5 : 0, pointBackgroundColor: "#0E6B5E",
    pointHoverBorderColor: "#fff", pointHoverBorderWidth: 2,
    fill: line, tension: .25, borderRadius: line ? undefined : 3,
  });
  return sets;
}

const cumChart = new Chart(document.getElementById("chart-cum"), {
  type: "line",
  data: { labels: MONTHS.map((m) => m.label), datasets: cumSets() },
  options: {
    responsive: true, maintainAspectRatio: false,
    interaction: { mode: "index", intersect: false },
    plugins: { ...noLegend, tooltip: { enabled: false, external: external(cumTip) } },
    scales: axes(),
  },
});

function renderCum() {
  cumChart.config.type = cumView === "cumulative" ? "line" : "bar";
  cumChart.data.datasets = cumSets();
  cumChart.update();
  document.getElementById("cum-title").textContent =
    cumView === "cumulative" ? "Cumulative recruitment vs target" : "Randomisations per month";
}

document.querySelectorAll("[data-view]").forEach((b) =>
  b.addEventListener("click", () => {
    cumView = b.dataset.view;
    document.querySelectorAll("[data-view]").forEach((x) => x.classList.toggle("on", x === b));
    hideTip(); renderCum();
  })
);
document.getElementById("toggle-target").addEventListener("click", (e) => {
  showTarget = !showTarget;
  e.currentTarget.classList.toggle("on", showTarget);
  hideTip(); renderCum();
});

buildTable(
  document.getElementById("tbl-recent"),
  [
    { label: "ID", get: (r) => `<span class="mono">${r.id}</span>` },
    { label: "Site", get: (r) => r.site },
    { label: "Randomised", get: (r) => `<span class="mono">${r.date}</span>` },
    { label: "Age", num: true, get: (r) => r.age },
    { label: "Sex", get: (r) => r.sex },
  ],
  RANDOMISATIONS.slice(0, 6),
  {
    tip: (r) => `<div class="tip-title">${r.id}</div>${row("Site", r.site)}${row("Randomised", r.date)}${row("Age", r.age)}${row("NELA risk", r.nela + "%")}<div class="tip-note">Allocation withheld — blinded view</div>`,
  }
);

// ══ RECRUITMENT ════════════════════════════════════════════════════════════
function buildRecKpis() {
  fillKpis("rec-kpis", [
    {
      label: "Current rate", value: rateRecent.toFixed(1), unit: "per month",
      pct: (rateRecent / 20) * 100, colour: "#0E6B5E",
      note: `Mean over the last 3 months; ${rateAll.toFixed(1)}/month since opening.`,
      tip: `<div class="tip-title">Recruitment rate</div>${row("Last 3 months", rateRecent.toFixed(2))}${row("All time", rateAll.toFixed(2))}`,
    },
    {
      label: "Rate required", value: (remaining / 23).toFixed(1), unit: "per month",
      pct: 100, colour: "#B67A16",
      note: `To reach ${TRIAL.target} by ${TRIAL.targetClose} from the current position.`,
      tip: `<div class="tip-title">Required rate</div>${row("Remaining", remaining)}${row("Months left", 23)}${row("Needed", (remaining / 23).toFixed(1) + "/mo")}`,
    },
    {
      label: "Best month", value: Math.max(...MONTHS.map((m) => m.randomised)), unit: "randomised",
      pct: (Math.max(...MONTHS.map((m) => m.randomised)) / 20) * 100, colour: "#3FA593",
      note: `${MONTHS.find((m) => m.randomised === Math.max(...MONTHS.map((x) => x.randomised))).label} — the strongest month so far.`,
    },
    {
      label: "Projected close", value: centralClose() || "beyond", unit: "",
      pct: 60, colour: centralClose() ? "#0E6B5E" : "#A63D2F",
      note: centralClose()
        ? `Central scenario, with sites still opening toward ${PROJECTION.targetSites}.`
        : `Central scenario does not reach ${TRIAL.target} inside the protocol window.`,
      tip: `<div class="tip-title">Projection</div>${row("Central scenario", centralClose() || "not reached")}${row("Protocol close", TRIAL.targetClose)}${row("Flat rate, no new sites", projectedClose(rateRecent))}<div class="tip-note">The flat rate ignores sites still to open</div>`,
    },
  ]);
}

document.getElementById("rec-legend").innerHTML = `
  <div class="legend-item"><span class="legend-line" style="border-top:2px solid #0E6B5E"></span>Randomised</div>
  <div class="legend-item"><span class="legend-line" style="border-top:2px dashed #B0AEA4"></span>Protocol target</div>
`;

new Chart(document.getElementById("chart-permonth"), {
  type: "bar",
  data: {
    labels: MONTHS.map((m) => m.label),
    datasets: [
      { label: "Target", data: MONTHS.map((m) => m.target), backgroundColor: "#EDEBE4", borderColor: "#B0AEA4", borderWidth: 1, borderRadius: 3 },
      { label: "Randomised", data: MONTHS.map((m) => m.randomised), backgroundColor: "#0E6B5E", borderRadius: 3 },
    ],
  },
  options: {
    responsive: true, maintainAspectRatio: false,
    interaction: { mode: "index", intersect: false },
    plugins: {
      ...noLegend,
      tooltip: {
        enabled: false,
        external: external((i) => {
          const m = MONTHS[i];
          return `<div class="tip-title">${m.label}</div>${row("Randomised", m.randomised)}${row("Target", m.target)}${variance(m.randomised, m.target)}`;
        }),
      },
    },
    scales: axes(),
  },
});

// ── Recruitment projection ──────────────────────────────────────────────────
// Mirrors the Shiny dashboard's model (functions/projection_math.R): each month
// a few more sites open, up to the target, and every open site recruits at a
// per-site rate. That matters because a straight line through the current rate
// assumes the network never grows, which understates a trial still opening
// sites — and overstates one that has stopped.

// Derive both rates from what has actually happened, the way
// .projection_smart_defaults does, falling back to the configured values when
// there is too little history to be meaningful.
function derivedRates() {
  const out = { rate: PROJECTION.ratePerSite, sites: PROJECTION.sitesPerMonth, derived: false };

  const elapsed = (CUT - new Date(TRIAL.firstRandomisation)) / (86400000 * 30.44);
  if (elapsed >= 3 && openSites.length > 0) {
    out.rate = totalRand / (elapsed * openSites.length);
    out.derived = true;
  }

  // Site opening rate: how quickly the network has actually grown.
  const openedDates = SITES.filter((s) => s.opened).map((s) => new Date(s.opened));
  if (openedDates.length > 1) {
    const span = (CUT - Math.min(...openedDates)) / (86400000 * 30.44);
    if (span >= 1) out.sites = openedDates.length / span;
  }
  return out;
}

const BASE = derivedRates();
let projRate = BASE.rate;
let projSites = BASE.sites;
let projView = "cumulative";
let projBand = true;

// One scenario: step forward month by month, opening sites and recruiting.
// Returns cumulative totals, and stops growing once the target is reached.
function simulate(months, startCum, startSites, ratePerSite, sitesPerMonth) {
  const out = [];
  let cum = startCum;
  let sites = startSites;
  for (let i = 0; i < months; i++) {
    sites += Math.min(sitesPerMonth, Math.max(0, PROJECTION.targetSites - sites));
    cum = Math.min(cum + sites * ratePerSite, TRIAL.target);
    out.push(cum);
  }
  return out;
}

const projLabels = [...MONTHS.map((m) => m.label), ...TARGET_SCHEDULE.map((t) => t.label)];
const anchor = MONTHS.length - 1; // last month with real data

function scenario(rate, sites) {
  const tail = simulate(TARGET_SCHEDULE.length, totalRand, openSites.length, rate, sites);
  // Start at the last actual so the projection joins the observed curve.
  const series = MONTHS.map(() => null);
  series[anchor] = totalRand;
  return series.concat(tail.map((v) => Math.round(v)));
}

// Month in which a scenario reaches the target, or null if it never does.
function reachMonth(series) {
  const i = series.findIndex((v) => v != null && v >= TRIAL.target);
  return i === -1 ? null : projLabels[i];
}

// Close month under the central scenario, for the headline KPI.
function centralClose() {
  return reachMonth(scenario(projRate, projSites));
}

function toMonthly(cum) {
  return cum.map((v, i) => {
    if (v == null) return null;
    const prev = i > 0 ? cum[i - 1] : 0;
    return prev == null ? null : Math.max(0, Math.round(v - prev));
  });
}

function projSets() {
  const central = scenario(projRate, projSites);
  const optimistic = scenario(projRate * (1 + PROJECTION.rateSpread), projSites + PROJECTION.siteSpread);
  const pessimistic = scenario(projRate * (1 - PROJECTION.rateSpread), Math.max(0, projSites - PROJECTION.siteSpread));

  const actualCum = [...cumActual, ...TARGET_SCHEDULE.map(() => null)];
  const planCum = [...cumTarget, ...TARGET_SCHEDULE.map((t) => t.cumulative)];

  const monthly = projView === "monthly";
  const shape = (c) => (monthly ? toMonthly(c) : c);

  const line = (label, data, colour, width, dash, fill) => ({
    label, data: shape(data), borderColor: colour, borderWidth: width,
    borderDash: dash, pointRadius: 0, pointHoverRadius: 0,
    backgroundColor: fill || "transparent", fill: fill ? "-1" : false, tension: 0.25,
  });

  const sets = [line("Protocol plan", planCum, "#B0AEA4", 1.6, [5, 5])];

  if (projBand) {
    // Drawn as a pair so the optimistic line fills down to the pessimistic one.
    sets.push(line("Pessimistic", pessimistic, "#CFCCC2", 1.2, [2, 3]));
    sets.push(line("Optimistic", optimistic, "#CFCCC2", 1.2, [2, 3], "rgba(221,234,231,.55)"));
  }

  sets.push(line("Projection", central, "#B67A16", 2.2, [4, 4]));
  sets.push(line("Actual", actualCum, "#0E6B5E", 2.8, undefined,
                 monthly ? undefined : "rgba(221,234,231,.8)"));
  if (!monthly) sets.at(-1).fill = "origin";

  return { sets, central, optimistic, pessimistic };
}

let projState = projSets();

const projChart = new Chart(document.getElementById("chart-projection"), {
  type: "line",
  data: { labels: projLabels, datasets: projState.sets },
  options: {
    responsive: true, maintainAspectRatio: false,
    interaction: { mode: "index", intersect: false },
    plugins: {
      ...noLegend,
      tooltip: {
        enabled: false,
        external: external((i) => {
          const at = (s) => (s[i] == null ? "—" : Math.round(s[i]));
          const past = i <= anchor;
          return `
            <div class="tip-title">${projLabels[i]}</div>
            ${past ? row("Randomised", cumActual[i] ?? "—") : row("Projected", at(projState.central))}
            ${!past && projBand ? row("Range", `${at(projState.pessimistic)} – ${at(projState.optimistic)}`) : ""}
            ${row("Protocol plan", i < cumTarget.length ? cumTarget[i] : TARGET_SCHEDULE[i - cumTarget.length].cumulative)}
            <div class="tip-note">${past ? "Actual" : "Projected at " + projRate.toFixed(2) + "/site/month"}</div>
          `;
        }),
      },
    },
    scales: axes({ x: { ticks: { maxTicksLimit: 12, font: { family: MONO, size: 10 } } } }),
  },
});

document.getElementById("proj-legend").innerHTML = `
  <div class="legend-item"><span class="legend-line" style="border-top:2.8px solid #0E6B5E"></span>Randomised</div>
  <div class="legend-item"><span class="legend-line" style="border-top:2.2px dashed #B67A16"></span>Projection</div>
  <div class="legend-item"><span class="legend-line" style="border-top:1.6px dashed #B0AEA4"></span>Protocol plan</div>
  <div class="legend-item"><span class="legend-line" style="border-top:1.2px dotted #CFCCC2"></span>Optimistic / pessimistic</div>
`;

function renderProjection() {
  projState = projSets();
  projChart.data.datasets = projState.sets;
  projChart.update();

  document.getElementById("out-rate").textContent = projRate.toFixed(2) + " / site / month";
  document.getElementById("out-sites").textContent =
    projSites.toFixed(2) + " site" + (projSites === 1 ? "" : "s") + " / month";

  const reach = reachMonth(projState.central);

  // The header stat follows the same model, so the two never disagree.
  const head = document.getElementById("head-close");
  head.textContent = reach || "beyond window";
  head.style.color = reach ? "" : "#A63D2F";

  document.getElementById("proj-sub").textContent =
    `${openSites.length} sites open now, ramping to ${PROJECTION.targetSites}. ` +
    (reach ? `Target reached ${reach}.` : `Target not reached within the protocol window.`) +
    (BASE.derived ? " Rates derived from recruitment to date." : "");

  buildRecKpis();
  buildScenarioTable();
  buildRateTable();
}

// ── Scenario summary ────────────────────────────────────────────────────────
function buildScenarioTable() {
  const rows = [
    { name: "Optimistic", series: projState.optimistic, rate: projRate * (1 + PROJECTION.rateSpread), sites: projSites + PROJECTION.siteSpread },
    { name: "Central", series: projState.central, rate: projRate, sites: projSites },
    { name: "Pessimistic", series: projState.pessimistic, rate: projRate * (1 - PROJECTION.rateSpread), sites: Math.max(0, projSites - PROJECTION.siteSpread) },
  ];

  buildTable(
    document.getElementById("tbl-scenarios"),
    [
      { label: "Scenario", get: (r) => r.name },
      { label: "Per site", num: true, cls: "mono", get: (r) => r.rate.toFixed(2) },
      { label: "Sites/mo", num: true, cls: "mono", get: (r) => r.sites.toFixed(2) },
      { label: "By close", num: true, cls: "mono", get: (r) => Math.round(r.series.at(-1)) },
      {
        label: "Target met", num: true,
        get: (r) => {
          const m = reachMonth(r.series);
          if (!m) return `<span class="pill pill-red">not met</span>`;
          const late = TARGET_SCHEDULE.findIndex((t) => t.label === m) > TARGET_SCHEDULE.length - 3;
          return `<span class="pill ${late ? "pill-amber" : "pill-green"}">${m}</span>`;
        },
      },
    ],
    rows,
    {
      tip: (r) => {
        const m = reachMonth(r.series);
        return `<div class="tip-title">${r.name}</div>${row("Per site / month", r.rate.toFixed(2))}${row("New sites / month", r.sites.toFixed(2))}${row("At protocol close", Math.round(r.series.at(-1)))}${row("Target met", m || "not within window")}<div class="tip-note">Target ${TRIAL.target} by ${TRIAL.targetClose}</div>`;
      },
    }
  );
}

// ── Observed rate summary ───────────────────────────────────────────────────
function buildRateTable() {
  const monthsLeft = TARGET_SCHEDULE.length;
  const rows = [
    { k: "Randomised to date", v: totalRand },
    { k: "Remaining to target", v: remaining },
    { k: "Observed, last 3 months", v: rateRecent.toFixed(1) + " / month" },
    { k: "Observed, all time", v: rateAll.toFixed(1) + " / month" },
    { k: "Per open site", v: BASE.rate.toFixed(2) + " / month" },
    { k: "Sites opened per month", v: BASE.sites.toFixed(2) },
    { k: "Rate required", v: (remaining / monthsLeft).toFixed(1) + " / month" },
    { k: "Protocol close", v: TRIAL.targetClose },
  ];
  buildTable(
    document.getElementById("tbl-rate"),
    [
      { label: "Measure", get: (r) => r.k },
      { label: "Value", num: true, cls: "mono", get: (r) => r.v },
    ],
    rows
  );
}

// ── Controls ────────────────────────────────────────────────────────────────
const rateInput = document.getElementById("ctrl-rate");
const sitesInput = document.getElementById("ctrl-sites");

rateInput.value = projRate;
sitesInput.value = projSites;

rateInput.addEventListener("input", (e) => {
  projRate = Number(e.target.value);
  hideTip(); renderProjection();
});
sitesInput.addEventListener("input", (e) => {
  projSites = Number(e.target.value);
  hideTip(); renderProjection();
});
document.getElementById("ctrl-reset").addEventListener("click", () => {
  projRate = BASE.rate;
  projSites = BASE.sites;
  rateInput.value = projRate;
  sitesInput.value = projSites;
  hideTip(); renderProjection();
});

document.querySelectorAll("[data-projview]").forEach((b) =>
  b.addEventListener("click", () => {
    projView = b.dataset.projview;
    document.querySelectorAll("[data-projview]").forEach((x) => x.classList.toggle("on", x === b));
    hideTip(); renderProjection();
  })
);
document.querySelector("[data-projband]").addEventListener("click", (e) => {
  projBand = !projBand;
  e.currentTarget.classList.toggle("on", projBand);
  hideTip(); renderProjection();
});

renderProjection();

// ══ SITES ══════════════════════════════════════════════════════════════════
const siteTotal = recruitingSites.reduce((s, x) => s + x.n, 0);
const topSite = [...recruitingSites].sort((a, b) => b.n - a.n)[0];

fillKpis("site-kpis", [
  {
    label: "Recruiting", value: recruitingSites.length, unit: `of ${TRIAL.targetSites}`,
    pct: (recruitingSites.length / TRIAL.targetSites) * 100, colour: "#0E6B5E",
    note: `${TRIAL.targetSites - recruitingSites.length} more sites to reach the planned network.`,
  },
  {
    label: "Open, none yet", value: openSites.length - recruitingSites.length, unit: "sites",
    pct: ((openSites.length - recruitingSites.length) / TRIAL.targetSites) * 100, colour: "#B67A16",
    note: "Green-lit but yet to randomise a first participant.",
  },
  {
    label: "Dormant", value: dormantSites.length, unit: "sites",
    pct: (dormantSites.length / Math.max(recruitingSites.length, 1)) * 100, colour: "#A63D2F",
    note: dormantSites.length
      ? `${dormantSites.map((s) => s.name.split(" ").slice(0, 2).join(" ")).join(", ")} — no randomisation in 60 days.`
      : "Every recruiting site has randomised recently.",
    tip: dormantSites.length
      ? `<div class="tip-title">Dormant sites</div>${dormantSites.map((s) => row(s.name, daysSince(s.lastRand) + "d")).join("")}`
      : null,
  },
  {
    label: "Top recruiter", value: topSite.n, unit: "participants",
    pct: (topSite.n / siteTotal) * 100, colour: "#3FA593",
    note: `${topSite.name} — ${((topSite.n / siteTotal) * 100).toFixed(0)}% of all randomisations.`,
  },
]);

let siteSort = "n";
const shortName = (n) => (n.length > 30 ? n.slice(0, 29) + "…" : n);

function sortedSites() {
  const c = [...recruitingSites];
  if (siteSort === "n") return c.sort((a, b) => b.n - a.n);
  if (siteSort === "name") return c.sort((a, b) => a.name.localeCompare(b.name));
  return c.sort((a, b) => new Date(a.opened) - new Date(b.opened) || b.n - a.n);
}

const siteChart = new Chart(document.getElementById("chart-sites"), {
  type: "bar",
  data: {
    labels: sortedSites().map((s) => shortName(s.name)),
    datasets: [{ data: sortedSites().map((s) => s.n), backgroundColor: "#0E6B5E", borderRadius: 3, barThickness: 14 }],
  },
  options: {
    responsive: true, maintainAspectRatio: false, indexAxis: "y",
    interaction: { mode: "index", intersect: false },
    plugins: {
      ...noLegend,
      tooltip: {
        enabled: false,
        external: external((i) => {
          const s = sortedSites()[i];
          return `<div class="tip-title">${s.name}</div>${row("Randomised", s.n)}${row("Share", ((s.n / siteTotal) * 100).toFixed(1) + "%")}${row("Opened", s.opened)}${row("Last randomisation", s.lastRand)}<div class="tip-note">${s.region}</div>`;
        }),
      },
    },
    scales: {
      x: { beginAtZero: true, grid: { color: GRID }, border: { display: false }, ticks: { font: { family: MONO, size: 10.5 }, precision: 0 } },
      y: { grid: { display: false }, border: { color: GRID }, ticks: { font: { size: 11 } } },
    },
  },
});

document.querySelectorAll("[data-sitesort]").forEach((b) =>
  b.addEventListener("click", () => {
    siteSort = b.dataset.sitesort;
    document.querySelectorAll("[data-sitesort]").forEach((x) => x.classList.toggle("on", x === b));
    hideTip();
    const d = sortedSites();
    siteChart.data.labels = d.map((s) => shortName(s.name));
    siteChart.data.datasets[0].data = d.map((s) => s.n);
    siteChart.update();
  })
);

// Site status doughnut
const STATUSES = ["Recruiting", "Open", "Set-up", "Identified"];
const statusCounts = STATUSES.map((s) => SITES.filter((x) => x.status === s).length);
const statusColours = ["#0E6B5E", "#B67A16", "#B0AEA4", "#D9D6CC"];

new Chart(document.getElementById("chart-status"), {
  type: "doughnut",
  data: { labels: STATUSES, datasets: [{ data: statusCounts, backgroundColor: statusColours, borderWidth: 2, borderColor: "#fff" }] },
  options: {
    responsive: true, maintainAspectRatio: false, cutout: "62%",
    plugins: {
      legend: { position: "bottom", labels: { usePointStyle: true, boxWidth: 7, padding: 14, font: { size: 11.5 } } },
      tooltip: {
        enabled: false,
        external: external((i) => `<div class="tip-title">${STATUSES[i]}</div>${row("Sites", statusCounts[i])}${row("Share", Math.round((statusCounts[i] / SITES.length) * 100) + "%")}`),
      },
    },
  },
});

buildTable(
  document.getElementById("tbl-status"),
  [
    { label: "Status", get: (r) => statusPill(r.s) },
    { label: "Sites", num: true, cls: "mono", get: (r) => r.n },
    { label: "Participants", num: true, cls: "mono", get: (r) => r.p },
  ],
  STATUSES.map((s) => ({
    s, n: SITES.filter((x) => x.status === s).length,
    p: SITES.filter((x) => x.status === s).reduce((a, x) => a + x.n, 0),
  }))
);

// Full site table with filters and click-to-sort headings
let siteFilter = "all";
let siteTableSort = { col: 4, dir: "desc" };

const STATUS_ORDER = { Recruiting: 0, Open: 1, "Set-up": 2, Identified: 3 };

const SITE_COLS = [
  { label: "Site", sortKey: "name", get: (r) => r.name },
  { label: "Region", sortKey: "region", get: (r) => `<span style="color:var(--muted)">${r.region}</span>` },
  { label: "Status", sortKey: (r) => STATUS_ORDER[r.status], get: (r) => statusPill(r.status) },
  { label: "Opened", cls: "mono", sortKey: (r) => (r.opened ? new Date(r.opened).getTime() : null), get: (r) => r.opened || "—" },
  { label: "N", num: true, cls: "mono", sortKey: "n", get: (r) => r.n },
  { label: "", cls: "bar-cell", get: () => "" },
];

function renderSiteTable() {
  let rows = SITES;
  if (siteFilter === "recruiting") rows = rows.filter((s) => s.status === "Recruiting");
  if (siteFilter === "dormant") rows = rows.filter((s) => dormantSites.includes(s));

  const host = document.getElementById("tbl-sites");
  if (!rows.length) {
    host.innerHTML = `<tbody><tr><td class="empty">No sites match this filter.</td></tr></tbody>`;
    return;
  }

  const key = SITE_COLS[siteTableSort.col].sortKey;
  rows = sortRows(rows, key, siteTableSort.dir);

  const maxN = Math.max(...rows.map((s) => s.n), 1);
  const cols = SITE_COLS.map((c, i) =>
    i === 5
      ? {
          ...c,
          get: (r) => `<div class="bar-bg"><div class="bar-fill" style="width:${(r.n / maxN) * 100}%;background:${dormantSites.includes(r) ? "#B67A16" : "#0E6B5E"}"></div></div>`,
        }
      : c
  );

  buildTable(host, cols, rows, {
    sort: siteTableSort,
    onSort: (col) => {
      if (!SITE_COLS[col].sortKey) return;
      // Re-clicking the active column flips direction; a new column starts
      // descending for counts and ascending for text.
      siteTableSort =
        siteTableSort.col === col
          ? { col, dir: siteTableSort.dir === "asc" ? "desc" : "asc" }
          : { col, dir: SITE_COLS[col].num ? "desc" : "asc" };
      hideTip();
      renderSiteTable();
    },
    tip: (r) => `
      <div class="tip-title">${r.name}</div>
      ${row("Status", r.status)}
      ${row("Randomised", r.n)}
      ${row("Opened", r.opened || "not yet")}
      ${row("Last randomisation", r.lastRand || "none")}
      ${dormantSites.includes(r) ? `<div class="tip-note">Dormant — ${daysSince(r.lastRand)} days since last randomisation</div>` : ""}
    `,
  });
}

document.querySelectorAll("[data-sitefilter]").forEach((b) =>
  b.addEventListener("click", () => {
    siteFilter = b.dataset.sitefilter;
    document.querySelectorAll("[data-sitefilter]").forEach((x) => x.classList.toggle("on", x === b));
    hideTip(); renderSiteTable();
  })
);
renderSiteTable();

// ══ RANDOMISATIONS ═════════════════════════════════════════════════════════
let randFilter = "all";
const randSites = [...new Set(RANDOMISATIONS.map((r) => r.site))];

const filterHost = document.getElementById("rand-filters");
[{ k: "all", l: "All sites" }, ...randSites.slice(0, 3).map((s) => ({ k: s, l: s.split(" ").slice(0, 2).join(" ") }))]
  .forEach(({ k, l }) => {
    const b = document.createElement("button");
    b.className = "toggle" + (k === "all" ? " on" : "");
    b.textContent = l;
    b.addEventListener("click", () => {
      randFilter = k;
      [...filterHost.children].forEach((x) => x.classList.toggle("on", x === b));
      hideTip(); renderRandTable();
    });
    filterHost.appendChild(b);
  });

function renderRandTable() {
  const rows = randFilter === "all" ? RANDOMISATIONS : RANDOMISATIONS.filter((r) => r.site === randFilter);
  document.getElementById("rand-sub").textContent =
    `Showing ${rows.length} of ${RANDOMISATIONS.length} most recent · ${totalRand} randomised in total`;

  const host = document.getElementById("tbl-rand");
  if (!rows.length) {
    host.innerHTML = `<tbody><tr><td class="empty">No randomisations at this site.</td></tr></tbody>`;
    return;
  }
  buildTable(
    host,
    [
      { label: "ID", cls: "mono", get: (r) => r.id },
      { label: "Site", get: (r) => r.site },
      { label: "Randomised", cls: "mono", get: (r) => r.date },
      { label: "Age", num: true, cls: "mono", get: (r) => r.age },
      { label: "Sex", get: (r) => r.sex },
      { label: "NELA risk", num: true, cls: "mono", get: (r) => r.nela.toFixed(1) + "%" },
      { label: "Allocation", get: () => `<span class="pill pill-grey">Blinded</span>` },
    ],
    rows,
    {
      tip: (r) => `<div class="tip-title">${r.id}</div>${row("Site", r.site)}${row("Randomised", r.date)}${row("Age", r.age)}${row("Sex", r.sex)}${row("NELA risk", r.nela + "%")}<div class="tip-note">Allocation withheld — blinded view</div>`,
    }
  );
}
renderRandTable();

// Age distribution
const AGE_BINS = [
  { label: "<60", lo: 0,  hi: 60 },
  { label: "60–69", lo: 60, hi: 70 },
  { label: "70–79", lo: 70, hi: 80 },
  { label: "80+",  lo: 80, hi: 200 },
];
const ageCounts = AGE_BINS.map((b) => RANDOMISATIONS.filter((r) => r.age >= b.lo && r.age < b.hi).length);

new Chart(document.getElementById("chart-age"), {
  type: "bar",
  data: { labels: AGE_BINS.map((b) => b.label), datasets: [{ data: ageCounts, backgroundColor: "#0E6B5E", borderRadius: 3 }] },
  options: {
    responsive: true, maintainAspectRatio: false,
    interaction: { mode: "index", intersect: false },
    plugins: {
      ...noLegend,
      tooltip: {
        enabled: false,
        external: external((i) => `<div class="tip-title">Age ${AGE_BINS[i].label}</div>${row("Participants", ageCounts[i])}${row("Share", Math.round((ageCounts[i] / RANDOMISATIONS.length) * 100) + "%")}`),
      },
    },
    scales: axes(),
  },
});

// NELA risk distribution
const NELA_BINS = [
  { label: "<5%", lo: 0, hi: 5 },
  { label: "5–10%", lo: 5, hi: 10 },
  { label: "10–20%", lo: 10, hi: 20 },
  { label: "20%+", lo: 20, hi: 1000 },
];
const nelaCounts = NELA_BINS.map((b) => RANDOMISATIONS.filter((r) => r.nela >= b.lo && r.nela < b.hi).length);

new Chart(document.getElementById("chart-nela"), {
  type: "bar",
  data: { labels: NELA_BINS.map((b) => b.label), datasets: [{ data: nelaCounts, backgroundColor: "#B67A16", borderRadius: 3 }] },
  options: {
    responsive: true, maintainAspectRatio: false,
    interaction: { mode: "index", intersect: false },
    plugins: {
      ...noLegend,
      tooltip: {
        enabled: false,
        external: external((i) => `<div class="tip-title">NELA ${NELA_BINS[i].label}</div>${row("Participants", nelaCounts[i])}${row("Share", Math.round((nelaCounts[i] / RANDOMISATIONS.length) * 100) + "%")}<div class="tip-note">Predicted 30-day mortality</div>`),
      },
    },
    scales: axes(),
  },
});

// ══ FOLLOW-UP ══════════════════════════════════════════════════════════════
const worstWindow = [...FOLLOWUP].sort((a, b) => a.complete / a.expected - b.complete / b.expected)[0];
const outstanding = fuExpected - fuComplete;

fillKpis("fu-kpis", [
  {
    label: "Overall return", value: fuRate + "%", unit: "",
    pct: fuRate, colour: fuRate >= 90 ? "#0E6B5E" : "#B67A16",
    note: `${fuComplete} of ${fuExpected} forms received across all windows.`,
  },
  {
    label: "Outstanding", value: outstanding, unit: "forms",
    pct: (outstanding / fuExpected) * 100, colour: "#B67A16",
    note: "Due at the data cut but not yet received.",
  },
  {
    label: "Weakest window", value: Math.round((worstWindow.complete / worstWindow.expected) * 100) + "%", unit: "",
    pct: (worstWindow.complete / worstWindow.expected) * 100, colour: "#A63D2F",
    note: `${worstWindow.name} — ${worstWindow.expected - worstWindow.complete} forms outstanding.`,
  },
  {
    label: "Sites at 100%", value: SITE_COMPLIANCE.filter((s) => s.complete === s.due).length, unit: `of ${SITE_COMPLIANCE.length}`,
    pct: (SITE_COMPLIANCE.filter((s) => s.complete === s.due).length / SITE_COMPLIANCE.length) * 100, colour: "#0E6B5E",
    note: "Sites with no outstanding follow-up forms.",
  },
]);

fillRings("rings-full", FOLLOWUP);

// Instrument completion, per window
let fuWindow = FOLLOWUP[1].name;
const fuToggleHost = document.getElementById("fu-window-toggles");

FOLLOWUP.forEach((f) => {
  const b = document.createElement("button");
  b.className = "toggle" + (f.name === fuWindow ? " on" : "");
  b.textContent = f.name;
  b.addEventListener("click", () => {
    fuWindow = f.name;
    [...fuToggleHost.children].forEach((x) => x.classList.toggle("on", x === b));
    hideTip(); renderInstruments();
  });
  fuToggleHost.appendChild(b);
});

const instChart = new Chart(document.getElementById("chart-instruments"), {
  type: "bar",
  data: { labels: [], datasets: [] },
  options: {
    responsive: true, maintainAspectRatio: false, indexAxis: "y",
    interaction: { mode: "index", intersect: false },
    plugins: {
      ...noLegend,
      tooltip: {
        enabled: false,
        external: external((i) => {
          const w = FOLLOWUP.find((f) => f.name === fuWindow);
          const inst = w.instruments[i];
          const pct = Math.round((inst.complete / inst.expected) * 100);
          return `<div class="tip-title">${inst.label}</div>${row("Received", inst.complete)}${row("Due", inst.expected)}${row("Outstanding", inst.expected - inst.complete)}${row("Rate", pct + "%")}<div class="tip-note">${w.name} window</div>`;
        }),
      },
    },
    scales: {
      x: { beginAtZero: true, max: 100, grid: { color: GRID }, border: { display: false }, ticks: { font: { family: MONO, size: 10.5 }, callback: (v) => v + "%" } },
      y: { grid: { display: false }, border: { color: GRID }, ticks: { font: { size: 11.5 } } },
    },
  },
});

function renderInstruments() {
  const w = FOLLOWUP.find((f) => f.name === fuWindow);
  instChart.data.labels = w.instruments.map((i) => i.label);
  instChart.data.datasets = [
    {
      data: w.instruments.map((i) => Math.round((i.complete / i.expected) * 100)),
      backgroundColor: w.colour,
      borderRadius: 3,
      barThickness: 18,
    },
  ];
  instChart.update();
}
renderInstruments();

// Compliance by site
const compRows = [...SITE_COMPLIANCE].sort((a, b) => a.complete / a.due - b.complete / b.due);

buildTable(
  document.getElementById("tbl-compliance"),
  [
    { label: "Site", get: (r) => r.name },
    { label: "Received", num: true, cls: "mono", get: (r) => r.complete },
    { label: "Due", num: true, cls: "mono", get: (r) => r.due },
    {
      label: "Rate", num: true,
      get: (r) => {
        const p = Math.round((r.complete / r.due) * 100);
        const cls = p === 100 ? "pill-green" : p >= 85 ? "pill-grey" : "pill-amber";
        return `<span class="pill ${cls}">${p}%</span>`;
      },
    },
  ],
  compRows,
  {
    tip: (r) => {
      const p = Math.round((r.complete / r.due) * 100);
      return `<div class="tip-title">${r.name}</div>${row("Received", r.complete)}${row("Due", r.due)}${row("Outstanding", r.due - r.complete)}${row("Rate", p + "%")}`;
    },
  }
);

// ── Start ───────────────────────────────────────────────────────────────────
goTo("overview");

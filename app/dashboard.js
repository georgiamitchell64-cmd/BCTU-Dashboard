// ── Trial data ─────────────────────────────────────────────────────────────
// Sample figures pending a REDCap export feed.
const TRIAL_TARGET = 898;
const TARGET_SITES = 24;

const months = ["Mar 2026", "Apr 2026", "May 2026", "Jun 2026", "Jul 2026"];
const monthlyActual = [3, 7, 10, 14, 13];
const cumulativeTarget = [4, 12, 24, 36, 48];

const cumulativeActual = [];
monthlyActual.reduce((sum, v) => {
  sum += v;
  cumulativeActual.push(sum);
  return sum;
}, 0);

const monthlyTarget = cumulativeTarget.map((v, i) =>
  i === 0 ? v : v - cumulativeTarget[i - 1]
);

const sites = [
  { name: "Queen Elizabeth Hospital Birmingham", n: 8, opened: "Mar 2026" },
  { name: "University Hospital Coventry", n: 6, opened: "Mar 2026" },
  { name: "Royal Liverpool Hospital", n: 5, opened: "Apr 2026" },
  { name: "St James's Hospital Leeds", n: 5, opened: "Apr 2026" },
  { name: "Bristol Royal Infirmary", n: 4, opened: "Apr 2026" },
  { name: "Sheffield Teaching Hospital", n: 4, opened: "May 2026" },
  { name: "Oxford University Hospitals", n: 3, opened: "May 2026" },
  { name: "Newcastle Royal Victoria", n: 3, opened: "May 2026" },
  { name: "Southampton General Hospital", n: 3, opened: "Jun 2026" },
  { name: "Glasgow Royal Infirmary", n: 2, opened: "Jun 2026" },
  { name: "Nottingham University Hospital", n: 2, opened: "Jun 2026" },
  { name: "Addenbrooke's Hospital Cambridge", n: 2, opened: "Jul 2026" },
];

const completion = [
  { name: "Baseline", complete: 47, eligible: 47, color: "#7C3AED" },
  { name: "Discharge", complete: 43, eligible: 45, color: "#3B82F6" },
  { name: "Day 30", complete: 34, eligible: 38, color: "#2EC4A5" },
  { name: "Day 90", complete: 16, eligible: 20, color: "#059669" },
];

const totalRecruited = cumulativeActual[cumulativeActual.length - 1];
const thisMonth = monthlyActual[monthlyActual.length - 1];
const lastMonth = monthlyActual[monthlyActual.length - 2];
const latestTarget = cumulativeTarget[cumulativeTarget.length - 1];
const siteTotal = sites.reduce((s, x) => s + x.n, 0);

// ── Shared tooltip ─────────────────────────────────────────────────────────
const tip = document.getElementById("tip");

function showTip(html, x, y) {
  tip.innerHTML = html;
  tip.classList.add("show");
  const r = tip.getBoundingClientRect();
  let left = x + 14;
  let top = y + 14;
  if (left + r.width > window.innerWidth - 8) left = x - r.width - 14;
  if (top + r.height > window.innerHeight - 8) top = y - r.height - 14;
  tip.style.left = left + "px";
  tip.style.top = top + "px";
}

function hideTip() {
  tip.classList.remove("show");
}

function varianceHtml(actual, target) {
  const d = actual - target;
  if (d === 0) return `<span class="tip-ahead">on target</span>`;
  const cls = d > 0 ? "tip-ahead" : "tip-behind";
  const word = d > 0 ? "ahead" : "behind";
  return `<span class="${cls}">${Math.abs(d)} ${word}</span>`;
}

// ── Headline metrics ───────────────────────────────────────────────────────
document.getElementById("m-recruited").textContent = totalRecruited;
document.getElementById("m-target").textContent = TRIAL_TARGET;
document.getElementById("m-month").textContent = thisMonth;

const diff = thisMonth - lastMonth;
document.getElementById("m-month-sub").innerHTML =
  `<span class="${diff > 0 ? "up" : diff < 0 ? "down" : "even"}">` +
  `${diff > 0 ? "+" : ""}${diff}</span> vs last month`;

document.getElementById("m-sites").textContent = sites.length;
document.getElementById("m-sites-sub").textContent = `of ${TARGET_SITES} target sites`;

const pctOfTarget = Math.round((totalRecruited / latestTarget) * 100);
document.getElementById("m-ontarget").textContent = pctOfTarget + "%";
document.getElementById("m-ontarget-sub").innerHTML =
  `<span class="${pctOfTarget >= 100 ? "up" : pctOfTarget >= 80 ? "even" : "down"}">` +
  `${totalRecruited}/${latestTarget}</span> current target`;

// ── Chart defaults ─────────────────────────────────────────────────────────
Chart.defaults.font.family = "system-ui, -apple-system, sans-serif";
Chart.defaults.font.size = 12;
Chart.defaults.color = "#64748B";

// Render our own tooltip so the charts and the DOM widgets share one style.
function externalTooltip(buildHtml) {
  return (ctx) => {
    const model = ctx.tooltip;
    if (!model || model.opacity === 0) return hideTip();
    const i = model.dataPoints[0].dataIndex;
    const box = ctx.chart.canvas.getBoundingClientRect();
    showTip(buildHtml(i), box.left + model.caretX, box.top + model.caretY);
  };
}

// ── Recruitment chart (cumulative / monthly, target on / off) ──────────────
let view = "cumulative";
let showTarget = true;

function trendTooltipHtml(i) {
  const actual = view === "cumulative" ? cumulativeActual[i] : monthlyActual[i];
  const target = view === "cumulative" ? cumulativeTarget[i] : monthlyTarget[i];
  const pct = Math.round((actual / target) * 100);
  return `
    <div class="tip-title">${months[i]}</div>
    <div class="tip-row"><span>Recruited</span><b>${actual}</b></div>
    ${showTarget ? `
      <div class="tip-row"><span>Target</span><b>${target}</b></div>
      <div class="tip-row"><span>Variance</span>${varianceHtml(actual, target)}</div>
      <div class="tip-row"><span>Of target</span><b>${pct}%</b></div>` : ""}
  `;
}

function trendDatasets() {
  const actual = view === "cumulative" ? cumulativeActual : monthlyActual;
  const target = view === "cumulative" ? cumulativeTarget : monthlyTarget;
  const line = view === "cumulative";

  const sets = [];
  if (showTarget) {
    sets.push({
      label: "Target",
      data: target,
      borderColor: "#C8D4E2",
      backgroundColor: line ? "rgba(221,229,238,.18)" : "rgba(221,229,238,.55)",
      borderWidth: line ? 2 : 1,
      borderDash: line ? [6, 3] : undefined,
      pointRadius: line ? 3 : 0,
      pointHoverRadius: line ? 6 : 0,
      pointBackgroundColor: "#C8D4E2",
      fill: line,
      tension: 0.3,
      borderRadius: line ? undefined : 4,
    });
  }
  sets.push({
    label: "Recruited",
    data: actual,
    borderColor: line ? "#2EC4A5" : "#0FA88E",
    backgroundColor: line ? "rgba(46,196,165,.12)" : "#2EC4A5",
    borderWidth: line ? 2.5 : 1,
    pointRadius: line ? 4 : 0,
    pointHoverRadius: line ? 7 : 0,
    pointBackgroundColor: "#2EC4A5",
    fill: line,
    tension: 0.3,
    borderRadius: line ? undefined : 4,
  });
  return sets;
}

const trendChart = new Chart(document.getElementById("chart-trend"), {
  type: "line",
  data: { labels: months, datasets: trendDatasets() },
  options: {
    responsive: true,
    maintainAspectRatio: false,
    interaction: { mode: "index", intersect: false },
    plugins: {
      legend: { position: "bottom", labels: { usePointStyle: true, padding: 20 } },
      tooltip: { enabled: false, external: externalTooltip(trendTooltipHtml) },
    },
    scales: {
      y: { beginAtZero: true, grid: { color: "#F1F5F9" }, title: { display: true, text: "Participants" } },
      x: { grid: { display: false } },
    },
  },
});

function renderTrend() {
  trendChart.config.type = view === "cumulative" ? "line" : "bar";
  trendChart.data.datasets = trendDatasets();
  trendChart.update();
  document.getElementById("trend-title").textContent =
    view === "cumulative" ? "Cumulative Recruitment" : "Monthly Recruitment";
}

document.querySelectorAll("[data-view]").forEach((btn) => {
  btn.addEventListener("click", () => {
    view = btn.dataset.view;
    document.querySelectorAll("[data-view]").forEach((b) =>
      b.classList.toggle("on", b === btn)
    );
    hideTip();
    renderTrend();
  });
});

document.getElementById("toggle-target").addEventListener("click", (e) => {
  showTarget = !showTarget;
  e.currentTarget.classList.toggle("on", showTarget);
  hideTip();
  renderTrend();
});

// ── Site chart (sortable) ──────────────────────────────────────────────────
let siteSort = "count";

function sortedSites() {
  const copy = [...sites];
  return siteSort === "count"
    ? copy.sort((a, b) => b.n - a.n)
    : copy.sort((a, b) => a.name.localeCompare(b.name));
}

// Trim the long NHS trust names so the axis stays readable.
function shortName(name) {
  return name.length > 26 ? name.slice(0, 25) + "…" : name;
}

const siteChart = new Chart(document.getElementById("chart-sites"), {
  type: "bar",
  data: {
    labels: sortedSites().map((s) => shortName(s.name)),
    datasets: [
      {
        label: "Participants",
        data: sortedSites().map((s) => s.n),
        backgroundColor: "#2EC4A5",
        borderColor: "#0FA88E",
        borderWidth: 1,
        borderRadius: 4,
      },
    ],
  },
  options: {
    responsive: true,
    maintainAspectRatio: false,
    indexAxis: "y",
    interaction: { mode: "index", intersect: false },
    plugins: {
      legend: { display: false },
      tooltip: {
        enabled: false,
        external: externalTooltip((i) => {
          const s = sortedSites()[i];
          const share = ((s.n / siteTotal) * 100).toFixed(1);
          return `
            <div class="tip-title">${s.name}</div>
            <div class="tip-row"><span>Recruited</span><b>${s.n}</b></div>
            <div class="tip-row"><span>Share of total</span><b>${share}%</b></div>
            <div class="tip-row"><span>Opened</span><b>${s.opened}</b></div>
          `;
        }),
      },
    },
    scales: {
      x: { beginAtZero: true, grid: { color: "#F1F5F9" }, ticks: { precision: 0 } },
      y: { grid: { display: false }, ticks: { font: { size: 11 } } },
    },
  },
});

function renderSiteChart() {
  const data = sortedSites();
  siteChart.data.labels = data.map((s) => shortName(s.name));
  siteChart.data.datasets[0].data = data.map((s) => s.n);
  siteChart.update();
}

document.querySelectorAll("[data-sort]").forEach((btn) => {
  btn.addEventListener("click", () => {
    siteSort = btn.dataset.sort;
    document.querySelectorAll("[data-sort]").forEach((b) =>
      b.classList.toggle("on", b === btn)
    );
    hideTip();
    renderSiteChart();
  });
});

// ── Completion rings ───────────────────────────────────────────────────────
const grid = document.getElementById("completion-grid");
completion.forEach((tp) => {
  const pct = Math.round((tp.complete / tp.eligible) * 100);
  const r = 34, circ = 2 * Math.PI * r;
  const offset = circ - (circ * pct) / 100;
  const card = document.createElement("div");
  card.className = "completion-card";
  card.innerHTML = `
    <div class="completion-name">${tp.name}</div>
    <div class="completion-ring">
      <svg width="80" height="80" viewBox="0 0 80 80">
        <circle cx="40" cy="40" r="${r}" fill="none" stroke="#F1F5F9" stroke-width="6"/>
        <circle cx="40" cy="40" r="${r}" fill="none" stroke="${tp.color}" stroke-width="6"
                stroke-dasharray="${circ}" stroke-dashoffset="${offset}" stroke-linecap="round"/>
      </svg>
      <div class="completion-pct">${pct}%</div>
    </div>
    <div class="completion-detail">${tp.complete} / ${tp.eligible} forms</div>
  `;

  const html = `
    <div class="tip-title">${tp.name} follow-up</div>
    <div class="tip-row"><span>Complete</span><b>${tp.complete}</b></div>
    <div class="tip-row"><span>Expected</span><b>${tp.eligible}</b></div>
    <div class="tip-row"><span>Outstanding</span><b>${tp.eligible - tp.complete}</b></div>
    <div class="tip-row"><span>Rate</span><b>${pct}%</b></div>
  `;
  card.addEventListener("mousemove", (e) => showTip(html, e.clientX, e.clientY));
  card.addEventListener("mouseleave", hideTip);
  grid.appendChild(card);
});

// ── Site table ─────────────────────────────────────────────────────────────
let siteLimit = "all";
const tbody = document.getElementById("site-tbody");

function renderSiteTable() {
  const ranked = [...sites].sort((a, b) => b.n - a.n);
  const rows = siteLimit === "top5" ? ranked.slice(0, 5) : ranked;
  const maxN = ranked[0].n;

  tbody.innerHTML = "";
  rows.forEach((s, i) => {
    const share = ((s.n / siteTotal) * 100).toFixed(1);
    const tr = document.createElement("tr");
    tr.innerHTML = `
      <td class="site-rank">${i + 1}</td>
      <td>${s.name}</td>
      <td style="text-align:right;font-weight:600">${s.n}</td>
      <td style="text-align:right;color:var(--muted)">${share}%</td>
      <td class="site-bar-cell">
        <div class="site-bar-bg">
          <div class="site-bar-fill" style="width:${(s.n / maxN) * 100}%"></div>
        </div>
      </td>
    `;
    const html = `
      <div class="tip-title">${s.name}</div>
      <div class="tip-row"><span>Recruited</span><b>${s.n}</b></div>
      <div class="tip-row"><span>Share of total</span><b>${share}%</b></div>
      <div class="tip-row"><span>Opened</span><b>${s.opened}</b></div>
      <div class="tip-row"><span>Rank</span><b>${i + 1} of ${ranked.length}</b></div>
    `;
    tr.addEventListener("mousemove", (e) => showTip(html, e.clientX, e.clientY));
    tr.addEventListener("mouseleave", hideTip);
    tbody.appendChild(tr);
  });
}

document.querySelectorAll("[data-sites]").forEach((btn) => {
  btn.addEventListener("click", () => {
    siteLimit = btn.dataset.sites;
    document.querySelectorAll("[data-sites]").forEach((b) =>
      b.classList.toggle("on", b === btn)
    );
    hideTip();
    renderSiteTable();
  });
});

renderSiteTable();

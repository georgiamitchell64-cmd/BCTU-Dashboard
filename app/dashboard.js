// ── Sample data ──────────────────────────────────────────────────────────
const TRIAL_TARGET = 898;

const months = ["Mar 2026","Apr 2026","May 2026","Jun 2026","Jul 2026"];
const monthlyActual   = [3, 7, 10, 14, 13];
const cumulativeTarget = [4, 12, 24, 36, 48];
const cumulativeActual = [];
monthlyActual.reduce((sum, v) => { sum += v; cumulativeActual.push(sum); return sum; }, 0);

const totalRecruited = cumulativeActual[cumulativeActual.length - 1];
const thisMonth = monthlyActual[monthlyActual.length - 1];
const lastMonth = monthlyActual[monthlyActual.length - 2];

const sites = [
  { name: "Queen Elizabeth Hospital Birmingham", n: 8 },
  { name: "University Hospital Coventry", n: 6 },
  { name: "Royal Liverpool Hospital", n: 5 },
  { name: "St James's Hospital Leeds", n: 5 },
  { name: "Bristol Royal Infirmary", n: 4 },
  { name: "Sheffield Teaching Hospital", n: 4 },
  { name: "Oxford University Hospitals", n: 3 },
  { name: "Newcastle Royal Victoria", n: 3 },
  { name: "Southampton General Hospital", n: 3 },
  { name: "Glasgow Royal Infirmary", n: 2 },
  { name: "Nottingham University Hospital", n: 2 },
  { name: "Addenbrooke's Hospital Cambridge", n: 2 },
].sort((a, b) => b.n - a.n);

const completion = [
  { name: "Baseline",  complete: 47, eligible: 47, color: "#7C3AED" },
  { name: "Discharge", complete: 43, eligible: 45, color: "#3B82F6" },
  { name: "Day 30",    complete: 34, eligible: 38, color: "#2EC4A5" },
  { name: "Day 90",    complete: 16, eligible: 20, color: "#059669" },
];

// ── Populate metrics ─────────────────────────────────────────────────────
document.getElementById("m-recruited").textContent = totalRecruited;
document.getElementById("m-target").textContent = TRIAL_TARGET;
document.getElementById("m-month").textContent = thisMonth;

const diff = thisMonth - lastMonth;
const arrow = diff > 0 ? "up" : diff < 0 ? "down" : "even";
const sign  = diff > 0 ? "+" : "";
document.getElementById("m-month-sub").innerHTML =
  `<span class="${arrow}">${sign}${diff}</span> vs last month`;

document.getElementById("m-sites").textContent = sites.length;
document.getElementById("m-sites-sub").textContent = "of 24 target sites";

const latestTarget = cumulativeTarget[cumulativeTarget.length - 1];
const pctOfTarget = Math.round((totalRecruited / latestTarget) * 100);
document.getElementById("m-ontarget").textContent = pctOfTarget + "%";
const ontargetClass = pctOfTarget >= 100 ? "up" : pctOfTarget >= 80 ? "even" : "down";
document.getElementById("m-ontarget-sub").innerHTML =
  `<span class="${ontargetClass}">${totalRecruited}/${latestTarget}</span> current target`;

// ── Chart defaults ───────────────────────────────────────────────────────
Chart.defaults.font.family = "system-ui, -apple-system, sans-serif";
Chart.defaults.font.size = 12;
Chart.defaults.color = "#64748B";

// ── Cumulative trend chart ───────────────────────────────────────────────
new Chart(document.getElementById("chart-trend"), {
  type: "line",
  data: {
    labels: months,
    datasets: [
      {
        label: "Target",
        data: cumulativeTarget,
        borderColor: "#DDE5EE",
        backgroundColor: "rgba(221,229,238,.15)",
        borderWidth: 2,
        borderDash: [6, 3],
        pointRadius: 3,
        pointBackgroundColor: "#DDE5EE",
        fill: true,
        tension: 0.3,
      },
      {
        label: "Actual",
        data: cumulativeActual,
        borderColor: "#2EC4A5",
        backgroundColor: "rgba(46,196,165,.1)",
        borderWidth: 2.5,
        pointRadius: 4,
        pointBackgroundColor: "#2EC4A5",
        fill: true,
        tension: 0.3,
      },
    ],
  },
  options: {
    responsive: true,
    maintainAspectRatio: false,
    plugins: {
      legend: { position: "bottom", labels: { usePointStyle: true, padding: 20 } },
    },
    scales: {
      y: {
        beginAtZero: true,
        grid: { color: "#F1F5F9" },
        title: { display: true, text: "Participants" },
      },
      x: { grid: { display: false } },
    },
  },
});

// ── Monthly bar chart ────────────────────────────────────────────────────
const monthlyTarget = cumulativeTarget.map((v, i) =>
  i === 0 ? v : v - cumulativeTarget[i - 1]
);

new Chart(document.getElementById("chart-monthly"), {
  type: "bar",
  data: {
    labels: months,
    datasets: [
      {
        label: "Target",
        data: monthlyTarget,
        backgroundColor: "rgba(221,229,238,.5)",
        borderColor: "#DDE5EE",
        borderWidth: 1,
        borderRadius: 4,
      },
      {
        label: "Recruited",
        data: monthlyActual,
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
    plugins: {
      legend: { position: "bottom", labels: { usePointStyle: true, padding: 20 } },
    },
    scales: {
      y: {
        beginAtZero: true,
        grid: { color: "#F1F5F9" },
        title: { display: true, text: "Participants" },
      },
      x: { grid: { display: false } },
    },
  },
});

// ── Completion rings ─────────────────────────────────────────────────────
const grid = document.getElementById("completion-grid");
completion.forEach((tp) => {
  const pct = Math.round((tp.complete / tp.eligible) * 100);
  const r = 34, cx = 40, cy = 40, circ = 2 * Math.PI * r;
  const offset = circ - (circ * pct) / 100;
  const card = document.createElement("div");
  card.className = "completion-card";
  card.innerHTML = `
    <div class="completion-name">${tp.name}</div>
    <div class="completion-ring">
      <svg width="80" height="80" viewBox="0 0 80 80">
        <circle cx="${cx}" cy="${cy}" r="${r}" fill="none"
                stroke="#F1F5F9" stroke-width="6"/>
        <circle cx="${cx}" cy="${cy}" r="${r}" fill="none"
                stroke="${tp.color}" stroke-width="6"
                stroke-dasharray="${circ}"
                stroke-dashoffset="${offset}"
                stroke-linecap="round"/>
      </svg>
      <div class="completion-pct">${pct}%</div>
    </div>
    <div class="completion-detail">${tp.complete} / ${tp.eligible} forms</div>
  `;
  grid.appendChild(card);
});

// ── Site table ───────────────────────────────────────────────────────────
const tbody = document.getElementById("site-tbody");
const maxN = sites[0].n;
sites.forEach((s, i) => {
  const pct = (s.n / maxN) * 100;
  const tr = document.createElement("tr");
  tr.innerHTML = `
    <td class="site-rank">${i + 1}</td>
    <td>${s.name}</td>
    <td style="text-align:right;font-weight:600">${s.n}</td>
    <td class="site-bar-cell">
      <div class="site-bar-bg">
        <div class="site-bar-fill" style="width:${pct}%"></div>
      </div>
    </td>
  `;
  tbody.appendChild(tr);
});

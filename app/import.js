// ============================================================================
// Data import
// ----------------------------------------------------------------------------
// Reads the two exports the Shiny dashboard already uses and maps them onto the
// dashboard's model:
//
//   REDCap export   — long format, one row per record per event. Drives
//                     recruitment, sites and the randomisation log.
//   Return rates    — Site / Event / Form / Expected / Due / Entered. Drives
//                     follow-up return by window and by site.
//
// Everything happens in this window: files are read with the FileReader API and
// never leave the machine. The result is stored in localStorage and applied on
// the next load, so an import survives closing the app.
//
// This file must run after data.js (whose values it replaces) and before
// dashboard.js (which derives everything else from them).
// ============================================================================

const STORE_KEY = "tonic_dashboard_import_v1";

// ── CSV ─────────────────────────────────────────────────────────────────────
// Small hand-rolled parser: quoted fields, embedded commas and newlines,
// doubled quotes, CRLF, and a leading byte-order mark.
function parseCSV(text) {
  if (text.charCodeAt(0) === 0xfeff) text = text.slice(1);

  const rows = [];
  let row = [];
  let field = "";
  let quoted = false;

  for (let i = 0; i < text.length; i++) {
    const c = text[i];

    if (quoted) {
      if (c === '"') {
        if (text[i + 1] === '"') { field += '"'; i++; }
        else quoted = false;
      } else field += c;
      continue;
    }

    if (c === '"') { quoted = true; }
    else if (c === ",") { row.push(field); field = ""; }
    else if (c === "\n" || c === "\r") {
      if (c === "\r" && text[i + 1] === "\n") i++;
      row.push(field); field = "";
      if (row.some((v) => v !== "")) rows.push(row);
      row = [];
    } else field += c;
  }
  row.push(field);
  if (row.some((v) => v !== "")) rows.push(row);

  if (!rows.length) return { header: [], rows: [] };

  const header = rows[0].map((h) => h.trim());
  const out = rows.slice(1).map((r) => {
    const o = {};
    header.forEach((h, i) => { o[h] = (r[i] ?? "").trim(); });
    return o;
  });
  return { header, rows: out };
}

// Find a column by any of several candidate names, case-insensitively.
function col(header, ...names) {
  for (const n of names) {
    const hit = header.find((h) => h.toLowerCase() === n.toLowerCase());
    if (hit) return hit;
  }
  return null;
}

// ── Dates ───────────────────────────────────────────────────────────────────
const MON = ["Jan", "Feb", "Mar", "Apr", "May", "Jun", "Jul", "Aug", "Sep", "Oct", "Nov", "Dec"];

function parseDate(v) {
  if (!v) return null;
  // REDCap writes YYYY-MM-DD, optionally with a time.
  const m = String(v).match(/^(\d{4})-(\d{2})-(\d{2})/);
  if (m) return new Date(+m[1], +m[2] - 1, +m[3]);
  const d = new Date(v);
  return isNaN(d) ? null : d;
}

const fmtDate = (d) =>
  `${String(d.getDate()).padStart(2, "0")} ${MON[d.getMonth()]} ${d.getFullYear()}`;
const monthLabel = (d) => `${MON[d.getMonth()]} ${d.getFullYear()}`;
const monthKey = (d) => d.getFullYear() * 12 + d.getMonth();
const keyToLabel = (k) => `${MON[k % 12]} ${Math.floor(k / 12)}`;

// ── Protocol schedule ───────────────────────────────────────────────────────
// The monthly targets and the forward curve both come from the protocol, not
// from the export, so they are rebuilt from whatever data.js shipped with.
function protocolCurve(baseMonths, baseSchedule) {
  const curve = [];
  let cum = 0;
  baseMonths.forEach((m) => {
    cum += m.target;
    const [mon, yr] = m.label.split(" ");
    curve.push({ key: +yr * 12 + MON.indexOf(mon), cumulative: cum });
  });
  baseSchedule.forEach((t) => {
    const [mon, yr] = t.label.split(" ");
    curve.push({ key: +yr * 12 + MON.indexOf(mon), cumulative: t.cumulative });
  });
  return curve.sort((a, b) => a.key - b.key);
}

const BASE_CURVE = protocolCurve(MONTHS, TARGET_SCHEDULE);

// ── REDCap export → recruitment, sites, randomisations ──────────────────────
const REDCAP_FIELDS = {
  id:    ["record_id", "participant_id", "id"],
  event: ["redcap_event_name", "event_name"],
  site:  ["site_name", "site", "centre", "center"],
  rand:  ["rand_dttm_s", "randomisation_datetime", "rand_date", "rand_dt"],
  age:   ["cae_age", "age", "base_age"],
  sex:   ["base_sex", "sex", "gender"],
  nela:  ["base_nela_score_mort", "nela_score", "nela"],
  eth:   ["base_ethnic_gp", "ethnicity", "ethnic_group", "eth"],
};

// REDCap exports coded values, not labels. Text is unambiguous; a bare 1/2 is
// not, so we apply the usual REDCap convention and say so rather than guessing
// silently — a sex breakdown printed the wrong way round would be worse than
// one that is flagged.
let sexWasCoded = false;
function readSex(v) {
  const t = String(v || "").trim().toUpperCase();
  if (!t) return "—";
  if (t.startsWith("F")) return "F";
  if (t.startsWith("M")) return "M";
  if (t === "1" || t === "2") {
    sexWasCoded = true;
    return t === "1" ? "M" : "F";
  }
  return "—";
}

// REDCap datetimes carry the time after the date; the out-of-hours chart needs
// it, so pull it out separately rather than discarding it with the date parse.
function parseTime(v) {
  const m = String(v || "").match(/(?:^|[ T])(\d{1,2}):(\d{2})/);
  if (!m) return null;
  const h = Number(m[1]);
  return h >= 0 && h < 24 ? `${String(h).padStart(2, "0")}:${m[2]}` : null;
}

function mapRedcap(text, opts = {}) {
  const { header, rows } = parseCSV(text);
  const warnings = [];
  sexWasCoded = false;
  if (!rows.length) throw new Error("The file has a header but no data rows.");

  const f = {};
  for (const [k, names] of Object.entries(REDCAP_FIELDS)) f[k] = col(header, ...names);

  if (!f.id) throw new Error("No record_id column found — is this a REDCap export?");
  if (!f.rand) {
    throw new Error(
      "No randomisation date column found (looked for " +
      REDCAP_FIELDS.rand.join(", ") + ")."
    );
  }
  if (!f.eth) {
    warnings.push(
      "No ethnicity column found (looked for " + REDCAP_FIELDS.eth.join(", ") +
      ") — the ethnicity chart will be empty."
    );
  }
  if (!f.site) warnings.push("No site column found — every participant will be filed under “Unknown”.");

  // One record may span several event rows; take the first non-empty value of
  // each field across them.
  const byId = new Map();
  for (const r of rows) {
    const id = r[f.id];
    if (!id) continue;
    let rec = byId.get(id);
    if (!rec) { rec = { id }; byId.set(id, rec); }
    const take = (key, colName) => {
      if (rec[key] == null && colName && r[colName]) rec[key] = r[colName];
    };
    take("site", f.site);
    take("rand", f.rand);
    take("age", f.age);
    take("sex", f.sex);
    take("nela", f.nela);
    take("eth", f.eth);
  }

  const people = [];
  let undated = 0;
  for (const rec of byId.values()) {
    const d = parseDate(rec.rand);
    if (!d) { undated++; continue; }
    people.push({
      id: rec.id,
      site: rec.site || "Unknown",
      date: fmtDate(d),
      _d: d,
      age: rec.age ? Math.round(Number(rec.age)) : null,
      sex: readSex(rec.sex),
      nela: rec.nela ? Number(rec.nela) : null,
      time: parseTime(rec.rand),
      eth: rec.eth ? String(rec.eth).trim() : null,
    });
  }

  if (!people.length) throw new Error("No records carried a usable randomisation date.");
  if (undated) {
    warnings.push(
      `${undated} record${undated === 1 ? " had" : "s had"} no randomisation date and ` +
      `${undated === 1 ? "was" : "were"} skipped — presumably consented but not yet randomised.`
    );
  }

  if (sexWasCoded) {
    warnings.push(
      "Sex was exported as codes rather than labels, and has been read as " +
      "1 = Male, 2 = Female. Check that against the TONIC data dictionary — " +
      "if it is the other way round the sex column will be reversed."
    );
  }

  people.sort((a, b) => b._d - a._d);

  // ── Months ────────────────────────────────────────────────────────────────
  const first = people.at(-1)._d;
  const last = people[0]._d;
  const counts = new Map();
  people.forEach((p) => {
    const k = monthKey(p._d);
    counts.set(k, (counts.get(k) || 0) + 1);
  });

  const months = [];
  let prevCum = 0;
  for (let k = monthKey(first); k <= monthKey(last); k++) {
    const entry = BASE_CURVE.find((c) => c.key === k);
    const cum = entry ? entry.cumulative : prevCum;
    months.push({
      label: keyToLabel(k),
      short: MON[k % 12],
      randomised: counts.get(k) || 0,
      target: Math.max(0, cum - prevCum),
    });
    prevCum = cum;
  }

  // Protocol curve beyond the last month with data.
  const schedule = BASE_CURVE
    .filter((c) => c.key > monthKey(last))
    .map((c) => ({ label: keyToLabel(c.key), cumulative: c.cumulative }));

  if (!schedule.length) {
    warnings.push("The export runs past the end of the protocol schedule, so there is no forward curve left to project against.");
  }

  // ── Sites ─────────────────────────────────────────────────────────────────
  const siteAgg = new Map();
  people.forEach((p) => {
    const s = siteAgg.get(p.site) || { n: 0, last: null };
    s.n++;
    if (!s.last || p._d > s.last) s.last = p._d;
    siteAgg.set(p.site, s);
  });

  // Merge onto the hand-maintained registry: counts come from the export,
  // everything else (region, status, opening date) stays manual, and sites in
  // set-up keep their place in the denominator.
  // With replaceSites the export is taken as the whole register, which is how
  // you clear the sample sites out. Without it, sites the export has never
  // heard of are kept at zero — that is what stops a site still in set-up from
  // vanishing and quietly flattering the "n of 24 recruiting" denominator.
  const base = opts.replaceSites ? SITES.filter((s) => siteAgg.has(s.name)) : SITES;
  const sites = base.map((s) => {
    const hit = siteAgg.get(s.name);
    if (!hit) return { ...s, n: 0, lastRand: null };
    siteAgg.delete(s.name);
    return {
      ...s,
      n: hit.n,
      lastRand: fmtDate(hit.last),
      status: s.status === "Set-up" || s.status === "Identified" ? "Recruiting" : s.status,
    };
  });

  // Anything in the export that the registry has never heard of.
  const unknown = [...siteAgg.entries()];
  unknown.forEach(([name, v]) => {
    sites.push({
      name, region: "—", status: "Recruiting",
      opened: monthLabel(v.last), n: v.n, lastRand: fmtDate(v.last),
    });
  });
  if (unknown.length) {
    const one = unknown.length === 1;
    warnings.push(
      `${unknown.length} site${one ? "" : "s"} in the export ${one ? "was" : "were"} not in ` +
      `the site register and ${one ? "was" : "were"} added with no region: ` +
      unknown.map(([n]) => `“${n}”`).join(", ") +
      `. If that is a spelling difference rather than a genuinely new site, correct the ` +
      `name in data.js so the counts do not split across two entries.`
    );
  }

  if (opts.replaceSites) {
    const dropped = SITES.length - base.length;
    if (dropped) {
      warnings.push(
        `${dropped} site${dropped === 1 ? "" : "s"} in the register ${dropped === 1 ? "was" : "were"} ` +
        `not in the export and ${dropped === 1 ? "has" : "have"} been dropped. Any site that has ` +
        `opened but not yet randomised will need adding back to data.js.`
      );
    }
  }

  const total = people.length;
  return {
    warnings,
    summary: `${total} randomised across ${sites.filter((s) => s.n > 0).length} sites, ${months[0].label} to ${months.at(-1).label}`,
    data: {
      months,
      schedule,
      sites,
      randomisations: people.map(({ _d, ...p }) => p),
      trial: {
        dataCut: fmtDate(last),
        firstRandomisation: fmtDate(first),
      },
    },
  };
}

// ── Return rates → follow-up windows and per-site compliance ────────────────
const WINDOW_COLOURS = ["#0E6B5E", "#3FA593", "#7FA79F", "#B0AEA4", "#CFCCC2"];

// "day_30_arm_1" → "Day 30"; anything unrecognised is title-cased.
function prettyEvent(raw) {
  let s = String(raw).replace(/_arm_\d+$/i, "").replace(/[_-]+/g, " ").trim();
  const m = s.match(/^day\s*(\d+)$/i);
  if (m) return `Day ${m[1]}`;
  return s.replace(/\b\w/g, (c) => c.toUpperCase());
}

function eventRank(label) {
  const l = label.toLowerCase();
  if (l.startsWith("baseline")) return 0;
  if (l.startsWith("discharge")) return 1;
  const m = l.match(/day\s*(\d+)/);
  if (m) return 10 + Number(m[1]);
  return 500;
}

function mapReturnRates(text) {
  const { header, rows } = parseCSV(text);
  const warnings = [];
  if (!rows.length) throw new Error("The file has a header but no data rows.");

  const cSite = col(header, "Site", "site_name", "Centre");
  const cEvent = col(header, "Event", "redcap_event_name", "Timepoint");
  const cForm = col(header, "Form", "Instrument");
  const cDue = col(header, "Due");
  const cEntered = col(header, "Entered", "Received", "Complete");

  const missing = [];
  if (!cSite) missing.push("Site");
  if (!cEvent) missing.push("Event");
  if (!cDue) missing.push("Due");
  if (!cEntered) missing.push("Entered");
  if (missing.length) {
    throw new Error(
      `Missing column${missing.length === 1 ? "" : "s"}: ${missing.join(", ")}. ` +
      `Found: ${header.join(", ")}`
    );
  }
  if (!cForm) warnings.push("No Form column — the per-instrument breakdown will be empty.");

  const num = (v) => {
    const n = Number(String(v).replace(/[^0-9.\-]/g, ""));
    return isFinite(n) ? n : 0;
  };

  const bySite = new Map();
  const byEvent = new Map();

  for (const r of rows) {
    const due = num(r[cDue]);
    const entered = num(r[cEntered]);
    if (due === 0 && entered === 0) continue;

    const site = r[cSite] || "Unknown";
    const s = bySite.get(site) || { complete: 0, due: 0 };
    s.complete += entered; s.due += due;
    bySite.set(site, s);

    const ev = prettyEvent(r[cEvent] || "Unspecified");
    const e = byEvent.get(ev) || { complete: 0, expected: 0, forms: new Map() };
    e.complete += entered; e.expected += due;
    if (cForm && r[cForm]) {
      const fm = e.forms.get(r[cForm]) || { complete: 0, expected: 0 };
      fm.complete += entered; fm.expected += due;
      e.forms.set(r[cForm], fm);
    }
    byEvent.set(ev, e);
  }

  if (!byEvent.size) throw new Error("Every row had zero due and zero entered — nothing to show.");

  const followup = [...byEvent.entries()]
    .sort((a, b) => eventRank(a[0]) - eventRank(b[0]))
    .map(([name, v], i) => ({
      name,
      event: name,
      colour: WINDOW_COLOURS[i % WINDOW_COLOURS.length],
      complete: Math.round(v.complete),
      expected: Math.round(v.expected),
      instruments: [...v.forms.entries()].map(([label, f]) => ({
        label,
        complete: Math.round(f.complete),
        expected: Math.round(f.expected),
      })),
    }));

  const compliance = [...bySite.entries()]
    .map(([name, v]) => ({ name, complete: Math.round(v.complete), due: Math.round(v.due) }))
    .sort((a, b) => b.due - a.due);

  const totC = followup.reduce((s, f) => s + f.complete, 0);
  const totE = followup.reduce((s, f) => s + f.expected, 0);

  return {
    warnings,
    summary: `${totC} of ${totE} forms returned (${Math.round((totC / totE) * 100)}%) across ${followup.length} windows and ${compliance.length} sites`,
    data: { followup, compliance },
  };
}

// ── Persistence ─────────────────────────────────────────────────────────────
// The stored payload is encrypted at rest; auth.js decrypts it during unlock
// and hands the plaintext over on window.__STORED. Writing goes back through
// the same key, which never leaves that module.
function loadStored() {
  return window.__STORED || null;
}

async function saveStored(obj) {
  if (!window.__CRYPTO) return false;
  try {
    const blob = await window.__CRYPTO.encryptJSON(obj);
    localStorage.setItem(window.__CRYPTO.DATA_KEY, JSON.stringify(blob));
    return true;
  } catch (e) {
    return false;
  }
}

// Overwrite the values data.js declared. Runs before dashboard.js derives
// anything from them.
(function applyStored() {
  const s = loadStored();
  if (!s) return;

  if (s.months) MONTHS = s.months;
  if (s.schedule) TARGET_SCHEDULE = s.schedule;
  if (s.sites) SITES = s.sites;
  if (s.randomisations) RANDOMISATIONS = s.randomisations;
  if (s.followup) FOLLOWUP = s.followup;
  if (s.compliance) SITE_COMPLIANCE = s.compliance;
  if (s.trial) TRIAL = { ...TRIAL, ...s.trial };

  window.__IMPORT_META = s.meta || null;
})();

// ── Dialog ──────────────────────────────────────────────────────────────────
(function importUI() {
  const modal = document.getElementById("import-modal");
  const log = document.getElementById("import-log");
  const applyBtn = document.getElementById("import-apply");
  const staged = { redcap: null, rr: null };

  const open = () => { modal.hidden = false; };
  const close = () => { modal.hidden = true; };

  document.getElementById("open-import").addEventListener("click", open);
  document.getElementById("close-import").addEventListener("click", close);
  document.getElementById("import-cancel").addEventListener("click", close);
  modal.addEventListener("click", (e) => { if (e.target === modal) close(); });
  document.addEventListener("keydown", (e) => {
    if (e.key === "Escape" && !modal.hidden) close();
  });

  function render() {
    const parts = [];
    for (const [key, label] of [["redcap", "REDCap export"], ["rr", "Return rates"]]) {
      const s = staged[key];
      if (!s) continue;
      if (s.error) {
        parts.push(`<div class="err"><b>${label} — not read.</b> ${s.error}</div>`);
      } else {
        parts.push(`<div><b>${label}:</b> ${s.summary}</div>`);
        if (s.warnings?.length) {
          parts.push(`<ul class="warn">${s.warnings.map((w) => `<li>${w}</li>`).join("")}</ul>`);
        }
      }
    }
    log.innerHTML = parts.join("");
    log.hidden = !parts.length;
    applyBtn.disabled = !(
      (staged.redcap && !staged.redcap.error) || (staged.rr && !staged.rr.error)
    );
  }

  function wireDrop(id, key, mapper) {
    const el = document.getElementById(id);
    const input = el.querySelector("input");
    const state = el.querySelector("[data-state]");

    const handle = (file) => {
      if (!file) return;
      const reader = new FileReader();
      reader.onload = () => {
        try {
          const res = mapper(String(reader.result));
          staged[key] = { ...res, file: file.name };
          el.classList.remove("bad");
          el.classList.add("loaded");
          state.textContent = file.name;
        } catch (err) {
          staged[key] = { error: err.message, file: file.name };
          el.classList.remove("loaded");
          el.classList.add("bad");
          state.textContent = file.name + " — could not be read";
        }
        render();
      };
      reader.onerror = () => {
        staged[key] = { error: "The file could not be opened." };
        el.classList.add("bad");
        render();
      };
      reader.readAsText(file);
    };

    input.addEventListener("change", (e) => handle(e.target.files[0]));
    // Re-read the staged file when an option changes the mapping.
    el._rehandle = () => { if (input.files[0]) handle(input.files[0]); };
    ["dragenter", "dragover"].forEach((ev) =>
      el.addEventListener(ev, (e) => { e.preventDefault(); el.classList.add("over"); })
    );
    ["dragleave", "drop"].forEach((ev) =>
      el.addEventListener(ev, (e) => { e.preventDefault(); el.classList.remove("over"); })
    );
    el.addEventListener("drop", (e) => handle(e.dataTransfer.files[0]));
  }

  const replaceOpt = document.getElementById("opt-replace-sites");
  const redcapMapper = (text) => mapRedcap(text, { replaceSites: replaceOpt.checked });

  wireDrop("drop-redcap", "redcap", redcapMapper);
  wireDrop("drop-rr", "rr", mapReturnRates);

  replaceOpt.addEventListener("change", () => {
    const el = document.getElementById("drop-redcap");
    if (el._rehandle) el._rehandle();
  });

  applyBtn.addEventListener("click", async () => {
    const stored = loadStored() || {};
    const meta = { at: new Date().toISOString(), files: [] };

    if (staged.redcap && !staged.redcap.error) {
      Object.assign(stored, staged.redcap.data);
      meta.files.push(staged.redcap.file);
    }
    if (staged.rr && !staged.rr.error) {
      stored.followup = staged.rr.data.followup;
      stored.compliance = staged.rr.data.compliance;
      meta.files.push(staged.rr.file);
    }
    // Keep any filenames from an earlier import of the other file.
    if (stored.meta?.files) {
      meta.files = [...new Set([...meta.files, ...stored.meta.files])];
    }
    stored.meta = meta;

    applyBtn.disabled = true;
    applyBtn.textContent = "Encrypting…";
    if (!(await saveStored(stored))) {
      applyBtn.disabled = false;
      applyBtn.textContent = "Apply";
      log.hidden = false;
      log.innerHTML =
        `<div class="err"><b>Could not save.</b> The data could not be encrypted ` +
        `and written to local storage, so the import cannot be kept.</div>`;
      return;
    }
    location.reload();
  });

  document.getElementById("import-revert").addEventListener("click", () => {
    try {
      localStorage.removeItem(window.__CRYPTO ? window.__CRYPTO.DATA_KEY : STORE_KEY);
    } catch (e) { /* nothing to clear */ }
    location.reload();
  });
})();

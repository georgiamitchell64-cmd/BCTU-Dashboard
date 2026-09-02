'use strict';

// Randomisation data: reading whatever shape the trial's export happens to be
// in, and reducing it to one model the charts and merge fields work from.
//
// Three layouts are supported, because trial units export all of them:
//
//   participant  one row per randomisation   Participant · Site · Date [· Arm]
//   site-month   one row per site per month  Site · Month · Randomised
//   site-total   one row per site            Site · Randomised [· Target]
//
// Only `participant` and `site-month` carry a trend; `site-total` gives
// standings without history, and the charts degrade accordingly.

const { normaliseHeader } = require('./importer');

const PATTERNS = {
  siteId: [/^siteid$/, /^siteno$/, /^sitenumber$/, /^sitecode$/, /^centreid$/, /^centreno$/,
    /^centrenumber$/, /^centrecode$/, /^site$/, /^centre$/],
  siteName: [/^sitename$/, /^centrename$/, /^hospital$/, /^trust$/, /^site$/, /^centre$/],
  date: [/^randomisationdate$/, /^randomizationdate$/, /^randdate$/, /^daterandomised$/,
    /^daterandomized$/, /^date$/, /^registrationdate$/, /^consentdate$/],
  month: [/^month$/, /^period$/, /^yearmonth$/, /^monthyear$/],
  count: [/^randomised$/, /^randomized$/, /^recruited$/, /^count$/, /^n$/, /^total$/,
    /^participants$/, /^numberrandomised$/, /^recruitment$/],
  target: [/^target$/, /^recruitmenttarget$/, /^sitetarget$/, /^overalltarget$/, /^goal$/],
  participant: [/^participantid$/, /^participant$/, /^subjectid$/, /^patientid$/, /^trialnumber$/,
    /^screeningnumber$/, /^id$/],
};

function matchColumn(headers, patterns, taken = new Set()) {
  for (const pattern of patterns) {
    for (const header of headers) {
      if (taken.has(header)) continue;
      if (pattern.test(normaliseHeader(header))) return header;
    }
  }
  return null;
}

// Rows that are a summary of the sheet rather than a site. A trial export
// usually ends with one, and counting it as a site would put a phantom entry
// at the top of every league table.
const TOTAL_ROW = /^(total|totals|grand\s*total|all\s*sites?|sum|overall)$/i;

function isTotalRow(name) {
  return TOTAL_ROW.test(String(name || '').trim());
}

/** Headers that are themselves a month, as in a cross-tab export. */
function findMonthColumns(headers) {
  return headers
    .map((header) => ({ header, month: toMonthKey(header) }))
    .filter((entry) => entry.month !== null);
}

/** Work out which of the layouts a sheet is, and which columns are which. */
function detectRecruitmentMapping(headers) {
  const cols = headers.filter((h) => String(h || '').trim() !== '');

  // A cross-tab: one row per site, one column per month. Detected first,
  // because such a sheet also has a "Total" column that would otherwise look
  // like a plain site-total layout.
  const monthColumns = findMonthColumns(cols);
  if (monthColumns.length >= 2) {
    const taken = new Set(monthColumns.map((m) => m.header));
    const siteName = matchColumn(cols, PATTERNS.siteName, taken)
      || matchColumn(cols, PATTERNS.siteId, taken);
    if (siteName) taken.add(siteName);
    return {
      layout: 'site-month-wide',
      siteId: matchColumn(cols, PATTERNS.siteId, taken),
      siteName,
      monthColumns: monthColumns.map((m) => ({ column: m.header, month: m.month })),
      count: matchColumn(cols, PATTERNS.count, taken),
      target: matchColumn(cols, PATTERNS.target, taken),
      opened: matchColumn(cols, [/^opened$/, /^siteopen(date)?$/, /^dateopened$/, /^greenlight$/], taken),
      date: null,
      month: null,
      participant: null,
    };
  }

  const taken = new Set();

  const siteId = matchColumn(cols, PATTERNS.siteId, taken);
  if (siteId) taken.add(siteId);
  const siteName = matchColumn(cols, PATTERNS.siteName, taken);
  if (siteName) taken.add(siteName);
  const date = matchColumn(cols, PATTERNS.date, taken);
  if (date) taken.add(date);
  const month = matchColumn(cols, PATTERNS.month, taken);
  if (month) taken.add(month);
  const count = matchColumn(cols, PATTERNS.count, taken);
  if (count) taken.add(count);
  const target = matchColumn(cols, PATTERNS.target, taken);
  if (target) taken.add(target);
  const participant = matchColumn(cols, PATTERNS.participant, taken);

  // A date column means each row is an event; a count column means each row is
  // already a total. A month alongside the count makes it a monthly series.
  let layout;
  if (date && !count) layout = 'participant';
  else if (count && (month || date)) layout = 'site-month';
  else if (count) layout = 'site-total';
  else layout = 'participant';

  return {
    layout,
    siteId: siteId || null,
    siteName: siteName || null,
    date: date || null,
    month: month || null,
    count: count || null,
    target: target || null,
    participant: participant || null,
  };
}

/** "2026-07" from a date string, an Excel-ish date, or an existing month label. */
function toMonthKey(value) {
  if (value === null || value === undefined) return null;
  const text = String(value).trim();
  if (!text) return null;

  // Already a month key.
  let match = /^(\d{4})[-/](\d{1,2})$/.exec(text);
  if (match) return `${match[1]}-${String(Number(match[2])).padStart(2, '0')}`;

  // ISO date, or anything Date can read.
  match = /^(\d{4})[-/](\d{1,2})[-/](\d{1,2})/.exec(text);
  if (match) return `${match[1]}-${String(Number(match[2])).padStart(2, '0')}`;

  // UK format, which Date parses as US and would silently mis-bucket.
  match = /^(\d{1,2})[-/](\d{1,2})[-/](\d{4})/.exec(text);
  if (match) return `${match[3]}-${String(Number(match[2])).padStart(2, '0')}`;

  // "Jul 2026" / "July 2026" / "2026 Jul".
  const months = ['jan', 'feb', 'mar', 'apr', 'may', 'jun', 'jul', 'aug', 'sep', 'oct', 'nov', 'dec'];
  const named = /([a-z]{3,})[a-z]*\s+(\d{4})|(\d{4})\s+([a-z]{3,})/i.exec(text);
  if (named) {
    const name = (named[1] || named[4] || '').slice(0, 3).toLowerCase();
    const year = named[2] || named[3];
    const index = months.indexOf(name);
    if (index >= 0) return `${year}-${String(index + 1).padStart(2, '0')}`;
  }

  const parsed = new Date(text);
  if (!Number.isNaN(parsed.getTime())) {
    return `${parsed.getFullYear()}-${String(parsed.getMonth() + 1).padStart(2, '0')}`;
  }
  return null;
}

/** "2026-07" -> "Jul 26", for compact chart axes. */
function formatMonthShort(monthKey) {
  const match = /^(\d{4})-(\d{2})$/.exec(String(monthKey || ''));
  if (!match) return String(monthKey || '');
  const names = ['Jan', 'Feb', 'Mar', 'Apr', 'May', 'Jun', 'Jul', 'Aug', 'Sep', 'Oct', 'Nov', 'Dec'];
  return `${names[Number(match[2]) - 1]} ${match[1].slice(2)}`;
}

function toNumber(value) {
  if (value === null || value === undefined) return null;
  const text = String(value).replace(/[, ]/g, '').trim();
  if (!text) return null;
  const number = Number(text);
  return Number.isFinite(number) ? number : null;
}

function cellText(row, column) {
  if (!column) return '';
  const value = row[column];
  return value === null || value === undefined ? '' : String(value).trim();
}

/**
 * Reduce rows to per-site totals and monthly series.
 *
 * @returns {{sites: Array, months: string[], totals: object, layout: string,
 *            warnings: Array<{row: number, message: string}>}}
 */
function buildRecruitment(rows, mapping, options = {}) {
  const warnings = [];
  const bySite = new Map();
  const monthSet = new Set();

  rows.forEach((row, index) => {
    const rowNumber = row.__rowNumber || (options.firstDataRow || 2) + index;

    const rawId = cellText(row, mapping.siteId);
    const rawName = cellText(row, mapping.siteName);
    if (!rawId && !rawName) {
      const hasAnything = Object.values(row).some((v) => String(v ?? '').trim() !== '');
      if (hasAnything) warnings.push({ row: rowNumber, message: 'No site — row skipped' });
      return;
    }

    // The export's own summary line is not a site.
    if (isTotalRow(rawName) || isTotalRow(rawId)) return;

    const siteId = rawId || rawName;
    const key = siteId.toLowerCase();
    let site = bySite.get(key);
    if (!site) {
      site = {
        key,
        siteId,
        siteName: rawName || rawId,
        randomised: 0,
        target: null,
        opened: null,
        byMonth: new Map(),
      };
      bySite.set(key, site);
    }
    if (!site.siteName && rawName) site.siteName = rawName;

    const target = toNumber(row[mapping.target]);
    if (target !== null && site.target === null) site.target = target;

    if (mapping.layout === 'site-month-wide') {
      const opened = cellText(row, mapping.opened);
      if (opened && opened !== '-') site.opened = opened;

      let summed = 0;
      let sawAny = false;
      for (const { column, month } of mapping.monthColumns || []) {
        const raw = cellText(row, column);
        // "-" means the site was not open that month, which is different from
        // an open site that recruited nobody. Only the latter is a real zero.
        if (raw === '' || raw === '-' || raw === '—') continue;
        const value = toNumber(raw);
        if (value === null) {
          warnings.push({ row: rowNumber, message: `Unreadable value "${raw}" in ${column} for ${site.siteName}` });
          continue;
        }
        sawAny = true;
        summed += value;
        monthSet.add(month);
        site.byMonth.set(month, (site.byMonth.get(month) || 0) + value);
      }

      // Prefer the file's own total column; fall back to the summed months.
      const stated = toNumber(row[mapping.count]);
      site.randomised = stated !== null ? stated : summed;
      if (stated !== null && sawAny && stated !== summed) {
        warnings.push({
          row: rowNumber,
          message: `${site.siteName}: total column says ${stated} but the months add up to ${summed}`,
        });
      }
      return;
    }

    if (mapping.layout === 'participant') {
      // One randomisation per row; a missing date still counts to the total.
      const month = toMonthKey(row[mapping.date]);
      site.randomised += 1;
      if (month) {
        monthSet.add(month);
        site.byMonth.set(month, (site.byMonth.get(month) || 0) + 1);
      } else if (mapping.date) {
        warnings.push({ row: rowNumber, message: `Unreadable date for ${site.siteName}` });
      }
      return;
    }

    const count = toNumber(row[mapping.count]);
    if (count === null) {
      warnings.push({ row: rowNumber, message: `No number for ${site.siteName}` });
      return;
    }

    if (mapping.layout === 'site-month') {
      const month = toMonthKey(row[mapping.month] ?? row[mapping.date]);
      if (month) {
        monthSet.add(month);
        site.byMonth.set(month, (site.byMonth.get(month) || 0) + count);
      } else {
        warnings.push({ row: rowNumber, message: `Unreadable month for ${site.siteName}` });
      }
      site.randomised += count;
    } else {
      // site-total: the last value wins rather than accumulating, so a file
      // listing a site twice does not silently double its total.
      site.randomised = count;
    }
  });

  const months = [...monthSet].sort();
  const sites = [...bySite.values()].map((site) => ({
    key: site.key,
    siteId: site.siteId,
    siteName: site.siteName,
    randomised: site.randomised,
    target: site.target,
    opened: site.opened,
    monthly: months.map((month) => ({ month, count: site.byMonth.get(month) || 0 })),
  }));

  // Rank on total randomised, highest first; equal totals share a rank.
  const ordered = [...sites].sort((a, b) => b.randomised - a.randomised);
  let rank = 0;
  let previous = null;
  ordered.forEach((site, index) => {
    if (previous === null || site.randomised !== previous) rank = index + 1;
    previous = site.randomised;
    site.rank = rank;
  });
  for (const site of sites) {
    site.of = sites.length;
    site.quartile = sites.length ? Math.min(4, Math.ceil((site.rank / sites.length) * 4)) : null;
    site.percentOfTarget = site.target ? Math.round((site.randomised / site.target) * 100) : null;
  }

  const totals = {
    randomised: sites.reduce((sum, s) => sum + s.randomised, 0),
    target: sites.reduce((sum, s) => sum + (s.target || 0), 0) || null,
    siteCount: sites.length,
    monthly: months.map((month) => ({
      month,
      count: sites.reduce((sum, s) => sum + (s.monthly.find((m) => m.month === month)?.count || 0), 0),
    })),
  };

  return { sites, months, totals, layout: mapping.layout, warnings };
}

/** Match a contact-list site to a recruitment site, by ID then by name. */
function findRecruitmentSite(dataset, site) {
  if (!dataset || !dataset.sites || !site) return null;
  const byId = dataset.sites.find((s) => s.key === String(site.siteId || '').toLowerCase());
  if (byId) return byId;
  const wanted = String(site.siteName || '').trim().toLowerCase();
  if (!wanted) return null;
  return dataset.sites.find((s) => s.siteName.trim().toLowerCase() === wanted) || null;
}

/** How many of the contact list's sites can be matched to recruitment rows. */
function matchReport(dataset, sites) {
  const matched = [];
  const unmatched = [];
  for (const site of sites) {
    if (findRecruitmentSite(dataset, site)) matched.push(site.siteName);
    else unmatched.push(site.siteName);
  }
  return { matched, unmatched };
}

module.exports = {
  detectRecruitmentMapping,
  buildRecruitment,
  findRecruitmentSite,
  matchReport,
  toMonthKey,
  formatMonthShort,
  toNumber,
};

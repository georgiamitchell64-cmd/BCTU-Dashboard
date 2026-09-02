'use strict';

// Data completeness from a return-rates export: the same file the TONIC
// dashboard reads, reduced to one model the charts and merge fields work from.
//
//   long          Site · Event · Form · Expected · Due · Entered · % ...
//   site-total    Site · Due · Entered
//   site-percent  Site · % Complete
//
// Rates are calculated against forms *due*, never against total *expected*:
// "expected" counts every form a participant will eventually reach, including
// windows that have not opened, which deflates the rate. Entries made before
// a window opens are capped at due so a site cannot exceed 100%.

const { normaliseHeader } = require('./importer');

const PATTERNS = {
  siteId: [/^siteid$/, /^siteno$/, /^sitenumber$/, /^sitecode$/, /^centreid$/, /^centreno$/,
    /^centrenumber$/, /^centrecode$/, /^site$/, /^centre$/],
  siteName: [/^sitename$/, /^centrename$/, /^hospital$/, /^trust$/, /^site$/, /^centre$/],
  event: [/^event$/, /^eventname$/, /^redcapeventname$/, /^timepoint$/, /^visit$/, /^visitname$/,
    /^followup$/, /^period$/],
  form: [/^form$/, /^formname$/, /^instrument$/, /^crf$/, /^crfname$/, /^questionnaire$/,
    /^datacollectioninstrument$/],
  expected: [/^expected$/, /^nexpected$/, /^formsexpected$/, /^expectedforms$/, /^total$/],
  due: [/^due$/, /^ndue$/, /^formsdue$/, /^dueforms$/, /^expecteddue$/, /^required$/],
  entered: [/^entered$/, /^nentered$/, /^formsentered$/, /^formscompleted$/, /^completedforms$/,
    /^received$/, /^returned$/, /^enteredforms$/],
  percent: [/^dueentered$/, /^pctdue$/, /^percentdue$/, /^percentcomplete$/, /^complete$/,
    /^completed$/, /^completeness$/, /^completion$/, /^returnrate$/, /^rate$/, /^percent$/, /^pct$/],
  percentExpected: [/^expectedentered$/, /^pctexpected$/, /^percentexpected$/],
  queriesRaised: [/^queries$/, /^queriesraised$/, /^totalqueries$/, /^raised$/, /^nqueries$/,
    /^discrepancies$/, /^datequeries$/, /^queriestotal$/],
  queriesOpen: [/^openqueries$/, /^queriesopen$/, /^outstandingqueries$/, /^queriesoutstanding$/,
    /^unresolvedqueries$/, /^open$/, /^outstanding$/, /^unresolved$/],
  queriesResolved: [/^resolvedqueries$/, /^queriesresolved$/, /^closedqueries$/, /^queriesclosed$/,
    /^resolved$/, /^closed$/],
  queriesOverdue: [/^overduequeries$/, /^queriesoverdue$/, /^overdue$/, /^lateequeries$/,
    /^queriesover30days$/, /^queriesover30$/],
};

const OVERALL_ROW = /^\.?\s*(overall|total|totals|grand\s*total|all\s*sites?|sum)\s*$/i;

const THRESHOLD_GOOD = 90;
const THRESHOLD_WARN = 70;
const HISTORY_LIMIT = 12;

function matchColumn(headers, patterns, taken = new Set()) {
  for (const pattern of patterns) {
    for (const header of headers) {
      if (taken.has(header)) continue;
      if (pattern.test(normaliseHeader(header))) return header;
    }
  }
  return null;
}

function isOverallRow(name) {
  return OVERALL_ROW.test(String(name || '').trim());
}

function toNumber(value) {
  if (value === null || value === undefined) return null;
  const text = String(value).replace(/[,%\s]/g, '').trim();
  if (!text || text === '-' || text === '—') return null;
  const number = Number(text);
  return Number.isFinite(number) ? number : null;
}

function cellText(row, column) {
  if (!column) return '';
  const value = row[column];
  return value === null || value === undefined ? '' : String(value).trim();
}

function detectCompletenessMapping(headers) {
  const cols = headers.filter((h) => String(h || '').trim() !== '');
  const taken = new Set();

  // Name before ID: an export with a single "Site" column is holding a label,
  // and that label is what the charts show.
  const siteName = matchColumn(cols, PATTERNS.siteName, taken);
  if (siteName) taken.add(siteName);
  const siteId = matchColumn(cols, PATTERNS.siteId, taken);
  if (siteId) taken.add(siteId);
  const event = matchColumn(cols, PATTERNS.event, taken);
  if (event) taken.add(event);
  const form = matchColumn(cols, PATTERNS.form, taken);
  if (form) taken.add(form);
  const expected = matchColumn(cols, PATTERNS.expected, taken);
  if (expected) taken.add(expected);
  const due = matchColumn(cols, PATTERNS.due, taken);
  if (due) taken.add(due);
  const entered = matchColumn(cols, PATTERNS.entered, taken);
  if (entered) taken.add(entered);
  const percentExpected = matchColumn(cols, PATTERNS.percentExpected, taken);
  if (percentExpected) taken.add(percentExpected);
  const percent = matchColumn(cols, PATTERNS.percent, taken);
  if (percent) taken.add(percent);

  // Optional: data-quality columns. Trial units often put queries in the same
  // export as return rates, and they answer the same question about a site.
  const queriesRaised = matchColumn(cols, PATTERNS.queriesRaised, taken);
  if (queriesRaised) taken.add(queriesRaised);
  const queriesOpen = matchColumn(cols, PATTERNS.queriesOpen, taken);
  if (queriesOpen) taken.add(queriesOpen);
  const queriesResolved = matchColumn(cols, PATTERNS.queriesResolved, taken);
  if (queriesResolved) taken.add(queriesResolved);
  const queriesOverdue = matchColumn(cols, PATTERNS.queriesOverdue, taken);

  const hasCounts = Boolean(entered && (due || expected));
  let layout;
  if (hasCounts && (event || form)) layout = 'long';
  else if (hasCounts) layout = 'site-total';
  else layout = 'site-percent';

  return {
    layout,
    basis: due ? 'due' : 'expected',
    siteId: siteId || null,
    siteName: siteName || null,
    event: event || null,
    form: form || null,
    expected: expected || null,
    due: due || null,
    entered: entered || null,
    percent: percent || null,
    queriesRaised: queriesRaised || null,
    queriesOpen: queriesOpen || null,
    queriesResolved: queriesResolved || null,
    queriesOverdue: queriesOverdue || null,
  };
}

function hasQueryColumns(mapping) {
  return Boolean(mapping
    && (mapping.queriesRaised || mapping.queriesOpen || mapping.queriesResolved
      || mapping.queriesOverdue));
}

function rate(entered, denominator) {
  if (!denominator || denominator <= 0) return null;
  return Math.min(entered, denominator) / denominator * 100;
}

function round1(value) {
  return value === null || value === undefined ? null : Math.round(value * 10) / 10;
}

function bucketAdd(map, key, due, expected, entered) {
  if (!key) return;
  let bucket = map.get(key);
  if (!bucket) {
    bucket = { name: key, due: 0, expected: 0, entered: 0 };
    map.set(key, bucket);
  }
  bucket.due += due;
  bucket.expected += expected;
  bucket.entered += entered;
}

function finishBuckets(map, basis, order) {
  const rows = [...map.values()].map((bucket) => {
    const denominator = basis === 'expected' ? bucket.expected : bucket.due;
    return {
      name: bucket.name,
      due: bucket.due,
      expected: bucket.expected,
      entered: bucket.entered,
      outstanding: Math.max(0, denominator - bucket.entered),
      percent: round1(rate(bucket.entered, denominator)),
    };
  });
  if (order && order.length) {
    const index = new Map(order.map((name, i) => [name, i]));
    rows.sort((a, b) => (index.get(a.name) ?? 999) - (index.get(b.name) ?? 999));
  }
  return rows;
}

/**
 * Reduce rows to per-site completeness, with an event and form breakdown.
 *
 * @returns {{sites: Array, events: string[], forms: string[], totals: object,
 *            layout: string, basis: string, warnings: Array}}
 */
function buildCompleteness(rows, mapping, options = {}) {
  const warnings = [];
  const bySite = new Map();
  const eventOrder = [];
  const formOrder = [];
  const basis = mapping.basis === 'expected' ? 'expected' : 'due';
  const queries = hasQueryColumns(mapping);
  let overall = null;

  rows.forEach((row, index) => {
    const rowNumber = row.__rowNumber || (options.firstDataRow || 2) + index;

    const rawId = cellText(row, mapping.siteId);
    const rawName = cellText(row, mapping.siteName);
    if (!rawId && !rawName) {
      const hasAnything = Object.values(row).some((v) => String(v ?? '').trim() !== '');
      if (hasAnything) warnings.push({ row: rowNumber, message: 'No site — row skipped' });
      return;
    }

    const eventName = cellText(row, mapping.event);
    const formName = cellText(row, mapping.form);
    if (eventName && !eventOrder.includes(eventName)) eventOrder.push(eventName);
    if (formName && !formOrder.includes(formName)) formOrder.push(formName);

    const due = toNumber(row[mapping.due]) || 0;
    const expected = toNumber(row[mapping.expected]) || 0;
    const entered = toNumber(row[mapping.entered]) || 0;

    // The export's own ".Overall" lines are the trial figure, not a site.
    if (isOverallRow(rawName) || isOverallRow(rawId)) {
      if (mapping.layout === 'site-percent') {
        overall = overall || { due: 0, expected: 0, entered: 0, percent: toNumber(row[mapping.percent]) };
      } else {
        overall = overall || { due: 0, expected: 0, entered: 0, percent: null };
        overall.due += due;
        overall.expected += expected;
        overall.entered += entered;
      }
      return;
    }

    const siteId = rawId || rawName;
    const key = siteId.toLowerCase();
    let site = bySite.get(key);
    if (!site) {
      site = {
        key,
        siteId,
        siteName: rawName || rawId,
        due: 0,
        expected: 0,
        entered: 0,
        statedPercent: null,
        queriesRaised: 0,
        queriesOpen: 0,
        queriesResolved: 0,
        queriesOverdue: 0,
        sawQueries: false,
        byEvent: new Map(),
        byForm: new Map(),
      };
      bySite.set(key, site);
    }
    if (!site.siteName && rawName) site.siteName = rawName;

    if (queries) {
      const raised = toNumber(row[mapping.queriesRaised]);
      const open = toNumber(row[mapping.queriesOpen]);
      const resolved = toNumber(row[mapping.queriesResolved]);
      const overdue = toNumber(row[mapping.queriesOverdue]);
      if (raised !== null || open !== null || resolved !== null || overdue !== null) {
        site.sawQueries = true;
        site.queriesRaised += raised || 0;
        site.queriesOpen += open || 0;
        site.queriesResolved += resolved || 0;
        site.queriesOverdue += overdue || 0;
      }
    }

    if (mapping.layout === 'site-percent') {
      const stated = toNumber(row[mapping.percent]);
      if (stated === null) {
        warnings.push({ row: rowNumber, message: `No completeness figure for ${site.siteName}` });
        return;
      }
      site.statedPercent = stated;
      return;
    }

    if (!mapping.entered) {
      warnings.push({ row: rowNumber, message: 'No "entered" column mapped' });
      return;
    }

    site.due += due;
    site.expected += expected;
    site.entered += entered;
    bucketAdd(site.byEvent, eventName, due, expected, entered);
    bucketAdd(site.byForm, formName, due, expected, entered);
  });

  const sites = [...bySite.values()].map((site) => {
    const denominator = basis === 'expected' ? site.expected : site.due;
    const percent = site.statedPercent !== null
      ? round1(site.statedPercent)
      : round1(rate(site.entered, denominator));
    return {
      key: site.key,
      siteId: site.siteId,
      siteName: site.siteName,
      due: site.due,
      expected: site.expected,
      entered: site.entered,
      outstanding: Math.max(0, denominator - site.entered),
      percent,
      // Queries raised is not always exported; when it is missing, open plus
      // resolved is the same number, so the resolution rate still holds.
      queriesRaised: site.queriesRaised || (site.queriesOpen + site.queriesResolved),
      queriesOpen: site.queriesOpen,
      queriesResolved: site.queriesResolved,
      queriesOverdue: site.queriesOverdue,
      hasQueries: site.sawQueries,
      byEvent: finishBuckets(site.byEvent, basis, eventOrder),
      byForm: finishBuckets(site.byForm, basis, formOrder),
    };
  });

  for (const site of sites) {
    site.queryResolvedPercent = site.queriesRaised > 0
      ? round1(Math.min(site.queriesResolved, site.queriesRaised) / site.queriesRaised * 100)
      : null;
  }

  // Sites are ranked on the share of queries they have closed, not on the raw
  // number outstanding: a large site would otherwise always look the worst.
  const withQueries = sites.filter((s) => s.hasQueries && s.queryResolvedPercent !== null);
  const queryOrdered = [...withQueries].sort((a, b) => b.queryResolvedPercent - a.queryResolvedPercent
    || a.queriesOpen - b.queriesOpen || a.siteName.localeCompare(b.siteName));
  let queryRank = 0;
  let previousQuery = null;
  queryOrdered.forEach((site, index) => {
    if (previousQuery === null || site.queryResolvedPercent !== previousQuery) queryRank = index + 1;
    previousQuery = site.queryResolvedPercent;
    site.queryRank = queryRank;
    site.queryOf = queryOrdered.length;
  });

  // Highest completeness first; sites with nothing due yet cannot be ranked
  // against sites that have, so they sit outside the table.
  const rankable = sites.filter((s) => s.percent !== null);
  const ordered = [...rankable].sort((a, b) => b.percent - a.percent
    || a.siteName.localeCompare(b.siteName));
  let rank = 0;
  let previous = null;
  ordered.forEach((site, index) => {
    if (previous === null || site.percent !== previous) rank = index + 1;
    previous = site.percent;
    site.rank = rank;
  });
  for (const site of sites) {
    site.of = rankable.length;
    if (site.percent === null) {
      site.rank = null;
      site.quartile = null;
      continue;
    }
    site.quartile = rankable.length ? Math.min(4, Math.ceil((site.rank / rankable.length) * 4)) : null;
  }

  const summedDue = sites.reduce((sum, s) => sum + s.due, 0);
  const summedExpected = sites.reduce((sum, s) => sum + s.expected, 0);
  const summedEntered = sites.reduce((sum, s) => sum + s.entered, 0);
  const summedDenominator = basis === 'expected' ? summedExpected : summedDue;

  // Prefer the export's own overall lines; fall back to the summed sites.
  let trialPercent = overall && overall.percent !== null && overall.percent !== undefined
    ? round1(overall.percent)
    : null;
  if (trialPercent === null && overall) {
    const denominator = basis === 'expected' ? overall.expected : overall.due;
    trialPercent = round1(rate(overall.entered, denominator));
  }
  const summedPercent = round1(rate(summedEntered, summedDenominator));
  if (trialPercent === null) trialPercent = summedPercent;
  else if (summedPercent !== null && Math.abs(trialPercent - summedPercent) > 1) {
    warnings.push({
      row: 0,
      message: `The file's overall figure is ${trialPercent}% but the sites add up to ${summedPercent}%`,
    });
  }

  const meanOfSites = rankable.length
    ? round1(rankable.reduce((sum, s) => sum + s.percent, 0) / rankable.length)
    : null;

  const eventTotals = new Map();
  const formTotals = new Map();
  for (const site of sites) {
    for (const row of site.byEvent) bucketAdd(eventTotals, row.name, row.due, row.expected, row.entered);
    for (const row of site.byForm) bucketAdd(formTotals, row.name, row.due, row.expected, row.entered);
  }

  const totals = {
    due: overall && overall.due ? overall.due : summedDue,
    expected: overall && overall.expected ? overall.expected : summedExpected,
    entered: overall && overall.entered ? overall.entered : summedEntered,
    percent: trialPercent,
    meanPercent: meanOfSites,
    siteCount: sites.length,
    rankedCount: rankable.length,
    leader: ordered.length ? { key: ordered[0].key, siteName: ordered[0].siteName, percent: ordered[0].percent } : null,
    byEvent: finishBuckets(eventTotals, basis, eventOrder),
    byForm: finishBuckets(formTotals, basis, formOrder),
    queriesRaised: sites.reduce((sum, s) => sum + s.queriesRaised, 0),
    queriesOpen: sites.reduce((sum, s) => sum + s.queriesOpen, 0),
    queriesResolved: sites.reduce((sum, s) => sum + s.queriesResolved, 0),
    queriesOverdue: sites.reduce((sum, s) => sum + s.queriesOverdue, 0),
  };
  totals.queryResolvedPercent = totals.queriesRaised > 0
    ? round1(Math.min(totals.queriesResolved, totals.queriesRaised) / totals.queriesRaised * 100)
    : null;
  totals.queryLeader = queryOrdered.length
    ? {
      key: queryOrdered[0].key,
      siteName: queryOrdered[0].siteName,
      percent: queryOrdered[0].queryResolvedPercent,
    }
    : null;
  totals.outstanding = Math.max(0, (basis === 'expected' ? totals.expected : totals.due) - totals.entered);

  return {
    sites,
    events: eventOrder,
    forms: formOrder,
    totals,
    layout: mapping.layout,
    basis,
    hasQueries: withQueries.length > 0,
    fromOverallRows: Boolean(overall),
    warnings,
  };
}

/** Match a contact-list site to a completeness row, by ID then by name. */
function findCompletenessSite(dataset, site) {
  if (!dataset || !dataset.sites || !site) return null;
  const byId = dataset.sites.find((s) => s.key === String(site.siteId || '').toLowerCase());
  if (byId) return byId;
  const wanted = String(site.siteName || '').trim().toLowerCase();
  if (!wanted) return null;
  return dataset.sites.find((s) => s.siteName.trim().toLowerCase() === wanted) || null;
}

function completenessMatchReport(dataset, sites) {
  const matched = [];
  const unmatched = [];
  for (const site of sites) {
    if (findCompletenessSite(dataset, site)) matched.push(site.siteName);
    else unmatched.push(site.siteName);
  }
  return { matched, unmatched };
}

/** A compact record of one import, kept so the next one can show movement. */
function snapshotCompleteness(dataset, importedAt = new Date().toISOString()) {
  if (!dataset || !dataset.sites) return null;
  return {
    importedAt,
    percent: dataset.totals ? dataset.totals.percent : null,
    sites: dataset.sites.map((s) => ({ key: s.key, percent: s.percent, rank: s.rank })),
  };
}

/**
 * Annotate a freshly built dataset with movement since the previous import,
 * which is what makes "up two places this month" possible.
 */
function applyCompletenessHistory(dataset, history = []) {
  if (!dataset || !dataset.sites) return dataset;
  const trimmed = history.slice(-HISTORY_LIMIT);
  const last = trimmed.length ? trimmed[trimmed.length - 1] : null;

  for (const site of dataset.sites) {
    const before = last ? last.sites.find((s) => s.key === site.key) : null;
    site.previousPercent = before && before.percent !== null ? before.percent : null;
    site.previousRank = before && before.rank ? before.rank : null;
    site.percentChange = site.percent !== null && site.previousPercent !== null
      ? round1(site.percent - site.previousPercent)
      : null;
    // A smaller rank number is better, so an improvement is a positive change.
    site.rankChange = site.rank && site.previousRank ? site.previousRank - site.rank : null;
    site.history = trimmed
      .map((entry) => {
        const found = entry.sites.find((s) => s.key === site.key);
        return found && found.percent !== null
          ? { importedAt: entry.importedAt, percent: found.percent }
          : null;
      })
      .filter(Boolean);
  }

  dataset.previous = last ? { importedAt: last.importedAt, percent: last.percent } : null;
  dataset.totals.percentChange = last && last.percent !== null && dataset.totals.percent !== null
    ? round1(dataset.totals.percent - last.percent)
    : null;
  dataset.history = trimmed.map((entry) => ({ importedAt: entry.importedAt, percent: entry.percent }));
  return dataset;
}

/** Where a site sits relative to its peers: the competitive figures. */
function standing(site, dataset) {
  if (!site || site.percent === null || !dataset || !dataset.totals) return null;
  const ranked = dataset.sites.filter((s) => s.percent !== null).sort((a, b) => b.percent - a.percent);
  const leader = ranked[0] || null;
  const above = ranked.filter((s) => s.rank < site.rank);
  const next = above.length ? above[above.length - 1] : null;

  return {
    percent: site.percent,
    rank: site.rank,
    of: dataset.totals.rankedCount,
    quartile: site.quartile,
    isLeader: Boolean(leader && leader.key === site.key),
    inTopThree: site.rank <= 3 && dataset.totals.rankedCount > 3,
    leaderPercent: leader ? leader.percent : null,
    leaderName: leader ? leader.siteName : null,
    gapToTop: leader ? round1(Math.max(0, leader.percent - site.percent)) : null,
    gapToNext: next ? round1(Math.max(0, next.percent - site.percent)) : null,
    nextRank: next ? next.rank : null,
    trialPercent: dataset.totals.percent,
    meanPercent: dataset.totals.meanPercent,
    vsAverage: dataset.totals.meanPercent !== null
      ? round1(site.percent - dataset.totals.meanPercent)
      : null,
    percentChange: site.percentChange ?? null,
    rankChange: site.rankChange ?? null,
    status: site.percent >= THRESHOLD_GOOD ? 'on target'
      : (site.percent >= THRESHOLD_WARN ? 'needs attention' : 'below target'),
  };
}

/** The same comparison, for outstanding data queries. */
function queryStanding(site, dataset) {
  if (!site || !site.hasQueries || !dataset || !dataset.totals) return null;
  const ranked = dataset.sites
    .filter((s) => s.hasQueries && s.queryResolvedPercent !== null)
    .sort((a, b) => b.queryResolvedPercent - a.queryResolvedPercent);
  const leader = ranked[0] || null;

  return {
    raised: site.queriesRaised,
    open: site.queriesOpen,
    resolved: site.queriesResolved,
    overdue: site.queriesOverdue,
    resolvedPercent: site.queryResolvedPercent,
    rank: site.queryRank || null,
    of: site.queryOf || ranked.length,
    trialOpen: dataset.totals.queriesOpen,
    trialResolvedPercent: dataset.totals.queryResolvedPercent,
    leaderName: leader ? leader.siteName : null,
    shareOfOpen: dataset.totals.queriesOpen > 0
      ? round1(site.queriesOpen / dataset.totals.queriesOpen * 100)
      : null,
  };
}

module.exports = {
  PATTERNS,
  THRESHOLD_GOOD,
  THRESHOLD_WARN,
  HISTORY_LIMIT,
  detectCompletenessMapping,
  buildCompleteness,
  findCompletenessSite,
  completenessMatchReport,
  snapshotCompleteness,
  applyCompletenessHistory,
  standing,
  queryStanding,
  hasQueryColumns,
  isOverallRow,
  toNumber,
  round1,
};

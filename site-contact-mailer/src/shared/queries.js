'use strict';

// Data queries from a REDCap Data Query Resolution export: one row per query,
// reduced to per-site standing, ageing, and the list of what is outstanding.
//
// The export this is written against:
//
//   ID (# of comments) · Site (DAG) · TNo · Event · Form/DQR · Instance ·
//   Question · Data Category · Date opened · Opened by · First Comment ·
//   Date of last comment · Last comment by · Last Comment · Query Status ·
//   Assigned To · If OPEN, Duration · Responded & OPEN
//
// Only the site and status columns are needed; everything else sharpens the
// picture where it is present.

const { normaliseHeader, matchSiteIn } = require('./importer');

const PATTERNS = {
  queryId: [/^id$/, /^idofcomments$/, /^queryid$/, /^dqrid$/, /^recordid$/],
  siteName: [/^sitedag$/, /^site$/, /^dag$/, /^dataaccessgroup$/, /^centre$/, /^sitename$/,
    /^centrename$/, /^hospital$/, /^trust$/],
  siteId: [/^siteid$/, /^siteno$/, /^sitecode$/, /^centreno$/, /^centrecode$/, /^dagname$/],
  participant: [/^tno$/, /^trialnumber$/, /^trialno$/, /^participantid$/, /^participant$/,
    /^subjectid$/, /^patientid$/, /^recordid$/],
  event: [/^event$/, /^eventname$/, /^redcapeventname$/, /^timepoint$/, /^visit$/],
  form: [/^formdqr$/, /^form$/, /^formname$/, /^instrument$/, /^crf$/, /^dqr$/],
  instance: [/^instance$/, /^repeatinstance$/, /^redcaprepeatinstance$/],
  question: [/^question$/, /^field$/, /^fieldname$/, /^variable$/, /^item$/],
  category: [/^datacategory$/, /^category$/, /^querycategory$/, /^querytype$/, /^type$/,
    /^reason$/],
  opened: [/^dateopened$/, /^opened$/, /^dateraised$/, /^raised$/, /^dateofquery$/,
    /^querydate$/, /^createddate$/],
  lastComment: [/^dateoflastcomment$/, /^lastcommentdate$/, /^datelastcomment$/,
    /^dateofresponse$/, /^lastupdated$/],
  status: [/^querystatus$/, /^status$/, /^state$/, /^resolution$/, /^resolutionstatus$/],
  assignedTo: [/^assignedto$/, /^assigned$/, /^owner$/, /^responsible$/],
  duration: [/^ifopenduration$/, /^duration$/, /^daysopen$/, /^ageindays$/, /^age$/, /^dayssinceopened$/],
  responded: [/^respondedopen$/, /^responded$/, /^respondedandopen$/, /^siteresponded$/],
  openedBy: [/^openedby$/, /^raisedby$/, /^createdby$/],
  lastCommentBy: [/^lastcommentby$/, /^respondedby$/],
};

// Anything not listed is reported as unrecognised rather than guessed at: a
// status silently counted the wrong way would put a wrong number in an email.
const CLOSED_STATUSES = /^(closed|close|resolved|resolve|complete|completed|verified|answered|actioned|cancelled|canceled|withdrawn|na|n\/a|no action|no action required)$/i;
const OPEN_STATUSES = /^(open|opened|outstanding|unresolved|pending|awaiting|awaiting response|in progress|active|new|reopened|re-opened|query raised)$/i;

const DEFAULT_OVERDUE_DAYS = 30;
const AGE_BANDS = [
  { label: '0-7 days', min: 0, max: 7 },
  { label: '8-14 days', min: 8, max: 14 },
  { label: '15-30 days', min: 15, max: 30 },
  { label: 'Over 30 days', min: 31, max: Infinity },
];
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

function cellText(row, column) {
  if (!column) return '';
  const value = row[column];
  return value === null || value === undefined ? '' : String(value).trim();
}

function toNumber(value) {
  if (value === null || value === undefined) return null;
  const text = String(value).replace(/[, ]/g, '').replace(/days?$/i, '').trim();
  if (!text || text === '-' || text === '—') return null;
  const number = Number(text);
  return Number.isFinite(number) ? number : null;
}

/** A yes-ish cell: "Yes", "Y", "TRUE", "1", a tick. */
function isYes(value) {
  const text = String(value ?? '').trim().toLowerCase();
  if (!text) return false;
  return /^(y|yes|true|1|x|✓|✔|open|responded)$/.test(text);
}

/**
 * A date, read as UK order.
 *
 * `new Date("07/04/2026")` is April in Britain and July in the parser, so the
 * day-first forms are matched explicitly before anything is handed over.
 */
function parseDate(value) {
  if (value === null || value === undefined || value === '') return null;
  if (value instanceof Date) return Number.isNaN(value.getTime()) ? null : value;

  const text = String(value).trim();
  if (!text) return null;

  // Four digits before two: the alternation is ordered, so `\d{2}` first would
  // read "03-08-2026" as the year 2020 and leave the "26" behind.
  let match = /^(\d{1,2})[-/.](\d{1,2})[-/.](\d{4}|\d{2})(?!\d)/.exec(text);
  if (match) {
    const year = match[3].length === 2 ? 2000 + Number(match[3]) : Number(match[3]);
    const date = new Date(Date.UTC(year, Number(match[2]) - 1, Number(match[1])));
    return Number.isNaN(date.getTime()) ? null : date;
  }

  match = /^(\d{4})[-/.](\d{1,2})[-/.](\d{1,2})/.exec(text);
  if (match) {
    const date = new Date(Date.UTC(Number(match[1]), Number(match[2]) - 1, Number(match[3])));
    return Number.isNaN(date.getTime()) ? null : date;
  }

  const parsed = new Date(text);
  return Number.isNaN(parsed.getTime()) ? null : parsed;
}

function daysBetween(from, to) {
  if (!from || !to) return null;
  return Math.max(0, Math.round((to.getTime() - from.getTime()) / 86400000));
}

function formatDate(date) {
  if (!date) return '';
  return date.toLocaleDateString('en-GB', { day: 'numeric', month: 'short', year: 'numeric' });
}

function detectQueryMapping(headers) {
  const cols = headers.filter((h) => String(h || '').trim() !== '');
  const taken = new Set();
  const take = (patterns) => {
    const found = matchColumn(cols, patterns, taken);
    if (found) taken.add(found);
    return found || null;
  };

  // Site first: "Site (DAG)" would otherwise be claimed by a looser pattern.
  const siteName = take(PATTERNS.siteName);
  const siteId = take(PATTERNS.siteId);
  const status = take(PATTERNS.status);
  const opened = take(PATTERNS.opened);
  const lastComment = take(PATTERNS.lastComment);
  const duration = take(PATTERNS.duration);
  const responded = take(PATTERNS.responded);
  const form = take(PATTERNS.form);
  const event = take(PATTERNS.event);
  const category = take(PATTERNS.category);
  const question = take(PATTERNS.question);
  const participant = take(PATTERNS.participant);
  const instance = take(PATTERNS.instance);
  const assignedTo = take(PATTERNS.assignedTo);
  const openedBy = take(PATTERNS.openedBy);
  const lastCommentBy = take(PATTERNS.lastCommentBy);
  const queryId = take(PATTERNS.queryId);

  return {
    layout: 'per-query',
    overdueDays: DEFAULT_OVERDUE_DAYS,
    siteName,
    siteId,
    status,
    opened,
    lastComment,
    duration,
    responded,
    form,
    event,
    category,
    question,
    participant,
    instance,
    assignedTo,
    openedBy,
    lastCommentBy,
    queryId,
  };
}

/** Open, closed, or neither — `null` when the status is not one we know. */
function classifyStatus(text) {
  const value = String(text || '').trim();
  if (!value) return null;
  if (CLOSED_STATUSES.test(value)) return 'closed';
  if (OPEN_STATUSES.test(value)) return 'open';
  return null;
}

function bump(map, key) {
  if (!key) return;
  map.set(key, (map.get(key) || 0) + 1);
}

function topOf(map) {
  let best = null;
  for (const [name, count] of map) {
    if (!best || count > best.count) best = { name, count };
  }
  return best;
}

function ageBandsFor(open) {
  return AGE_BANDS.map((band) => ({
    name: band.label,
    count: open.filter((q) => q.days !== null && q.days >= band.min && q.days <= band.max).length,
  }));
}

function median(values) {
  if (values.length === 0) return null;
  const sorted = [...values].sort((a, b) => a - b);
  const middle = Math.floor(sorted.length / 2);
  return sorted.length % 2
    ? sorted[middle]
    : Math.round((sorted[middle - 1] + sorted[middle]) / 2);
}

/**
 * Reduce one row per query to per-site standing.
 *
 * @returns {{sites: Array, totals: object, statuses: Array, warnings: Array}}
 */
function buildQueries(rows, mapping, options = {}) {
  const warnings = [];
  const bySite = new Map();
  const statusCounts = new Map();
  const unknownStatuses = new Set();
  const overdueDays = Number(mapping.overdueDays) || DEFAULT_OVERDUE_DAYS;
  const today = options.today || new Date();

  rows.forEach((row, index) => {
    const rowNumber = row.__rowNumber || (options.firstDataRow || 2) + index;

    const rawName = cellText(row, mapping.siteName);
    const rawId = cellText(row, mapping.siteId);
    if (!rawName && !rawId) {
      const hasAnything = Object.values(row).some((v) => String(v ?? '').trim() !== '');
      if (hasAnything) warnings.push({ row: rowNumber, message: 'No site — query skipped' });
      return;
    }

    const statusText = cellText(row, mapping.status);
    const state = classifyStatus(statusText);
    if (statusText) bump(statusCounts, statusText);
    if (statusText && state === null) unknownStatuses.add(statusText);

    const siteId = rawName || rawId;
    const key = siteId.toLowerCase();
    let site = bySite.get(key);
    if (!site) {
      site = {
        key,
        siteId: rawId || rawName,
        siteName: rawName || rawId,
        queries: [],
        byForm: new Map(),
        byEvent: new Map(),
        byCategory: new Map(),
      };
      bySite.set(key, site);
    }

    const openedDate = parseDate(row[mapping.opened]);
    // The export's own duration is authoritative where it has one; otherwise
    // age it from the opening date, which is what that column is counting.
    const stated = toNumber(row[mapping.duration]);
    const days = stated !== null ? stated : daysBetween(openedDate, today);

    const query = {
      id: cellText(row, mapping.queryId),
      participant: cellText(row, mapping.participant),
      event: cellText(row, mapping.event),
      form: cellText(row, mapping.form),
      instance: cellText(row, mapping.instance),
      question: cellText(row, mapping.question),
      category: cellText(row, mapping.category),
      assignedTo: cellText(row, mapping.assignedTo),
      status: statusText,
      state,
      opened: openedDate,
      openedText: formatDate(openedDate),
      lastComment: parseDate(row[mapping.lastComment]),
      days: state === 'closed' ? null : days,
      responded: isYes(row[mapping.responded]),
    };
    site.queries.push(query);

    // Breakdowns count what is outstanding: a closed query is not a problem.
    if (state !== 'closed') {
      bump(site.byForm, query.form);
      bump(site.byEvent, query.event);
      bump(site.byCategory, query.category);
    }
  });

  if (unknownStatuses.size) {
    warnings.push({
      row: 0,
      message: `Status not recognised, so counted as open: ${[...unknownStatuses].slice(0, 5).join(', ')}`,
    });
  }

  const sites = [...bySite.values()].map((site) => {
    // An unrecognised status counts as open: an outstanding query wrongly
    // called closed disappears from the chase, which is the worse mistake.
    const open = site.queries.filter((q) => q.state !== 'closed');
    const closed = site.queries.filter((q) => q.state === 'closed');
    const overdue = open.filter((q) => q.days !== null && q.days > overdueDays);
    const awaiting = open.filter((q) => !q.responded);
    const ages = open.map((q) => q.days).filter((d) => d !== null);

    const outstanding = [...open].sort((a, b) => (b.days ?? -1) - (a.days ?? -1));
    const topForm = topOf(site.byForm);
    const topCategory = topOf(site.byCategory);

    return {
      key: site.key,
      siteId: site.siteId,
      siteName: site.siteName,
      raised: site.queries.length,
      open: open.length,
      closed: closed.length,
      overdue: overdue.length,
      awaitingSite: awaiting.length,
      responded: open.length - awaiting.length,
      medianDays: median(ages),
      oldestDays: ages.length ? Math.max(...ages) : null,
      oldestOpened: outstanding.length ? outstanding[0].openedText : '',
      resolvedPercent: site.queries.length
        ? Math.round(closed.length / site.queries.length * 1000) / 10
        : null,
      ageBands: ageBandsFor(open),
      byForm: [...site.byForm.entries()].map(([name, count]) => ({ name, count }))
        .sort((a, b) => b.count - a.count),
      byEvent: [...site.byEvent.entries()].map(([name, count]) => ({ name, count }))
        .sort((a, b) => b.count - a.count),
      byCategory: [...site.byCategory.entries()].map(([name, count]) => ({ name, count }))
        .sort((a, b) => b.count - a.count),
      topForm: topForm ? topForm.name : '',
      topCategory: topCategory ? topCategory.name : '',
      outstanding,
    };
  });

  // Ranked on the share closed rather than the number outstanding, so a site
  // is not bottom for being large. Ties break on fewer left open.
  const ranked = sites.filter((s) => s.raised > 0);
  const ordered = [...ranked].sort((a, b) => b.resolvedPercent - a.resolvedPercent
    || a.open - b.open || a.siteName.localeCompare(b.siteName));
  let rank = 0;
  let previous = null;
  ordered.forEach((site, index) => {
    if (previous === null || site.resolvedPercent !== previous) rank = index + 1;
    previous = site.resolvedPercent;
    site.rank = rank;
  });
  for (const site of sites) {
    site.of = ranked.length;
    if (!site.raised) site.rank = null;
  }

  const allQueries = sites.reduce((sum, s) => sum + s.raised, 0);
  const allOpen = sites.reduce((sum, s) => sum + s.open, 0);
  const allClosed = sites.reduce((sum, s) => sum + s.closed, 0);
  const allAges = sites.flatMap((s) => s.outstanding.map((q) => q.days)).filter((d) => d !== null);

  const totals = {
    raised: allQueries,
    open: allOpen,
    closed: allClosed,
    overdue: sites.reduce((sum, s) => sum + s.overdue, 0),
    awaitingSite: sites.reduce((sum, s) => sum + s.awaitingSite, 0),
    siteCount: sites.length,
    rankedCount: ranked.length,
    medianDays: median(allAges),
    resolvedPercent: allQueries ? Math.round(allClosed / allQueries * 1000) / 10 : null,
    ageBands: ageBandsFor(sites.flatMap((s) => s.outstanding)),
    leader: ordered.length
      ? { key: ordered[0].key, siteName: ordered[0].siteName, percent: ordered[0].resolvedPercent }
      : null,
  };

  return {
    sites,
    totals,
    overdueDays,
    statuses: [...statusCounts.entries()].map(([status, count]) => ({ status, count }))
      .sort((a, b) => b.count - a.count),
    unknownStatuses: [...unknownStatuses],
    warnings,
  };
}

function findQuerySite(dataset, site) {
  if (!dataset || !dataset.sites) return null;
  return matchSiteIn(dataset.sites, site);
}

function queryMatchReport(dataset, sites) {
  const matched = [];
  const unmatched = [];
  for (const site of sites) {
    if (findQuerySite(dataset, site)) matched.push(site.siteName);
    else unmatched.push(site.siteName);
  }
  return { matched, unmatched };
}

function snapshotQueries(dataset, importedAt = new Date().toISOString()) {
  if (!dataset || !dataset.sites) return null;
  return {
    importedAt,
    open: dataset.totals ? dataset.totals.open : null,
    sites: dataset.sites.map((s) => ({ key: s.key, open: s.open, rank: s.rank })),
  };
}

/** Annotate with movement since the previous import. */
function applyQueryHistory(dataset, history = []) {
  if (!dataset || !dataset.sites) return dataset;
  const trimmed = history.slice(-HISTORY_LIMIT);
  const last = trimmed.length ? trimmed[trimmed.length - 1] : null;

  for (const site of dataset.sites) {
    const before = last ? last.sites.find((s) => s.key === site.key) : null;
    site.previousOpen = before && before.open !== undefined ? before.open : null;
    site.previousRank = before && before.rank ? before.rank : null;
    // Fewer open is better, so a fall in the count is a positive change.
    site.openChange = site.previousOpen !== null ? site.previousOpen - site.open : null;
    site.rankChange = site.rank && site.previousRank ? site.previousRank - site.rank : null;
  }
  dataset.previous = last ? { importedAt: last.importedAt, open: last.open } : null;
  return dataset;
}

/** Where a site stands on its queries, in one object the charts also read. */
function queryStanding(site, dataset) {
  if (!site || !dataset || !dataset.totals) return null;
  return {
    raised: site.raised,
    open: site.open,
    closed: site.closed,
    overdue: site.overdue,
    awaitingSite: site.awaitingSite,
    responded: site.responded,
    resolvedPercent: site.resolvedPercent,
    medianDays: site.medianDays,
    oldestDays: site.oldestDays,
    oldestOpened: site.oldestOpened,
    rank: site.rank,
    of: dataset.totals.rankedCount,
    openChange: site.openChange ?? null,
    rankChange: site.rankChange ?? null,
    trialOpen: dataset.totals.open,
    trialResolvedPercent: dataset.totals.resolvedPercent,
    leaderName: dataset.totals.leader ? dataset.totals.leader.siteName : null,
    shareOfOpen: dataset.totals.open > 0
      ? Math.round(site.open / dataset.totals.open * 1000) / 10
      : null,
    topForm: site.topForm,
    topCategory: site.topCategory,
    overdueDays: dataset.overdueDays,
  };
}

module.exports = {
  PATTERNS,
  AGE_BANDS,
  DEFAULT_OVERDUE_DAYS,
  HISTORY_LIMIT,
  detectQueryMapping,
  buildQueries,
  classifyStatus,
  parseDate,
  formatDate,
  findQuerySite,
  queryMatchReport,
  snapshotQueries,
  applyQueryHistory,
  queryStanding,
};

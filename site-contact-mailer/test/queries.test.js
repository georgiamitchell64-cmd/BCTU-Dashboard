'use strict';

const test = require('node:test');
const assert = require('node:assert');

const {
  detectQueryMapping, buildQueries, classifyStatus, parseDate, findQuerySite,
  queryMatchReport, applyQueryHistory, snapshotQueries, queryStanding,
} = require('../src/shared/queries');
const {
  queryResolutionChart, queryBreakdownChart, queryAgeingChart, queryGroupChart,
  queryListTable, qualityScorecard,
} = require('../src/shared/charts');
const { buildContext, QUALITY_FIELDS } = require('../src/shared/compose');
const { detectCompletenessMapping, buildCompleteness } = require('../src/shared/completeness');
const { htmlToText } = require('../src/shared/html');

// The real export's headers, verbatim.
const HEADERS = ['ID (# of comments)', 'Site (DAG)', 'TNo', 'Event', 'Form/DQR', 'Instance',
  'Question', 'Data Category', 'Date opened', 'Opened by', 'First Comment',
  'Date of last comment', 'Last comment by', 'Last Comment', 'Query Status',
  'Assigned To', 'If OPEN, Duration', 'Responded & OPEN'];

const TODAY = new Date(Date.UTC(2026, 8, 2));

function query(overrides = {}) {
  return {
    'ID (# of comments)': '1001 (2)',
    'Site (DAG)': 'freeman_hospital',
    TNo: 'FR101',
    Event: 'Baseline',
    'Form/DQR': 'Bowel Function',
    Instance: 1,
    Question: 'Score is outside the expected range',
    'Data Category': 'Out of range',
    'Date opened': '01-08-2026',
    'Opened by': 'Trial team',
    'First Comment': 'Please check.',
    'Date of last comment': '05-08-2026',
    'Last comment by': 'Trial team',
    'Last Comment': 'Please check.',
    'Query Status': 'Open',
    'Assigned To': 'Site',
    'If OPEN, Duration': 32,
    'Responded & OPEN': '',
    ...overrides,
  };
}

function build(rows, mappingOverrides = {}) {
  const mapping = { ...detectQueryMapping(HEADERS), ...mappingOverrides };
  return buildQueries(rows, mapping, { today: TODAY });
}

test('every column of the real export is recognised', () => {
  const mapping = detectQueryMapping(HEADERS);
  assert.equal(mapping.siteName, 'Site (DAG)');
  assert.equal(mapping.status, 'Query Status');
  assert.equal(mapping.opened, 'Date opened');
  assert.equal(mapping.duration, 'If OPEN, Duration');
  assert.equal(mapping.responded, 'Responded & OPEN');
  assert.equal(mapping.form, 'Form/DQR');
  assert.equal(mapping.participant, 'TNo');
  assert.equal(mapping.category, 'Data Category');
  assert.equal(mapping.lastComment, 'Date of last comment');
  assert.equal(mapping.queryId, 'ID (# of comments)');
});

test('"Site (DAG)" is not stolen by the looser site patterns', () => {
  const mapping = detectQueryMapping(['Site (DAG)', 'Site ID', 'Query Status']);
  assert.equal(mapping.siteName, 'Site (DAG)');
  assert.equal(mapping.siteId, 'Site ID');
});

test('statuses are classified, and anything unknown is reported', () => {
  assert.equal(classifyStatus('Open'), 'open');
  assert.equal(classifyStatus('CLOSED'), 'closed');
  assert.equal(classifyStatus('Resolved'), 'closed');
  assert.equal(classifyStatus('Awaiting response'), 'open');
  assert.equal(classifyStatus('Escalated to sponsor'), null);
  assert.equal(classifyStatus(''), null);

  const dataset = build([query({ 'Query Status': 'Escalated to sponsor' })]);
  assert.deepEqual(dataset.unknownStatuses, ['Escalated to sponsor']);
  assert.ok(dataset.warnings.some((w) => /not recognised/.test(w.message)));
});

test('an unrecognised status counts as open rather than vanishing', () => {
  const dataset = build([query({ 'Query Status': 'Escalated to sponsor' })]);
  assert.equal(dataset.sites[0].open, 1);
  assert.equal(dataset.sites[0].closed, 0);
});

test('dates are read day-first, as the export writes them', () => {
  assert.equal(parseDate('07-04-2026').getUTCMonth(), 3);
  assert.equal(parseDate('07/04/2026').getUTCDate(), 7);
  assert.equal(parseDate('2026-04-07').getUTCMonth(), 3);
  assert.equal(parseDate(''), null);
  assert.equal(parseDate('not a date'), null);
});

test('age comes from the duration column, or from the opening date without one', () => {
  const stated = build([query({ 'If OPEN, Duration': 32 })]);
  assert.equal(stated.sites[0].outstanding[0].days, 32);

  const derived = build([query({ 'If OPEN, Duration': '', 'Date opened': '03-08-2026' })]);
  assert.equal(derived.sites[0].outstanding[0].days, 30);
});

test('per-site counts split open, closed, overdue and awaiting the site', () => {
  const dataset = build([
    query({ 'If OPEN, Duration': 40 }),
    query({ 'If OPEN, Duration': 5, 'Responded & OPEN': 'Yes' }),
    query({ 'Query Status': 'Closed', 'If OPEN, Duration': '' }),
    query({ 'Query Status': 'Closed', 'If OPEN, Duration': '' }),
  ]);
  const site = dataset.sites[0];
  assert.equal(site.raised, 4);
  assert.equal(site.open, 2);
  assert.equal(site.closed, 2);
  assert.equal(site.overdue, 1);
  assert.equal(site.awaitingSite, 1);
  assert.equal(site.responded, 1);
  assert.equal(site.resolvedPercent, 50);
  assert.equal(site.oldestDays, 40);
});

test('the overdue threshold is configurable', () => {
  const rows = [query({ 'If OPEN, Duration': 20 })];
  assert.equal(build(rows).sites[0].overdue, 0);
  assert.equal(build(rows, { overdueDays: 14 }).sites[0].overdue, 1);
});

test('closed queries are left out of the ageing and the breakdowns', () => {
  const dataset = build([
    query({ 'Query Status': 'Closed', 'Form/DQR': 'Demographics' }),
    query({ 'If OPEN, Duration': 3, 'Form/DQR': 'Bowel Function' }),
  ]);
  const site = dataset.sites[0];
  assert.deepEqual(site.byForm, [{ name: 'Bowel Function', count: 1 }]);
  assert.equal(site.ageBands.find((b) => b.name === '0-7 days').count, 1);
  assert.equal(site.ageBands.reduce((sum, b) => sum + b.count, 0), 1);
});

test('outstanding queries are listed oldest first', () => {
  const dataset = build([
    query({ TNo: 'A', 'If OPEN, Duration': 5 }),
    query({ TNo: 'B', 'If OPEN, Duration': 60 }),
    query({ TNo: 'C', 'If OPEN, Duration': 20 }),
  ]);
  assert.deepEqual(dataset.sites[0].outstanding.map((q) => q.participant), ['B', 'C', 'A']);
});

test('sites are ranked on the share closed, not the number outstanding', () => {
  const big = Array.from({ length: 40 }, (unused, i) => query({
    'Site (DAG)': 'big_site', 'Query Status': i < 30 ? 'Closed' : 'Open',
  }));
  const small = Array.from({ length: 6 }, (unused, i) => query({
    'Site (DAG)': 'small_site', 'Query Status': i < 2 ? 'Closed' : 'Open',
  }));
  const dataset = build([...big, ...small]);
  const bigSite = dataset.sites.find((s) => s.siteName === 'big_site');
  const smallSite = dataset.sites.find((s) => s.siteName === 'small_site');

  // The big site has more open in absolute terms but is doing far better.
  assert.equal(bigSite.open, 10);
  assert.equal(smallSite.open, 4);
  assert.ok(bigSite.open > smallSite.open);
  assert.equal(bigSite.resolvedPercent, 75);
  assert.equal(smallSite.resolvedPercent, 33.3);
  assert.equal(bigSite.rank, 1);
  assert.equal(smallSite.rank, 2);
});

test('a REDCap data access group matches the contact list name', () => {
  const dataset = build([query()]);
  const matched = findQuerySite(dataset, { siteId: '004', siteName: 'Freeman Hospital' });
  assert.ok(matched);
  assert.equal(matched.siteName, 'freeman_hospital');

  const report = queryMatchReport(dataset, [
    { siteId: '', siteName: 'Freeman Hospital' },
    { siteId: '', siteName: 'Nowhere General' },
  ]);
  assert.deepEqual(report.unmatched, ['Nowhere General']);
});

test('movement compares against the previous import', () => {
  const first = build([query(), query(), query({ 'Query Status': 'Closed' })]);
  const history = [snapshotQueries(first, '2026-08-01T00:00:00.000Z')];
  const second = build([query(), query({ 'Query Status': 'Closed' }), query({ 'Query Status': 'Closed' })]);
  applyQueryHistory(second, history);

  const site = second.sites[0];
  assert.equal(site.previousOpen, 2);
  assert.equal(site.open, 1);
  // Fewer open is an improvement, so the change reads positive.
  assert.equal(site.openChange, 1);
});

test('trial totals add up across sites', () => {
  const dataset = build([
    query({ 'Site (DAG)': 'a', 'If OPEN, Duration': 40 }),
    query({ 'Site (DAG)': 'b', 'Query Status': 'Closed' }),
    query({ 'Site (DAG)': 'b', 'If OPEN, Duration': 2 }),
  ]);
  assert.equal(dataset.totals.raised, 3);
  assert.equal(dataset.totals.open, 2);
  assert.equal(dataset.totals.overdue, 1);
  assert.equal(dataset.totals.siteCount, 2);
});

// ── Merge fields ──────────────────────────────────────────────────────────

function contextFor(siteName, dataset, extra = {}) {
  return buildContext({ siteName, siteId: '', contacts: [], fields: {} },
    { queries: dataset, ...extra });
}

test('the query fields describe what the site has to do', () => {
  const dataset = build([
    query({ TNo: 'FR101', 'If OPEN, Duration': 40 }),
    query({ TNo: 'FR102', 'If OPEN, Duration': 5, 'Responded & OPEN': 'Yes' }),
    query({ TNo: 'FR103', 'Query Status': 'Closed' }),
  ]);
  const context = contextFor('Freeman Hospital', dataset);
  assert.equal(context.queries_open, '2');
  assert.equal(context.queries_awaiting_you, '1');
  assert.equal(context.queries_overdue, '1');
  assert.ok(context.query_headline.includes('Freeman Hospital has 2 data queries outstanding'));
  assert.ok(context.query_action.includes('waiting on a reply from your site'));
  assert.ok(context.query_list.includes('FR101'));
});

test('a site with nothing outstanding is thanked, not chased', () => {
  const dataset = build([
    query({ 'Query Status': 'Closed' }),
    query({ 'Site (DAG)': 'other_site', 'If OPEN, Duration': 10 }),
  ]);
  const context = contextFor('freeman_hospital', dataset);
  assert.equal(context.queries_open, '0');
  assert.equal(context.query_action, 'You have no outstanding data queries — thank you.');
  assert.equal(context.query_list, '');
});

test('a site that has replied to everything is not told to reply', () => {
  const dataset = build([query({ 'If OPEN, Duration': 10, 'Responded & OPEN': 'Yes' })]);
  const context = contextFor('freeman_hospital', dataset);
  assert.ok(context.query_action.includes('with the trial team'));
});

test('a site missing from the export gets no figures, only trial-wide ones', () => {
  const dataset = build([query()]);
  const context = contextFor('Nowhere General', dataset);
  assert.equal(context.queries_open, undefined);
  assert.equal(context.query_list, undefined);
  assert.equal(context.trial_queries_open, '1');
});

test('every advertised query field is produced when the data supports it', () => {
  const rows = [
    query({ TNo: 'FR101', 'If OPEN, Duration': 40 }),
    query({ TNo: 'FR102', 'If OPEN, Duration': 5, 'Responded & OPEN': 'Yes' }),
    query({ TNo: 'FR103', 'Query Status': 'Closed' }),
    query({ 'Site (DAG)': 'other_site', 'Query Status': 'Closed' }),
  ];
  const dataset = applyQueryHistory(build(rows), [{
    importedAt: '2026-08-01T00:00:00.000Z',
    open: 5,
    sites: [{ key: 'freeman_hospital', open: 5, rank: 2 }],
  }]);
  const completeness = buildCompleteness(
    [{ Site: 'Freeman Hospital', Due: 100, Entered: 80 }],
    detectCompletenessMapping(['Site', 'Due', 'Entered']),
  );
  const context = contextFor('Freeman Hospital', dataset, { completeness });

  const missing = QUALITY_FIELDS.map((f) => f.key).filter((key) => !(key in context));
  assert.deepEqual(missing, []);
  assert.ok(context.query_movement.includes('fewer open'));
});

// ── Charts ────────────────────────────────────────────────────────────────

test('the outstanding table lists the queries a site must answer', () => {
  const dataset = build([
    query({ TNo: 'FR101', 'If OPEN, Duration': 40, Event: 'Baseline' }),
    query({ TNo: 'FR102', 'If OPEN, Duration': 5 }),
  ]);
  const html = queryListTable(dataset.sites[0].outstanding, { overdueDays: 30 });
  assert.ok(html.includes('FR101'));
  assert.ok(html.includes('Baseline'));
  assert.ok(html.includes('40'));

  const text = htmlToText(html);
  assert.ok(text.includes('FR101'));
});

test('a long question is truncated rather than breaking the table', () => {
  const dataset = build([query({ Question: 'x'.repeat(200), 'If OPEN, Duration': 3 })]);
  const html = queryListTable(dataset.sites[0].outstanding);
  assert.ok(html.includes('…'));
  assert.ok(!html.includes('x'.repeat(60)));
});

test('query charts are email-safe: tables and bgcolor, no script, image or svg', () => {
  const dataset = build([
    query({ 'If OPEN, Duration': 40 }),
    query({ 'Site (DAG)': 'other_site', 'Query Status': 'Closed' }),
  ]);
  const html = [
    queryResolutionChart(dataset, { naming: 'all' }),
    queryBreakdownChart(dataset.sites[0], dataset),
    queryAgeingChart(dataset.sites[0].ageBands),
    queryGroupChart(dataset.sites[0].byForm),
    queryListTable(dataset.sites[0].outstanding),
  ].join('');
  assert.ok(html.includes('bgcolor='));
  assert.ok(!/<script|<svg|<img|background-image/i.test(html));
  assert.ok(!/\son[a-z]+=/i.test(html));
});

test('the scorecard pairs the two imports, and copes when one lacks a site', () => {
  const queriesData = build([query({ 'Site (DAG)': 'Freeman Hospital', 'If OPEN, Duration': 9 })]);
  const completeness = buildCompleteness([
    { Site: 'Freeman Hospital', Due: 100, Entered: 80 },
    { Site: 'Southmead Hospital', Due: 100, Entered: 95 },
  ], detectCompletenessMapping(['Site', 'Due', 'Entered']));

  const html = qualityScorecard(completeness, queriesData, { naming: 'all' });
  assert.ok(html.includes('Queries open'));
  assert.ok(html.includes('Southmead Hospital'));
  // Southmead has completeness but no queries, so its query cells are dashes.
  assert.ok(html.includes('—'));

  const alone = qualityScorecard(completeness, null, { naming: 'all' });
  assert.ok(!alone.includes('Queries open'));
});

test('charts asked for without data return nothing rather than an empty frame', () => {
  const empty = build([]);
  assert.equal(queryResolutionChart(empty, {}), '');
  assert.equal(queryAgeingChart([]), '');
  assert.equal(queryGroupChart([]), '');
  assert.equal(queryListTable([]), '');
});

test('standing reports a site\'s share of the trial\'s open queries', () => {
  const dataset = build([
    query({ 'Site (DAG)': 'a', 'If OPEN, Duration': 10 }),
    query({ 'Site (DAG)': 'a', 'If OPEN, Duration': 10 }),
    query({ 'Site (DAG)': 'b', 'If OPEN, Duration': 10 }),
  ]);
  const view = queryStanding(dataset.sites.find((s) => s.siteName === 'a'), dataset);
  assert.equal(view.open, 2);
  assert.equal(view.shareOfOpen, 66.7);
  assert.equal(view.trialOpen, 3);
});

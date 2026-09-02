'use strict';

const test = require('node:test');
const assert = require('node:assert');

const {
  detectCompletenessMapping, buildCompleteness, findCompletenessSite,
  completenessMatchReport, applyCompletenessHistory, snapshotCompleteness,
  standing, queryStanding, isOverallRow,
} = require('../src/shared/completeness');
const {
  completenessBarChart, completenessLeaderboard, completenessGauge,
  completenessBreakdownChart, ordinal,
} = require('../src/shared/charts');
const { buildContext, renderTemplate, COMPLETENESS_FIELDS } = require('../src/shared/compose');
const { htmlToText } = require('../src/shared/html');

// The shape of the return-rates export the dashboard reads: one row per site
// per event per form, with ".Overall" carrying the trial figure.
const HEADERS = ['Site', 'Event', 'Form', 'Expected', 'Due', 'Entered', '% Due Entered', '% Expected Entered'];

function row(site, event, form, expected, due, entered) {
  return {
    Site: site,
    Event: event,
    Form: form,
    Expected: expected,
    Due: due,
    Entered: entered,
    '% Due Entered': due ? Math.round(entered / due * 100) : '',
    '% Expected Entered': expected ? Math.round(entered / expected * 100) : '',
  };
}

const ROWS = [
  row('Queen Elizabeth Hospital', 'Baseline', 'Demographics', 12, 12, 12),
  row('Queen Elizabeth Hospital', 'Baseline', 'Bowel Function', 12, 12, 11),
  row('Queen Elizabeth Hospital', 'Day 30', 'Bowel Function', 12, 8, 6),
  row('Musgrove Park Hospital', 'Baseline', 'Demographics', 13, 13, 9),
  row('Musgrove Park Hospital', 'Baseline', 'Bowel Function', 13, 13, 8),
  row('Musgrove Park Hospital', 'Day 30', 'Bowel Function', 13, 10, 4),
  row('Northern General Hospital', 'Baseline', 'Demographics', 3, 3, 3),
  row('Northern General Hospital', 'Baseline', 'Bowel Function', 3, 3, 3),
  row('Northern General Hospital', 'Day 30', 'Bowel Function', 3, 1, 1),
  row('.Overall', 'Baseline', 'Demographics', 28, 28, 24),
];

// Six sites with events and forms — enough for every completeness field.
const FULL_HEADERS = [...HEADERS];

function fullRows() {
  const rows = [];
  ['A', 'B', 'C', 'D', 'E', 'F'].forEach((letter, index) => {
    const entered = 20 - index * 2;
    [['Baseline', 'Demographics'], ['Baseline', 'Bowel Function'],
      ['Day 30', 'Bowel Function']].forEach(([event, form], position) => {
      rows.push({
        ...row(`Hospital ${letter}`, event, form, 24, 20, position === 2 ? entered - 3 : entered),
      });
    });
  });
  return rows;
}

function fullDataset() {
  const mapping = detectCompletenessMapping(FULL_HEADERS);
  const dataset = buildCompleteness(fullRows(), mapping);
  return applyCompletenessHistory(dataset, [{
    importedAt: '2026-06-01T00:00:00.000Z',
    percent: 80,
    sites: dataset.sites.map((site, index) => ({
      key: site.key, percent: 80, rank: index === 1 ? 4 : index + 1,
    })),
  }]);
}

function build(rows = ROWS, headers = HEADERS) {
  const mapping = detectCompletenessMapping(headers);
  return { mapping, dataset: buildCompleteness(rows, mapping) };
}

test('detects the return-rates layout', () => {
  const mapping = detectCompletenessMapping(HEADERS);
  assert.equal(mapping.layout, 'long');
  assert.equal(mapping.basis, 'due');
  assert.equal(mapping.siteName, 'Site');
  assert.equal(mapping.siteId, null);
  assert.equal(mapping.event, 'Event');
  assert.equal(mapping.form, 'Form');
  assert.equal(mapping.due, 'Due');
  assert.equal(mapping.entered, 'Entered');
});

test('rates are calculated against forms due, not forms expected', () => {
  const { dataset } = build();
  const qe = dataset.sites.find((s) => s.siteName.startsWith('Queen'));
  // 29 of 32 due, not 29 of 36 expected.
  assert.equal(qe.due, 32);
  assert.equal(qe.expected, 36);
  assert.equal(qe.entered, 29);
  assert.equal(qe.percent, 90.6);
});

test('entries made before a window opens cannot push a site over 100%', () => {
  const rows = [row('Early Site', 'Day 30', 'Bowel Function', 10, 4, 7)];
  const { dataset } = build(rows);
  assert.equal(dataset.sites[0].percent, 100);
});

test('a site with nothing due yet is not ranked against sites that have', () => {
  const rows = [...ROWS, row('Brand New Hospital', 'Baseline', 'Demographics', 2, 0, 0)];
  const { dataset } = build(rows);
  const fresh = dataset.sites.find((s) => s.siteName.startsWith('Brand'));
  assert.equal(fresh.percent, null);
  assert.equal(fresh.rank, null);
  assert.equal(dataset.totals.rankedCount, 3);
});

test('.Overall rows are the trial figure, not a site', () => {
  const { dataset } = build();
  assert.equal(dataset.sites.length, 3);
  assert.ok(dataset.fromOverallRows);
  assert.ok(isOverallRow('.Overall'));
  assert.ok(isOverallRow('Total'));
  assert.ok(!isOverallRow('Queen Elizabeth Hospital'));
});

test('sites are ranked on completeness, best first', () => {
  const { dataset } = build();
  const ranked = [...dataset.sites].sort((a, b) => a.rank - b.rank).map((s) => s.siteName);
  assert.deepEqual(ranked, [
    'Northern General Hospital',
    'Queen Elizabeth Hospital',
    'Musgrove Park Hospital',
  ]);
  assert.equal(dataset.sites.find((s) => s.rank === 1).percent, 100);
});

test('equal completeness shares a rank', () => {
  const rows = [
    row('A', 'Baseline', 'Demographics', 10, 10, 9),
    row('B', 'Baseline', 'Demographics', 20, 20, 18),
    row('C', 'Baseline', 'Demographics', 10, 10, 5),
  ];
  const { dataset } = build(rows);
  const byName = Object.fromEntries(dataset.sites.map((s) => [s.siteName, s.rank]));
  assert.equal(byName.A, 1);
  assert.equal(byName.B, 1);
  assert.equal(byName.C, 3);
});

test('breaks completeness down by event and by form', () => {
  const { dataset } = build();
  const musgrove = dataset.sites.find((s) => s.siteName.startsWith('Musgrove'));
  const baseline = musgrove.byEvent.find((e) => e.name === 'Baseline');
  assert.equal(baseline.due, 26);
  assert.equal(baseline.entered, 17);
  const bowel = musgrove.byForm.find((f) => f.name === 'Bowel Function');
  assert.equal(bowel.due, 23);
  assert.equal(bowel.outstanding, 11);
});

test('the site totals include an outstanding count', () => {
  const { dataset } = build();
  const musgrove = dataset.sites.find((s) => s.siteName.startsWith('Musgrove'));
  assert.equal(musgrove.outstanding, musgrove.due - musgrove.entered);
  assert.ok(dataset.totals.outstanding > 0);
});

test('a site-total layout without events still ranks', () => {
  const headers = ['Centre', 'Forms Due', 'Forms Entered'];
  const rows = [
    { Centre: 'Alpha', 'Forms Due': 100, 'Forms Entered': 91 },
    { Centre: 'Beta', 'Forms Due': 50, 'Forms Entered': 30 },
  ];
  const mapping = detectCompletenessMapping(headers);
  assert.equal(mapping.layout, 'site-total');
  const dataset = buildCompleteness(rows, mapping);
  assert.equal(dataset.sites.find((s) => s.siteName === 'Alpha').percent, 91);
  assert.equal(dataset.sites.find((s) => s.siteName === 'Beta').rank, 2);
});

test('a percentage-only layout is taken at face value', () => {
  const headers = ['Site', '% Complete'];
  const rows = [{ Site: 'Alpha', '% Complete': '88.5%' }, { Site: 'Beta', '% Complete': '72' }];
  const mapping = detectCompletenessMapping(headers);
  assert.equal(mapping.layout, 'site-percent');
  const dataset = buildCompleteness(rows, mapping);
  assert.equal(dataset.sites[0].percent, 88.5);
  assert.equal(dataset.sites[1].percent, 72);
});

test('standing gives the competitive figures', () => {
  const { dataset } = build();
  const musgrove = dataset.sites.find((s) => s.siteName.startsWith('Musgrove'));
  const view = standing(musgrove, dataset);
  assert.equal(view.rank, 3);
  assert.equal(view.of, 3);
  assert.equal(view.isLeader, false);
  assert.equal(view.leaderPercent, 100);
  assert.equal(view.gapToTop, Math.round((100 - musgrove.percent) * 10) / 10);
  assert.ok(view.vsAverage < 0);
  assert.equal(view.status, 'below target');
});

test('movement is measured against the previous import, not this one', () => {
  const { dataset: first } = build();
  const history = [snapshotCompleteness(first, '2026-06-01T00:00:00.000Z')];

  // Musgrove catches up and overtakes Queen Elizabeth.
  const improved = ROWS.map((r) => (r.Site.startsWith('Musgrove') ? { ...r, Entered: r.Due } : r));
  const { dataset: second } = build(improved);
  applyCompletenessHistory(second, history);

  const musgrove = second.sites.find((s) => s.siteName.startsWith('Musgrove'));
  assert.equal(musgrove.rank, 1);
  assert.equal(musgrove.previousRank, 3);
  assert.equal(musgrove.rankChange, 2);
  assert.ok(musgrove.percentChange > 0);
  assert.equal(musgrove.history.length, 1);
});

test('a site absent from the previous import has no movement, not a fake one', () => {
  const { dataset } = build();
  applyCompletenessHistory(dataset, [{
    importedAt: '2026-06-01T00:00:00.000Z',
    percent: 80,
    sites: [{ key: 'someone else', percent: 80, rank: 1 }],
  }]);
  const site = dataset.sites[0];
  assert.equal(site.previousRank, null);
  assert.equal(site.rankChange, null);
  assert.equal(site.percentChange, null);
});

test('contact-list sites are matched by id then by name', () => {
  const { dataset } = build();
  const matched = findCompletenessSite(dataset, { siteId: 'X1', siteName: 'Musgrove Park Hospital' });
  assert.equal(matched.siteName, 'Musgrove Park Hospital');
  const report = completenessMatchReport(dataset, [
    { siteId: '', siteName: 'Musgrove Park Hospital' },
    { siteId: '', siteName: 'Nowhere General' },
  ]);
  assert.deepEqual(report.unmatched, ['Nowhere General']);
});

// ── Merge fields and charts ───────────────────────────────────────────────

function contextFor(siteName, dataset, options = {}) {
  return buildContext({ siteName, siteId: '', contacts: [], fields: {} }, {
    completeness: dataset, ...options,
  });
}

test('the merge fields describe where a site stands', () => {
  const { dataset } = build();
  const context = contextFor('Musgrove Park Hospital', dataset);
  assert.equal(context.completeness_rank_of, '3 of 3');
  assert.equal(context.completeness_position, '3rd of 3');
  assert.equal(context.completeness_status, 'below target');
  assert.ok(context.completeness_vs_average_words.endsWith('below the trial average'));
  assert.ok(context.completeness_headline.includes('3rd of 3'));
  assert.ok(context.completeness_gap_to_top.endsWith('points'));
  assert.equal(context.trial_completeness_average, `${dataset.totals.meanPercent}%`);
});

test('the leader gets the trophy line, and nobody else claims it', () => {
  const { dataset } = build();
  const leader = contextFor('Northern General Hospital', dataset);
  assert.ok(leader.completeness_trophy.includes('top spot'));
  const last = contextFor('Musgrove Park Hospital', dataset);
  assert.equal(last.completeness_trophy, undefined);
});

test('a podium place is only a podium when there is a field behind it', () => {
  // Third of three is last, and must not be sold to the site as a podium.
  const { dataset: three } = build();
  assert.equal(contextFor('Queen Elizabeth Hospital', three).completeness_trophy, undefined);
  const context = contextFor('Hospital B', fullDataset());
  assert.ok(context.completeness_trophy.includes('2nd'));
});

test('every advertised field is produced when the data supports it', () => {
  const context = contextFor('Hospital B', fullDataset());
  const missing = COMPLETENESS_FIELDS.map((f) => f.key).filter((key) => !(key in context));
  assert.deepEqual(missing, []);
  assert.ok(context.completeness_gap_to_next.includes('1st place'));
  assert.ok(context.completeness_movement.includes('place'));
});

test('the return-rates import produces no query fields on its own', () => {
  // Queries arrive as their own export; completeness alone must not imply them.
  const context = contextFor('Hospital B', fullDataset());
  assert.equal(context.queries_open, undefined);
  assert.equal(context.query_headline, undefined);
  assert.equal(context.quality_scorecard, undefined);
});

test('a site the export does not cover gets no figures rather than wrong ones', () => {
  const { dataset } = build();
  const context = contextFor('Nowhere General', dataset);
  assert.equal(context.completeness, undefined);
  assert.equal(context.completeness_rank, undefined);
  // Trial-wide figures still apply — they do not depend on the recipient.
  assert.ok(context.trial_completeness);
});

test('charts name every site by default and can be anonymised', () => {
  const { dataset } = build();
  const named = completenessBarChart(dataset, { focusKey: 'musgrove park hospital', naming: 'all' });
  assert.ok(named.includes('Northern General Hospital'));
  assert.ok(!named.includes('Site A'));

  const anonymous = completenessBarChart(dataset, { focusKey: 'musgrove park hospital', naming: 'none' });
  assert.ok(anonymous.includes('Site A'));
  assert.ok(anonymous.includes('Musgrove Park Hospital'));
  assert.ok(!anonymous.includes('Northern General'));
});

test('naming the leading three leaves the tail anonymous', () => {
  const rows = ['A', 'B', 'C', 'D', 'E'].map((name, index) => row(`Hospital ${name}`, 'Baseline', 'Demographics', 10, 10, 10 - index));
  const { dataset } = build(rows);
  const html = completenessBarChart(dataset, { focusKey: 'hospital e', naming: 'top3' });
  assert.ok(html.includes('Hospital A'));
  assert.ok(html.includes('Hospital C'));
  assert.ok(!html.includes('Hospital D'));
  assert.ok(html.includes('Hospital E'));
});

test('the recipient is always shown even when outside the top rows', () => {
  const rows = Array.from({ length: 20 }, (unused, index) => row(`Site ${index}`, 'Baseline', 'Demographics', 100, 100, 100 - index));
  const { dataset } = build(rows);
  const html = completenessBarChart(dataset, { focusKey: 'site 19', naming: 'all', maxRows: 5 });
  assert.ok(html.includes('Site 19'));
  assert.ok(html.includes('(you)'));
});

test('charts are email-safe: tables and bgcolor, no script, image or svg', () => {
  const { dataset } = build();
  const html = [
    completenessBarChart(dataset, { focusKey: 'musgrove park hospital' }),
    completenessLeaderboard(dataset, { focusKey: 'musgrove park hospital' }),
    completenessGauge(dataset.sites[0], dataset),
    completenessBreakdownChart(dataset.sites[0].byForm, { worstFirst: true }),
  ].join('');
  assert.ok(html.includes('bgcolor='));
  assert.ok(!/<script|<svg|<img|background-image/i.test(html));
  assert.ok(!/\son[a-z]+=/i.test(html));
});

test('charts survive the plain-text alternative as a readable list', () => {
  const { dataset } = build();
  const text = htmlToText(completenessLeaderboard(dataset, { focusKey: 'musgrove park hospital', naming: 'all' }));
  assert.ok(text.includes('Musgrove Park Hospital'));
  assert.ok(/\d+(\.\d+)?%/.test(text));
});

test('a chart asked for without data returns nothing rather than an empty frame', () => {
  const empty = buildCompleteness([], detectCompletenessMapping(HEADERS));
  assert.equal(completenessBarChart(empty, {}), '');
  assert.equal(completenessLeaderboard(empty, {}), '');
});

test('a placeholder with no value falls back rather than leaving a hole', () => {
  const { dataset } = build();
  const context = contextFor('Nowhere General', dataset);
  const { text, missing } = renderTemplate('You are {{completeness_position|not yet ranked}}.', context);
  assert.equal(text, 'You are not yet ranked.');
  assert.deepEqual(missing, []);
});

test('ordinals read correctly, including the teens', () => {
  assert.deepEqual([1, 2, 3, 4, 11, 12, 13, 21, 22].map(ordinal),
    ['1st', '2nd', '3rd', '4th', '11th', '12th', '13th', '21st', '22nd']);
});

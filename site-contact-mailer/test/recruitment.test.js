'use strict';

const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const os = require('os');
const path = require('path');

const {
  detectRecruitmentMapping, buildRecruitment, findRecruitmentSite, matchReport,
  toMonthKey, formatMonthShort,
} = require('../src/shared/recruitment');
const { readWorkbook, findHeaderRow } = require('../src/main/workbook');
const { rankedBarChart, progressChart, overallChart, anonymousLabel } = require('../src/shared/charts');
const { htmlToText, sanitizeHtml } = require('../src/shared/html');

// The real shape of a TONIC recruitment export: a row of machine codes above
// the headers, months across the columns, "-" for months before the site
// opened, and a Total row at the bottom.
const TONIC_CSV = [
  '"_","__","Fst","Lst","M_","M1","M2","M3","M4","M5"',
  '"Site","Opened","First","Last","Total","Apr-2026","May-2026","Jun-2026","Jul-2026",""',
  '"Blackpool Victoria Hospital","12-06-2026","21-07-2026","21-07-2026","1","-","-","0","1",""',
  '"George Elliot Hospital","13-07-2026","17-07-2026","22-07-2026","3","-","-","-","3",""',
  '"Musgrove Park Hospital","01-04-2026","01-04-2026","10-07-2026","13","4","2","6","1",""',
  '"Northern General Hospital","17-06-2026","01-07-2026","23-07-2026","3","-","-","0","3",""',
  '"Queen Elizabeth Hospital (Birmingham)","07-04-2026","13-04-2026","17-07-2026","12","4","0","3","5",""',
  '"Total","-","01-04-2026","23-07-2026","32","8","2","9","13",""',
].join('\n');

async function loadTonic() {
  const file = path.join(os.tmpdir(), `scm-rec-${process.pid}-${Math.random().toString(36).slice(2)}.csv`);
  fs.writeFileSync(file, TONIC_CSV);
  try {
    const workbook = await readWorkbook(file);
    const sheet = workbook.sheets[0];
    const mapping = detectRecruitmentMapping(sheet.headers);
    return { sheet, mapping, dataset: buildRecruitment(sheet.rows, mapping, { firstDataRow: sheet.firstDataRow }) };
  } finally {
    fs.unlinkSync(file);
  }
}

// ── Month parsing ─────────────────────────────────────────────────────────

test('month keys are read from the formats a trial export uses', () => {
  assert.strictEqual(toMonthKey('Apr-2026'), '2026-04');
  assert.strictEqual(toMonthKey('2026-04'), '2026-04');
  assert.strictEqual(toMonthKey('July 2026'), '2026-07');
  assert.strictEqual(toMonthKey('2026-04-17'), '2026-04');
});

test('a UK date is not misread as an American one', () => {
  // 07/04/2026 is 7 April here, not 4 July.
  assert.strictEqual(toMonthKey('07-04-2026'), '2026-04');
  assert.strictEqual(toMonthKey('13-07-2026'), '2026-07');
});

test('unparseable months are reported as such rather than guessed', () => {
  assert.strictEqual(toMonthKey(''), null);
  assert.strictEqual(toMonthKey('-'), null);
  assert.strictEqual(toMonthKey('not a date'), null);
});

test('formatMonthShort is compact enough for a chart axis', () => {
  assert.strictEqual(formatMonthShort('2026-04'), 'Apr 26');
});

// ── Reading the real export ───────────────────────────────────────────────

test('the header row is found beneath the row of machine codes', () => {
  const grid = [
    ['_', '__', 'Fst', 'M1', 'M2'],
    ['Site', 'Opened', 'First', 'Apr-2026', 'May-2026'],
    ['QE', '01-04-2026', '02-04-2026', '4', '2'],
  ];
  assert.strictEqual(findHeaderRow(grid), 1);
});

test('a normal header row is still chosen when there are no codes above it', () => {
  const grid = [
    ['Site ID', 'Site Name', 'Email'],
    ['001', 'QE', 'a@nhs.net'],
  ];
  assert.strictEqual(findHeaderRow(grid), 0);
});

test('the cross-tab layout is detected from the month columns', async () => {
  const { mapping } = await loadTonic();
  assert.strictEqual(mapping.layout, 'site-month-wide');
  assert.strictEqual(mapping.siteName, 'Site');
  assert.strictEqual(mapping.count, 'Total');
  assert.strictEqual(mapping.opened, 'Opened');
  assert.deepStrictEqual(mapping.monthColumns.map((c) => c.month),
    ['2026-04', '2026-05', '2026-06', '2026-07']);
});

test('the export\'s own Total row is not treated as a site', async () => {
  const { dataset } = await loadTonic();
  assert.strictEqual(dataset.sites.length, 5);
  assert.ok(!dataset.sites.some((s) => /^total$/i.test(s.siteName)));
});

test('site totals and the trial total match the source file', async () => {
  const { dataset } = await loadTonic();
  const qe = dataset.sites.find((s) => s.siteName.startsWith('Queen Elizabeth'));
  assert.strictEqual(qe.randomised, 12);
  assert.deepStrictEqual(qe.monthly.map((m) => m.count), [4, 0, 3, 5]);
  // The file's own Total row says 32 and 8/2/9/13.
  assert.strictEqual(dataset.totals.randomised, 32);
  assert.deepStrictEqual(dataset.totals.monthly.map((m) => m.count), [8, 2, 9, 13]);
});

test('"-" before a site opened is not counted as a zero month', async () => {
  const { dataset } = await loadTonic();
  const george = dataset.sites.find((s) => s.siteName.startsWith('George'));
  // Only July has a figure; the rest were before the site opened.
  assert.strictEqual(george.randomised, 3);
  assert.deepStrictEqual(george.monthly.map((m) => m.count), [0, 0, 0, 3]);
});

test('the date a site opened is captured', async () => {
  const { dataset } = await loadTonic();
  const musgrove = dataset.sites.find((s) => s.siteName.startsWith('Musgrove'));
  assert.strictEqual(musgrove.opened, '01-04-2026');
});

test('sites are ranked, with ties sharing a place', async () => {
  const { dataset } = await loadTonic();
  const byName = Object.fromEntries(dataset.sites.map((s) => [s.siteName.split(' ')[0], s]));
  assert.strictEqual(byName.Musgrove.rank, 1);
  assert.strictEqual(byName.Queen.rank, 2);
  // George Elliot and Northern General both have 3.
  assert.strictEqual(byName.George.rank, 3);
  assert.strictEqual(byName.Northern.rank, 3);
  assert.strictEqual(byName.Blackpool.rank, 5);
  assert.strictEqual(byName.Queen.of, 5);
});

test('a total that disagrees with its months is flagged, not silently trusted', () => {
  const rows = [{ Site: 'QE', Total: '99', 'Apr-2026': '4', 'May-2026': '2' }];
  const mapping = detectRecruitmentMapping(['Site', 'Total', 'Apr-2026', 'May-2026']);
  const { sites, warnings } = buildRecruitment(rows, mapping);
  assert.strictEqual(sites[0].randomised, 99);
  assert.ok(warnings.some((w) => /total column says 99/.test(w.message)));
});

// ── The other layouts ─────────────────────────────────────────────────────

test('one row per participant is aggregated into monthly counts', () => {
  const rows = [
    { 'Participant ID': 'P1', Site: 'QE', 'Randomisation Date': '2026-04-03' },
    { 'Participant ID': 'P2', Site: 'QE', 'Randomisation Date': '2026-04-19' },
    { 'Participant ID': 'P3', Site: 'QE', 'Randomisation Date': '2026-05-02' },
    { 'Participant ID': 'P4', Site: 'Addenbrookes', 'Randomisation Date': '2026-05-11' },
  ];
  const mapping = detectRecruitmentMapping(Object.keys(rows[0]));
  assert.strictEqual(mapping.layout, 'participant');
  const { sites, totals } = buildRecruitment(rows, mapping);
  const qe = sites.find((s) => s.siteId === 'QE');
  assert.strictEqual(qe.randomised, 3);
  assert.deepStrictEqual(qe.monthly.map((m) => m.count), [2, 1]);
  assert.strictEqual(totals.randomised, 4);
});

test('one row per site per month is summed per site', () => {
  const rows = [
    { Site: 'QE', Month: 'Apr-2026', Randomised: '4' },
    { Site: 'QE', Month: 'May-2026', Randomised: '2' },
  ];
  const mapping = detectRecruitmentMapping(['Site', 'Month', 'Randomised']);
  assert.strictEqual(mapping.layout, 'site-month');
  const { sites } = buildRecruitment(rows, mapping);
  assert.strictEqual(sites[0].randomised, 6);
});

test('site totals with a target give a percentage', () => {
  const rows = [{ Site: 'QE', Randomised: '12', Target: '30' }];
  const mapping = detectRecruitmentMapping(['Site', 'Randomised', 'Target']);
  assert.strictEqual(mapping.layout, 'site-total');
  const { sites } = buildRecruitment(rows, mapping);
  assert.strictEqual(sites[0].target, 30);
  assert.strictEqual(sites[0].percentOfTarget, 40);
});

test('a site listed twice in a totals file is not double-counted', () => {
  const rows = [{ Site: 'QE', Randomised: '12' }, { Site: 'QE', Randomised: '12' }];
  const mapping = detectRecruitmentMapping(['Site', 'Randomised']);
  const { sites } = buildRecruitment(rows, mapping);
  assert.strictEqual(sites.length, 1);
  assert.strictEqual(sites[0].randomised, 12);
});

// ── Joining to the contact list ───────────────────────────────────────────

test('recruitment rows match contacts by site ID, then by name', async () => {
  const { dataset } = await loadTonic();
  const byName = findRecruitmentSite(dataset, { siteName: 'Musgrove Park Hospital' });
  assert.strictEqual(byName.randomised, 13);
  assert.strictEqual(findRecruitmentSite(dataset, { siteName: 'Nowhere General' }), null);
});

test('matchReport names the sites that will get no charts', async () => {
  const { dataset } = await loadTonic();
  const report = matchReport(dataset, [
    { siteName: 'Musgrove Park Hospital' },
    { siteName: 'Somewhere Else' },
  ]);
  assert.deepStrictEqual(report.matched, ['Musgrove Park Hospital']);
  assert.deepStrictEqual(report.unmatched, ['Somewhere Else']);
});

// ── Charts ────────────────────────────────────────────────────────────────

test('the ranked chart names only the recipient and anonymises the rest', async () => {
  const { dataset } = await loadTonic();
  const qe = dataset.sites.find((s) => s.siteName.startsWith('Queen Elizabeth'));
  const html = rankedBarChart(dataset, { focusKey: qe.key, anonymise: true });

  assert.ok(html.includes('Queen Elizabeth Hospital (Birmingham)'));
  assert.ok(html.includes('(you)'));
  // No other real site name may appear.
  assert.ok(!html.includes('Musgrove'), 'other sites must not be named');
  assert.ok(!html.includes('Blackpool'));
  assert.ok(html.includes('Site A'));
});

test('naming can be turned back on', async () => {
  const { dataset } = await loadTonic();
  const html = rankedBarChart(dataset, { focusKey: null, anonymise: false });
  assert.ok(html.includes('Musgrove Park Hospital'));
});

test('the recipient is shown even when outside the top of the table', () => {
  const sites = Array.from({ length: 15 }, (_, i) => ({
    key: `s${i}`, siteId: `S${i}`, siteName: `Site number ${i}`, randomised: 100 - i, target: null, monthly: [],
    rank: i + 1, of: 15, quartile: 1, percentOfTarget: null,
  }));
  const html = rankedBarChart({ sites, totals: {} }, { focusKey: 's14', anonymise: true, maxRows: 5 });
  assert.ok(html.includes('Site number 14'), 'the recipient must always appear');
  assert.ok(html.includes('⋮'), 'a gap marker shows the jump down the table');
});

test('charts are email-safe: tables and bgcolor, no script or flexbox', async () => {
  const { dataset } = await loadTonic();
  const html = rankedBarChart(dataset, { focusKey: null }) + overallChart(dataset);
  assert.ok(/<table/.test(html) && /bgcolor="/.test(html));
  assert.ok(!/<script/i.test(html));
  assert.ok(!/display:\s*flex/i.test(html));
  assert.ok(!/<svg/i.test(html));
  // And they must survive the sanitiser the editor puts everything through.
  assert.strictEqual(sanitizeHtml(html), html);
});

test('a chart reads sensibly once flattened to plain text', async () => {
  const { dataset } = await loadTonic();
  const qe = dataset.sites.find((s) => s.siteName.startsWith('Queen Elizabeth'));
  const text = htmlToText(rankedBarChart(dataset, { focusKey: qe.key, anonymise: true }));
  assert.match(text, /Site A\t13/);
  assert.match(text, /Queen Elizabeth Hospital \(Birmingham\) \(you\)\t12/);
});

test('progress against a missing target says so rather than showing 0%', () => {
  const html = progressChart({ siteName: 'QE', randomised: 12, target: null });
  assert.ok(!html.includes('%'));
  assert.match(html, /No recruitment target/);
});

test('progress over target does not overflow the bar', () => {
  const html = progressChart({ siteName: 'QE', randomised: 40, target: 30 });
  assert.match(html, /133%/);
  const widths = [...html.matchAll(/width="(\d+)"/g)].map((m) => Number(m[1]));
  assert.ok(widths.every((w) => w <= 520), 'no bar may exceed the chart width');
});

test('an empty dataset produces no chart rather than a broken one', () => {
  assert.strictEqual(rankedBarChart({ sites: [], totals: {} }, {}), '');
  assert.strictEqual(rankedBarChart(null, {}), '');
});

test('anonymous labels keep going past 26 sites', () => {
  assert.strictEqual(anonymousLabel(0), 'Site A');
  assert.strictEqual(anonymousLabel(25), 'Site Z');
  assert.strictEqual(anonymousLabel(26), 'Site AA');
});

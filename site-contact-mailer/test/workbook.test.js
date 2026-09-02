'use strict';

// End-to-end over a real .xlsx: file on disk -> sites and contacts.

const test = require('node:test');
const assert = require('node:assert');
const path = require('path');
const fs = require('fs');
const os = require('os');
const ExcelJS = require('exceljs');

const { readWorkbook, cellToString, findHeaderRow, normaliseHeaders } = require('../src/main/workbook');
const { detectMapping, buildSites } = require('../src/shared/importer');

const SAMPLE = path.join(__dirname, '..', 'sample', 'site-contacts-sample.xlsx');
const hasSample = fs.existsSync(SAMPLE);

test('cellToString flattens the shapes ExcelJS returns', () => {
  assert.strictEqual(cellToString('  text  '), 'text');
  assert.strictEqual(cellToString(42), '42');
  assert.strictEqual(cellToString(null), '');
  assert.strictEqual(cellToString({ richText: [{ text: 'a' }, { text: 'b' }] }), 'ab');
  assert.strictEqual(cellToString({ formula: 'A1', result: 'computed' }), 'computed');
  // An email cell is often a hyperlink; the mailto: prefix must not survive.
  assert.strictEqual(cellToString({ text: '', hyperlink: 'mailto:a@nhs.net' }), 'a@nhs.net');
  assert.strictEqual(cellToString({ text: 'a@nhs.net', hyperlink: 'mailto:a@nhs.net' }), 'a@nhs.net');
});

test('findHeaderRow skips a title row above the real headers', () => {
  const grid = [
    ['TONIC — site contact list', '', ''],
    ['', '', ''],
    ['Site ID', 'Site Name', 'Email'],
    ['001', 'QE', 'a@nhs.net'],
  ];
  assert.strictEqual(findHeaderRow(grid), 2);
});

test('findHeaderRow returns -1 when there is no data under any header', () => {
  assert.strictEqual(findHeaderRow([['Site', 'Email']]), -1);
});

test('duplicate and blank headers are made unique', () => {
  assert.deepStrictEqual(
    normaliseHeaders(['Email', 'Email', '', 'Site']),
    ['Email', 'Email (2)', 'Column 3', 'Site'],
  );
});

// Site IDs are identifiers, not quantities. Losing a leading zero turns site
// 001 into site 1 and stops it matching anything else the trial holds.
test('leading zeros survive a CSV import', async () => {
  const file = path.join(os.tmpdir(), `scm-zeros-${process.pid}.csv`);
  fs.writeFileSync(file, 'Site ID,Site Name,Email\n001,QE,a@nhs.net\n023,Addenbrookes,b@nhs.net\n');
  try {
    const workbook = await readWorkbook(file);
    assert.deepStrictEqual(workbook.sheets[0].rows.map((r) => r['Site ID']), ['001', '023']);
  } finally {
    fs.unlinkSync(file);
  }
});

test('a number shown with a zero-padding format keeps its padding', async () => {
  const file = path.join(os.tmpdir(), `scm-numfmt-${process.pid}.xlsx`);
  const workbook = new ExcelJS.Workbook();
  const sheet = workbook.addWorksheet('Sites');
  sheet.addRow(['Site ID', 'Site Name', 'Email']);
  // Stored as the number 1 but displayed as 001, which is how Excel users
  // normally set site numbers up.
  sheet.addRow([1, 'QE', 'a@nhs.net']).getCell(1).numFmt = '000';
  sheet.addRow([23, 'Addenbrookes', 'b@nhs.net']).getCell(1).numFmt = '000';
  // A real number must not be padded or otherwise mangled.
  sheet.addRow([3.5, 'Decimal', 'c@nhs.net']);
  await workbook.xlsx.writeFile(file);

  try {
    const read = await readWorkbook(file);
    assert.deepStrictEqual(read.sheets[0].rows.map((r) => r['Site ID']), ['001', '023', '3.5']);
  } finally {
    fs.unlinkSync(file);
  }
});

test('an unsupported file type is rejected with a clear message', async () => {
  await assert.rejects(() => readWorkbook('/tmp/nope.docx'), /Unsupported file type/);
});

test('a legacy .xls is rejected with instructions rather than a parse error', async () => {
  await assert.rejects(() => readWorkbook('/tmp/old.xls'), /Save As/);
});

test('reads the per-contact sheet of the sample workbook', { skip: !hasSample && 'run `npm run sample` first' }, async () => {
  const workbook = await readWorkbook(SAMPLE);
  const sheet = workbook.sheets.find((s) => s.name === 'Contacts by person');

  // The title row and the blank row above the headers must be skipped.
  assert.deepStrictEqual(sheet.headers, ['Site ID', 'Site Name', 'City', 'Status', 'Contact Name', 'Role', 'Email']);
  assert.strictEqual(sheet.firstDataRow, 4);

  const { sites, warnings } = buildSites(sheet.rows, detectMapping(sheet.headers));
  assert.strictEqual(sites.length, 6);

  const qe = sites.find((s) => s.siteId === '001');
  assert.strictEqual(qe.contacts.length, 3);
  assert.strictEqual(qe.status, 'Recruiting');
  assert.strictEqual(qe.fields.city, 'Birmingham');

  // The cell holding two addresses becomes two contacts.
  const freeman = sites.find((s) => s.siteId === '004');
  assert.strictEqual(freeman.contacts.length, 2);

  // The row number in the warning must match what the user sees in Excel.
  assert.strictEqual(warnings.length, 1);
  assert.strictEqual(warnings[0].row, 11);
  assert.match(warnings[0].message, /Southmead/);
});

test('reads the per-site sheet of the sample workbook', { skip: !hasSample && 'run `npm run sample` first' }, async () => {
  const workbook = await readWorkbook(SAMPLE);
  const sheet = workbook.sheets.find((s) => s.name === 'One row per site');
  const mapping = detectMapping(sheet.headers);
  assert.strictEqual(mapping.layout, 'per-site');

  const { sites } = buildSites(sheet.rows, mapping);
  const qe = sites.find((s) => s.siteId === '001');
  assert.strictEqual(qe.contacts.length, 3);

  const pi = qe.contacts.find((c) => c.role === 'PI');
  assert.strictEqual(pi.name, 'Dr Jane Bloggs');
  assert.strictEqual(pi.email, 'jane.bloggs@uhb.nhs.uk');

  // A blank email column contributes nothing rather than an empty contact.
  const addenbrookes = sites.find((s) => s.siteId === '002');
  assert.strictEqual(addenbrookes.contacts.length, 2);

  // Numeric columns are still usable as merge fields.
  assert.strictEqual(qe.fields.randomised, '24');
});

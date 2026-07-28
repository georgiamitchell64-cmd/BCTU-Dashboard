'use strict';

const test = require('node:test');
const assert = require('node:assert');

const { parseAddressCell, dedupeContacts, firstNameOf, formatAddress } = require('../src/shared/emails');
const { detectMapping, buildSites, mergeSiteLists, roleFromEmailColumn } = require('../src/shared/importer');

test('parseAddressCell splits the separators a real spreadsheet uses', () => {
  const { contacts } = parseAddressCell('a@nhs.net; b@nhs.net, c@nhs.net\nd@nhs.net');
  assert.deepStrictEqual(contacts.map((c) => c.email), ['a@nhs.net', 'b@nhs.net', 'c@nhs.net', 'd@nhs.net']);
});

test('parseAddressCell keeps display names', () => {
  const { contacts } = parseAddressCell('Jane Bloggs <j.bloggs@nhs.net>; k@nhs.net');
  assert.strictEqual(contacts[0].name, 'Jane Bloggs');
  assert.strictEqual(contacts[0].email, 'j.bloggs@nhs.net');
  assert.strictEqual(contacts[1].name, '');
});

test('parseAddressCell recovers an address written without brackets', () => {
  const { contacts, invalid } = parseAddressCell('Dr Ade Okoro a.okoro@nhs.net');
  assert.strictEqual(invalid.length, 0);
  assert.strictEqual(contacts[0].email, 'a.okoro@nhs.net');
  assert.strictEqual(contacts[0].name, 'Dr Ade Okoro');
});

test('parseAddressCell reports rubbish rather than dropping it', () => {
  const { contacts, invalid } = parseAddressCell('to be confirmed; real@nhs.net');
  assert.deepStrictEqual(contacts.map((c) => c.email), ['real@nhs.net']);
  assert.deepStrictEqual(invalid, ['to be confirmed']);
});

test('parseAddressCell ignores empty cells', () => {
  assert.deepStrictEqual(parseAddressCell('').contacts, []);
  assert.deepStrictEqual(parseAddressCell(null).contacts, []);
  assert.deepStrictEqual(parseAddressCell('   ').contacts, []);
});

test('dedupeContacts is case-insensitive and keeps the better name', () => {
  const result = dedupeContacts([
    { name: '', email: 'A@NHS.net' },
    { name: 'Ann', email: 'a@nhs.net' },
  ]);
  assert.strictEqual(result.length, 1);
  assert.strictEqual(result[0].name, 'Ann');
});

test('firstNameOf strips titles and falls back to the address', () => {
  assert.strictEqual(firstNameOf({ name: 'Dr Jane Bloggs' }), 'Jane');
  assert.strictEqual(firstNameOf({ name: '', email: 'ade.okoro@nhs.net' }), 'Ade');
});

test('formatAddress quotes names that need it', () => {
  assert.strictEqual(formatAddress({ name: 'Jane Bloggs', email: 'j@nhs.net' }), 'Jane Bloggs <j@nhs.net>');
  assert.strictEqual(formatAddress({ name: 'Bloggs, Jane', email: 'j@nhs.net' }), '"Bloggs, Jane" <j@nhs.net>');
  assert.strictEqual(formatAddress({ name: '', email: 'j@nhs.net' }), 'j@nhs.net');
});

// ── Layout detection ──────────────────────────────────────────────────────

test('detects the one-row-per-contact layout', () => {
  const mapping = detectMapping(['Site ID', 'Site Name', 'Contact Name', 'Role', 'Email']);
  assert.strictEqual(mapping.layout, 'per-contact');
  assert.strictEqual(mapping.siteId, 'Site ID');
  assert.strictEqual(mapping.siteName, 'Site Name');
  assert.strictEqual(mapping.contactName, 'Contact Name');
  assert.strictEqual(mapping.role, 'Role');
  assert.deepStrictEqual(mapping.emailColumns.map((c) => c.column), ['Email']);
});

test('detects the one-row-per-site layout and pairs name columns', () => {
  const mapping = detectMapping(['Centre No', 'Hospital', 'PI Name', 'PI Email', 'Research Nurse', 'Research Nurse Email']);
  assert.strictEqual(mapping.layout, 'per-site');
  assert.strictEqual(mapping.siteId, 'Centre No');
  assert.strictEqual(mapping.siteName, 'Hospital');
  const pi = mapping.emailColumns.find((c) => c.column === 'PI Email');
  assert.strictEqual(pi.role, 'PI');
  assert.strictEqual(pi.nameColumn, 'PI Name');
  const nurse = mapping.emailColumns.find((c) => c.column === 'Research Nurse Email');
  assert.strictEqual(nurse.nameColumn, 'Research Nurse');
});

test('a column is never mapped to two roles at once', () => {
  const mapping = detectMapping(['Site', 'Name', 'Email']);
  assert.notStrictEqual(mapping.siteName, mapping.contactName);
});

test('roleFromEmailColumn strips the email wording', () => {
  assert.strictEqual(roleFromEmailColumn('PI Email'), 'PI');
  assert.strictEqual(roleFromEmailColumn('Research Nurse E-mail Address'), 'Research Nurse');
  assert.strictEqual(roleFromEmailColumn('Email'), 'Contact');
});

// ── Building sites ────────────────────────────────────────────────────────

const perContactRows = [
  { 'Site ID': '001', 'Site Name': 'Queen Elizabeth', 'Contact Name': 'Jane Bloggs', Role: 'PI', Email: 'jane@qe.nhs.uk' },
  { 'Site ID': '001', 'Site Name': 'Queen Elizabeth', 'Contact Name': 'Ade Okoro', Role: 'Nurse', Email: 'ade@qe.nhs.uk' },
  { 'Site ID': '002', 'Site Name': 'Addenbrookes', 'Contact Name': 'Sam Reed', Role: 'PI', Email: 'sam@add.nhs.uk' },
];

test('groups contacts under one site', () => {
  const mapping = detectMapping(Object.keys(perContactRows[0]));
  const { sites } = buildSites(perContactRows, mapping);
  assert.strictEqual(sites.length, 2);
  const qe = sites.find((s) => s.siteId === '001');
  assert.strictEqual(qe.contacts.length, 2);
  assert.deepStrictEqual(qe.contacts.map((c) => c.role), ['PI', 'Nurse']);
});

test('splits a multi-address cell into separate contacts', () => {
  const rows = [{ 'Site Name': 'Royal Free', Email: 'a@nhs.net; b@nhs.net' }];
  const { sites } = buildSites(rows, detectMapping(['Site Name', 'Email']));
  assert.strictEqual(sites[0].contacts.length, 2);
});

test('builds one site per row in the per-site layout', () => {
  const headers = ['Centre No', 'Hospital', 'PI Name', 'PI Email', 'Nurse Email'];
  const rows = [
    { 'Centre No': '01', Hospital: 'QE', 'PI Name': 'Jane Bloggs', 'PI Email': 'jane@qe.nhs.uk', 'Nurse Email': 'nurse@qe.nhs.uk' },
  ];
  const { sites } = buildSites(rows, detectMapping(headers));
  assert.strictEqual(sites.length, 1);
  assert.strictEqual(sites[0].contacts.length, 2);
  const pi = sites[0].contacts.find((c) => c.email === 'jane@qe.nhs.uk');
  assert.strictEqual(pi.name, 'Jane Bloggs');
  assert.strictEqual(pi.role, 'PI');
});

test('unmapped columns become merge fields on the site', () => {
  const rows = [{ 'Site Name': 'QE', Email: 'a@nhs.net', 'Randomised To Date': '14', Region: 'West Midlands' }];
  const mapping = detectMapping(Object.keys(rows[0]));
  const { sites } = buildSites(rows, mapping);
  assert.strictEqual(sites[0].fields.randomised_to_date, '14');
  assert.strictEqual(sites[0].fields.region, 'West Midlands');
});

test('warns about rows with no address and rows with no site', () => {
  const rows = [
    { 'Site Name': 'QE', Email: '' },
    { 'Site Name': '', Email: 'orphan@nhs.net' },
  ];
  const { sites, warnings } = buildSites(rows, detectMapping(['Site Name', 'Email']));
  assert.strictEqual(sites.length, 1);
  assert.ok(warnings.some((w) => /No email address/.test(w.message)));
  assert.ok(warnings.some((w) => /No site name or ID/.test(w.message)));
});

test('a disabled email column is not imported', () => {
  const headers = ['Hospital', 'PI Email', 'Admin Email'];
  const rows = [{ Hospital: 'QE', 'PI Email': 'pi@nhs.net', 'Admin Email': 'admin@nhs.net' }];
  const mapping = detectMapping(headers);
  mapping.emailColumns.find((c) => c.column === 'Admin Email').enabled = false;
  const { sites } = buildSites(rows, mapping);
  assert.deepStrictEqual(sites[0].contacts.map((c) => c.email), ['pi@nhs.net']);
});

test('the same address twice in one site collapses to one contact', () => {
  const rows = [
    { 'Site Name': 'QE', Email: 'dup@nhs.net' },
    { 'Site Name': 'QE', Email: 'DUP@nhs.net' },
  ];
  const { sites } = buildSites(rows, detectMapping(['Site Name', 'Email']));
  assert.strictEqual(sites[0].contacts.length, 1);
});

test('merging keeps existing sites and adds new addresses', () => {
  const existing = [{
    key: 'qe', siteId: 'QE', siteName: 'Queen Elizabeth', status: '', fields: {},
    contacts: [{ name: 'Jane', email: 'jane@qe.nhs.uk', role: 'PI', selected: false }],
  }];
  const incoming = [
    { key: 'qe', siteId: 'QE', siteName: 'Queen Elizabeth', status: 'Recruiting', fields: { region: 'WM' }, contacts: [{ name: 'Ade', email: 'ade@qe.nhs.uk', role: 'Nurse', selected: true }] },
    { key: 'add', siteId: 'ADD', siteName: 'Addenbrookes', status: '', fields: {}, contacts: [] },
  ];
  const merged = mergeSiteLists(existing, incoming, 'merge');
  assert.strictEqual(merged.length, 2);
  const qe = merged.find((s) => s.key === 'qe');
  assert.strictEqual(qe.contacts.length, 2);
  // The de-selection the user made previously must survive a re-import.
  assert.strictEqual(qe.contacts[0].selected, false);
  assert.strictEqual(qe.status, 'Recruiting');
});

test('replacing discards the old list', () => {
  const existing = [{ key: 'old', siteId: 'O', siteName: 'Old', contacts: [], fields: {} }];
  const incoming = [{ key: 'new', siteId: 'N', siteName: 'New', contacts: [], fields: {} }];
  assert.deepStrictEqual(mergeSiteLists(existing, incoming, 'replace'), incoming);
});

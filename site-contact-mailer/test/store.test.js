'use strict';

const test = require('node:test');
const assert = require('node:assert');
const fs = require('fs');
const os = require('os');
const path = require('path');

const { Store, DEFAULT_SETTINGS } = require('../src/main/store');

function tempStore(seed) {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scm-store-'));
  if (seed) fs.writeFileSync(path.join(dir, 'data.json'), JSON.stringify(seed), 'utf8');
  return { dir, store: new Store(dir) };
}

test('a fresh store defaults to formatted messages', () => {
  const { store } = tempStore();
  assert.strictEqual(store.getSettings().bodyFormat, 'html');
  assert.strictEqual(store.getSettings().deliveryMethod, 'eml');
  assert.strictEqual(store.getSettings().putSelfInTo, true);
});

test('a version 1 store is migrated off plain text', () => {
  // Version 1 defaulted to plain, which would now discard the formatting.
  const { store } = tempStore({
    version: 1,
    settings: { ...DEFAULT_SETTINGS, bodyFormat: 'plain', senderAddress: 'me@bham.ac.uk' },
    sites: [],
    templates: [],
  });
  assert.strictEqual(store.getSettings().bodyFormat, 'html');
  // Other settings must survive the migration untouched.
  assert.strictEqual(store.getSettings().senderAddress, 'me@bham.ac.uk');
  assert.strictEqual(store.state.version, 3);
});

test('a version 2 store is migrated to named sites in the charts', () => {
  // Anonymising was the old default and was never exposed in the UI, so a
  // stored `true` is the old default rather than a choice to preserve.
  const { store } = tempStore({
    version: 2,
    settings: { ...DEFAULT_SETTINGS, anonymiseOtherSites: true },
    sites: [],
    templates: [],
  });
  assert.strictEqual(store.getSettings().anonymiseOtherSites, false);
  assert.strictEqual(store.state.version, 3);
});

test('completeness movement is measured against the previous import', () => {
  const { store } = tempStore();
  const dataset = (percent) => ({
    sites: [{ key: 'a', siteName: 'A', percent, rank: 1 }],
    totals: { percent },
  });

  store.setCompleteness(dataset(70), { importedAt: '2026-06-01T00:00:00.000Z' });
  assert.strictEqual(store.getCompleteness().sites[0].percentChange, null);

  const second = store.setCompleteness(dataset(85), { importedAt: '2026-07-01T00:00:00.000Z' });
  assert.strictEqual(second.sites[0].previousPercent, 70);
  assert.strictEqual(second.sites[0].percentChange, 15);
  assert.strictEqual(store.state.completenessHistory.length, 2);
});

test('removing completeness data keeps the history, so movement survives a re-import', () => {
  const { store } = tempStore();
  store.setCompleteness({ sites: [{ key: 'a', percent: 70, rank: 1 }], totals: { percent: 70 } });
  store.clearCompleteness();
  assert.strictEqual(store.getCompleteness(), null);
  assert.strictEqual(store.state.completenessHistory.length, 1);

  store.clearCompleteness({ keepHistory: false });
  assert.strictEqual(store.state.completenessHistory.length, 0);
});

test('a version 2 store keeps a deliberate plain-text choice', () => {
  const { store } = tempStore({
    version: 2,
    settings: { ...DEFAULT_SETTINGS, bodyFormat: 'plain' },
    sites: [],
    templates: [],
  });
  assert.strictEqual(store.getSettings().bodyFormat, 'plain');
});

test('templates keep both the HTML and the plain-text alternative', () => {
  const { store } = tempStore();
  const saved = store.saveTemplate({
    name: 'Monthly update',
    subject: '{{site_name}}',
    bodyHtml: '<p>Hello <b>{{site_name}}</b></p>',
    body: 'Hello {{site_name}}',
  });
  assert.strictEqual(saved.bodyHtml, '<p>Hello <b>{{site_name}}</b></p>');
  assert.strictEqual(saved.body, 'Hello {{site_name}}');

  // And they survive a reload from disk.
  const reopened = new Store(path.dirname(store.file));
  assert.strictEqual(reopened.getTemplates()[0].bodyHtml, '<p>Hello <b>{{site_name}}</b></p>');
});

test('saving a template twice under the same id updates rather than duplicates', () => {
  const { store } = tempStore();
  const first = store.saveTemplate({ name: 'A', subject: 's', bodyHtml: '<p>1</p>' });
  store.saveTemplate({ id: first.id, name: 'A', subject: 's', bodyHtml: '<p>2</p>' });
  assert.strictEqual(store.getTemplates().length, 1);
  assert.strictEqual(store.getTemplates()[0].bodyHtml, '<p>2</p>');
});

test('a corrupt store is set aside rather than losing the app', () => {
  const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'scm-store-'));
  fs.writeFileSync(path.join(dir, 'data.json'), '{ this is not json', 'utf8');
  const store = new Store(dir);

  assert.deepStrictEqual(store.getSites(), []);
  // The unreadable file is kept, so nothing the user imported is destroyed.
  assert.ok(fs.readdirSync(dir).some((f) => f.startsWith('data.json.corrupt-')));
});

test('sites round-trip through disk', () => {
  const { store } = tempStore();
  store.setSites([{ key: 'qe', siteId: '001', siteName: 'QE', contacts: [], fields: {} }]);
  const reopened = new Store(path.dirname(store.file));
  assert.strictEqual(reopened.getSites()[0].siteName, 'QE');
});

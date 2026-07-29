import assert from 'node:assert/strict';
import { test } from 'node:test';

import { parseQuickAdd } from '../src/js/core/parse.js';

// 2026-09-09 is a Wednesday — every relative expectation below hangs off that.
const TODAY = '2026-09-09';
const opts = { today: TODAY, weekStart: 1, trialCodes: ['TONIC', 'BCTU2'] };

test('a plain sentence stays a plain sentence', () => {
  const result = parseQuickAdd('Call the site about the missing CRF', opts);
  assert.equal(result.title, 'Call the site about the missing CRF');
  assert.equal(result.date, null);
  assert.equal(result.urgency, null);
});

test('the kitchen sink parses and leaves a clean title', () => {
  const result = parseQuickAdd(
    'Chase site 12 for SAE forms tomorrow at 9:30 !1 @tonic #monitoring ~45m *',
    opts
  );
  assert.equal(result.title, 'Chase site 12 for SAE forms');
  assert.equal(result.date, '2026-09-10');
  assert.equal(result.time, '09:30');
  assert.equal(result.urgency, 'critical');
  assert.equal(result.trial, 'tonic');
  assert.deepEqual(result.tags, ['monitoring']);
  assert.equal(result.estimate, 45);
  assert.equal(result.important, true);
});

test('day names resolve forwards, and "next" jumps a week', () => {
  assert.equal(parseQuickAdd('write report friday', opts).date, '2026-09-11');
  assert.equal(parseQuickAdd('write report next friday', opts).date, '2026-09-18');
  // Today's own weekday means today.
  assert.equal(parseQuickAdd('write report wednesday', opts).date, TODAY);
});

test('relative phrases', () => {
  assert.equal(parseQuickAdd('x today', opts).date, TODAY);
  assert.equal(parseQuickAdd('x tomorrow', opts).date, '2026-09-10');
  assert.equal(parseQuickAdd('x in 3 days', opts).date, '2026-09-12');
  assert.equal(parseQuickAdd('x in 2 weeks', opts).date, '2026-09-23');
  assert.equal(parseQuickAdd('x eow', opts).date, '2026-09-13');
  assert.equal(parseQuickAdd('x eom', opts).date, '2026-09-30');
});

test('dates are read UK-first and roll to next year when already past', () => {
  assert.equal(parseQuickAdd('deadline 12/09', opts).date, '2026-09-12');
  assert.equal(parseQuickAdd('deadline 01/03', opts).date, '2027-03-01');
  assert.equal(parseQuickAdd('deadline 12/09/2027', opts).date, '2027-09-12');
  assert.equal(parseQuickAdd('deadline 2026-10-05', opts).date, '2026-10-05');
});

test('times in both notations', () => {
  assert.equal(parseQuickAdd('standup at 14:00', opts).time, '14:00');
  assert.equal(parseQuickAdd('standup at 3pm', opts).time, '15:00');
  assert.equal(parseQuickAdd('standup at 9.30am', opts).time, null); // dots are not a time separator
  assert.equal(parseQuickAdd('standup 7:05am', opts).time, '07:05');
});

test('urgency shorthand', () => {
  assert.equal(parseQuickAdd('a !1', opts).urgency, 'critical');
  assert.equal(parseQuickAdd('a !4', opts).urgency, 'low');
  assert.equal(parseQuickAdd('a !urgent', opts).urgency, 'critical');
  assert.equal(parseQuickAdd('a !!', opts).urgency, 'high');
  // An unknown bang word is left in the title rather than silently eaten.
  assert.equal(parseQuickAdd('a !whatever', opts).title, 'a !whatever');
});

test('estimates in minutes and hours', () => {
  assert.equal(parseQuickAdd('a ~30m', opts).estimate, 30);
  assert.equal(parseQuickAdd('a ~2h', opts).estimate, 120);
  assert.equal(parseQuickAdd('a ~1.5h', opts).estimate, 90);
  assert.equal(parseQuickAdd('a ~90', opts).estimate, 90);
});

test('trial codes are only accepted when they exist', () => {
  assert.equal(parseQuickAdd('a @tonic', opts).trial, 'tonic');
  const unknown = parseQuickAdd('a @nosuchtrial', opts);
  assert.equal(unknown.trial, null);
  assert.equal(unknown.title, 'a @nosuchtrial');
});

test('repeat phrases', () => {
  assert.deepEqual(parseQuickAdd('standup every day', opts).recur, { freq: 'daily', interval: 1, byday: [] });
  assert.deepEqual(parseQuickAdd('review every 2 weeks', opts).recur, { freq: 'weekly', interval: 2, byday: [] });
  const monday = parseQuickAdd('TMG every monday', opts);
  assert.deepEqual(monday.recur, { freq: 'weekly', interval: 1, byday: [1] });
  assert.equal(monday.date, '2026-09-14');
  assert.equal(monday.title, 'TMG');
  assert.deepEqual(parseQuickAdd('timesheet every weekday', opts).recur, { freq: 'weekdays', interval: 1, byday: [] });
});

test('planning horizon keywords', () => {
  assert.equal(parseQuickAdd('tidy the shared drive this week', opts).scope, 'week');
  assert.equal(parseQuickAdd('portfolio review this month', opts).scope, 'month');
});

test('multiple tags, deduplicated', () => {
  const result = parseQuickAdd('a #one #two #one', opts);
  assert.deepEqual(result.tags, ['one', 'two']);
  assert.equal(result.title, 'a');
});

test('empty and shorthand-only input is handled', () => {
  assert.equal(parseQuickAdd('', opts).title, '');
  assert.equal(parseQuickAdd('   ', opts).title, '');
  assert.equal(parseQuickAdd('!1 @tonic', opts).title, '');
});

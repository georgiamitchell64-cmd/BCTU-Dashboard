import assert from 'node:assert/strict';
import { test } from 'node:test';

import { describeRule, matches, nextOccurrence, occurrencesBetween } from '../src/js/core/recur.js';

test('daily rules step by the interval', () => {
  const rule = { freq: 'daily', interval: 3 };
  assert.equal(nextOccurrence(rule, '2026-09-01'), '2026-09-01');
  assert.equal(nextOccurrence(rule, '2026-09-01', '2026-09-01'), '2026-09-04');
  assert.ok(!matches(rule, '2026-09-01', '2026-09-03'));
});

test('weekdays skip the weekend', () => {
  const rule = { freq: 'weekdays', interval: 1 };
  // 2026-09-04 is a Friday.
  assert.equal(nextOccurrence(rule, '2026-09-01', '2026-09-04'), '2026-09-07');
});

test('weekly rules honour selected days and the interval', () => {
  const rule = { freq: 'weekly', interval: 2, byday: [1, 4] }; // Mon + Thu
  const anchor = '2026-09-07'; // a Monday
  assert.ok(matches(rule, anchor, '2026-09-10')); // Thu, same week
  assert.ok(!matches(rule, anchor, '2026-09-14')); // Mon, skipped week
  assert.ok(matches(rule, anchor, '2026-09-21')); // Mon, two weeks on
});

test('monthly rules clamp rather than skip a short month', () => {
  const rule = { freq: 'monthly', interval: 1, monthDay: 31 };
  const anchor = '2026-01-31';
  assert.ok(matches(rule, anchor, '2026-02-28'));
  assert.ok(matches(rule, anchor, '2026-04-30'));
  assert.ok(!matches(rule, anchor, '2026-04-29'));
});

test('monthly last-day rules land on the last day', () => {
  const rule = { freq: 'monthly', interval: 1, monthDay: 'last' };
  assert.ok(matches(rule, '2026-01-31', '2026-02-28'));
  assert.ok(matches(rule, '2026-01-31', '2026-03-31'));
});

test('until stops the series', () => {
  const rule = { freq: 'daily', interval: 1, until: '2026-09-03' };
  assert.equal(nextOccurrence(rule, '2026-09-01', '2026-09-03'), null);
  assert.deepEqual(
    occurrencesBetween(rule, '2026-09-01', '2026-09-01', '2026-09-10'),
    ['2026-09-01', '2026-09-02', '2026-09-03']
  );
});

test('occurrences never run away', () => {
  const rule = { freq: 'daily', interval: 1 };
  const list = occurrencesBetween(rule, '2020-01-01', '2020-01-01', '2030-01-01', 10);
  assert.equal(list.length, 10);
});

test('rules describe themselves in plain English', () => {
  assert.equal(describeRule(null), 'Does not repeat');
  assert.equal(describeRule({ freq: 'daily', interval: 1 }), 'Every day');
  assert.equal(describeRule({ freq: 'weekly', interval: 2, byday: [1, 4] }), 'Every 2 weeks on Mon, Thu');
  assert.equal(describeRule({ freq: 'weekdays', interval: 1 }), 'Every weekday');
});

test('a malformed rule is treated as no rule', () => {
  assert.equal(nextOccurrence({}, '2026-09-01'), null);
  assert.equal(nextOccurrence({ freq: 'daily' }, 'not-a-date'), null);
  assert.equal(matches({ freq: 'weekly', byday: 'nonsense' }, '2026-09-07', '2026-09-07'), true);
});

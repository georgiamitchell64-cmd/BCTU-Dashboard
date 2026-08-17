import assert from 'node:assert/strict';
import { test } from 'node:test';

import {
  addDays, addMonths, diffDays, endOfMonth, endOfWeek, formatDuration, isoWeek,
  minutesToTime, monthGrid, relativeLabel, startOfWeek, timeToMinutes, toKey
} from '../src/js/core/dates.js';

test('date keys survive a round trip', () => {
  assert.equal(toKey(new Date(2026, 8, 9)), '2026-09-09');
  assert.equal(addDays('2026-09-09', 1), '2026-09-10');
  assert.equal(addDays('2026-12-31', 1), '2027-01-01');
  assert.equal(addDays('2026-03-01', -1), '2026-02-28');
});

test('addMonths clamps to the end of a short month', () => {
  assert.equal(addMonths('2026-01-31', 1), '2026-02-28');
  assert.equal(addMonths('2024-01-31', 1), '2024-02-29'); // leap year
  assert.equal(addMonths('2026-11-30', 1), '2026-12-30');
});

test('week boundaries respect the week start setting', () => {
  // 2026-09-09 is a Wednesday.
  assert.equal(startOfWeek('2026-09-09', 1), '2026-09-07');
  assert.equal(endOfWeek('2026-09-09', 1), '2026-09-13');
  assert.equal(startOfWeek('2026-09-09', 0), '2026-09-06');
});

test('diffDays is not thrown off by daylight saving', () => {
  // The UK clocks go back on 25 October 2026.
  assert.equal(diffDays('2026-10-24', '2026-10-26'), 2);
  assert.equal(diffDays('2026-10-26', '2026-10-24'), -2);
});

test('month grid covers whole weeks and includes every day of the month', () => {
  const cells = monthGrid('2026-09', 1);
  assert.equal(cells.length % 7, 0);
  assert.ok(cells.includes('2026-09-01'));
  assert.ok(cells.includes('2026-09-30'));
  assert.equal(cells[0], startOfWeek('2026-09-01', 1));
});

test('end of month handles February', () => {
  assert.equal(endOfMonth('2026-02-10'), '2026-02-28');
  assert.equal(endOfMonth('2024-02-10'), '2024-02-29');
});

test('iso week numbers match the standard', () => {
  assert.equal(isoWeek('2026-01-01'), 1);
  assert.equal(isoWeek('2026-12-31'), 53);
});

test('relative labels read naturally', () => {
  assert.equal(relativeLabel('2026-09-09', '2026-09-09'), 'Today');
  assert.equal(relativeLabel('2026-09-10', '2026-09-09'), 'Tomorrow');
  assert.equal(relativeLabel('2026-09-08', '2026-09-09'), 'Yesterday');
  assert.equal(relativeLabel('2026-09-06', '2026-09-09'), '3 days ago');
});

test('time helpers round trip and reject nonsense', () => {
  assert.equal(timeToMinutes('09:30'), 570);
  assert.equal(timeToMinutes('24:00'), null);
  assert.equal(timeToMinutes('nope'), null);
  assert.equal(minutesToTime(570), '09:30');
  assert.equal(formatDuration(90), '1h 30m');
  assert.equal(formatDuration(45), '45m');
  assert.equal(formatDuration(0), '');
});

/* ============================================================================
 * recur.js — repeat rules
 * ----------------------------------------------------------------------------
 * A rule is a plain object:
 *
 *   { freq: 'daily'|'weekdays'|'weekly'|'monthly'|'yearly',
 *     interval: 1,          // every N days/weeks/months/years
 *     byday: [1,3,5],       // weekly only: 0=Sun … 6=Sat
 *     monthDay: 15|'last',  // monthly only; defaults to the anchor's day
 *     until: 'YYYY-MM-DD'|null,
 *     count: 12|null }      // stop after N occurrences
 *
 * Matching is done by walking days from the anchor, which keeps every branch
 * (including month-end clamping) in one obvious place.
 * ========================================================================== */

import {
  addDays, daysInMonth, diffDays, DAY_SHORT, fromKey, isKey, startOfWeek, weekdayOf
} from './dates.js';

export const FREQUENCIES = [
  { id: 'daily', label: 'Daily' },
  { id: 'weekdays', label: 'Every weekday' },
  { id: 'weekly', label: 'Weekly' },
  { id: 'monthly', label: 'Monthly' },
  { id: 'yearly', label: 'Yearly' }
];

const SEARCH_LIMIT = 800; // days — beyond this a rule is effectively finished

export function normaliseRule(rule) {
  if (!rule || !rule.freq) return null;
  const interval = Math.max(1, Math.round(Number(rule.interval) || 1));
  const byday = Array.isArray(rule.byday)
    ? [...new Set(rule.byday.map(Number).filter((d) => d >= 0 && d <= 6))].sort()
    : [];
  return {
    freq: rule.freq,
    interval,
    byday,
    monthDay: rule.monthDay ?? null,
    until: isKey(rule.until) ? rule.until : null,
    count: Number.isFinite(rule.count) && rule.count > 0 ? Math.round(rule.count) : null
  };
}

/** Does `key` land on an occurrence of `rule` anchored at `anchor`? */
export function matches(rule, anchor, key) {
  const normalised = normaliseRule(rule);
  if (!normalised || !isKey(anchor) || !isKey(key)) return false;
  if (key < anchor) return false;
  if (normalised.until && key > normalised.until) return false;

  const { freq, interval, byday, monthDay } = normalised;
  const date = fromKey(key);

  if (freq === 'daily') return diffDays(anchor, key) % interval === 0;

  if (freq === 'weekdays') {
    const day = weekdayOf(key);
    return day >= 1 && day <= 5;
  }

  if (freq === 'weekly') {
    const days = byday.length ? byday : [weekdayOf(anchor)];
    if (!days.includes(weekdayOf(key))) return false;
    const weeks = diffDays(startOfWeek(anchor), startOfWeek(key)) / 7;
    return Number.isInteger(weeks) && weeks % interval === 0;
  }

  if (freq === 'monthly') {
    const anchorDate = fromKey(anchor);
    const months = (date.getFullYear() - anchorDate.getFullYear()) * 12
      + (date.getMonth() - anchorDate.getMonth());
    if (months < 0 || months % interval !== 0) return false;
    const lastDay = daysInMonth(date.getFullYear(), date.getMonth());
    if (monthDay === 'last') return date.getDate() === lastDay;
    const wanted = Number(monthDay) || anchorDate.getDate();
    // A 31st rule falls on the 30th in November rather than skipping the month.
    return date.getDate() === Math.min(wanted, lastDay);
  }

  if (freq === 'yearly') {
    const anchorDate = fromKey(anchor);
    const years = date.getFullYear() - anchorDate.getFullYear();
    if (years < 0 || years % interval !== 0) return false;
    return date.getMonth() === anchorDate.getMonth() && date.getDate() === anchorDate.getDate();
  }

  return false;
}

/** First occurrence strictly after `after` (defaults to the anchor itself). */
export function nextOccurrence(rule, anchor, after = null) {
  const normalised = normaliseRule(rule);
  if (!normalised || !isKey(anchor)) return null;

  let key = after && after >= anchor ? addDays(after, 1) : anchor;
  for (let i = 0; i < SEARCH_LIMIT; i += 1, key = addDays(key, 1)) {
    if (normalised.until && key > normalised.until) return null;
    if (matches(normalised, anchor, key)) return key;
  }
  return null;
}

/** Every occurrence in [from, to], capped so a stray rule cannot hang the UI. */
export function occurrencesBetween(rule, anchor, from, to, limit = 400) {
  const out = [];
  const normalised = normaliseRule(rule);
  if (!normalised || !isKey(anchor) || !isKey(from) || !isKey(to) || to < from) return out;

  let key = from > anchor ? from : anchor;
  let index = 0;
  while (key <= to && out.length < limit && index < SEARCH_LIMIT * 2) {
    if (matches(normalised, anchor, key)) out.push(key);
    key = addDays(key, 1);
    index += 1;
  }
  return normalised.count ? out.slice(0, normalised.count) : out;
}

/** Human-readable summary, e.g. "Every 2 weeks on Mon, Thu until 12 Dec". */
export function describeRule(rule, anchor = null) {
  const normalised = normaliseRule(rule);
  if (!normalised) return 'Does not repeat';

  const { freq, interval, byday, monthDay, until, count } = normalised;
  const every = interval === 1 ? 'Every' : `Every ${interval}`;
  let text;

  switch (freq) {
    case 'daily':
      text = interval === 1 ? 'Every day' : `${every} days`;
      break;
    case 'weekdays':
      text = 'Every weekday';
      break;
    case 'weekly': {
      const days = (byday.length ? byday : anchor ? [weekdayOf(anchor)] : [])
        .map((d) => DAY_SHORT[d]).join(', ');
      text = `${interval === 1 ? 'Every week' : `${every} weeks`}${days ? ` on ${days}` : ''}`;
      break;
    }
    case 'monthly': {
      const day = monthDay === 'last'
        ? 'the last day'
        : `day ${Number(monthDay) || (anchor ? fromKey(anchor).getDate() : 1)}`;
      text = `${interval === 1 ? 'Every month' : `${every} months`} on ${day}`;
      break;
    }
    case 'yearly':
      text = interval === 1 ? 'Every year' : `${every} years`;
      break;
    default:
      text = 'Repeats';
  }

  if (count) text += `, ${count} times`;
  if (until) text += `, until ${until}`;
  return text;
}

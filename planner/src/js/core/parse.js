/* ============================================================================
 * parse.js — natural-language quick add
 * ----------------------------------------------------------------------------
 * Turns a single line such as
 *
 *   "Chase site 12 for SAE forms tomorrow at 9:30 !1 @tonic #monitoring ~45m"
 *
 * into a structured draft task. Everything it recognises is stripped from the
 * title, so what's left reads like a normal sentence.
 *
 * Dates are UK-first: 09/12 is 9 December.
 * ========================================================================== */

import { addDays, addMonths, DAY_NAMES, endOfMonth, endOfWeek, isoWeek, todayKey, weekdayOf } from './dates.js';

export const URGENCIES = [
  { id: 'critical', label: 'Critical', short: 'P1', rank: 0 },
  { id: 'high', label: 'High', short: 'P2', rank: 1 },
  { id: 'medium', label: 'Medium', short: 'P3', rank: 2 },
  { id: 'low', label: 'Low', short: 'P4', rank: 3 }
];

export const urgencyRank = (id) => URGENCIES.find((u) => u.id === id)?.rank ?? 2;

export const TOKEN_HELP = [
  { token: 'tomorrow, fri, 12/09, in 3 days', meaning: 'when it is due' },
  { token: 'at 9:30, 3pm', meaning: 'start time' },
  { token: '!1 … !4  or  !urgent', meaning: 'urgency' },
  { token: '@tonic', meaning: 'which trial' },
  { token: '#monitoring', meaning: 'tag' },
  { token: '~45m, ~2h', meaning: 'time estimate' },
  { token: '*', meaning: 'mark as important' },
  { token: 'every monday, every 2 weeks', meaning: 'repeat' },
  { token: 'this week / this month', meaning: 'planning horizon' }
];

const URGENCY_WORDS = {
  1: 'critical', 2: 'high', 3: 'medium', 4: 'low',
  critical: 'critical', urgent: 'critical', crit: 'critical', p1: 'critical',
  high: 'high', hi: 'high', p2: 'high',
  medium: 'medium', med: 'medium', normal: 'medium', p3: 'medium',
  low: 'low', p4: 'low', someday: 'low'
};

const DAY_LOOKUP = (() => {
  const map = {};
  DAY_NAMES.forEach((name, index) => {
    map[name.toLowerCase()] = index;
    map[name.toLowerCase().slice(0, 3)] = index;
  });
  map.tues = 2; map.thur = 4; map.thurs = 4; map.weds = 3;
  return map;
})();

const DAY_PATTERN = Object.keys(DAY_LOOKUP).sort((a, b) => b.length - a.length).join('|');

/** Next date whose weekday is `weekday`; today counts as a match. */
function nextWeekday(weekday, from) {
  const delta = (weekday - weekdayOf(from) + 7) % 7;
  return addDays(from, delta);
}

function clean(text) {
  return text.replace(/\s{2,}/g, ' ').replace(/\s+([,.;:])/g, '$1').trim();
}

/**
 * @param {string} input
 * @param {{ today?: string, weekStart?: number, trialCodes?: string[] }} [options]
 */
export function parseQuickAdd(input, options = {}) {
  const today = options.today || todayKey();
  const weekStart = options.weekStart ?? 1;
  const trialCodes = (options.trialCodes || []).map((c) => String(c).toLowerCase());

  const result = {
    title: '',
    urgency: null,
    trial: null,
    tags: [],
    estimate: null,
    date: null,
    time: null,
    recur: null,
    important: false,
    scope: null,
    matched: []
  };

  let text = ` ${String(input || '')} `;

  /** Strip the first match of `regex`, handing the match to `handler`. */
  const take = (regex, handler) => {
    text = text.replace(regex, (...args) => {
      const match = args.slice(0, -2);
      const outcome = handler(...match);
      if (outcome === false) return match[0];
      result.matched.push(match[0].trim());
      return ' ';
    });
  };

  /* ── Repeat rules (first: they contain day names and numbers) ───────────── */
  take(/\severy\s+(\d+)?\s*(day|days|week|weeks|month|months|year|years)\b/i, (_all, count, unit) => {
    const interval = Math.max(1, Number(count) || 1);
    const freq = { day: 'daily', week: 'weekly', month: 'monthly', year: 'yearly' }[unit.replace(/s$/, '')];
    result.recur = { freq, interval, byday: [] };
  });
  take(new RegExp(`\\severy\\s+(${DAY_PATTERN})\\b`, 'i'), (_all, day) => {
    result.recur = { freq: 'weekly', interval: 1, byday: [DAY_LOOKUP[day.toLowerCase()]] };
    if (!result.date) result.date = nextWeekday(DAY_LOOKUP[day.toLowerCase()], today);
  });
  take(/\severy\s+(weekday|working day)s?\b/i, () => {
    result.recur = { freq: 'weekdays', interval: 1, byday: [] };
  });
  take(/\s(daily|weekly|monthly|yearly|annually)\b/i, (_all, word) => {
    const freq = { daily: 'daily', weekly: 'weekly', monthly: 'monthly', yearly: 'yearly', annually: 'yearly' }[word.toLowerCase()];
    result.recur = { freq, interval: 1, byday: [] };
  });

  /* ── Planning horizon ──────────────────────────────────────────────────── */
  take(/\sthis\s+week\b/i, () => { result.scope = 'week'; });
  take(/\sthis\s+month\b/i, () => { result.scope = 'month'; });

  /* ── Explicit dates ────────────────────────────────────────────────────── */
  take(/\s(\d{4})-(\d{2})-(\d{2})\b/, (_all, y, m, d) => {
    result.date = `${y}-${m}-${d}`;
  });
  take(/\s(\d{1,2})[/.](\d{1,2})(?:[/.](\d{2,4}))?\b/, (_all, d, m, y) => {
    const day = Number(d);
    const month = Number(m);
    if (day < 1 || day > 31 || month < 1 || month > 12) return false;
    let year = y ? Number(y) : Number(today.slice(0, 4));
    if (y && y.length === 2) year += 2000;
    const key = `${year}-${String(month).padStart(2, '0')}-${String(day).padStart(2, '0')}`;
    // A bare day/month that has already gone means next year.
    result.date = !y && key < today ? `${year + 1}${key.slice(4)}` : key;
    return true;
  });

  /* ── Relative dates ────────────────────────────────────────────────────── */
  take(/\sin\s+(\d+)\s+(day|days|week|weeks|month|months)\b/i, (_all, count, unit) => {
    const n = Number(count);
    if (/^month/i.test(unit)) result.date = addMonths(today, n);
    else result.date = addDays(today, /^week/i.test(unit) ? n * 7 : n);
  });
  take(/\s(today|tonight)\b/i, () => { result.date = today; });
  take(/\s(tomorrow|tmrw?|tmw)\b/i, () => { result.date = addDays(today, 1); });
  take(/\syesterday\b/i, () => { result.date = addDays(today, -1); });
  take(/\s(eod|end of day)\b/i, () => { result.date = today; });
  take(/\s(eow|end of week)\b/i, () => { result.date = endOfWeek(today, weekStart); });
  take(/\s(eom|end of month)\b/i, () => { result.date = endOfMonth(today); });
  take(/\snext\s+week\b/i, () => { result.date = addDays(today, 7); result.scope = result.scope || 'week'; });
  take(/\snext\s+month\b/i, () => { result.date = addMonths(today, 1); result.scope = result.scope || 'month'; });
  take(new RegExp(`\\s(next\\s+)?(?:on\\s+)?(${DAY_PATTERN})\\b`, 'i'), (_all, next, day) => {
    let key = nextWeekday(DAY_LOOKUP[day.toLowerCase()], today);
    // "next friday" means the one in the week after this, not this week's.
    if (next && isoWeek(key) === isoWeek(today)) key = addDays(key, 7);
    result.date = key;
  });

  /* ── Times ─────────────────────────────────────────────────────────────── */
  take(/\s(?:at\s+)?([01]?\d|2[0-3]):([0-5]\d)\b/, (_all, h, m) => {
    result.time = `${String(Number(h)).padStart(2, '0')}:${m}`;
  });
  take(/\s(?:at\s+)?(\d{1,2})(?::([0-5]\d))?\s*(am|pm)\b/i, (_all, h, m, meridiem) => {
    let hours = Number(h) % 12;
    if (/pm/i.test(meridiem)) hours += 12;
    result.time = `${String(hours).padStart(2, '0')}:${m || '00'}`;
  });

  /* ── Tokens ────────────────────────────────────────────────────────────── */
  take(/\s~\s?(\d+(?:\.\d+)?)\s*(m|min|mins|minutes|h|hr|hrs|hours)?\b/i, (_all, amount, unit) => {
    const value = Number(amount);
    const isHours = unit ? /^h/i.test(unit) : value <= 8;
    result.estimate = Math.max(1, Math.round(isHours ? value * 60 : value));
  });
  take(/\s!([a-z0-9]+)\b/i, (_all, word) => {
    const urgency = URGENCY_WORDS[word.toLowerCase()];
    if (!urgency) return false;
    result.urgency = urgency;
    return true;
  });
  take(/\s(!{1,3})(?=\s|$)/, (_all, bangs) => {
    result.urgency = ['medium', 'high', 'critical'][bangs.length - 1];
  });
  take(/\s@([\w-]+)\b/g, (_all, code) => {
    const lower = code.toLowerCase();
    if (trialCodes.length && !trialCodes.includes(lower)) return false;
    result.trial = lower;
    return true;
  });
  text = text.replace(/\s#([\w-]+)/g, (all, tag) => {
    result.tags.push(tag.toLowerCase());
    result.matched.push(all.trim());
    return ' ';
  });
  take(/\s\*(?=\s|$)/, () => { result.important = true; });

  result.tags = [...new Set(result.tags)];
  result.title = clean(text);
  return result;
}

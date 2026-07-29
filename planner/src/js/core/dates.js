/* ============================================================================
 * dates.js — date helpers
 * ----------------------------------------------------------------------------
 * Dates are stored as local "YYYY-MM-DD" keys, never as timestamps. A task
 * planned for Tuesday should stay on Tuesday regardless of clocks, DST or the
 * hour the app happens to be opened.
 * ========================================================================== */

export const MS_DAY = 86400000;

export const DAY_NAMES = ['Sunday', 'Monday', 'Tuesday', 'Wednesday', 'Thursday', 'Friday', 'Saturday'];
export const DAY_SHORT = ['Sun', 'Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat'];
export const MONTH_NAMES = ['January', 'February', 'March', 'April', 'May', 'June',
  'July', 'August', 'September', 'October', 'November', 'December'];

const pad = (n) => String(n).padStart(2, '0');

/** Local date key for a Date object. */
export function toKey(date) {
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}-${pad(date.getDate())}`;
}

/** Parse a "YYYY-MM-DD" key into a local Date at midnight. */
export function fromKey(key) {
  const [y, m, d] = String(key).split('-').map(Number);
  return new Date(y, (m || 1) - 1, d || 1);
}

export const isKey = (value) => /^\d{4}-\d{2}-\d{2}$/.test(String(value || ''));

export const todayKey = (now = new Date()) => toKey(now);

export function addDays(key, days) {
  const date = fromKey(key);
  date.setDate(date.getDate() + days);
  return toKey(date);
}

export function addMonths(key, months) {
  const date = fromKey(key);
  const targetMonth = date.getMonth() + months;
  const anchor = new Date(date.getFullYear(), targetMonth, 1);
  // Clamp: 31 Jan + 1 month is 28/29 Feb, not 2/3 March.
  const lastDay = daysInMonth(anchor.getFullYear(), anchor.getMonth());
  anchor.setDate(Math.min(date.getDate(), lastDay));
  return toKey(anchor);
}

export const daysInMonth = (year, monthIndex) => new Date(year, monthIndex + 1, 0).getDate();

/** Whole days from `a` to `b` (negative if b is before a). */
export function diffDays(a, b) {
  const ms = fromKey(b).setHours(12, 0, 0, 0) - fromKey(a).setHours(12, 0, 0, 0);
  return Math.round(ms / MS_DAY);
}

export const weekdayOf = (key) => fromKey(key).getDay();

/** Monday-start by default; `weekStart` of 0 gives Sunday-start weeks. */
export function startOfWeek(key, weekStart = 1) {
  const day = weekdayOf(key);
  const delta = (day - weekStart + 7) % 7;
  return addDays(key, -delta);
}

export const endOfWeek = (key, weekStart = 1) => addDays(startOfWeek(key, weekStart), 6);

export const weekDays = (key, weekStart = 1) => {
  const start = startOfWeek(key, weekStart);
  return Array.from({ length: 7 }, (_, i) => addDays(start, i));
};

export const monthKey = (key) => String(key).slice(0, 7);
export const startOfMonth = (key) => `${monthKey(key)}-01`;

export function endOfMonth(key) {
  const date = fromKey(key);
  return toKey(new Date(date.getFullYear(), date.getMonth() + 1, 0));
}

export function addMonthsToMonthKey(mKey, months) {
  const [y, m] = mKey.split('-').map(Number);
  const date = new Date(y, m - 1 + months, 1);
  return `${date.getFullYear()}-${pad(date.getMonth() + 1)}`;
}

/** Every day cell for a month grid, padded to whole weeks. */
export function monthGrid(mKey, weekStart = 1) {
  const first = `${mKey}-01`;
  const gridStart = startOfWeek(first, weekStart);
  const last = endOfMonth(first);
  const gridEnd = endOfWeek(last, weekStart);
  const cells = [];
  for (let key = gridStart; ; key = addDays(key, 1)) {
    cells.push(key);
    if (key === gridEnd) break;
  }
  return cells;
}

/** ISO-8601 week number — the one clinical trial reporting tends to use. */
export function isoWeek(key) {
  const date = fromKey(key);
  const target = new Date(date.getFullYear(), date.getMonth(), date.getDate());
  target.setDate(target.getDate() + 3 - ((target.getDay() + 6) % 7));
  const firstThursday = new Date(target.getFullYear(), 0, 4);
  firstThursday.setDate(firstThursday.getDate() + 3 - ((firstThursday.getDay() + 6) % 7));
  return 1 + Math.round((target - firstThursday) / (7 * MS_DAY));
}

/* ── Formatting ───────────────────────────────────────────────────────────── */

export function formatLong(key) {
  const date = fromKey(key);
  return `${DAY_NAMES[date.getDay()]} ${date.getDate()} ${MONTH_NAMES[date.getMonth()]} ${date.getFullYear()}`;
}

export function formatShort(key) {
  const date = fromKey(key);
  return `${date.getDate()} ${MONTH_NAMES[date.getMonth()].slice(0, 3)}`;
}

export function formatMonth(mKey) {
  const [y, m] = mKey.split('-').map(Number);
  return `${MONTH_NAMES[m - 1]} ${y}`;
}

export function formatWeekRange(key, weekStart = 1) {
  const start = startOfWeek(key, weekStart);
  const end = endOfWeek(key, weekStart);
  return `${formatShort(start)} – ${formatShort(end)}`;
}

/** "Today", "Tomorrow", "3 days overdue", "Fri 12 Sep" — whichever reads best. */
export function relativeLabel(key, today = todayKey()) {
  const delta = diffDays(today, key);
  if (delta === 0) return 'Today';
  if (delta === 1) return 'Tomorrow';
  if (delta === -1) return 'Yesterday';
  if (delta < 0) return `${Math.abs(delta)} days ago`;
  if (delta < 7) return DAY_NAMES[weekdayOf(key)];
  return formatShort(key);
}

/** Minutes since midnight from "HH:MM"; null when absent or malformed. */
export function timeToMinutes(time) {
  const match = /^(\d{1,2}):(\d{2})$/.exec(String(time || ''));
  if (!match) return null;
  const hours = Number(match[1]);
  const minutes = Number(match[2]);
  if (hours > 23 || minutes > 59) return null;
  return hours * 60 + minutes;
}

export function minutesToTime(minutes) {
  const total = ((Math.round(minutes) % 1440) + 1440) % 1440;
  return `${pad(Math.floor(total / 60))}:${pad(total % 60)}`;
}

export function formatTime(time, use24h = true) {
  const minutes = timeToMinutes(time);
  if (minutes === null) return '';
  if (use24h) return minutesToTime(minutes);
  const hours = Math.floor(minutes / 60);
  const suffix = hours < 12 ? 'am' : 'pm';
  const display = hours % 12 === 0 ? 12 : hours % 12;
  const mins = minutes % 60;
  return mins ? `${display}.${pad(mins)}${suffix}` : `${display}${suffix}`;
}

export function formatDuration(minutes) {
  if (!minutes || minutes <= 0) return '';
  if (minutes < 60) return `${minutes}m`;
  const hours = Math.floor(minutes / 60);
  const rest = minutes % 60;
  return rest ? `${hours}h ${rest}m` : `${hours}h`;
}

export function greeting(now = new Date()) {
  const hour = now.getHours();
  if (hour < 5) return 'Still up';
  if (hour < 12) return 'Good morning';
  if (hour < 18) return 'Good afternoon';
  return 'Good evening';
}

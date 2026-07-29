/* ============================================================================
 * select.js — querying, sorting and summarising tasks
 * ----------------------------------------------------------------------------
 * Pure functions over a task array. No DOM, no store — so every view shares
 * one definition of "what counts as overdue" and the whole lot is testable.
 * ========================================================================== */

import { addDays, diffDays, monthKey, startOfWeek, timeToMinutes, todayKey } from './dates.js';
import { urgencyRank } from './parse.js';
import { occurrencesBetween } from './recur.js';

export const SORTS = [
  { id: 'manual', label: 'My order' },
  { id: 'urgency', label: 'Urgency' },
  { id: 'time', label: 'Time of day' },
  { id: 'trial', label: 'Trial' },
  { id: 'title', label: 'A–Z' },
  { id: 'created', label: 'Newest first' }
];

export const emptyFilter = () => ({
  query: '',
  trialId: 'all',
  urgency: 'all',
  tag: 'all',
  showDone: true,
  sort: 'manual'
});

const byOrder = (a, b) => (a.order - b.order) || a.createdAt.localeCompare(b.createdAt);

export function sortTasks(tasks, mode = 'manual', trials = []) {
  const list = [...tasks];
  const trialName = (id) => trials.find((t) => t.id === id)?.code ?? '~';

  switch (mode) {
    case 'urgency':
      return list.sort((a, b) => urgencyRank(a.urgency) - urgencyRank(b.urgency)
        || Number(b.important) - Number(a.important)
        || byOrder(a, b));
    case 'time':
      return list.sort((a, b) => {
        const at = timeToMinutes(a.time);
        const bt = timeToMinutes(b.time);
        if (at === null && bt === null) return byOrder(a, b);
        if (at === null) return 1;
        if (bt === null) return -1;
        return at - bt || byOrder(a, b);
      });
    case 'trial':
      return list.sort((a, b) => trialName(a.trialId).localeCompare(trialName(b.trialId)) || byOrder(a, b));
    case 'title':
      return list.sort((a, b) => a.title.localeCompare(b.title, undefined, { sensitivity: 'base' }));
    case 'created':
      return list.sort((a, b) => b.createdAt.localeCompare(a.createdAt));
    default:
      return list.sort(byOrder);
  }
}

/** Completed tasks always sink below open ones within a list. */
export const doneLast = (tasks) => [
  ...tasks.filter((t) => !t.done),
  ...tasks.filter((t) => t.done)
];

export function matchesFilter(task, filter, trials = []) {
  if (!filter) return true;
  if (!filter.showDone && task.done) return false;
  if (filter.trialId && filter.trialId !== 'all') {
    if (filter.trialId === 'none' ? task.trialId : task.trialId !== filter.trialId) return false;
  }
  if (filter.urgency && filter.urgency !== 'all' && task.urgency !== filter.urgency) return false;
  if (filter.tag && filter.tag !== 'all' && !task.tags.includes(filter.tag)) return false;

  const query = (filter.query || '').trim().toLowerCase();
  if (query) {
    const trial = trials.find((t) => t.id === task.trialId);
    const haystack = [
      task.title, task.notes, ...task.tags, trial?.code, trial?.name,
      ...task.subtasks.map((s) => s.title)
    ].filter(Boolean).join(' ').toLowerCase();
    if (!query.split(/\s+/).every((word) => haystack.includes(word))) return false;
  }
  return true;
}

export const applyFilter = (tasks, filter, trials = []) =>
  tasks.filter((task) => matchesFilter(task, filter, trials));

/* ── Bucket queries ───────────────────────────────────────────────────────── */

export const dayTasks = (tasks, key) =>
  tasks.filter((t) => t.scope === 'day' && t.date === key);

export const weekTasks = (tasks, key, weekStart = 1) => {
  const monday = startOfWeek(key, weekStart);
  return tasks.filter((t) => t.scope === 'week' && t.week === monday);
};

export const monthTasks = (tasks, key) => {
  const month = key.length === 7 ? key : monthKey(key);
  return tasks.filter((t) => t.scope === 'month' && t.month === month);
};

/** Day-scope tasks anywhere inside the given week, for weekly roll-ups. */
export const daysInWeekTasks = (tasks, key, weekStart = 1) => {
  const monday = startOfWeek(key, weekStart);
  const sunday = addDays(monday, 6);
  return tasks.filter((t) => t.scope === 'day' && t.date >= monday && t.date <= sunday);
};

export const overdueTasks = (tasks, today = todayKey()) =>
  tasks.filter((t) => t.scope === 'day' && !t.done && t.date && t.date < today);

/** Ghost markers for future repeats, so the calendar shows what is coming. */
export function projectedOccurrences(tasks, from, to) {
  const out = [];
  for (const task of tasks) {
    if (!task.recur || task.scope !== 'day') continue;
    for (const key of occurrencesBetween(task.recur, task.anchor || task.date, from, to, 60)) {
      if (key !== task.date) out.push({ task, date: key });
    }
  }
  return out;
}

/* ── Summaries ────────────────────────────────────────────────────────────── */

export function progress(tasks) {
  const total = tasks.length;
  const done = tasks.filter((t) => t.done).length;
  return { total, done, remaining: total - done, pct: total ? Math.round((done / total) * 100) : 0 };
}

export function workload(tasks) {
  const open = tasks.filter((t) => !t.done);
  const minutes = open.reduce((sum, t) => sum + (t.estimate || 0), 0);
  const unestimated = open.filter((t) => !t.estimate).length;
  return { minutes, unestimated, count: open.length };
}

export const countByUrgency = (tasks) => tasks.reduce((acc, task) => {
  acc[task.urgency] = (acc[task.urgency] || 0) + 1;
  return acc;
}, {});

export function countByTrial(tasks, trials) {
  const rows = trials.map((trial) => {
    const mine = tasks.filter((t) => t.trialId === trial.id);
    return { trial, total: mine.length, done: mine.filter((t) => t.done).length };
  });
  const untagged = tasks.filter((t) => !t.trialId);
  if (untagged.length) {
    rows.push({
      trial: { id: 'none', code: 'No trial', name: 'Not linked to a trial', colour: '#94A3B8' },
      total: untagged.length,
      done: untagged.filter((t) => t.done).length
    });
  }
  return rows.sort((a, b) => b.total - a.total);
}

export function allTags(tasks) {
  const counts = new Map();
  for (const task of tasks) {
    for (const tag of task.tags) counts.set(tag, (counts.get(tag) || 0) + 1);
  }
  return [...counts.entries()]
    .sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]))
    .map(([tag, count]) => ({ tag, count }));
}

/** Was anything completed on `key`? Counts one-off completions and repeats. */
export function completionsOn(tasks, key) {
  return tasks.filter((task) => {
    if (task.history?.includes(key)) return true;
    return task.done && task.completedAt?.slice(0, 10) === key;
  });
}

/** Consecutive days up to today with at least one completion. */
export function streak(tasks, today = todayKey()) {
  let count = 0;
  for (let i = 0; i < 400; i += 1) {
    const key = addDays(today, -i);
    const hits = completionsOn(tasks, key).length;
    if (hits === 0) {
      // Today not being finished yet shouldn't wipe out yesterday's streak.
      if (i === 0) continue;
      break;
    }
    count += 1;
  }
  return count;
}

export function dailyCompletionSeries(tasks, today = todayKey(), days = 28) {
  return Array.from({ length: days }, (_, index) => {
    const key = addDays(today, -(days - 1 - index));
    return { date: key, count: completionsOn(tasks, key).length };
  });
}

/* ── Priority matrix ──────────────────────────────────────────────────────── */

export const QUADRANTS = [
  { id: 'do', label: 'Do now', hint: 'Urgent + important', urgent: true, important: true },
  { id: 'plan', label: 'Schedule', hint: 'Important, not urgent', urgent: false, important: true },
  { id: 'delegate', label: 'Delegate / quick', hint: 'Urgent, not important', urgent: true, important: false },
  { id: 'drop', label: 'Later', hint: 'Neither', urgent: false, important: false }
];

export const isUrgent = (task, today = todayKey()) => {
  if (task.urgency === 'critical' || task.urgency === 'high') return true;
  if (task.scope === 'day' && task.date) return diffDays(today, task.date) <= 1;
  return false;
};

export function quadrantOf(task, today = todayKey()) {
  const urgent = isUrgent(task, today);
  const important = task.important || task.urgency === 'critical';
  return QUADRANTS.find((q) => q.urgent === urgent && q.important === important)?.id ?? 'drop';
}

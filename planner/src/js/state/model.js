/* ============================================================================
 * model.js — shape of the saved data
 * ----------------------------------------------------------------------------
 * One JSON document holds everything: profile, settings, trials, tasks and
 * daily notes. Every read path goes through `migrate()` so an older or
 * hand-edited file can never crash the app — unknown fields are dropped and
 * missing ones are filled with defaults.
 * ========================================================================== */

import { isKey, minutesToTime, monthKey, startOfWeek, timeToMinutes, todayKey } from '../core/dates.js';
import { normaliseRule } from '../core/recur.js';
import { URGENCIES } from '../core/parse.js';

export const DATA_VERSION = 1;

export const TRIAL_COLOURS = [
  '#2EC4A5', '#3B82F6', '#8B5CF6', '#F59E0B', '#EF4444',
  '#EC4899', '#14B8A6', '#6366F1', '#84CC16', '#F97316'
];

export const uid = (prefix = 't') => {
  const random = globalThis.crypto?.randomUUID
    ? globalThis.crypto.randomUUID().replace(/-/g, '').slice(0, 12)
    : Math.random().toString(36).slice(2, 14);
  return `${prefix}_${random}`;
};

const nowISO = () => new Date().toISOString();
const str = (value, fallback = '') => (typeof value === 'string' ? value : fallback);
const bool = (value, fallback = false) => (typeof value === 'boolean' ? value : fallback);
const num = (value, fallback = null) => (Number.isFinite(Number(value)) && value !== null && value !== '' ? Number(value) : fallback);
const oneOf = (value, allowed, fallback) => (allowed.includes(value) ? value : fallback);

/* ── Defaults ─────────────────────────────────────────────────────────────── */

export function defaultSettings() {
  return {
    theme: 'system',
    accent: '#2EC4A5',
    density: 'comfortable',
    weekStart: 1,
    use24h: true,
    showWeekends: true,
    dayStart: '08:00',
    dayEnd: '18:00',
    defaultTrial: null,
    defaultUrgency: 'medium',
    defaultSort: 'manual',
    carryOverdue: true,
    notifications: true,
    reminderLead: 15,
    celebrate: true,
    focusMinutes: 25,
    breakMinutes: 5,
    startView: 'today',
    hiddenViews: []
  };
}

export function defaultProfile() {
  return { name: 'Georgia Mitchell', role: 'Trial Manager', showGreeting: true };
}

export function defaultTrials() {
  return [{
    id: uid('tr'),
    code: 'TONIC',
    name: 'TONIC — Early Parenteral Nutrition vs Standard Care',
    colour: '#2EC4A5',
    active: true,
    notes: '',
    createdAt: nowISO()
  }];
}

export function defaultState() {
  return {
    version: DATA_VERSION,
    profile: defaultProfile(),
    settings: defaultSettings(),
    trials: defaultTrials(),
    tasks: [],
    notes: {},
    meta: { createdAt: nowISO(), lastOpened: nowISO() }
  };
}

/* ── Tasks ────────────────────────────────────────────────────────────────── */

export function createTask(partial = {}, settings = defaultSettings()) {
  const scope = oneOf(partial.scope, ['day', 'week', 'month'], 'day');
  const today = todayKey();
  const date = isKey(partial.date) ? partial.date : today;

  return normaliseTask({
    id: uid(),
    title: str(partial.title).trim() || 'Untitled task',
    notes: str(partial.notes),
    scope,
    date: scope === 'day' ? date : null,
    week: scope === 'week' ? startOfWeek(isKey(partial.week) ? partial.week : date, settings.weekStart) : null,
    month: scope === 'month' ? (partial.month || monthKey(date)) : null,
    time: partial.time || null,
    estimate: num(partial.estimate),
    urgency: oneOf(partial.urgency, URGENCIES.map((u) => u.id), settings.defaultUrgency),
    important: bool(partial.important),
    trialId: partial.trialId ?? settings.defaultTrial ?? null,
    tags: Array.isArray(partial.tags) ? partial.tags : [],
    subtasks: Array.isArray(partial.subtasks) ? partial.subtasks : [],
    recur: partial.recur || null,
    anchor: isKey(partial.anchor) ? partial.anchor : date,
    history: [],
    done: false,
    completedAt: null,
    order: num(partial.order, Date.now()),
    createdAt: nowISO(),
    updatedAt: nowISO()
  }, settings);
}

export function normaliseTask(raw, settings = defaultSettings()) {
  if (!raw || typeof raw !== 'object') return null;

  const scope = oneOf(raw.scope, ['day', 'week', 'month'], 'day');
  const date = isKey(raw.date) ? raw.date : null;
  const fallbackDate = date || todayKey();

  const task = {
    id: str(raw.id) || uid(),
    title: str(raw.title).trim() || 'Untitled task',
    notes: str(raw.notes),
    scope,
    date: scope === 'day' ? (date || fallbackDate) : null,
    week: scope === 'week'
      ? startOfWeek(isKey(raw.week) ? raw.week : fallbackDate, settings.weekStart)
      : null,
    month: scope === 'month'
      ? (/^\d{4}-\d{2}$/.test(raw.month) ? raw.month : monthKey(fallbackDate))
      : null,
    time: timeToMinutes(raw.time) === null ? null : minutesToTime(timeToMinutes(raw.time)),
    estimate: num(raw.estimate),
    urgency: oneOf(raw.urgency, URGENCIES.map((u) => u.id), 'medium'),
    important: bool(raw.important),
    trialId: raw.trialId ? str(raw.trialId) : null,
    tags: Array.isArray(raw.tags)
      ? [...new Set(raw.tags.filter((t) => typeof t === 'string' && t.trim()).map((t) => t.trim().toLowerCase()))]
      : [],
    subtasks: Array.isArray(raw.subtasks)
      ? raw.subtasks
        .filter((s) => s && typeof s.title === 'string')
        .map((s) => ({ id: str(s.id) || uid('s'), title: s.title.trim(), done: bool(s.done) }))
      : [],
    recur: normaliseRule(raw.recur),
    anchor: isKey(raw.anchor) ? raw.anchor : (date || fallbackDate),
    history: Array.isArray(raw.history) ? raw.history.filter(isKey) : [],
    done: bool(raw.done),
    completedAt: str(raw.completedAt) || null,
    order: num(raw.order, 0),
    createdAt: str(raw.createdAt) || nowISO(),
    updatedAt: str(raw.updatedAt) || nowISO()
  };

  if (task.done && !task.completedAt) task.completedAt = nowISO();
  if (!task.done) task.completedAt = null;
  return task;
}

function normaliseTrial(raw, index = 0) {
  if (!raw || typeof raw !== 'object') return null;
  const code = str(raw.code).trim() || `TRIAL${index + 1}`;
  return {
    id: str(raw.id) || uid('tr'),
    code: code.toUpperCase().slice(0, 16),
    name: str(raw.name).trim() || code,
    colour: /^#[0-9a-f]{6}$/i.test(raw.colour) ? raw.colour : TRIAL_COLOURS[index % TRIAL_COLOURS.length],
    active: raw.active !== false,
    notes: str(raw.notes),
    createdAt: str(raw.createdAt) || nowISO()
  };
}

/* ── Migration ────────────────────────────────────────────────────────────── */

export function migrate(raw) {
  const base = defaultState();
  if (!raw || typeof raw !== 'object') return base;

  const settings = { ...base.settings, ...(typeof raw.settings === 'object' && raw.settings ? raw.settings : {}) };
  settings.theme = oneOf(settings.theme, ['system', 'light', 'dark'], 'system');
  settings.density = oneOf(settings.density, ['comfortable', 'compact'], 'comfortable');
  settings.weekStart = oneOf(Number(settings.weekStart), [0, 1], 1);
  settings.accent = /^#[0-9a-f]{6}$/i.test(settings.accent) ? settings.accent : base.settings.accent;
  settings.focusMinutes = Math.min(120, Math.max(5, num(settings.focusMinutes, 25)));
  settings.breakMinutes = Math.min(60, Math.max(1, num(settings.breakMinutes, 5)));
  settings.reminderLead = Math.min(180, Math.max(0, num(settings.reminderLead, 15)));
  settings.hiddenViews = Array.isArray(settings.hiddenViews) ? settings.hiddenViews : [];

  const trials = (Array.isArray(raw.trials) ? raw.trials : base.trials)
    .map(normaliseTrial)
    .filter(Boolean);

  const trialIds = new Set(trials.map((t) => t.id));
  const tasks = (Array.isArray(raw.tasks) ? raw.tasks : [])
    .map((task) => normaliseTask(task, settings))
    .filter(Boolean)
    .map((task) => (task.trialId && !trialIds.has(task.trialId) ? { ...task, trialId: null } : task));

  if (settings.defaultTrial && !trialIds.has(settings.defaultTrial)) settings.defaultTrial = null;

  const notes = {};
  if (raw.notes && typeof raw.notes === 'object') {
    for (const [key, value] of Object.entries(raw.notes)) {
      if (isKey(key) && typeof value === 'string' && value.trim()) notes[key] = value;
    }
  }

  return {
    version: DATA_VERSION,
    profile: { ...base.profile, ...(typeof raw.profile === 'object' && raw.profile ? raw.profile : {}) },
    settings,
    trials,
    tasks,
    notes,
    meta: { ...base.meta, ...(typeof raw.meta === 'object' && raw.meta ? raw.meta : {}), lastOpened: nowISO() }
  };
}

/* ── Sample content for a brand new file ──────────────────────────────────── */

export function seedTasks(state) {
  const today = todayKey();
  const trialId = state.trials[0]?.id ?? null;
  const drafts = [
    { title: 'Welcome — click me to see the task editor', notes: 'Everything here is editable. Delete these three when you are ready to start.', urgency: 'medium', time: '09:00', estimate: 15, trialId, subtasks: [{ id: uid('s'), title: 'Try dragging a task to another day', done: false }, { id: uid('s'), title: 'Press Ctrl+K for the command palette', done: false }] },
    { title: 'Type a task then press Enter — try "review SAE log tomorrow 2pm !1 #safety"', urgency: 'high', trialId },
    { title: 'Weekly: site contact round-up', scope: 'week', urgency: 'medium', trialId },
    { title: 'Monthly: TMG report and portfolio review', scope: 'month', urgency: 'high', trialId }
  ];
  return drafts.map((draft, index) => createTask({ ...draft, date: today, order: index }, state.settings));
}

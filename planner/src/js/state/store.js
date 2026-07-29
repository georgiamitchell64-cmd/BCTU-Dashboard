/* ============================================================================
 * store.js — application state
 * ----------------------------------------------------------------------------
 * A small observable store with undo/redo and debounced persistence.
 *
 * Persistence goes through `window.planner` (the Electron preload bridge) when
 * it exists and falls back to localStorage otherwise, which is what makes the
 * renderer runnable in a plain browser for development and testing.
 * ========================================================================== */

import { createTask, defaultState, migrate, normaliseTask, seedTasks, uid } from './model.js';
import { todayKey } from '../core/dates.js';
import { nextOccurrence } from '../core/recur.js';

const BRIDGE = globalThis.planner ?? null;
const LOCAL_KEY = 'bctu-planner-data';
const SAVE_DEBOUNCE = 400;
const HISTORY_LIMIT = 60;

const clone = (value) => (globalThis.structuredClone
  ? globalThis.structuredClone(value)
  : JSON.parse(JSON.stringify(value)));

class Store {
  constructor() {
    this.state = defaultState();
    this.listeners = new Set();
    this.undoStack = [];
    this.redoStack = [];
    this.saveTimer = null;
    this.ready = false;
  }

  /* ── Lifecycle ──────────────────────────────────────────────────────────── */

  async init() {
    let raw = null;
    try {
      raw = BRIDGE ? await BRIDGE.load() : JSON.parse(localStorage.getItem(LOCAL_KEY) || 'null');
    } catch (error) {
      console.error('Could not read saved data, starting fresh.', error);
    }

    const isFirstRun = !raw;
    this.state = migrate(raw);
    if (isFirstRun) {
      this.state.tasks = seedTasks(this.state);
      this.persist();
    }
    this.ready = true;
    this.emit();
    return this.state;
  }

  subscribe(listener) {
    this.listeners.add(listener);
    return () => this.listeners.delete(listener);
  }

  emit() {
    for (const listener of this.listeners) listener(this.state);
  }

  persist() {
    clearTimeout(this.saveTimer);
    this.saveTimer = setTimeout(() => {
      const snapshot = clone(this.state);
      if (BRIDGE) BRIDGE.save(snapshot).catch((error) => console.error('Save failed', error));
      else localStorage.setItem(LOCAL_KEY, JSON.stringify(snapshot));
    }, SAVE_DEBOUNCE);
  }

  /**
   * Apply a mutation to a draft copy of the state.
   * @param {(draft: object) => void} mutator
   * @param {{ label?: string, undoable?: boolean }} [options]
   */
  update(mutator, options = {}) {
    const { label = 'change', undoable = true } = options;
    const previous = clone(this.state);
    const draft = clone(this.state);
    mutator(draft);
    this.state = draft;

    if (undoable) {
      this.undoStack.push({ label, state: previous });
      if (this.undoStack.length > HISTORY_LIMIT) this.undoStack.shift();
      this.redoStack.length = 0;
    }
    this.persist();
    this.emit();
    return this.state;
  }

  /** Replace everything (import, reset). */
  replace(raw, { undoable = true } = {}) {
    const previous = clone(this.state);
    this.state = migrate(raw);
    if (undoable) this.undoStack.push({ label: 'replace data', state: previous });
    this.persist();
    this.emit();
  }

  undo() {
    const entry = this.undoStack.pop();
    if (!entry) return null;
    this.redoStack.push({ label: entry.label, state: clone(this.state) });
    this.state = entry.state;
    this.persist();
    this.emit();
    return entry.label;
  }

  redo() {
    const entry = this.redoStack.pop();
    if (!entry) return null;
    this.undoStack.push({ label: entry.label, state: clone(this.state) });
    this.state = entry.state;
    this.persist();
    this.emit();
    return entry.label;
  }

  get canUndo() { return this.undoStack.length > 0; }
  get canRedo() { return this.redoStack.length > 0; }

  /* ── Convenience accessors ──────────────────────────────────────────────── */

  get settings() { return this.state.settings; }
  get tasks() { return this.state.tasks; }
  get trials() { return this.state.trials; }
  get profile() { return this.state.profile; }

  task(id) { return this.state.tasks.find((t) => t.id === id) || null; }
  trial(id) { return this.state.trials.find((t) => t.id === id) || null; }
  trialByCode(code) {
    const wanted = String(code || '').toLowerCase();
    return this.state.trials.find((t) => t.code.toLowerCase() === wanted) || null;
  }

  /* ── Task operations ────────────────────────────────────────────────────── */

  addTask(partial) {
    let created = null;
    this.update((draft) => {
      created = createTask(partial, draft.settings);
      // New work goes to the top of its list rather than the bottom.
      const siblings = draft.tasks.filter((t) => t.scope === created.scope);
      created.order = Math.min(0, ...siblings.map((t) => t.order)) - 1;
      draft.tasks.push(created);
    }, { label: 'add task' });
    return created;
  }

  updateTask(id, patch, label = 'edit task') {
    this.update((draft) => {
      const index = draft.tasks.findIndex((t) => t.id === id);
      if (index === -1) return;
      const merged = { ...draft.tasks[index], ...patch, updatedAt: new Date().toISOString() };
      draft.tasks[index] = normaliseTask(merged, draft.settings);
    }, { label });
  }

  removeTask(id) {
    this.update((draft) => {
      draft.tasks = draft.tasks.filter((t) => t.id !== id);
    }, { label: 'delete task' });
  }

  duplicateTask(id) {
    const original = this.task(id);
    if (!original) return null;
    const copy = {
      ...clone(original),
      id: uid(),
      title: `${original.title} (copy)`,
      done: false,
      completedAt: null,
      history: [],
      order: original.order - 0.5,
      createdAt: new Date().toISOString()
    };
    this.update((draft) => { draft.tasks.push(copy); }, { label: 'duplicate task' });
    return copy;
  }

  /**
   * Toggle completion. A repeating task is never "finished": completing it
   * records the date and rolls it forward to its next occurrence.
   */
  toggleTask(id) {
    let outcome = { completed: false, rolledTo: null };
    this.update((draft) => {
      const task = draft.tasks.find((t) => t.id === id);
      if (!task) return;

      if (task.done) {
        task.done = false;
        task.completedAt = null;
        return;
      }

      if (task.recur && task.scope === 'day') {
        const from = task.date || todayKey();
        const next = nextOccurrence(task.recur, task.anchor || from, from);
        task.history = [...new Set([...(task.history || []), from])].slice(-400);
        if (next) {
          task.date = next;
          task.done = false;
          task.completedAt = null;
          outcome = { completed: true, rolledTo: next };
          return;
        }
      }

      task.done = true;
      task.completedAt = new Date().toISOString();
      outcome = { completed: true, rolledTo: null };
    }, { label: 'complete task' });
    return outcome;
  }

  toggleSubtask(taskId, subtaskId) {
    this.update((draft) => {
      const task = draft.tasks.find((t) => t.id === taskId);
      const subtask = task?.subtasks.find((s) => s.id === subtaskId);
      if (subtask) subtask.done = !subtask.done;
    }, { label: 'check subtask' });
  }

  /** Move a task to a day/week/month bucket, optionally into a position. */
  moveTask(id, target) {
    this.update((draft) => {
      const task = draft.tasks.find((t) => t.id === id);
      if (!task) return;
      if (target.scope) task.scope = target.scope;
      if (target.date !== undefined) task.date = target.date;
      if (target.week !== undefined) task.week = target.week;
      if (target.month !== undefined) task.month = target.month;
      if (target.order !== undefined) task.order = target.order;
      if (task.scope === 'day' && task.recur) task.anchor = task.date;
      task.updatedAt = new Date().toISOString();
      const normalised = normaliseTask(task, draft.settings);
      draft.tasks = draft.tasks.map((t) => (t.id === id ? normalised : t));
    }, { label: 'move task' });
  }

  /** Pull unfinished work from before `today` onto today. */
  carryOverdue(today = todayKey()) {
    let moved = 0;
    this.update((draft) => {
      for (const task of draft.tasks) {
        if (task.scope === 'day' && !task.done && task.date && task.date < today) {
          task.date = today;
          task.updatedAt = new Date().toISOString();
          moved += 1;
        }
      }
    }, { label: 'carry over' });
    return moved;
  }

  clearCompleted(filter = () => true) {
    let removed = 0;
    this.update((draft) => {
      const before = draft.tasks.length;
      draft.tasks = draft.tasks.filter((t) => !(t.done && filter(t)));
      removed = before - draft.tasks.length;
    }, { label: 'clear completed' });
    return removed;
  }

  /* ── Trials ─────────────────────────────────────────────────────────────── */

  addTrial(partial) {
    let created = null;
    this.update((draft) => {
      created = {
        id: uid('tr'),
        code: String(partial.code || 'TRIAL').toUpperCase().slice(0, 16),
        name: partial.name || partial.code || 'New trial',
        colour: partial.colour || '#3B82F6',
        active: true,
        notes: partial.notes || '',
        createdAt: new Date().toISOString()
      };
      draft.trials.push(created);
    }, { label: 'add trial' });
    return created;
  }

  updateTrial(id, patch) {
    this.update((draft) => {
      const trial = draft.trials.find((t) => t.id === id);
      if (!trial) return;
      Object.assign(trial, patch);
      trial.code = String(trial.code || 'TRIAL').toUpperCase().slice(0, 16);
    }, { label: 'edit trial' });
  }

  /** Deleting a trial keeps its tasks; they simply become untagged. */
  removeTrial(id) {
    this.update((draft) => {
      draft.trials = draft.trials.filter((t) => t.id !== id);
      draft.tasks = draft.tasks.map((t) => (t.trialId === id ? { ...t, trialId: null } : t));
      if (draft.settings.defaultTrial === id) draft.settings.defaultTrial = null;
    }, { label: 'delete trial' });
  }

  /* ── Settings, profile, notes ───────────────────────────────────────────── */

  setSetting(key, value) {
    this.update((draft) => { draft.settings[key] = value; }, { label: 'settings', undoable: false });
  }

  setProfile(patch) {
    this.update((draft) => { Object.assign(draft.profile, patch); }, { label: 'profile', undoable: false });
  }

  setNote(dateKey, text) {
    this.update((draft) => {
      if (text && text.trim()) draft.notes[dateKey] = text;
      else delete draft.notes[dateKey];
    }, { label: 'note', undoable: false });
  }
}

export const store = new Store();
export { clone };

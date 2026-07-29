/* ============================================================================
 * task.js — the task row, used by every view
 * ----------------------------------------------------------------------------
 * One renderer keeps a task looking and behaving the same whether it is in the
 * Today list, a week column, the matrix or a search result.
 * ========================================================================== */

import { h, icon, toast } from './dom.js';
import { makeDraggable } from './dnd.js';
import { store } from '../state/store.js';
import { formatDuration, formatTime, relativeLabel, todayKey } from '../core/dates.js';
import { URGENCIES } from '../core/parse.js';
import { describeRule } from '../core/recur.js';
import { openTaskEditor } from './editor.js';

const urgencyLabel = (id) => URGENCIES.find((u) => u.id === id)?.label ?? 'Medium';

export function trialChip(trialId, { small = false } = {}) {
  const trial = store.trial(trialId);
  if (!trial) return null;
  return h(`span.chip${small ? '.tiny' : ''}`, {
    title: trial.name,
    style: {
      background: `color-mix(in srgb, ${trial.colour} 14%, transparent)`,
      color: trial.colour,
      borderColor: `color-mix(in srgb, ${trial.colour} 34%, transparent)`
    }
  }, h('i.chip-dot', { style: { background: trial.colour } }), trial.code);
}

/**
 * @param {object} task
 * @param {{ compact?: boolean, showDate?: boolean, draggable?: boolean,
 *           onChange?: Function, ghostDate?: string }} [options]
 */
export function taskRow(task, options = {}) {
  const { compact = false, showDate = false, draggable = true, onChange = null, ghostDate = null } = options;
  const settings = store.settings;
  const today = todayKey();
  const isGhost = Boolean(ghostDate);
  const overdue = !task.done && task.scope === 'day' && task.date && task.date < today;

  const check = h('button.task-check', {
    type: 'button',
    'aria-label': task.done ? 'Mark as not done' : 'Mark as done',
    onclick: (event) => {
      event.stopPropagation();
      const outcome = store.toggleTask(task.id);
      if (outcome.rolledTo) {
        toast(`Done — next one ${relativeLabel(outcome.rolledTo, today).toLowerCase()}`, { kind: 'success' });
      }
      onChange?.(outcome);
    }
  });

  const meta = [];
  if (task.time) meta.push(h('span.task-time', formatTime(task.time, settings.use24h)));
  if (showDate && task.date) {
    meta.push(h(`span.chip${overdue ? '.pill-overdue' : ''}`, relativeLabel(ghostDate || task.date, today)));
  }
  if (!compact || task.urgency === 'critical' || task.urgency === 'high') {
    meta.push(h('span.chip.chip-urgency', urgencyLabel(task.urgency)));
  }
  const chip = trialChip(task.trialId);
  if (chip) meta.push(chip);
  if (task.important) meta.push(h('span.chip', { title: 'Important', style: { color: 'var(--u-high)' } }, icon('star', { size: 10 })));
  if (task.estimate) meta.push(h('span.chip', icon('clock', { size: 10 }), formatDuration(task.estimate)));
  if (task.recur) {
    meta.push(h('span.chip', { title: describeRule(task.recur, task.anchor) }, icon('repeat', { size: 10 })));
  }
  if (task.notes) meta.push(h('span.task-note-flag', { title: 'Has notes' }, icon('note', { size: 11 })));
  for (const tag of task.tags.slice(0, compact ? 1 : 4)) meta.push(h('span.chip', `#${tag}`));

  if (task.subtasks.length) {
    const done = task.subtasks.filter((s) => s.done).length;
    meta.push(h('span.chip',
      h('span.subtask-bar', h('i', { style: { width: `${(done / task.subtasks.length) * 100}%` } })),
      `${done}/${task.subtasks.length}`));
  }

  const actions = h('div.task-actions',
    h('button.btn.btn-icon.btn-ghost', {
      type: 'button', title: 'Duplicate', 'aria-label': 'Duplicate task',
      onclick: (event) => { event.stopPropagation(); store.duplicateTask(task.id); toast('Duplicated'); }
    }, icon('copy', { size: 13 })),
    h('button.btn.btn-icon.btn-ghost', {
      type: 'button', title: 'Delete', 'aria-label': 'Delete task',
      onclick: (event) => {
        event.stopPropagation();
        const snapshot = { ...task };
        store.removeTask(task.id);
        toast(`Deleted “${truncate(snapshot.title, 32)}”`, {
          kind: 'undo',
          action: { label: 'Undo', onClick: () => store.undo() }
        });
      }
    }, icon('trash', { size: 13 })));

  const row = h(`div.task.u-${task.urgency}`, {
    class: [task.done ? 'done' : '', task.important ? 'important' : '', isGhost ? 'task-ghost' : ''].filter(Boolean).join(' '),
    tabIndex: 0,
    role: 'listitem',
    'aria-label': `${task.title}${task.done ? ', done' : ''}`,
    onclick: () => { if (!isGhost) openTaskEditor(task.id); },
    onkeydown: (event) => {
      if (event.key === 'Enter') { event.preventDefault(); if (!isGhost) openTaskEditor(task.id); }
      if (event.key === ' ') { event.preventDefault(); check.click(); }
    }
  },
  isGhost ? h('span.ico', { style: { color: 'var(--faint)', marginTop: '2px' } }, icon('repeat', { size: 14 })) : check,
  h('div.task-body',
    h('div.task-title', task.title),
    meta.length ? h('div.task-meta', meta) : null),
  isGhost ? null : actions);

  if (overdue) row.style.setProperty('--u', 'var(--u-critical)');
  if (draggable && !isGhost) makeDraggable(row, task.id);
  return row;
}

/** Condensed one-line version used inside month cells. */
export function taskMini(task, { ghost = false } = {}) {
  return h(`div.mini.u-${task.urgency}`, {
    class: [task.done ? 'done' : '', ghost ? 'ghost' : ''].filter(Boolean).join(' '),
    title: task.title,
    style: { '--u': `var(--u-${task.urgency})` },
    dataset: ghost ? {} : { taskId: task.id },
    onclick: (event) => { event.stopPropagation(); if (!ghost) openTaskEditor(task.id); }
  },
  task.time ? h('b', formatTime(task.time, store.settings.use24h)) : null,
  h('span.grow', { style: { overflow: 'hidden', textOverflow: 'ellipsis' } }, task.title));
}

export function taskList(tasks, options = {}) {
  const list = h('div.list-group', { role: 'list' });
  for (const task of tasks) list.appendChild(taskRow(task, options));
  return list;
}

export const truncate = (text, max) => (text.length > max ? `${text.slice(0, max - 1)}…` : text);

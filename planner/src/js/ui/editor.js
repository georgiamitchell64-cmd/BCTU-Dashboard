/* ============================================================================
 * editor.js — the task editor drawer
 * ----------------------------------------------------------------------------
 * Edits a local draft and only writes to the store on Save, so Escape always
 * means "leave it as it was".
 * ========================================================================== */

import { append, clear, confirmDialog, h, icon, overlay, toast } from './dom.js';
import { store } from '../state/store.js';
import {
  DAY_SHORT, formatLong, formatMonth, formatWeekRange, monthKey, startOfWeek, todayKey
} from '../core/dates.js';
import { URGENCIES } from '../core/parse.js';
import { FREQUENCIES, describeRule } from '../core/recur.js';
import { uid } from '../state/model.js';

const SCOPE_OPTIONS = [
  { id: 'day', label: 'A day', hint: 'Lands on a specific date' },
  { id: 'week', label: 'This week', hint: 'Do it sometime this week' },
  { id: 'month', label: 'This month', hint: 'A goal for the month' }
];

/**
 * @param {string|null} taskId  null to create a new task
 * @param {object} [defaults]   seed values when creating
 */
export function openTaskEditor(taskId, defaults = {}) {
  const existing = taskId ? store.task(taskId) : null;
  const settings = store.settings;
  const isNew = !existing;

  const draft = existing
    ? structuredClone(existing)
    : {
      title: '',
      notes: '',
      scope: 'day',
      date: todayKey(),
      week: startOfWeek(todayKey(), settings.weekStart),
      month: monthKey(todayKey()),
      time: null,
      estimate: null,
      urgency: settings.defaultUrgency,
      important: false,
      trialId: settings.defaultTrial ?? null,
      tags: [],
      subtasks: [],
      recur: null,
      ...defaults
    };

  let body;

  const save = (close) => {
    const title = draft.title.trim();
    if (!title) {
      toast('Give the task a title first', { kind: 'error' });
      body.querySelector('[name="title"]')?.focus();
      return;
    }
    draft.title = title;
    if (isNew) {
      store.addTask(draft);
      toast('Task added', { kind: 'success' });
    } else {
      store.updateTask(existing.id, draft);
    }
    close();
  };

  overlay((close) => {
    body = h('div.modal-body');
    const rerender = () => { clear(body); append(body, [fields(draft, rerender)]); };
    rerender();

    const panel = h('div.drawer', {
      role: 'dialog', 'aria-modal': 'true', 'aria-label': isNew ? 'New task' : 'Edit task',
      onkeydown: (event) => {
        if ((event.metaKey || event.ctrlKey) && event.key === 'Enter') { event.preventDefault(); save(close); }
      }
    },
    h('div.modal-head',
      h('h3.card-title.grow', isNew ? 'New task' : 'Edit task'),
      existing ? h('button.btn.btn-sm', {
        type: 'button',
        onclick: () => { store.duplicateTask(existing.id); toast('Duplicated'); close(); }
      }, icon('copy', { size: 12 }), 'Duplicate') : null,
      h('button.btn.btn-icon.btn-ghost', { type: 'button', 'aria-label': 'Close', onclick: close }, icon('close', { size: 15 }))),
    body,
    h('div.modal-foot',
      existing ? h('button.btn.btn-danger', {
        type: 'button',
        onclick: async () => {
          const ok = await confirmDialog({
            title: 'Delete this task?',
            message: `“${existing.title}” will be removed. You can undo this from the toast or with Ctrl+Z.`
          });
          if (!ok) return;
          store.removeTask(existing.id);
          toast('Task deleted', { kind: 'undo', action: { label: 'Undo', onClick: () => store.undo() } });
          close();
        }
      }, icon('trash', { size: 13 }), 'Delete') : null,
      h('span.grow'),
      h('button.btn', { type: 'button', onclick: close }, 'Cancel'),
      h('button.btn.btn-primary', { type: 'button', onclick: () => save(close) },
        isNew ? 'Add task' : 'Save',
        h('span.kbd', { style: { marginLeft: '4px' } }, '⌘⏎'))));

    return panel;
  });
}

/* ── Field groups ─────────────────────────────────────────────────────────── */

function fields(draft, rerender) {
  return [
    h('div.field',
      h('label.field-label', { for: 'task-title' }, 'Task'),
      h('input.input#task-title', {
        name: 'title',
        value: draft.title,
        placeholder: 'What needs doing?',
        'data-autofocus': '',
        oninput: (event) => { draft.title = event.target.value; }
      })),

    scopeField(draft, rerender),
    schedulingField(draft, rerender),
    urgencyField(draft, rerender),
    trialField(draft),
    tagsField(draft),
    subtasksField(draft, rerender),
    repeatField(draft, rerender),

    h('div.field',
      h('label.field-label', { for: 'task-notes' }, 'Notes'),
      h('textarea.textarea#task-notes', {
        value: draft.notes || '',
        placeholder: 'Context, links, who you spoke to…',
        oninput: (event) => { draft.notes = event.target.value; }
      }))
  ];
}

function scopeField(draft, rerender) {
  return h('div.field',
    h('span.field-label', 'Plan it as'),
    h('div.seg', SCOPE_OPTIONS.map((option) => h('button', {
      type: 'button',
      class: draft.scope === option.id ? 'on' : '',
      title: option.hint,
      onclick: () => {
        draft.scope = option.id;
        if (option.id === 'day' && !draft.date) draft.date = todayKey();
        if (option.id === 'week') draft.week = startOfWeek(draft.date || todayKey(), store.settings.weekStart);
        if (option.id === 'month') draft.month = monthKey(draft.date || todayKey());
        rerender();
      }
    }, option.label))));
}

function schedulingField(draft, rerender) {
  if (draft.scope === 'week') {
    const week = draft.week || startOfWeek(todayKey(), store.settings.weekStart);
    return h('div.field',
      h('label.field-label', { for: 'task-week' }, 'Week beginning'),
      h('input.input#task-week', {
        type: 'date',
        value: week,
        onchange: (event) => {
          draft.week = startOfWeek(event.target.value || todayKey(), store.settings.weekStart);
          rerender();
        }
      }),
      h('span.field-hint', formatWeekRange(week, store.settings.weekStart)));
  }

  if (draft.scope === 'month') {
    const month = draft.month || monthKey(todayKey());
    return h('div.field',
      h('label.field-label', { for: 'task-month' }, 'Month'),
      h('input.input#task-month', {
        type: 'month',
        value: month,
        onchange: (event) => { draft.month = event.target.value || month; rerender(); }
      }),
      h('span.field-hint', formatMonth(month)));
  }

  return h('div.row.wrap', { style: { alignItems: 'flex-end', gap: '10px' } },
    h('div.field.grow',
      h('label.field-label', { for: 'task-date' }, 'Date'),
      h('input.input#task-date', {
        type: 'date',
        value: draft.date || todayKey(),
        onchange: (event) => { draft.date = event.target.value || todayKey(); rerender(); }
      }),
      h('span.field-hint', formatLong(draft.date || todayKey()))),
    h('div.field', { style: { width: '116px' } },
      h('label.field-label', { for: 'task-time' }, 'Start'),
      h('input.input#task-time', {
        type: 'time',
        value: draft.time || '',
        onchange: (event) => { draft.time = event.target.value || null; }
      })),
    h('div.field', { style: { width: '104px' } },
      h('label.field-label', { for: 'task-est' }, 'Minutes'),
      h('input.input#task-est', {
        type: 'number', min: '0', step: '5',
        value: draft.estimate ?? '',
        placeholder: '—',
        oninput: (event) => { draft.estimate = event.target.value ? Number(event.target.value) : null; }
      })));
}

function urgencyField(draft, rerender) {
  return h('div.row.wrap', { style: { alignItems: 'flex-end', gap: '14px' } },
    h('div.field.grow',
      h('span.field-label', 'Urgency'),
      h('div.seg', URGENCIES.map((urgency) => h('button', {
        type: 'button',
        class: draft.urgency === urgency.id ? 'on' : '',
        style: draft.urgency === urgency.id ? { color: `var(--u-${urgency.id})` } : {},
        onclick: () => { draft.urgency = urgency.id; rerender(); }
      }, urgency.label)))),
    h('label.row', { style: { gap: '8px', paddingBottom: '6px', cursor: 'pointer' } },
      h('span.switch',
        h('input', {
          type: 'checkbox',
          checked: Boolean(draft.important),
          onchange: (event) => { draft.important = event.target.checked; }
        }),
        h('span')),
      h('span.small', 'Important')));
}

function trialField(draft) {
  const trials = store.trials.filter((t) => t.active || t.id === draft.trialId);
  return h('div.field',
    h('label.field-label', { for: 'task-trial' }, 'Trial'),
    h('select.select#task-trial', {
      onchange: (event) => { draft.trialId = event.target.value || null; }
    },
    h('option', { value: '', selected: !draft.trialId }, 'No trial / general'),
    trials.map((trial) => h('option', {
      value: trial.id,
      selected: draft.trialId === trial.id
    }, `${trial.code} — ${trial.name}`))));
}

function tagsField(draft) {
  const chips = h('div.row.wrap', { style: { gap: '5px' } });

  const paint = () => {
    clear(chips);
    for (const tag of draft.tags) {
      chips.appendChild(h('span.chip.chip-btn', {
        onclick: () => { draft.tags = draft.tags.filter((t) => t !== tag); paint(); }
      }, `#${tag}`, icon('close', { size: 10 })));
    }
    if (!draft.tags.length) chips.appendChild(h('span.field-hint', 'No tags yet'));
  };
  paint();

  return h('div.field',
    h('label.field-label', { for: 'task-tags' }, 'Tags'),
    chips,
    h('input.input#task-tags', {
      placeholder: 'Type a tag and press Enter',
      onkeydown: (event) => {
        if (event.key !== 'Enter') return;
        event.preventDefault();
        const value = event.target.value.trim().replace(/^#/, '').toLowerCase();
        if (value && !draft.tags.includes(value)) draft.tags.push(value);
        event.target.value = '';
        paint();
      }
    }));
}

function subtasksField(draft, rerender) {
  const rows = draft.subtasks.map((subtask) => h('div.row', { style: { gap: '8px' } },
    h('input', {
      type: 'checkbox',
      checked: subtask.done,
      onchange: (event) => { subtask.done = event.target.checked; }
    }),
    h('input.input.grow', {
      value: subtask.title,
      oninput: (event) => { subtask.title = event.target.value; }
    }),
    h('button.btn.btn-icon.btn-ghost', {
      type: 'button', 'aria-label': 'Remove step',
      onclick: () => {
        draft.subtasks = draft.subtasks.filter((s) => s.id !== subtask.id);
        rerender();
      }
    }, icon('close', { size: 13 }))));

  return h('div.field',
    h('span.field-label', `Checklist${draft.subtasks.length ? ` (${draft.subtasks.filter((s) => s.done).length}/${draft.subtasks.length})` : ''}`),
    rows,
    h('button.btn.btn-sm', {
      type: 'button',
      style: { alignSelf: 'flex-start' },
      onclick: () => { draft.subtasks.push({ id: uid('s'), title: '', done: false }); rerender(); }
    }, icon('plus', { size: 12 }), 'Add step'));
}

function repeatField(draft, rerender) {
  const rule = draft.recur;

  const controls = [];
  if (rule) {
    controls.push(h('div.row.wrap', { style: { gap: '8px' } },
      h('select.select', {
        style: { width: '150px' },
        onchange: (event) => {
          draft.recur = { ...rule, freq: event.target.value };
          rerender();
        }
      }, FREQUENCIES.map((freq) => h('option', {
        value: freq.id, selected: rule.freq === freq.id
      }, freq.label))),
      h('span.small.muted', 'every'),
      h('input.input', {
        type: 'number', min: '1', max: '99',
        style: { width: '70px' },
        value: rule.interval || 1,
        oninput: (event) => { draft.recur = { ...rule, interval: Number(event.target.value) || 1 }; }
      })));

    if (rule.freq === 'weekly') {
      controls.push(h('div.row.wrap', { style: { gap: '5px' } },
        DAY_SHORT.map((name, index) => h('button.chip.chip-btn', {
          type: 'button',
          class: (rule.byday || []).includes(index) ? 'on' : '',
          onclick: () => {
            const byday = new Set(rule.byday || []);
            if (byday.has(index)) byday.delete(index); else byday.add(index);
            draft.recur = { ...rule, byday: [...byday].sort() };
            rerender();
          }
        }, name))));
    }

    controls.push(h('div.row.wrap', { style: { gap: '8px', alignItems: 'flex-end' } },
      h('div.field',
        h('span.field-label', 'Until (optional)'),
        h('input.input', {
          type: 'date',
          value: rule.until || '',
          onchange: (event) => { draft.recur = { ...rule, until: event.target.value || null }; rerender(); }
        }))));

    controls.push(h('span.field-hint', describeRule(rule, draft.date || draft.anchor)));
  }

  return h('div.field',
    h('div.row',
      h('span.field-label.grow', 'Repeat'),
      h('label.row', { style: { gap: '8px', cursor: 'pointer' } },
        h('span.switch',
          h('input', {
            type: 'checkbox',
            checked: Boolean(rule),
            onchange: (event) => {
              draft.recur = event.target.checked ? { freq: 'weekly', interval: 1, byday: [] } : null;
              rerender();
            }
          }),
          h('span')))),
    controls.length ? controls : h('span.field-hint', 'One-off task'));
}

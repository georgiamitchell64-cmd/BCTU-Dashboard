/* ============================================================================
 * trials.js — trial management
 * ----------------------------------------------------------------------------
 * Add as many trials as you end up running. Each gets a code, a colour and a
 * share of the workload, and every task can be tagged to one of them.
 * ========================================================================== */

import { clear, confirmDialog, h, icon, overlay, toast } from '../ui/dom.js';
import { taskRow } from '../ui/task.js';
import { openTaskEditor } from '../ui/editor.js';
import { store } from '../state/store.js';
import { ui } from '../state/ui.js';
import { rerender } from '../bus.js';
import { TRIAL_COLOURS } from '../state/model.js';
import { todayKey } from '../core/dates.js';
import { doneLast, overdueTasks, progress, sortTasks } from '../core/select.js';

export function renderTrials() {
  const today = todayKey();
  const trials = store.trials;

  return h('div.view-inner',
    h('div.page-head',
      h('div',
        h('h1.page-title', 'Trials'),
        h('div.page-sub', trials.length === 1
          ? 'One trial so far — add another whenever you pick one up'
          : `${trials.length} trials, ${trials.filter((t) => t.active).length} active`)),
      h('button.btn.btn-primary', { type: 'button', onclick: () => trialDialog(null) },
        icon('plus', { size: 13 }), 'Add trial')),

    h('div.trial-grid',
      trials.map((trial) => trialCard(trial, today)),
      untaggedCard()),

    trials.length ? h('div', { style: { marginTop: '24px' } },
      h('h2.card-title', { style: { marginBottom: '10px' } }, 'Open work by trial'),
      h('div.col', trials.filter((t) => t.active).map((trial) => trialTaskList(trial)))) : null);
}

/* ── Cards ────────────────────────────────────────────────────────────────── */

function trialCard(trial, today) {
  const tasks = store.tasks.filter((t) => t.trialId === trial.id);
  const stats = progress(tasks);
  const late = overdueTasks(tasks, today).length;
  const isDefault = store.settings.defaultTrial === trial.id;

  return h('div.card.trial-card', { style: { '--tc': trial.colour, opacity: trial.active ? 1 : 0.62 } },
    h('div.row',
      h('div.grow',
        h('div.row', { style: { gap: '7px' } },
          h('span.trial-code', trial.code),
          isDefault ? h('span.chip', { title: 'New tasks get this trial by default' }, 'Default') : null,
          trial.active ? null : h('span.chip', 'Archived')),
        h('div.trial-name', trial.name)),
      h('div.row', { style: { gap: '2px' } },
        h('button.btn.btn-icon.btn-ghost', {
          type: 'button', title: 'Edit trial', 'aria-label': `Edit ${trial.code}`,
          onclick: () => trialDialog(trial)
        }, icon('edit', { size: 14 })),
        h('button.btn.btn-icon.btn-ghost', {
          type: 'button', title: 'Delete trial', 'aria-label': `Delete ${trial.code}`,
          onclick: async () => {
            const ok = await confirmDialog({
              title: `Delete ${trial.code}?`,
              message: tasks.length
                ? `${tasks.length} task${tasks.length === 1 ? '' : 's'} will stay, but will no longer be linked to a trial.`
                : 'This trial has no tasks.'
            });
            if (!ok) return;
            store.removeTrial(trial.id);
            toast(`${trial.code} deleted`, { kind: 'undo', action: { label: 'Undo', onClick: () => store.undo() } });
          }
        }, icon('trash', { size: 14 })))),

    h('div.trial-stats',
      stat(String(stats.remaining), 'Open'),
      stat(String(stats.done), 'Done'),
      stat(late ? String(late) : '—', 'Overdue', late ? 'var(--u-critical)' : null)),

    h('div.bar', h('i', { style: { width: `${stats.pct}%` } })),
    h('div.row', { style: { marginTop: '10px' } },
      h('span.tiny.faint.grow', `${stats.pct}% complete`),
      h('button.btn.btn-sm.btn-ghost', {
        type: 'button',
        onclick: () => { ui.filter.trialId = trial.id; rerender(); }
      }, 'Filter to this'),
      h('button.btn.btn-sm', {
        type: 'button',
        onclick: () => openTaskEditor(null, { trialId: trial.id })
      }, icon('plus', { size: 11 }), 'Task')));
}

function untaggedCard() {
  const tasks = store.tasks.filter((t) => !t.trialId);
  if (!tasks.length) return null;
  const stats = progress(tasks);

  return h('div.card.trial-card', { style: { '--tc': 'var(--u-low)' } },
    h('div.trial-code', 'General'),
    h('div.trial-name', 'Tasks not linked to any trial'),
    h('div.trial-stats', stat(String(stats.remaining), 'Open'), stat(String(stats.done), 'Done')),
    h('div.bar', h('i', { style: { width: `${stats.pct}%` } })),
    h('div.row', { style: { marginTop: '10px' } },
      h('span.grow'),
      h('button.btn.btn-sm.btn-ghost', {
        type: 'button',
        onclick: () => { ui.filter.trialId = 'none'; rerender(); }
      }, 'Filter to this')));
}

const stat = (value, label, colour = null) => h('div',
  h('div.stat-value', { style: colour ? { color: colour } : {} }, value),
  h('div.stat-label', label));

function trialTaskList(trial) {
  const tasks = doneLast(sortTasks(
    store.tasks.filter((t) => t.trialId === trial.id && !t.done), 'urgency', store.trials
  )).slice(0, 6);
  if (!tasks.length) return null;

  return h('div.card',
    h('div.card-head',
      h('i.chip-dot', { style: { background: trial.colour } }),
      h('div.card-title.grow', trial.code),
      h('span.tiny.faint', `${tasks.length} shown`)),
    h('div', { style: { padding: '10px' } },
      h('div.list-group', tasks.map((task) => taskRow(task, { showDate: true, draggable: false })))));
}

/* ── Add / edit dialog ────────────────────────────────────────────────────── */

function trialDialog(trial) {
  const isNew = !trial;
  const used = new Set(store.trials.map((t) => t.colour));
  const draft = trial
    ? { ...trial }
    : {
      code: '',
      name: '',
      colour: TRIAL_COLOURS.find((c) => !used.has(c)) || TRIAL_COLOURS[0],
      active: true,
      notes: ''
    };

  overlay((close) => {
    const swatches = h('div.swatches');
    const paintSwatches = () => {
      clear(swatches);
      for (const colour of TRIAL_COLOURS) {
        swatches.appendChild(h('button.swatch', {
          type: 'button',
          class: draft.colour === colour ? 'on' : '',
          style: { background: colour },
          'aria-label': `Use colour ${colour}`,
          onclick: () => { draft.colour = colour; paintSwatches(); }
        }));
      }
    };
    paintSwatches();

    const save = () => {
      const code = draft.code.trim();
      if (!code) { toast('A short code is required, e.g. TONIC', { kind: 'error' }); return; }
      const clash = store.trials.some((t) => t.id !== trial?.id && t.code.toLowerCase() === code.toLowerCase());
      if (clash) { toast(`There is already a trial called ${code.toUpperCase()}`, { kind: 'error' }); return; }

      if (isNew) store.addTrial(draft);
      else store.updateTrial(trial.id, draft);
      toast(isNew ? `${code.toUpperCase()} added` : 'Trial updated', { kind: 'success' });
      close();
    };

    return h('div.dialog',
      h('div.modal-head', h('h3.card-title.grow', isNew ? 'Add a trial' : `Edit ${trial.code}`)),
      h('div.modal-body',
        h('div.row.wrap', { style: { alignItems: 'flex-start', gap: '12px' } },
          h('div.field', { style: { width: '150px' } },
            h('label.field-label', { for: 'trial-code' }, 'Short code'),
            h('input.input#trial-code', {
              value: draft.code,
              placeholder: 'TONIC',
              maxLength: 16,
              'data-autofocus': '',
              oninput: (event) => { draft.code = event.target.value; }
            })),
          h('div.field.grow',
            h('label.field-label', { for: 'trial-name' }, 'Full name'),
            h('input.input#trial-name', {
              value: draft.name,
              placeholder: 'TONIC — Early Parenteral Nutrition vs Standard Care',
              oninput: (event) => { draft.name = event.target.value; }
            }))),

        h('div.field', h('span.field-label', 'Colour'), swatches),

        h('div.field',
          h('label.field-label', { for: 'trial-notes' }, 'Notes'),
          h('textarea.textarea#trial-notes', {
            value: draft.notes || '',
            placeholder: 'Sponsor, ISRCTN, key contacts…',
            oninput: (event) => { draft.notes = event.target.value; }
          })),

        h('div.setting-row',
          h('div', h('div.label', 'Active'), h('div.desc', 'Archived trials stay in your history but drop out of the pickers')),
          h('div.control', h('label.switch',
            h('input', {
              type: 'checkbox',
              checked: draft.active !== false,
              onchange: (event) => { draft.active = event.target.checked; }
            }),
            h('span')))),

        h('div.setting-row',
          h('div', h('div.label', 'Default for new tasks'), h('div.desc', 'New tasks start tagged to this trial')),
          h('div.control', h('label.switch',
            h('input', {
              type: 'checkbox',
              checked: store.settings.defaultTrial === trial?.id,
              onchange: (event) => {
                if (!trial) { toast('Save the trial first, then set it as default'); event.target.checked = false; return; }
                store.setSetting('defaultTrial', event.target.checked ? trial.id : null);
              }
            }),
            h('span'))))),

      h('div.modal-foot',
        h('span.grow'),
        h('button.btn', { type: 'button', onclick: close }, 'Cancel'),
        h('button.btn.btn-primary', { type: 'button', onclick: save }, isNew ? 'Add trial' : 'Save')));
  }, { center: true });
}

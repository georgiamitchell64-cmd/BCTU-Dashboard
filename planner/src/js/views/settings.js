/* ============================================================================
 * settings.js — everything that can be customised
 * ========================================================================== */

import { confirmDialog, h, icon, toast } from '../ui/dom.js';
import { store } from '../state/store.js';
import { rerender } from '../bus.js';
import { ACCENTS, applyTheme } from '../ui/theme.js';
import { SORTS } from '../core/select.js';
import { URGENCIES } from '../core/parse.js';
import { exportCSV, exportJSON, importFromFile, resetEverything } from '../data-io.js';
import { SHORTCUTS } from '../shortcuts.js';

const VIEWS = [
  { id: 'today', label: 'Today' },
  { id: 'week', label: 'Week' },
  { id: 'month', label: 'Month' },
  { id: 'matrix', label: 'Priority matrix' },
  { id: 'trials', label: 'Trials' },
  { id: 'insights', label: 'Insights' }
];

const set = (key, value) => {
  store.setSetting(key, value);
  applyTheme();
  rerender();
};

export function renderSettings() {
  const settings = store.settings;
  const profile = store.profile;

  return h('div.view-inner',
    h('div.page-head',
      h('div',
        h('h1.page-title', 'Settings'),
        h('div.page-sub', 'Make it yours — nothing here leaves your machine'))),

    h('div.settings-grid',
      profileCard(profile),
      appearanceCard(settings),
      planningCard(settings),
      defaultsCard(settings),
      focusCard(settings),
      dataCard(),
      shortcutsCard(),
      aboutCard()));
}

/* ── Cards ────────────────────────────────────────────────────────────────── */

function profileCard(profile) {
  return card('You', 'settings', [
    h('div.field',
      h('label.field-label', { for: 'set-name' }, 'Your name'),
      h('input.input#set-name', {
        value: profile.name || '',
        placeholder: 'Georgia Mitchell',
        'data-keep': 'set-name',
        oninput: (event) => { store.setProfile({ name: event.target.value }); }
      }),
      h('span.field-hint', 'Shown in the sidebar and in the greeting on the Today view')),

    h('div.field',
      h('label.field-label', { for: 'set-role' }, 'Role'),
      h('input.input#set-role', {
        value: profile.role || '',
        placeholder: 'Trial Manager',
        'data-keep': 'set-role',
        oninput: (event) => { store.setProfile({ role: event.target.value }); }
      })),

    toggleRow('Greet me by name', 'Say good morning on the Today view', profile.showGreeting !== false,
      (checked) => { store.setProfile({ showGreeting: checked }); rerender(); })
  ]);
}

function appearanceCard(settings) {
  const themes = [
    { id: 'light', label: 'Light', bg: '#EEF3F8', side: '#1B4F6B' },
    { id: 'dark', label: 'Dark', bg: '#1A2438', side: '#101B2E' },
    { id: 'system', label: 'System', bg: 'linear-gradient(90deg,#EEF3F8 50%,#1A2438 50%)', side: '#1B4F6B' }
  ];

  return card('Appearance', 'sun', [
    h('div.field',
      h('span.field-label', 'Theme'),
      h('div.theme-cards', themes.map((theme) => h('button.theme-card', {
        type: 'button',
        class: settings.theme === theme.id ? 'on' : '',
        onclick: () => set('theme', theme.id)
      },
      h('div.preview',
        h('i', { style: { background: theme.side } }),
        h('i', { style: { background: theme.bg } })),
      h('div.name', theme.label))))),

    h('div.field',
      h('span.field-label', 'Accent colour'),
      h('div.swatches',
        ACCENTS.map((accent) => h('button.swatch', {
          type: 'button',
          class: settings.accent === accent.id ? 'on' : '',
          style: { background: accent.id },
          title: accent.name,
          'aria-label': accent.name,
          onclick: () => set('accent', accent.id)
        })),
        h('label.swatch', {
          style: { background: 'conic-gradient(red,orange,yellow,lime,cyan,blue,magenta,red)', display: 'grid', placeItems: 'center', cursor: 'pointer' },
          title: 'Custom colour'
        },
        h('input', {
          type: 'color',
          value: settings.accent,
          style: { opacity: 0, width: '100%', height: '100%', cursor: 'pointer' },
          oninput: (event) => set('accent', event.target.value)
        })))),

    selectRow('Density', 'How much breathing room the lists get', settings.density,
      [{ value: 'comfortable', label: 'Comfortable' }, { value: 'compact', label: 'Compact' }],
      (value) => set('density', value)),

    selectRow('Opening view', 'Where the app starts', settings.startView,
      VIEWS.map((view) => ({ value: view.id, label: view.label })),
      (value) => set('startView', value)),

    h('div.field',
      h('span.field-label', 'Hide views you do not use'),
      h('div.row.wrap', { style: { gap: '5px' } },
        VIEWS.filter((view) => view.id !== 'today').map((view) => {
          const hidden = settings.hiddenViews.includes(view.id);
          return h('button.chip.chip-btn', {
            type: 'button',
            class: hidden ? '' : 'on',
            onclick: () => {
              const next = hidden
                ? settings.hiddenViews.filter((id) => id !== view.id)
                : [...settings.hiddenViews, view.id];
              set('hiddenViews', next);
            }
          }, view.label);
        })))
  ]);
}

function planningCard(settings) {
  return card('Planning', 'week', [
    selectRow('Week starts on', 'Affects the week board and the calendar', String(settings.weekStart),
      [{ value: '1', label: 'Monday' }, { value: '0', label: 'Sunday' }],
      (value) => set('weekStart', Number(value))),

    toggleRow('Show weekends', 'Turn off for a five-day week board', settings.showWeekends,
      (checked) => set('showWeekends', checked)),

    toggleRow('24-hour clock', '09:30 rather than 9.30am', settings.use24h,
      (checked) => set('use24h', checked)),

    toggleRow('Carry overdue tasks forward', 'Offer to move yesterday’s leftovers onto today when you open the app',
      settings.carryOverdue, (checked) => set('carryOverdue', checked)),

    h('div.row.wrap', { style: { gap: '10px' } },
      h('div.field.grow',
        h('label.field-label', { for: 'set-daystart' }, 'Day starts'),
        h('input.input#set-daystart', {
          type: 'time', value: settings.dayStart,
          onchange: (event) => set('dayStart', event.target.value || '08:00')
        })),
      h('div.field.grow',
        h('label.field-label', { for: 'set-dayend' }, 'Day ends'),
        h('input.input#set-dayend', {
          type: 'time', value: settings.dayEnd,
          onchange: (event) => set('dayEnd', event.target.value || '18:00')
        })))
  ]);
}

function defaultsCard(settings) {
  const trials = store.trials.filter((t) => t.active);

  return card('New task defaults', 'plus', [
    selectRow('Default urgency', 'Used when you do not say otherwise', settings.defaultUrgency,
      URGENCIES.map((urgency) => ({ value: urgency.id, label: urgency.label })),
      (value) => set('defaultUrgency', value)),

    selectRow('Default trial', 'Handy when most of your work is one trial', settings.defaultTrial || '',
      [{ value: '', label: 'No trial' }, ...trials.map((trial) => ({ value: trial.id, label: trial.code }))],
      (value) => set('defaultTrial', value || null)),

    selectRow('Default sort', 'How lists are ordered to begin with', settings.defaultSort || 'manual',
      SORTS.map((sort) => ({ value: sort.id, label: sort.label })),
      (value) => set('defaultSort', value)),

    toggleRow('Celebrate a finished day', 'A small burst of confetti when the last task is ticked off',
      settings.celebrate, (checked) => set('celebrate', checked))
  ]);
}

function focusCard(settings) {
  return card('Focus and reminders', 'target', [
    h('div.row.wrap', { style: { gap: '10px' } },
      h('div.field.grow',
        h('label.field-label', { for: 'set-focus' }, 'Focus session (min)'),
        h('input.input#set-focus', {
          type: 'number', min: '5', max: '120', step: '5', value: settings.focusMinutes,
          onchange: (event) => set('focusMinutes', Number(event.target.value) || 25)
        })),
      h('div.field.grow',
        h('label.field-label', { for: 'set-break' }, 'Break (min)'),
        h('input.input#set-break', {
          type: 'number', min: '1', max: '60', value: settings.breakMinutes,
          onchange: (event) => set('breakMinutes', Number(event.target.value) || 5)
        }))),

    toggleRow('Desktop reminders', 'Notify shortly before a timed task starts', settings.notifications,
      (checked) => {
        set('notifications', checked);
        if (checked && typeof Notification !== 'undefined' && Notification.permission === 'default') {
          Notification.requestPermission();
        }
      }),

    h('div.field',
      h('label.field-label', { for: 'set-lead' }, 'Remind me this many minutes ahead'),
      h('input.input#set-lead', {
        type: 'number', min: '0', max: '180', step: '5', value: settings.reminderLead,
        onchange: (event) => set('reminderLead', Number(event.target.value) || 0)
      }))
  ]);
}

function dataCard() {
  const counts = {
    tasks: store.tasks.length,
    trials: store.trials.length,
    notes: Object.keys(store.state.notes).length
  };

  return card('Your data', 'folder', [
    h('p.small.muted',
      `${counts.tasks} tasks, ${counts.trials} trials and ${counts.notes} day notes, stored as plain JSON on this computer.`),

    h('div.row.wrap', { style: { gap: '8px' } },
      h('button.btn', { type: 'button', onclick: exportJSON }, icon('download', { size: 13 }), 'Export JSON'),
      h('button.btn', { type: 'button', onclick: exportCSV }, icon('download', { size: 13 }), 'Export CSV'),
      h('button.btn', { type: 'button', onclick: importFromFile }, icon('upload', { size: 13 }), 'Import'),
      globalThis.planner?.revealDataFolder
        ? h('button.btn', {
          type: 'button',
          onclick: () => globalThis.planner.revealDataFolder()
        }, icon('folder', { size: 13 }), 'Open folder')
        : null),

    h('div.row.wrap', { style: { gap: '8px', marginTop: '4px' } },
      h('button.btn.btn-sm.btn-ghost', {
        type: 'button',
        onclick: async () => {
          const ok = await confirmDialog({
            title: 'Clear completed tasks?',
            message: 'Every finished task is removed. Repeating tasks and their history stay.',
            confirmLabel: 'Clear'
          });
          if (!ok) return;
          const removed = store.clearCompleted();
          toast(`${removed} completed task${removed === 1 ? '' : 's'} cleared`, {
            kind: 'undo', action: { label: 'Undo', onClick: () => store.undo() }
          });
          rerender();
        }
      }, 'Clear completed'),
      h('button.btn.btn-sm.btn-danger', { type: 'button', onclick: resetEverything }, 'Reset everything'))
  ]);
}

function shortcutsCard() {
  return card('Keyboard shortcuts', 'keyboard', [
    h('table.shortcut-table',
      h('tbody', SHORTCUTS.map((shortcut) => h('tr',
        h('td', shortcut.description),
        h('td', h('span.kbd', shortcut.keys))))))
  ]);
}

function aboutCard() {
  return card('About', 'inbox', [
    h('p.small.muted',
      'Built as a companion to the BCTU clinical trials dashboard. Everything runs locally: ',
      'no accounts, no sync, no network calls.'),
    h('p.tiny.faint', { style: { marginTop: '8px' } },
      globalThis.planner?.isDesktop
        ? 'Running as a desktop app.'
        : 'Running in a browser — data is kept in this browser’s local storage.')
  ]);
}

/* ── Little builders ──────────────────────────────────────────────────────── */

function card(title, iconName, children) {
  return h('div.card',
    h('div.card-head', icon(iconName, { size: 15 }), h('div.card-title.grow', title)),
    h('div', { style: { padding: 'var(--card-pad)', display: 'flex', flexDirection: 'column', gap: '14px' } },
      children));
}

function toggleRow(label, description, checked, onChange) {
  return h('div.setting-row',
    h('div', h('div.label', label), description ? h('div.desc', description) : null),
    h('div.control',
      h('label.switch',
        h('input', { type: 'checkbox', checked, onchange: (event) => onChange(event.target.checked) }),
        h('span'))));
}

function selectRow(label, description, value, options, onChange) {
  return h('div.setting-row',
    h('div', h('div.label', label), description ? h('div.desc', description) : null),
    h('div.control',
      h('select.select', { onchange: (event) => onChange(event.target.value) },
        options.map((option) => h('option', {
          value: option.value, selected: String(option.value) === String(value)
        }, option.label)))));
}

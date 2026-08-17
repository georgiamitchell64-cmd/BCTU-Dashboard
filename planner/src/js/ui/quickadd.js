/* ============================================================================
 * quickadd.js — the one-line capture bar
 * ----------------------------------------------------------------------------
 * Shows, live, what it understood from what you typed. Nothing is hidden:
 * if the parser read "friday" as a date, a chip says so before you hit Enter.
 * ========================================================================== */

import { clear, h, icon, toast } from './dom.js';
import { store } from '../state/store.js';
import { parseQuickAdd, TOKEN_HELP, URGENCIES } from '../core/parse.js';
import { formatDuration, formatTime, relativeLabel, todayKey } from '../core/dates.js';
import { describeRule } from '../core/recur.js';
import { openTaskEditor } from './editor.js';

/**
 * @param {{ defaults?: object, placeholder?: string, onAdd?: Function }} [options]
 *        `defaults` seeds date/scope/trial for the view it sits in.
 */
export function quickAdd(options = {}) {
  const {
    defaults = {},
    placeholder = 'Add a task — try "chase site 12 friday 9:30 !1 @tonic #monitoring ~45m"',
    onAdd = null
  } = options;

  const preview = h('div.qa-preview');
  const help = h('div.qa-help.hidden', TOKEN_HELP.map((row) =>
    h('div.row', { style: { gap: '8px' } }, h('code', row.token), h('span.muted', row.meaning))));

  const input = h('input', {
    type: 'text',
    placeholder,
    'aria-label': 'Add a task',
    'data-keep': 'quickadd',
    autocomplete: 'off',
    spellcheck: 'true',
    oninput: () => paint(),
    onkeydown: (event) => {
      if (event.key === 'Enter') { event.preventDefault(); submit(); }
      if (event.key === 'Escape') { input.value = ''; paint(); input.blur(); }
    }
  });

  const parse = () => parseQuickAdd(input.value, {
    today: todayKey(),
    weekStart: store.settings.weekStart,
    trialCodes: store.trials.map((t) => t.code)
  });

  function paint() {
    clear(preview);
    if (!input.value.trim()) return;

    const parsed = parse();
    const chips = [];
    if (parsed.date) chips.push(chip('today', relativeLabel(parsed.date), parsed.date));
    if (parsed.scope) chips.push(chip('layers', parsed.scope === 'week' ? 'This week' : 'This month'));
    if (parsed.time) chips.push(chip('clock', formatTime(parsed.time, store.settings.use24h)));
    if (parsed.urgency) {
      const urgency = URGENCIES.find((u) => u.id === parsed.urgency);
      chips.push(h('span.chip.chip-urgency', { class: `u-${parsed.urgency}` }, urgency.label));
    }
    if (parsed.trial) {
      const trial = store.trialByCode(parsed.trial);
      if (trial) {
        chips.push(h('span.chip', {
          style: { color: trial.colour, background: `color-mix(in srgb, ${trial.colour} 14%, transparent)` }
        }, trial.code));
      }
    }
    if (parsed.estimate) chips.push(chip('clock', formatDuration(parsed.estimate)));
    if (parsed.important) chips.push(chip('star', 'Important'));
    if (parsed.recur) chips.push(chip('repeat', describeRule(parsed.recur, parsed.date || todayKey())));
    for (const tag of parsed.tags) chips.push(h('span.chip', `#${tag}`));

    if (chips.length) {
      preview.append(h('span.faint.tiny', { style: { alignSelf: 'center' } }, 'Reading:'), ...chips);
    }
  }

  function submit() {
    const raw = input.value.trim();
    if (!raw) return;
    const parsed = parse();
    if (!parsed.title) {
      toast('That is all shorthand — add some words too', { kind: 'error' });
      return;
    }

    const trial = parsed.trial ? store.trialByCode(parsed.trial) : null;
    const task = store.addTask({
      ...defaults,
      title: parsed.title,
      scope: parsed.scope || defaults.scope || 'day',
      date: parsed.date || defaults.date,
      week: defaults.week,
      month: defaults.month,
      time: parsed.time,
      estimate: parsed.estimate,
      urgency: parsed.urgency || defaults.urgency,
      important: parsed.important || Boolean(defaults.important),
      trialId: trial ? trial.id : (defaults.trialId ?? store.settings.defaultTrial ?? null),
      tags: [...new Set([...(defaults.tags || []), ...parsed.tags])],
      recur: parsed.recur,
      anchor: parsed.date || defaults.date
    });

    input.value = '';
    paint();
    onAdd?.(task);
  }

  const bar = h('div.quickadd',
    icon('plus', { size: 15, className: 'faint' }),
    input,
    h('button.btn.btn-icon.btn-ghost.hint-toggle', {
      type: 'button', title: 'What can I type here?', 'aria-label': 'Quick add syntax help',
      onclick: () => help.classList.toggle('hidden')
    }, icon('keyboard', { size: 15 })),
    h('button.btn.btn-icon.btn-ghost', {
      type: 'button', title: 'Open the full editor', 'aria-label': 'Open the full editor',
      onclick: () => {
        const parsed = parse();
        const trial = parsed.trial ? store.trialByCode(parsed.trial) : null;
        openTaskEditor(null, {
          ...defaults,
          title: parsed.title,
          date: parsed.date || defaults.date || todayKey(),
          time: parsed.time,
          estimate: parsed.estimate,
          urgency: parsed.urgency || defaults.urgency || store.settings.defaultUrgency,
          important: parsed.important,
          tags: parsed.tags,
          recur: parsed.recur,
          trialId: trial ? trial.id : (defaults.trialId ?? store.settings.defaultTrial ?? null)
        });
        input.value = '';
        paint();
      }
    }, icon('edit', { size: 15 })),
    h('button.btn.btn-primary', { type: 'button', onclick: submit }, 'Add'));

  return h('div', { style: { marginBottom: 'var(--gap)' } }, bar, preview, help);
}

const chip = (name, label, title = '') =>
  h('span.chip', { title }, icon(name, { size: 10 }), label);

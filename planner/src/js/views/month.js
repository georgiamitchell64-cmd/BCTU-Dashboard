/* ============================================================================
 * month.js — the month calendar
 * ----------------------------------------------------------------------------
 * A drag-and-drop calendar grid, with the month's own objectives in a panel
 * beside it. Future repeats show as dashed ghosts so the shape of the month is
 * visible before anything is generated.
 * ========================================================================== */

import { h, icon } from '../ui/dom.js';
import { taskMini, taskRow } from '../ui/task.js';
import { quickAdd } from '../ui/quickadd.js';
import { filterBar } from '../ui/filters.js';
import { makeDropZone, orderFor } from '../ui/dnd.js';
import { openTaskEditor } from '../ui/editor.js';
import { store } from '../state/store.js';
import { ui } from '../state/ui.js';
import { rerender, navigate } from '../bus.js';
import {
  addMonthsToMonthKey, DAY_SHORT, endOfMonth, formatMonth, fromKey, monthGrid,
  monthKey, todayKey
} from '../core/dates.js';
import {
  applyFilter, countByTrial, dayTasks, doneLast, monthTasks, progress,
  projectedOccurrences, sortTasks
} from '../core/select.js';

const MAX_PER_CELL = 4;

export function renderMonth() {
  const settings = store.settings;
  const today = todayKey();
  const month = ui.month;
  const cells = monthGrid(month, settings.weekStart);

  const visible = applyFilter(store.tasks, ui.filter, store.trials);
  const inMonth = store.tasks.filter((t) => t.scope === 'day' && monthKey(t.date) === month);
  const stats = progress([...inMonth, ...monthTasks(store.tasks, month)]);

  const step = (months) => { ui.month = addMonthsToMonthKey(month, months); rerender(); };

  const dayNames = Array.from({ length: 7 }, (_, i) => DAY_SHORT[(i + settings.weekStart) % 7]);

  return h('div.view-inner.view-wide',
    h('div.page-head',
      h('div',
        h('h1.page-title', formatMonth(month)),
        h('div.page-sub', `${stats.done}/${stats.total} done · ${stats.remaining} still open`)),
      h('div.row',
        h('button.btn', { type: 'button', 'aria-label': 'Previous month', onclick: () => step(-1) }, icon('chevronLeft', { size: 14 })),
        h('button.btn', { type: 'button', onclick: () => { ui.month = monthKey(today); rerender(); } }, 'This month'),
        h('button.btn', { type: 'button', 'aria-label': 'Next month', onclick: () => step(1) }, icon('chevronRight', { size: 14 })))),

    quickAdd({
      defaults: { scope: 'day', date: monthKey(today) === month ? today : `${month}-01` },
      placeholder: 'Add to this month — "ethics amendment 15/09 !2 @tonic"',
      onAdd: () => rerender()
    }),

    filterBar({ showSort: false }),

    h('div.month-layout',
      h('div.month-grid',
        dayNames.map((name) => h('div.month-dow', name)),
        cells.map((key) => monthCell(key, month, visible, today))),
      h('div',
        goalsCard(month, visible),
        breakdownCard(month))));
}

/* ── Cell ─────────────────────────────────────────────────────────────────── */

function monthCell(key, month, visible, today) {
  const date = fromKey(key);
  const outside = monthKey(key) !== month;
  const weekend = [0, 6].includes(date.getDay());
  const expanded = ui.expandedDays.has(key);

  const tasks = sortTasks(dayTasks(visible, key), 'time', store.trials);
  const ghosts = projectedOccurrences(store.tasks, key, key)
    .filter(({ task }) => !tasks.some((t) => t.id === task.id));

  const items = [...tasks.map((t) => ({ task: t, ghost: false })),
    ...ghosts.map(({ task }) => ({ task, ghost: true }))];
  const shown = expanded ? items : items.slice(0, MAX_PER_CELL);
  const hidden = items.length - shown.length;

  const cell = h('div.month-cell', {
    class: [outside ? 'out' : '', key === today ? 'today' : '', weekend ? 'weekend' : ''].filter(Boolean).join(' '),
    ondblclick: () => { ui.cursor = key; navigate('today'); }
  },
  h('div.month-date',
    h('span', String(date.getDate())),
    tasks.length ? h('span.tiny.faint', `${tasks.filter((t) => t.done).length}/${tasks.length}`) : null),
  shown.map(({ task, ghost }) => taskMini(task, { ghost })),
  hidden > 0
    ? h('div.month-more', {
      onclick: (event) => { event.stopPropagation(); ui.expandedDays.add(key); rerender(); }
    }, `+${hidden} more`)
    : null,
  expanded && items.length > MAX_PER_CELL
    ? h('div.month-more', {
      onclick: (event) => { event.stopPropagation(); ui.expandedDays.delete(key); rerender(); }
    }, 'show less')
    : null);

  makeDropZone(cell, (taskId, beforeId) => {
    const siblings = dayTasks(store.tasks, key);
    store.moveTask(taskId, { scope: 'day', date: key, order: orderFor(siblings, beforeId, taskId) });
  }, { sortable: false });

  return cell;
}

/* ── Monthly objectives ───────────────────────────────────────────────────── */

function goalsCard(month, visible) {
  const tasks = doneLast(sortTasks(monthTasks(visible, month), ui.filter.sort, store.trials));

  const body = h('div.list-group', { style: { minHeight: '44px' } },
    tasks.length
      ? tasks.map((task) => taskRow(task, { compact: true }))
      : h('div.empty', { style: { padding: '16px' } }, 'No objectives set for this month'));

  makeDropZone(body, (taskId, beforeId) => {
    const siblings = monthTasks(store.tasks, month);
    store.moveTask(taskId, { scope: 'month', month, order: orderFor(siblings, beforeId, taskId) });
  });

  return h('div.card.side-card',
    h('div.card-head',
      icon('target', { size: 15 }),
      h('div.grow',
        h('div.card-title', 'Monthly objectives'),
        h('div.card-sub', `${tasks.filter((t) => !t.done).length} open`)),
      h('button.btn.btn-icon.btn-ghost', {
        type: 'button', 'aria-label': 'Add an objective',
        onclick: () => openTaskEditor(null, { scope: 'month', month })
      }, icon('plus', { size: 14 }))),
    h('div', { style: { padding: '10px' } }, body));
}

/* ── Per-trial breakdown ──────────────────────────────────────────────────── */

function breakdownCard(month) {
  const start = `${month}-01`;
  const end = endOfMonth(start);
  const tasks = store.tasks.filter((task) => {
    if (task.scope === 'month') return task.month === month;
    if (task.scope === 'day') return task.date >= start && task.date <= end;
    return task.week >= start && task.week <= end;
  });

  const rows = countByTrial(tasks, store.trials).filter((row) => row.total);

  return h('div.card.side-card',
    h('div.card-head', icon('trials', { size: 15 }), h('div.card-title.grow', 'Where the month goes')),
    h('div', { style: { padding: 'var(--card-pad)' } },
      rows.length ? rows.map((row) => {
        const pct = row.total ? Math.round((row.done / row.total) * 100) : 0;
        return h('div', { style: { marginBottom: '12px' } },
          h('div.row',
            h('i.chip-dot', { style: { background: row.trial.colour } }),
            h('span.small.grow', { style: { fontWeight: 600 } }, row.trial.code),
            h('span.tiny.faint', `${row.done}/${row.total}`)),
          h('div.bar', { style: { '--tc': row.trial.colour } }, h('i', { style: { width: `${pct}%` } })));
      }) : h('div.empty', { style: { padding: '16px' } }, 'Nothing scheduled this month')));
}

/* ============================================================================
 * week.js — the week board
 * ----------------------------------------------------------------------------
 * Seven columns you can drag tasks between, with a lane above for work that
 * belongs to the week as a whole rather than to any one day.
 * ========================================================================== */

import { h, icon } from '../ui/dom.js';
import { taskRow } from '../ui/task.js';
import { quickAdd } from '../ui/quickadd.js';
import { filterBar } from '../ui/filters.js';
import { makeDropZone, orderFor } from '../ui/dnd.js';
import { openTaskEditor } from '../ui/editor.js';
import { store } from '../state/store.js';
import { ui } from '../state/ui.js';
import { rerender } from '../bus.js';
import {
  addDays, DAY_SHORT, formatDuration, formatWeekRange, fromKey, isoWeek,
  startOfWeek, todayKey, weekDays
} from '../core/dates.js';
import {
  applyFilter, dayTasks, daysInWeekTasks, doneLast, progress, projectedOccurrences,
  sortTasks, weekTasks, workload
} from '../core/select.js';

export function renderWeek() {
  const settings = store.settings;
  const today = todayKey();
  const monday = startOfWeek(ui.cursor, settings.weekStart);
  const days = weekDays(ui.cursor, settings.weekStart)
    .filter((key) => settings.showWeekends || ![0, 6].includes(fromKey(key).getDay()));

  const visible = applyFilter(store.tasks, ui.filter, store.trials);
  const weekStats = progress(daysInWeekTasks(store.tasks, monday, settings.weekStart)
    .concat(weekTasks(store.tasks, monday, settings.weekStart)));
  const load = workload(daysInWeekTasks(store.tasks, monday, settings.weekStart));

  const step = (weeks) => { ui.cursor = addDays(monday, weeks * 7); rerender(); };

  return h('div.view-inner.view-wide',
    h('div.page-head',
      h('div',
        h('h1.page-title', formatWeekRange(monday, settings.weekStart)),
        h('div.page-sub',
          `Week ${isoWeek(monday)} · ${weekStats.done}/${weekStats.total} done`,
          load.minutes ? ` · ${formatDuration(load.minutes)} planned` : '')),
      h('div.row',
        h('button.btn', { type: 'button', 'aria-label': 'Previous week', onclick: () => step(-1) }, icon('chevronLeft', { size: 14 })),
        h('button.btn', { type: 'button', onclick: () => { ui.cursor = today; rerender(); } }, 'This week'),
        h('button.btn', { type: 'button', 'aria-label': 'Next week', onclick: () => step(1) }, icon('chevronRight', { size: 14 })))),

    quickAdd({
      defaults: { scope: 'day', date: monday <= today && today <= addDays(monday, 6) ? today : monday },
      placeholder: 'Add to this week — "site file check thu 10:00 !2 @tonic"',
      onAdd: () => rerender()
    }),

    filterBar(),
    weekLane(monday, visible),

    h('div.week-grid', { style: { '--cols': String(days.length) } },
      days.map((key) => dayColumn(key, visible, today))));
}

/* ── Week-scope lane ──────────────────────────────────────────────────────── */

function weekLane(monday, visible) {
  const settings = store.settings;
  const tasks = doneLast(sortTasks(weekTasks(visible, monday, settings.weekStart), ui.filter.sort, store.trials));

  const body = h('div.lane-body', { style: { minHeight: '42px' } },
    tasks.length
      ? tasks.map((task) => taskRow(task, { compact: true }))
      : h('div.empty', { style: { padding: '14px', gridColumn: '1 / -1' } },
        'Drag anything here that needs doing this week, but not on a particular day'));

  makeDropZone(body, (taskId, beforeId) => {
    const siblings = weekTasks(store.tasks, monday, settings.weekStart);
    store.moveTask(taskId, { scope: 'week', week: monday, order: orderFor(siblings, beforeId, taskId) });
  });

  return h('div.lane',
    h('div.lane-head',
      icon('layers', { size: 15 }),
      h('span.card-title.grow', 'This week — any day'),
      h('span.tiny.faint', `${tasks.filter((t) => !t.done).length} open`),
      h('button.btn.btn-sm', {
        type: 'button',
        onclick: () => openTaskEditor(null, { scope: 'week', week: monday })
      }, icon('plus', { size: 12 }), 'Add')),
    body);
}

/* ── Day column ───────────────────────────────────────────────────────────── */

function dayColumn(key, visible, today) {
  const date = fromKey(key);
  const isToday = key === today;
  const weekend = [0, 6].includes(date.getDay());

  const tasks = doneLast(sortTasks(dayTasks(visible, key), ui.filter.sort === 'manual' ? 'time' : ui.filter.sort, store.trials));
  const ghosts = projectedOccurrences(store.tasks, key, key)
    .filter(({ task }) => !tasks.some((t) => t.id === task.id));
  const load = workload(dayTasks(store.tasks, key));

  const body = h('div.day-body',
    tasks.map((task) => taskRow(task, { compact: true })),
    ghosts.map(({ task, date: ghostDate }) => taskRow(task, { compact: true, ghostDate, draggable: false })),
    h('button.day-add', {
      type: 'button',
      onclick: () => openTaskEditor(null, { scope: 'day', date: key })
    }, '+ add'));

  makeDropZone(body, (taskId, beforeId) => {
    const siblings = dayTasks(store.tasks, key);
    store.moveTask(taskId, { scope: 'day', date: key, order: orderFor(siblings, beforeId, taskId) });
  });

  return h('div.day-col', {
    class: [isToday ? 'today' : '', weekend ? 'weekend' : '', key < today ? 'past' : ''].filter(Boolean).join(' ')
  },
  h('div.day-head', {
    ondblclick: () => { ui.cursor = key; rerender(); }
  },
  h('span.day-num', String(date.getDate())),
  h('span.day-name', DAY_SHORT[date.getDay()]),
  h('span.day-load', load.minutes ? formatDuration(load.minutes) : (load.count ? `${load.count}` : ''))),
  body);
}

/* ============================================================================
 * today.js — the day view
 * ----------------------------------------------------------------------------
 * A single day: what is scheduled, what is loose, what is overdue, plus the
 * week and month commitments alongside so the day is never planned in
 * isolation.
 * ========================================================================== */

import { h, icon, ring, toast } from '../ui/dom.js';
import { taskRow } from '../ui/task.js';
import { quickAdd } from '../ui/quickadd.js';
import { filterBar } from '../ui/filters.js';
import { makeDropZone, orderFor } from '../ui/dnd.js';
import { openTaskEditor } from '../ui/editor.js';
import { store } from '../state/store.js';
import { ui } from '../state/ui.js';
import { rerender, navigate } from '../bus.js';
import {
  addDays, formatDuration, formatLong, greeting, minutesToTime, monthKey,
  relativeLabel, startOfWeek, timeToMinutes, todayKey
} from '../core/dates.js';
import {
  applyFilter, dayTasks, doneLast, monthTasks, overdueTasks, progress,
  projectedOccurrences, sortTasks, streak, weekTasks, workload
} from '../core/select.js';

export function renderToday() {
  const today = todayKey();
  const cursor = ui.cursor;
  const isToday = cursor === today;

  const all = store.tasks;
  const visible = applyFilter(all, ui.filter, store.trials);

  const dayList = doneLast(sortTasks(dayTasks(visible, cursor), ui.filter.sort, store.trials));
  const scheduled = dayList.filter((t) => t.time);
  const loose = dayList.filter((t) => !t.time);
  const overdue = isToday ? sortTasks(overdueTasks(visible, today), 'urgency') : [];
  const ghosts = projectedOccurrences(all, cursor, cursor)
    .filter(({ task }) => !dayList.some((t) => t.id === task.id));

  const stats = progress(dayTasks(all, cursor));
  const load = workload(dayTasks(all, cursor));

  return h('div.view-inner',
    hero({ cursor, isToday, stats, load, today }),
    quickAdd({
      defaults: { scope: 'day', date: cursor },
      onAdd: (task) => { toast(`Added “${task.title}”`, { kind: 'success' }); rerender(); }
    }),
    filterBar(),
    h('div.today-grid',
      h('div',
        overdue.length ? overdueCard(overdue, today) : null,
        dayPanel({ cursor, scheduled, loose, ghosts, stats, isToday })),
      h('div',
        horizonLane('week', cursor),
        horizonLane('month', cursor),
        noteCard(cursor),
        lookaheadCard(cursor, visible))));
}

/* ── Hero ─────────────────────────────────────────────────────────────────── */

function hero({ cursor, isToday, stats, load, today }) {
  const profile = store.profile;
  const firstName = (profile.name || '').trim().split(/\s+/)[0] || 'there';
  const run = streak(store.tasks, today);

  const step = (days) => { ui.cursor = addDays(ui.cursor, days); rerender(); };

  return h('div.hero',
    h('div.hero-row',
      h('div.grow', { style: { minWidth: '220px' } },
        h('div.hero-greet', profile.showGreeting !== false && isToday
          ? `${greeting()}, ${firstName}`
          : formatLong(cursor).split(' ').slice(0, 2).join(' ')),
        h('div.hero-date',
          formatLong(cursor),
          isToday ? null : h('span', { style: { marginLeft: '8px', opacity: 0.8 } }, `· ${relativeLabel(cursor, today)}`)),
        h('div.row', { style: { marginTop: '12px', gap: '6px' } },
          h('button.btn.btn-sm', { type: 'button', 'aria-label': 'Previous day', onclick: () => step(-1) }, icon('chevronLeft', { size: 13 })),
          h('button.btn.btn-sm', { type: 'button', onclick: () => { ui.cursor = today; rerender(); } }, 'Today'),
          h('button.btn.btn-sm', { type: 'button', 'aria-label': 'Next day', onclick: () => step(1) }, icon('chevronRight', { size: 13 })),
          h('input.input', {
            type: 'date',
            value: cursor,
            'aria-label': 'Jump to a date',
            style: { width: '150px', background: 'rgba(255,255,255,.14)', border: '1px solid rgba(255,255,255,.22)', color: '#fff' },
            onchange: (event) => { ui.cursor = event.target.value || today; rerender(); }
          }))),

      h('div.hero-stats',
        ring(stats.pct, { size: 62, stroke: 6, label: `${stats.pct}%` }),
        heroStat(`${stats.done}/${stats.total}`, 'Done'),
        heroStat(load.minutes ? formatDuration(load.minutes) : '—', 'Planned'),
        heroStat(String(run), 'Day streak'))));
}

const heroStat = (value, label) => h('div',
  h('div.hero-stat-value', value),
  h('div.hero-stat-label', label));

/* ── Overdue ──────────────────────────────────────────────────────────────── */

function overdueCard(tasks, today) {
  return h('div.card', { style: { marginBottom: 'var(--gap)', borderColor: 'color-mix(in srgb, var(--u-critical) 34%, var(--border))' } },
    h('div.card-head',
      icon('alert', { size: 15, className: 'ico-lg' }),
      h('div.grow',
        h('div.card-title', `${tasks.length} overdue`),
        h('div.card-sub', 'Left over from earlier days')),
      h('button.btn.btn-sm', {
        type: 'button',
        onclick: () => {
          const moved = store.carryOverdue(today);
          toast(`Moved ${moved} task${moved === 1 ? '' : 's'} to today`, {
            kind: 'success',
            action: { label: 'Undo', onClick: () => store.undo() }
          });
        }
      }, icon('arrowDown', { size: 12 }), 'Move all to today')),
    h('div', { style: { padding: '10px' } },
      h('div.list-group', tasks.map((task) => taskRow(task, { showDate: true })))));
}

/* ── The day itself ───────────────────────────────────────────────────────── */

function dayPanel({ cursor, scheduled, loose, ghosts, stats, isToday }) {
  const panel = h('div.card',
    h('div.card-head',
      icon('today', { size: 15 }),
      h('div.grow',
        h('div.card-title', isToday ? 'Today' : formatLong(cursor)),
        h('div.card-sub', stats.total
          ? `${stats.remaining} to go · ${stats.done} done`
          : 'Nothing planned yet')),
      h('button.btn.btn-sm', {
        type: 'button',
        onclick: () => openTaskEditor(null, { scope: 'day', date: cursor })
      }, icon('plus', { size: 12 }), 'Task')),

    h('div', { style: { padding: 'var(--card-pad)' } },
      scheduled.length ? timeline(scheduled, isToday) : null,
      scheduled.length && (loose.length || ghosts.length) ? h('div', { style: { height: '14px' } }) : null,
      looseList(loose, ghosts, cursor),
      !scheduled.length && !loose.length && !ghosts.length
        ? h('div.empty',
          h('strong', isToday ? 'A clear day' : 'Nothing here yet'),
          'Type in the box above, or press N for the full editor.')
        : null));

  return panel;
}

function timeline(tasks, isToday) {
  const now = new Date();
  const nowMinutes = now.getHours() * 60 + now.getMinutes();

  const wrap = h('div.timeline');
  let nowPlaced = false;

  for (const task of tasks) {
    const minutes = timeToMinutes(task.time) ?? 0;
    if (isToday && !nowPlaced && minutes > nowMinutes) {
      wrap.appendChild(h('div.now-line'));
      nowPlaced = true;
    }
    const slot = h('div.slot', { class: isToday && Math.abs(minutes - nowMinutes) < 30 ? 'now' : '' },
      h('span.slot-time', minutesToTime(minutes)),
      taskRow(task));
    wrap.appendChild(slot);
  }

  if (isToday && !nowPlaced) wrap.appendChild(h('div.now-line'));
  return h('div',
    h('div.group-head', h('span.section-title', 'Scheduled'), h('span.group-count', tasks.length)),
    wrap);
}

function looseList(tasks, ghosts, cursor) {
  const list = h('div.list-group', { style: { minHeight: '48px' } },
    tasks.map((task) => taskRow(task)),
    ghosts.map(({ task, date }) => taskRow(task, { ghostDate: date, draggable: false })));

  makeDropZone(list, (taskId, beforeId) => {
    const siblings = dayTasks(store.tasks, cursor).filter((t) => !t.time);
    store.moveTask(taskId, {
      scope: 'day',
      date: cursor,
      order: orderFor(siblings, beforeId, taskId)
    });
  });

  return h('div',
    h('div.group-head',
      h('span.section-title', 'Anytime'),
      h('span.group-count', tasks.length),
      h('span.grow'),
      h('span.tiny.faint', 'drag tasks here')),
    list);
}

/* ── Week / month commitments ─────────────────────────────────────────────── */

function horizonLane(scope, cursor) {
  const settings = store.settings;
  const key = scope === 'week' ? startOfWeek(cursor, settings.weekStart) : monthKey(cursor);
  const tasks = doneLast(sortTasks(
    scope === 'week'
      ? weekTasks(applyFilter(store.tasks, ui.filter, store.trials), cursor, settings.weekStart)
      : monthTasks(applyFilter(store.tasks, ui.filter, store.trials), cursor),
    ui.filter.sort, store.trials));

  const body = h('div.list-group', { style: { minHeight: '40px' } },
    tasks.length
      ? tasks.map((task) => taskRow(task, { compact: true }))
      : h('div.empty', { style: { padding: '16px' } },
        scope === 'week' ? 'No weekly commitments' : 'No monthly goals'));

  makeDropZone(body, (taskId, beforeId) => {
    const siblings = scope === 'week'
      ? weekTasks(store.tasks, cursor, settings.weekStart)
      : monthTasks(store.tasks, cursor);
    store.moveTask(taskId, {
      scope,
      [scope]: key,
      order: orderFor(siblings, beforeId, taskId)
    });
    toast(scope === 'week' ? 'Moved to this week' : 'Moved to this month');
  });

  return h('div.card.side-card',
    h('div.card-head',
      icon(scope === 'week' ? 'week' : 'month', { size: 14 }),
      h('div.grow',
        h('div.card-title', scope === 'week' ? 'This week' : 'This month'),
        h('div.card-sub', `${tasks.filter((t) => !t.done).length} open`)),
      h('button.btn.btn-icon.btn-ghost', {
        type: 'button', 'aria-label': `Add a ${scope} task`,
        onclick: () => openTaskEditor(null, scope === 'week'
          ? { scope: 'week', week: key }
          : { scope: 'month', month: key })
      }, icon('plus', { size: 14 }))),
    h('div', { style: { padding: '10px' } }, body));
}

/* ── Daily note ───────────────────────────────────────────────────────────── */

function noteCard(cursor) {
  let saveTimer = null;
  const area = h('textarea.note-area', {
    value: store.state.notes[cursor] || '',
    placeholder: 'Notes for the day — calls, decisions, anything worth remembering.',
    'data-keep': `note-${cursor}`,
    oninput: (event) => {
      clearTimeout(saveTimer);
      const { value } = event.target;
      saveTimer = setTimeout(() => store.setNote(cursor, value), 500);
    }
  });

  return h('div.card.side-card',
    h('div.card-head', icon('note', { size: 14 }), h('div.card-title.grow', 'Day note')),
    h('div', { style: { padding: 'var(--card-pad)' } }, area));
}

/* ── Look ahead ───────────────────────────────────────────────────────────── */

function lookaheadCard(cursor, visible) {
  const rows = [];
  for (let offset = 1; offset <= 3; offset += 1) {
    const key = addDays(cursor, offset);
    const tasks = sortTasks(dayTasks(visible, key).filter((t) => !t.done), 'urgency');
    if (!tasks.length) continue;
    rows.push(h('div', { style: { marginBottom: '10px' } },
      h('div.row', { style: { marginBottom: '5px' } },
        h('span.section-title.grow', relativeLabel(key, cursor)),
        h('button.btn.btn-sm.btn-ghost', {
          type: 'button',
          onclick: () => { ui.cursor = key; rerender(); }
        }, 'Open')),
      h('div.list-group', tasks.slice(0, 4).map((task) => taskRow(task, { compact: true }))),
      tasks.length > 4 ? h('div.tiny.faint', { style: { padding: '4px 2px' } }, `+${tasks.length - 4} more`) : null));
  }

  return h('div.card.side-card',
    h('div.card-head',
      icon('arrowRight', { size: 14 }),
      h('div.card-title.grow', 'Coming up'),
      h('button.btn.btn-sm.btn-ghost', { type: 'button', onclick: () => navigate('week') }, 'Week')),
    h('div', { style: { padding: 'var(--card-pad)' } },
      rows.length ? rows : h('div.empty', { style: { padding: '18px' } }, 'Nothing in the next three days')));
}

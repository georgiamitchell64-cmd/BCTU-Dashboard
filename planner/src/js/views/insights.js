/* ============================================================================
 * insights.js — how the work is actually going
 * ----------------------------------------------------------------------------
 * Charts are hand-drawn from divs and SVG. No chart library, no network, and
 * the same palette as the rest of the app so nothing needs a second legend.
 * ========================================================================== */

import { h, icon, ring } from '../ui/dom.js';
import { taskRow } from '../ui/task.js';
import { store } from '../state/store.js';
import { ui } from '../state/ui.js';
import { rerender } from '../bus.js';
import {
  addDays, DAY_SHORT, formatDuration, formatShort, startOfWeek, todayKey, weekdayOf
} from '../core/dates.js';
import { URGENCIES } from '../core/parse.js';
import {
  allTags, completionsOn, countByTrial, countByUrgency, dailyCompletionSeries,
  daysInWeekTasks, overdueTasks, progress, sortTasks, streak, workload
} from '../core/select.js';

export function renderInsights() {
  const today = todayKey();
  const tasks = store.tasks;
  const weekStart = store.settings.weekStart;

  const series = dailyCompletionSeries(tasks, today, 28);
  const thisWeek = series.slice(-7).reduce((sum, day) => sum + day.count, 0);
  const lastWeek = series.slice(-14, -7).reduce((sum, day) => sum + day.count, 0);
  const open = tasks.filter((t) => !t.done);
  const late = overdueTasks(tasks, today);
  const weekProgress = progress(daysInWeekTasks(tasks, today, weekStart));
  const load = workload(open);
  const run = streak(tasks, today);

  return h('div.view-inner',
    h('div.page-head',
      h('div',
        h('h1.page-title', 'Insights'),
        h('div.page-sub', `${store.profile.name || 'Your'} planning, last four weeks`))),

    h('div.stat-grid',
      tile(String(thisWeek), 'Completed this week', trendLabel(thisWeek, lastWeek)),
      tile(String(run), run === 1 ? 'Day streak' : 'Day streak', run >= 3 ? 'Going well' : 'Keep it up', 'fire'),
      tile(String(open.length), 'Open tasks', load.minutes ? `${formatDuration(load.minutes)} estimated` : `${load.unestimated} unestimated`),
      tile(late.length ? String(late.length) : '0', 'Overdue', late.length ? 'Needs a decision' : 'All caught up', 'alert', late.length ? 'var(--u-critical)' : null)),

    h('div.today-grid', { style: { marginTop: 'var(--gap)' } },
      h('div',
        completionChart(series),
        heatmapCard(tasks, today, weekStart),
        weekdayCard(tasks, today)),
      h('div',
        weekRingCard(weekProgress),
        urgencyCard(open),
        trialCard(tasks),
        tagCard(tasks),
        staleCard(tasks, today))));
}

/* ── Tiles ────────────────────────────────────────────────────────────────── */

function tile(value, caption, hint = '', iconName = null, colour = null) {
  return h('div.card.stat-tile',
    h('div.row',
      h('div.grow',
        h('div.stat-value', { style: colour ? { color: colour } : {} }, value),
        h('div.stat-caption', caption)),
      iconName ? icon(iconName, { size: 20, className: 'faint' }) : null),
    hint ? h('div.tiny.faint', { style: { marginTop: '6px' } }, hint) : null);
}

function trendLabel(current, previous) {
  if (!previous && !current) return 'No activity yet';
  if (!previous) return 'First week of data';
  const delta = current - previous;
  if (delta === 0) return 'Same as last week';
  return `${delta > 0 ? '+' : ''}${delta} vs last week`;
}

/* ── Charts ───────────────────────────────────────────────────────────────── */

function completionChart(series) {
  const max = Math.max(1, ...series.map((day) => day.count));

  return h('div.card',
    h('div.card-head',
      icon('insights', { size: 15 }),
      h('div.grow', h('div.card-title', 'Tasks completed'), h('div.card-sub', 'Last 28 days')),
      h('span.tiny.faint', `peak ${max}/day`)),
    h('div', { style: { padding: 'var(--card-pad)' } },
      h('div.chart', { role: 'img', 'aria-label': `Completed tasks per day over the last 28 days, peak ${max}` },
        series.map((day, index) => h('div.chart-col', { title: `${formatShort(day.date)}: ${day.count}` },
          h('div.chart-bar', {
            class: day.count ? '' : 'zero',
            style: { height: `${Math.max(3, (day.count / max) * 120)}px` }
          }),
          index % 4 === 0 || index === series.length - 1
            ? h('span.chart-x', formatShort(day.date))
            : h('span.chart-x', ' '))))));
}

function heatmapCard(tasks, today, weekStart) {
  const weeks = 13;
  const start = addDays(startOfWeek(today, weekStart), -(weeks - 1) * 7);
  const cells = [];
  let total = 0;

  for (let i = 0; i < weeks * 7; i += 1) {
    const key = addDays(start, i);
    const count = key <= today ? completionsOn(tasks, key).length : 0;
    total += count;
    const level = count === 0 ? 0 : count <= 1 ? 1 : count <= 3 ? 2 : count <= 6 ? 3 : 4;
    cells.push(h('i', { dataset: { l: String(level) }, title: `${formatShort(key)} · ${count} done` }));
  }

  return h('div.card.side-card', { style: { marginTop: 'var(--gap)' } },
    h('div.card-head',
      icon('fire', { size: 15 }),
      h('div.grow', h('div.card-title', 'Consistency'), h('div.card-sub', `${total} tasks over 13 weeks`))),
    h('div', { style: { padding: 'var(--card-pad)', overflowX: 'auto' } },
      h('div.heat', cells),
      h('div.row', { style: { marginTop: '10px', gap: '5px' } },
        h('span.tiny.faint', 'quiet'),
        [0, 1, 2, 3, 4].map((level) => h('i', {
          style: { width: '11px', height: '11px', borderRadius: '3px' },
          dataset: { l: String(level) },
          class: 'heat-key'
        })),
        h('span.tiny.faint', 'busy'))));
}

function weekdayCard(tasks, today) {
  const buckets = Array.from({ length: 7 }, () => 0);
  for (let i = 0; i < 84; i += 1) {
    const key = addDays(today, -i);
    buckets[weekdayOf(key)] += completionsOn(tasks, key).length;
  }
  const max = Math.max(1, ...buckets);
  const order = Array.from({ length: 7 }, (_, i) => (i + store.settings.weekStart) % 7);
  const best = buckets.indexOf(max);

  return h('div.card.side-card', { style: { marginTop: 'var(--gap)' } },
    h('div.card-head',
      icon('week', { size: 15 }),
      h('div.grow',
        h('div.card-title', 'Your rhythm'),
        h('div.card-sub', buckets[best] ? `${DAY_SHORT[best]} is your most productive day` : 'Not enough data yet'))),
    h('div', { style: { padding: 'var(--card-pad)' } },
      h('div.chart', { style: { height: '110px' } },
        order.map((day) => h('div.chart-col', { title: `${DAY_SHORT[day]}: ${buckets[day]}` },
          h('div.chart-bar', {
            class: buckets[day] ? '' : 'zero',
            style: { height: `${Math.max(3, (buckets[day] / max) * 86)}px`, maxWidth: '34px' }
          }),
          h('span.chart-x', DAY_SHORT[day]))))));
}

/* ── Side panels ──────────────────────────────────────────────────────────── */

function weekRingCard(stats) {
  return h('div.card.card-pad',
    h('div.row', { style: { gap: '16px' } },
      ring(stats.pct, { size: 74, stroke: 7, label: `${stats.pct}%` }),
      h('div',
        h('div.card-title', 'This week'),
        h('div.small.muted', `${stats.done} of ${stats.total} day tasks done`),
        h('div.tiny.faint', { style: { marginTop: '4px' } },
          stats.remaining ? `${stats.remaining} still to go` : 'Week complete — nicely done'))));
}

function urgencyCard(open) {
  const counts = countByUrgency(open);
  const total = open.length || 1;

  return h('div.card.side-card',
    h('div.card-head', icon('flag', { size: 15 }), h('div.card-title.grow', 'Open by urgency')),
    h('div', { style: { padding: 'var(--card-pad)' } },
      h('div.split-bar', URGENCIES.map((urgency) => h('i', {
        title: `${urgency.label}: ${counts[urgency.id] || 0}`,
        style: { width: `${((counts[urgency.id] || 0) / total) * 100}%`, background: `var(--u-${urgency.id})` }
      }))),
      h('div.legend', { style: { marginTop: '10px' } },
        URGENCIES.map((urgency) => h('span',
          h('i', { style: { background: `var(--u-${urgency.id})` } }),
          `${urgency.label} ${counts[urgency.id] || 0}`)))));
}

function trialCard(tasks) {
  const rows = countByTrial(tasks, store.trials).filter((row) => row.total);
  if (!rows.length) return null;

  return h('div.card.side-card',
    h('div.card-head', icon('trials', { size: 15 }), h('div.card-title.grow', 'Workload by trial')),
    h('div', { style: { padding: 'var(--card-pad)' } },
      rows.map((row) => {
        const pct = Math.round((row.done / row.total) * 100);
        return h('div', { style: { marginBottom: '12px' } },
          h('div.row',
            h('i.chip-dot', { style: { background: row.trial.colour } }),
            h('span.small.grow', { style: { fontWeight: 600 } }, row.trial.code),
            h('span.tiny.faint', `${row.done}/${row.total} · ${pct}%`)),
          h('div.bar', { style: { '--tc': row.trial.colour } }, h('i', { style: { width: `${pct}%` } })));
      })));
}

function tagCard(tasks) {
  const tags = allTags(tasks).slice(0, 10);
  if (!tags.length) return null;

  return h('div.card.side-card',
    h('div.card-head', icon('filter', { size: 15 }), h('div.card-title.grow', 'Most used tags')),
    h('div', { style: { padding: 'var(--card-pad)' } },
      h('div.row.wrap', { style: { gap: '6px' } },
        tags.map(({ tag, count }) => h('button.chip.chip-btn', {
          type: 'button',
          onclick: () => { ui.filter.tag = tag; rerender(); }
        }, `#${tag}`, h('b', { style: { opacity: 0.6 } }, count))))));
}

/** Open tasks nobody has touched in a fortnight — usually the real problem. */
function staleCard(tasks, today) {
  const cutoff = addDays(today, -14);
  const stale = sortTasks(
    tasks.filter((t) => !t.done && (t.updatedAt || '').slice(0, 10) < cutoff),
    'urgency', store.trials
  ).slice(0, 5);
  if (!stale.length) return null;

  return h('div.card.side-card',
    h('div.card-head',
      icon('clock', { size: 15 }),
      h('div.grow',
        h('div.card-title', 'Gathering dust'),
        h('div.card-sub', 'Untouched for over two weeks'))),
    h('div', { style: { padding: '10px' } },
      h('div.list-group', stale.map((task) => taskRow(task, { showDate: true, compact: true, draggable: false })))));
}

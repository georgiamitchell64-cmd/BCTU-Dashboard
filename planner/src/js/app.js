/* ============================================================================
 * app.js — bootstrap, shell and routing
 * ========================================================================== */

import { $, clear, confetti, h, icon, toast } from './ui/dom.js';
import { store } from './state/store.js';
import { ui, resetCursor } from './state/ui.js';
import { on, navigate, rerender } from './bus.js';
import { applyTheme, watchSystemTheme } from './ui/theme.js';
import { installShortcuts, showShortcuts, defaultsForView } from './shortcuts.js';
import { openTaskEditor } from './ui/editor.js';
import { openPalette, registerCommands } from './ui/palette.js';
import { timerPill, start as startTimer } from './ui/timer.js';
import { resetReminderMemory, startReminders } from './ui/notify.js';
import { exportCSV, exportJSON, importFromFile } from './data-io.js';
import { formatMonth, formatWeekRange, formatLong, monthKey, todayKey } from './core/dates.js';
import { dayTasks, overdueTasks, progress } from './core/select.js';

import { renderToday } from './views/today.js';
import { renderWeek } from './views/week.js';
import { renderMonth } from './views/month.js';
import { renderMatrix } from './views/matrix.js';
import { renderTrials } from './views/trials.js';
import { renderInsights } from './views/insights.js';
import { renderSettings } from './views/settings.js';

const VIEWS = [
  { id: 'today', label: 'Today', icon: 'today', render: renderToday, group: 'Plan' },
  { id: 'week', label: 'Week', icon: 'week', render: renderWeek, group: 'Plan' },
  { id: 'month', label: 'Month', icon: 'month', render: renderMonth, group: 'Plan' },
  { id: 'matrix', label: 'Priority matrix', icon: 'matrix', render: renderMatrix, group: 'Focus' },
  { id: 'trials', label: 'Trials', icon: 'trials', render: renderTrials, group: 'Focus' },
  { id: 'insights', label: 'Insights', icon: 'insights', render: renderInsights, group: 'Focus' },
  { id: 'settings', label: 'Settings', icon: 'settings', render: renderSettings, group: 'You' }
];

const viewById = (id) => VIEWS.find((view) => view.id === id) || VIEWS[0];

let scheduled = false;
let wasDayComplete = false;

/* ── Render ───────────────────────────────────────────────────────────────── */

function paint() {
  const focus = captureFocus();

  applyTheme();
  paintSidebar();
  paintTopbar();

  const host = $('#view');
  clear(host);
  host.appendChild(viewById(ui.view).render());
  host.scrollTop = viewScroll.get(ui.view) ?? 0;

  restoreFocus(focus);
  celebrateIfDayFinished();
}

/** Coalesce bursts of store updates into one repaint. */
function schedulePaint() {
  if (scheduled) return;
  scheduled = true;
  requestAnimationFrame(() => {
    scheduled = false;
    paint();
  });
}

const viewScroll = new Map();

function captureFocus() {
  const el = document.activeElement;
  if (!el?.dataset?.keep) return null;
  return {
    keep: el.dataset.keep,
    value: el.value,
    start: el.selectionStart,
    end: el.selectionEnd
  };
}

function restoreFocus(snapshot) {
  if (!snapshot) return;
  const el = document.querySelector(`[data-keep="${CSS.escape(snapshot.keep)}"]`);
  if (!el) return;
  if (typeof snapshot.value === 'string' && el.value !== snapshot.value) el.value = snapshot.value;
  el.focus();
  try {
    el.setSelectionRange(snapshot.start, snapshot.end);
  } catch {
    // Not every input type supports a selection range; focus alone is enough.
  }
}

/* ── Shell ────────────────────────────────────────────────────────────────── */

function paintSidebar() {
  const nav = $('#nav');
  clear(nav);

  const hidden = new Set(store.settings.hiddenViews);
  const today = todayKey();
  const counts = {
    today: dayTasks(store.tasks, today).filter((t) => !t.done).length,
    matrix: overdueTasks(store.tasks, today).length
  };

  let lastGroup = null;
  for (const view of VIEWS) {
    if (hidden.has(view.id)) continue;
    if (view.group !== lastGroup) {
      lastGroup = view.group;
      nav.appendChild(h('div.nav-section', view.group));
    }
    const count = counts[view.id];
    nav.appendChild(h('button.nav-item', {
      type: 'button',
      class: ui.view === view.id ? 'active' : '',
      title: view.label,
      'aria-current': ui.view === view.id ? 'page' : null,
      onclick: () => navigate(view.id)
    },
    icon(view.icon, { size: 17 }),
    h('span.nav-label', view.label),
    count ? h('span.nav-count', String(count)) : null));
  }

  const profile = store.profile;
  const initials = (profile.name || 'You').trim().split(/\s+/).slice(0, 2)
    .map((word) => word[0]?.toUpperCase() || '').join('') || 'Y';

  const foot = $('#side-foot');
  clear(foot);
  foot.appendChild(h('div.avatar', { title: profile.name }, initials));
  foot.appendChild(h('div.side-foot-text',
    h('div.side-foot-name', profile.name || 'Your planner'),
    h('div.side-foot-role', profile.role || 'Personal planner')));
  foot.appendChild(h('button.btn.btn-icon.btn-ghost', {
    type: 'button',
    'aria-label': 'Settings',
    style: { color: 'rgba(255,255,255,.55)' },
    onclick: () => navigate('settings')
  }, icon('settings', { size: 15 })));
}

function paintTopbar() {
  const bar = $('#topbar');
  clear(bar);

  const today = todayKey();
  const subtitle = {
    today: formatLong(ui.cursor),
    week: formatWeekRange(ui.cursor, store.settings.weekStart),
    month: formatMonth(ui.month)
  }[ui.view] || '';

  bar.append(
    h('button.btn.btn-icon.btn-ghost', {
      type: 'button', 'aria-label': 'Collapse the sidebar', title: 'Collapse the sidebar',
      onclick: () => { ui.rail = !ui.rail; $('#app').classList.toggle('rail', ui.rail); }
    }, icon('menu', { size: 16 })),
    h('div',
      h('div.topbar-title', viewById(ui.view).label),
      subtitle ? h('div.topbar-sub', subtitle) : null),
    h('span.topbar-spacer'),
    timerPill(),
    h('button.btn.btn-sm', {
      type: 'button', title: 'Command palette (Ctrl+K)',
      onclick: () => openPalette()
    }, icon('search', { size: 13 }), h('span.kbd', '⌘K')),
    h('button.btn.btn-sm.btn-primary', {
      type: 'button', title: 'New task (N)',
      onclick: () => openTaskEditor(null, defaultsForView())
    }, icon('plus', { size: 13 }), 'New task'));
}

/* ── Celebration ──────────────────────────────────────────────────────────── */

function celebrateIfDayFinished() {
  const stats = progress(dayTasks(store.tasks, todayKey()));
  const complete = stats.total > 0 && stats.remaining === 0;
  if (complete && !wasDayComplete && store.settings.celebrate && ui.view === 'today') {
    confetti();
    toast('That is the day cleared. Nicely done.', { kind: 'success' });
  }
  wasDayComplete = complete;
}

/* ── Commands ─────────────────────────────────────────────────────────────── */

function commands() {
  const list = VIEWS.map((view) => ({
    id: `go-${view.id}`,
    group: 'Go to',
    icon: view.icon,
    label: view.label,
    run: () => navigate(view.id)
  }));

  return [
    { id: 'new-task', group: 'Create', icon: 'plus', label: 'New task', keys: 'N', run: () => openTaskEditor(null, defaultsForView()) },
    { id: 'new-week', group: 'Create', icon: 'week', label: 'New task for this week', run: () => openTaskEditor(null, { scope: 'week' }) },
    { id: 'new-month', group: 'Create', icon: 'month', label: 'New objective for this month', run: () => openTaskEditor(null, { scope: 'month', month: ui.month }) },
    ...list,
    { id: 'today', group: 'Actions', icon: 'today', label: 'Back to today', keys: 'T', run: () => { resetCursor(); rerender(); } },
    {
      id: 'carry',
      group: 'Actions',
      icon: 'arrowDown',
      label: 'Move overdue tasks to today',
      run: () => {
        const moved = store.carryOverdue();
        toast(moved ? `Moved ${moved} task${moved === 1 ? '' : 's'}` : 'Nothing overdue');
      }
    },
    { id: 'focus', group: 'Actions', icon: 'target', label: 'Start a focus session', keys: 'F', run: () => startTimer('focus') },
    {
      id: 'theme',
      group: 'Actions',
      icon: 'moon',
      label: 'Toggle dark mode',
      keys: 'D',
      run: () => {
        store.setSetting('theme', document.documentElement.dataset.theme === 'dark' ? 'light' : 'dark');
        applyTheme();
      }
    },
    { id: 'export-json', group: 'Data', icon: 'download', label: 'Export a JSON backup', run: exportJSON },
    { id: 'export-csv', group: 'Data', icon: 'download', label: 'Export tasks as CSV', run: exportCSV },
    { id: 'import', group: 'Data', icon: 'upload', label: 'Import a backup', run: importFromFile },
    { id: 'shortcuts', group: 'Help', icon: 'keyboard', label: 'Keyboard shortcuts', keys: '?', run: showShortcuts }
  ];
}

/* ── Desktop menu wiring ──────────────────────────────────────────────────── */

function wireDesktopMenu() {
  const bridge = globalThis.planner;
  if (!bridge?.on) return;

  bridge.on('menu:new-task', () => openTaskEditor(null, defaultsForView()));
  bridge.on('menu:quick-capture', () => {
    navigate('today');
    setTimeout(() => document.querySelector('[data-keep="quickadd"]')?.focus(), 60);
  });
  bridge.on('menu:navigate', (view) => navigate(view));
  bridge.on('menu:undo', () => { store.undo(); rerender(); });
  bridge.on('menu:redo', () => { store.redo(); rerender(); });
  bridge.on('menu:search', () => openPalette());
  bridge.on('menu:palette', () => openPalette());
  bridge.on('menu:shortcuts', () => showShortcuts());
  bridge.on('menu:export-csv', () => exportCSV());
  bridge.on('menu:toggle-theme', () => {
    store.setSetting('theme', document.documentElement.dataset.theme === 'dark' ? 'light' : 'dark');
    applyTheme();
  });
  bridge.on('data:replaced', (data) => {
    store.replace(data, { undoable: false });
    rerender();
    toast('Data imported', { kind: 'success' });
  });
  bridge.on('toast', ({ message, kind }) => toast(message, { kind }));
}

/* ── Startup ──────────────────────────────────────────────────────────────── */

function offerCarryOver() {
  if (!store.settings.carryOverdue) return;
  const late = overdueTasks(store.tasks, todayKey());
  if (!late.length) return;

  toast(`${late.length} task${late.length === 1 ? '' : 's'} left over from earlier`, {
    kind: 'info',
    duration: 12000,
    action: {
      label: 'Move to today',
      onClick: () => {
        const moved = store.carryOverdue();
        toast(`Moved ${moved} to today`, { kind: 'success', action: { label: 'Undo', onClick: () => store.undo() } });
      }
    }
  });
}

/** Roll the cursor forward if the app has been left open overnight. */
function watchDateRollover() {
  let known = todayKey();
  setInterval(() => {
    const now = todayKey();
    if (now === known) return;
    const wasOnToday = ui.cursor === known;
    known = now;
    resetReminderMemory();
    if (wasOnToday) {
      ui.cursor = now;
      ui.month = monthKey(now);
    }
    rerender();
  }, 60_000);
}

async function boot() {
  await store.init();

  ui.view = store.settings.startView || 'today';
  ui.filter.sort = store.settings.defaultSort || 'manual';
  resetCursor();

  applyTheme();
  watchSystemTheme();
  registerCommands(commands);
  installShortcuts();
  wireDesktopMenu();
  startReminders();
  watchDateRollover();

  store.subscribe(schedulePaint);
  on('rerender', schedulePaint);
  on('navigate', (view) => {
    viewScroll.set(ui.view, $('#view')?.scrollTop ?? 0);
    ui.view = view;
    if (view !== 'settings') store.setSetting('startView', view);
    paint();
  });

  paint();
  wasDayComplete = progress(dayTasks(store.tasks, todayKey())).remaining === 0;
  offerCarryOver();

  document.body.classList.remove('loading');
}

boot().catch((error) => {
  console.error(error);
  document.body.append(h('div.empty', { style: { margin: '40px' } },
    h('strong', 'The planner could not start'), String(error?.message || error)));
});

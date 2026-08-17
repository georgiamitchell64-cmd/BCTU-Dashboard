/* ============================================================================
 * shortcuts.js — global keyboard handling
 * ----------------------------------------------------------------------------
 * Single-key shortcuts are ignored while you are typing, so "n" in the middle
 * of a task title stays an "n".
 * ========================================================================== */

import { store } from './state/store.js';
import { ui } from './state/ui.js';
import { navigate, rerender } from './bus.js';
import { openTaskEditor } from './ui/editor.js';
import { openPalette } from './ui/palette.js';
import { toast, h, overlay, icon } from './ui/dom.js';
import { applyTheme } from './ui/theme.js';
import { addDays, addMonthsToMonthKey, monthKey, todayKey } from './core/dates.js';
import { isRunning, start as startTimer, stop as stopTimer } from './ui/timer.js';

export const SHORTCUTS = [
  { keys: 'N', description: 'New task' },
  { keys: 'Ctrl / ⌘ + K', description: 'Command palette' },
  { keys: '/', description: 'Search tasks' },
  { keys: '1 – 6', description: 'Jump between views' },
  { keys: 'T', description: 'Back to today' },
  { keys: '← / →', description: 'Previous / next day, week or month' },
  { keys: 'F', description: 'Start or stop the focus timer' },
  { keys: 'D', description: 'Toggle dark mode' },
  { keys: 'Ctrl / ⌘ + Z', description: 'Undo' },
  { keys: 'Ctrl / ⌘ + Shift + Z', description: 'Redo' },
  { keys: '?', description: 'This list' },
  { keys: 'Esc', description: 'Close whatever is open' }
];

const VIEW_KEYS = { 1: 'today', 2: 'week', 3: 'month', 4: 'matrix', 5: 'trials', 6: 'insights' };

const isTyping = () => {
  const el = document.activeElement;
  if (!el) return false;
  return el.isContentEditable
    || ['INPUT', 'TEXTAREA', 'SELECT'].includes(el.tagName);
};

function step(direction) {
  if (ui.view === 'month') ui.month = addMonthsToMonthKey(ui.month, direction);
  else if (ui.view === 'week') ui.cursor = addDays(ui.cursor, direction * 7);
  else ui.cursor = addDays(ui.cursor, direction);
  rerender();
}

export function showShortcuts() {
  overlay((close) => h('div.dialog',
    h('div.modal-head',
      icon('keyboard', { size: 16 }),
      h('h3.card-title.grow', 'Keyboard shortcuts'),
      h('button.btn.btn-icon.btn-ghost', { type: 'button', 'aria-label': 'Close', onclick: close }, icon('close', { size: 15 }))),
    h('div.modal-body',
      h('table.shortcut-table',
        h('tbody', SHORTCUTS.map((shortcut) => h('tr',
          h('td', shortcut.description),
          h('td', h('span.kbd', shortcut.keys)))))))
  ), { center: true });
}

export function installShortcuts() {
  document.addEventListener('keydown', (event) => {
    const meta = event.metaKey || event.ctrlKey;

    if (meta && event.key.toLowerCase() === 'k') {
      event.preventDefault();
      openPalette();
      return;
    }
    if (meta && event.key.toLowerCase() === 'z') {
      event.preventDefault();
      const label = event.shiftKey ? store.redo() : store.undo();
      toast(label ? `${event.shiftKey ? 'Redid' : 'Undid'} ${label}` : 'Nothing to undo');
      rerender();
      return;
    }
    if (meta && event.key.toLowerCase() === 'f') {
      event.preventDefault();
      focusSearch();
      return;
    }
    if (meta) return;
    if (isTyping()) return;

    switch (event.key) {
      case 'n':
      case 'N':
        event.preventDefault();
        openTaskEditor(null, defaultsForView());
        break;
      case '/':
        event.preventDefault();
        focusSearch();
        break;
      case 't':
      case 'T':
        ui.cursor = todayKey();
        ui.month = monthKey(todayKey());
        rerender();
        break;
      case 'ArrowLeft':
        step(-1);
        break;
      case 'ArrowRight':
        step(1);
        break;
      case 'f':
      case 'F':
        if (isRunning()) stopTimer(); else startTimer('focus');
        break;
      case 'd':
      case 'D': {
        const next = document.documentElement.dataset.theme === 'dark' ? 'light' : 'dark';
        store.setSetting('theme', next);
        applyTheme();
        break;
      }
      case '?':
        showShortcuts();
        break;
      default:
        if (VIEW_KEYS[event.key]) navigate(VIEW_KEYS[event.key]);
    }
  });
}

function focusSearch() {
  const input = document.querySelector('[data-keep="filter-search"]')
    || document.querySelector('[data-keep="quickadd"]');
  if (input) {
    input.focus();
    input.select?.();
  } else {
    openPalette();
  }
}

export function defaultsForView() {
  if (ui.view === 'month') return { scope: 'month', month: ui.month };
  if (ui.view === 'week') return { scope: 'day', date: ui.cursor };
  return { scope: 'day', date: ui.cursor };
}

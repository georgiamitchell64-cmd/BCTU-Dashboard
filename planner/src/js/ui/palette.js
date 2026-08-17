/* ============================================================================
 * palette.js — Ctrl/Cmd+K command palette
 * ----------------------------------------------------------------------------
 * Runs commands and finds tasks in the same box. Commands are registered by
 * app.js so the palette itself stays ignorant of routing.
 * ========================================================================== */

import { clear, h, icon, overlay } from './dom.js';
import { store } from '../state/store.js';
import { relativeLabel, todayKey } from '../core/dates.js';
import { openTaskEditor } from './editor.js';
import { matchesFilter } from '../core/select.js';

let commandProvider = () => [];

/** @param {() => Array<{id,label,hint?,icon?,group?,run:Function}>} provider */
export function registerCommands(provider) {
  commandProvider = provider;
}

const score = (text, query) => {
  const haystack = text.toLowerCase();
  if (haystack.startsWith(query)) return 0;
  const index = haystack.indexOf(query);
  return index === -1 ? Infinity : index + 1;
};

export function openPalette(initialQuery = '') {
  overlay((close) => {
    const list = h('div.palette-list', { role: 'listbox' });
    let items = [];
    let cursor = 0;

    const input = h('input', {
      type: 'text',
      value: initialQuery,
      placeholder: 'Type a command, or search your tasks…',
      'aria-label': 'Command palette',
      'data-autofocus': '',
      autocomplete: 'off',
      oninput: () => { cursor = 0; paint(); },
      onkeydown: (event) => {
        if (event.key === 'ArrowDown') { event.preventDefault(); move(1); }
        else if (event.key === 'ArrowUp') { event.preventDefault(); move(-1); }
        else if (event.key === 'Enter') { event.preventDefault(); run(items[cursor]); }
        else if (event.key === 'Tab') { event.preventDefault(); move(event.shiftKey ? -1 : 1); }
      }
    });

    const move = (delta) => {
      if (!items.length) return;
      cursor = (cursor + delta + items.length) % items.length;
      paint(true);
    };

    const run = (item) => {
      if (!item) return;
      close();
      item.run();
    };

    function collect() {
      const query = input.value.trim().toLowerCase();
      const commands = commandProvider()
        .map((command) => ({
          ...command,
          _score: query
            ? Math.min(score(command.label, query), command.hint ? score(command.hint, query) + 2 : Infinity)
            : 0
        }))
        .filter((command) => command._score !== Infinity)
        .sort((a, b) => a._score - b._score);

      if (!query) return commands.slice(0, 12);

      const today = todayKey();
      const tasks = store.tasks
        .filter((task) => matchesFilter(task, { query, showDone: true }, store.trials))
        .slice(0, 8)
        .map((task) => ({
          id: `task-${task.id}`,
          group: 'Tasks',
          icon: task.done ? 'check' : 'today',
          label: task.title,
          hint: task.scope === 'day' && task.date
            ? relativeLabel(task.date, today)
            : task.scope === 'week' ? 'This week' : 'This month',
          run: () => openTaskEditor(task.id)
        }));

      return [...commands.slice(0, 8), ...tasks];
    }

    function paint(cursorOnly = false) {
      if (!cursorOnly) items = collect();
      clear(list);

      if (!items.length) {
        list.appendChild(h('div.empty', 'Nothing matches that'));
        return;
      }

      let lastGroup = null;
      items.forEach((item, index) => {
        if (item.group && item.group !== lastGroup) {
          lastGroup = item.group;
          list.appendChild(h('div.palette-group', item.group));
        }
        const row = h('div.palette-item', {
          role: 'option',
          'aria-selected': index === cursor ? 'true' : 'false',
          onmousemove: () => { if (cursor !== index) { cursor = index; paint(true); } },
          onclick: () => run(item)
        },
        icon(item.icon || 'arrowRight', { size: 15 }),
        h('span.grow', item.label),
        item.hint ? h('span.tiny.faint', item.hint) : null,
        item.keys ? h('span.kbd', item.keys) : null);
        list.appendChild(row);
        if (index === cursor) queueMicrotask(() => row.scrollIntoView({ block: 'nearest' }));
      });
    }

    paint();
    return h('div.palette', { role: 'dialog', 'aria-label': 'Command palette' }, input, list);
  });
}

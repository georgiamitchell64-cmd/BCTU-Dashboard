/* ============================================================================
 * matrix.js — urgent/important priority matrix
 * ----------------------------------------------------------------------------
 * The Eisenhower grid. Dragging a task into a quadrant is a real edit: it sets
 * the urgency and the important flag to match where you dropped it.
 * ========================================================================== */

import { h, icon, toast } from '../ui/dom.js';
import { taskRow } from '../ui/task.js';
import { filterBar } from '../ui/filters.js';
import { makeDropZone } from '../ui/dnd.js';
import { store } from '../state/store.js';
import { ui } from '../state/ui.js';
import { rerender } from '../bus.js';
import { todayKey } from '../core/dates.js';
import { applyFilter, QUADRANTS, quadrantOf, sortTasks } from '../core/select.js';

export function renderMatrix() {
  const today = todayKey();
  const open = applyFilter(store.tasks, ui.filter, store.trials).filter((t) => !t.done);
  const grouped = Object.fromEntries(QUADRANTS.map((q) => [q.id, []]));
  for (const task of open) grouped[quadrantOf(task, today)].push(task);

  return h('div.view-inner',
    h('div.page-head',
      h('div',
        h('h1.page-title', 'Priority matrix'),
        h('div.page-sub', `${open.length} open tasks, sorted by what actually matters`)),
      h('div.legend',
        h('span', h('i', { style: { background: 'var(--u-critical)' } }), 'Urgent = critical, high, or due within a day'),
        h('span', h('i', { style: { background: 'var(--accent)' } }), 'Important = flagged, or critical'))),

    filterBar({ showSort: true }),

    h('div.matrix', QUADRANTS.map((quadrant) => {
      const tasks = sortTasks(grouped[quadrant.id], ui.filter.sort === 'manual' ? 'urgency' : ui.filter.sort, store.trials);

      const body = h('div.quad-body',
        tasks.length
          ? tasks.map((task) => taskRow(task, { showDate: true, compact: true }))
          : h('div.empty', { style: { padding: '18px' } }, 'Nothing here'));

      makeDropZone(body, (taskId) => {
        const task = store.task(taskId);
        if (!task) return;
        const patch = { important: quadrant.important };
        if (quadrant.urgent) {
          if (!['critical', 'high'].includes(task.urgency)) patch.urgency = 'high';
        } else if (['critical', 'high'].includes(task.urgency)) {
          patch.urgency = 'medium';
        }
        store.updateTask(taskId, patch, 'reprioritise');
        toast(`Moved to “${quadrant.label}”`, {
          kind: 'success',
          action: { label: 'Undo', onClick: () => store.undo() }
        });
      }, { sortable: false });

      return h('div.quad', { dataset: { q: quadrant.id } },
        h('div.quad-head',
          h('div.grow',
            h('div.card-title', quadrant.label),
            h('div.card-sub', quadrant.hint)),
          h('span.group-count', tasks.length)),
        body);
    })),

    h('div.card.card-pad', { style: { marginTop: 'var(--gap)' } },
      h('div.row',
        icon('alert', { size: 15, className: 'faint' }),
        h('span.small.muted.grow',
          'Drop a task into a quadrant to change its urgency and importance. ' +
          'If “Do now” is filling up faster than it empties, that is the signal, not the failure.'),
        h('button.btn.btn-sm', {
          type: 'button',
          onclick: () => { ui.filter.sort = 'urgency'; rerender(); }
        }, 'Sort by urgency'))));
}

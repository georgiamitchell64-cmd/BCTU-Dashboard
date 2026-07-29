/* ============================================================================
 * dnd.js — drag and drop
 * ----------------------------------------------------------------------------
 * HTML5 drag and drop, with the dragged id also kept in a module variable:
 * dataTransfer is unreadable during dragover, which is exactly when the drop
 * indicator needs to know what is being dragged.
 * ========================================================================== */

let dragging = null;
let indicator = null;

export function makeDraggable(el, taskId, { handle = null } = {}) {
  el.draggable = true;
  el.dataset.taskId = taskId;

  el.addEventListener('dragstart', (event) => {
    if (handle && !event.target.closest(handle)) {
      event.preventDefault();
      return;
    }
    dragging = taskId;
    el.classList.add('dragging');
    event.dataTransfer.effectAllowed = 'move';
    event.dataTransfer.setData('text/plain', taskId);
  });

  el.addEventListener('dragend', () => {
    dragging = null;
    el.classList.remove('dragging');
    removeIndicator();
    document.querySelectorAll('.drop-zone.over').forEach((z) => z.classList.remove('over'));
  });

  return el;
}

function removeIndicator() {
  indicator?.remove();
  indicator = null;
}

/**
 * @param {HTMLElement} el              container that accepts drops
 * @param {(taskId: string, beforeId: string|null) => void} onDrop
 * @param {{ sortable?: boolean }} [options]  sortable inserts a position line
 */
export function makeDropZone(el, onDrop, { sortable = true } = {}) {
  el.classList.add('drop-zone');

  el.addEventListener('dragover', (event) => {
    if (!dragging) return;
    event.preventDefault();
    event.dataTransfer.dropEffect = 'move';
    el.classList.add('over');
    if (sortable) showIndicator(el, event.clientY);
  });

  el.addEventListener('dragleave', (event) => {
    if (el.contains(event.relatedTarget)) return;
    el.classList.remove('over');
    removeIndicator();
  });

  el.addEventListener('drop', (event) => {
    event.preventDefault();
    event.stopPropagation();
    const taskId = dragging || event.dataTransfer.getData('text/plain');
    const beforeId = sortable ? beforeTaskId(el, event.clientY) : null;
    el.classList.remove('over');
    removeIndicator();
    dragging = null;
    if (taskId) onDrop(taskId, beforeId);
  });

  return el;
}

/** Id of the task the drop should land in front of, or null for "append". */
function beforeTaskId(container, y) {
  for (const child of container.querySelectorAll(':scope > [data-task-id]')) {
    if (child.classList.contains('dragging')) continue;
    const box = child.getBoundingClientRect();
    if (y < box.top + box.height / 2) return child.dataset.taskId;
  }
  return null;
}

function showIndicator(container, y) {
  const before = beforeTaskId(container, y);
  if (!indicator) indicator = document.createElement('div');
  indicator.className = 'drop-line';
  const target = before ? container.querySelector(`[data-task-id="${CSS.escape(before)}"]`) : null;
  if (target) container.insertBefore(indicator, target);
  else container.appendChild(indicator);
}

/**
 * Work out the new `order` value for a task dropped into a list.
 * Returns a number that sits between its new neighbours.
 */
export function orderFor(list, beforeId, movingId) {
  const others = list.filter((t) => t.id !== movingId);
  if (!others.length) return 0;

  const index = beforeId ? others.findIndex((t) => t.id === beforeId) : -1;
  if (index === -1) return others[others.length - 1].order + 1;
  if (index === 0) return others[0].order - 1;
  return (others[index - 1].order + others[index].order) / 2;
}

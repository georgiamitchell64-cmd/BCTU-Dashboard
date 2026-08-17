/* ============================================================================
 * data-io.js — export, import and reset
 * ----------------------------------------------------------------------------
 * Uses the Electron save dialog when it is there, and falls back to an ordinary
 * browser download when the renderer is running outside the desktop shell.
 * ========================================================================== */

import { confirmDialog, toast } from './ui/dom.js';
import { store } from './state/store.js';
import { rerender } from './bus.js';
import { defaultState } from './state/model.js';
import { formatTime } from './core/dates.js';

const BRIDGE = globalThis.planner ?? null;
const stamp = () => new Date().toISOString().slice(0, 10);

async function saveFile(filename, contents, mime, filters) {
  if (BRIDGE?.saveText) {
    const path = await BRIDGE.saveText({ defaultPath: filename, contents, filters });
    if (path) toast(`Saved to ${path.split(/[/\\]/).pop()}`, { kind: 'success' });
    return;
  }
  const url = URL.createObjectURL(new Blob([contents], { type: mime }));
  const link = document.createElement('a');
  link.href = url;
  link.download = filename;
  link.click();
  URL.revokeObjectURL(url);
  toast(`Downloaded ${filename}`, { kind: 'success' });
}

export function exportJSON() {
  return saveFile(
    `planner-backup-${stamp()}.json`,
    JSON.stringify(store.state, null, 2),
    'application/json',
    [{ name: 'JSON', extensions: ['json'] }]
  );
}

const csvCell = (value) => {
  const text = value === null || value === undefined ? '' : String(value);
  // Prefix formula-looking cells so a spreadsheet treats them as text.
  const safe = /^[=+\-@]/.test(text) ? `'${text}` : text;
  return /[",\n]/.test(safe) ? `"${safe.replace(/"/g, '""')}"` : safe;
};

export function exportCSV() {
  const headers = ['Title', 'Scope', 'Date', 'Week', 'Month', 'Time', 'Estimate (min)',
    'Urgency', 'Important', 'Trial', 'Tags', 'Repeats', 'Done', 'Completed at', 'Notes'];

  const rows = store.tasks.map((task) => [
    task.title,
    task.scope,
    task.date || '',
    task.week || '',
    task.month || '',
    task.time ? formatTime(task.time, true) : '',
    task.estimate ?? '',
    task.urgency,
    task.important ? 'yes' : '',
    store.trial(task.trialId)?.code || '',
    task.tags.join(' '),
    task.recur ? task.recur.freq : '',
    task.done ? 'yes' : 'no',
    task.completedAt || '',
    (task.notes || '').replace(/\s+/g, ' ').trim()
  ]);

  const csv = [headers, ...rows].map((row) => row.map(csvCell).join(',')).join('\r\n');
  return saveFile(`planner-tasks-${stamp()}.csv`, `﻿${csv}`, 'text/csv',
    [{ name: 'CSV', extensions: ['csv'] }]);
}

/** Browser-side import. In the desktop app the File menu route is richer. */
export function importFromFile() {
  const input = document.createElement('input');
  input.type = 'file';
  input.accept = 'application/json,.json';
  input.onchange = async () => {
    const file = input.files?.[0];
    if (!file) return;
    try {
      const parsed = JSON.parse(await file.text());
      const ok = await confirmDialog({
        title: 'Replace your planner data?',
        message: `“${file.name}” will replace everything currently in the app. You can undo this straight afterwards with Ctrl+Z.`,
        confirmLabel: 'Replace',
        danger: true
      });
      if (!ok) return;
      store.replace(parsed);
      rerender();
      toast('Data imported', { kind: 'success', action: { label: 'Undo', onClick: () => { store.undo(); rerender(); } } });
    } catch {
      toast('That file could not be read as planner data', { kind: 'error' });
    }
  };
  input.click();
}

export async function resetEverything() {
  const ok = await confirmDialog({
    title: 'Reset everything?',
    message: 'All tasks, trials, notes and settings are cleared and the app starts fresh. Export a backup first if you might want any of it back.',
    confirmLabel: 'Reset'
  });
  if (!ok) return;
  store.replace(defaultState());
  rerender();
  toast('Planner reset', { kind: 'undo', action: { label: 'Undo', onClick: () => { store.undo(); rerender(); } } });
}

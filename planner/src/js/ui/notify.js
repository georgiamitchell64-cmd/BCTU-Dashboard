/* ============================================================================
 * notify.js — desktop notifications and due-time reminders
 * ----------------------------------------------------------------------------
 * Checks once a minute for tasks whose start time is coming up, and fires one
 * notification per task per day. Silently does nothing if notifications are
 * switched off in Settings or denied by the OS.
 * ========================================================================== */

import { store } from '../state/store.js';
import { timeToMinutes, todayKey } from '../core/dates.js';
import { toast } from './dom.js';

const fired = new Set();
let timer = null;

export function notify(title, body) {
  if (!store.settings.notifications) return;
  try {
    if (typeof Notification === 'undefined') return;
    if (Notification.permission === 'granted') {
      new Notification(title, { body, silent: false });
    } else if (Notification.permission !== 'denied') {
      Notification.requestPermission().then((permission) => {
        if (permission === 'granted') new Notification(title, { body });
      });
    }
  } catch {
    // A notification failing is never worth interrupting the user for.
  }
}

function check() {
  if (!store.settings.notifications) return;
  const today = todayKey();
  const now = new Date();
  const nowMinutes = now.getHours() * 60 + now.getMinutes();
  const lead = store.settings.reminderLead;

  for (const task of store.tasks) {
    if (task.done || task.scope !== 'day' || task.date !== today || !task.time) continue;

    const startsAt = timeToMinutes(task.time);
    if (startsAt === null) continue;

    const key = `${today}:${task.id}`;
    if (fired.has(key)) continue;

    const minutesAway = startsAt - nowMinutes;
    if (minutesAway > lead || minutesAway < -1) continue;

    fired.add(key);
    const when = minutesAway <= 0 ? 'now' : `in ${minutesAway} min`;
    notify(`${task.title}`, `Starting ${when}${task.estimate ? ` · ${task.estimate} min` : ''}`);
    toast(`${task.title} — starting ${when}`, { kind: 'info', duration: 8000 });
  }
}

export function startReminders() {
  clearInterval(timer);
  check();
  timer = setInterval(check, 60_000);
}

export function resetReminderMemory() {
  fired.clear();
}

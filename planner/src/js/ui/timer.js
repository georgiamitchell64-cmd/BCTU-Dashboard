/* ============================================================================
 * timer.js — focus timer
 * ----------------------------------------------------------------------------
 * A pomodoro-style timer that lives in the top bar. It counts down from the
 * length set in Settings, then offers a break. The end time is stored as a
 * timestamp rather than a tick count, so it stays accurate even if the app is
 * backgrounded and the interval is throttled.
 * ========================================================================== */

import { clear, h, icon, toast } from './dom.js';
import { store } from '../state/store.js';
import { notify } from './notify.js';

const state = {
  mode: 'idle', // idle | focus | break
  endsAt: null,
  remaining: 0,
  taskId: null,
  completedSessions: 0
};

const listeners = new Set();
let ticker = null;

export const timerState = () => ({ ...state });
export const onTimerChange = (fn) => { listeners.add(fn); return () => listeners.delete(fn); };
const emit = () => { for (const fn of listeners) fn(timerState()); };

function tick() {
  if (!state.endsAt) return;
  state.remaining = Math.max(0, Math.round((state.endsAt - Date.now()) / 1000));
  if (state.remaining === 0) finish();
  else emit();
}

function finish() {
  const wasFocus = state.mode === 'focus';
  stopTicker();
  if (wasFocus) {
    state.completedSessions += 1;
    const task = state.taskId ? store.task(state.taskId) : null;
    notify('Focus session done', task ? `Time's up on “${task.title}”. Take a break.` : "Time's up. Take a break.");
    toast('Focus session finished — take a break', {
      kind: 'success',
      action: { label: 'Start break', onClick: () => start('break') }
    });
  } else {
    notify('Break over', 'Ready for another focus session?');
    toast('Break over');
  }
  state.mode = 'idle';
  state.endsAt = null;
  state.remaining = 0;
  emit();
}

function startTicker() {
  stopTicker();
  ticker = setInterval(tick, 500);
}

function stopTicker() {
  clearInterval(ticker);
  ticker = null;
}

export function start(mode = 'focus', taskId = null) {
  const minutes = mode === 'break' ? store.settings.breakMinutes : store.settings.focusMinutes;
  state.mode = mode;
  state.taskId = taskId;
  state.endsAt = Date.now() + minutes * 60_000;
  state.remaining = minutes * 60;
  startTicker();
  emit();
}

export function pause() {
  if (state.mode === 'idle' || !state.endsAt) return;
  state.remaining = Math.max(0, Math.round((state.endsAt - Date.now()) / 1000));
  state.endsAt = null;
  stopTicker();
  emit();
}

export function resume() {
  if (state.mode === 'idle' || state.endsAt) return;
  state.endsAt = Date.now() + state.remaining * 1000;
  startTicker();
  emit();
}

export function stop() {
  stopTicker();
  state.mode = 'idle';
  state.endsAt = null;
  state.remaining = 0;
  state.taskId = null;
  emit();
}

export const isRunning = () => state.mode !== 'idle' && Boolean(state.endsAt);

export const formatClock = (seconds) =>
  `${Math.floor(seconds / 60)}:${String(seconds % 60).padStart(2, '0')}`;

/** The top-bar control. Re-renders itself on every timer change. */
export function timerPill() {
  const host = h('div');

  const paint = () => {
    clear(host);
    const running = isRunning();
    const paused = state.mode !== 'idle' && !state.endsAt;

    if (state.mode === 'idle') {
      host.appendChild(h('button.btn.btn-sm', {
        type: 'button',
        title: `Start a ${store.settings.focusMinutes} minute focus session`,
        onclick: () => start('focus')
      }, icon('play', { size: 12 }), 'Focus'));
      return;
    }

    const task = state.taskId ? store.task(state.taskId) : null;
    host.appendChild(h('div.timer-pill', {
      class: [running ? 'running' : '', state.mode === 'break' ? 'break' : ''].filter(Boolean).join(' '),
      title: task ? task.title : state.mode === 'break' ? 'Break' : 'Focus session'
    },
    icon(state.mode === 'break' ? 'sun' : 'target', { size: 13 }),
    h('span.timer-time', formatClock(state.remaining)),
    h('button.btn.btn-icon.btn-ghost', {
      type: 'button', 'aria-label': paused ? 'Resume' : 'Pause',
      onclick: () => (paused ? resume() : pause())
    }, icon(paused ? 'play' : 'pause', { size: 12 })),
    h('button.btn.btn-icon.btn-ghost', {
      type: 'button', 'aria-label': 'Stop timer', onclick: stop
    }, icon('stop', { size: 12 }))));
  };

  paint();
  onTimerChange(paint);
  return host;
}

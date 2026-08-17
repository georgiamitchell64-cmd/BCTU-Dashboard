/* ============================================================================
 * dom.js — element helpers, toasts, dialogs
 * ----------------------------------------------------------------------------
 * `h()` builds elements without ever going through innerHTML, so a task called
 * "<script>" is just a task called "<script>".
 * ========================================================================== */

import { icon } from './icons.js';

/**
 * h('div.card#main', { onclick, dataset: {…} }, child, [children])
 * The tag accepts CSS-ish shorthand: 'button.btn.btn-primary', 'input.input#task-title'.
 */
export function h(spec, props = null, ...children) {
  const text = String(spec);
  const tag = text.match(/^[a-zA-Z0-9-]*/)[0] || 'div';
  const classes = [...text.matchAll(/\.([^.#]+)/g)].map((m) => m[1]);
  const id = text.match(/#([^.#]+)/)?.[1];

  const el = document.createElement(tag);
  if (classes.length) el.className = classes.join(' ');
  if (id) el.id = id;

  if (props && typeof props === 'object' && !(props instanceof Node) && !Array.isArray(props)) {
    for (const [key, value] of Object.entries(props)) {
      if (value === null || value === undefined || value === false) continue;
      if (key === 'class' || key === 'className') {
        el.className = `${el.className} ${value}`.trim();
      } else if (key === 'style' && typeof value === 'object') {
        Object.assign(el.style, value);
      } else if (key === 'dataset') {
        Object.assign(el.dataset, value);
      } else if (key.startsWith('on') && typeof value === 'function') {
        el.addEventListener(key.slice(2), value);
      } else if (key === 'html') {
        el.textContent = ''; // guard: never assign raw HTML
        el.append(String(value));
      } else if (key in el && key !== 'list' && typeof value !== 'object') {
        el[key] = value;
      } else {
        el.setAttribute(key, value === true ? '' : String(value));
      }
    }
  } else if (props !== null && props !== undefined) {
    children.unshift(props);
  }

  append(el, children);
  return el;
}

export function append(parent, children) {
  for (const child of children.flat(4)) {
    if (child === null || child === undefined || child === false || child === '') continue;
    parent.append(child instanceof Node ? child : document.createTextNode(String(child)));
  }
  return parent;
}

export const clear = (el) => { while (el.firstChild) el.removeChild(el.firstChild); return el; };

export const $ = (selector, root = document) => root.querySelector(selector);
export const $$ = (selector, root = document) => [...root.querySelectorAll(selector)];

export function debounce(fn, wait = 200) {
  let timer;
  return (...args) => {
    clearTimeout(timer);
    timer = setTimeout(() => fn(...args), wait);
  };
}

/* ── Progress ring ────────────────────────────────────────────────────────── */

export function ring(pct, { size = 54, stroke = 5, label = null } = {}) {
  const radius = (size - stroke) / 2;
  const circumference = 2 * Math.PI * radius;
  const svgNS = 'http://www.w3.org/2000/svg';

  const svg = document.createElementNS(svgNS, 'svg');
  svg.setAttribute('class', 'ring');
  svg.setAttribute('width', size);
  svg.setAttribute('height', size);
  svg.setAttribute('viewBox', `0 0 ${size} ${size}`);

  for (const cls of ['track', 'value']) {
    const circle = document.createElementNS(svgNS, 'circle');
    circle.setAttribute('class', cls);
    circle.setAttribute('cx', size / 2);
    circle.setAttribute('cy', size / 2);
    circle.setAttribute('r', radius);
    circle.setAttribute('stroke-width', stroke);
    if (cls === 'value') {
      circle.setAttribute('stroke-dasharray', circumference);
      circle.setAttribute('stroke-dashoffset', circumference * (1 - Math.max(0, Math.min(100, pct)) / 100));
    }
    svg.appendChild(circle);
  }

  return h('div.ring-wrap', svg, label === null ? null : h('span.ring-label', label));
}

/* ── Toasts ───────────────────────────────────────────────────────────────── */

let toastHost = null;

export function toast(message, { kind = 'info', action = null, duration = 4200 } = {}) {
  if (!toastHost) {
    toastHost = h('div.toasts', { role: 'status', 'aria-live': 'polite' });
    document.body.appendChild(toastHost);
  }

  const iconName = { success: 'check', error: 'alert', info: 'inbox', undo: 'undo' }[kind] || 'inbox';
  const el = h(`div.toast.${kind}`,
    icon(iconName, { size: 15 }),
    h('span.grow', message),
    action ? h('button', {
      type: 'button',
      onclick: () => { action.onClick(); dismiss(); }
    }, action.label) : null);

  const dismiss = () => {
    if (!el.isConnected) return;
    el.classList.add('out');
    setTimeout(() => el.remove(), 220);
  };

  toastHost.appendChild(el);
  const timer = setTimeout(dismiss, duration);
  el.addEventListener('mouseenter', () => clearTimeout(timer));
  return dismiss;
}

/* ── Overlays ─────────────────────────────────────────────────────────────── */

const overlayStack = [];

/**
 * Show an overlay. `render(close)` returns the panel element.
 * Escape and a click on the backdrop both close it.
 */
export function overlay(render, { center = false, onClose = null } = {}) {
  const scrim = h(`div.scrim${center ? '.center' : ''}`);
  const lastFocused = document.activeElement;

  const close = () => {
    if (!scrim.isConnected) return;
    scrim.remove();
    document.removeEventListener('keydown', onKey, true);
    const index = overlayStack.indexOf(close);
    if (index >= 0) overlayStack.splice(index, 1);
    onClose?.();
    if (lastFocused instanceof HTMLElement) lastFocused.focus();
  };

  const onKey = (event) => {
    if (event.key !== 'Escape') return;
    if (overlayStack[overlayStack.length - 1] !== close) return;
    event.stopPropagation();
    event.preventDefault();
    close();
  };

  scrim.addEventListener('mousedown', (event) => { if (event.target === scrim) close(); });
  document.addEventListener('keydown', onKey, true);
  overlayStack.push(close);

  const panel = render(close);
  scrim.appendChild(panel);
  document.body.appendChild(scrim);

  // Focus the first sensible control so the keyboard works immediately.
  const focusTarget = panel.querySelector('[data-autofocus]')
    || panel.querySelector('input, textarea, select, button');
  focusTarget?.focus?.();
  if (focusTarget?.select && focusTarget.dataset.selectAll !== undefined) focusTarget.select();

  return close;
}

export function confirmDialog({ title, message, confirmLabel = 'Delete', danger = true }) {
  return new Promise((resolve) => {
    let settled = false;
    const finish = (value, close) => { settled = true; close(); resolve(value); };

    overlay((close) => h('div.dialog', { style: { maxWidth: '420px' } },
      h('div.modal-head', h('h3.card-title', title)),
      h('div.modal-body', h('p.small.muted', message)),
      h('div.modal-foot',
        h('span.grow'),
        h('button.btn', { type: 'button', onclick: () => finish(false, close) }, 'Cancel'),
        h(`button.btn.${danger ? 'btn-danger' : 'btn-primary'}`, {
          type: 'button',
          'data-autofocus': '',
          onclick: () => finish(true, close)
        }, confirmLabel))
    ), { center: true, onClose: () => { if (!settled) resolve(false); } });
  });
}

/** Small celebration when a day is finished. Purely decorative. */
export function confetti(colours = ['#2EC4A5', '#3B82F6', '#F59E0B', '#EF4444', '#8B5CF6']) {
  const host = h('div.confetti');
  for (let i = 0; i < 44; i += 1) {
    host.appendChild(h('i', {
      style: {
        left: `${Math.random() * 100}%`,
        background: colours[i % colours.length],
        animationDelay: `${Math.random() * 320}ms`,
        animationDuration: `${1100 + Math.random() * 700}ms`,
        borderRadius: Math.random() > 0.5 ? '50%' : '2px'
      }
    }));
  }
  document.body.appendChild(host);
  setTimeout(() => host.remove(), 2400);
}

export { icon };

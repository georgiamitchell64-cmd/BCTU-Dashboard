/* ============================================================================
 * bus.js — tiny event bus
 * ----------------------------------------------------------------------------
 * Lets deep components ask for a re-render or a navigation without importing
 * app.js, which would create an import cycle.
 * ========================================================================== */

const target = new EventTarget();

export const on = (type, handler) => {
  const listener = (event) => handler(event.detail);
  target.addEventListener(type, listener);
  return () => target.removeEventListener(type, listener);
};

export const emit = (type, detail = null) =>
  target.dispatchEvent(new CustomEvent(type, { detail }));

export const rerender = () => emit('rerender');
export const navigate = (view) => emit('navigate', view);

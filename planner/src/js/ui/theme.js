/* ============================================================================
 * theme.js — applies appearance settings to the document
 * ========================================================================== */

import { store } from '../state/store.js';

export const ACCENTS = [
  { id: '#2EC4A5', name: 'BCTU teal' },
  { id: '#3B82F6', name: 'Blue' },
  { id: '#8B5CF6', name: 'Violet' },
  { id: '#EC4899', name: 'Pink' },
  { id: '#F59E0B', name: 'Amber' },
  { id: '#10B981', name: 'Green' },
  { id: '#EF4444', name: 'Coral' },
  { id: '#0EA5E9', name: 'Sky' }
];

const media = globalThis.matchMedia?.('(prefers-color-scheme: dark)');

/** Mix an accent with black so text on tinted backgrounds stays readable. */
function inkFor(hex) {
  const value = hex.replace('#', '');
  const [r, g, b] = [0, 2, 4].map((i) => parseInt(value.slice(i, i + 2), 16));
  const darken = (channel) => Math.round(channel * 0.42);
  return `#${[r, g, b].map((c) => darken(c).toString(16).padStart(2, '0')).join('')}`;
}

function shade(hex, amount) {
  const value = hex.replace('#', '');
  const channels = [0, 2, 4].map((i) => parseInt(value.slice(i, i + 2), 16));
  const shifted = channels.map((c) => Math.max(0, Math.min(255, Math.round(c + amount))));
  return `#${shifted.map((c) => c.toString(16).padStart(2, '0')).join('')}`;
}

export function applyTheme(settings = store.settings) {
  const root = document.documentElement;
  const dark = settings.theme === 'dark'
    || (settings.theme === 'system' && Boolean(media?.matches));

  root.dataset.theme = dark ? 'dark' : 'light';
  root.dataset.density = settings.density;

  const accent = settings.accent || '#2EC4A5';
  root.style.setProperty('--accent', accent);
  root.style.setProperty('--accent-dk', shade(accent, -28));
  root.style.setProperty('--accent-ink', inkFor(accent));
  root.style.setProperty('--accent-soft', `color-mix(in srgb, ${accent} 14%, transparent)`);

  const meta = document.querySelector('meta[name="theme-color"]');
  if (meta) meta.content = dark ? '#0F172A' : '#EEF3F8';
}

/** Follow the OS when the theme is set to "system". */
export function watchSystemTheme() {
  media?.addEventListener?.('change', () => {
    if (store.settings.theme === 'system') applyTheme();
  });
}

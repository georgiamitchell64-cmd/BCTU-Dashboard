/* ============================================================================
 * preload.js
 * ----------------------------------------------------------------------------
 * The only bridge between the renderer and Node. Everything exposed here is a
 * deliberate, narrow capability — no ipcRenderer, no fs, no require.
 * ========================================================================== */

const { contextBridge, ipcRenderer } = require('electron');

/** Menu items and main-process events the renderer is allowed to listen for. */
const CHANNELS = [
  'menu:new-task',
  'menu:quick-capture',
  'menu:navigate',
  'menu:undo',
  'menu:redo',
  'menu:search',
  'menu:palette',
  'menu:toggle-theme',
  'menu:shortcuts',
  'menu:export-csv',
  'data:replaced',
  'toast'
];

contextBridge.exposeInMainWorld('planner', {
  isDesktop: true,
  platform: process.platform,

  load: () => ipcRenderer.invoke('store:load'),
  save: (data) => ipcRenderer.invoke('store:save', data),
  paths: () => ipcRenderer.invoke('store:paths'),
  revealDataFolder: () => ipcRenderer.invoke('store:reveal'),

  saveText: (options) => ipcRenderer.invoke('file:save-text', options),
  confirm: (options) => ipcRenderer.invoke('dialog:confirm', options),

  on: (channel, handler) => {
    if (!CHANNELS.includes(channel)) return () => {};
    const listener = (_event, ...args) => handler(...args);
    ipcRenderer.on(channel, listener);
    return () => ipcRenderer.removeListener(channel, listener);
  }
});

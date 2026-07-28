'use strict';

// The only bridge between the renderer and Node. Each method is an explicit
// IPC call; the renderer gets no direct filesystem, shell or network access.

const { contextBridge, ipcRenderer } = require('electron');

const invoke = (channel, payload) => ipcRenderer.invoke(channel, payload);

contextBridge.exposeInMainWorld('api', {
  chooseWorkbook: () => invoke('workbook:choose'),
  openWorkbook: (filePath) => invoke('workbook:open', filePath),
  buildSites: (payload) => invoke('workbook:build', payload),
  commitImport: (payload) => invoke('workbook:commit', payload),

  loadState: () => invoke('state:load'),
  saveSites: (sites) => invoke('state:saveSites', sites),
  updateSettings: (patch) => invoke('settings:update', patch),
  saveTemplate: (template) => invoke('templates:save', template),
  deleteTemplate: (id) => invoke('templates:delete', id),

  planMessages: (payload) => invoke('compose:plan', payload),
  sendViaMailto: (payload) => invoke('send:mailto', payload),
  createDrafts: (payload) => invoke('send:drafts', payload),
  sendViaSmtp: (payload) => invoke('send:smtp', payload),
  setSmtpPassword: (password) => invoke('smtp:setPassword', password),
  verifySmtp: () => invoke('smtp:verify'),

  chooseAttachments: () => invoke('attachments:choose'),
  loadHtmlBody: () => invoke('body:loadHtml'),

  copyToClipboard: (text) => invoke('clipboard:write', text),
  exportCsv: (payload) => invoke('export:csv', payload),
  appInfo: () => invoke('app:info'),

  onImportRequested: (callback) => {
    ipcRenderer.on('menu:import', () => callback());
  },
  onSendProgress: (callback) => {
    ipcRenderer.on('send:progress', (_event, data) => callback(data));
  },
});

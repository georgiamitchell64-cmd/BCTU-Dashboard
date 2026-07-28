'use strict';

const path = require('path');
const fs = require('fs');
const os = require('os');
const { app, BrowserWindow, dialog, ipcMain, shell, safeStorage, Menu, clipboard } = require('electron');

const { readWorkbook } = require('./workbook');
const { Store } = require('./store');
const { detectMapping, buildSites, mergeSiteLists } = require('../shared/importer');
const { buildCombinedMessage, buildMergeQueue, BUILT_IN_FIELDS } = require('../shared/compose');
const { buildEml, buildMailto, draftFileName, toNodemailer } = require('../shared/mailer');

let mainWindow = null;
let store = null;

// The parsed workbook stays in the main process: the renderer only ever needs
// headers and a preview, so full sheets never cross the IPC boundary.
let loadedWorkbook = null;

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1280,
    height: 860,
    minWidth: 940,
    minHeight: 620,
    title: 'Site Contact Mailer',
    backgroundColor: '#f5f7fa',
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: false,
    },
  });

  mainWindow.loadFile(path.join(__dirname, '..', 'renderer', 'index.html'));
  mainWindow.on('closed', () => { mainWindow = null; });

  // Anything that is not the app's own page opens in the real browser.
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    if (/^https?:/i.test(url)) shell.openExternal(url);
    return { action: 'deny' };
  });
}

function buildMenu() {
  const isMac = process.platform === 'darwin';
  const template = [
    ...(isMac ? [{ role: 'appMenu' }] : []),
    {
      label: 'File',
      submenu: [
        {
          label: 'Import contact list…',
          accelerator: 'CmdOrCtrl+O',
          click: () => mainWindow && mainWindow.webContents.send('menu:import'),
        },
        { type: 'separator' },
        isMac ? { role: 'close' } : { role: 'quit' },
      ],
    },
    { role: 'editMenu' },
    {
      label: 'View',
      submenu: [{ role: 'reload' }, { role: 'toggleDevTools' }, { type: 'separator' },
        { role: 'resetZoom' }, { role: 'zoomIn' }, { role: 'zoomOut' }],
    },
  ];
  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

app.whenReady().then(() => {
  store = new Store(app.getPath('userData'), safeStorage);
  buildMenu();
  createWindow();

  app.on('activate', () => {
    if (BrowserWindow.getAllWindows().length === 0) createWindow();
  });
});

app.on('window-all-closed', () => {
  if (process.platform !== 'darwin') app.quit();
});

/** Wrap a handler so the renderer always gets `{ok}` rather than a raw throw. */
function handle(channel, fn) {
  ipcMain.handle(channel, async (event, ...args) => {
    try {
      return { ok: true, data: await fn(...args) };
    } catch (error) {
      return { ok: false, error: error.message || String(error), code: error.code || null };
    }
  });
}

// ── Import ────────────────────────────────────────────────────────────────

handle('workbook:choose', async () => {
  const result = await dialog.showOpenDialog(mainWindow, {
    title: 'Choose a contact list',
    properties: ['openFile'],
    filters: [
      { name: 'Spreadsheets', extensions: ['xlsx', 'xlsm', 'csv', 'tsv'] },
      { name: 'All files', extensions: ['*'] },
    ],
  });
  if (result.canceled || result.filePaths.length === 0) return null;
  return openWorkbook(result.filePaths[0]);
});

handle('workbook:open', async (filePath) => openWorkbook(filePath));

async function openWorkbook(filePath) {
  loadedWorkbook = await readWorkbook(filePath);
  return {
    filePath: loadedWorkbook.filePath,
    fileName: loadedWorkbook.fileName,
    sheets: loadedWorkbook.sheets.map((sheet) => ({
      name: sheet.name,
      headers: sheet.headers,
      rowCount: sheet.rowCount,
      mapping: detectMapping(sheet.headers),
      preview: sheet.rows.slice(0, 8),
    })),
  };
}

/** Re-run the import with whatever mapping the user has adjusted to. */
handle('workbook:build', async ({ sheetName, mapping }) => {
  if (!loadedWorkbook) throw new Error('No spreadsheet is open. Import a file first.');
  const sheet = loadedWorkbook.sheets.find((s) => s.name === sheetName);
  if (!sheet) throw new Error(`Sheet "${sheetName}" not found.`);
  return buildSites(sheet.rows, mapping, { firstDataRow: sheet.firstDataRow });
});

handle('workbook:commit', async ({ sites, strategy, meta, mapping }) => {
  const merged = mergeSiteLists(store.getSites(), sites, strategy || 'replace');
  store.setSites(merged, meta);
  if (mapping) store.setLastMapping(mapping);
  return merged;
});

// ── Stored state ──────────────────────────────────────────────────────────

handle('state:load', async () => ({
  settings: store.getSettings(),
  sites: store.getSites(),
  templates: store.getTemplates(),
  lastImport: store.state.lastImport,
  builtInFields: BUILT_IN_FIELDS,
  encryptionAvailable: Boolean(safeStorage && safeStorage.isEncryptionAvailable()),
  platform: process.platform,
}));

handle('state:saveSites', async (sites) => store.setSites(sites));
handle('settings:update', async (patch) => store.updateSettings(patch));
handle('templates:save', async (template) => store.saveTemplate(template));
handle('templates:delete', async (id) => store.deleteTemplate(id));
handle('smtp:setPassword', async (password) => store.setSmtpPassword(password));

// ── Preparing messages ────────────────────────────────────────────────────

/**
 * Build the messages a send would produce, without sending anything. The
 * renderer uses this for the preview and the confirmation counts, so what is
 * previewed is exactly what gets delivered.
 */
function planMessages({ sites, template, mode, options }) {
  const settings = store.getSettings();
  const opts = {
    senderAddress: settings.senderAddress,
    forceBcc: settings.forceBcc,
    alwaysBccSelfAddress: settings.putSelfInTo,
    ...options,
  };
  if (mode === 'merge') {
    return buildMergeQueue(sites, template, { ...opts, perContact: Boolean(opts.perContact) });
  }
  return [buildCombinedMessage(sites, template, opts)];
}

handle('compose:plan', async (payload) => {
  const messages = planMessages(payload);
  return messages.map((message) => ({
    ...message,
    mailto: buildMailto(message),
  }));
});

// ── Delivery ──────────────────────────────────────────────────────────────

handle('send:mailto', async (payload) => {
  const messages = planMessages(payload);
  const opened = [];
  for (const message of messages) {
    const mailto = buildMailto(message);
    if (mailto.tooLong) {
      throw new Error(
        `The message for ${message.siteName} is too long for a mailto link (${mailto.length} characters). Use "Create drafts" instead.`,
      );
    }
    await shell.openExternal(mailto.url);
    opened.push(message.siteName);
    // Opening many compose windows at once makes Outlook drop some of them.
    if (messages.length > 1) await new Promise((resolve) => setTimeout(resolve, 600));
  }
  return { opened: opened.length };
});

handle('send:drafts', async (payload) => {
  const messages = planMessages(payload);
  const settings = store.getSettings();
  const format = settings.bodyFormat === 'html' ? 'html' : 'plain';

  let folder = payload.folder || settings.draftFolder;
  if (!folder) {
    const result = await dialog.showOpenDialog(mainWindow, {
      title: 'Where should the draft files go?',
      properties: ['openDirectory', 'createDirectory'],
      defaultPath: path.join(app.getPath('documents')),
    });
    if (result.canceled || !result.filePaths.length) return { cancelled: true };
    folder = result.filePaths[0];
    store.updateSettings({ draftFolder: folder });
  }

  // Group each run in its own folder so repeated sends do not overwrite.
  const stamp = new Date().toISOString().replace(/[:.]/g, '-').slice(0, 16);
  const target = messages.length > 1 ? path.join(folder, `drafts ${stamp}`) : folder;
  fs.mkdirSync(target, { recursive: true });

  const written = [];
  messages.forEach((message, index) => {
    const file = path.join(target, draftFileName(message, index));
    fs.writeFileSync(file, buildEml(message, { format }), 'utf8');
    written.push(file);
  });

  // Opening dozens of drafts at once would swamp the mail client, so only a
  // single draft is opened automatically; otherwise reveal the folder.
  if (written.length === 1 && payload.openAfter !== false) {
    await shell.openPath(written[0]);
  } else if (payload.openAfter !== false) {
    shell.showItemInFolder(written[0]);
  }

  return { written: written.length, folder: target, files: written };
});

handle('send:smtp', async (payload) => {
  const settings = store.getSettings();
  const smtp = settings.smtp || {};
  if (!smtp.host) throw new Error('No SMTP server is configured. Add one in Settings, or use drafts instead.');

  const password = store.getSmtpPassword();
  if (smtp.user && !password) {
    throw new Error('No SMTP password is saved. Re-enter it in Settings.');
  }

  const nodemailer = require('nodemailer');
  const transport = nodemailer.createTransport({
    host: smtp.host,
    port: Number(smtp.port) || 587,
    secure: Boolean(smtp.secure),
    auth: smtp.user ? { user: smtp.user, pass: password } : undefined,
  });

  const messages = planMessages(payload);
  const format = settings.bodyFormat === 'html' ? 'html' : 'plain';
  const from = settings.senderAddress || smtp.user;
  const results = [];

  for (const message of messages) {
    try {
      await transport.sendMail(toNodemailer(message, { from, format }));
      results.push({ site: message.siteName, ok: true });
    } catch (error) {
      results.push({ site: message.siteName, ok: false, error: error.message });
    }
    if (mainWindow) {
      mainWindow.webContents.send('send:progress', {
        done: results.length,
        total: messages.length,
        site: message.siteName,
      });
    }
  }
  transport.close();
  return { results, sent: results.filter((r) => r.ok).length, failed: results.filter((r) => !r.ok).length };
});

handle('smtp:verify', async () => {
  const settings = store.getSettings();
  const smtp = settings.smtp || {};
  if (!smtp.host) throw new Error('Enter a server address first.');
  const nodemailer = require('nodemailer');
  const transport = nodemailer.createTransport({
    host: smtp.host,
    port: Number(smtp.port) || 587,
    secure: Boolean(smtp.secure),
    auth: smtp.user ? { user: smtp.user, pass: store.getSmtpPassword() } : undefined,
  });
  await transport.verify();
  transport.close();
  return { verified: true };
});

// ── Utilities ─────────────────────────────────────────────────────────────

handle('clipboard:write', async (text) => {
  clipboard.writeText(String(text || ''));
  return { copied: true };
});

handle('export:csv', async ({ rows, suggestedName }) => {
  const result = await dialog.showSaveDialog(mainWindow, {
    title: 'Export contact list',
    defaultPath: path.join(app.getPath('documents'), suggestedName || 'site-contacts.csv'),
    filters: [{ name: 'CSV', extensions: ['csv'] }],
  });
  if (result.canceled || !result.filePath) return { cancelled: true };

  const escape = (value) => {
    const text = String(value ?? '');
    return /[",\n]/.test(text) ? `"${text.replace(/"/g, '""')}"` : text;
  };
  const csv = rows.map((row) => row.map(escape).join(',')).join('\r\n');
  // BOM so Excel opens UTF-8 site names correctly.
  fs.writeFileSync(result.filePath, `﻿${csv}`, 'utf8');
  return { filePath: result.filePath };
});

handle('app:info', async () => ({
  version: app.getVersion(),
  platform: process.platform,
  userData: app.getPath('userData'),
  host: os.hostname(),
}));

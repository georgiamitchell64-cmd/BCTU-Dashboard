'use strict';

const path = require('path');
const fs = require('fs');
const os = require('os');
const { app, BrowserWindow, dialog, ipcMain, shell, safeStorage, Menu, clipboard } = require('electron');

const { readWorkbook } = require('./workbook');
const { Store } = require('./store');
const { detectMapping, buildSites, mergeSiteLists } = require('../shared/importer');
const { parseAddressCell } = require('../shared/emails');
const { sanitizeHtml, cleanPastedHtml } = require('../shared/html');
const {
  buildCombinedMessage, buildMergeQueue, BUILT_IN_FIELDS, RECRUITMENT_FIELDS,
  COMPLETENESS_FIELDS, QUALITY_FIELDS,
} = require('../shared/compose');
const { detectRecruitmentMapping, buildRecruitment, matchReport } = require('../shared/recruitment');
const {
  detectCompletenessMapping, buildCompleteness, completenessMatchReport,
} = require('../shared/completeness');
const { availableBuiltIns } = require('../shared/templates');
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
    // Sets the window and taskbar icon at runtime, independently of whatever
    // icon the .exe itself carries. See the icon section of the README.
    icon: path.join(__dirname, '..', 'assets', 'icon.png'),
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

// ── Randomisation data ────────────────────────────────────────────────────

// Kept separately from the contact workbook so importing recruitment figures
// never disturbs a contact-list import that is mid-flow.
let loadedRecruitmentWorkbook = null;

handle('recruitment:choose', async () => {
  const result = await dialog.showOpenDialog(mainWindow, {
    title: 'Choose a randomisation export',
    properties: ['openFile'],
    filters: [
      { name: 'Spreadsheets', extensions: ['xlsx', 'xlsm', 'csv', 'tsv'] },
      { name: 'All files', extensions: ['*'] },
    ],
  });
  if (result.canceled || result.filePaths.length === 0) return null;

  loadedRecruitmentWorkbook = await readWorkbook(result.filePaths[0]);
  return {
    filePath: loadedRecruitmentWorkbook.filePath,
    fileName: loadedRecruitmentWorkbook.fileName,
    sheets: loadedRecruitmentWorkbook.sheets.map((sheet) => ({
      name: sheet.name,
      headers: sheet.headers,
      rowCount: sheet.rowCount,
      mapping: detectRecruitmentMapping(sheet.headers),
      preview: sheet.rows.slice(0, 8),
    })),
  };
});

handle('recruitment:build', async ({ sheetName, mapping }) => {
  if (!loadedRecruitmentWorkbook) throw new Error('No randomisation file is open.');
  const sheet = loadedRecruitmentWorkbook.sheets.find((s) => s.name === sheetName);
  if (!sheet) throw new Error(`Sheet "${sheetName}" not found.`);
  const dataset = buildRecruitment(sheet.rows, mapping, { firstDataRow: sheet.firstDataRow });
  // Tell the user up front how much of their contact list this data covers.
  return { ...dataset, match: matchReport(dataset, store.getSites()) };
});

handle('recruitment:commit', async ({ dataset, meta }) => {
  const stored = store.setRecruitment(dataset, meta);
  return { recruitment: stored, match: matchReport(stored, store.getSites()) };
});

handle('recruitment:clear', async () => {
  store.clearRecruitment();
  return true;
});

// ── Data completeness ─────────────────────────────────────────────────────

let loadedCompletenessWorkbook = null;

handle('completeness:choose', async () => {
  const result = await dialog.showOpenDialog(mainWindow, {
    title: 'Choose a return-rates export',
    properties: ['openFile'],
    filters: [
      { name: 'Spreadsheets', extensions: ['xlsx', 'xlsm', 'csv', 'tsv'] },
      { name: 'All files', extensions: ['*'] },
    ],
  });
  if (result.canceled || result.filePaths.length === 0) return null;

  loadedCompletenessWorkbook = await readWorkbook(result.filePaths[0]);
  return {
    filePath: loadedCompletenessWorkbook.filePath,
    fileName: loadedCompletenessWorkbook.fileName,
    sheets: loadedCompletenessWorkbook.sheets.map((sheet) => ({
      name: sheet.name,
      headers: sheet.headers,
      rowCount: sheet.rowCount,
      mapping: detectCompletenessMapping(sheet.headers),
      preview: sheet.rows.slice(0, 8),
    })),
  };
});

handle('completeness:build', async ({ sheetName, mapping }) => {
  if (!loadedCompletenessWorkbook) throw new Error('No return-rates file is open.');
  const sheet = loadedCompletenessWorkbook.sheets.find((s) => s.name === sheetName);
  if (!sheet) throw new Error(`Sheet "${sheetName}" not found.`);
  const dataset = buildCompleteness(sheet.rows, mapping, { firstDataRow: sheet.firstDataRow });
  return { ...dataset, match: completenessMatchReport(dataset, store.getSites()) };
});

handle('completeness:commit', async ({ dataset, meta }) => {
  const stored = store.setCompleteness(dataset, meta);
  return { completeness: stored, match: completenessMatchReport(stored, store.getSites()) };
});

handle('completeness:clear', async (options) => {
  store.clearCompleteness(options || {});
  return true;
});

// ── Stored state ──────────────────────────────────────────────────────────

handle('state:load', async () => ({
  settings: store.getSettings(),
  sites: store.getSites(),
  templates: store.getTemplates(),
  drafts: store.getDrafts(),
  lastImport: store.state.lastImport,
  recruitment: store.getRecruitment(),
  recruitmentImport: store.state.recruitmentImport,
  completeness: store.getCompleteness(),
  completenessImport: store.state.completenessImport,
  builtInFields: BUILT_IN_FIELDS,
  recruitmentFields: RECRUITMENT_FIELDS,
  completenessFields: COMPLETENESS_FIELDS,
  qualityFields: QUALITY_FIELDS,
  builtInTemplates: availableBuiltIns({
    hasRecruitment: Boolean(store.getRecruitment() && store.getRecruitment().sites.length),
    hasCompleteness: Boolean(store.getCompleteness() && store.getCompleteness().sites.length),
  }),
  encryptionAvailable: Boolean(safeStorage && safeStorage.isEncryptionAvailable()),
  platform: process.platform,
}));

handle('state:saveSites', async (sites) => store.setSites(sites));
handle('settings:update', async (patch) => store.updateSettings(patch));
handle('templates:save', async (template) => store.saveTemplate(template));
handle('templates:delete', async (id) => store.deleteTemplate(id));
handle('drafts:save', async (draft) => store.saveDraft(draft));
handle('drafts:delete', async (id) => store.deleteDraft(id));
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
    // Charts and recruitment figures come from whatever was last imported.
    recruitment: store.getRecruitment(),
    anonymiseOtherSites: settings.anonymiseOtherSites !== false,
    completeness: store.getCompleteness(),
    completenessNaming: settings.completenessNaming || 'top3',
    ...options,
  };
  // "Plain text only" in Settings discards the formatting rather than
  // ignoring the setting — the text alternative is already generated from it.
  const effective = settings.bodyFormat === 'plain'
    ? { ...template, bodyHtml: '' }
    : template;

  const messages = mode === 'merge'
    ? buildMergeQueue(sites, effective, { ...opts, perContact: Boolean(opts.perContact) })
    : [buildCombinedMessage(sites, effective, opts)];

  // Cc and attachments are the same on every message in a send, so they are
  // applied here rather than threaded through the merge logic.
  const cc = parseAddressCell(opts.cc || '').contacts;
  const attachments = opts.attachments || [];
  return messages.map((message) => ({
    ...message,
    cc: [...(message.cc || []), ...cc],
    attachments,
  }));
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

// Anything much larger than this makes for a message most mail servers will
// bounce; Exchange commonly caps a message at 25-35 MB after encoding.
const MAX_ATTACHMENT_BYTES = 20 * 1024 * 1024;

handle('attachments:choose', async () => {
  const result = await dialog.showOpenDialog(mainWindow, {
    title: 'Attach files',
    properties: ['openFile', 'multiSelections'],
  });
  if (result.canceled || !result.filePaths.length) return [];

  return result.filePaths.map((filePath) => {
    const stats = fs.statSync(filePath);
    if (stats.size > MAX_ATTACHMENT_BYTES) {
      throw new Error(
        `"${path.basename(filePath)}" is ${(stats.size / 1024 / 1024).toFixed(1)} MB. `
        + 'Most mail systems reject anything over about 20 MB — send a link instead.',
      );
    }
    return {
      fileName: path.basename(filePath),
      contentType: contentTypeFor(filePath),
      size: stats.size,
      base64: fs.readFileSync(filePath).toString('base64'),
    };
  });
});

const CONTENT_TYPES = {
  '.pdf': 'application/pdf',
  '.doc': 'application/msword',
  '.docx': 'application/vnd.openxmlformats-officedocument.wordprocessingml.document',
  '.xls': 'application/vnd.ms-excel',
  '.xlsx': 'application/vnd.openxmlformats-officedocument.spreadsheetml.sheet',
  '.ppt': 'application/vnd.ms-powerpoint',
  '.pptx': 'application/vnd.openxmlformats-officedocument.presentationml.presentation',
  '.png': 'image/png',
  '.jpg': 'image/jpeg',
  '.jpeg': 'image/jpeg',
  '.gif': 'image/gif',
  '.txt': 'text/plain',
  '.csv': 'text/csv',
  '.zip': 'application/zip',
  '.rtf': 'application/rtf',
};

function contentTypeFor(filePath) {
  return CONTENT_TYPES[path.extname(filePath).toLowerCase()] || 'application/octet-stream';
}

/**
 * Load an .html or .eml file as the message body — the route for reusing a
 * newsletter or a message someone else drafted.
 */
handle('body:loadHtml', async () => {
  const result = await dialog.showOpenDialog(mainWindow, {
    title: 'Load a message',
    properties: ['openFile'],
    filters: [
      { name: 'HTML or email', extensions: ['html', 'htm', 'eml', 'txt'] },
      { name: 'All files', extensions: ['*'] },
    ],
  });
  if (result.canceled || !result.filePaths.length) return null;

  const filePath = result.filePaths[0];
  const raw = fs.readFileSync(filePath, 'utf8');
  const extension = path.extname(filePath).toLowerCase();

  let html = raw;
  if (extension === '.eml') {
    html = extractHtmlFromEml(raw);
    if (!html) throw new Error('No HTML body found in that .eml file.');
  } else if (extension === '.txt') {
    html = require('../shared/html').textToHtmlFragment(raw);
  }

  // Keep only what is inside <body>, so the app's own page is not affected.
  const bodyMatch = /<body\b[^>]*>([\s\S]*?)<\/body\s*>/i.exec(html);
  if (bodyMatch) html = bodyMatch[1];

  return { fileName: path.basename(filePath), html: sanitizeHtml(cleanPastedHtml(html)) };
});

/** Pull the text/html part out of a saved message, decoding its transfer encoding. */
function extractHtmlFromEml(raw) {
  const normalised = raw.replace(/\r\n/g, '\n');
  // Split into MIME parts if there is a boundary, otherwise treat as one part.
  const boundary = /boundary\s*=\s*"?([^";\n]+)"?/i.exec(normalised);
  const chunks = boundary
    ? normalised.split(new RegExp(`--${boundary[1].replace(/[.*+?^${}()|[\]\\]/g, '\\$&')}`))
    : [normalised];

  for (const chunk of chunks) {
    if (!/content-type\s*:\s*text\/html/i.test(chunk)) continue;
    const split = chunk.indexOf('\n\n');
    if (split === -1) continue;
    const headers = chunk.slice(0, split);
    let body = chunk.slice(split + 2);
    const encoding = /content-transfer-encoding\s*:\s*(\S+)/i.exec(headers);
    const scheme = encoding ? encoding[1].toLowerCase() : '7bit';
    if (scheme === 'base64') {
      body = Buffer.from(body.replace(/\s+/g, ''), 'base64').toString('utf8');
    } else if (scheme === 'quoted-printable') {
      body = body
        .replace(/=\n/g, '')
        .replace(/=([0-9A-F]{2})/gi, (_m, hex) => String.fromCharCode(parseInt(hex, 16)));
    }
    return body;
  }
  return null;
}

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

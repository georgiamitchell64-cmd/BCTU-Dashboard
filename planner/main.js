/* ============================================================================
 * main.js — Electron main process
 * ----------------------------------------------------------------------------
 * Owns the window, the application menu and the on-disk data file. The
 * renderer never touches the filesystem directly; everything goes through the
 * narrow IPC surface exposed in preload.js.
 *
 * Renderer assets are served over a custom `app://` scheme rather than
 * `file://` so that native ES modules load without tripping Chromium's
 * file-origin CORS rules.
 * ========================================================================== */

const { app, BrowserWindow, Menu, dialog, ipcMain, net, protocol, shell } = require('electron');
const fs = require('node:fs');
const fsp = require('node:fs/promises');
const path = require('node:path');
const { pathToFileURL } = require('node:url');

const RENDERER_ROOT = path.join(__dirname, 'src');
const IS_DEV = process.argv.includes('--dev');

let mainWindow = null;

/* ── Data file ────────────────────────────────────────────────────────────── */

const dataFile = () => path.join(app.getPath('userData'), 'planner-data.json');
const stateFile = () => path.join(app.getPath('userData'), 'window-state.json');

/** Write atomically so a crash mid-save can never truncate the real file. */
async function writeJSON(file, value) {
  const tmp = `${file}.${process.pid}.tmp`;
  await fsp.mkdir(path.dirname(file), { recursive: true });
  await fsp.writeFile(tmp, JSON.stringify(value, null, 2), 'utf8');
  await fsp.rename(tmp, file);
}

async function readJSON(file) {
  try {
    return JSON.parse(await fsp.readFile(file, 'utf8'));
  } catch (err) {
    if (err.code === 'ENOENT') return null;
    throw err;
  }
}

/**
 * Keep a rolling set of backups. Planning data is small and losing it is
 * annoying out of all proportion to the disk cost of keeping copies.
 */
async function backup() {
  const src = dataFile();
  if (!fs.existsSync(src)) return;
  const dir = path.join(app.getPath('userData'), 'backups');
  await fsp.mkdir(dir, { recursive: true });
  const stamp = new Date().toISOString().slice(0, 10);
  const dest = path.join(dir, `planner-${stamp}.json`);
  if (!fs.existsSync(dest)) await fsp.copyFile(src, dest);

  const keep = 14;
  const files = (await fsp.readdir(dir)).filter((f) => f.startsWith('planner-')).sort();
  await Promise.all(files.slice(0, Math.max(0, files.length - keep))
    .map((f) => fsp.unlink(path.join(dir, f)).catch(() => {})));
}

/* ── Window ───────────────────────────────────────────────────────────────── */

async function restoreBounds() {
  const saved = await readJSON(stateFile());
  const bounds = { width: 1360, height: 900, ...(saved?.bounds || {}) };
  return { bounds, maximized: Boolean(saved?.maximized) };
}

function persistBounds(win) {
  if (!win || win.isDestroyed()) return;
  const payload = { bounds: win.getNormalBounds(), maximized: win.isMaximized() };
  writeJSON(stateFile(), payload).catch(() => {});
}

async function createWindow() {
  const { bounds, maximized } = await restoreBounds();

  mainWindow = new BrowserWindow({
    ...bounds,
    minWidth: 1000,
    minHeight: 640,
    show: false,
    backgroundColor: '#EEF3F8',
    title: 'Planner',
    webPreferences: {
      preload: path.join(__dirname, 'preload.js'),
      contextIsolation: true,
      nodeIntegration: false,
      sandbox: true,
      spellcheck: true
    }
  });

  if (maximized) mainWindow.maximize();

  mainWindow.once('ready-to-show', () => mainWindow.show());
  mainWindow.on('resize', () => persistBounds(mainWindow));
  mainWindow.on('move', () => persistBounds(mainWindow));
  mainWindow.on('close', () => persistBounds(mainWindow));
  mainWindow.on('closed', () => { mainWindow = null; });

  // External links open in the real browser, never inside the app shell.
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    if (/^https?:$/.test(new URL(url).protocol)) shell.openExternal(url);
    return { action: 'deny' };
  });

  await mainWindow.loadURL('app://planner/index.html');
  if (IS_DEV) mainWindow.webContents.openDevTools({ mode: 'detach' });
}

/* ── Menu ─────────────────────────────────────────────────────────────────── */

const send = (channel, ...args) => mainWindow?.webContents.send(channel, ...args);

function buildMenu() {
  const isMac = process.platform === 'darwin';
  const template = [
    ...(isMac ? [{ role: 'appMenu' }] : []),
    {
      label: 'File',
      submenu: [
        { label: 'New Task…', accelerator: 'CmdOrCtrl+N', click: () => send('menu:new-task') },
        { label: 'Quick Capture', accelerator: 'CmdOrCtrl+Shift+N', click: () => send('menu:quick-capture') },
        { type: 'separator' },
        { label: 'Export Data…', click: () => exportData() },
        { label: 'Import Data…', click: () => importData() },
        { label: 'Export Tasks as CSV…', click: () => send('menu:export-csv') },
        { type: 'separator' },
        { label: 'Open Data Folder', click: () => shell.openPath(app.getPath('userData')) },
        { type: 'separator' },
        isMac ? { role: 'close' } : { role: 'quit' }
      ]
    },
    {
      label: 'Edit',
      submenu: [
        { label: 'Undo', accelerator: 'CmdOrCtrl+Z', click: () => send('menu:undo') },
        { label: 'Redo', accelerator: 'CmdOrCtrl+Shift+Z', click: () => send('menu:redo') },
        { type: 'separator' },
        { role: 'cut' }, { role: 'copy' }, { role: 'paste' }, { role: 'selectAll' },
        { type: 'separator' },
        { label: 'Find…', accelerator: 'CmdOrCtrl+F', click: () => send('menu:search') }
      ]
    },
    {
      label: 'View',
      submenu: [
        { label: 'Today', accelerator: 'CmdOrCtrl+1', click: () => send('menu:navigate', 'today') },
        { label: 'Week', accelerator: 'CmdOrCtrl+2', click: () => send('menu:navigate', 'week') },
        { label: 'Month', accelerator: 'CmdOrCtrl+3', click: () => send('menu:navigate', 'month') },
        { label: 'Priority Matrix', accelerator: 'CmdOrCtrl+4', click: () => send('menu:navigate', 'matrix') },
        { label: 'Trials', accelerator: 'CmdOrCtrl+5', click: () => send('menu:navigate', 'trials') },
        { label: 'Insights', accelerator: 'CmdOrCtrl+6', click: () => send('menu:navigate', 'insights') },
        { type: 'separator' },
        { label: 'Command Palette', accelerator: 'CmdOrCtrl+K', click: () => send('menu:palette') },
        { label: 'Toggle Theme', accelerator: 'CmdOrCtrl+Shift+L', click: () => send('menu:toggle-theme') },
        { label: 'Settings', accelerator: 'CmdOrCtrl+,', click: () => send('menu:navigate', 'settings') },
        { type: 'separator' },
        { role: 'resetZoom' }, { role: 'zoomIn' }, { role: 'zoomOut' },
        { type: 'separator' },
        { role: 'togglefullscreen' },
        { role: 'toggleDevTools' }
      ]
    },
    { role: 'windowMenu' },
    {
      role: 'help',
      submenu: [
        { label: 'Keyboard Shortcuts', accelerator: 'CmdOrCtrl+/', click: () => send('menu:shortcuts') },
        {
          label: 'About Planner',
          click: () => dialog.showMessageBox(mainWindow, {
            type: 'info',
            title: 'About Planner',
            message: `Planner ${app.getVersion()}`,
            detail: 'A desktop planner for daily, weekly and monthly trial work.\n\n' +
              `Data folder:\n${app.getPath('userData')}`
          })
        }
      ]
    }
  ];
  Menu.setApplicationMenu(Menu.buildFromTemplate(template));
}

/* ── Import / export ──────────────────────────────────────────────────────── */

async function exportData() {
  const { canceled, filePath } = await dialog.showSaveDialog(mainWindow, {
    title: 'Export planner data',
    defaultPath: `planner-backup-${new Date().toISOString().slice(0, 10)}.json`,
    filters: [{ name: 'JSON', extensions: ['json'] }]
  });
  if (canceled || !filePath) return;
  const data = (await readJSON(dataFile())) ?? {};
  await writeJSON(filePath, data);
  send('toast', { kind: 'success', message: `Exported to ${path.basename(filePath)}` });
}

async function importData() {
  const { canceled, filePaths } = await dialog.showOpenDialog(mainWindow, {
    title: 'Import planner data',
    properties: ['openFile'],
    filters: [{ name: 'JSON', extensions: ['json'] }]
  });
  if (canceled || !filePaths?.length) return;

  let incoming;
  try {
    incoming = await readJSON(filePaths[0]);
  } catch {
    return dialog.showErrorBox('Import failed', 'That file is not valid JSON.');
  }
  if (!incoming || typeof incoming !== 'object') {
    return dialog.showErrorBox('Import failed', 'That file does not look like planner data.');
  }

  const { response } = await dialog.showMessageBox(mainWindow, {
    type: 'warning',
    buttons: ['Replace everything', 'Cancel'],
    defaultId: 1,
    cancelId: 1,
    message: 'Replace your current planner data?',
    detail: 'Your current data is backed up first, but this cannot be undone from inside the app.'
  });
  if (response !== 0) return;

  await backup();
  await writeJSON(dataFile(), incoming);
  send('data:replaced', incoming);
}

/* ── IPC ──────────────────────────────────────────────────────────────────── */

ipcMain.handle('store:load', async () => readJSON(dataFile()));

ipcMain.handle('store:save', async (_event, data) => {
  await writeJSON(dataFile(), data);
  return true;
});

ipcMain.handle('store:paths', () => ({
  data: dataFile(),
  folder: app.getPath('userData'),
  version: app.getVersion()
}));

ipcMain.handle('store:reveal', () => shell.openPath(app.getPath('userData')));

ipcMain.handle('file:save-text', async (_event, { defaultPath, contents, filters }) => {
  const { canceled, filePath } = await dialog.showSaveDialog(mainWindow, { defaultPath, filters });
  if (canceled || !filePath) return null;
  await fsp.writeFile(filePath, contents, 'utf8');
  return filePath;
});

ipcMain.handle('dialog:confirm', async (_event, { message, detail, confirmLabel }) => {
  const { response } = await dialog.showMessageBox(mainWindow, {
    type: 'question',
    buttons: [confirmLabel || 'OK', 'Cancel'],
    defaultId: 1,
    cancelId: 1,
    message,
    detail
  });
  return response === 0;
});

/* ── Lifecycle ────────────────────────────────────────────────────────────── */

protocol.registerSchemesAsPrivileged([{
  scheme: 'app',
  privileges: { standard: true, secure: true, supportFetchAPI: true, stream: true }
}]);

if (!app.requestSingleInstanceLock()) {
  app.quit();
} else {
  app.on('second-instance', () => {
    if (!mainWindow) return;
    if (mainWindow.isMinimized()) mainWindow.restore();
    mainWindow.focus();
  });

  app.whenReady().then(async () => {
    protocol.handle('app', (request) => {
      const { pathname } = new URL(request.url);
      const relative = decodeURIComponent(pathname).replace(/^\/+/, '');
      const target = path.join(RENDERER_ROOT, relative);

      // Never serve anything outside src/, whatever the URL claims.
      if (!target.startsWith(RENDERER_ROOT + path.sep)) {
        return new Response('Forbidden', { status: 403 });
      }
      return net.fetch(pathToFileURL(target).toString());
    });

    buildMenu();
    await backup();
    await createWindow();

    app.on('activate', () => {
      if (BrowserWindow.getAllWindows().length === 0) createWindow();
    });
  });

  app.on('window-all-closed', () => {
    if (process.platform !== 'darwin') app.quit();
  });
}

'use strict';
// =============================================================================
// BCTU Clinical Trials Dashboard — desktop app
// =============================================================================
// Starts the Shiny app with the R bundled beside it and shows it in a window.
// Everything with logic in it lives in lib/, so it can be tested without
// Electron; this file is the wiring.
//
// The app writes to the per-user data folder (BCTU_DATA_DIR) because the
// install folder is read-only, and finds pandoc and Chrome in the runtime
// folder shipped with it (BCTU_RUNTIME_DIR). Both are read by the R side —
// globals/paths.R and globals/runtime.R.
// =============================================================================

const { app, BrowserWindow, dialog, shell } = require('electron');
const path = require('path');
const fs = require('fs');

const { resolvePaths, appEnv } = require('./lib/locate');
const { freePort, startR, waitForServer, stopR } = require('./lib/server');

let win = null;
let rProc = null;
let logStream = null;
const recentLog = [];          // the tail, for the error dialog

function log(line) {
  recentLog.push(line);
  if (recentLog.length > 40) recentLog.shift();
  if (logStream) { try { logStream.write(line + '\n'); } catch (_) {} }
}

function paths() {
  return resolvePaths({
    appPath: app.getAppPath(),
    resourcesPath: process.resourcesPath,
    isPackaged: app.isPackaged,
    userData: app.getPath('userData'),
    platform: process.platform,
    env: process.env,
    exists: fs.existsSync,
    listDir: fs.readdirSync,
  });
}

function createWindow() {
  win = new BrowserWindow({
    width: 1500,
    height: 950,
    show: true,
    backgroundColor: '#F4F4F4',
    title: 'BCTU Clinical Trials Dashboard',
    icon: path.join(__dirname, 'build', process.platform === 'win32' ? 'icon.ico' : 'icon.png'),
    webPreferences: { nodeIntegration: false, contextIsolation: true },
  });
  win.setMenuBarVisibility(false);
  win.loadFile(path.join(__dirname, 'loading.html'));

  // The dashboard opens documentation and NIHR/ISRCTN links; those belong in
  // the real browser, not in a dashboard window with no address bar.
  win.webContents.setWindowOpenHandler(({ url }) => {
    shell.openExternal(url);
    return { action: 'deny' };
  });
  win.on('closed', () => { win = null; });
}

function fail(title, detail) {
  const p = paths();
  const body = detail + (recentLog.length ? '\n\nLast output from R:\n' + recentLog.slice(-15).join('\n') : '');
  if (win && !win.isDestroyed()) win.destroy();
  dialog.showErrorBox(title, body + `\n\nFull log: ${p.logFile}`);
  app.exit(1);
}

async function start() {
  const p = paths();

  fs.mkdirSync(p.dataDir, { recursive: true });
  try {
    logStream = fs.createWriteStream(p.logFile, { flags: 'a' });
    log(`\n=== ${new Date().toISOString()} — starting ===`);
    log(`app      ${p.appDir}`);
    log(`runtime  ${p.runtimeDir}`);
    log(`data     ${p.dataDir}`);
    log(`Rscript  ${p.rscript}`);
  } catch (_) { /* a log we can't write is not a reason not to start */ }

  if (!p.rscript) {
    return fail('R could not be found',
      'The dashboard needs R, and neither a bundled copy nor an installed one was found.\n\n' +
      'Install R from https://cran.r-project.org and start the dashboard again.\n\n' +
      'Looked in:\n' + p.rscriptTried.join('\n'));
  }
  if (!fs.existsSync(path.join(p.appDir, 'app.R'))) {
    return fail('The dashboard files are missing',
      `No app.R in:\n${p.appDir}\n\nThe installation looks incomplete — reinstall the dashboard.`);
  }

  const port = await freePort();
  log(`port     ${port}`);

  rProc = startR({
    rscript: p.rscript,
    appDir: p.appDir,
    port,
    env: appEnv(process.env, p),
    onLog: log,
  });
  rProc.on('error', (e) => fail('R could not be started', String(e && e.message)));

  let exited = false;
  rProc.on('exit', (code) => {
    exited = true;
    log(`R exited with code ${code}`);
    // Only a problem if it happened before we had a window on it; afterwards
    // it's just the app being closed.
    if (win && win.webContents.getURL().startsWith('file://')) {
      fail('The dashboard stopped while starting up',
        `R exited with code ${code} before the dashboard was ready.`);
    }
  });

  try {
    const ms = await waitForServer(port, { hasExited: () => exited });
    log(`ready in ${ms}ms`);
    if (win && !win.isDestroyed()) win.loadURL(`http://127.0.0.1:${port}/`);
  } catch (e) {
    fail('The dashboard did not start', String(e && e.message));
  }
}

// A second launch focuses the window that is already open rather than
// starting a second R alongside the first.
if (!app.requestSingleInstanceLock()) {
  app.quit();
} else {
  app.on('second-instance', () => {
    if (win) { if (win.isMinimized()) win.restore(); win.focus(); }
  });

  app.whenReady().then(() => {
    createWindow();
    start();
    app.on('activate', () => {
      if (BrowserWindow.getAllWindows().length === 0) { createWindow(); start(); }
    });
  });

  app.on('window-all-closed', () => app.quit());
  // Closing the window stops R — it has no other owner, and leaving it running
  // would hold both the port and the databases.
  app.on('before-quit', () => { stopR(rProc); });
  app.on('quit', () => { stopR(rProc); if (logStream) logStream.end(); });
}

// =============================================================================
// BCTU Clinical Trials Dashboard — Electron main process
// =============================================================================
// Boots a bundled R + Shiny server as a child process, waits for it to start
// listening, then points a BrowserWindow at it.
//
// The important part is BCTU_DATA_ROOT. The R code writes every database,
// trial config and override through that path (see globals/app_paths.R).
// It is set to Electron's userData directory, which is per-user, writable,
// and — critically — NOT replaced when the app updates. Writing inside the
// app bundle instead would lose all data on every update.
// =============================================================================

const { app, BrowserWindow, shell, dialog, Menu } = require('electron');
const { spawn } = require('child_process');
const path = require('path');
const fs = require('fs');
const net = require('net');

const isDev = !app.isPackaged;

// Loopback port range. Shiny is bound to 127.0.0.1 only — never externally.
const PORT_MIN = 7100;
const PORT_MAX = 7999;

// R has to unpack and load ~28 packages on a cold start, which on a slow
// managed laptop can genuinely take a while. Fail slower rather than
// wrongly telling the user it is broken.
const START_TIMEOUT_MS = 180000;

let rProcess = null;
let mainWindow = null;
let shinyPort = null;
let rLog = [];              // tail of R output, shown if startup fails
let startupFailed = false;

// ── Paths ────────────────────────────────────────────────────────────────────

/** Root containing the R application source (read-only once packaged). */
function appRoot() {
  return isDev
    ? path.join(__dirname, '..', 'BCTU_Clinical_Trials_Dashboard')
    : path.join(process.resourcesPath, 'app');
}

/** Bundled R executable. R ships inside the installer; the user needs none. */
function rExecutable() {
  const base = isDev
    ? path.join(__dirname, 'runtime', 'R')
    : path.join(process.resourcesPath, 'R');
  const exe = path.join(base, 'bin', 'x64', 'Rscript.exe');
  return fs.existsSync(exe) ? exe : path.join(base, 'bin', 'Rscript.exe');
}

/** Writable, update-surviving data root. Also where we keep the log. */
function dataRoot() {
  const d = app.getPath('userData');
  fs.mkdirSync(d, { recursive: true });
  return d;
}

function logFile() {
  return path.join(dataRoot(), 'startup.log');
}

// ── Remembered window size/position ──────────────────────────────────────────
// Small nicety, but the app feels broken without it: reopening should give you
// the window where you left it, not a default rectangle every time.

function windowStateFile() {
  return path.join(dataRoot(), 'window-state.json');
}

function readWindowState() {
  try {
    const s = JSON.parse(fs.readFileSync(windowStateFile(), 'utf8'));
    if (Number.isFinite(s.width) && Number.isFinite(s.height)) return s;
  } catch (_) { /* first run, or unreadable — fall through to defaults */ }
  return { width: 1600, height: 1000, maximised: true };
}

function saveWindowState() {
  if (!mainWindow || mainWindow.isDestroyed()) return;
  try {
    const b = mainWindow.getNormalBounds();
    fs.writeFileSync(windowStateFile(), JSON.stringify({
      x: b.x, y: b.y, width: b.width, height: b.height,
      maximised: mainWindow.isMaximized(),
    }));
  } catch (_) { /* never block quitting on this */ }
}

// ── Splash messaging ─────────────────────────────────────────────────────────

function setStatus(text) {
  if (!mainWindow || mainWindow.isDestroyed()) return;
  mainWindow.webContents.executeJavaScript(
    `window.postMessage(${JSON.stringify({ status: text })}, '*')`
  ).catch(() => { /* splash already replaced by the app */ });
}

function showFailure(message) {
  startupFailed = true;
  const tail = rLog.slice(-40).join('');
  const detail = `${message}\n\n${tail}`.trim();
  try { fs.writeFileSync(logFile(), detail); } catch (_) {}
  if (!mainWindow || mainWindow.isDestroyed()) return;
  mainWindow.webContents.executeJavaScript(
    `window.postMessage(${JSON.stringify({ error: detail })}, '*')`
  ).catch(() => {
    dialog.showErrorBox('Could not start', detail);
  });
}

// ── Port discovery ───────────────────────────────────────────────────────────

function isPortFree(port) {
  return new Promise((resolve) => {
    const srv = net.createServer();
    srv.once('error', () => resolve(false));
    srv.once('listening', () => srv.close(() => resolve(true)));
    srv.listen(port, '127.0.0.1');
  });
}

async function findFreePort() {
  for (let p = PORT_MIN; p <= PORT_MAX; p++) {
    if (await isPortFree(p)) return p;
  }
  throw new Error('No free port available in range 7100-7999.');
}

/** Poll until Shiny actually accepts connections. */
function waitForServer(port, timeoutMs = START_TIMEOUT_MS) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    (function attempt() {
      if (startupFailed) return reject(new Error('R exited during startup.'));
      const sock = net.connect(port, '127.0.0.1');
      sock.once('connect', () => { sock.destroy(); resolve(); });
      sock.once('error', () => {
        sock.destroy();
        const waited = Date.now() - started;
        if (waited > timeoutMs) {
          reject(new Error(`Timed out after ${Math.round(timeoutMs / 1000)}s waiting for the R server.`));
        } else {
          if (waited > 6000)  setStatus('Loading R packages…');
          if (waited > 20000) setStatus('Still starting — first run takes longest…');
          setTimeout(attempt, 300);
        }
      });
    })();
  });
}

// ── R process ────────────────────────────────────────────────────────────────

function startR(port) {
  const root = appRoot();
  const rscript = rExecutable();

  if (!fs.existsSync(rscript)) {
    throw new Error(
      `Bundled R not found at:\n${rscript}\n\n` +
      'The installer appears to be incomplete — reinstalling should fix it.'
    );
  }

  const expr = `
    setwd(${JSON.stringify(root)});
    options(shiny.port = ${port}, shiny.host = "127.0.0.1", shiny.launch.browser = FALSE);
    shiny::runApp(".", port = ${port}, host = "127.0.0.1", launch.browser = FALSE);
  `.trim();

  // rmarkdown shells out to pandoc to render the TMG/TSC reports. Pandoc
  // normally arrives with RStudio, which a packaged app does not have, so it is
  // bundled and pointed at explicitly — otherwise report export fails on a
  // clean machine with a confusing "pandoc not found" error.
  const pandocDir = isDev
    ? path.join(__dirname, 'runtime', 'pandoc')
    : path.join(process.resourcesPath, 'pandoc');

  const child = spawn(rscript, ['-e', expr], {
    cwd: root,
    env: {
      ...process.env,
      BCTU_DATA_ROOT: dataRoot(),
      R_LIBS_USER: path.join(path.dirname(path.dirname(rscript)), 'library'),
      RSTUDIO_PANDOC: pandocDir,
      PATH: `${pandocDir}${path.delimiter}${process.env.PATH || ''}`,
    },
    windowsHide: true,
  });

  const capture = (d) => {
    const s = String(d);
    rLog.push(s);
    if (rLog.length > 200) rLog.shift();
    process.stdout.write(`[R] ${s}`);
  };
  child.stdout.on('data', capture);
  child.stderr.on('data', capture);

  child.on('error', (err) => showFailure(`Could not launch R: ${err.message}`));
  child.on('exit', (code) => {
    if (code !== 0 && code !== null && !startupFailed) {
      showFailure(`The R engine stopped unexpectedly (exit code ${code}).`);
    }
  });

  return child;
}

// ── Menu ─────────────────────────────────────────────────────────────────────
// The previous build removed the menu entirely, which also removed copy/paste
// and zoom accelerators. This keeps the app looking clean (no File/Edit clutter)
// while restoring the shortcuts people expect, plus a route to the data folder
// for backups.

function buildMenu() {
  Menu.setApplicationMenu(Menu.buildFromTemplate([
    {
      label: 'View',
      submenu: [
        { role: 'reload', label: 'Reload dashboard' },
        { type: 'separator' },
        { role: 'resetZoom', label: 'Actual size' },
        { role: 'zoomIn',    label: 'Zoom in' },
        { role: 'zoomOut',   label: 'Zoom out' },
        { type: 'separator' },
        { role: 'togglefullscreen' },
      ],
    },
    {
      label: 'Edit',
      submenu: [
        { role: 'cut' }, { role: 'copy' }, { role: 'paste' },
        { role: 'selectAll' },
      ],
    },
    {
      label: 'Help',
      submenu: [
        {
          label: 'Open data folder',
          click: () => shell.openPath(dataRoot()),
        },
        {
          label: 'Open startup log',
          click: () => {
            if (fs.existsSync(logFile())) shell.openPath(logFile());
            else dialog.showMessageBox(mainWindow, {
              message: 'No startup problems have been logged.',
              buttons: ['OK'], type: 'info',
            });
          },
        },
        { type: 'separator' },
        {
          label: 'About',
          click: () => dialog.showMessageBox(mainWindow, {
            type: 'info',
            title: 'About',
            message: 'BCTU Clinical Trials Dashboard',
            detail:
              `Version ${app.getVersion()}\n\n` +
              'Birmingham Clinical Trials Unit\n\n' +
              `Your data is stored in:\n${dataRoot()}\n\n` +
              'That folder survives app updates. Copy it to back up.',
            buttons: ['OK'],
          }),
        },
      ],
    },
  ]));
}

// ── Window ───────────────────────────────────────────────────────────────────

function createWindow() {
  const st = readWindowState();

  mainWindow = new BrowserWindow({
    x: st.x, y: st.y,
    width: st.width, height: st.height,
    minWidth: 1100, minHeight: 700,
    show: false,
    backgroundColor: '#EEF3F8',       // avoids a white flash before paint
    title: 'BCTU Clinical Trials Dashboard',
    webPreferences: { nodeIntegration: false, contextIsolation: true },
  });

  if (st.maximised) mainWindow.maximize();
  buildMenu();

  // Show the splash immediately so there is never a blank window.
  mainWindow.loadFile(path.join(__dirname, 'splash.html'));
  mainWindow.once('ready-to-show', () => mainWindow.show());

  // External links open in the real browser, not inside the app shell.
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    shell.openExternal(url);
    return { action: 'deny' };
  });

  mainWindow.on('close', saveWindowState);
  mainWindow.on('closed', () => { mainWindow = null; });
}

// ── Lifecycle ────────────────────────────────────────────────────────────────

app.whenReady().then(async () => {
  createWindow();
  try {
    setStatus('Finding a free port…');
    shinyPort = await findFreePort();

    setStatus('Starting the R engine…');
    rProcess = startR(shinyPort);

    await waitForServer(shinyPort);

    setStatus('Almost ready…');
    await mainWindow.loadURL(`http://127.0.0.1:${shinyPort}`);
  } catch (err) {
    showFailure(String(err.message || err));
  }
});

/** Make sure R never outlives the window. */
function stopR() {
  if (!rProcess || rProcess.killed) return;
  try {
    if (process.platform === 'win32') {
      // Shiny spawns child processes; /t kills the whole tree.
      spawn('taskkill', ['/pid', String(rProcess.pid), '/f', '/t']);
    } else {
      rProcess.kill('SIGTERM');
    }
  } catch (_) { /* already gone */ }
  rProcess = null;
}

app.on('window-all-closed', () => { stopR(); app.quit(); });
app.on('before-quit', () => { saveWindowState(); stopR(); });
process.on('exit', stopR);

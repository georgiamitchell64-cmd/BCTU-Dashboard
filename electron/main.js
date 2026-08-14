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

// Port range to search for a free port. Shiny is bound to loopback only.
const PORT_MIN = 7100;
const PORT_MAX = 7999;

let rProcess = null;
let mainWindow = null;
let shinyPort = null;

// ── Paths ────────────────────────────────────────────────────────────────────

/** Root containing the R application source (read-only once packaged). */
function appRoot() {
  return isDev
    ? path.join(__dirname, '..', 'BCTU_Clinical_Trials_Dashboard')
    : path.join(process.resourcesPath, 'app');
}

/** Bundled R executable. R is shipped inside the installer. */
function rExecutable() {
  const base = isDev
    ? path.join(__dirname, 'runtime', 'R')
    : path.join(process.resourcesPath, 'R');
  const exe = path.join(base, 'bin', 'x64', 'Rscript.exe');
  return fs.existsSync(exe) ? exe : path.join(base, 'bin', 'Rscript.exe');
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
  throw new Error('No free port available in range.');
}

/** Poll until Shiny is actually accepting connections. */
function waitForServer(port, timeoutMs = 120000) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    (function attempt() {
      const sock = net.connect(port, '127.0.0.1');
      sock.once('connect', () => { sock.destroy(); resolve(); });
      sock.once('error', () => {
        sock.destroy();
        if (Date.now() - started > timeoutMs) {
          reject(new Error('Timed out waiting for the R server to start.'));
        } else {
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
      'Run electron/scripts/fetch-r.ps1 to download the R runtime before building.'
    );
  }

  // Writable, update-surviving location for all user data.
  const dataRoot = app.getPath('userData');
  fs.mkdirSync(dataRoot, { recursive: true });

  const expr = `
    setwd(${JSON.stringify(root)});
    options(shiny.port = ${port}, shiny.host = "127.0.0.1", shiny.launch.browser = FALSE);
    shiny::runApp(".", port = ${port}, host = "127.0.0.1", launch.browser = FALSE);
  `.trim();

  const child = spawn(rscript, ['-e', expr], {
    cwd: root,
    env: {
      ...process.env,
      BCTU_DATA_ROOT: dataRoot,
      R_LIBS_USER: path.join(path.dirname(path.dirname(rscript)), 'library'),
    },
    windowsHide: true,
  });

  child.stdout.on('data', (d) => process.stdout.write(`[R] ${d}`));
  child.stderr.on('data', (d) => process.stderr.write(`[R] ${d}`));
  child.on('exit', (code) => {
    if (code !== 0 && code !== null && mainWindow) {
      dialog.showErrorBox('Dashboard stopped', `The R server exited with code ${code}.`);
    }
  });

  return child;
}

// ── Window ───────────────────────────────────────────────────────────────────

function createWindow() {
  mainWindow = new BrowserWindow({
    width: 1600,
    height: 1000,
    show: false,
    title: 'BCTU Clinical Trials Dashboard',
    webPreferences: { nodeIntegration: false, contextIsolation: true },
  });

  Menu.setApplicationMenu(null);

  // Open external links in the real browser, not inside the app shell.
  mainWindow.webContents.setWindowOpenHandler(({ url }) => {
    shell.openExternal(url);
    return { action: 'deny' };
  });

  mainWindow.once('ready-to-show', () => mainWindow.show());
  mainWindow.on('closed', () => { mainWindow = null; });
}

// ── Lifecycle ────────────────────────────────────────────────────────────────

app.whenReady().then(async () => {
  createWindow();
  try {
    shinyPort = await findFreePort();
    rProcess = startR(shinyPort);
    await waitForServer(shinyPort);
    mainWindow.loadURL(`http://127.0.0.1:${shinyPort}`);
  } catch (err) {
    dialog.showErrorBox('Failed to start', String(err.message || err));
    app.quit();
  }
});

/** Make sure R never outlives the window. */
function stopR() {
  if (!rProcess || rProcess.killed) return;
  try {
    if (process.platform === 'win32') {
      spawn('taskkill', ['/pid', String(rProcess.pid), '/f', '/t']);
    } else {
      rProcess.kill('SIGTERM');
    }
  } catch (_) { /* already gone */ }
  rProcess = null;
}

app.on('window-all-closed', () => { stopR(); app.quit(); });
app.on('before-quit', stopR);
process.on('exit', stopR);

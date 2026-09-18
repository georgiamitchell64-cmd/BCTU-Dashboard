'use strict';
// =============================================================================
// Starting the Shiny app, and knowing when it's ready
// =============================================================================
// Electron can't show the dashboard until R is serving it, and R takes a few
// seconds to load its packages. So: take a port the OS says is free, start
// Rscript on it, poll until something answers, then point the window at it.
//
// A fixed port would collide with a second copy, with the browser shortcut,
// and with whatever else on the machine fancies 3838 — asking for a free one
// costs nothing and removes the whole class of problem.
//
// No Electron in here, so it can be tested against a real R process — see
// test/server.test.js.
// =============================================================================

const net = require('net');
const http = require('http');
const { spawn } = require('child_process');

/** A port nothing is listening on, from the OS rather than a guess. */
function freePort() {
  return new Promise((resolve, reject) => {
    const srv = net.createServer();
    srv.unref();
    srv.on('error', reject);
    srv.listen(0, '127.0.0.1', () => {
      const { port } = srv.address();
      srv.close(() => resolve(port));
    });
  });
}

// Shiny prints this once the server is accepting connections. It is the
// authoritative ready signal: everything after it is rendering, which can take
// a while the first time and must not be mistaken for a failure to start.
const LISTENING = /Listening on\s+https?:\/\//i;

/**
 * Start the Shiny app.
 *
 * Returns the child process. Its stdout and stderr go to onLog line by line —
 * main.js writes them to the log file, and shows the tail of them if R dies
 * before it ever serves anything, which is the only way to tell a missing
 * package from a syntax error.
 *
 * onListening fires when Shiny says it is up.
 */
function startR({ rscript, appDir, port, env, onLog = () => {}, onListening = () => {} }) {
  const expr =
    `shiny::runApp(${JSON.stringify(appDir)}, host = "127.0.0.1", ` +
    `port = ${port}, launch.browser = FALSE)`;
  // --vanilla: no .Rprofile, no .Renviron, no saved workspace. The dashboard
  // must behave the same on every machine, and a person who uses R already has
  // a personal R setup that would otherwise apply here. A .Renviron in
  // particular is applied *after* the environment we pass, so a stale
  // BCTU_DATA_DIR in one would silently redirect where the dashboard writes.
  const child = spawn(rscript, ['--vanilla', '-e', expr], {
    cwd: appDir,
    env,
    windowsHide: true,
  });
  let announced = false;
  const pipe = (stream) => {
    let buf = '';
    stream.setEncoding('utf8');
    stream.on('data', (chunk) => {
      buf += chunk;
      const lines = buf.split(/\r?\n/);
      buf = lines.pop();
      for (const l of lines) {
        if (!l.trim()) continue;
        onLog(l);
        if (!announced && LISTENING.test(l)) { announced = true; onListening(); }
      }
    });
  };
  pipe(child.stdout);
  pipe(child.stderr);
  return child;
}

/**
 * Does something answer on this port yet?
 *
 * The timeout is generous because the first request is the one that renders the
 * whole UI, and a slow render is not a dead server. A short timeout here also
 * means every probe leaves Shiny rendering a page nobody will read — poll fast
 * enough and the probes are the reason it never finishes.
 */
function ping(port, timeoutMs = 15000) {
  return new Promise((resolve) => {
    const req = http.get(
      { host: '127.0.0.1', port, path: '/', timeout: timeoutMs },
      (res) => { res.resume(); resolve(true); }
    );
    req.on('timeout', () => { req.destroy(); resolve(false); });
    req.on('error', () => resolve(false));
  });
}

/**
 * Wait until the app is up.
 *
 * `isListening` — Shiny's own "Listening on" line — settles it: the server is
 * accepting connections, and how long the first page then takes to render is
 * the window's problem, not ours. Waiting for a *complete response* instead is
 * what broke this: bslib fetches a Google font while rendering, and on a slow
 * or filtered network that first render outlasted any sane timeout, so a
 * dashboard that had started fine was reported as having failed to start.
 *
 * The HTTP poll stays as the fallback for a Shiny that word it differently,
 * and one probe at a time — they cost a full render each.
 *
 * Rejects if nothing happens within timeoutMs, and — the case that actually
 * happens — as soon as R exits, since then no amount of waiting will help.
 */
function waitForServer(port, {
  timeoutMs = 120000,
  intervalMs = 1000,
  hasExited = () => false,
  isListening = () => false,
} = {}) {
  const started = Date.now();
  return new Promise((resolve, reject) => {
    const tick = async () => {
      if (isListening()) return resolve(Date.now() - started);
      if (hasExited())
        return reject(new Error('R stopped before the dashboard was ready'));
      if (await ping(port, 4000)) return resolve(Date.now() - started);
      if (isListening()) return resolve(Date.now() - started);
      if (hasExited())
        return reject(new Error('R stopped before the dashboard was ready'));
      if (Date.now() - started > timeoutMs)
        return reject(new Error(`the dashboard did not start within ${Math.round(timeoutMs / 1000)}s`));
      setTimeout(tick, intervalMs);
    };
    tick();
  });
}

/**
 * Stop R. SIGTERM first so Shiny can close its database connections, then
 * SIGKILL for anything still up after graceMs. On Windows, where there is no
 * real SIGTERM, taskkill takes the process tree — otherwise the R child
 * outlives the window and keeps the port.
 */
function stopR(child, { graceMs = 3000, platform = process.platform } = {}) {
  if (!child || child.exitCode !== null || child.killed) return;
  if (platform === 'win32') {
    try {
      spawn('taskkill', ['/pid', String(child.pid), '/f', '/t'], { windowsHide: true });
      return;
    } catch (_) { /* fall through to the signal */ }
  }
  try { child.kill('SIGTERM'); } catch (_) {}
  setTimeout(() => {
    if (child.exitCode === null && !child.killed) {
      try { child.kill('SIGKILL'); } catch (_) {}
    }
  }, graceMs).unref();
}

module.exports = { freePort, startR, ping, waitForServer, stopR };

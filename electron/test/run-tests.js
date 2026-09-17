'use strict';
// =============================================================================
// Tests for the launcher logic
// =============================================================================
// Plain node, no test framework and no Electron: `npm test` has to run on a
// machine that hasn't done `npm install` yet, and in CI before the Electron
// binary is downloaded.
//
// lib/locate.js is pure, so it's tested by feeding it each layout. lib/server.js
// isn't, so it's tested against a real Rscript serving a real (one-line) Shiny
// app — that round trip is the part the desktop build actually depends on, and
// the part that a mock would prove nothing about. Those tests skip themselves
// when there's no R on the machine.
// =============================================================================

const path = require('path');
const fs = require('fs');
const os = require('os');
const { execFileSync } = require('child_process');

const { resolvePaths, rscriptCandidates, appEnv } = require('../lib/locate');
const { freePort, startR, ping, waitForServer, stopR } = require('../lib/server');

let passed = 0, failed = 0, skipped = 0;

function ok(cond, label) {
  if (cond) { passed++; }
  else { failed++; console.log(`  FAIL  ${label}`); }
}
function eq(actual, expected, label) {
  const same = actual === expected;
  if (!same) console.log(`        got ${JSON.stringify(actual)}, wanted ${JSON.stringify(expected)}`);
  ok(same, label);
}
async function section(name, fn) {
  console.log(`\n${name}`);
  try { await fn(); } catch (e) { failed++; console.log(`  FAIL  threw: ${e && e.stack}`); }
}

// -- lib/locate --------------------------------------------------------------

// A fake filesystem: exists() is true for anything in the set, listDir() reads
// the folder names out of it.
function fakeFs(present) {
  const set = new Set(present.map((p) => p.replace(/\\/g, '/')));
  return {
    exists: (p) => set.has(p.replace(/\\/g, '/')),
    listDir: (dir) => {
      const d = dir.replace(/\\/g, '/').replace(/\/$/, '') + '/';
      const names = new Set();
      for (const p of set) {
        if (p.startsWith(d)) names.add(p.slice(d.length).split('/')[0]);
      }
      if (!names.size) throw new Error('ENOENT');
      return [...names];
    },
  };
}

async function testLocate() {
  await section('locate: packaged layout', () => {
    const res = '/opt/BCTU/resources';
    const fsx = fakeFs([`${res}/runtime/R/bin/Rscript`]);
    const p = resolvePaths({
      appPath: `${res}/app.asar`, resourcesPath: res, isPackaged: true,
      userData: '/home/g/.config/BCTU', platform: 'linux', env: {}, ...fsx,
    });
    eq(p.appDir, '/opt/BCTU/resources/app', 'app comes from resources/app');
    eq(p.runtimeDir, '/opt/BCTU/resources/runtime', 'runtime comes from resources/runtime');
    eq(p.dataDir, '/home/g/.config/BCTU', 'data is the per-user folder');
    eq(p.logFile, '/home/g/.config/BCTU/dashboard.log', 'log sits in the data folder');
    eq(p.rscript, `${res}/runtime/R/bin/Rscript`, 'finds the bundled R');
  });

  await section('locate: development layout', () => {
    const el = '/home/g/BCTU-Dashboard/electron';
    const p = resolvePaths({
      appPath: el, resourcesPath: '/unused', isPackaged: false,
      userData: '/home/g/.config/BCTU', platform: 'linux', env: {},
      ...fakeFs(['/usr/bin/Rscript']),
    });
    eq(p.appDir, '/home/g/BCTU-Dashboard/BCTU_Clinical_Trials_Dashboard',
       'app is the checkout beside electron/');
    eq(p.runtimeDir, `${el}/runtime`, 'runtime is electron/runtime');
    eq(p.rscript, '/usr/bin/Rscript', 'falls back to an installed R');
  });

  await section('locate: which R wins', () => {
    const el = '/e';
    const ctx = {
      appPath: el, resourcesPath: '/r', isPackaged: false,
      userData: '/u', platform: 'linux', env: {},
    };
    let p = resolvePaths({ ...ctx, ...fakeFs([`${el}/runtime/R/bin/Rscript`, '/usr/bin/Rscript']) });
    eq(p.rscript, `${el}/runtime/R/bin/Rscript`,
       'the bundled R is preferred over an installed one');

    p = resolvePaths({ ...ctx, ...fakeFs(['/usr/local/bin/Rscript', '/usr/bin/Rscript']) });
    eq(p.rscript, '/usr/local/bin/Rscript', 'and among installed ones, the first listed');

    p = resolvePaths({ ...ctx, ...fakeFs([]) });
    eq(p.rscript, null, 'no R found is null, not a throw');
    ok(p.rscriptTried.length > 0, 'and reports where it looked, for the error message');
  });

  await section('locate: Windows', () => {
    const el = 'C:/e';
    const env = { ProgramFiles: 'C:/Program Files', LOCALAPPDATA: 'C:/Users/g/AppData/Local' };
    const ctx = { appPath: el, resourcesPath: 'C:/r', isPackaged: false, userData: 'C:/u',
                  platform: 'win32', env };

    let p = resolvePaths({ ...ctx, ...fakeFs([`${el}/runtime/R/bin/x64/Rscript.exe`]) });
    eq(p.rscript, 'C:/e/runtime/R/bin/x64/Rscript.exe', 'prefers bin/x64 when it exists');

    p = resolvePaths({ ...ctx, ...fakeFs([`${el}/runtime/R/bin/Rscript.exe`]) });
    eq(p.rscript, 'C:/e/runtime/R/bin/Rscript.exe', 'and takes plain bin/ when it does not');

    // The installed-R glob: R keeps each version in its own folder.
    p = resolvePaths({ ...ctx, ...fakeFs([
      'C:/Program Files/R/R-4.3.1/bin/x64/Rscript.exe',
      'C:/Program Files/R/R-4.4.2/bin/x64/Rscript.exe',
    ]) });
    eq(p.rscript, 'C:/Program Files/R/R-4.4.2/bin/x64/Rscript.exe',
       'picks the newest installed R version');

    p = resolvePaths({ ...ctx, ...fakeFs([
      'C:/Users/g/AppData/Local/Programs/R/R-4.4.2/bin/x64/Rscript.exe',
    ]) });
    eq(p.rscript, 'C:/Users/g/AppData/Local/Programs/R/R-4.4.2/bin/x64/Rscript.exe',
       'and finds a per-user install too');

    ok(!resolvePaths({ ...ctx, ...fakeFs(['C:/Program Files/R/notR/bin/x64/Rscript.exe']) }).rscript,
       'a folder that is not R-something is not treated as an R install');

    ok(rscriptCandidates('C:/e/runtime', 'win32', {}).every((c) => c.endsWith('.exe')),
       'every Windows candidate is an .exe');
    ok(rscriptCandidates('/e/runtime', 'linux', {}).every((c) => !c.endsWith('.exe')),
       'and no Unix candidate is');
  });

  await section('locate: the environment R runs with', () => {
    const e = appEnv({ PATH: '/usr/bin', HOME: '/home/g' },
                     { dataDir: '/data', runtimeDir: '/rt' });
    eq(e.BCTU_DATA_DIR, '/data', 'points the app at the per-user data folder');
    eq(e.BCTU_RUNTIME_DIR, '/rt', 'and at the bundled pandoc and Chrome');
    eq(e.PATH, '/usr/bin', 'keeps the inherited environment');
    ok(!('BCTU_DESKTOP' in e),
       'does not set BCTU_DESKTOP — the idle shutdown would fight the window');
  });

  // The R side reads these two names; if either is renamed this fails here
  // rather than as a dashboard that writes to a read-only folder.
  await section('locate: agrees with the R side', () => {
    const r = path.join(__dirname, '..', '..', 'BCTU_Clinical_Trials_Dashboard', 'globals');
    const paths = fs.readFileSync(path.join(r, 'paths.R'), 'utf8');
    const runtime = fs.readFileSync(path.join(r, 'runtime.R'), 'utf8');
    ok(paths.includes('BCTU_DATA_DIR'), 'globals/paths.R reads BCTU_DATA_DIR');
    ok(runtime.includes('BCTU_RUNTIME_DIR'), 'globals/runtime.R reads BCTU_RUNTIME_DIR');
    ok(runtime.includes('"pandoc"') && runtime.includes('chrome'),
       'and looks for pandoc and chrome under it');
  });
}

// -- lib/server --------------------------------------------------------------

function findRscript() {
  for (const c of ['/usr/local/bin/Rscript', '/usr/bin/Rscript', '/opt/homebrew/bin/Rscript']) {
    if (fs.existsSync(c)) return c;
  }
  return null;
}
function hasShiny(rscript) {
  try {
    return execFileSync(rscript, ['-e', 'cat(requireNamespace("shiny", quietly=TRUE))'],
                        { encoding: 'utf8', timeout: 60000 }).includes('TRUE');
  } catch (_) { return false; }
}

async function testServer() {
  await section('server: free ports', async () => {
    const a = await freePort();
    ok(Number.isInteger(a) && a > 1024, 'gives a usable port number');
    ok(!(await ping(a, 300)), 'and nothing is listening on it');
    const b = await freePort();
    ok(a !== b, 'successive calls do not hand out the same port');
  });

  await section('server: waiting gives up for the right reasons', async () => {
    const port = await freePort();
    let err = null;
    try { await waitForServer(port, { timeoutMs: 600, intervalMs: 100 }); }
    catch (e) { err = e; }
    ok(err && /did not start within/.test(err.message), 'times out when nothing ever answers');

    err = null;
    const t0 = Date.now();
    try { await waitForServer(port, { timeoutMs: 30000, intervalMs: 100, hasExited: () => true }); }
    catch (e) { err = e; }
    ok(err && /stopped before/.test(err.message), 'and reports a dead R immediately');
    ok(Date.now() - t0 < 5000, 'rather than sitting out the whole timeout');
  });

  await section('server: stopping is safe to call on nothing', () => {
    stopR(null);
    stopR(undefined);
    stopR({ exitCode: 0, killed: false });
    ok(true, 'stopR ignores a process that is absent or already gone');
  });

  const rscript = findRscript();
  if (!rscript || !hasShiny(rscript)) {
    console.log('\nserver: against a real R\n  SKIP  no R with shiny on this machine');
    skipped += 1;
    return;
  }

  await section('server: against a real R', async () => {
    // A one-page Shiny app, so this exercises the same spawn/wait/stop path the
    // desktop build uses, on the real thing.
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'bctu-launch-'));
    fs.writeFileSync(path.join(dir, 'app.R'),
      'library(shiny)\n' +
      'shinyApp(ui = fluidPage(Sys.getenv("BCTU_DATA_DIR")), server = function(input, output) {})\n');

    const port = await freePort();
    const lines = [];
    let exited = false;
    const child = startR({
      rscript, appDir: dir, port,
      env: appEnv(process.env, { dataDir: dir, runtimeDir: path.join(dir, 'rt') }),
      onLog: (l) => lines.push(l),
    });
    child.on('exit', () => { exited = true; });

    try {
      const ms = await waitForServer(port, { timeoutMs: 90000, hasExited: () => exited });
      ok(ms > 0, `the app started and answered (${ms}ms)`);
      ok(await ping(port), 'and keeps answering');
      ok(lines.some((l) => /Listening on/.test(l)),
         'R\'s output is captured line by line — this is what the log and the error dialog show');
    } finally {
      stopR(child, { graceMs: 1000 });
    }

    // Closing the window has to actually free the port, or a restart collides.
    const gone = await new Promise((resolve) => {
      if (exited) return resolve(true);
      const timer = setTimeout(() => resolve(false), 15000);
      child.on('exit', () => { clearTimeout(timer); resolve(true); });
    });
    ok(gone, 'stopR ends the R process');
    ok(!(await ping(port, 500)), 'and the port is free again');
  });

  await section('server: an R that dies is reported, not waited on', async () => {
    const dir = fs.mkdtempSync(path.join(os.tmpdir(), 'bctu-broken-'));
    // The realistic failure: app.R exists but doesn't load.
    fs.writeFileSync(path.join(dir, 'app.R'), 'stop("a package is missing")\n');

    const port = await freePort();
    const lines = [];
    let exited = false;
    const child = startR({ rscript, appDir: dir, port, env: process.env,
                           onLog: (l) => lines.push(l) });
    child.on('exit', () => { exited = true; });

    let err = null;
    try { await waitForServer(port, { timeoutMs: 90000, hasExited: () => exited }); }
    catch (e) { err = e; }
    ok(err && /stopped before/.test(err.message), 'waiting rejects once R exits');
    ok(lines.some((l) => /a package is missing/.test(l)),
       'and the reason is in the captured output, which is what the dialog shows');
    stopR(child, { graceMs: 500 });
  });
}

// -- packaging ---------------------------------------------------------------

async function testPackaging() {
  await section('packaging', () => {
    const root = path.join(__dirname, '..');
    const pkg = JSON.parse(fs.readFileSync(path.join(root, 'package.json'), 'utf8'));
    eq(pkg.main, 'main.js', 'the entry point is main.js');
    for (const f of ['main.js', 'loading.html', 'lib/locate.js', 'lib/server.js',
                     'build/icon.ico', 'build/icon.png']) {
      ok(fs.existsSync(path.join(root, f)), `${f} is present`);
    }

    const extra = pkg.build.extraResources;
    const app = extra.find((e) => e.to === 'app');
    const rt = extra.find((e) => e.to === 'runtime');
    ok(app && rt, 'both the app and the runtime are packaged');
    // The whole point of staging: the working folder holds real participant
    // data, and must never be what gets packaged.
    eq(app.from, '../dist-staging',
       'the app is packaged from the staged clean copy, not the working folder');
    ok(pkg.scripts.stage && /prepare_distribution\.R/.test(pkg.scripts.stage),
       'and `npm run stage` is what creates it, via the tested R script');
    ok(/npm run stage/.test(pkg.scripts.dist) || /stage/.test(pkg.scripts.predist || ''),
       'so that `npm run dist` cannot package an unstaged folder by accident');

    // resolvePaths puts the app at resources/app and the runtime at
    // resources/runtime; if these ever disagree the app starts and finds nothing.
    const p = resolvePaths({
      appPath: '/res/app.asar', resourcesPath: '/res', isPackaged: true,
      userData: '/u', platform: 'linux', env: {}, ...fakeFs([]),
    });
    eq(p.appDir, `/res/${app.to}`, 'and main.js looks where the builder puts the app');
    eq(p.runtimeDir, `/res/${rt.to}`, 'and where it puts the runtime');
  });
}

// ----------------------------------------------------------------------------

(async () => {
  await testLocate();
  await testServer();
  await testPackaging();
  console.log(`\n${passed} passed, ${failed} failed${skipped ? `, ${skipped} skipped` : ''}`);
  process.exit(failed ? 1 : 0);
})();

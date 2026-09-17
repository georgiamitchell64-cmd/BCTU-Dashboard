'use strict';
// =============================================================================
// Where everything is
// =============================================================================
// The desktop build carries four things that have to be found at startup: the
// Shiny app itself, an R to run it with, the pandoc and Chrome that report
// export needs, and a folder the app may write to.
//
// Two layouts, because the app runs from a checkout while it is being worked
// on and from inside an installer afterwards:
//
//   developing   electron/main.js      app at ../BCTU_Clinical_Trials_Dashboard
//   packaged     resources/app.asar    app at resources/app
//
// Everything here is a pure function over an injected context, so the whole
// thing can be tested without Electron — see test/locate.test.js.
// =============================================================================

const path = require('path');

/** Candidate Rscript paths, best first: what we shipped, then what's installed. */
function rscriptCandidates(runtimeDir, platform, env = {}) {
  const win = platform === 'win32';
  const exe = win ? 'Rscript.exe' : 'Rscript';
  const out = [
    // Bundled by .github/workflows/desktop-build.yml. On Windows R keeps both
    // a bin/x64 and a bin copy; which one exists depends on the R version.
    path.join(runtimeDir, 'R', 'bin', 'x64', exe),
    path.join(runtimeDir, 'R', 'bin', exe),
  ];
  if (win) {
    for (const base of [env.ProgramFiles, env.LOCALAPPDATA && path.join(env.LOCALAPPDATA, 'Programs')]) {
      if (base) out.push(path.join(base, 'R', '__GLOB__', 'bin', 'x64', exe),
                         path.join(base, 'R', '__GLOB__', 'bin', exe));
    }
  } else {
    out.push('/usr/local/bin/Rscript', '/opt/homebrew/bin/Rscript',
             '/Library/Frameworks/R.framework/Resources/bin/Rscript',
             '/usr/bin/Rscript');
  }
  return out;
}

/**
 * Resolve every path the launcher needs.
 *
 * ctx: { appPath, resourcesPath, isPackaged, userData, platform, env, exists,
 *        listDir }
 *   exists(p)   -> boolean            (fs.existsSync)
 *   listDir(p)  -> string[]           (fs.readdirSync, for the R-x.y.z glob)
 *
 * Returns { appDir, runtimeDir, dataDir, logFile, rscript, rscriptTried }.
 * rscript is null when no R could be found — main.js turns that into a
 * readable message rather than a window that never loads.
 */
function resolvePaths(ctx) {
  const { appPath, resourcesPath, isPackaged, userData, platform } = ctx;
  const env = ctx.env || {};
  const exists = ctx.exists || (() => false);
  const listDir = ctx.listDir || (() => []);

  // Packaged, main.js runs from inside app.asar, so the app and the runtime
  // sit beside it in resources/ — unpacked, since neither R nor a browser can
  // be executed from inside an archive.
  const appDir = isPackaged
    ? path.join(resourcesPath, 'app')
    : path.resolve(appPath, '..', 'BCTU_Clinical_Trials_Dashboard');
  const runtimeDir = isPackaged
    ? path.join(resourcesPath, 'runtime')
    : path.join(appPath, 'runtime');

  // Expand the R-4.4.2-style folder in the Windows install paths.
  const candidates = [];
  for (const c of rscriptCandidates(runtimeDir, platform, env)) {
    if (!c.includes('__GLOB__')) { candidates.push(c); continue; }
    const base = c.slice(0, c.indexOf('__GLOB__') - 1);
    let versions = [];
    try { versions = listDir(base).filter((d) => /^R-/.test(d)).sort().reverse(); } catch (_) {}
    for (const v of versions) candidates.push(c.replace('__GLOB__', v));
  }

  const rscript = candidates.find((p) => exists(p)) || null;

  return {
    appDir,
    runtimeDir,
    dataDir: userData,
    logFile: path.join(userData, 'dashboard.log'),
    rscript,
    rscriptTried: candidates,
  };
}

/**
 * The environment the R process runs with.
 *
 * BCTU_DATA_DIR and BCTU_RUNTIME_DIR are what globals/paths.R and
 * globals/runtime.R read: the first sends everything the app writes to the
 * per-user folder (the install folder is read-only), the second points
 * report export at the pandoc and Chrome shipped alongside.
 */
function appEnv(base, { dataDir, runtimeDir }) {
  return Object.assign({}, base, {
    BCTU_DATA_DIR: dataDir,
    BCTU_RUNTIME_DIR: runtimeDir,
    // Not BCTU_DESKTOP: that arms the ten-minute idle shutdown meant for the
    // browser shortcut. Here the window owns the lifetime — closing it stops
    // R directly, so a timer would only ever fire at the wrong moment.
  });
}

module.exports = { resolvePaths, rscriptCandidates, appEnv };

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
 * Returns { appDir, runtimeDir, dataDir, logFile, rscript, rscriptTried,
 * bundledR }. rscript is null when no R could be found — main.js turns that
 * into a readable message rather than a window that never loads.
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

  // Whether we are running the R we shipped, which decides whether the
  // packages are ours to pin — see appEnv().
  const slash = (p) => String(p).replace(/\\/g, '/');
  const rHome = slash(path.join(runtimeDir, 'R')) + '/';
  const bundledR = !!rscript && slash(rscript).startsWith(rHome);

  return {
    appDir,
    runtimeDir,
    dataDir: userData,
    logFile: path.join(userData, 'dashboard.log'),
    rscript,
    rscriptTried: candidates,
    bundledR,
  };
}

/**
 * The environment the R process runs with.
 *
 * BCTU_DATA_DIR and BCTU_RUNTIME_DIR are what globals/paths.R and
 * globals/runtime.R read: the first sends everything the app writes to the
 * per-user folder (the install folder is read-only), the second points
 * report export at the pandoc and Chrome shipped alongside.
 *
 * When we are running our own R, the library is pinned to the one we shipped.
 * R otherwise puts the person's own package library first — on Windows that is
 * %LOCALAPPDATA%\R\win-library\<x.y> — so anyone who already has R of the
 * same minor version would silently run the dashboard against their packages
 * rather than the tested ones. That is the whole point of bundling, and the
 * failure it causes ("object not found" somewhere deep in a package) looks
 * nothing like its cause. R_LIBS and R_LIBS_SITE go the same way, since either
 * can be set machine-wide and would be inherited here.
 *
 * On a fallback to an installed R we leave all of it alone: there, the
 * person's library is exactly where the packages are.
 */
function appEnv(base, { dataDir, runtimeDir, bundledR = false }) {
  const env = Object.assign({}, base, {
    BCTU_DATA_DIR: dataDir,
    BCTU_RUNTIME_DIR: runtimeDir,
    // Not BCTU_DESKTOP: that arms the ten-minute idle shutdown meant for the
    // browser shortcut. Here the window owns the lifetime — closing it stops
    // R directly, so a timer would only ever fire at the wrong moment.
  });
  if (bundledR) {
    const lib = path.join(runtimeDir, 'R', 'library');
    env.R_LIBS = lib;
    env.R_LIBS_USER = lib;
    env.R_LIBS_SITE = lib;
  }
  return env;
}

module.exports = { resolvePaths, rscriptCandidates, appEnv };

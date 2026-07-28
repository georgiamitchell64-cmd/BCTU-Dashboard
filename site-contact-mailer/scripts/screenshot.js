'use strict';

// Dev helper: boots the real app in a throwaway profile, optionally seeded
// with a data.json, and writes a screenshot. Useful for checking the UI
// without a desktop session:
//
//   SCM_SHOT=/tmp/app.png xvfb-run -a npx electron --no-sandbox scripts/screenshot.js
//
// Paths come from the environment rather than argv, because Electron's own
// flags shift argv positions.

const path = require('path');
const fs = require('fs');
const os = require('os');
const { app, BrowserWindow } = require('electron');

const output = process.env.SCM_SHOT || path.join(os.tmpdir(), 'site-contact-mailer.png');
const seedFile = process.env.SCM_SEED;

if (!/\.png$/i.test(output)) {
  process.stderr.write(`Refusing to write a PNG to "${output}" — SCM_SHOT must end in .png\n`);
  process.exit(1);
}

// Never touch the real app's stored contact list.
app.setPath('userData', fs.mkdtempSync(path.join(os.tmpdir(), 'scm-shot-')));
if (seedFile) fs.copyFileSync(seedFile, path.join(app.getPath('userData'), 'data.json'));

require('../src/main/main.js');

const problems = [];

app.whenReady().then(async () => {
  await new Promise((resolve) => setTimeout(resolve, 1200));

  const [window] = BrowserWindow.getAllWindows();
  if (!window) {
    process.stderr.write('No window was created\n');
    app.exit(1);
    return;
  }

  window.webContents.on('console-message', (_event, level, message) => {
    if (level >= 2) problems.push(message);
  });
  window.webContents.on('render-process-gone', (_event, details) => {
    problems.push(`renderer gone: ${details.reason}`);
  });

  // Let the renderer finish its initial load-and-render.
  await new Promise((resolve) => setTimeout(resolve, 1200));

  if (process.env.SCM_EVAL) {
    try {
      await window.webContents.executeJavaScript(process.env.SCM_EVAL);
      await new Promise((resolve) => setTimeout(resolve, 500));
    } catch (error) {
      problems.push(`eval failed: ${error.message}`);
    }
  }

  fs.writeFileSync(output, (await window.webContents.capturePage()).toPNG());
  process.stdout.write(`Wrote ${output}\n`);

  const summary = await window.webContents.executeJavaScript(
    'JSON.stringify({sites: state.sites.length, selected: state.sites.filter(s=>s.selected).length, mode: state.mode})',
  );
  process.stdout.write(`Renderer state: ${summary}\n`);
  process.stdout.write(problems.length ? `Console problems:\n${problems.join('\n')}\n` : 'No console errors\n');

  app.exit(problems.length ? 1 : 0);
});

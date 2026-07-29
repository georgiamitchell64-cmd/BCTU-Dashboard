/* ============================================================================
 * preview-server.js — serve src/ over http for browser-based development
 * ----------------------------------------------------------------------------
 * Not used by the desktop app. It exists so the renderer can be opened in a
 * normal browser (and driven by a headless one in tests) without Electron.
 * In that mode the store falls back to localStorage.
 *
 *   node tools/preview-server.js [port]
 * ========================================================================== */

const http = require('node:http');
const fs = require('node:fs');
const path = require('node:path');

const ROOT = path.join(__dirname, '..', 'src');
const PORT = Number(process.argv[2]) || 4173;

const TYPES = {
  '.html': 'text/html; charset=utf-8',
  '.js': 'text/javascript; charset=utf-8',
  '.css': 'text/css; charset=utf-8',
  '.json': 'application/json; charset=utf-8',
  '.svg': 'image/svg+xml',
  '.png': 'image/png'
};

const server = http.createServer((req, res) => {
  const url = new URL(req.url, `http://localhost:${PORT}`);
  const relative = decodeURIComponent(url.pathname === '/' ? '/index.html' : url.pathname);
  const file = path.join(ROOT, relative);

  if (!file.startsWith(ROOT + path.sep)) {
    res.writeHead(403).end('Forbidden');
    return;
  }

  fs.readFile(file, (err, data) => {
    if (err) {
      res.writeHead(404, { 'content-type': 'text/plain' }).end('Not found');
      return;
    }
    res.writeHead(200, {
      'content-type': TYPES[path.extname(file)] || 'application/octet-stream',
      'cache-control': 'no-store'
    }).end(data);
  });
});

server.listen(PORT, () => {
  console.log(`Planner preview running at http://localhost:${PORT}`);
});

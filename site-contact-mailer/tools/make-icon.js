/* ============================================================================
 * make-icon.js — generate the app icon
 * ----------------------------------------------------------------------------
 * Draws the icon as SVG and renders it to the files electron-builder looks for:
 *
 *   build/icon.ico       multi-size Windows icon (16 → 256)
 *   build/icon.png       1024×1024 master render
 *   src/assets/icon.png  256×256, shipped inside the app for the window icon
 *
 * Kept as a script rather than committed binaries so the icon can be changed by
 * editing the shape below and re-running, instead of hunting for whatever tool
 * produced it.
 *
 *   npm run icon
 *
 * Needs sharp, which is a devDependency:  npm install
 * ========================================================================== */

const fs = require('node:fs/promises');
const path = require('node:path');
const sharp = require('sharp');

const OUT = path.join(__dirname, '..', 'build');
const ASSETS = path.join(__dirname, '..', 'src', 'assets');

// Matches the tokens in src/renderer/styles.css.
const BRAND = '#1B4F6B';
const BRAND_DK = '#123B52';
const GREEN = '#10B981';

/**
 * An envelope, in the same navy as the planner but with this app's green, so
 * the two read as a pair without being mistaken for each other in the taskbar.
 * The flap is the only element that has to survive at 16px, so it carries the
 * contrast.
 */
const svg = `
<svg xmlns="http://www.w3.org/2000/svg" width="1024" height="1024" viewBox="0 0 1024 1024">
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0" stop-color="${BRAND}"/>
      <stop offset="1" stop-color="${BRAND_DK}"/>
    </linearGradient>
    <linearGradient id="flap" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#34D9A4"/>
      <stop offset="1" stop-color="${GREEN}"/>
    </linearGradient>
  </defs>

  <rect width="1024" height="1024" rx="224" fill="url(#bg)"/>

  <!-- envelope body -->
  <rect x="176" y="292" width="672" height="440" rx="64"
        fill="#FFFFFF" opacity="0.12"/>
  <rect x="176" y="292" width="672" height="440" rx="64"
        fill="none" stroke="#FFFFFF" stroke-opacity="0.26" stroke-width="16"/>

  <!-- the flap, drawn thick so it still reads at taskbar size -->
  <path d="M214 356 L512 574 L810 356"
        fill="none" stroke="url(#flap)" stroke-width="88"
        stroke-linecap="round" stroke-linejoin="round"/>

  <!-- lower fold, faint, just enough to say "envelope" rather than "chevron" -->
  <g stroke="#FFFFFF" stroke-opacity="0.22" stroke-width="34"
     stroke-linecap="round" fill="none">
    <path d="M214 676 L392 536"/>
    <path d="M810 676 L632 536"/>
  </g>
</svg>`;

/** Build a Windows .ico containing PNG-compressed entries (Vista and later). */
function buildIco(images) {
  const count = images.length;
  const header = Buffer.alloc(6);
  header.writeUInt16LE(0, 0); // reserved
  header.writeUInt16LE(1, 2); // 1 = icon
  header.writeUInt16LE(count, 4);

  const directory = Buffer.alloc(16 * count);
  let offset = 6 + directory.length;

  images.forEach(({ size, data }, index) => {
    const entry = index * 16;
    directory.writeUInt8(size >= 256 ? 0 : size, entry + 0); // width, 0 means 256
    directory.writeUInt8(size >= 256 ? 0 : size, entry + 1); // height
    directory.writeUInt8(0, entry + 2); // palette size
    directory.writeUInt8(0, entry + 3); // reserved
    directory.writeUInt16LE(1, entry + 4); // colour planes
    directory.writeUInt16LE(32, entry + 6); // bits per pixel
    directory.writeUInt32LE(data.length, entry + 8);
    directory.writeUInt32LE(offset, entry + 12);
    offset += data.length;
  });

  return Buffer.concat([header, directory, ...images.map((image) => image.data)]);
}

async function main() {
  await fs.mkdir(OUT, { recursive: true });
  await fs.mkdir(ASSETS, { recursive: true });
  const source = Buffer.from(svg);

  const png = await sharp(source, { density: 384 }).resize(1024, 1024).png().toBuffer();
  await fs.writeFile(path.join(OUT, 'icon.png'), png);

  const windowIcon = await sharp(source, { density: 384 }).resize(256, 256).png().toBuffer();
  await fs.writeFile(path.join(ASSETS, 'icon.png'), windowIcon);

  const sizes = [16, 24, 32, 48, 64, 128, 256];
  const images = await Promise.all(sizes.map(async (size) => ({
    size,
    data: await sharp(source, { density: 384 }).resize(size, size).png({ compressionLevel: 9 }).toBuffer()
  })));
  await fs.writeFile(path.join(OUT, 'icon.ico'), buildIco(images));

  console.log(`Wrote ${path.relative(process.cwd(), OUT)}/icon.png (1024×1024)`);
  console.log(`Wrote ${path.relative(process.cwd(), OUT)}/icon.ico (${sizes.join(', ')})`);
  console.log(`Wrote ${path.relative(process.cwd(), ASSETS)}/icon.png (256×256)`);
}

main().catch((error) => {
  console.error(error);
  process.exit(1);
});

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

// TONIC brand colours, sampled from the trial's own logo. The teal is the
// accent used across the app and the recruitment charts.
const NAVY = '#00344C';
const TEAL = '#12A192';
const PAPER = '#FFFFFF';
const EDGE = '#D7E3E8';

/**
 * A light-themed envelope in the TONIC palette, with the trial's drip motif
 * standing in for the seal.
 *
 * Light rather than dark on purpose: Windows taskbars and Start tiles are
 * usually dark, so a pale tile separates from its background, and it matches
 * the logo's own white-background lockup.
 *
 * Only three things have to survive at 16px — the tile, the flap and the
 * droplet — so those carry the contrast and everything else is detail that is
 * allowed to disappear.
 */
const svg = `
<svg xmlns="http://www.w3.org/2000/svg" width="1024" height="1024" viewBox="0 0 1024 1024">
  <!-- tile -->
  <rect width="1024" height="1024" rx="224" fill="${PAPER}"/>
  <rect x="8" y="8" width="1008" height="1008" rx="216"
        fill="none" stroke="${EDGE}" stroke-width="16"/>

  <!-- envelope body -->
  <rect x="168" y="304" width="688" height="452" rx="56"
        fill="none" stroke="${NAVY}" stroke-width="52"/>

  <!-- the flap, the strongest element so it still reads in the taskbar -->
  <path d="M206 368 L512 592 L818 368"
        fill="none" stroke="${TEAL}" stroke-width="86"
        stroke-linecap="round" stroke-linejoin="round"/>

  <!-- TONIC's drip, as the seal -->
  <path d="M512 690 C 512 690 452 762 452 800 a 60 60 0 0 0 120 0 C 572 762 512 690 512 690 Z"
        fill="${TEAL}"/>
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

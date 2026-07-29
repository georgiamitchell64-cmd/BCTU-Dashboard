/* ============================================================================
 * make-icon.js — generate the app icon
 * ----------------------------------------------------------------------------
 * Draws the icon as SVG and renders it to the files electron-builder looks for:
 *
 *   build/icon.png       1024×1024, used for Linux and as the source for macOS
 *   build/icon.ico       multi-size Windows icon (16 → 256)
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

// Brand colours, matching src/styles/tokens.css.
const NAVY = '#1B4F6B';
const NAVY_DK = '#123B52';
const TEAL = '#2EC4A5';

/**
 * A diary page with a tick through it. The tick is deliberately fat and
 * high-contrast so the icon still reads at 16px in the taskbar.
 */
const svg = `
<svg xmlns="http://www.w3.org/2000/svg" width="1024" height="1024" viewBox="0 0 1024 1024">
  <defs>
    <linearGradient id="bg" x1="0" y1="0" x2="1" y2="1">
      <stop offset="0" stop-color="${NAVY}"/>
      <stop offset="1" stop-color="${NAVY_DK}"/>
    </linearGradient>
    <linearGradient id="tick" x1="0" y1="0" x2="0" y2="1">
      <stop offset="0" stop-color="#41E0BE"/>
      <stop offset="1" stop-color="${TEAL}"/>
    </linearGradient>
  </defs>

  <rect width="1024" height="1024" rx="224" fill="url(#bg)"/>

  <!-- binder rings, so it reads as a diary rather than a generic square -->
  <g fill="${TEAL}" opacity="0.55">
    <rect x="330" y="150" width="52" height="132" rx="26"/>
    <rect x="642" y="150" width="52" height="132" rx="26"/>
  </g>

  <!-- page -->
  <rect x="212" y="246" width="600" height="580" rx="72"
        fill="#FFFFFF" opacity="0.10"/>
  <rect x="212" y="246" width="600" height="580" rx="72"
        fill="none" stroke="#FFFFFF" stroke-opacity="0.22" stroke-width="14"/>

  <!-- ruled lines -->
  <g fill="#FFFFFF" opacity="0.24">
    <rect x="300" y="392" width="424" height="26" rx="13"/>
    <rect x="300" y="486" width="300" height="26" rx="13"/>
  </g>

  <!-- the tick -->
  <path d="M318 622 L446 748 L742 452"
        fill="none" stroke="url(#tick)" stroke-width="86"
        stroke-linecap="round" stroke-linejoin="round"/>
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

  // Shipped inside the asar: BrowserWindow uses it for the window and taskbar
  // icon at runtime, which is what gives us a real icon on Windows despite
  // signAndEditExecutable being off. See README.
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

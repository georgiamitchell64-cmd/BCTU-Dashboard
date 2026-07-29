/* ============================================================================
 * after-pack.js — stamp the Windows executable with our icon and metadata
 * ----------------------------------------------------------------------------
 * electron-builder would normally do this itself, but only via its winCodeSign
 * toolkit, which we deliberately do not download: that archive contains macOS
 * symlinks, and unpacking them on Windows needs the create-symlink privilege a
 * standard account on a managed machine does not hold. See
 * win.signAndEditExecutable in package.json and the icon section of the README.
 *
 * The rcedit npm package ships rcedit.exe on its own, with no archive and no
 * symlinks, so it does the same job without the privilege. Running it on
 * Windows needs nothing extra; on Linux or macOS it needs wine.
 *
 * This hook is deliberately best-effort. If rcedit is missing or fails — which
 * is what happens when packaging for Windows from Linux without wine — it warns
 * and lets the build continue. A wrong icon is a blemish; a failed build is not.
 * ========================================================================== */

const fs = require('node:fs');
const path = require('node:path');

exports.default = async function afterPack(context) {
  if (context.electronPlatformName !== 'win32') return;

  const { build, version, author } = require('../package.json');
  const productName = build.productName;
  const company = typeof author === 'string' ? author : author?.name ?? productName;

  const exePath = path.join(context.appOutDir, `${productName}.exe`);
  const iconPath = path.join(__dirname, 'icon.ico');

  if (!fs.existsSync(exePath)) {
    console.warn(`  • icon skipped  reason=${exePath} not found`);
    return;
  }
  if (!fs.existsSync(iconPath)) {
    console.warn('  • icon skipped  reason=build/icon.ico not found, run "npm run icon"');
    return;
  }

  let rcedit;
  try {
    // v5 exports { rcedit }; older versions export the function itself.
    const mod = require('rcedit');
    rcedit = typeof mod === 'function' ? mod : (mod.rcedit ?? mod.default);
  } catch {
    console.warn('  • icon skipped  reason=rcedit is not installed');
    return;
  }

  if (typeof rcedit !== 'function') {
    console.warn('  • icon skipped  reason=rcedit did not export a callable');
    return;
  }

  try {
    await rcedit(exePath, {
      icon: iconPath,
      'file-version': version,
      'product-version': version,
      'version-string': {
        ProductName: productName,
        FileDescription: productName,
        CompanyName: company,
        LegalCopyright: `© ${new Date().getFullYear()} ${company}`,
        OriginalFilename: `${productName}.exe`
      }
    });
    console.log(`  • icon applied to ${productName}.exe`);
  } catch (error) {
    // Almost always "wine is required" when building for Windows off Windows.
    console.warn(`  • icon skipped  reason=${error.message.split('\n')[0]}`);
    console.warn('    Build on Windows to get the executable icon; the app itself is unaffected.');
  }
};

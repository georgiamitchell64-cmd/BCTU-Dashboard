/* ============================================================================
 * after-pack.js — stamp the Windows executable with our icon and metadata
 * ----------------------------------------------------------------------------
 * electron-builder would normally do this itself, but only via its winCodeSign
 * toolkit, which we deliberately do not download: that archive contains macOS
 * symlinks, and unpacking them on Windows needs the create-symlink privilege a
 * standard account on a managed machine does not hold. That is what produces
 * "Cannot create symbolic link : A required privilege is not held by the
 * client" on libcrypto.dylib — macOS libraries, in a Windows-only build. See
 * win.signAndEditExecutable in package.json.
 *
 * The rcedit npm package ships rcedit.exe on its own, with no archive and no
 * symlinks, so it does the same job without the privilege. On Windows it needs
 * nothing extra; on Linux or macOS it needs wine.
 *
 * Deliberately best-effort: if rcedit is missing or fails — which is what
 * happens when packaging for Windows from Linux without wine — it warns and
 * lets the build continue. A missing icon is a blemish; a failed build is not.
 * ========================================================================== */

const fs = require("node:fs");
const path = require("node:path");

exports.default = async function afterPack(context) {
  if (context.electronPlatformName !== "win32") return;

  const { build, version, author } = require("../package.json");
  const productName = build.productName;
  const company = typeof author === "string" ? author : author?.name ?? productName;

  const iconPath = path.join(__dirname, "icon.ico");

  // Prefer the executable named after the product, but fall back to whatever
  // .exe is in the output directory. Guessing the filename is the easiest way
  // for this to silently do nothing, and a missing icon is hard to trace back
  // to a warning nobody read.
  let exePath = path.join(context.appOutDir, `${productName}.exe`);
  if (!fs.existsSync(exePath)) {
    const candidates = fs
      .readdirSync(context.appOutDir)
      .filter((f) => f.toLowerCase().endsWith(".exe"))
      // Electron ships helper executables; the app is the largest.
      .map((f) => ({ f, size: fs.statSync(path.join(context.appOutDir, f)).size }))
      .sort((a, b) => b.size - a.size);

    if (!candidates.length) {
      console.warn(`  • icon SKIPPED  reason=no .exe found in ${context.appOutDir}`);
      return;
    }
    exePath = path.join(context.appOutDir, candidates[0].f);
    console.warn(
      `  • icon: "${productName}.exe" not found, using "${candidates[0].f}" instead`
    );
  }
  if (!fs.existsSync(iconPath)) {
    console.warn("  • icon SKIPPED  reason=build/icon.ico not found");
    return;
  }

  let rcedit;
  try {
    // v5 exports { rcedit }; older versions export the function itself.
    const mod = require("rcedit");
    rcedit = typeof mod === "function" ? mod : mod.rcedit ?? mod.default;
  } catch {
    console.warn("  • icon SKIPPED  reason=rcedit is not installed (npm install)");
    return;
  }

  if (typeof rcedit !== "function") {
    console.warn("  • icon SKIPPED  reason=rcedit did not export a callable");
    return;
  }

  try {
    await rcedit(exePath, {
      icon: iconPath,
      "file-version": version,
      "product-version": version,
      "version-string": {
        ProductName: productName,
        FileDescription: productName,
        CompanyName: company,
        LegalCopyright: `© ${new Date().getFullYear()} ${company}`,
        OriginalFilename: `${productName}.exe`,
      },
    });
    console.log(`  • icon APPLIED to ${path.basename(exePath)}`);
  } catch (error) {
    // Almost always "wine is required" when building for Windows off Windows.
    console.warn(`  • icon SKIPPED  reason=${error.message.split("\n")[0]}`);
    console.warn("    Build on Windows to get the executable icon; the app itself is unaffected.");
  }
};

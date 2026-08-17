// ============================================================================
// Lock screen and at-rest encryption
// ----------------------------------------------------------------------------
// WHAT THIS DOES
//   Imported trial data is encrypted before it is written to local storage,
//   with a key derived from the password (PBKDF2-SHA256, 250k iterations →
//   AES-GCM-256). Without the password the stored data cannot be read, which
//   is a genuine protection for data at rest.
//
// WHAT THIS DOES NOT DO
//   It is not a user-account system and it does not protect a machine that is
//   already unlocked with the app open. Anyone who can run code in this window
//   can read what is on screen once it is unlocked. It is a complement to disk
//   encryption and file permissions, not a replacement for them.
//
//   There is deliberately no password recovery: the key exists only in memory
//   for the session. Losing the password means clearing the stored data and
//   importing again, which is the honest consequence of encrypting properly.
//
// This runs before everything else and loads the rest of the app only once
// the password has been accepted, so nothing renders behind the lock screen.
// ============================================================================

const AUTH_KEY = "tonic_auth_v1";
const DATA_KEY = "tonic_dashboard_import_v1";
const SCRIPTS = ["chart.umd.js", "data.js", "import.js", "dashboard.js"];
const PBKDF2_ROUNDS = 250000;
const MIN_PW = 8;

const enc = new TextEncoder();
const dec = new TextDecoder();

const b64 = (buf) => btoa(String.fromCharCode(...new Uint8Array(buf)));
const unb64 = (s) => Uint8Array.from(atob(s), (c) => c.charCodeAt(0));

async function deriveKey(password, salt) {
  const base = await crypto.subtle.importKey(
    "raw", enc.encode(password), "PBKDF2", false, ["deriveKey"]
  );
  return crypto.subtle.deriveKey(
    { name: "PBKDF2", salt, iterations: PBKDF2_ROUNDS, hash: "SHA-256" },
    base,
    { name: "AES-GCM", length: 256 },
    false,
    ["encrypt", "decrypt"]
  );
}

async function encryptJSON(key, obj) {
  const iv = crypto.getRandomValues(new Uint8Array(12));
  const ct = await crypto.subtle.encrypt(
    { name: "AES-GCM", iv }, key, enc.encode(JSON.stringify(obj))
  );
  return { iv: b64(iv), ct: b64(ct) };
}

async function decryptJSON(key, blob) {
  const plain = await crypto.subtle.decrypt(
    { name: "AES-GCM", iv: unb64(blob.iv) }, key, unb64(blob.ct)
  );
  return JSON.parse(dec.decode(plain));
}

const readLS = (k) => { try { return JSON.parse(localStorage.getItem(k)); } catch { return null; } };
const writeLS = (k, v) => { try { localStorage.setItem(k, JSON.stringify(v)); return true; } catch { return false; } };

// ── Boot ────────────────────────────────────────────────────────────────────
const lock = document.getElementById("lock");
const form = document.getElementById("lock-form");
const pw = document.getElementById("lock-pw");
const pw2 = document.getElementById("lock-pw2");
const confirmWrap = document.getElementById("lock-confirm-wrap");
const errBox = document.getElementById("lock-err");
const goBtn = document.getElementById("lock-go");
const resetBtn = document.getElementById("lock-reset");
const title = document.getElementById("lock-title");
const blurb = document.getElementById("lock-blurb");

const existing = readLS(AUTH_KEY);
const firstRun = !existing;

if (firstRun) {
  title.textContent = "Set a password";
  blurb.innerHTML =
    "This password encrypts any trial data you import, so it cannot be read " +
    `from this machine without it. Minimum ${MIN_PW} characters. ` +
    "<b>There is no way to recover it</b> — if it is lost the stored data has " +
    "to be cleared and re-imported.";
  confirmWrap.hidden = false;
  pw.setAttribute("autocomplete", "new-password");
  goBtn.textContent = "Set password and continue";
} else {
  title.textContent = "Unlock";
  blurb.textContent =
    "Enter the password to decrypt the imported trial data on this machine.";
  resetBtn.hidden = false;
}

function showError(msg) {
  errBox.textContent = msg;
  errBox.hidden = false;
}

// Load the app's scripts in order, once past the lock screen.
function loadApp() {
  return SCRIPTS.reduce(
    (chain, src) =>
      chain.then(
        () =>
          new Promise((res, rej) => {
            const s = document.createElement("script");
            s.src = src;
            s.onload = res;
            s.onerror = () => rej(new Error("Could not load " + src));
            document.body.appendChild(s);
          })
      ),
    Promise.resolve()
  );
}

form.addEventListener("submit", async (e) => {
  e.preventDefault();
  errBox.hidden = true;

  const password = pw.value;
  if (firstRun) {
    if (password.length < MIN_PW) return showError(`Use at least ${MIN_PW} characters.`);
    if (password !== pw2.value) return showError("The two passwords do not match.");
  } else if (!password) {
    return showError("Enter the password.");
  }

  goBtn.disabled = true;
  goBtn.textContent = "Working…";

  try {
    let key, stored = null;

    if (firstRun) {
      const salt = crypto.getRandomValues(new Uint8Array(16));
      key = await deriveKey(password, salt);
      // A known value encrypted under the key, so a later attempt can be
      // checked without keeping the password anywhere.
      const check = await encryptJSON(key, { ok: true });
      if (!writeLS(AUTH_KEY, { salt: b64(salt), check })) {
        throw new Error("This browser refused local storage, so a password cannot be stored.");
      }
    } else {
      key = await deriveKey(password, unb64(existing.salt));
      try {
        await decryptJSON(key, existing.check);   // throws if the key is wrong
      } catch {
        goBtn.disabled = false;
        goBtn.textContent = "Unlock";
        return showError("That password is not correct.");
      }
      const blob = readLS(DATA_KEY);
      if (blob && blob.ct) {
        try {
          stored = await decryptJSON(key, blob);
        } catch {
          showError("The stored data could not be decrypted and has been ignored.");
        }
      }
    }

    // Hand the key and the decrypted payload to the rest of the app. The key
    // lives only here, in memory, for this session.
    window.__CRYPTO = {
      encryptJSON: (obj) => encryptJSON(key, obj),
      DATA_KEY,
    };
    window.__STORED = stored;

    await loadApp();
    lock.hidden = true;
  } catch (err) {
    goBtn.disabled = false;
    goBtn.textContent = firstRun ? "Set password and continue" : "Unlock";
    showError(err.message || "Something went wrong unlocking the app.");
  }
});

resetBtn.addEventListener("click", () => {
  if (!confirm(
    "This clears the saved password and any imported trial data on this " +
    "machine. The data cannot be recovered without the password, so it will " +
    "need importing again. Continue?"
  )) return;
  try {
    localStorage.removeItem(AUTH_KEY);
    localStorage.removeItem(DATA_KEY);
  } catch { /* nothing to clear */ }
  location.reload();
});

pw.focus();

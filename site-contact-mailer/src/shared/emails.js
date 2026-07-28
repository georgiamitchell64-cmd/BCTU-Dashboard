'use strict';

// Address handling shared by the importer, the merge engine and the mailer.
// Deliberately dependency-free so it can be unit tested without Electron.

// Permissive on purpose: this validates addresses typed into a trial
// spreadsheet, so it should accept anything a mail server would, and only
// reject things that are obviously not an address.
const EMAIL_RE = /^[^\s@,;<>]+@[^\s@,;<>]+\.[^\s@,;<>]{2,}$/;

// Matches "Jane Bloggs <j.bloggs@nhs.net>" as well as a bare address.
const NAMED_RE = /^\s*(?:"([^"]*)"|([^<>]*?))\s*<\s*([^<>\s]+)\s*>\s*$/;

function isValidEmail(value) {
  return typeof value === 'string' && EMAIL_RE.test(value.trim());
}

/**
 * Split one spreadsheet cell into individual addresses.
 *
 * Site contact lists routinely put several addresses in a single cell,
 * separated by semicolons, commas, slashes or newlines, and often decorated
 * with a display name. Returns `{ contacts, invalid }` so the caller can
 * surface the entries that could not be understood rather than dropping them.
 */
function parseAddressCell(value) {
  const contacts = [];
  const invalid = [];
  if (value === null || value === undefined) return { contacts, invalid };

  const text = String(value).trim();
  if (!text) return { contacts, invalid };

  // Split on separators, but not on a comma inside a quoted display name.
  const parts = text
    .split(/[;\n\r]+|,(?=(?:[^"]*"[^"]*")*[^"]*$)|\s+\/\s+/)
    .map((p) => p.trim())
    .filter(Boolean);

  for (const part of parts) {
    const named = NAMED_RE.exec(part);
    if (named) {
      const name = (named[1] || named[2] || '').trim();
      const email = named[3].trim();
      if (isValidEmail(email)) contacts.push({ name, email });
      else invalid.push(part);
      continue;
    }
    if (isValidEmail(part)) {
      contacts.push({ name: '', email: part });
      continue;
    }
    // "Jane Bloggs j.bloggs@nhs.net" with no angle brackets.
    const loose = part.match(/[^\s<>,;]+@[^\s<>,;]+\.[^\s<>,;]{2,}/);
    if (loose && isValidEmail(loose[0])) {
      contacts.push({ name: part.replace(loose[0], '').replace(/[<>()]/g, '').trim(), email: loose[0] });
    } else {
      invalid.push(part);
    }
  }
  return { contacts, invalid };
}

function normaliseEmail(email) {
  return String(email || '').trim().toLowerCase();
}

/** Format for a mail header, quoting the display name only when required. */
function formatAddress(contact) {
  if (!contact || !contact.email) return '';
  const name = (contact.name || '').trim();
  if (!name) return contact.email;
  const needsQuoting = /[",;:<>@()\[\]\\.]/.test(name);
  const safe = name.replace(/(["\\])/g, '\\$1');
  return `${needsQuoting ? `"${safe}"` : name} <${contact.email}>`;
}

/** De-duplicate by address, keeping the first entry that carries a name. */
function dedupeContacts(contacts) {
  const byEmail = new Map();
  for (const contact of contacts) {
    const key = normaliseEmail(contact.email);
    if (!key) continue;
    const existing = byEmail.get(key);
    if (!existing) {
      byEmail.set(key, { ...contact, email: contact.email.trim() });
    } else if (!existing.name && contact.name) {
      existing.name = contact.name;
    }
  }
  return [...byEmail.values()];
}

/** "Dr Jane Bloggs" -> "Jane"; falls back to the local part of the address. */
function firstNameOf(contact) {
  const name = (contact && contact.name ? contact.name : '').trim();
  if (name) {
    const titles = /^(dr|mr|mrs|ms|miss|prof|professor|sr|sister|mx)\.?$/i;
    const words = name.split(/\s+/).filter((w) => !titles.test(w));
    if (words.length) return words[0];
  }
  const email = (contact && contact.email) || '';
  const local = email.split('@')[0] || '';
  const guess = local.split(/[._-]/).filter(Boolean)[0] || '';
  return guess ? guess.charAt(0).toUpperCase() + guess.slice(1) : '';
}

module.exports = {
  EMAIL_RE,
  isValidEmail,
  parseAddressCell,
  normaliseEmail,
  formatAddress,
  dedupeContacts,
  firstNameOf,
};

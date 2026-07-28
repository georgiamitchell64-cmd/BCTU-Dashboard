'use strict';

// Turns a parsed spreadsheet into the site/contact model the app works with.
// Kept separate from the ExcelJS file reading so it can be tested with plain
// arrays of objects.

const { parseAddressCell, dedupeContacts, normaliseEmail } = require('./emails');

// Set non-enumerably on each parsed row by the workbook reader, so warnings
// can cite the row number the user sees in Excel rather than an array index.
const ROW_NUMBER = '__rowNumber';

/** Normalise a header for matching: lowercase, letters and digits only. */
function normaliseHeader(header) {
  return String(header || '').toLowerCase().replace(/[^a-z0-9]/g, '');
}

/** Column key used for merge placeholders, e.g. "Site Name" -> "site_name". */
function toFieldKey(header) {
  return String(header || '')
    .trim()
    .toLowerCase()
    .replace(/[^a-z0-9]+/g, '_')
    .replace(/^_+|_+$/g, '');
}

// Ordered most-specific first, so "site name" wins over a bare "name".
const PATTERNS = {
  siteId: [/^siteid$/, /^siteno$/, /^sitenumber$/, /^sitecode$/, /^siteref$/, /^centreid$/,
    /^centreno$/, /^centrenumber$/, /^centrecode$/, /^centre$/, /^trustcode$/, /^orgcode$/,
    /^site$/, /^id$/, /^code$/],
  siteName: [/^sitename$/, /^centrename$/, /^centername$/, /^hospitalname$/, /^hospital$/,
    /^trustname$/, /^trust$/, /^organisation(name)?$/, /^organization(name)?$/,
    /^institution$/, /^sitetitle$/, /^site$/, /^centre$/, /^name$/],
  status: [/^sitestatus$/, /^recruitmentstatus$/, /^status$/],
  contactName: [/^contactname$/, /^contact$/, /^fullname$/, /^staffname$/, /^personname$/,
    /^name$/],
  firstName: [/^firstname$/, /^forename$/, /^givenname$/],
  lastName: [/^lastname$/, /^surname$/, /^familyname$/],
  role: [/^jobrole$/, /^jobtitle$/, /^role$/, /^position$/, /^title$/, /^responsibility$/],
};

function matchColumn(headers, patterns, taken = new Set()) {
  for (const pattern of patterns) {
    for (const header of headers) {
      if (taken.has(header)) continue;
      if (pattern.test(normaliseHeader(header))) return header;
    }
  }
  return null;
}

/** Columns that look like they hold an email address. */
function findEmailColumns(headers) {
  return headers.filter((h) => {
    const n = normaliseHeader(h);
    return n.includes('email') || n === 'mail' || n.endsWith('mailaddress');
  });
}

/**
 * "PI Email" -> "PI"; "Research Nurse E-mail" -> "Research Nurse".
 * The leftover text is the person's role in the per-site layout.
 */
function roleFromEmailColumn(header) {
  const label = String(header || '')
    .replace(/e-?mail\s*(address)?/gi, ' ')
    .replace(/[_\-]+/g, ' ')
    .replace(/\s+/g, ' ')
    .trim();
  return label || 'Contact';
}

/** Find the "PI Name" column that goes with a "PI Email" column, if present. */
function findPairedNameColumn(headers, emailColumn) {
  const role = normaliseHeader(roleFromEmailColumn(emailColumn));
  if (!role || role === 'contact') return null;
  const candidates = headers.filter((h) => {
    const n = normaliseHeader(h);
    if (n.includes('email') || n === 'mail') return false;
    return n === role || n === `${role}name` || n === `${role}contact` || n === `name${role}`;
  });
  return candidates[0] || null;
}

/**
 * Guess how the sheet is laid out.
 *
 * `per-contact`: one row per person, a single email column.
 * `per-site`:    one row per site, several email columns (PI, nurse, ...).
 */
function detectMapping(headers) {
  const cols = headers.filter((h) => String(h || '').trim() !== '');
  const emailColumns = findEmailColumns(cols);
  const taken = new Set(emailColumns);

  const siteId = matchColumn(cols, PATTERNS.siteId, taken);
  if (siteId) taken.add(siteId);
  const siteName = matchColumn(cols, PATTERNS.siteName, taken);
  if (siteName) taken.add(siteName);
  const status = matchColumn(cols, PATTERNS.status, taken);
  if (status) taken.add(status);

  const layout = emailColumns.length > 1 ? 'per-site' : 'per-contact';

  const mapping = {
    layout,
    siteId: siteId || null,
    siteName: siteName || null,
    status: status || null,
    contactName: null,
    firstName: null,
    lastName: null,
    role: null,
    emailColumns: [],
  };

  if (layout === 'per-site') {
    mapping.emailColumns = emailColumns.map((column) => ({
      column,
      role: roleFromEmailColumn(column),
      nameColumn: findPairedNameColumn(cols, column),
      enabled: true,
    }));
  } else {
    const contactName = matchColumn(cols, PATTERNS.contactName, taken);
    if (contactName) taken.add(contactName);
    const firstName = matchColumn(cols, PATTERNS.firstName, taken);
    if (firstName) taken.add(firstName);
    const lastName = matchColumn(cols, PATTERNS.lastName, taken);
    if (lastName) taken.add(lastName);
    const role = matchColumn(cols, PATTERNS.role, taken);
    if (role) taken.add(role);

    mapping.contactName = contactName;
    mapping.firstName = firstName;
    mapping.lastName = lastName;
    mapping.role = role;
    mapping.emailColumns = emailColumns.map((column) => ({
      column,
      role: null,
      nameColumn: null,
      enabled: true,
    }));
  }

  return mapping;
}

// Contact lists spell the same job several ways — "PI", "P.I.", "Principal
// Investigator" — and sending "just to the PIs" has to catch all of them.
// Each entry is [canonical name, test]. Order matters: the first match wins,
// so the more specific patterns come first.
const ROLE_GROUPS = [
  ['Principal Investigator', /\b(p\.?i\.?|principal\s*investigator|chief\s*investigator|c\.?i\.?|local\s*investigator)\b/i],
  ['Sub-Investigator', /\b(sub\s*-?\s*i\.?|sub\s*-?\s*investigator|co\s*-?\s*investigator)\b/i],
  ['Research Nurse', /\b(research\s*(nurse|practitioner|midwife)|nurse|r\.?g\.?n\.?|ccrn?)\b/i],
  ['Research Team', /\b(research\s*(team|team\s*inbox|group|delivery)|study\s*team|trials?\s*team|trials?\s*unit)\b/i],
  ['R&D', /\b(r\s*&\s*d|r\s*and\s*d|research\s*(and|&)\s*development|r\s*&\s*i|research\s*(office|governance|department))\b/i],
  ['Pharmacy', /\b(pharmac(y|ist)|dispensary)\b/i],
  ['Data Manager', /\b(data\s*(manager|management|entry)|d\.?m\.?)\b/i],
  ['Trial Coordinator', /\b(trials?\s*(co\s*-?\s*ordinator|coordinator|administrator|assistant|manager)|c\.?t\.?a\.?|study\s*co\s*-?\s*ordinator|co\s*-?\s*ordinator|coordinator|administrator|admin)\b/i],
  ['Laboratory', /\b(lab(oratory)?|pathology|biochem)\b/i],
];

/**
 * Group a free-text role into a consistent label so it can be filtered on.
 * Anything unrecognised keeps its original wording rather than being forced
 * into a bucket it does not belong in.
 */
function canonicalRole(role) {
  const text = String(role || '').trim();
  if (!text) return '';
  for (const [name, pattern] of ROLE_GROUPS) {
    if (pattern.test(text)) return name;
  }
  return text;
}

function cellText(row, column) {
  if (!column) return '';
  const value = row[column];
  if (value === null || value === undefined) return '';
  return String(value).trim();
}

function contactNameFor(row, mapping) {
  const full = cellText(row, mapping.contactName);
  if (full) return full;
  const first = cellText(row, mapping.firstName);
  const last = cellText(row, mapping.lastName);
  return [first, last].filter(Boolean).join(' ');
}

/**
 * Build the site list.
 *
 * Rows are grouped by site identifier. Every column that is not consumed by
 * the mapping is kept on the site as an extra field, so it can be used as a
 * `{{placeholder}}` when writing a mail-merge.
 *
 * @returns {{sites: Array, warnings: Array<{row: number, message: string}>}}
 */
function buildSites(rows, mapping, options = {}) {
  const warnings = [];
  const sites = new Map();
  const consumed = new Set(
    [mapping.siteId, mapping.siteName, mapping.status, mapping.contactName,
      mapping.firstName, mapping.lastName, mapping.role,
      ...mapping.emailColumns.map((c) => c.column),
      ...mapping.emailColumns.map((c) => c.nameColumn)].filter(Boolean),
  );

  rows.forEach((row, index) => {
    const rowNumber = row[ROW_NUMBER] || (options.firstDataRow || 2) + index;

    const rawId = cellText(row, mapping.siteId);
    const rawName = cellText(row, mapping.siteName);
    if (!rawId && !rawName) {
      const hasAnything = Object.values(row).some((v) => String(v ?? '').trim() !== '');
      if (hasAnything) warnings.push({ row: rowNumber, message: 'No site name or ID — row skipped' });
      return;
    }

    const siteId = rawId || rawName;
    const siteName = rawName || rawId;
    const key = siteId.toLowerCase();

    let site = sites.get(key);
    if (!site) {
      site = {
        key,
        siteId,
        siteName,
        status: cellText(row, mapping.status),
        contacts: [],
        fields: {},
        rowNumbers: [],
      };
      sites.set(key, site);
    }
    site.rowNumbers.push(rowNumber);
    if (!site.status) site.status = cellText(row, mapping.status);

    // First non-empty value wins, so a site spread over several rows keeps
    // the detail from whichever row actually filled it in.
    for (const [column, value] of Object.entries(row)) {
      if (consumed.has(column)) continue;
      const text = String(value ?? '').trim();
      if (!text) continue;
      const field = toFieldKey(column);
      if (field && !site.fields[field]) site.fields[field] = text;
    }

    const activeColumns = mapping.emailColumns.filter((c) => c.enabled !== false);
    let foundOnRow = 0;

    for (const spec of activeColumns) {
      const cell = row[spec.column];
      const { contacts, invalid } = parseAddressCell(cell);
      for (const bad of invalid) {
        warnings.push({ row: rowNumber, message: `Not a valid email address: "${bad}"` });
      }
      const explicitName = spec.nameColumn ? cellText(row, spec.nameColumn) : contactNameFor(row, mapping);
      const role = spec.role || cellText(row, mapping.role);

      for (const contact of contacts) {
        foundOnRow += 1;
        site.contacts.push({
          name: contact.name || explicitName || '',
          email: contact.email,
          role: role || '',
          roleGroup: canonicalRole(role),
          selected: true,
          sourceRow: rowNumber,
        });
      }
    }

    if (foundOnRow === 0) {
      warnings.push({ row: rowNumber, message: `No email address for "${siteName}"` });
    }
  });

  const list = [...sites.values()].map((site) => {
    const contacts = dedupeContacts(site.contacts).map((c) => ({ ...c, selected: c.selected !== false }));
    return { ...site, contacts };
  });

  list.sort((a, b) => a.siteName.localeCompare(b.siteName, undefined, { numeric: true }));
  return { sites: list, warnings };
}

/** Merge a freshly imported list into the existing one without losing edits. */
function mergeSiteLists(existing, incoming, strategy = 'replace') {
  if (strategy === 'replace') return incoming;

  const byKey = new Map(existing.map((s) => [s.key, { ...s, contacts: [...s.contacts] }]));
  for (const site of incoming) {
    const current = byKey.get(site.key);
    if (!current) {
      byKey.set(site.key, site);
      continue;
    }
    current.siteName = site.siteName || current.siteName;
    current.status = site.status || current.status;
    current.fields = { ...current.fields, ...site.fields };
    const seen = new Set(current.contacts.map((c) => normaliseEmail(c.email)));
    for (const contact of site.contacts) {
      if (!seen.has(normaliseEmail(contact.email))) {
        current.contacts.push(contact);
        seen.add(normaliseEmail(contact.email));
      }
    }
  }
  return [...byKey.values()].sort((a, b) => a.siteName.localeCompare(b.siteName, undefined, { numeric: true }));
}

module.exports = {
  ROW_NUMBER,
  ROLE_GROUPS,
  canonicalRole,
  normaliseHeader,
  toFieldKey,
  detectMapping,
  findEmailColumns,
  roleFromEmailColumn,
  buildSites,
  mergeSiteLists,
};

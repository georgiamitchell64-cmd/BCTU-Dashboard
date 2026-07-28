'use strict';

// Template substitution and the rules that decide who ends up in To/Cc/Bcc.

const { dedupeContacts, firstNameOf, formatAddress } = require('./emails');
const { renderPlaceholdersInHtml, htmlToText, isEmptyHtml } = require('./html');

const PLACEHOLDER_RE = /\{\{\s*([a-zA-Z0-9_]+)\s*(?:\|([^}]*))?\}\}/g;

/** Fields always available in a template, on top of the spreadsheet columns. */
const BUILT_IN_FIELDS = [
  { key: 'site_name', label: 'Site name' },
  { key: 'site_id', label: 'Site ID' },
  { key: 'site_status', label: 'Site status' },
  { key: 'contact_name', label: 'Contact name (per-site emails only)' },
  { key: 'first_name', label: 'Contact first name (per-site emails only)' },
  { key: 'contact_email', label: 'Contact email (per-site emails only)' },
  { key: 'recipient_names', label: 'All recipient names, comma separated' },
  { key: 'site_count', label: 'Number of sites in this send' },
  { key: 'today', label: "Today's date" },
];

function formatToday(date = new Date()) {
  return date.toLocaleDateString('en-GB', { day: 'numeric', month: 'long', year: 'numeric' });
}

function joinNames(names) {
  const clean = names.filter(Boolean);
  if (clean.length === 0) return '';
  if (clean.length === 1) return clean[0];
  return `${clean.slice(0, -1).join(', ')} and ${clean[clean.length - 1]}`;
}

/**
 * Build the substitution context for one outgoing email.
 *
 * `contact` is only set when the send is personalised down to the individual,
 * which is why `{{first_name}}` is unavailable on a combined email.
 */
function buildContext(site, options = {}) {
  const { contact = null, recipients = [], siteCount = 1, today = new Date() } = options;
  const context = {
    site_name: site ? site.siteName : '',
    site_id: site ? site.siteId : '',
    site_status: site ? site.status || '' : '',
    contact_name: contact ? contact.name || '' : '',
    first_name: contact ? firstNameOf(contact) : '',
    contact_email: contact ? contact.email : '',
    recipient_names: joinNames(recipients.map((r) => r.name || firstNameOf(r))),
    site_count: String(siteCount),
    today: formatToday(today),
  };
  if (site && site.fields) {
    for (const [key, value] of Object.entries(site.fields)) {
      if (!(key in context)) context[key] = value;
    }
  }
  return context;
}

/**
 * Replace `{{field}}` placeholders. `{{field|Colleagues}}` supplies a fallback
 * for when the spreadsheet left that column blank.
 *
 * @returns {{text: string, missing: string[]}} `missing` lists placeholders
 * that resolved to nothing and had no fallback, so the UI can warn before send.
 */
function renderTemplate(template, context) {
  const missing = new Set();
  const text = String(template || '').replace(PLACEHOLDER_RE, (match, key, fallback) => {
    const value = context[key];
    if (value !== undefined && value !== null && String(value).trim() !== '') return String(value);
    if (fallback !== undefined) return fallback.trim();
    missing.add(key);
    return '';
  });
  return { text, missing: [...missing] };
}

/** Every placeholder used in a template, for validation and highlighting. */
function placeholdersUsed(template) {
  const found = new Set();
  let match;
  PLACEHOLDER_RE.lastIndex = 0;
  while ((match = PLACEHOLDER_RE.exec(String(template || '')))) found.add(match[1]);
  return [...found];
}

/**
 * Whether a contact should receive this send.
 *
 * `roles` is the set of role groups the user has chosen to write to — empty
 * or absent means everyone. This is how "just the PIs" or "just R&D" is
 * expressed, on top of any individuals unticked by hand.
 */
function contactIsIncluded(contact, roles) {
  if (contact.selected === false) return false;
  if (!roles || roles.length === 0) return true;
  return roles.includes(contact.roleGroup || contact.role || '');
}

/** Contacts of a single site that are in scope for this send. */
function contactsForSite(site, options = {}) {
  return dedupeContacts(site.contacts.filter((c) => contactIsIncluded(c, options.roles)));
}

/** Only the contacts the user has left ticked, for sites that are selected. */
function selectedContacts(sites, options = {}) {
  return dedupeContacts(
    sites.flatMap((site) => site.contacts.filter((c) => contactIsIncluded(c, options.roles))),
  );
}

/** Every distinct role group across a set of sites, with a count of each. */
function roleSummary(sites) {
  const counts = new Map();
  for (const site of sites) {
    for (const contact of site.contacts) {
      const key = contact.roleGroup || contact.role || 'Unspecified';
      counts.set(key, (counts.get(key) || 0) + 1);
    }
  }
  return [...counts.entries()]
    .map(([role, count]) => ({ role, count }))
    .sort((a, b) => b.count - a.count || a.role.localeCompare(b.role));
}

/**
 * Decide the To/Cc/Bcc split for a combined email.
 *
 * One site puts its contacts straight in To. More than one site moves everyone
 * to Bcc so sites cannot see each other's addresses, which means the message
 * still needs something in To — the sender's own address, so the mail looks
 * sane in the recipient's client and in the sender's Sent items.
 */
function buildCombinedRecipients(sites, options = {}) {
  const { senderAddress = '', forceBcc = false, alwaysBccSelfAddress = true, roles = [] } = options;
  const contacts = selectedContacts(sites, { roles });
  const useBcc = forceBcc || sites.length > 1;

  if (!useBcc) {
    return { to: contacts, cc: [], bcc: [], usedBcc: false };
  }

  const self = senderAddress && alwaysBccSelfAddress
    ? [{ name: '', email: senderAddress.trim() }]
    : [];
  return { to: self, cc: [], bcc: contacts, usedBcc: true };
}

/**
 * Build the queue for a mail-merge: one message per site, addressed To that
 * site's contacts, with the subject and body rendered per site.
 *
 * `perContact` splits it further into one message per person, which is what
 * you want when the body opens with "Dear {{first_name}}".
 */
/**
 * Render a template's subject and body against one context.
 *
 * A template is HTML when it carries `bodyHtml`; the plain-text part is then
 * derived from the rendered HTML so the two alternatives always agree.
 */
function renderBody(template, context) {
  const subject = renderTemplate(template.subject, context);

  if (template.bodyHtml && !isEmptyHtml(template.bodyHtml)) {
    const rendered = renderPlaceholdersInHtml(template.bodyHtml, context);
    return {
      subject: subject.text,
      bodyHtml: rendered.html,
      body: htmlToText(rendered.html),
      isHtml: true,
      missing: [...new Set([...subject.missing, ...rendered.missing])],
    };
  }

  const body = renderTemplate(template.body, context);
  return {
    subject: subject.text,
    bodyHtml: null,
    body: body.text,
    isHtml: false,
    missing: [...new Set([...subject.missing, ...body.missing])],
  };
}

function buildMergeQueue(sites, template, options = {}) {
  const { perContact = false, today = new Date(), senderAddress = '', roles = [] } = options;
  const messages = [];

  for (const site of sites) {
    const contacts = contactsForSite(site, { roles });
    if (contacts.length === 0) continue;

    const targets = perContact ? contacts.map((c) => [c]) : [contacts];
    for (const group of targets) {
      const context = buildContext(site, {
        contact: group.length === 1 ? group[0] : null,
        recipients: group,
        siteCount: sites.length,
        today,
      });
      const rendered = renderBody(template, context);
      messages.push({
        siteKey: site.key,
        siteId: site.siteId,
        siteName: site.siteName,
        to: group,
        cc: [],
        bcc: [],
        senderAddress,
        ...rendered,
      });
    }
  }
  return messages;
}

/** The single combined message, rendered against the whole selection. */
function buildCombinedMessage(sites, template, options = {}) {
  const recipients = buildCombinedRecipients(sites, options);
  const all = [...recipients.to, ...recipients.bcc];
  const context = buildContext(sites.length === 1 ? sites[0] : null, {
    recipients: all,
    siteCount: sites.length,
    today: options.today || new Date(),
  });
  if (sites.length !== 1) {
    context.site_name = sites.map((s) => s.siteName).join(', ');
    context.site_id = sites.map((s) => s.siteId).join(', ');
  }
  const rendered = renderBody(template, context);
  return {
    to: recipients.to,
    cc: recipients.cc,
    bcc: recipients.bcc,
    usedBcc: recipients.usedBcc,
    senderAddress: options.senderAddress || '',
    siteName: sites.length === 1 ? sites[0].siteName : `${sites.length} sites`,
    ...rendered,
  };
}

function addressLine(contacts) {
  return contacts.map(formatAddress).join('; ');
}

module.exports = {
  BUILT_IN_FIELDS,
  buildContext,
  renderTemplate,
  renderBody,
  placeholdersUsed,
  contactIsIncluded,
  contactsForSite,
  roleSummary,
  selectedContacts,
  buildCombinedRecipients,
  buildCombinedMessage,
  buildMergeQueue,
  addressLine,
  formatToday,
};

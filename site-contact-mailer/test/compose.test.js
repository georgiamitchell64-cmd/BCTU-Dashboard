'use strict';

const test = require('node:test');
const assert = require('node:assert');

const {
  renderTemplate, buildContext, buildCombinedRecipients, buildCombinedMessage,
  buildMergeQueue, placeholdersUsed,
} = require('../src/shared/compose');
const { buildEml, buildMailto, draftFileName, textToHtml } = require('../src/shared/mailer');

function site(id, name, contacts, fields = {}) {
  return {
    key: id.toLowerCase(),
    siteId: id,
    siteName: name,
    status: 'Recruiting',
    fields,
    contacts: contacts.map((c) => ({ selected: true, role: '', name: '', ...c })),
  };
}

const qe = site('001', 'Queen Elizabeth', [
  { name: 'Jane Bloggs', email: 'jane@qe.nhs.uk', role: 'PI' },
  { name: 'Ade Okoro', email: 'ade@qe.nhs.uk', role: 'Nurse' },
], { region: 'West Midlands' });

const add = site('002', 'Addenbrookes', [{ name: 'Sam Reed', email: 'sam@add.nhs.uk' }]);

// ── Templates ─────────────────────────────────────────────────────────────

test('renderTemplate substitutes fields', () => {
  const context = buildContext(qe, { siteCount: 1 });
  const { text } = renderTemplate('Hello {{site_name}} ({{site_id}})', context);
  assert.strictEqual(text, 'Hello Queen Elizabeth (001)');
});

test('renderTemplate uses spreadsheet columns as fields', () => {
  const context = buildContext(qe, { siteCount: 1 });
  assert.strictEqual(renderTemplate('{{region}}', context).text, 'West Midlands');
});

test('an empty field falls back when given one, and is reported when not', () => {
  const context = buildContext(qe, { siteCount: 1 });
  assert.strictEqual(renderTemplate('Dear {{first_name|colleagues}},', context).text, 'Dear colleagues,');
  const bare = renderTemplate('Dear {{first_name}},', context);
  assert.strictEqual(bare.text, 'Dear ,');
  assert.deepStrictEqual(bare.missing, ['first_name']);
});

test('first_name is available when the email is addressed to one person', () => {
  const context = buildContext(qe, { contact: qe.contacts[0], siteCount: 1 });
  assert.strictEqual(renderTemplate('Dear {{first_name}},', context).text, 'Dear Jane,');
});

test('recipient_names reads as a sentence', () => {
  const context = buildContext(qe, { recipients: qe.contacts, siteCount: 1 });
  assert.strictEqual(context.recipient_names, 'Jane Bloggs and Ade Okoro');
});

test('placeholdersUsed finds every field in a template', () => {
  assert.deepStrictEqual(
    placeholdersUsed('{{site_name}} {{first_name|there}} {{site_name}}').sort(),
    ['first_name', 'site_name'],
  );
});

// ── The To/Bcc rule ───────────────────────────────────────────────────────

test('one site puts its contacts in To', () => {
  const result = buildCombinedRecipients([qe], { senderAddress: 'me@bham.ac.uk' });
  assert.strictEqual(result.usedBcc, false);
  assert.deepStrictEqual(result.to.map((c) => c.email), ['jane@qe.nhs.uk', 'ade@qe.nhs.uk']);
  assert.deepStrictEqual(result.bcc, []);
});

test('more than one site moves everyone to Bcc, with the sender in To', () => {
  const result = buildCombinedRecipients([qe, add], { senderAddress: 'me@bham.ac.uk' });
  assert.strictEqual(result.usedBcc, true);
  assert.deepStrictEqual(result.to.map((c) => c.email), ['me@bham.ac.uk']);
  assert.deepStrictEqual(result.bcc.map((c) => c.email), ['jane@qe.nhs.uk', 'ade@qe.nhs.uk', 'sam@add.nhs.uk']);
});

test('forceBcc applies the Bcc rule to a single site too', () => {
  const result = buildCombinedRecipients([qe], { senderAddress: 'me@bham.ac.uk', forceBcc: true });
  assert.strictEqual(result.usedBcc, true);
  assert.strictEqual(result.bcc.length, 2);
});

test('the same person at two sites is only addressed once', () => {
  const shared = site('003', 'Shared', [{ name: 'Jane Bloggs', email: 'JANE@qe.nhs.uk' }]);
  const result = buildCombinedRecipients([qe, shared], { senderAddress: 'me@bham.ac.uk' });
  assert.strictEqual(result.bcc.filter((c) => c.email.toLowerCase() === 'jane@qe.nhs.uk').length, 1);
});

test('unticked contacts are left out', () => {
  const trimmed = JSON.parse(JSON.stringify(qe));
  trimmed.contacts[1].selected = false;
  const result = buildCombinedRecipients([trimmed], {});
  assert.deepStrictEqual(result.to.map((c) => c.email), ['jane@qe.nhs.uk']);
});

test('with no sender address set, To is simply empty rather than invented', () => {
  const result = buildCombinedRecipients([qe, add], { senderAddress: '' });
  assert.deepStrictEqual(result.to, []);
  assert.strictEqual(result.bcc.length, 3);
});

// ── Whole messages ────────────────────────────────────────────────────────

test('a combined message across sites lists them in the subject field', () => {
  const message = buildCombinedMessage([qe, add], { subject: 'Update for {{site_name}}', body: 'Hello' }, { senderAddress: 'me@bham.ac.uk' });
  assert.strictEqual(message.subject, 'Update for Queen Elizabeth, Addenbrookes');
  assert.strictEqual(message.usedBcc, true);
});

test('a mail merge produces one message per site, addressed To that site', () => {
  const messages = buildMergeQueue([qe, add], { subject: '{{site_name}} update', body: 'Dear {{site_name}} team' });
  assert.strictEqual(messages.length, 2);
  assert.strictEqual(messages[0].subject, 'Queen Elizabeth update');
  assert.deepStrictEqual(messages[0].to.map((c) => c.email), ['jane@qe.nhs.uk', 'ade@qe.nhs.uk']);
  assert.deepStrictEqual(messages[0].bcc, []);
  assert.strictEqual(messages[1].subject, 'Addenbrookes update');
});

test('perContact produces one message per person with their own name', () => {
  const messages = buildMergeQueue([qe], { subject: 'Hi', body: 'Dear {{first_name}},' }, { perContact: true });
  assert.strictEqual(messages.length, 2);
  assert.strictEqual(messages[0].body, 'Dear Jane,');
  assert.strictEqual(messages[1].body, 'Dear Ade,');
  assert.strictEqual(messages[0].to.length, 1);
});

test('a site with no ticked contacts is skipped by the merge', () => {
  const none = site('004', 'Empty', [{ email: 'x@nhs.net', selected: false }]);
  assert.strictEqual(buildMergeQueue([none], { subject: 'a', body: 'b' }).length, 0);
});

// ── Delivery formats ──────────────────────────────────────────────────────

test('the draft carries X-Unsent so Outlook opens it as editable', () => {
  const eml = buildEml({ to: qe.contacts, subject: 'Hello', body: 'Body', senderAddress: 'me@bham.ac.uk' });
  assert.match(eml, /^X-Unsent: 1$/m);
  assert.match(eml, /^From: me@bham\.ac\.uk$/m);
  assert.match(eml, /^To: Jane Bloggs <jane@qe\.nhs\.uk>, Ade Okoro <ade@qe\.nhs\.uk>$/m);
});

test('the draft body survives a round trip, including non-ASCII', () => {
  const body = 'Dear colleagues,\n\nRecruitment is going well — thank you.\nBest wishes';
  const eml = buildEml({ to: [{ email: 'a@nhs.net' }], subject: 'Résumé', body });
  const encoded = eml.split('\r\n\r\n')[1].replace(/\r\n/g, '');
  assert.strictEqual(Buffer.from(encoded, 'base64').toString('utf8'), body);
  // A non-ASCII subject must be RFC 2047 encoded, not passed through raw.
  assert.match(eml, /^Subject: =\?UTF-8\?B\?/m);
});

test('Bcc recipients appear in the draft headers', () => {
  const eml = buildEml({ to: [{ email: 'me@bham.ac.uk' }], bcc: qe.contacts, subject: 'S', body: 'B' });
  assert.match(eml, /^Bcc: .*jane@qe\.nhs\.uk/m);
});

test('an HTML draft escapes the body and keeps line breaks', () => {
  const html = textToHtml('a < b\nsecond line\n\nnew paragraph');
  assert.match(html, /a &lt; b<br>second line/);
  assert.strictEqual((html.match(/<p>/g) || []).length, 2);
});

test('mailto encodes the subject, body and recipients', () => {
  const result = buildMailto({ to: [{ email: 'a@nhs.net' }, { email: 'b@nhs.net' }], bcc: [{ email: 'c@nhs.net' }], subject: 'Hi there', body: 'Line one\nLine two' });
  assert.match(result.url, /^mailto:a@nhs\.net,b@nhs\.net\?/);
  assert.match(result.url, /bcc=c@nhs\.net/);
  assert.match(result.url, /subject=Hi%20there/);
  assert.match(result.url, /body=Line%20one%0ALine%20two/);
});

test('an over-long mailto is flagged rather than silently truncated', () => {
  const short = buildMailto({ to: [{ email: 'a@nhs.net' }], subject: 'Hi', body: 'Short' });
  assert.strictEqual(short.risky, false);
  assert.strictEqual(short.tooLong, false);

  const long = buildMailto({ to: [{ email: 'a@nhs.net' }], subject: 'Hi', body: 'x'.repeat(7000) });
  assert.strictEqual(long.tooLong, true);
  assert.strictEqual(long.risky, true);
});

test('draft file names are ordered and safe for the filesystem', () => {
  assert.strictEqual(draftFileName({ siteId: '001', siteName: 'Queen Elizabeth' }, 0), '001 Queen Elizabeth.eml');
  assert.strictEqual(draftFileName({ siteId: 'A/B', siteName: 'X:Y?' }, 9), '010 XY.eml');
  // Characters Windows forbids in a filename must not survive.
  assert.strictEqual(draftFileName({ siteName: 'A\\B*C|D' }, 0), '001 ABCD.eml');
  assert.strictEqual(draftFileName({ siteId: '007', siteName: '' }, 1), '002 007.eml');
});

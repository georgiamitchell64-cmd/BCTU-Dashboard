'use strict';

const test = require('node:test');
const assert = require('node:assert');

const {
  renderTemplate, buildContext, buildCombinedRecipients, buildCombinedMessage,
  buildMergeQueue, placeholdersUsed,
} = require('../src/shared/compose');
const { buildEml, buildMailto, draftFileName, textToHtml, toNodemailer } = require('../src/shared/mailer');

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

// ── Role filtering: "just the PIs", "just R&D" ────────────────────────────

const { canonicalRole } = require('../src/shared/importer');
const { contactsForSite, roleSummary, renderBody } = require('../src/shared/compose');

const mixed = site('010', 'Mixed Site', [
  { name: 'Dr Jane Bloggs', email: 'pi@nhs.net', role: 'Principal Investigator', roleGroup: 'Principal Investigator' },
  { name: 'Ade Okoro', email: 'nurse@nhs.net', role: 'Research Nurse', roleGroup: 'Research Nurse' },
  { name: 'R&D Office', email: 'rd@nhs.net', role: 'R&D', roleGroup: 'R&D' },
]);

test('canonicalRole groups the ways a spreadsheet writes the same job', () => {
  for (const written of ['PI', 'P.I.', 'Principal Investigator', 'Chief Investigator']) {
    assert.strictEqual(canonicalRole(written), 'Principal Investigator', `failed for "${written}"`);
  }
  for (const written of ['R&D', 'R & D', 'Research and Development', 'R&D Office']) {
    assert.strictEqual(canonicalRole(written), 'R&D', `failed for "${written}"`);
  }
  assert.strictEqual(canonicalRole('Research Nurse'), 'Research Nurse');
  assert.strictEqual(canonicalRole('Pharmacist'), 'Pharmacy');
});

test('an unrecognised role keeps its original wording', () => {
  assert.strictEqual(canonicalRole('Sonographer'), 'Sonographer');
  assert.strictEqual(canonicalRole(''), '');
});

test('no role filter means everyone at the site', () => {
  assert.strictEqual(contactsForSite(mixed, {}).length, 3);
  assert.strictEqual(contactsForSite(mixed, { roles: [] }).length, 3);
});

test('filtering to one role writes only to those people', () => {
  const pis = contactsForSite(mixed, { roles: ['Principal Investigator'] });
  assert.deepStrictEqual(pis.map((c) => c.email), ['pi@nhs.net']);
});

test('several roles can be combined', () => {
  const chosen = contactsForSite(mixed, { roles: ['Principal Investigator', 'R&D'] });
  assert.deepStrictEqual(chosen.map((c) => c.email), ['pi@nhs.net', 'rd@nhs.net']);
});

test('the role filter applies to a combined send too', () => {
  const result = buildCombinedRecipients([mixed, qe], {
    senderAddress: 'me@bham.ac.uk',
    roles: ['Principal Investigator'],
  });
  // qe's contacts have no roleGroup set, so only the PI matches.
  assert.deepStrictEqual(result.bcc.map((c) => c.email), ['pi@nhs.net']);
});

test('a merge skips sites with nobody in the chosen role', () => {
  const noPi = site('011', 'Nurses Only', [
    { email: 'n@nhs.net', role: 'Research Nurse', roleGroup: 'Research Nurse' },
  ]);
  const messages = buildMergeQueue([mixed, noPi], { subject: 'x', body: 'y' },
    { roles: ['Principal Investigator'] });
  assert.strictEqual(messages.length, 1);
  assert.strictEqual(messages[0].siteId, '010');
});

test('roleSummary counts each role across the selection', () => {
  assert.deepStrictEqual(roleSummary([mixed]), [
    { role: 'Principal Investigator', count: 1 },
    { role: 'R&D', count: 1 },
    { role: 'Research Nurse', count: 1 },
  ]);
});

// ── HTML message bodies ───────────────────────────────────────────────────

test('an HTML template renders and carries a plain-text alternative', () => {
  const context = buildContext(qe, { siteCount: 1 });
  const rendered = renderBody({
    subject: '{{site_name}} update',
    bodyHtml: '<p>Dear <b>{{site_name}}</b>,</p><p>Thanks.</p>',
  }, context);

  assert.strictEqual(rendered.isHtml, true);
  assert.strictEqual(rendered.subject, 'Queen Elizabeth update');
  assert.strictEqual(rendered.bodyHtml, '<p>Dear <b>Queen Elizabeth</b>,</p><p>Thanks.</p>');
  assert.strictEqual(rendered.body, 'Dear Queen Elizabeth,\n\nThanks.');
});

test('a plain template is unaffected by the HTML path', () => {
  const context = buildContext(qe, { siteCount: 1 });
  const rendered = renderBody({ subject: 'S', body: 'Plain {{site_name}}' }, context);
  assert.strictEqual(rendered.isHtml, false);
  assert.strictEqual(rendered.bodyHtml, null);
  assert.strictEqual(rendered.body, 'Plain Queen Elizabeth');
});

test('an empty HTML body falls back to the plain body', () => {
  const context = buildContext(qe, { siteCount: 1 });
  const rendered = renderBody({ subject: 'S', bodyHtml: '<p><br></p>', body: 'Fallback' }, context);
  assert.strictEqual(rendered.isHtml, false);
  assert.strictEqual(rendered.body, 'Fallback');
});

test('a merge carries HTML through to every message', () => {
  const messages = buildMergeQueue([qe, add], {
    subject: '{{site_name}}',
    bodyHtml: '<p>Hello {{site_name}}</p>',
  });
  assert.strictEqual(messages.length, 2);
  assert.strictEqual(messages[0].bodyHtml, '<p>Hello Queen Elizabeth</p>');
  assert.strictEqual(messages[1].bodyHtml, '<p>Hello Addenbrookes</p>');
});

// ── MIME structure of rich drafts ─────────────────────────────────────────

const PNG_1PX = 'iVBORw0KGgoAAAANSUhEUgAAAAEAAAABCAYAAAAfFcSJAAAADUlEQVR42mP8z8BQDwAEhQGAhKmMIQAAAABJRU5ErkJggg==';

test('a plain message stays single-part', () => {
  const eml = buildEml({ to: [{ email: 'a@nhs.net' }], subject: 'S', body: 'B' });
  assert.match(eml, /^Content-Type: text\/plain; charset=UTF-8$/m);
  assert.ok(!/multipart/.test(eml));
});

test('an HTML message is multipart/alternative with both parts', () => {
  const eml = buildEml({
    to: [{ email: 'a@nhs.net' }], subject: 'S',
    bodyHtml: '<p>Hello <b>there</b></p>', body: 'Hello there',
  });
  assert.match(eml, /^Content-Type: multipart\/alternative; boundary="(.+)"$/m);
  assert.match(eml, /Content-Type: text\/plain; charset=UTF-8/);
  assert.match(eml, /Content-Type: text\/html; charset=UTF-8/);
});

test('a pasted image becomes a related part referenced by cid', () => {
  const eml = buildEml({
    to: [{ email: 'a@nhs.net' }], subject: 'S',
    bodyHtml: `<p><img src="data:image/png;base64,${PNG_1PX}"></p>`,
  });
  assert.match(eml, /^Content-Type: multipart\/related;/m);
  assert.match(eml, /Content-ID: <img1@site-contact-mailer>/);
  assert.match(eml, /Content-Disposition: inline; filename="image1\.png"/);
  // The data: URI must not survive into the sent HTML.
  const html = Buffer.from(
    /Content-Type: text\/html[\s\S]*?\r\n\r\n([A-Za-z0-9+/=\r\n]+)/.exec(eml)[1].replace(/\s/g, ''),
    'base64',
  ).toString('utf8');
  assert.match(html, /src="cid:img1@site-contact-mailer"/);
  assert.ok(!/data:image/.test(html));
});

test('attachments wrap the message in multipart/mixed', () => {
  const eml = buildEml({
    to: [{ email: 'a@nhs.net' }], subject: 'S', body: 'B',
    attachments: [{ fileName: 'protocol.pdf', contentType: 'application/pdf', base64: 'JVBERi0=' }],
  });
  assert.match(eml, /^Content-Type: multipart\/mixed;/m);
  assert.match(eml, /Content-Type: application\/pdf; name="protocol\.pdf"/);
  assert.match(eml, /Content-Disposition: attachment; filename="protocol\.pdf"/);
});

test('an attachment with a non-ASCII name is RFC 2231 encoded', () => {
  const eml = buildEml({
    to: [{ email: 'a@nhs.net' }], subject: 'S', body: 'B',
    attachments: [{ fileName: 'protocole-résumé.pdf', contentType: 'application/pdf', base64: 'JVBERi0=' }],
  });
  assert.match(eml, /filename\*=UTF-8''protocole-r%C3%A9sum%C3%A9\.pdf/);
});

test('nodemailer gets html, a text alternative and attachments', () => {
  const payload = toNodemailer({
    to: [{ email: 'a@nhs.net' }], subject: 'S',
    bodyHtml: '<p>Hi</p>', body: 'Hi',
    attachments: [{ fileName: 'x.pdf', contentType: 'application/pdf', base64: 'AAA' }],
  }, { from: 'me@bham.ac.uk' });
  assert.match(payload.html, /<p>Hi<\/p>/);
  assert.strictEqual(payload.text, 'Hi');
  assert.strictEqual(payload.attachments[0].filename, 'x.pdf');
  assert.strictEqual(payload.attachments[0].encoding, 'base64');
});

// ── Recruitment merge fields and charts ───────────────────────────────────

const RECRUITMENT = {
  sites: [
    { key: '001', siteId: '001', siteName: 'Queen Elizabeth', randomised: 12, target: null, opened: '07-04-2026', monthly: [{ month: '2026-04', count: 12 }], rank: 2, of: 3, quartile: 2, percentOfTarget: null },
    { key: '002', siteId: '002', siteName: 'Addenbrookes', randomised: 20, target: 40, opened: null, monthly: [{ month: '2026-04', count: 20 }], rank: 1, of: 3, quartile: 1, percentOfTarget: 50 },
    { key: '003', siteId: '003', siteName: 'Royal Free', randomised: 2, target: null, opened: null, monthly: [{ month: '2026-04', count: 2 }], rank: 3, of: 3, quartile: 3, percentOfTarget: null },
  ],
  months: ['2026-04'],
  totals: { randomised: 34, target: 40, siteCount: 3, monthly: [{ month: '2026-04', count: 34 }] },
};

test('recruitment numbers reach the merge fields', () => {
  const context = buildContext(qe, { siteCount: 1, recruitment: RECRUITMENT });
  assert.strictEqual(context.site_randomised, '12');
  assert.strictEqual(context.site_rank, '2');
  assert.strictEqual(context.site_rank_of, '2 of 3');
  assert.strictEqual(context.trial_randomised, '34');
  assert.strictEqual(context.trial_sites, '3');
  assert.strictEqual(context.site_opened, '07-04-2026');
});

test('a missing target falls back to one from the contact list', () => {
  // The recruitment export has no target for site 001; the contact sheet does.
  const withTarget = { ...qe, fields: { ...qe.fields, target: '30' } };
  const context = buildContext(withTarget, { siteCount: 1, recruitment: RECRUITMENT });
  assert.strictEqual(context.site_target, '30');
  assert.strictEqual(context.site_percent, '40%');
  assert.match(context.progress_chart, /40%/);
});

test('a site with no recruitment row gets no figures rather than wrong ones', () => {
  const unknown = site('999', 'Nowhere General', [{ email: 'a@nhs.net' }]);
  const context = buildContext(unknown, { siteCount: 1, recruitment: RECRUITMENT });
  assert.strictEqual(context.site_randomised, undefined);
  assert.strictEqual(context.progress_chart, undefined);
  // The trial-wide chart still applies, since it does not depend on the site.
  assert.ok(context.overall_chart);
});

test('chart placeholders are inserted as HTML, not escaped text', () => {
  const messages = buildMergeQueue([qe], {
    subject: 'x',
    bodyHtml: '<p>Here:</p>{{recruitment_chart}}',
  }, { recruitment: RECRUITMENT });
  assert.match(messages[0].bodyHtml, /<table/);
  assert.ok(!messages[0].bodyHtml.includes('&lt;table'), 'chart HTML must not be escaped');
});

test('ordinary fields are still escaped alongside raw chart fields', () => {
  const risky = site('004', "Guy's & St Thomas'", [{ email: 'a@nhs.net' }]);
  const messages = buildMergeQueue([risky], {
    subject: 'x',
    bodyHtml: '<p>{{site_name}}</p>{{overall_chart}}',
  }, { recruitment: RECRUITMENT });
  assert.match(messages[0].bodyHtml, /Guy&#39;s &amp; St Thomas&#39;|Guy's &amp; St Thomas'/);
  assert.match(messages[0].bodyHtml, /<table/);
});

test('without recruitment data the chart fields are simply absent', () => {
  const context = buildContext(qe, { siteCount: 1 });
  assert.strictEqual(context.recruitment_chart, undefined);
  const messages = buildMergeQueue([qe], { subject: 'x', bodyHtml: '{{recruitment_chart}}' }, {});
  assert.deepStrictEqual(messages[0].missing, ['recruitment_chart']);
});

test('the built-in monthly template renders with no missing fields', () => {
  const { MONTHLY_RECRUITMENT } = require('../src/shared/templates');
  const withTarget = { ...qe, fields: { ...qe.fields, target: '30' } };
  const messages = buildMergeQueue([withTarget], MONTHLY_RECRUITMENT, {
    recruitment: RECRUITMENT, perContact: true,
  });
  assert.deepStrictEqual(messages[0].missing, []);
  assert.match(messages[0].subject, /Queen Elizabeth — TONIC recruitment update/);
  assert.match(messages[0].body, /You have randomised 12 participants/);
});

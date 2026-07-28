'use strict';

const test = require('node:test');
const assert = require('node:assert');

const {
  cleanPastedHtml, sanitizeHtml, htmlToText, normalisePlaceholders,
  renderPlaceholdersInHtml, extractDataImages, isEmptyHtml, isSafeUrl,
} = require('../src/shared/html');

// ── Cleaning what Outlook puts on the clipboard ───────────────────────────

test('Office conditional comments are removed', () => {
  const html = 'Before<!--[if gte mso 9]><xml><o:OfficeDocumentSettings/></xml><![endif]-->After';
  assert.strictEqual(cleanPastedHtml(html), 'BeforeAfter');
});

test('Office namespace tags are unwrapped', () => {
  assert.strictEqual(cleanPastedHtml('<p>Text<o:p></o:p></p>'), '<p>Text</p>');
});

test('mso- style properties are stripped but real ones survive', () => {
  const html = '<p style="mso-line-height:1; color:red; mso-fareast-font:x">Hi</p>';
  assert.strictEqual(cleanPastedHtml(html), '<p style="color:red">Hi</p>');
});

test('a style attribute left empty by cleaning is removed entirely', () => {
  assert.strictEqual(cleanPastedHtml('<p style="mso-line-height:1">Hi</p>'), '<p >Hi</p>');
});

test('Word class names are dropped', () => {
  assert.strictEqual(cleanPastedHtml('<p class="MsoNormal">Hi</p>'), '<p>Hi</p>');
});

// ── Sanitising ────────────────────────────────────────────────────────────

test('script tags and their contents are removed', () => {
  assert.strictEqual(sanitizeHtml('<p>Hi</p><script>alert(1)</script>'), '<p>Hi</p>');
});

test('event handler attributes are stripped', () => {
  const out = sanitizeHtml('<p onclick="steal()" style="color:red">Hi</p>');
  assert.ok(!/onclick/i.test(out));
  assert.match(out, /style="color:red"/);
});

test('javascript: links are dropped but the text survives', () => {
  const out = sanitizeHtml('<a href="javascript:alert(1)">Click</a>');
  assert.ok(!/javascript:/i.test(out));
  assert.match(out, /Click/);
});

test('formatting tags and inline styles are preserved', () => {
  const html = '<p style="text-align:center"><b>Bold</b> <i>italic</i> '
    + '<span style="color:#ff0000;font-family:Arial">red</span></p>';
  assert.strictEqual(sanitizeHtml(html), html);
});

test('tables and links survive sanitising', () => {
  const html = '<table><tr><td style="border:1px solid #000">'
    + '<a href="https://bctu.ac.uk">Link</a></td></tr></table>';
  assert.strictEqual(sanitizeHtml(html), html);
});

test('data: image sources are allowed, other data: URIs are not', () => {
  assert.ok(isSafeUrl('data:image/png;base64,AAAA', { allowData: true }));
  assert.ok(!isSafeUrl('data:text/html;base64,AAAA', { allowData: true }));
  assert.ok(!isSafeUrl('java\tscript:alert(1)'));
});

test('iframes and forms are removed with their contents', () => {
  const out = sanitizeHtml('<p>Keep</p><iframe src="http://x"></iframe><form><input></form>');
  assert.strictEqual(out, '<p>Keep</p>');
});

// ── Plain-text alternative ────────────────────────────────────────────────

test('htmlToText keeps paragraph and line breaks', () => {
  assert.strictEqual(
    htmlToText('<p>Dear colleagues,</p><p>Line one<br>Line two</p>'),
    'Dear colleagues,\n\nLine one\nLine two',
  );
});

test('htmlToText renders lists as bullets', () => {
  assert.strictEqual(htmlToText('<ul><li>One</li><li>Two</li></ul>'), '• One\n• Two');
});

test('htmlToText keeps a link target that differs from its text', () => {
  assert.strictEqual(
    htmlToText('<a href="https://bctu.ac.uk">our site</a>'),
    'our site (https://bctu.ac.uk)',
  );
  // No point repeating the address when the label already is the address.
  assert.strictEqual(htmlToText('<a href="mailto:a@nhs.net">a@nhs.net</a>'), 'a@nhs.net');
});

test('htmlToText decodes entities', () => {
  assert.strictEqual(htmlToText('<p>Guy&#39;s &amp; St Thomas&rsquo;&nbsp;NHS</p>'), "Guy's & St Thomas’ NHS");
});

test('isEmptyHtml sees through empty markup but not an image', () => {
  assert.ok(isEmptyHtml('<p></p><div><br></div>'));
  assert.ok(isEmptyHtml(''));
  assert.ok(!isEmptyHtml('<p>Hi</p>'));
  assert.ok(!isEmptyHtml('<p><img src="cid:x"></p>'));
});

// ── Placeholders inside formatted text ────────────────────────────────────

test('a placeholder split by formatting tags is repaired', () => {
  // Selecting part of {{site_name}} and bolding it produces exactly this.
  assert.strictEqual(normalisePlaceholders('{{site_<b>name</b>}}'), '{{site_name}}');
  assert.strictEqual(
    normalisePlaceholders('<span style="color:red">{{</span>first_name}}'),
    '<span style="color:red">{{first_name}}',
  );
});

test('normalisePlaceholders leaves ordinary braces alone', () => {
  assert.strictEqual(normalisePlaceholders('{{not a field}}'), '{{not a field}}');
});

test('placeholders in HTML are substituted and escaped', () => {
  const result = renderPlaceholdersInHtml(
    '<p>Dear <b>{{site_name}}</b></p>',
    { site_name: "Guy's & St Thomas'" },
  );
  assert.strictEqual(result.html, '<p>Dear <b>Guy&#39;s &amp; St Thomas&#39;</b></p>'
    .replace('&#39;s', "'s").replace('Thomas&#39;', "Thomas'"));
  // The ampersand must be escaped so it cannot break the surrounding markup.
  assert.match(result.html, /&amp;/);
});

test('a split placeholder still resolves end to end', () => {
  const result = renderPlaceholdersInHtml('<p>{{site_<b>name</b>}}</p>', { site_name: 'QE' });
  assert.strictEqual(result.html, '<p>QE</p>');
});

test('HTML placeholders honour fallbacks and report what is missing', () => {
  const withFallback = renderPlaceholdersInHtml('<p>Dear {{first_name|colleagues}},</p>', {});
  assert.strictEqual(withFallback.html, '<p>Dear colleagues,</p>');

  const without = renderPlaceholdersInHtml('<p>Dear {{first_name}},</p>', {});
  assert.deepStrictEqual(without.missing, ['first_name']);
});

// ── Inline images ─────────────────────────────────────────────────────────

test('data: images become cid references plus attachment parts', () => {
  const html = '<p><img src="data:image/png;base64,AAAB" alt="logo"></p>';
  const { html: out, images } = extractDataImages(html);
  assert.strictEqual(images.length, 1);
  assert.strictEqual(images[0].contentType, 'image/png');
  assert.strictEqual(images[0].base64, 'AAAB');
  assert.strictEqual(images[0].fileName, 'image1.png');
  assert.match(out, new RegExp(`src="cid:${images[0].cid}"`));
});

test('a jpeg keeps a sensible file extension', () => {
  const { images } = extractDataImages('<img src="data:image/jpeg;base64,AAAB">');
  assert.strictEqual(images[0].fileName, 'image1.jpg');
});

test('remote images are left untouched', () => {
  const html = '<img src="https://bctu.ac.uk/logo.png">';
  const { html: out, images } = extractDataImages(html);
  assert.strictEqual(images.length, 0);
  assert.strictEqual(out, html);
});

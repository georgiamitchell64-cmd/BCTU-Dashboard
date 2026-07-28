'use strict';

// Turns a composed message into something a mail client will open.
//
// Three delivery routes, in the order most trial teams will want them:
//
//  1. `eml`    - write an RFC 5322 file carrying `X-Unsent: 1` and open it.
//                Outlook (and Windows Mail) treat that header as "this is a
//                draft", so it opens in a normal compose window with a Send
//                button, the user's own signature and Sent-items record. No
//                credentials, no IT involvement. Handles long bodies.
//  2. `mailto` - hand the address list to the default client over a mailto:
//                URL. Universally supported but length-limited, so it is only
//                safe for short messages and modest recipient lists.
//  3. `smtp`   - send directly. Needs a server that will accept the account,
//                which in most NHS/university tenants means an app password
//                or a departmental relay.

const { formatAddress } = require('./emails');
const { htmlToText, wrapHtmlDocument, extractDataImages, textToHtmlFragment } = require('./html');

let boundaryCounter = 0;

/** Unique MIME boundary. Deterministic per process so tests can rely on it. */
function makeBoundary(label) {
  boundaryCounter += 1;
  return `----=_SCM_${label}_${Date.now().toString(36)}_${boundaryCounter}`;
}

/** Reset boundary numbering — used by tests to get stable output. */
function resetBoundaries() {
  boundaryCounter = 0;
}

// Conservative: Outlook's own command line tops out around 8k, older
// Windows shell handlers cut off far sooner, so warn well before that.
const MAILTO_SAFE_LENGTH = 1800;
const MAILTO_HARD_LENGTH = 6000;

function encodeHeaderWord(text) {
  const value = String(text || '');
  // Plain ASCII needs no encoding; anything else goes as RFC 2047 base64.
  if (/^[\x20-\x7E]*$/.test(value)) return value;
  return `=?UTF-8?B?${Buffer.from(value, 'utf8').toString('base64')}?=`;
}

function foldBase64(base64) {
  return (base64.match(/.{1,76}/g) || []).join('\r\n');
}

/** Wrap a plain-text body as HTML, preserving the line breaks the user typed. */
function textToHtml(text) {
  return wrapHtmlDocument(`\n${textToHtmlFragment(text)}\n`);
}

function headerAddressList(contacts) {
  return contacts
    .map((c) => {
      const formatted = formatAddress(c);
      const named = /^(.*)\s<([^>]+)>$/.exec(formatted);
      if (!named) return formatted;
      return `${encodeHeaderWord(named[1].replace(/^"|"$/g, ''))} <${named[2]}>`;
    })
    .join(', ');
}

/** One MIME part: headers, blank line, base64 body. */
function mimePart(contentType, extraHeaders, content) {
  return [
    `Content-Type: ${contentType}`,
    ...extraHeaders,
    'Content-Transfer-Encoding: base64',
    '',
    foldBase64(Buffer.from(content, 'utf8').toString('base64')),
  ].join('\r\n');
}

function encodeFileNameParam(name) {
  const safe = String(name || 'attachment');
  // RFC 2231 for non-ASCII filenames, plain quoting otherwise.
  if (/^[\x20-\x7E]*$/.test(safe)) return `"${safe.replace(/"/g, '')}"`;
  return `UTF-8''${encodeURIComponent(safe)}`;
}

function attachmentPart(attachment) {
  const disposition = attachment.inline ? 'inline' : 'attachment';
  const name = encodeFileNameParam(attachment.fileName);
  const isPlain = /^"/.test(name);
  const headers = [
    `Content-Disposition: ${disposition}; ${isPlain ? `filename=${name}` : `filename*=${name}`}`,
  ];
  if (attachment.cid) headers.push(`Content-ID: <${attachment.cid}>`);
  return [
    `Content-Type: ${attachment.contentType || 'application/octet-stream'}; ${isPlain ? `name=${name}` : `name*=${name}`}`,
    ...headers,
    'Content-Transfer-Encoding: base64',
    '',
    foldBase64(attachment.base64),
  ].join('\r\n');
}

function multipart(subtype, parts, boundary, extraParams = '') {
  const body = parts.map((part) => `--${boundary}\r\n${part}`).join('\r\n');
  return {
    contentType: `multipart/${subtype}; boundary="${boundary}"${extraParams}`,
    body: `${body}\r\n--${boundary}--`,
  };
}

/**
 * Build a draft .eml.
 *
 * The structure is chosen from what the message actually contains, because
 * every extra layer is another thing a mail client can render badly:
 *
 *   text only, no files            -> text/plain
 *   HTML                           -> multipart/alternative (plain + HTML)
 *   HTML with pasted images        -> multipart/related wrapping the above
 *   anything with attachments      -> multipart/mixed wrapping the above
 *
 * The plain-text alternative is always included with an HTML message: it is
 * what recipients on restricted mail clients see, and its absence is a common
 * reason for a message to score as spam.
 */
function buildEml(message, options = {}) {
  const { format = 'plain' } = options;
  const attachments = (message.attachments || []).filter((a) => a && a.base64);

  const wantsHtml = format === 'html' || Boolean(message.bodyHtml);
  let htmlBody = null;
  let inlineImages = [];

  if (wantsHtml) {
    const raw = message.bodyHtml ? wrapHtmlDocument(message.bodyHtml) : textToHtml(message.body);
    // Inline base64 images have to become real parts: most clients refuse to
    // render a data: URI in a received message.
    const extracted = extractDataImages(raw);
    htmlBody = extracted.html;
    inlineImages = extracted.images.map((image) => ({ ...image, inline: true }));
  }

  const plainBody = message.body !== undefined && message.body !== null && String(message.body) !== ''
    ? String(message.body)
    : htmlToText(htmlBody || '');

  let contentType;
  let body;

  if (!wantsHtml) {
    contentType = 'text/plain; charset=UTF-8';
    body = foldBase64(Buffer.from(plainBody, 'utf8').toString('base64'));
  } else {
    const alternative = multipart('alternative', [
      mimePart('text/plain; charset=UTF-8', [], plainBody),
      mimePart('text/html; charset=UTF-8', [], htmlBody),
    ], makeBoundary('ALT'));

    let current = alternative;
    if (inlineImages.length) {
      const relatedParts = [
        `Content-Type: ${current.contentType}\r\n\r\n${current.body}`,
        ...inlineImages.map(attachmentPart),
      ];
      current = multipart('related', relatedParts, makeBoundary('REL'), '; type="multipart/alternative"');
    }
    contentType = current.contentType;
    body = current.body;
  }

  if (attachments.length) {
    const inner = `Content-Type: ${contentType}\r\n${wantsHtml ? '' : 'Content-Transfer-Encoding: base64\r\n'}\r\n${body}`;
    const mixed = multipart('mixed', [inner, ...attachments.map(attachmentPart)], makeBoundary('MIX'));
    contentType = mixed.contentType;
    body = mixed.body;
  }

  const headers = [];
  if (message.senderAddress) headers.push(`From: ${message.senderAddress}`);
  if (message.to && message.to.length) headers.push(`To: ${headerAddressList(message.to)}`);
  if (message.cc && message.cc.length) headers.push(`Cc: ${headerAddressList(message.cc)}`);
  if (message.bcc && message.bcc.length) headers.push(`Bcc: ${headerAddressList(message.bcc)}`);
  headers.push(`Subject: ${encodeHeaderWord(message.subject || '')}`);
  // Tells Outlook to open this as an editable draft rather than a read message.
  headers.push('X-Unsent: 1');
  headers.push('MIME-Version: 1.0');
  headers.push(`Content-Type: ${contentType}`);
  // A single-part body is base64; multipart bodies carry per-part encodings.
  if (!/^multipart\//.test(contentType)) headers.push('Content-Transfer-Encoding: base64');
  headers.push('');

  return `${headers.join('\r\n')}\r\n${body}\r\n`;
}

/**
 * mailto: encoding. Note `encodeURIComponent` leaves `'` and `!` alone, which
 * some Windows handlers mangle, so they are escaped explicitly.
 */
function encodeMailtoComponent(text) {
  return encodeURIComponent(String(text || ''))
    .replace(/'/g, '%27')
    .replace(/!/g, '%21')
    .replace(/\*/g, '%2A');
}

/**
 * Address lists keep their `@` and `,` literal. Both are legal here, and some
 * Windows mail handlers fail to decode a percent-encoded `@` in a recipient.
 */
function encodeAddressList(contacts) {
  return contacts
    .map((c) => encodeMailtoComponent(c.email).replace(/%40/g, '@'))
    .join(',');
}

function buildMailto(message) {
  const params = [];
  if (message.cc && message.cc.length) params.push(`cc=${encodeAddressList(message.cc)}`);
  if (message.bcc && message.bcc.length) params.push(`bcc=${encodeAddressList(message.bcc)}`);
  if (message.subject) params.push(`subject=${encodeMailtoComponent(message.subject)}`);
  if (message.body) params.push(`body=${encodeMailtoComponent(message.body)}`);

  const url = `mailto:${encodeAddressList(message.to || [])}${params.length ? `?${params.join('&')}` : ''}`;
  return {
    url,
    length: url.length,
    // Long mailto URLs are silently truncated rather than rejected, which
    // would quietly send a half-written email — so flag it before that.
    tooLong: url.length > MAILTO_HARD_LENGTH,
    risky: url.length > MAILTO_SAFE_LENGTH,
  };
}

/**
 * Filename for a draft, e.g. "003 Queen Elizabeth Hospital.eml". The numeric
 * prefix keeps the folder in send order; the site ID is only added when there
 * is no site name to use.
 */
function draftFileName(message, index = 0) {
  const stem = String(message.siteName || message.siteId || '')
    .replace(/[<>:"/\\|?*\x00-\x1F]/g, '')
    .replace(/\s+/g, ' ')
    .trim()
    .slice(0, 60);
  const prefix = String(index + 1).padStart(3, '0');
  return `${prefix} ${stem || 'email'}.eml`;
}

/** Shape a message for nodemailer, which wants header-formatted strings. */
function toNodemailer(message, options = {}) {
  const payload = {
    from: options.from || message.senderAddress,
    subject: message.subject || '',
  };
  if (message.to && message.to.length) payload.to = message.to.map(formatAddress);
  if (message.cc && message.cc.length) payload.cc = message.cc.map(formatAddress);
  if (message.bcc && message.bcc.length) payload.bcc = message.bcc.map(formatAddress);
  if (message.bodyHtml) {
    payload.html = wrapHtmlDocument(message.bodyHtml);
    // Always send the text alternative alongside it.
    payload.text = message.body || htmlToText(message.bodyHtml);
  } else if (options.format === 'html') {
    payload.html = textToHtml(message.body);
    payload.text = String(message.body || '');
  } else {
    payload.text = String(message.body || '');
  }
  if (message.attachments && message.attachments.length) {
    payload.attachments = message.attachments.map((a) => ({
      filename: a.fileName,
      content: a.base64,
      encoding: 'base64',
      contentType: a.contentType,
      ...(a.cid ? { cid: a.cid } : {}),
    }));
  }
  if (options.replyTo) payload.replyTo = options.replyTo;
  return payload;
}

module.exports = {
  MAILTO_SAFE_LENGTH,
  MAILTO_HARD_LENGTH,
  resetBoundaries,
  buildEml,
  buildMailto,
  textToHtml,
  draftFileName,
  toNodemailer,
  encodeHeaderWord,
};

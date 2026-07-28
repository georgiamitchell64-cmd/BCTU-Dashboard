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

function escapeHtml(text) {
  return String(text || '')
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;');
}

/** Wrap a plain-text body as HTML, preserving the line breaks the user typed. */
function textToHtml(text) {
  const paragraphs = String(text || '')
    .split(/\n{2,}/)
    .map((block) => `<p>${escapeHtml(block).replace(/\n/g, '<br>')}</p>`)
    .join('\n');
  return `<html><body style="font-family:Calibri,Arial,sans-serif;font-size:11pt;color:#000;">\n${paragraphs}\n</body></html>`;
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

/**
 * Build a draft .eml. `format` is 'plain' or 'html'.
 *
 * The message is intentionally single-part: Outlook's draft handling is more
 * reliable without multipart/alternative, and the user is going to review the
 * message in their client before sending anyway.
 */
function buildEml(message, options = {}) {
  const { format = 'plain' } = options;
  const lines = [];

  if (message.senderAddress) lines.push(`From: ${message.senderAddress}`);
  if (message.to && message.to.length) lines.push(`To: ${headerAddressList(message.to)}`);
  if (message.cc && message.cc.length) lines.push(`Cc: ${headerAddressList(message.cc)}`);
  if (message.bcc && message.bcc.length) lines.push(`Bcc: ${headerAddressList(message.bcc)}`);
  lines.push(`Subject: ${encodeHeaderWord(message.subject || '')}`);
  lines.push('X-Unsent: 1');
  lines.push('MIME-Version: 1.0');
  lines.push(`Content-Type: text/${format === 'html' ? 'html' : 'plain'}; charset=UTF-8`);
  lines.push('Content-Transfer-Encoding: base64');
  lines.push('');

  const body = format === 'html' ? textToHtml(message.body) : String(message.body || '');
  lines.push(foldBase64(Buffer.from(body, 'utf8').toString('base64')));

  return `${lines.join('\r\n')}\r\n`;
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
  if (options.format === 'html') payload.html = textToHtml(message.body);
  else payload.text = String(message.body || '');
  if (options.replyTo) payload.replyTo = options.replyTo;
  return payload;
}

module.exports = {
  MAILTO_SAFE_LENGTH,
  MAILTO_HARD_LENGTH,
  buildEml,
  buildMailto,
  textToHtml,
  draftFileName,
  toNodemailer,
  encodeHeaderWord,
};

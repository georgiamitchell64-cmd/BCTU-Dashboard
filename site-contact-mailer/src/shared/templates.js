'use strict';

// Templates shipped with the app. They appear in the Templates menu below the
// user's own and cannot be overwritten — loading one copies it into the editor.

const fs = require('fs');
const path = require('path');

const LOGO_FILE = path.join(__dirname, '..', 'assets', 'tonic-logo.png');
let logoTag = null;

/**
 * The TONIC logo as an inline `data:` image.
 *
 * It goes in as a data URI so the template is a self-contained string; the
 * mailer turns every data: image into a proper `cid:` attachment when the
 * message is built, because most clients refuse to render a data URI in
 * received mail. Read once and cached.
 */
function logoHtml() {
  if (logoTag !== null) return logoTag;
  try {
    const base64 = fs.readFileSync(LOGO_FILE).toString('base64');
    logoTag = `<img src="data:image/png;base64,${base64}" alt="TONIC" width="150"`
      + ' style="width:150px;height:auto;border:0;display:block;">';
  } catch {
    // A missing logo must not stop a template loading.
    logoTag = '';
  }
  return logoTag;
}

const MONTHLY_RECRUITMENT = {
  id: 'builtin_monthly_recruitment',
  builtIn: true,
  name: 'Monthly recruitment update',
  requires: 'recruitment',
  subject: '{{site_name}} — TONIC recruitment update, {{today}}',
  bodyHtml: [
    // The trial's logo, so the message is recognisably from TONIC. It goes in
    // as a data: URI and the mailer converts it to a cid: attachment on send.
    `<p>${logoHtml()}</p>`,
    '<p>Dear {{first_name|colleagues}},</p>',
    '<p>Thank you for your continued work on TONIC. Here is where',
    ' <strong>{{site_name}}</strong> stands this month.</p>',
    '<p><strong>You have randomised {{site_randomised}} participants',
    ' and are currently {{site_rank_of}} sites.</strong></p>',
    '{{recruitment_chart}}',
    '{{progress_chart}}',
    '<p>Across the whole trial we have now randomised',
    ' {{trial_randomised}} participants at {{trial_sites}} sites.</p>',
    '{{overall_chart}}',
    '<p>If there is anything holding up recruitment at your site — screening,',
    ' staffing, pharmacy — please do let us know and we will help where we can.</p>',
    '<p>With thanks,<br>The TONIC trial team</p>',
  ].join(''),
};

const BUILT_IN_TEMPLATES = [MONTHLY_RECRUITMENT];

/** Built-in templates that make sense given what has been imported. */
function availableBuiltIns({ hasRecruitment = false } = {}) {
  return BUILT_IN_TEMPLATES.filter((t) => t.requires !== 'recruitment' || hasRecruitment);
}

module.exports = { BUILT_IN_TEMPLATES, MONTHLY_RECRUITMENT, availableBuiltIns };

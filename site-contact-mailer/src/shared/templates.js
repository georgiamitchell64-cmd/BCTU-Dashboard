'use strict';

// Templates shipped with the app. They appear in the Templates menu below the
// user's own and cannot be overwritten — loading one copies it into the editor.

const fs = require('fs');
const path = require('path');

const LOGO_FILE = path.join(__dirname, '..', 'assets', 'bctu-logo.png');
let logoTag = null;

/**
 * The BCTU logo as an inline `data:` image.
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
    logoTag = `<img src="data:image/png;base64,${base64}" alt="University of Birmingham - Birmingham Clinical Trials Unit" width="190"`
      + ' style="width:190px;height:auto;border:0;display:block;">';
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
    // The unit's logo, so the message is recognisably from BCTU. It goes in
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

const DATA_COMPLETENESS = {
  id: 'builtin_data_completeness',
  builtIn: true,
  name: 'Data completeness update',
  requires: 'completeness',
  subject: '{{site_name}} — data completeness, {{today}}',
  bodyHtml: [
    `<p>${logoHtml()}</p>`,
    '<p>Dear {{first_name|colleagues}},</p>',
    '<p>Here is this month&rsquo;s data completeness summary for',
    ' <strong>{{site_name}}</strong>.</p>',
    '<p><strong>{{completeness_headline}}</strong></p>',
    '{{completeness_gauge}}',
    '{{completeness_leaderboard}}',
    '<p>Where your outstanding forms are:</p>',
    '{{completeness_form_chart}}',
    '<p>Across the whole trial, {{trial_completeness}} of forms due have been',
    ' entered, with {{trial_completeness_outstanding}} still outstanding.</p>',
    '<p>If anything is holding up data entry at your site, please let us know',
    ' and we will help where we can.</p>',
    '<p>With thanks,<br>The TONIC trial team</p>',
  ].join(''),
};

const COMPLETENESS_LEAGUE = {
  id: 'builtin_completeness_league',
  builtIn: true,
  name: 'Completeness league table (trophy chase)',
  requires: 'completeness',
  subject: 'TONIC data completeness league table — {{today}}',
  bodyHtml: [
    `<p>${logoHtml()}</p>`,
    '<p>Dear {{first_name|colleagues}},</p>',
    '<p>The race for the data completeness trophy stands as follows.</p>',
    '{{completeness_leaderboard}}',
    '<p><strong>{{site_name}} is {{completeness_position}}</strong> on',
    ' {{completeness}}, {{completeness_vs_average_words}}.</p>',
    '<p>{{completeness_trophy|The top spot is still up for grabs.}}',
    ' Movement since the last update: {{completeness_movement|no change}}.</p>',
    '{{completeness_chart}}',
    '<p>Your completeness by timepoint:</p>',
    '{{completeness_event_chart}}',
    '<p>With thanks,<br>The TONIC trial team</p>',
  ].join(''),
};

const MONTHLY_UPDATE = {
  id: 'builtin_monthly_update',
  builtIn: true,
  name: 'Monthly update: recruitment and completeness',
  requires: 'both',
  subject: '{{site_name}} — TONIC monthly update, {{today}}',
  bodyHtml: [
    `<p>${logoHtml()}</p>`,
    '<p>Dear {{first_name|colleagues}},</p>',
    '<p>Thank you for your continued work on TONIC. Here is where',
    ' <strong>{{site_name}}</strong> stands this month.</p>',
    '<h3 style="font-family:Calibri,Arial,sans-serif;">Recruitment</h3>',
    '<p>You have randomised {{site_randomised}} participants and are currently',
    ' {{site_rank_of}} sites.</p>',
    '{{recruitment_chart}}',
    '<h3 style="font-family:Calibri,Arial,sans-serif;">Data completeness</h3>',
    '<p>{{completeness_headline}}</p>',
    '{{completeness_leaderboard}}',
    '{{completeness_form_chart}}',
    '<p>With thanks,<br>The TONIC trial team</p>',
  ].join(''),
};

const QUERY_CHASE = {
  id: 'builtin_query_chase',
  builtIn: true,
  name: 'Outstanding data queries',
  requires: 'queries',
  subject: '{{site_name}} — outstanding data queries, {{today}}',
  bodyHtml: [
    `<p>${logoHtml()}</p>`,
    '<p>Dear {{first_name|colleagues}},</p>',
    '<p>{{query_action}}</p>',
    '{{query_list}}',
    '<p>Where they sit:</p>',
    '{{query_ageing_chart}}',
    '{{query_form_chart}}',
    '<p>If any of these are unclear, or you need the data clarified before you',
    ' can answer, reply to this email and we will help.</p>',
    '<p>With thanks,<br>The TONIC trial team</p>',
  ].join(''),
};

const DATA_QUALITY_SUMMARY = {
  id: 'builtin_data_quality',
  builtIn: true,
  name: 'Data quality: completeness and queries',
  requires: 'quality',
  subject: '{{site_name}} — data quality summary, {{today}}',
  bodyHtml: [
    `<p>${logoHtml()}</p>`,
    '<p>Dear {{first_name|colleagues}},</p>',
    '<p>Here is where <strong>{{site_name}}</strong> stands on data this month.</p>',
    '<h3 style="font-family:Calibri,Arial,sans-serif;">Completeness</h3>',
    '<p>{{completeness_headline}}</p>',
    '{{completeness_gauge}}',
    '<h3 style="font-family:Calibri,Arial,sans-serif;">Outstanding queries</h3>',
    '<p>{{query_action}}</p>',
    '{{query_breakdown_chart}}',
    '{{query_list}}',
    '<h3 style="font-family:Calibri,Arial,sans-serif;">Across the trial</h3>',
    '{{quality_scorecard}}',
    '<p>With thanks,<br>The TONIC trial team</p>',
  ].join(''),
};

const BUILT_IN_TEMPLATES = [
  MONTHLY_RECRUITMENT, DATA_COMPLETENESS, COMPLETENESS_LEAGUE, MONTHLY_UPDATE,
  QUERY_CHASE, DATA_QUALITY_SUMMARY,
];

/** Built-in templates that make sense given what has been imported. */
function availableBuiltIns({
  hasRecruitment = false, hasCompleteness = false, hasQueries = false,
} = {}) {
  const has = {
    recruitment: hasRecruitment,
    completeness: hasCompleteness,
    queries: hasQueries,
    both: hasRecruitment && hasCompleteness,
    quality: hasCompleteness && hasQueries,
  };
  return BUILT_IN_TEMPLATES.filter((t) => !t.requires || has[t.requires]);
}

module.exports = {
  BUILT_IN_TEMPLATES,
  MONTHLY_RECRUITMENT,
  DATA_COMPLETENESS,
  COMPLETENESS_LEAGUE,
  MONTHLY_UPDATE,
  QUERY_CHASE,
  DATA_QUALITY_SUMMARY,
  availableBuiltIns,
};

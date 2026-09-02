'use strict';

// Template substitution and the rules that decide who ends up in To/Cc/Bcc.

const { dedupeContacts, firstNameOf, formatAddress } = require('./emails');
const { renderPlaceholdersInHtml, htmlToText, isEmptyHtml } = require('./html');
const { findRecruitmentSite } = require('./recruitment');
const { findCompletenessSite, standing } = require('./completeness');
const { findQuerySite, queryStanding } = require('./queries');
const {
  rankedBarChart, progressChart, overallChart, siteTrendChart,
  completenessBarChart, completenessLeaderboard, completenessBreakdownChart,
  completenessGauge, completenessTrendChart, overallCompletenessChart, ordinal,
  queryResolutionChart, queryBreakdownChart, queryAgeingChart, queryGroupChart,
  queryListTable, qualityScorecard,
} = require('./charts');

// Fields whose value is generated HTML and must not be escaped on the way in.
const RAW_FIELDS = new Set([
  'recruitment_chart', 'progress_chart', 'overall_chart', 'trend_chart',
  'completeness_chart', 'completeness_leaderboard', 'completeness_gauge',
  'completeness_event_chart', 'completeness_form_chart', 'completeness_trend_chart',
  'overall_completeness_chart', 'query_chart', 'query_breakdown_chart',
  'query_ageing_chart', 'query_form_chart', 'query_category_chart',
  'query_list', 'quality_scorecard',
]);

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

// Only offered once randomisation data has been imported.
const RECRUITMENT_FIELDS = [
  { key: 'recruitment_chart', label: 'Chart: all sites ranked (yours highlighted)' },
  { key: 'progress_chart', label: 'Chart: your progress against target' },
  { key: 'overall_chart', label: 'Chart: whole-trial recruitment' },
  { key: 'trend_chart', label: 'Chart: your recruitment by month' },
  { key: 'site_randomised', label: 'Your site: randomised to date' },
  { key: 'site_target', label: 'Your site: recruitment target' },
  { key: 'site_percent', label: 'Your site: percent of target' },
  { key: 'site_rank', label: 'Your site: rank (e.g. 4)' },
  { key: 'site_rank_of', label: 'Your site: rank out of (e.g. 4 of 23)' },
  { key: 'site_quartile', label: 'Your site: quartile (1 = top)' },
  { key: 'trial_randomised', label: 'Whole trial: randomised to date' },
  { key: 'trial_target', label: 'Whole trial: target' },
  { key: 'trial_sites', label: 'Whole trial: number of sites' },
];

// Only offered once a return-rates export has been imported.
const COMPLETENESS_FIELDS = [
  { key: 'completeness_chart', label: 'Chart: completeness, all sites ranked' },
  { key: 'completeness_leaderboard', label: 'Chart: league table with movement' },
  { key: 'completeness_gauge', label: 'Chart: you vs the average and the leader' },
  { key: 'completeness_event_chart', label: 'Chart: your completeness by timepoint' },
  { key: 'completeness_form_chart', label: 'Chart: your weakest forms' },
  { key: 'completeness_trend_chart', label: 'Chart: your completeness over time' },
  { key: 'overall_completeness_chart', label: 'Chart: whole-trial completeness' },
  { key: 'completeness', label: 'Your site: overall completeness (e.g. 87.4%)' },
  { key: 'completeness_entered', label: 'Your site: forms entered' },
  { key: 'completeness_due', label: 'Your site: forms due' },
  { key: 'completeness_outstanding', label: 'Your site: forms outstanding' },
  { key: 'completeness_rank', label: 'Your site: rank (e.g. 4)' },
  { key: 'completeness_rank_of', label: 'Your site: rank out of (e.g. 4 of 23)' },
  { key: 'completeness_position', label: 'Your site: position in words (e.g. 4th of 23)' },
  { key: 'completeness_quartile', label: 'Your site: quartile (1 = top)' },
  { key: 'completeness_status', label: 'Your site: on target / needs attention / below target' },
  { key: 'completeness_vs_average', label: 'Your site: points above or below the trial average' },
  { key: 'completeness_vs_average_words', label: 'Your site: "6.2 points above the trial average"' },
  { key: 'completeness_gap_to_top', label: 'Your site: points behind the leading site' },
  { key: 'completeness_gap_to_next', label: 'Your site: points from the next place up' },
  { key: 'completeness_change', label: 'Your site: change since the last update' },
  { key: 'completeness_movement', label: 'Your site: "up 2 places since last month"' },
  { key: 'completeness_headline', label: 'Your site: a ready-made one-line summary' },
  { key: 'completeness_trophy', label: 'Your site: trophy line when in the top three' },
  { key: 'completeness_worst_form', label: 'Your site: the form furthest behind' },
  { key: 'completeness_worst_event', label: 'Your site: the timepoint furthest behind' },
  { key: 'trial_completeness', label: 'Whole trial: overall completeness' },
  { key: 'trial_completeness_entered', label: 'Whole trial: forms entered' },
  { key: 'trial_completeness_due', label: 'Whole trial: forms due' },
  { key: 'trial_completeness_outstanding', label: 'Whole trial: forms outstanding' },
  { key: 'trial_completeness_average', label: 'Whole trial: average site completeness' },
  { key: 'trial_completeness_leader', label: 'Whole trial: the leading site' },
  { key: 'completeness_sites', label: 'Whole trial: number of sites ranked' },
];

// Only offered once a data query export has been imported.
const QUALITY_FIELDS = [
  { key: 'query_list', label: 'Table: your outstanding queries, oldest first' },
  { key: 'query_breakdown_chart', label: 'Chart: your queries resolved, open, awaiting you' },
  { key: 'query_ageing_chart', label: 'Chart: how long your queries have been open' },
  { key: 'query_form_chart', label: 'Chart: which forms your open queries are on' },
  { key: 'query_category_chart', label: 'Chart: your open queries by data category' },
  { key: 'query_chart', label: 'Chart: queries resolved, all sites ranked' },
  { key: 'quality_scorecard', label: 'Chart: completeness and queries side by side' },
  { key: 'queries_open', label: 'Your site: queries still open' },
  { key: 'queries_awaiting_you', label: 'Your site: open queries you have not replied to' },
  { key: 'queries_raised', label: 'Your site: queries raised' },
  { key: 'queries_resolved', label: 'Your site: queries resolved' },
  { key: 'queries_overdue', label: 'Your site: queries open beyond the overdue threshold' },
  { key: 'queries_resolved_percent', label: 'Your site: percent of queries closed' },
  { key: 'queries_median_days', label: 'Your site: median days a query stays open' },
  { key: 'queries_oldest_days', label: 'Your site: age of the oldest open query' },
  { key: 'queries_oldest_date', label: 'Your site: when the oldest open query was raised' },
  { key: 'query_top_form', label: 'Your site: the form with the most open queries' },
  { key: 'query_top_category', label: 'Your site: the most common data category' },
  { key: 'query_rank_of', label: 'Your site: query rank out of (e.g. 4 of 23)' },
  { key: 'query_position', label: 'Your site: query position in words (e.g. 4th of 23)' },
  { key: 'query_movement', label: 'Your site: change in open queries since last time' },
  { key: 'query_headline', label: 'Your site: a ready-made one-line summary' },
  { key: 'query_action', label: 'Your site: a ready-made "what we need from you" line' },
  { key: 'trial_queries_open', label: 'Whole trial: queries still open' },
  { key: 'trial_queries_overdue', label: 'Whole trial: queries past the overdue threshold' },
  { key: 'trial_queries_resolved_percent', label: 'Whole trial: percent of queries closed' },
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

  // Recruitment figures and charts, when randomisation data has been imported.
  // A site the data does not cover simply gets no values, so a missing site
  // shows up as an empty placeholder warning rather than a wrong number.
  const dataset = options.recruitment;
  if (dataset && dataset.sites && dataset.sites.length) {
    const match = site ? findRecruitmentSite(dataset, site) : null;
    const anonymise = options.anonymiseOtherSites !== false;

    if (match) {
      // Recruitment exports often carry no target. Fall back to one from the
      // contact list, where a "Target" column is common, so the progress
      // chart still works without editing the trial's export.
      const fallbackTarget = Number(
        (site.fields || {}).target
        ?? (site.fields || {}).recruitment_target
        ?? (site.fields || {}).site_target,
      );
      const resolved = {
        ...match,
        target: match.target ?? (Number.isFinite(fallbackTarget) && fallbackTarget > 0 ? fallbackTarget : null),
      };
      resolved.percentOfTarget = resolved.target
        ? Math.round((resolved.randomised / resolved.target) * 100)
        : null;

      context.site_randomised = String(resolved.randomised);
      if (resolved.target !== null) context.site_target = String(resolved.target);
      if (resolved.percentOfTarget !== null) context.site_percent = `${resolved.percentOfTarget}%`;
      context.site_rank = String(resolved.rank);
      context.site_rank_of = `${resolved.rank} of ${resolved.of}`;
      context.site_quartile = String(resolved.quartile);
      if (resolved.opened) context.site_opened = String(resolved.opened);
      context.progress_chart = progressChart(resolved);
      context.trend_chart = siteTrendChart(resolved);
    }

    context.recruitment_chart = rankedBarChart(dataset, {
      focusKey: match ? match.key : null,
      anonymise,
    });
    context.overall_chart = overallChart(dataset);
    context.trial_randomised = String(dataset.totals.randomised);
    if (dataset.totals.target) context.trial_target = String(dataset.totals.target);
    context.trial_sites = String(dataset.totals.siteCount);
  }

  addCompletenessFields(context, site, options);
  addQualityFields(context, site, options.completeness, options);

  return context;
}

function signed(points) {
  return `${points > 0 ? '+' : ''}${points}`;
}

/**
 * Completeness figures and charts, from the return-rates export.
 *
 * Everything comparative goes through `standing()`, so the wording in the
 * merge fields and the wording on the charts cannot drift apart.
 */
function addCompletenessFields(context, site, options) {
  const dataset = options.completeness;
  if (!dataset || !dataset.sites || !dataset.sites.length) return context;

  const naming = options.completenessNaming || 'top3';
  const match = site ? findCompletenessSite(dataset, site) : null;

  context.trial_completeness = dataset.totals.percent === null ? '' : `${dataset.totals.percent}%`;
  context.trial_completeness_entered = String(dataset.totals.entered);
  context.trial_completeness_due = String(dataset.totals.due);
  context.trial_completeness_outstanding = String(dataset.totals.outstanding);
  if (dataset.totals.meanPercent !== null) {
    context.trial_completeness_average = `${dataset.totals.meanPercent}%`;
  }
  if (dataset.totals.leader && naming !== 'none') {
    context.trial_completeness_leader = `${dataset.totals.leader.siteName} (${dataset.totals.leader.percent}%)`;
  }
  context.completeness_sites = String(dataset.totals.rankedCount);
  context.overall_completeness_chart = overallCompletenessChart(dataset);
  context.completeness_chart = completenessBarChart(dataset, {
    focusKey: match ? match.key : null,
    naming,
  });
  context.completeness_leaderboard = completenessLeaderboard(dataset, {
    focusKey: match ? match.key : null,
    naming,
  });

  if (!match) return context;

  const view = standing(match, dataset);
  context.completeness_entered = String(match.entered);
  context.completeness_due = String(match.due);
  context.completeness_outstanding = String(match.outstanding);
  context.completeness_event_chart = completenessBreakdownChart(match.byEvent, {
    title: 'Your completeness by timepoint',
  });
  context.completeness_form_chart = completenessBreakdownChart(match.byForm, {
    title: 'The forms furthest behind at your site',
    worstFirst: true,
    maxRows: 6,
  });
  context.completeness_trend_chart = completenessTrendChart(match);

  const worstForm = [...(match.byForm || [])].filter((r) => r.percent !== null)
    .sort((a, b) => a.percent - b.percent)[0];
  if (worstForm) context.completeness_worst_form = `${worstForm.name} (${worstForm.percent}%)`;
  const worstEvent = [...(match.byEvent || [])].filter((r) => r.percent !== null)
    .sort((a, b) => a.percent - b.percent)[0];
  if (worstEvent) context.completeness_worst_event = `${worstEvent.name} (${worstEvent.percent}%)`;

  if (!view) return context;

  context.completeness = `${view.percent}%`;
  context.completeness_rank = String(view.rank);
  context.completeness_rank_of = `${view.rank} of ${view.of}`;
  context.completeness_position = `${ordinal(view.rank)} of ${view.of}`;
  context.completeness_quartile = String(view.quartile);
  context.completeness_status = view.status;
  context.completeness_gauge = completenessGauge(match, dataset, { naming });

  if (view.vsAverage !== null) {
    context.completeness_vs_average = `${signed(view.vsAverage)} points`;
    context.completeness_vs_average_words = view.vsAverage === 0
      ? 'exactly on the trial average'
      : `${Math.abs(view.vsAverage)} points ${view.vsAverage > 0 ? 'above' : 'below'} the trial average`;
  }
  if (view.gapToTop !== null) context.completeness_gap_to_top = `${view.gapToTop} points`;
  if (view.gapToNext !== null && view.nextRank) {
    context.completeness_gap_to_next = `${view.gapToNext} points from ${ordinal(view.nextRank)} place`;
  }
  if (view.percentChange !== null) context.completeness_change = `${signed(view.percentChange)} points`;
  if (view.rankChange) {
    const places = Math.abs(view.rankChange);
    context.completeness_movement = `${view.rankChange > 0 ? 'up' : 'down'} ${places} `
      + `place${places === 1 ? '' : 's'} since the last update`;
  }

  // A line the trial office can drop straight into a message without having
  // to assemble the figures themselves.
  const headline = [`${view.percent}% of your due forms are entered`,
    `which puts ${site.siteName} ${ordinal(view.rank)} of ${view.of} sites`];
  if (view.vsAverage !== null && view.vsAverage !== 0) {
    headline.push(`${Math.abs(view.vsAverage)} points ${view.vsAverage > 0 ? 'above' : 'below'} the trial average`);
  }
  context.completeness_headline = `${headline.join(', ')}.`;

  if (view.isLeader) {
    context.completeness_trophy = `${site.siteName} currently holds the top spot for data completeness.`;
  } else if (view.inTopThree) {
    context.completeness_trophy = `${site.siteName} is ${ordinal(view.rank)} for data completeness`
      + `${view.gapToTop ? `, ${view.gapToTop} points off the lead` : ''}.`;
  }

  return context;
}

/** Outstanding data queries, from the query export. */
function addQualityFields(context, site, completeness, options) {
  const dataset = options.queries;
  if (!dataset || !dataset.sites || !dataset.sites.length) return context;

  const naming = options.completenessNaming || 'all';
  const match = site ? findQuerySite(dataset, site) : null;

  context.trial_queries_open = String(dataset.totals.open);
  context.trial_queries_overdue = String(dataset.totals.overdue);
  if (dataset.totals.resolvedPercent !== null) {
    context.trial_queries_resolved_percent = `${dataset.totals.resolvedPercent}%`;
  }
  context.query_chart = queryResolutionChart(dataset, {
    focusKey: match ? match.key : null,
    naming,
  });
  if (completeness && completeness.sites && completeness.sites.length) {
    const completenessMatch = site ? findCompletenessSite(completeness, site) : null;
    context.quality_scorecard = qualityScorecard(completeness, dataset, {
      focusKey: completenessMatch ? completenessMatch.key : null,
      naming,
    });
  }

  if (!match) return context;

  const view = queryStanding(match, dataset);
  context.queries_open = String(view.open);
  context.queries_awaiting_you = String(view.awaitingSite);
  context.queries_raised = String(view.raised);
  context.queries_resolved = String(view.closed);
  context.queries_overdue = String(view.overdue);
  if (view.resolvedPercent !== null) context.queries_resolved_percent = `${view.resolvedPercent}%`;
  if (view.medianDays !== null) context.queries_median_days = String(view.medianDays);
  if (view.oldestDays !== null) context.queries_oldest_days = String(view.oldestDays);
  if (view.oldestOpened) context.queries_oldest_date = view.oldestOpened;
  if (view.topForm) context.query_top_form = view.topForm;
  if (view.topCategory) context.query_top_category = view.topCategory;
  if (view.rank) {
    context.query_rank_of = `${view.rank} of ${view.of}`;
    context.query_position = `${ordinal(view.rank)} of ${view.of}`;
  }
  if (view.openChange) {
    const moved = Math.abs(view.openChange);
    context.query_movement = view.openChange > 0
      ? `${moved} fewer open than last time`
      : `${moved} more open than last time`;
  }

  context.query_list = queryListTable(match.outstanding, { overdueDays: view.overdueDays });
  context.query_breakdown_chart = queryBreakdownChart(match, dataset);
  context.query_ageing_chart = queryAgeingChart(match.ageBands);
  context.query_form_chart = queryGroupChart(match.byForm, {
    title: 'Which forms your open queries are on',
  });
  context.query_category_chart = queryGroupChart(match.byCategory, {
    title: 'Your open queries by data category',
  });

  const parts = [`${site.siteName} has ${view.open} data quer${view.open === 1 ? 'y' : 'ies'} outstanding`];
  if (view.overdue) parts.push(`${view.overdue} open longer than ${view.overdueDays} days`);
  if (view.resolvedPercent !== null) parts.push(`${view.resolvedPercent}% of those raised are closed`);
  context.query_headline = `${parts.join(', ')}.`;

  // The line that says what the site actually has to do, rather than how it
  // compares — a chase email needs one of these more than a league table.
  if (view.open === 0) {
    context.query_action = 'You have no outstanding data queries — thank you.';
  } else if (view.awaitingSite > 0) {
    context.query_action = `${view.awaitingSite} of your ${view.open} open quer`
      + `${view.open === 1 ? 'y is' : 'ies are'} waiting on a reply from your site`
      + `${view.oldestDays !== null ? `, the oldest raised ${view.oldestDays} days ago` : ''}.`;
  } else {
    context.query_action = `All ${view.open} of your open queries have been replied to `
      + 'and are with the trial team.';
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
    const rendered = renderPlaceholdersInHtml(template.bodyHtml, context, { rawFields: RAW_FIELDS });
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
        recruitment: options.recruitment,
        anonymiseOtherSites: options.anonymiseOtherSites,
        completeness: options.completeness,
        completenessNaming: options.completenessNaming,
        queries: options.queries,
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
    recruitment: options.recruitment,
    anonymiseOtherSites: options.anonymiseOtherSites,
    completeness: options.completeness,
    completenessNaming: options.completenessNaming,
    queries: options.queries,
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
  RECRUITMENT_FIELDS,
  COMPLETENESS_FIELDS,
  QUALITY_FIELDS,
  RAW_FIELDS,
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

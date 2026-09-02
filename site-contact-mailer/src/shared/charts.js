'use strict';

// Recruitment charts as email-safe HTML.
//
// These are rendered with nested tables, `bgcolor` and pixel `width`
// attributes rather than CSS, because Outlook renders mail through Word: no
// flexbox, no grid, no background images, no SVG, and no JavaScript. A bar is
// a table cell with a background colour and a fixed pixel width, which is the
// one construct every mail client agrees on.
//
// Widths are computed in pixels against a fixed chart width; percentage widths
// inside nested tables are unreliable in Outlook.

const { escapeHtml } = require('./html');
const { formatMonthShort } = require('./recruitment');
const { THRESHOLD_GOOD, THRESHOLD_WARN, standing, queryStanding } = require('./completeness');

// Kept in one place so the whole set can be restyled to the design system
// without touching the layout code.
const THEME = {
  focus: '#12A192',        // the recipient's own site — TONIC's teal
  other: '#9FB3BC',        // everyone else, muted so the focus bar dominates
  overall: '#00344C',      // trial-wide totals — TONIC's navy
  behind: '#F59E0B',       // under target
  good: '#2EC4A5',         // at or above 90% — the dashboard's thresholds
  warn: '#F0A500',         // 70-89%
  poor: '#E05C3A',         // below 70%
  gold: '#B8860B',         // league-table medals
  track: '#EDF3F5',        // the unfilled part of a bar
  text: '#06283A',
  muted: '#7B8E99',
  rule: '#DBE5EA',
  font: 'Calibri, Arial, sans-serif',
};

const CHART_WIDTH = 520;
const LABEL_WIDTH = 132;
const VALUE_WIDTH = 54;

/** Anonymous labels for other sites: A, B, ... Z, AA, AB, ... */
function anonymousLabel(index) {
  let n = index;
  let label = '';
  do {
    label = String.fromCharCode(65 + (n % 26)) + label;
    n = Math.floor(n / 26) - 1;
  } while (n >= 0);
  return `Site ${label}`;
}

function cell(content, { width, align = 'left', color = THEME.text, size = '10pt', bold = false, extra = '' }) {
  const style = `font-family:${THEME.font};font-size:${size};color:${color};`
    + `text-align:${align};${bold ? 'font-weight:bold;' : ''}${extra}`;
  return `<td${width ? ` width="${width}"` : ''} align="${align}" style="${style}">${content}</td>`;
}

/**
 * One horizontal bar, as a two-cell table: filled portion, then the track.
 * `barWidth` and `trackWidth` are pixels.
 */
function bar(barWidth, trackWidth, colour, { height = 14 } = {}) {
  const filled = Math.max(0, Math.round(barWidth));
  const rest = Math.max(0, Math.round(trackWidth) - filled);
  const solid = (width, background) => (width <= 0 ? '' : `<td width="${width}" bgcolor="${background}"`
    + ` style="width:${width}px;background-color:${background};height:${height}px;`
    + `line-height:${height}px;font-size:1px;">&nbsp;</td>`);
  // A zero-width cell renders as a sliver in some clients, so it is omitted.
  return `<table role="presentation" cellpadding="0" cellspacing="0" border="0"`
    + ` style="border-collapse:collapse;"><tr>`
    + `${solid(filled, colour)}${solid(rest, THEME.track)}`
    + `</tr></table>`;
}

function chartFrame(title, note, rowsHtml) {
  const heading = title
    ? `<tr><td colspan="3" style="font-family:${THEME.font};font-size:11pt;font-weight:bold;`
      + `color:${THEME.text};padding:0 0 8px 0;">${escapeHtml(title)}</td></tr>`
    : '';
  const footer = note
    ? `<tr><td colspan="3" style="font-family:${THEME.font};font-size:8.5pt;color:${THEME.muted};`
      + `padding:8px 0 0 0;">${escapeHtml(note)}</td></tr>`
    : '';
  return `<table role="presentation" cellpadding="0" cellspacing="0" border="0" width="${CHART_WIDTH}"`
    + ` style="border-collapse:collapse;width:${CHART_WIDTH}px;margin:12px 0;">`
    + `${heading}${rowsHtml}${footer}</table>`;
}

/**
 * Ranked bar chart of every site, the recipient's own highlighted.
 *
 * With more than `maxRows` sites the list is truncated, but the recipient's
 * own site is always shown — falling off the bottom of the chart is exactly
 * the case where they most need to see where they stand.
 */
function rankedBarChart(dataset, options = {}) {
  const {
    focusKey = null, anonymise = true, maxRows = 10, title = 'Recruitment by site',
  } = options;
  if (!dataset || !dataset.sites || dataset.sites.length === 0) return '';

  const ordered = [...dataset.sites].sort((a, b) => b.randomised - a.randomised || a.siteName.localeCompare(b.siteName));
  const max = Math.max(...ordered.map((s) => s.randomised), 1);
  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;

  const focusIndex = ordered.findIndex((s) => s.key === focusKey);
  const shown = ordered.slice(0, maxRows);
  const focusOutside = focusIndex >= maxRows;
  if (focusOutside) shown.push(ordered[focusIndex]);

  const rows = shown.map((site, position) => {
    const isFocus = site.key === focusKey;
    const trueIndex = ordered.indexOf(site);
    const label = isFocus
      ? site.siteName
      : (anonymise ? anonymousLabel(trueIndex) : site.siteName);
    const colour = isFocus ? THEME.focus : THEME.other;

    // A gap row marks the jump when the recipient sits outside the top list.
    const gap = focusOutside && position === shown.length - 1 && trueIndex > maxRows
      ? `<tr><td colspan="3" style="font-family:${THEME.font};font-size:9pt;color:${THEME.muted};`
        + `padding:2px 0;">⋮</td></tr>`
      : '';

    return gap + '<tr>'
      + cell(
        `${escapeHtml(label)}${isFocus ? ' <span style="color:' + THEME.focus + ';">(you)</span>' : ''}`,
        { width: LABEL_WIDTH, bold: isFocus, extra: 'padding:3px 8px 3px 0;' },
      )
      + `<td width="${barSpace}" style="padding:3px 0;">${bar(site.randomised / max * barSpace, barSpace, colour)}</td>`
      + cell(String(site.randomised), {
        width: VALUE_WIDTH, align: 'right', bold: isFocus, extra: 'padding:3px 0 3px 8px;',
      })
      + '</tr>';
  }).join('');

  const hidden = ordered.length - shown.length;
  const note = anonymise
    ? `Other sites are shown anonymously.${hidden > 0 ? ` ${hidden} further site${hidden === 1 ? '' : 's'} not shown.` : ''}`
    : (hidden > 0 ? `${hidden} further site${hidden === 1 ? '' : 's'} not shown.` : '');

  return chartFrame(title, note, rows);
}

/** A single site's progress towards its recruitment target. */
function progressChart(site, options = {}) {
  if (!site) return '';
  const { title = 'Your progress against target' } = options;
  if (!site.target) {
    // Without a target there is nothing to be a proportion of; show the count
    // rather than inventing a denominator.
    return chartFrame(title, 'No recruitment target is set for your site.',
      `<tr>${cell(`<strong>${site.randomised}</strong> randomised to date`,
        { width: CHART_WIDTH, extra: 'padding:4px 0;' })}</tr>`);
  }

  const percent = Math.round((site.randomised / site.target) * 100);
  const barSpace = CHART_WIDTH - VALUE_WIDTH - 8;
  const colour = percent >= 100 ? THEME.focus : (percent >= 75 ? THEME.overall : THEME.behind);

  const rows = `<tr>`
    + `<td width="${barSpace}" style="padding:3px 0;">`
    + `${bar(Math.min(percent, 100) / 100 * barSpace, barSpace, colour, { height: 18 })}</td>`
    + cell(`${percent}%`, { width: VALUE_WIDTH, align: 'right', bold: true, extra: 'padding:3px 0 3px 8px;' })
    + `</tr>`
    + `<tr>${cell(`<strong>${site.randomised}</strong> of <strong>${site.target}</strong> randomised`,
      { width: CHART_WIDTH, size: '9.5pt', color: THEME.muted, extra: 'padding:4px 0 0 0;' })}</tr>`;

  return chartFrame(title, '', rows);
}

/** Trial-wide recruitment: a monthly series, or a single total if no history. */
function overallChart(dataset, options = {}) {
  const { title = 'Recruitment across the whole trial', months = 12 } = options;
  if (!dataset || !dataset.totals) return '';

  const series = (dataset.totals.monthly || []).slice(-months);
  if (series.length === 0) {
    const { randomised, target, siteCount } = dataset.totals;
    const text = target
      ? `<strong>${randomised}</strong> of <strong>${target}</strong> randomised across ${siteCount} sites`
      : `<strong>${randomised}</strong> randomised across ${siteCount} sites`;
    return chartFrame(title, '', `<tr>${cell(text, { width: CHART_WIDTH, extra: 'padding:4px 0;' })}</tr>`);
  }

  const max = Math.max(...series.map((m) => m.count), 1);
  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;
  const rows = series.map((point) => '<tr>'
    + cell(formatMonthShort(point.month), {
      width: LABEL_WIDTH, color: THEME.muted, size: '9.5pt', extra: 'padding:2px 8px 2px 0;',
    })
    + `<td width="${barSpace}" style="padding:2px 0;">`
    + `${bar(point.count / max * barSpace, barSpace, THEME.overall, { height: 12 })}</td>`
    + cell(String(point.count), {
      width: VALUE_WIDTH, align: 'right', size: '9.5pt', extra: 'padding:2px 0 2px 8px;',
    })
    + '</tr>').join('');

  const { randomised, target, siteCount } = dataset.totals;
  const note = target
    ? `${randomised} randomised in total across ${siteCount} sites, against a target of ${target}.`
    : `${randomised} randomised in total across ${siteCount} sites.`;

  return chartFrame(title, note, rows);
}

/** A site's own month-by-month recruitment. */
function siteTrendChart(site, options = {}) {
  const { title = 'Your recruitment by month', months = 12 } = options;
  if (!site || !site.monthly || site.monthly.length === 0) return '';

  const series = site.monthly.slice(-months);
  const max = Math.max(...series.map((m) => m.count), 1);
  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;

  const rows = series.map((point) => '<tr>'
    + cell(formatMonthShort(point.month), {
      width: LABEL_WIDTH, color: THEME.muted, size: '9.5pt', extra: 'padding:2px 8px 2px 0;',
    })
    + `<td width="${barSpace}" style="padding:2px 0;">`
    + `${bar(point.count / max * barSpace, barSpace, THEME.focus, { height: 12 })}</td>`
    + cell(String(point.count), {
      width: VALUE_WIDTH, align: 'right', size: '9.5pt', extra: 'padding:2px 0 2px 8px;',
    })
    + '</tr>').join('');

  return chartFrame(title, '', rows);
}

// ── Data completeness ─────────────────────────────────────────────────────

function rateColour(percent) {
  if (percent === null || percent === undefined) return THEME.other;
  if (percent >= THRESHOLD_GOOD) return THEME.good;
  if (percent >= THRESHOLD_WARN) return THEME.warn;
  return THEME.poor;
}

function formatPercent(percent) {
  return percent === null || percent === undefined ? '—' : `${percent}%`;
}

/** "up 2", "down 1", "no change" — movement since the previous import. */
function formatRankChange(change) {
  if (change === null || change === undefined || change === 0) return '';
  return change > 0 ? `▲ ${change}` : `▼ ${Math.abs(change)}`;
}

function ordinal(n) {
  const suffix = (n % 100 >= 11 && n % 100 <= 13) ? 'th'
    : ({ 1: 'st', 2: 'nd', 3: 'rd' }[n % 10] || 'th');
  return `${n}${suffix}`;
}

/**
 * How other sites are labelled. `all` names everyone, `none` anonymises
 * everyone but the recipient, and `top3` — the default — names the leading
 * three so the trophy race is public while nobody is named as the laggard.
 */
function completenessLabel(site, position, { focusKey, naming }) {
  if (site.key === focusKey) return site.siteName;
  if (naming === 'all') return site.siteName;
  if (naming === 'top3' && site.rank && site.rank <= 3) return site.siteName;
  return anonymousLabel(position);
}

function namingNote(naming) {
  if (naming === 'all') return '';
  if (naming === 'top3') return 'The leading three sites are named; the rest are shown anonymously.';
  return 'Other sites are shown anonymously.';
}

/** Ranked completeness across every site, the recipient's own highlighted. */
function completenessBarChart(dataset, options = {}) {
  const {
    focusKey = null, naming = 'top3', maxRows = 10, title = 'Data completeness by site',
  } = options;
  if (!dataset || !dataset.sites || dataset.sites.length === 0) return '';

  const ordered = dataset.sites
    .filter((s) => s.percent !== null)
    .sort((a, b) => b.percent - a.percent || a.siteName.localeCompare(b.siteName));
  if (ordered.length === 0) return '';

  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;
  const focusIndex = ordered.findIndex((s) => s.key === focusKey);
  const shown = ordered.slice(0, maxRows);
  const focusOutside = focusIndex >= maxRows;
  if (focusOutside) shown.push(ordered[focusIndex]);

  const rows = shown.map((site, position) => {
    const isFocus = site.key === focusKey;
    const trueIndex = ordered.indexOf(site);
    const label = completenessLabel(site, trueIndex, { focusKey, naming });
    const colour = isFocus ? THEME.focus : rateColour(site.percent);
    const trophy = site.rank === 1 ? '&#127942; ' : '';

    const gap = focusOutside && position === shown.length - 1 && trueIndex > maxRows
      ? `<tr><td colspan="3" style="font-family:${THEME.font};font-size:9pt;color:${THEME.muted};`
        + `padding:2px 0;">&#8942;</td></tr>`
      : '';

    return gap + '<tr>'
      + cell(
        `${trophy}${escapeHtml(label)}${isFocus ? ' <span style="color:' + THEME.focus + ';">(you)</span>' : ''}`,
        { width: LABEL_WIDTH, bold: isFocus, extra: 'padding:3px 8px 3px 0;' },
      )
      + `<td width="${barSpace}" style="padding:3px 0;">`
      + `${bar(site.percent / 100 * barSpace, barSpace, colour)}</td>`
      + cell(formatPercent(site.percent), {
        width: VALUE_WIDTH, align: 'right', bold: isFocus, extra: 'padding:3px 0 3px 8px;',
      })
      + '</tr>';
  }).join('');

  const hidden = ordered.length - shown.length;
  const parts = [namingNote(naming)];
  if (hidden > 0) parts.push(`${hidden} further site${hidden === 1 ? '' : 's'} not shown.`);
  return chartFrame(title, parts.filter(Boolean).join(' '), rows);
}

/**
 * The trophy table: the top few sites with their position, plus the
 * recipient's own row when it falls outside them.
 */
function completenessLeaderboard(dataset, options = {}) {
  const {
    focusKey = null, naming = 'top3', top = 5, title = 'Completeness league table',
  } = options;
  if (!dataset || !dataset.sites) return '';

  const ordered = dataset.sites
    .filter((s) => s.percent !== null)
    .sort((a, b) => b.percent - a.percent || a.siteName.localeCompare(b.siteName));
  if (ordered.length === 0) return '';

  const focusIndex = ordered.findIndex((s) => s.key === focusKey);
  const shown = ordered.slice(0, top);
  if (focusIndex >= top) shown.push(ordered[focusIndex]);

  const RANK_WIDTH = 46;
  const MOVE_WIDTH = 52;
  const nameWidth = CHART_WIDTH - RANK_WIDTH - VALUE_WIDTH - MOVE_WIDTH;

  const header = '<tr>'
    + cell('#', { width: RANK_WIDTH, size: '9pt', color: THEME.muted, extra: `padding:0 0 4px 0;border-bottom:1px solid ${THEME.rule};` })
    + cell('Site', { width: nameWidth, size: '9pt', color: THEME.muted, extra: `padding:0 8px 4px 0;border-bottom:1px solid ${THEME.rule};` })
    + cell('Complete', { width: VALUE_WIDTH, align: 'right', size: '9pt', color: THEME.muted, extra: `padding:0 0 4px 0;border-bottom:1px solid ${THEME.rule};` })
    + cell('Move', { width: MOVE_WIDTH, align: 'right', size: '9pt', color: THEME.muted, extra: `padding:0 0 4px 8px;border-bottom:1px solid ${THEME.rule};` })
    + '</tr>';

  const rows = shown.map((site, position) => {
    const isFocus = site.key === focusKey;
    const trueIndex = ordered.indexOf(site);
    const label = completenessLabel(site, trueIndex, { focusKey, naming });
    const move = formatRankChange(site.rankChange);
    const moveColour = (site.rankChange || 0) > 0 ? THEME.good : THEME.poor;
    const background = isFocus ? ' bgcolor="#EEF8F6"' : '';

    const gap = focusIndex >= top && position === shown.length - 1 && trueIndex > top
      ? `<tr><td colspan="4" style="font-family:${THEME.font};font-size:9pt;color:${THEME.muted};`
        + `padding:2px 0;">&#8942;</td></tr>`
      : '';

    return gap + `<tr${background}>`
      + cell(site.rank === 1 ? '&#127942;' : String(site.rank), {
        width: RANK_WIDTH, bold: site.rank <= 3, color: site.rank <= 3 ? THEME.gold : THEME.text,
        extra: 'padding:4px 0;',
      })
      + cell(`${escapeHtml(label)}${isFocus ? ' <span style="color:' + THEME.focus + ';">(you)</span>' : ''}`,
        { width: nameWidth, bold: isFocus, extra: 'padding:4px 8px 4px 0;' })
      + cell(formatPercent(site.percent), {
        width: VALUE_WIDTH, align: 'right', bold: isFocus, color: rateColour(site.percent),
        extra: 'padding:4px 0;',
      })
      + cell(move, {
        width: MOVE_WIDTH, align: 'right', size: '9pt', color: move ? moveColour : THEME.muted,
        extra: 'padding:4px 0 4px 8px;',
      })
      + '</tr>';
  }).join('');

  const note = [namingNote(naming), 'Move shows places gained or lost since the last update.']
    .filter(Boolean).join(' ');
  return chartFrame(title, note, header + rows);
}

/** Completeness split by timepoint or by form, worst first when asked. */
function completenessBreakdownChart(rows, options = {}) {
  const { title = 'Your completeness by form', maxRows = 10, worstFirst = false } = options;
  if (!rows || rows.length === 0) return '';

  const usable = rows.filter((row) => row.percent !== null);
  if (usable.length === 0) return '';
  const ordered = worstFirst ? [...usable].sort((a, b) => a.percent - b.percent) : usable;
  const shown = ordered.slice(0, maxRows);
  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;

  const body = shown.map((row) => '<tr>'
    + cell(escapeHtml(row.name), {
      width: LABEL_WIDTH, size: '9.5pt', extra: 'padding:3px 8px 3px 0;',
    })
    + `<td width="${barSpace}" style="padding:3px 0;">`
    + `${bar(row.percent / 100 * barSpace, barSpace, rateColour(row.percent), { height: 12 })}</td>`
    + cell(`${formatPercent(row.percent)}`, {
      width: VALUE_WIDTH, align: 'right', size: '9.5pt', extra: 'padding:3px 0 3px 8px;',
    })
    + '</tr>').join('');

  const outstanding = shown.reduce((sum, row) => sum + (row.outstanding || 0), 0);
  const hidden = ordered.length - shown.length;
  const parts = [];
  if (outstanding > 0) parts.push(`${outstanding} form${outstanding === 1 ? '' : 's'} outstanding in the rows shown.`);
  if (hidden > 0) parts.push(`${hidden} further row${hidden === 1 ? '' : 's'} not shown.`);
  return chartFrame(title, parts.join(' '), body);
}

/** The recipient's completeness set against the trial average and the leader. */
function completenessGauge(site, dataset, options = {}) {
  const { title = 'Where your site stands', naming = 'top3' } = options;
  const view = standing(site, dataset);
  if (!view) return '';

  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;
  const leaderLabel = naming === 'none' ? 'Leading site' : `Leading site (${view.leaderName})`;
  const lines = [
    { label: 'Your site', percent: view.percent, colour: THEME.focus, bold: true },
    { label: 'Trial average', percent: view.meanPercent, colour: THEME.overall, bold: false },
    { label: view.isLeader ? 'You are leading' : leaderLabel, percent: view.leaderPercent, colour: THEME.gold, bold: false },
  ].filter((line) => line.percent !== null);

  const body = lines.map((line) => '<tr>'
    + cell(escapeHtml(line.label), {
      width: LABEL_WIDTH, bold: line.bold, size: '9.5pt', extra: 'padding:3px 8px 3px 0;',
    })
    + `<td width="${barSpace}" style="padding:3px 0;">`
    + `${bar(line.percent / 100 * barSpace, barSpace, line.colour, { height: 14 })}</td>`
    + cell(formatPercent(line.percent), {
      width: VALUE_WIDTH, align: 'right', bold: line.bold, extra: 'padding:3px 0 3px 8px;',
    })
    + '</tr>').join('');

  const notes = [`You are ${ordinal(view.rank)} of ${view.of} sites.`];
  if (view.gapToTop !== null && view.gapToTop > 0) {
    notes.push(`${view.gapToTop} points behind the leader.`);
  }
  if (view.rankChange) {
    notes.push(view.rankChange > 0
      ? `Up ${view.rankChange} place${view.rankChange === 1 ? '' : 's'} since the last update.`
      : `Down ${Math.abs(view.rankChange)} place${Math.abs(view.rankChange) === 1 ? '' : 's'} since the last update.`);
  }
  return chartFrame(title, notes.join(' '), body);
}

/** A site's completeness across the last few imports. */
function completenessTrendChart(site, options = {}) {
  const { title = 'Your completeness over time', points = 8 } = options;
  if (!site || !site.history || site.history.length < 2) return '';

  const series = site.history.slice(-points);
  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;
  const label = (iso) => {
    const date = new Date(iso);
    return Number.isNaN(date.getTime()) ? String(iso) : formatMonthShort(
      `${date.getFullYear()}-${String(date.getMonth() + 1).padStart(2, '0')}`,
    );
  };

  const body = series.map((point) => '<tr>'
    + cell(label(point.importedAt), {
      width: LABEL_WIDTH, color: THEME.muted, size: '9.5pt', extra: 'padding:2px 8px 2px 0;',
    })
    + `<td width="${barSpace}" style="padding:2px 0;">`
    + `${bar(point.percent / 100 * barSpace, barSpace, rateColour(point.percent), { height: 12 })}</td>`
    + cell(formatPercent(point.percent), {
      width: VALUE_WIDTH, align: 'right', size: '9.5pt', extra: 'padding:2px 0 2px 8px;',
    })
    + '</tr>').join('');

  return chartFrame(title, '', body);
}

/** Trial-wide completeness, with the split by timepoint underneath. */
function overallCompletenessChart(dataset, options = {}) {
  const { title = 'Data completeness across the whole trial' } = options;
  if (!dataset || !dataset.totals || dataset.totals.percent === null) return '';

  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;
  const rows = [{ name: 'All timepoints', percent: dataset.totals.percent }]
    .concat((dataset.totals.byEvent || []).filter((row) => row.percent !== null));

  const body = rows.map((row, index) => '<tr>'
    + cell(escapeHtml(row.name), {
      width: LABEL_WIDTH, bold: index === 0, size: '9.5pt', extra: 'padding:3px 8px 3px 0;',
    })
    + `<td width="${barSpace}" style="padding:3px 0;">`
    + `${bar(row.percent / 100 * barSpace, barSpace, index === 0 ? THEME.overall : rateColour(row.percent), { height: index === 0 ? 16 : 12 })}</td>`
    + cell(formatPercent(row.percent), {
      width: VALUE_WIDTH, align: 'right', bold: index === 0, extra: 'padding:3px 0 3px 8px;',
    })
    + '</tr>').join('');

  const { entered, outstanding, siteCount } = dataset.totals;
  const note = `${entered} forms entered across ${siteCount} sites`
    + (outstanding ? `, ${outstanding} still outstanding.` : '.');
  return chartFrame(title, note, body);
}

// ── Data quality (outstanding queries) ────────────────────────────────────

/** Sites ranked on the share of their data queries that have been closed. */
function queryResolutionChart(dataset, options = {}) {
  const {
    focusKey = null, naming = 'all', maxRows = 10, title = 'Data queries resolved by site',
  } = options;
  if (!dataset || !dataset.sites || !dataset.hasQueries) return '';

  const ordered = dataset.sites
    .filter((s) => s.hasQueries && s.queryResolvedPercent !== null)
    .sort((a, b) => b.queryResolvedPercent - a.queryResolvedPercent
      || a.queriesOpen - b.queriesOpen || a.siteName.localeCompare(b.siteName));
  if (ordered.length === 0) return '';

  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH - 40;
  const focusIndex = ordered.findIndex((s) => s.key === focusKey);
  const shown = ordered.slice(0, maxRows);
  if (focusIndex >= maxRows) shown.push(ordered[focusIndex]);

  const rows = shown.map((site, position) => {
    const isFocus = site.key === focusKey;
    const trueIndex = ordered.indexOf(site);
    const label = completenessLabel({ ...site, rank: site.queryRank }, trueIndex, { focusKey, naming });
    const colour = isFocus ? THEME.focus : rateColour(site.queryResolvedPercent);

    const gap = focusIndex >= maxRows && position === shown.length - 1 && trueIndex > maxRows
      ? `<tr><td colspan="4" style="font-family:${THEME.font};font-size:9pt;color:${THEME.muted};`
        + `padding:2px 0;">&#8942;</td></tr>`
      : '';

    return gap + '<tr>'
      + cell(`${escapeHtml(label)}${isFocus ? ' <span style="color:' + THEME.focus + ';">(you)</span>' : ''}`,
        { width: LABEL_WIDTH, bold: isFocus, extra: 'padding:3px 8px 3px 0;' })
      + `<td width="${barSpace}" style="padding:3px 0;">`
      + `${bar(site.queryResolvedPercent / 100 * barSpace, barSpace, colour)}</td>`
      + cell(formatPercent(site.queryResolvedPercent), {
        width: VALUE_WIDTH, align: 'right', bold: isFocus, extra: 'padding:3px 0 3px 8px;',
      })
      + cell(`${site.queriesOpen} open`, {
        width: 40, align: 'right', size: '9pt', color: THEME.muted, extra: 'padding:3px 0 3px 8px;',
      })
      + '</tr>';
  }).join('');

  const note = `${dataset.totals.queriesOpen} queries outstanding across the trial`
    + (dataset.totals.queriesOverdue ? `, ${dataset.totals.queriesOverdue} of them overdue.` : '.');
  return chartFrame(title, note, rows);
}

/** A site's own query position: raised, resolved, still open, overdue. */
function queryBreakdownChart(site, dataset, options = {}) {
  const { title = 'Your outstanding data queries' } = options;
  const view = queryStanding(site, dataset);
  if (!view || view.raised === 0) return '';

  const barSpace = CHART_WIDTH - LABEL_WIDTH - VALUE_WIDTH;
  const lines = [
    { label: 'Resolved', value: view.resolved, colour: THEME.good },
    { label: 'Still open', value: view.open, colour: THEME.warn },
    { label: 'Overdue', value: view.overdue, colour: THEME.poor },
  ].filter((line) => line.value > 0);
  if (lines.length === 0) return '';

  const max = Math.max(...lines.map((line) => line.value), 1);
  const body = lines.map((line) => '<tr>'
    + cell(line.label, { width: LABEL_WIDTH, size: '9.5pt', extra: 'padding:3px 8px 3px 0;' })
    + `<td width="${barSpace}" style="padding:3px 0;">`
    + `${bar(line.value / max * barSpace, barSpace, line.colour, { height: 14 })}</td>`
    + cell(String(line.value), {
      width: VALUE_WIDTH, align: 'right', bold: true, extra: 'padding:3px 0 3px 8px;',
    })
    + '</tr>').join('');

  const notes = [`${view.resolvedPercent}% of the ${view.raised} queries raised at your site are closed.`];
  if (view.rank) notes.push(`That is ${ordinal(view.rank)} of ${view.of} sites.`);
  return chartFrame(title, notes.join(' '), body);
}

/**
 * One table covering both measures, for a message that reports on data
 * quality as a whole rather than on completeness alone.
 */
function qualityScorecard(dataset, options = {}) {
  const {
    focusKey = null, naming = 'all', top = 8, title = 'Data quality scorecard',
  } = options;
  if (!dataset || !dataset.sites) return '';

  const ordered = dataset.sites
    .filter((s) => s.percent !== null)
    .sort((a, b) => b.percent - a.percent || a.siteName.localeCompare(b.siteName));
  if (ordered.length === 0) return '';

  const focusIndex = ordered.findIndex((s) => s.key === focusKey);
  const shown = ordered.slice(0, top);
  if (focusIndex >= top) shown.push(ordered[focusIndex]);

  const RANK_WIDTH = 40;
  const NUM_WIDTH = 74;
  const showQueries = Boolean(dataset.hasQueries);
  const nameWidth = CHART_WIDTH - RANK_WIDTH - NUM_WIDTH * (showQueries ? 3 : 1);
  const head = (text, width, align = 'left') => cell(text, {
    width, align, size: '9pt', color: THEME.muted,
    extra: `padding:0 8px 4px 0;border-bottom:1px solid ${THEME.rule};`,
  });

  const header = '<tr>'
    + head('#', RANK_WIDTH)
    + head('Site', nameWidth)
    + head('Complete', NUM_WIDTH, 'right')
    + (showQueries ? head('Queries open', NUM_WIDTH, 'right') + head('Resolved', NUM_WIDTH, 'right') : '')
    + '</tr>';

  const rows = shown.map((site) => {
    const isFocus = site.key === focusKey;
    const trueIndex = ordered.indexOf(site);
    const label = completenessLabel(site, trueIndex, { focusKey, naming });
    const background = isFocus ? ' bgcolor="#EEF8F6"' : '';
    return `<tr${background}>`
      + cell(site.rank === 1 ? '&#127942;' : String(site.rank), {
        width: RANK_WIDTH, bold: site.rank <= 3, color: site.rank <= 3 ? THEME.gold : THEME.text,
        extra: 'padding:4px 8px 4px 0;',
      })
      + cell(`${escapeHtml(label)}${isFocus ? ' <span style="color:' + THEME.focus + ';">(you)</span>' : ''}`,
        { width: nameWidth, bold: isFocus, size: '9.5pt', extra: 'padding:4px 8px 4px 0;' })
      + cell(formatPercent(site.percent), {
        width: NUM_WIDTH, align: 'right', color: rateColour(site.percent), bold: isFocus,
        extra: 'padding:4px 8px 4px 0;',
      })
      + (showQueries
        ? cell(site.hasQueries ? String(site.queriesOpen) : '—', {
          width: NUM_WIDTH, align: 'right', size: '9.5pt', extra: 'padding:4px 8px 4px 0;',
        })
        + cell(site.queryResolvedPercent === null ? '—' : formatPercent(site.queryResolvedPercent), {
          width: NUM_WIDTH, align: 'right', size: '9.5pt',
          color: rateColour(site.queryResolvedPercent), extra: 'padding:4px 8px 4px 0;',
        })
        : '')
      + '</tr>';
  }).join('');

  return chartFrame(title, namingNote(naming), header + rows);
}

module.exports = {
  THEME,
  CHART_WIDTH,
  anonymousLabel,
  bar,
  rankedBarChart,
  progressChart,
  overallChart,
  siteTrendChart,
  rateColour,
  ordinal,
  formatPercent,
  completenessLabel,
  completenessBarChart,
  completenessLeaderboard,
  completenessBreakdownChart,
  completenessGauge,
  completenessTrendChart,
  overallCompletenessChart,
  queryResolutionChart,
  queryBreakdownChart,
  qualityScorecard,
};

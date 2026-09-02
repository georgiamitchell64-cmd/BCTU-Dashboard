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

// Kept in one place so the whole set can be restyled to the design system
// without touching the layout code.
const THEME = {
  focus: '#12A192',        // the recipient's own site — TONIC's teal
  other: '#9FB3BC',        // everyone else, muted so the focus bar dominates
  overall: '#00344C',      // trial-wide totals — TONIC's navy
  behind: '#F59E0B',       // under target
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

module.exports = {
  THEME,
  CHART_WIDTH,
  anonymousLabel,
  bar,
  rankedBarChart,
  progressChart,
  overallChart,
  siteTrendChart,
};

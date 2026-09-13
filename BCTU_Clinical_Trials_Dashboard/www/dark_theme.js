// ─────────────────────────────────────────────────────────────────────────────
// Dark mode for the echarts charts. Their colours live in the chart options,
// not in CSS, so www/dark_theme.css can't reach them. When the theme is dark
// this lightens axis labels, gridlines, legends, titles and tooltips, and it
// puts each chart's own light-mode values back when dark mode is switched off.
// Runs when the theme changes and whenever a chart finishes drawing.
// ─────────────────────────────────────────────────────────────────────────────
(function () {
  'use strict';

  var INK = '#C9D3DD', FAINT = '#8E9AA7', LINE = '#2E3945', TIP_BG = '#1F2730', TIP_INK = '#E8EDF2';
  var AXES = ['xAxis', 'yAxis', 'radiusAxis', 'angleAxis', 'singleAxis'];

  function isDarkTheme() { return document.documentElement.getAttribute('data-theme') === 'dark'; }
  function arr(x) { return x == null ? [] : (Array.isArray(x) ? x : [x]); }
  function get(o, path, dflt) {
    for (var i = 0; i < path.length; i++) { if (o == null) return dflt; o = o[path[i]]; }
    return o == null ? dflt : o;
  }

  // One patch shape for both directions: values come from `pick`
  function patch(opt, pick) {
    var p = {};
    AXES.forEach(function (k) {
      if (!opt[k]) return;
      p[k] = arr(opt[k]).map(function (a) {
        return { axisLabel: { color: pick('label', a) }, nameTextStyle: { color: pick('label', a) },
                 axisLine: { lineStyle: { color: pick('axis', a) } },
                 axisTick: { lineStyle: { color: pick('axis', a) } },
                 splitLine: { lineStyle: { color: pick('split', a) } } };
      });
    });
    if (opt.legend) p.legend = arr(opt.legend).map(function (l) {
      return { textStyle: { color: pick('legend', l) }, pageTextStyle: { color: pick('legend', l) } };
    });
    if (opt.title) p.title = arr(opt.title).map(function (t) {
      return { textStyle: { color: pick('title', t) }, subtextStyle: { color: pick('subtitle', t) } };
    });
    if (opt.tooltip) p.tooltip = arr(opt.tooltip).map(function (t) {
      return { backgroundColor: pick('tipBg', t), borderColor: pick('tipBorder', t),
               textStyle: { color: pick('tipInk', t) } };
    });
    return p;
  }

  var DARK = { label: INK, axis: LINE, split: LINE, legend: INK, title: TIP_INK, subtitle: FAINT,
               tipBg: TIP_BG, tipBorder: LINE, tipInk: TIP_INK };
  function darkPick(k) { return DARK[k]; }
  function lightPick(k, o) {
    switch (k) {
      case 'label':     return get(o, ['axisLabel', 'color'], '#6E7079');
      case 'axis':      return get(o, ['axisLine', 'lineStyle', 'color'], '#6E7079');
      case 'split':     return get(o, ['splitLine', 'lineStyle', 'color'], '#E0E6F1');
      case 'legend':    return get(o, ['textStyle', 'color'], '#333');
      case 'title':     return get(o, ['textStyle', 'color'], '#464646');
      case 'subtitle':  return get(o, ['subtextStyle', 'color'], '#6E7079');
      case 'tipBg':     return get(o, ['backgroundColor'], '#fff');
      case 'tipBorder': return get(o, ['borderColor'], '#333');
      case 'tipInk':    return get(o, ['textStyle', 'color'], '#666');
    }
  }

  function sync(chart) {
    if (!chart || chart.isDisposed()) return;
    var o = chart.getOption();
    if (!o) return;
    var first = arr(o.xAxis)[0] || arr(o.yAxis)[0], lg = arr(o.legend)[0];
    // A redraw with fresh options drops the patch, so look at the chart itself
    var looksDark = first ? get(first, ['axisLabel', 'color']) === INK
                  : lg ? get(lg, ['textStyle', 'color']) === INK : !!chart.__bctuDark;
    if (isDarkTheme() && !looksDark) {
      chart.__bctuLight = patch(o, lightPick);
      chart.__bctuDark = true;
      chart.setOption(patch(o, darkPick));
    } else if (!isDarkTheme() && looksDark && chart.__bctuLight) {
      chart.setOption(chart.__bctuLight);
      chart.__bctuLight = null;
      chart.__bctuDark = false;
    }
  }

  function scan() {
    if (!window.echarts) return;
    document.querySelectorAll('[_echarts_instance_]').forEach(function (el) {
      var chart = window.echarts.getInstanceByDom(el);
      if (!chart) return;
      if (!chart.__bctuHooked) {
        chart.__bctuHooked = true;
        chart.on('finished', function () { sync(chart); });
      }
      sync(chart);
    });
  }

  var queued = null;
  function schedule() {
    if (queued) return;
    queued = setTimeout(function () { queued = null; scan(); }, 400);
  }
  function init() {
    new MutationObserver(scan).observe(document.documentElement, { attributes: true, attributeFilter: ['data-theme'] });
    new MutationObserver(schedule).observe(document.body, { childList: true, subtree: true });
    scan();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', init);
  else init();
})();

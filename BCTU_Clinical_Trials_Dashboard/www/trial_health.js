/* =============================================================================
   Trial health widgets — funnel plot, CRF completeness grid, randomisation
   punchcard and trajectory fan chart. Data comes from
   functions/trial_health_ui.R as JSON in the sibling
   <script type="application/json" id="<widget id>-data">. Widgets mount on any
   `.th-widget` Shiny inserts and re-render when their width changes.
   ========================================================================== */
(function () {
  'use strict';

  const DAY = 864e5;
  const esc = s => String(s == null ? '' : s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const arr = v => (v === null || v === undefined) ? [] : (Array.isArray(v) ? v : [v]);
  const ms  = s => Date.parse(s + 'T00:00:00Z');
  const fmt = (t, o) => new Date(t).toLocaleDateString('en-GB', Object.assign({ day: 'numeric', month: 'short', year: 'numeric', timeZone: 'UTC' }, o || {}));
  const fmtM = s => s ? new Date(ms(s)).toLocaleDateString('en-GB', { month: 'short', year: 'numeric', timeZone: 'UTC' }) : '—';
  const shiny = (name, value) => { if (window.Shiny && Shiny.setInputValue) Shiny.setInputValue(name, value, { priority: 'event' }); };
  const pad2 = n => String(n).padStart(2, '0');

  function niceTicks(max, n) {
    if (!(max > 0)) return [0];
    const raw = max / n, p = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / p;
    const step = (f <= 1 ? 1 : f <= 2 ? 2 : f <= 5 ? 5 : 10) * p;
    const out = [];
    for (let v = 0; v <= max * 1.0001; v += step) out.push(+v.toFixed(10));
    return out;
  }

  // Ellipsise to fit on a canvas (maxWidth would squash the glyphs instead)
  function fit(ctx, t, maxW) {
    t = String(t);
    if (ctx.measureText(t).width <= maxW) return t;
    while (t.length > 1 && ctx.measureText(t + '…').width > maxW) t = t.slice(0, -1);
    return t.trimEnd() + '…';
  }

  // ── Shared tooltip ─────────────────────────────────────────────────────
  let tipEl = null;
  function tip(html, e) {
    if (!tipEl) { tipEl = document.createElement('div'); tipEl.className = 'th-tip'; document.body.appendChild(tipEl); }
    tipEl.innerHTML = html;
    tipEl.classList.add('show');
    const w = tipEl.offsetWidth, h = tipEl.offsetHeight;
    let x = e.clientX + 14, y = e.clientY + 14;
    if (x + w > window.innerWidth - 8) x = e.clientX - w - 14;
    if (y + h > window.innerHeight - 8) y = e.clientY - h - 14;
    tipEl.style.left = Math.max(4, x) + 'px';
    tipEl.style.top = Math.max(4, y) + 'px';
  }
  function untip() { if (tipEl) tipEl.classList.remove('show'); }

  // Re-render on width change; stop once Shiny has replaced the widget.
  function observe(el, render) {
    let lastW = -1;
    const ro = new ResizeObserver(() => {
      if (!el.isConnected) { ro.disconnect(); untip(); return; }
      if (el.clientWidth !== lastW) { lastW = el.clientWidth; render(); }
    });
    ro.observe(el);
    render();
  }

  function pillGroup(el, sel, key, onPick, multi) {
    el.querySelectorAll(sel + ' button').forEach(b => b.addEventListener('click', () => {
      if (multi) b.classList.toggle('on');
      else el.querySelectorAll(sel + ' button').forEach(x => x.classList.toggle('on', x === b));
      onPick(b.dataset[key], b.classList.contains('on'));
    }));
  }

  // ═══ Funnel plot ═══════════════════════════════════════════════════════
  // Each site's rate against its size, with binomial control limits around the
  // trial-wide rate — a site outside the funnel is unusual, not just small.
  function Funnel(el, D) {
    let metric = 'cos', cur = [];
    el.innerHTML = `
      <div class="th-toolbar">
        <div class="th-pills th-metric" role="group" aria-label="Measure">
          <button type="button" class="on" data-m="cos">Change of status</button>
          <button type="button" data-m="over">Overdue CRFs</button>
        </div>
        <span class="th-note"></span>
      </div>
      <div class="th-plot"></div>
      <div class="th-legend">
        <span><i class="th-lg-line th-lg-mean"></i>Trial-wide rate</span>
        <span><i class="th-lg-line th-lg-warn"></i>95% limit</span>
        <span><i class="th-lg-line th-lg-alarm"></i>99.8% limit</span>
        <span><i class="th-lg-dot" style="background:#E30513"></i>Above 99.8%</span>
        <span><i class="th-lg-dot" style="background:#F07F3C"></i>Above 95%</span>
        <span><i class="th-lg-dot" style="background:#1B1B1B"></i>Within limits</span>
        <span><i class="th-lg-dot" style="background:#8A8A8C"></i>Too few to judge</span>
      </div>`;
    const box = el.querySelector('.th-plot'), note = el.querySelector('.th-note');
    pillGroup(el, '.th-metric', 'm', m => { metric = m; render(); });

    function render() {
      const w = box.clientWidth;
      if (w < 120) return;
      const cos = metric === 'cos';
      const p = +(cos ? D.p_cos : D.p_over);
      const zw = +D.z_warn || 1.96, za = +D.z_alarm || 3.09;
      note.textContent = cos ? 'Participants with a change of status (deaths excluded) against site size'
                             : 'Overdue CRFs against the CRFs expected so far';
      cur = arr(D.sites).map(s => {
        const x = +(cos ? s.n : s.expected), k = +(cos ? s.cos : s.overdue);
        const z = x > 0 && p > 0 && p < 1 ? (k / x - p) / Math.sqrt(p * (1 - p) / x) : 0;
        return { site: s.site, x, k, r: x > 0 ? k / x : 0, z, small: cos && x < +D.min_n };
      }).filter(s => s.x > 0);
      if (!cur.length || !(p >= 0)) {
        box.innerHTML = `<div class="th-empty">${cos ? 'No participants yet.' : 'No CRFs are due yet.'}</div>`;
        return;
      }
      const h = 300, ml = 46, mr = 16, mt = 12, mb = 38;
      const nBig = Math.max(...cur.map(s => s.x));
      const xmax = nBig * 1.1 + 1;
      const lim = (n, z) => p + z * Math.sqrt(p * (1 - p) / n);
      // Scale to the sites rather than the funnel's mouth: keep the alarm limit
      // visible at the largest site and let the curves leave through the top.
      const ymax = Math.min(1, Math.max(0.05, p * 2, lim(nBig, za) * 1.12, ...cur.map(s => s.r * 1.3)));
      const X = n => ml + n / xmax * (w - ml - mr);
      const Y = v => mt + (1 - v / ymax) * (h - mt - mb);
      let g = '';
      niceTicks(ymax * 100, 4).forEach(v => {
        g += `<line x1="${ml}" x2="${w - mr}" y1="${Y(v / 100)}" y2="${Y(v / 100)}" class="th-gl"/>` +
             `<text x="${ml - 6}" y="${Y(v / 100) + 3.5}" class="th-ax" text-anchor="end">${v}%</text>`;
      });
      niceTicks(xmax, 5).forEach(v => { g += `<text x="${X(v)}" y="${h - mb + 16}" class="th-ax" text-anchor="middle">${v}</text>`; });
      g += `<text x="${ml + (w - ml - mr) / 2}" y="${h - 4}" class="th-ax th-ax-title" text-anchor="middle">${cos ? 'Participants randomised' : 'CRFs expected by now'}</text>`;
      const curve = z => {
        let d = '', started = false;
        for (let i = 0; i <= 160; i++) {
          const n = Math.max(0.3, xmax * i / 160), v = lim(n, z);
          if (v < 0 || v > ymax) { started = false; continue; }
          d += (started ? 'L' : 'M') + X(n).toFixed(1) + ',' + Y(v).toFixed(1);
          started = true;
        }
        return d;
      };
      g += `<path d="${curve(za)}" class="th-lim-alarm"/><path d="${curve(zw)}" class="th-lim-warn"/>` +
           `<path d="${curve(-zw)}" class="th-lim-warn th-lim-low"/>`;
      g += `<line x1="${ml}" x2="${w - mr}" y1="${Y(p)}" y2="${Y(p)}" class="th-mean"/>` +
           `<text x="${w - mr}" y="${Y(p) - 5}" class="th-ax th-mean-lbl" text-anchor="end">Trial-wide ${(p * 100).toFixed(1)}%</text>`;
      cur.forEach((s, i) => {
        const col = s.small ? '#8A8A8C' : s.z >= za ? '#E30513' : s.z >= zw ? '#F07F3C' : '#1B1B1B';
        g += `<circle cx="${X(s.x)}" cy="${Y(s.r)}" r="7" fill="${col}" class="th-pt" data-i="${i}"/>`;
      });

      // Labels: try right, left, above, below; skip one that can't fit without
      // overlapping a point or another label (it's still in the tooltip).
      const mc = document.createElement('canvas').getContext('2d');
      mc.font = `600 11px ${getComputedStyle(el).fontFamily || 'sans-serif'}`;
      const taken = cur.map(s => ({ x0: X(s.x) - 8, x1: X(s.x) + 8, y0: Y(s.r) - 8, y1: Y(s.r) + 8 }));
      const meanW = mc.measureText(`Trial-wide ${(p * 100).toFixed(1)}%`).width;
      taken.push({ x0: w - mr - meanW - 4, x1: w - mr, y0: Y(p) - 17, y1: Y(p) - 2 });
      const clash = b => b.x0 < ml || b.x1 > w - mr || b.y0 < mt - 4 || b.y1 > h - mb ||
                         taken.some(o => b.x0 < o.x1 && b.x1 > o.x0 && b.y0 < o.y1 && b.y1 > o.y0);
      const wanted = cur.length <= 14;
      cur.map((s, i) => i)
        .sort((a, b) => ((cur[b].z >= zw) - (cur[a].z >= zw)) || (cur[b].x - cur[a].x))
        .forEach(i => {
          const s = cur[i];
          if (!wanted && s.z < zw) return;
          const cx = X(s.x), cy = Y(s.r), tw = mc.measureText(s.site).width;
          const spot = [
            { x: cx + 11, y: cy + 4,  a: 'start',  b: { x0: cx + 10, x1: cx + 13 + tw, y0: cy - 7, y1: cy + 6 } },
            { x: cx - 11, y: cy + 4,  a: 'end',    b: { x0: cx - 13 - tw, x1: cx - 10, y0: cy - 7, y1: cy + 6 } },
            { x: cx,      y: cy - 12, a: 'middle', b: { x0: cx - tw / 2 - 2, x1: cx + tw / 2 + 2, y0: cy - 22, y1: cy - 9 } },
            { x: cx,      y: cy + 21, a: 'middle', b: { x0: cx - tw / 2 - 2, x1: cx + tw / 2 + 2, y0: cy + 9, y1: cy + 22 } }
          ].find(c => !clash(c.b));
          if (!spot) return;
          taken.push(spot.b);
          g += `<text x="${spot.x}" y="${spot.y}" class="th-pt-lbl" text-anchor="${spot.a}">${esc(s.site)}</text>`;
        });
      box.innerHTML = `<svg width="${w}" height="${h}" role="img" aria-label="Funnel plot of ${cos ? 'change-of-status' : 'overdue CRF'} rates by site">${g}</svg>`;
    }

    box.addEventListener('mousemove', e => {
      const c = e.target.closest('[data-i]');
      if (!c) return untip();
      const s = cur[+c.dataset.i], cos = metric === 'cos';
      const p = +(cos ? D.p_cos : D.p_over);
      tip(`<b>${esc(s.site)}</b><div>${s.k} of ${s.x} ${cos ? 'participants' : 'expected CRFs'} (${Math.round(s.r * 100)}%)</div>` +
          `<div>Trial-wide ${(p * 100).toFixed(1)}% · z = ${s.z.toFixed(2)}</div>` +
          (s.small ? '<div class="th-tip-m">Too few participants to judge yet</div>' : '') +
          '<div class="th-tip-m">Click for site details</div>', e);
    });
    box.addEventListener('mouseleave', untip);
    box.addEventListener('click', e => {
      const c = e.target.closest('[data-i]');
      if (c) shiny('th_site_open', { site: cur[+c.dataset.i].site, n: Math.random() });
    });
    observe(el, render);
  }

  // ═══ CRF completeness grid ═════════════════════════════════════════════
  const CELL = {
    c: { name: 'Complete',            col: '#00ACA9' },
    o: { name: 'Overdue',             col: '#E30513' },
    d: { name: 'Due now',             col: '#FBBF24' },
    n: { name: 'Not due yet',         col: '#E8EDF3' },
    a: { name: 'Awaiting visit date', col: '#CFCFCF' },
    x: { name: 'Not required',        col: null },
    '-': { name: 'Not tracked',       col: null }
  };

  function CrfGrid(el, D) {
    const forms = arr(D.forms);
    const rows0 = arr(D.rows).map(r => ({ id: String(r.id), site: String(r.site), stage: r.stage || '',
                                          s: String(r.s || ''), d: arr(r.d), e: String(r.e || '') }));
    const sites = [...new Set(rows0.map(r => r.site))].sort();
    el.innerHTML = `
      <div class="th-toolbar">
        <select class="th-sel" aria-label="Site"><option value="">All sites</option>${sites.map(s => `<option value="${esc(s)}">${esc(s)}</option>`).join('')}</select>
        <div class="th-pills th-kind" role="group" aria-label="Forms">
          <button type="button" class="on" data-k="all">All forms</button><button type="button" data-k="CRF">Site CRFs</button><button type="button" data-k="PROM">Questionnaires</button>
        </div>
        <div class="th-pills th-sort" role="group" aria-label="Sort">
          <button type="button" class="on" data-s="site">By site</button><button type="button" data-s="over">Most overdue</button>
        </div>
        <label class="th-check"><input type="checkbox"> Only participants with overdue forms</label>
      </div>
      <div class="th-glegend"></div>
      <div class="th-ghead"><canvas></canvas></div>
      <div class="th-gbody"><canvas></canvas></div>
      <div class="th-gfoot"></div>`;
    const sel = el.querySelector('.th-sel'), only = el.querySelector('.th-check input');
    const hc = el.querySelector('.th-ghead canvas'), bc = el.querySelector('.th-gbody canvas');
    const body = el.querySelector('.th-gbody'), legend = el.querySelector('.th-glegend'), foot = el.querySelector('.th-gfoot');
    const LBL = 176, RH = 17, HH = 96;
    let kind = 'all', sortBy = 'site', cols = [], rows = [], cw = 20, F = 'system-ui, sans-serif';
    pillGroup(el, '.th-kind', 'k', v => { kind = v; render(); });
    pillGroup(el, '.th-sort', 's', v => { sortBy = v; render(); });
    sel.addEventListener('change', render);
    only.addEventListener('change', render);

    function setup(cv, w, h) {
      const d = window.devicePixelRatio || 1;
      cv.width = Math.round(w * d); cv.height = Math.round(h * d);
      cv.style.width = w + 'px'; cv.style.height = h + 'px';
      const c = cv.getContext('2d'); c.setTransform(d, 0, 0, d, 0, 0);
      return c;
    }

    function render() {
      const W = body.clientWidth;
      if (W < 200) return;
      F = getComputedStyle(el).fontFamily || F;
      cols = forms.map((f, j) => ({ f, j })).filter(c => kind === 'all' || c.f.kind === kind);
      const site = sel.value;
      rows0.forEach(r => {
        r.o = 0; r.maxD = 0;
        cols.forEach(c => { if (r.s[c.j] === 'o') { r.o++; r.maxD = Math.max(r.maxD, +r.d[c.j] || 0); } });
      });
      rows = rows0.filter(r => (!site || r.site === site) && (!only.checked || r.o > 0));
      rows.sort(sortBy === 'over'
        ? (a, b) => (b.o - a.o) || (b.maxD - a.maxD) || a.id.localeCompare(b.id)
        : (a, b) => a.site.localeCompare(b.site) || a.id.localeCompare(b.id));
      cw = Math.max(14, Math.min(44, (W - LBL - 4) / Math.max(1, cols.length)));
      const gw = LBL + cw * cols.length;

      const tally = {};
      let est = false;
      rows.forEach(r => cols.forEach(c => {
        const ch = r.s[c.j] || '-';
        tally[ch] = (tally[ch] || 0) + 1;
        if (r.e[c.j] === '1' && (ch === 'o' || ch === 'd')) est = true;
      }));
      legend.innerHTML = ['c', 'o', 'd', 'n', 'a', 'x'].filter(k => tally[k])
        .map(k => `<span><i class="th-cell th-cell-${k}"></i>${CELL[k].name} <b>${tally[k].toLocaleString()}</b></span>`).join('') +
        `<span class="th-glegend-r">${rows.length} participant${rows.length === 1 ? '' : 's'} · ${cols.length} form${cols.length === 1 ? '' : 's'}</span>`;

      // Header: timepoint bands and angled form names
      const hx = setup(hc, W, HH);
      hx.clearRect(0, 0, W, HH);
      hx.textBaseline = 'middle';
      let j0 = 0, band = false;
      while (j0 < cols.length) {
        let j1 = j0;
        while (j1 + 1 < cols.length && cols[j1 + 1].f.tp === cols[j0].f.tp) j1++;
        const x0 = LBL + j0 * cw, x1 = LBL + (j1 + 1) * cw;
        hx.fillStyle = band ? '#F4F4F4' : '#E9E9E9';
        hx.fillRect(x0 + 1, 2, x1 - x0 - 2, 18);
        hx.fillStyle = '#1B1B1B'; hx.font = `700 10.5px ${F}`; hx.textAlign = 'center';
        hx.fillText(fit(hx, cols[j0].f.tp, x1 - x0 - 6), (x0 + x1) / 2, 11);
        band = !band; j0 = j1 + 1;
      }
      hx.textAlign = 'left';
      cols.forEach((c, k) => {
        hx.save();
        hx.translate(LBL + (k + 0.5) * cw + 3, HH - 6);
        hx.rotate(-Math.PI / 3.4);
        hx.fillStyle = c.f.kind === 'CRF' ? '#1B1B1B' : '#58595B';
        hx.font = `${c.f.kind === 'CRF' ? 600 : 500} 10.5px ${F}`;
        hx.fillText(fit(hx, c.f.form, 84), 0, 0);
        hx.restore();
      });
      hx.fillStyle = '#58595B'; hx.font = `600 10px ${F}`;
      hx.fillText('PARTICIPANT', 8, HH - 10);
      hx.fillText('SITE', 84, HH - 10);

      // Body
      const Hh = Math.max(RH, rows.length * RH);
      const ctx = setup(bc, W, Hh);
      ctx.clearRect(0, 0, W, Hh);
      ctx.textBaseline = 'middle';
      let prev = null; band = false;
      rows.forEach((r, i) => {
        const y = i * RH;
        if (sortBy === 'site' && r.site !== prev) {
          band = !band; prev = r.site;
          if (i) { ctx.fillStyle = '#E3E3E3'; ctx.fillRect(0, y, gw, 1); }
        }
        if (band && sortBy === 'site') { ctx.fillStyle = '#F8F8F8'; ctx.fillRect(0, y, LBL - 4, RH); }
        ctx.fillStyle = '#1B1B1B'; ctx.font = `600 11px ${F}`;
        ctx.fillText(fit(ctx, r.id, 72), 8, y + RH / 2);
        ctx.fillStyle = '#58595B'; ctx.font = `400 10.5px ${F}`;
        ctx.fillText(fit(ctx, r.site, LBL - 92), 84, y + RH / 2);
        cols.forEach((c, k) => {
          const ch = r.s[c.j] || '-', x = LBL + k * cw + 1, cy = y + 2, w = cw - 2, h = RH - 4;
          if (ch === 'x' || ch === '-') {
            ctx.fillStyle = '#F8F8F8'; ctx.fillRect(x, cy, w, h);
            if (ch === 'x') {
              ctx.strokeStyle = '#D9D9D9'; ctx.lineWidth = 1;
              ctx.beginPath(); ctx.moveTo(x + 1, cy + h - 1); ctx.lineTo(x + w - 1, cy + 1); ctx.stroke();
            }
            return;
          }
          ctx.fillStyle = ch === 'o' && (+r.d[c.j] || 0) >= 30 ? '#C20019' : CELL[ch].col;
          ctx.beginPath(); ctx.roundRect ? ctx.roundRect(x, cy, w, h, 2) : ctx.rect(x, cy, w, h); ctx.fill();
          if (r.e[c.j] === '1' && (ch === 'o' || ch === 'd')) {
            ctx.fillStyle = '#fff'; ctx.beginPath(); ctx.arc(x + w - 4, cy + 4, 1.8, 0, 7); ctx.fill();
          }
        });
      });
      if (!rows.length) {
        ctx.fillStyle = '#58595B'; ctx.font = `500 12px ${F}`;
        ctx.fillText('No participants match these filters.', 8, RH / 2);
      }
      foot.textContent = 'Dark red = more than 30 days overdue. Hover over a cell for details, or click a row to open that participant.' +
        (est ? ' A white dot means the due date is estimated because the operation or discharge date hasn\'t been entered yet.' : '');
    }

    bc.addEventListener('mousemove', e => {
      const b = bc.getBoundingClientRect(), mx = e.clientX - b.left, my = e.clientY - b.top;
      const r = rows[Math.floor(my / RH)];
      if (!r) return untip();
      if (mx < LBL) {
        return tip(`<b>${esc(r.id)}</b> · ${esc(r.site)}<div>${esc(r.stage)}</div>` +
                   `<div>${r.o ? `${r.o} overdue form${r.o === 1 ? '' : 's'}, longest ${r.maxD} days` : 'Nothing overdue'}</div>` +
                   '<div class="th-tip-m">Click to open this participant</div>', e);
      }
      const c = cols[Math.floor((mx - LBL) / cw)];
      if (!c) return untip();
      const ch = r.s[c.j] || '-', dd = +r.d[c.j];
      tip(`<b>${esc(r.id)}</b> · ${esc(r.site)}<div>${esc(c.f.tp)} — ${esc(c.f.form)}</div>` +
          `<div class="th-tip-st th-tip-${ch}">${CELL[ch].name}${ch === 'o' && dd >= 0 ? ` by ${dd} day${dd === 1 ? '' : 's'}` : ''}</div>` +
          (r.e[c.j] === '1' && (ch === 'o' || ch === 'd') ? '<div class="th-tip-m">Due date estimated — visit date not entered</div>' : ''), e);
    });
    bc.addEventListener('mouseleave', untip);
    bc.addEventListener('click', e => {
      const r = rows[Math.floor((e.clientY - bc.getBoundingClientRect().top) / RH)];
      if (r) shiny('th_participant_open', { id: r.id, n: Math.random() });
    });
    observe(el, render);
  }

  // ═══ Randomisation punchcard ═══════════════════════════════════════════
  function Punchcard(el, D) {
    const DAYS = ['Mon', 'Tue', 'Wed', 'Thu', 'Fri', 'Sat', 'Sun'];
    const heat = arr(D.heat).map(r => arr(r).map(Number));
    const wd = arr(D.by_wday).map(Number), hr = arr(D.by_hour).map(Number);
    const work = arr(D.work_days).map(Number);
    const max = Math.max(1, ...heat.flat());
    const CAT = { 'In hours': '#00ACA9', 'Weekday out of hours': '#1B1B1B', 'Weekend': '#C59A00',
                  'Bank holiday': '#CF4527', 'Time not recorded': '#CFCFCF' };
    const cats = D.cats || {};
    const keys = Object.keys(CAT).filter(k => +cats[k] > 0);
    const tot = keys.reduce((a, k) => a + +cats[k], 0) || 1;
    el.innerHTML = `<div class="th-plot"></div>
      <div class="th-cats">
        <div class="th-stack">${keys.map(k => `<i style="width:${100 * cats[k] / tot}%;background:${CAT[k]}"></i>`).join('')}</div>
        <div class="th-cat-list">${keys.map(k => `<span><i style="background:${CAT[k]}"></i>${k} <b>${cats[k]}</b> <em>${Math.round(100 * cats[k] / tot)}%</em></span>`).join('')}</div>
      </div>`;
    const box = el.querySelector('.th-plot');
    const inHours = (r, c) => work.includes(r + 1) && c * 60 >= +D.start && c * 60 < +D.end;

    function render() {
      const w = box.clientWidth;
      if (w < 240) return;
      const ml = 40, mr = 70, mt = 6, mb = 60;
      const cwid = (w - ml - mr) / 24, ch = Math.min(30, Math.max(18, cwid * 1.05));
      const gh = 7 * ch, h = mt + gh + mb;
      const X = hh => ml + hh * cwid;
      let g = '';
      work.forEach(d => {
        g += `<rect x="${X(+D.start / 60)}" y="${mt + (d - 1) * ch + 1}" width="${X(+D.end / 60) - X(+D.start / 60)}" height="${ch - 2}" rx="4" class="th-wh"/>`;
      });
      const wmax = Math.max(1, ...wd);
      for (let r = 0; r < 7; r++) {
        const y = mt + r * ch;
        g += `<text x="${ml - 8}" y="${y + ch / 2 + 4}" class="th-ax${work.includes(r + 1) ? '' : ' th-ax-we'}" text-anchor="end">${DAYS[r]}</text>`;
        for (let c = 0; c < 24; c++) {
          const v = (heat[r] || [])[c] || 0, cx = X(c + 0.5), cy = y + ch / 2;
          if (v) {
            const rad = 2.5 + Math.sqrt(v / max) * (Math.min(cwid, ch) / 2 - 3.5);
            g += `<circle cx="${cx}" cy="${cy}" r="${rad.toFixed(1)}" class="th-pc" style="fill-opacity:${(0.35 + 0.65 * v / max).toFixed(2)}"/>`;
          } else g += `<circle cx="${cx}" cy="${cy}" r="1.2" class="th-pc0"/>`;
          g += `<rect x="${X(c)}" y="${y}" width="${cwid}" height="${ch}" fill="transparent" data-r="${r}" data-c="${c}"/>`;
        }
        const bw = (mr - 34) * (wd[r] || 0) / wmax;
        g += `<rect x="${w - mr + 8}" y="${y + ch * 0.22}" width="${bw}" height="${ch * 0.56}" rx="2" class="th-marg"/>` +
             `<text x="${w - mr + 12 + bw}" y="${y + ch / 2 + 4}" class="th-ax">${wd[r] || 0}</text>`;
      }
      const hmax = Math.max(1, ...hr), by = mt + gh + 10, bh = 28;
      for (let c = 0; c < 24; c++) {
        const bhh = bh * (hr[c] || 0) / hmax;
        g += `<rect x="${X(c) + 1.5}" y="${by + bh - bhh}" width="${Math.max(1, cwid - 3)}" height="${bhh}" rx="1.5" class="th-marg${c * 60 >= +D.start && c * 60 < +D.end ? ' th-marg-in' : ''}"/>`;
        if (c % 3 === 0) g += `<text x="${X(c)}" y="${by + bh + 15}" class="th-ax">${pad2(c)}:00</text>`;
      }
      box.innerHTML = `<svg width="${w}" height="${h}" role="img" aria-label="Randomisations by weekday and hour of day">${g}</svg>`;
    }

    box.addEventListener('mousemove', e => {
      const c = e.target.closest('[data-r]');
      if (!c) return untip();
      const r = +c.dataset.r, hh = +c.dataset.c, v = (heat[r] || [])[hh] || 0;
      tip(`<b>${DAYS[r]} ${pad2(hh)}:00–${pad2((hh + 1) % 24)}:00</b>` +
          `<div>${v} randomisation${v === 1 ? '' : 's'}</div>` +
          `<div class="th-tip-m">${inHours(r, hh) ? 'In working hours' : 'Out of hours'}</div>`, e);
    });
    box.addEventListener('mouseleave', untip);
    observe(el, render);
  }

  // ═══ Trajectory fan chart ══════════════════════════════════════════════
  function Trajectory(el, D) {
    const show = { band: true, pace: true, trend: true, sites: true, schedule: true };
    let hz = 'target', view = null;
    const W0 = arr(D.weeks).map(ms), A = arr(D.actual).map(Number), PW = arr(D.pweeks).map(ms);
    const S = { pace: arr(D.pace).map(Number), trend: arr(D.trend).map(Number), sites: arr(D.sites).map(Number) };
    const B = {};
    ['p10', 'p25', 'p50', 'p75', 'p90'].forEach(k => { B[k] = arr((D.band || {})[k]).map(Number); });
    const sched = D.schedule ? arr(D.schedule.t).map((t, i) => [ms(t), +arr(D.schedule.v)[i]]) : [];
    const target = +D.target, today = ms(D.today), n0 = +D.n_now;
    const f = D.finish || {};
    const AX = W0.map((t, i) => i === W0.length - 1 ? today : t + 7 * DAY);   // end of each week
    const NAMES = { pace: 'Current pace', trend: 'Recent trend', sites: 'Site-based' };
    const growth = +D.growth;
    const stalled = !(+D.lambda > 0) && n0 < target;
    const T = `<b>${target.toLocaleString()}</b>`;
    const headline = n0 >= target
      ? `The target of ${T} has been reached — <b>${n0.toLocaleString()}</b> randomised so far.`
      : stalled
        ? `No one has been randomised in the last <b>${D.window} weeks</b>${D.last_rand ? ` (most recently on ${fmt(ms(D.last_rand))})` : ''}, so at the current pace the target of ${T} won't be reached.`
        : (f.p50 || f.pace)
          ? `At the current pace of <b>${D.per_month} a month</b>, the target of ${T} is reached around <b>${fmtM(f.p50 || f.pace)}</b>${f.p10 && f.p90 ? ` <span class="th-muted">(80% range ${fmtM(f.p10)} – ${fmtM(f.p90)})</span>` : ''}.`
          : `At the current pace of <b>${D.per_month} a month</b>, the target of ${T} isn't reached within the projection.`;
    const notReached = 'target not reached';

    el.innerHTML = `
      <div class="th-traj-sum">
        <div class="th-traj-main">${headline}</div>
        <div class="th-traj-facts">
          ${D.prob != null && D.end_date && n0 < target ? `<span class="th-chip ${D.prob >= 0.8 ? 'good' : D.prob >= 0.5 ? 'warn' : 'bad'}"><b>${Math.round(D.prob * 100)}%</b> chance of reaching target by ${fmtM(D.end_date)}</span>` : ''}
          ${stalled ? '' : `<span class="th-chip">${growth >= 0 ? `If the last ${D.window} weeks' growth continues (+${growth}% a month)` : `If the last ${D.window} weeks' slowdown continues (${growth}% a month)`} → ${f.trend ? fmtM(f.trend) : notReached}</span>`}
          <span class="th-chip">Site-based: ${D.sites_per_month} a month from ${D.open_sites} open site${D.open_sites === 1 ? '' : 's'}${D.to_open ? ` plus ${D.to_open} still to open` : ''} → ${f.sites ? fmtM(f.sites) : notReached}</span>
        </div>
      </div>
      <div class="th-toolbar">
        <div class="th-pills th-scen" role="group" aria-label="Show">
          ${[['pace', 'Current pace'], ['trend', 'Recent trend'], ['sites', 'Site-based'], ['band', 'Likely range'], ['schedule', 'Target schedule']]
            .filter(([k]) => k !== 'schedule' || sched.length)
            .map(([k, l]) => `<button type="button" class="on" data-k="${k}"><i class="th-sw th-sw-${k}"></i>${l}</button>`).join('')}
        </div>
        <div class="th-pills th-hz" role="group" aria-label="Time range">
          <button type="button" class="on" data-h="target">To target</button><button type="button" data-h="year">Next 12 months</button>
        </div>
      </div>
      <div class="th-plot"></div>`;
    const box = el.querySelector('.th-plot');
    pillGroup(el, '.th-scen', 'k', (k, on) => { show[k] = on; render(); }, true);
    pillGroup(el, '.th-hz', 'h', v => { hz = v; render(); });

    const schedAt = t => {
      if (!sched.length) return null;
      if (t <= sched[0][0]) return sched[0][1];
      for (let i = 1; i < sched.length; i++)
        if (t <= sched[i][0]) return sched[i - 1][1] + (sched[i][1] - sched[i - 1][1]) * (t - sched[i - 1][0]) / (sched[i][0] - sched[i - 1][0]);
      return sched[sched.length - 1][1];
    };

    function render() {
      const w = box.clientWidth;
      if (w < 240 || !W0.length) return;
      const h = 340, ml = 50, mr = 20, mt = 16, mb = 30;
      // "To target": out to the latest projected finish or the schedule's end,
      // but never more than three years ahead (a stalled trial never finishes).
      const fin = [f.p50, f.pace, f.trend, f.sites, D.end_date].filter(Boolean).map(ms);
      let xEnd = hz === 'year' ? today + 365 * DAY
        : Math.min(today + 3 * 365 * DAY, Math.max(today + (fin.length ? 180 : 365) * DAY, ...fin) + 60 * DAY);
      if (PW.length) xEnd = Math.min(xEnd, PW[PW.length - 1]);
      const x0 = W0[0];
      const idx = PW.map((t, i) => i).filter(i => PW[i] <= xEnd + 7 * DAY);
      const px = [today, ...idx.map(i => PW[i])];
      let ymax;
      if (hz === 'target') ymax = target * 1.08;
      else {
        let m = n0;
        idx.forEach(i => {
          ['pace', 'trend', 'sites'].forEach(k => { if (show[k]) m = Math.max(m, S[k][i]); });
          if (show.band && B.p90.length) m = Math.max(m, B.p90[i]);
        });
        if (show.schedule) sched.forEach(([t, v]) => { if (t <= xEnd) m = Math.max(m, v); });
        ymax = Math.max(10, m * 1.1);
      }
      const X = t => ml + (t - x0) / (xEnd - x0) * (w - ml - mr);
      const Y = v => mt + (1 - v / ymax) * (h - mt - mb);
      view = { X, Y, x0, xEnd, ml, mr, mt, mb, w, h };
      const line = (xs, ys) => xs.map((t, i) => `${i ? 'L' : 'M'}${X(t).toFixed(1)},${Y(ys[i]).toFixed(1)}`).join('');
      const clip = el.id + '-clip';
      let g = `<defs><clipPath id="${clip}"><rect x="${ml}" y="${mt}" width="${w - ml - mr}" height="${h - mt - mb}"/></clipPath></defs>`;
      niceTicks(ymax, 4).forEach(v => {
        g += `<line x1="${ml}" x2="${w - mr}" y1="${Y(v)}" y2="${Y(v)}" class="th-gl"/>` +
             `<text x="${ml - 6}" y="${Y(v) + 3.5}" class="th-ax" text-anchor="end">${v.toLocaleString()}</text>`;
      });
      const months = [];
      for (let d = new Date(x0); d.getTime() <= xEnd; d = new Date(Date.UTC(d.getUTCFullYear(), d.getUTCMonth() + 1, 1))) {
        const t = Date.UTC(d.getUTCFullYear(), d.getUTCMonth(), 1);
        if (t >= x0) months.push(t);
      }
      const every = Math.max(1, Math.ceil(months.length / 8));
      months.forEach((t, i) => {
        if (i % every) return;
        g += `<text x="${X(t)}" y="${h - 10}" class="th-ax" text-anchor="middle">${new Date(t).toLocaleDateString('en-GB', { month: 'short', year: '2-digit', timeZone: 'UTC' })}</text>`;
      });
      let c = `<g clip-path="url(#${clip})">`;
      if (show.band && B.p10.length) {
        const area = (lo, hi) => {
          const a = [n0, ...idx.map(i => B[lo][i])], b = [n0, ...idx.map(i => B[hi][i])];
          return 'M' + px.map((t, i) => `${X(t).toFixed(1)},${Y(b[i]).toFixed(1)}`).join('L') +
                 'L' + px.map((t, i) => `${X(t).toFixed(1)},${Y(a[i]).toFixed(1)}`).reverse().join('L') + 'Z';
        };
        c += `<path d="${area('p10', 'p90')}" class="th-band1"/><path d="${area('p25', 'p75')}" class="th-band2"/>`;
      }
      if (show.schedule && sched.length) c += `<path d="${line(sched.map(s => s[0]), sched.map(s => s[1]))}" class="th-sched"/>`;
      c += `<line x1="${ml}" x2="${w - mr}" y1="${Y(target)}" y2="${Y(target)}" class="th-target"/>`;
      // Projections that land on the same line (e.g. all flat when recruitment
      // has stalled) are drawn once and named, rather than as a tangle of dashes.
      const drawn = [];
      ['pace', 'trend', 'sites'].forEach(k => {
        if (!show[k] || !S[k].length) return;
        const ys = idx.map(i => S[k][i]);
        const tol = Math.max(0.5, ymax * 0.004);
        const twin = drawn.find(d => d.ys.every((v, j) => Math.abs(v - ys[j]) <= tol));
        if (twin) twin.names.push(NAMES[k]); else drawn.push({ k, ys, names: [NAMES[k]] });
      });
      drawn.slice().reverse().forEach(d => { c += `<path d="${line(px, [n0, ...d.ys])}" class="th-l-${d.k}"/>`; });
      c += `<path d="${line(AX, A)}" class="th-l-actual"/></g>`;
      drawn.filter(d => d.names.length > 1 && d.ys.length).forEach(d => {
        const nm = d.names.map((n, i) => i ? n.toLowerCase() : n);
        const txt = `${nm.slice(0, -1).join(', ')} and ${nm[nm.length - 1]} are the same`;
        const ly = Math.max(mt + 22, Y(d.ys[d.ys.length - 1]) - 8);
        g += `<text x="${w - mr - 4}" y="${ly}" class="th-ax th-merge-lbl" text-anchor="end">${txt}</text>`;
      });
      if (Y(target) > mt + 8) g += `<text x="${ml + 6}" y="${Y(target) - 6}" class="th-ax th-target-lbl">Target ${target.toLocaleString()}</text>`;
      g += `<line x1="${X(today)}" x2="${X(today)}" y1="${mt}" y2="${h - mb}" class="th-today"/>` +
           `<text x="${X(today) + 4}" y="${mt + 10}" class="th-ax">Today · ${n0}</text>`;
      ['pace', 'trend', 'sites'].forEach(k => {
        if (!show[k] || !f[k] || ms(f[k]) > xEnd) return;
        g += `<circle cx="${X(ms(f[k]))}" cy="${Y(target)}" r="5" class="th-fin th-fin-${k}"/>`;
      });
      g += `<line class="th-cross" x1="0" x2="0" y1="${mt}" y2="${h - mb}" style="display:none"/>` +
           `<rect x="${ml}" y="${mt}" width="${w - ml - mr}" height="${h - mt - mb}" fill="transparent" class="th-hover"/>`;
      box.innerHTML = `<svg width="${w}" height="${h}" role="img" aria-label="Cumulative recruitment with projected trajectories">${g}${c}</svg>`;
    }

    box.addEventListener('mousemove', e => {
      if (!view || !e.target.closest('.th-hover')) { untip(); return; }
      const sv = box.querySelector('svg'), cross = box.querySelector('.th-cross');
      const mx = e.clientX - sv.getBoundingClientRect().left;
      const t = view.x0 + (mx - view.ml) / (view.w - view.ml - view.mr) * (view.xEnd - view.x0);
      let html, xt;
      if (t <= today) {
        let i = 0;
        while (i + 1 < AX.length && Math.abs(AX[i + 1] - t) < Math.abs(AX[i] - t)) i++;
        xt = AX[i];
        const sv2 = schedAt(xt);
        html = `<b>${fmt(xt)}</b><div>${A[i]} randomised</div>` +
               (sv2 != null ? `<div class="th-tip-m">Target schedule: ${Math.round(sv2)}</div>` : '');
      } else {
        let i = 0;
        while (i + 1 < PW.length && Math.abs(PW[i + 1] - t) < Math.abs(PW[i] - t)) i++;
        xt = PW[i];
        const sv2 = schedAt(xt);
        html = `<b>${fmt(xt)}</b>` +
          ['pace', 'trend', 'sites'].filter(k => show[k]).map(k => `<div><i class="th-sw th-sw-${k}"></i>${NAMES[k]}: ${Math.round(S[k][i])}</div>`).join('') +
          (show.band && B.p10.length ? `<div class="th-tip-m">Likely range ${Math.round(B.p10[i])}–${Math.round(B.p90[i])}</div>` : '') +
          (sv2 != null ? `<div class="th-tip-m">Target schedule: ${Math.round(sv2)}</div>` : '');
      }
      const cx = view.X(xt);
      cross.setAttribute('x1', cx); cross.setAttribute('x2', cx); cross.style.display = '';
      tip(html, e);
    });
    box.addEventListener('mouseleave', () => {
      untip();
      const cross = box.querySelector('.th-cross');
      if (cross) cross.style.display = 'none';
    });
    observe(el, render);
  }

  // ── Mounting ───────────────────────────────────────────────────────────
  const KINDS = { funnel: Funnel, crfgrid: CrfGrid, punchcard: Punchcard, trajectory: Trajectory };
  function mount(el) {
    if (el.dataset.mounted) return;
    const make = KINDS[el.dataset.kind], src = document.getElementById(el.id + '-data');
    if (!make || !src) return;
    let data;
    try { data = JSON.parse(src.textContent); }
    catch (e) { console.error('Trial health: could not read data for', el.id, e); return; }
    el.dataset.mounted = '1';
    try { make(el, data); }
    catch (e) { console.error('Trial health widget failed:', el.id, e); el.textContent = 'This chart could not be drawn for the current data.'; }
  }
  function scan() { document.querySelectorAll('.th-widget:not([data-mounted])').forEach(mount); }

  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', scan);
  else scan();
  window.TrialHealth = { mount, scan };
})();

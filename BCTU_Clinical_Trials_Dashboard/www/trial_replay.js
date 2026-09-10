/* =============================================================================
   Trial replay — animated participant flow on the Overview tab
   =============================================================================
   Mounts on every `.tr-root` that appears in the page (Shiny re-renders
   included). Data comes from functions/trial_replay.R as JSON in the sibling
   <script type="application/json" id="<root id>-data">. Times are days since
   `start`; null means "hasn't happened / not recorded".
   ========================================================================== */
(function () {
  'use strict';

  const DAY = 864e5;
  const RACE_TOP = 10;
  const SITE_COLS = ['#1B1B1B', '#00ACA9', '#F07F3C', '#C59A00', '#2581C4', '#CF4527',
                     '#65A30D', '#00788E', '#B45309', '#0057BF', '#0F766E', '#8A8A8C'];
  const ICON_PLAY  = '<svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><path d="M8 5.5v13l11-6.5z"/></svg>';
  const ICON_PAUSE = '<svg width="16" height="16" viewBox="0 0 24 24" fill="currentColor" aria-hidden="true"><rect x="6" y="5" width="4" height="14" rx="1"/><rect x="14" y="5" width="4" height="14" rx="1"/></svg>';

  const esc = s => String(s).replace(/[&<>"']/g, c => ({ '&': '&amp;', '<': '&lt;', '>': '&gt;', '"': '&quot;', "'": '&#39;' }[c]));
  const arr = v => (v === null || v === undefined) ? [] : (Array.isArray(v) ? v : [v]);
  const num = v => (v === null || v === undefined || v === '') ? Infinity : +v;

  const TEMPLATE = `
    <div class="tr-timeline">
      <button type="button" class="tr-play" aria-label="Play"></button>
      <div class="tr-now">
        <div class="tr-now-date"></div><div class="tr-now-sub"></div>
        <button type="button" class="tr-link tr-today">Jump to today</button>
      </div>
      <div class="tr-scrub">
        <div class="tr-spark" aria-hidden="true"></div>
        <input type="range" class="tr-range" min="0" step="0.1" aria-label="Replay date">
        <div class="tr-mlabels" aria-hidden="true"></div>
      </div>
      <div class="tr-pills tr-speed" role="group" aria-label="Playback speed">
        <button type="button" data-s="7">1 wk/s</button><button type="button" class="on" data-s="14">2 wk/s</button><button type="button" data-s="30">1 mo/s</button>
      </div>
    </div>
    <div class="tr-kpis" aria-live="polite"></div>
    <div class="tr-section-head">
      <div>
        <h4>Living CONSORT</h4>
        <div class="tr-sub">Each dot is one participant. The big numbers are cumulative, as in a CONSORT diagram. The dots show where each participant is on the replay date. Hover over a dot to see that participant's pathway.</div>
      </div>
      <div class="tr-pills tr-colour" role="group" aria-label="Dot colour">
        <button type="button" class="on" data-c="status">Colour by status</button><button type="button" data-c="site">Colour by site</button>
      </div>
    </div>
    <div class="tr-pipe"><canvas role="img" aria-label="Participant flow"></canvas><div class="tr-tip"></div></div>
    <div class="tr-legend">
      <span><i class="tr-ld tr-ld-dot"></i>Participant</span>
      <span class="tr-legend-done"><i class="tr-ld tr-ld-done"></i><span></span></span>
      <span class="tr-legend-part"><i class="tr-ld tr-ld-ring"></i>Part withdrawal (still in some follow-up)</span>
      <span><i class="tr-ld tr-ld-pulse"></i>Ring = something just happened</span>
    </div>
    <div class="tr-grid">
      <div class="tr-panel tr-panel-race">
        <h4>Site recruitment race</h4>
        <div class="tr-sub">Sites re-rank as they overtake each other. Hover over a site to highlight its participants in the Living CONSORT.</div>
        <div class="tr-race"></div>
        <button type="button" class="tr-link tr-race-more" hidden></button>
        <div class="tr-race-key">
          <span><i class="tr-key-on"></i>On pace</span>
          <span><i class="tr-key-behind"></i>Behind expected</span>
          <span><i class="tr-key-tick"></i>Expected from the site's monthly target</span>
        </div>
      </div>
      <div class="tr-panel tr-panel-pace">
        <div class="tr-panel-head"><h4>Pace vs target</h4><span class="tr-delta" hidden></span></div>
        <div class="tr-pace"></div>
      </div>
      <div class="tr-panel tr-panel-tick">
        <h4>What just happened</h4>
        <ul class="tr-ticker"></ul>
      </div>
    </div>
    <div class="tr-foot"></div>`;

  function Replay(root, D) {
    root.innerHTML = TEMPLATE;
    const $ = s => root.querySelector(s);
    const START = Date.parse(D.start + 'T00:00:00Z');
    const END = +D.end;
    const fmt = (d, y) => new Date(START + d * DAY).toLocaleDateString('en-GB',
      y ? { day: 'numeric', month: 'short', year: 'numeric', timeZone: 'UTC' }
        : { day: 'numeric', month: 'short', timeZone: 'UTC' });
    const RM = !!(window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches);

    // ── Data ────────────────────────────────────────────────────────────────
    const STAGES = arr(D.stages), NS = STAGES.length, LAST = NS - 1;
    const OP_IDX = STAGES.findIndex(s => s.key === 'op');
    const EXITS = arr(D.exits);
    const SITES = arr(D.sites).map((s, i) => ({
      name: String(s.name), openD: num(s.open), rate: +s.monthly_target || 2,
      col: SITE_COLS[i % SITE_COLS.length], n: 0, expected: 0
    }));
    const P = arr(D.p).map(r => {
      const x = (r.x === null || r.x === undefined) ? -1 : +r.x;
      return { id: String(r.id), site: SITES[+r.s], m: arr(r.m).map(num),
               x: x < EXITS.length ? x : -1, xa: num(r.xa), pa: num(r.pa) };
    }).filter(p => p.site && isFinite(p.m[0]));
    P.sort((a, b) => a.m[0] - b.m[0]);

    const TGT = D.target ? arr(D.target.t).map((t, i) => [+t, +arr(D.target.v)[i]]) : null;
    const TRIAL_TARGET = +D.trial_target || 0;
    const PILOT = (D.pilot && +D.pilot.target) ? D.pilot : null;
    const HAS_PART = P.some(p => isFinite(p.pa));
    const targetAt = t => {
      if (!TGT || !TGT.length) return null;
      if (t <= TGT[0][0]) return TGT[0][1];
      for (let i = 1; i < TGT.length; i++) {
        const [a, va] = TGT[i - 1], [b, vb] = TGT[i];
        if (t <= b) return va + (vb - va) * (t - a) / (b - a);
      }
      return TGT[TGT.length - 1][1];
    };

    // Month starts from `start` to the first one after today (pace chart edge)
    const d0 = new Date(START), MONTHS = [];
    for (let i = 0; i < 600; i++) {
      const ms = Date.UTC(d0.getUTCFullYear(), d0.getUTCMonth() + i, 1), t = (ms - START) / DAY;
      MONTHS.push([t, new Date(ms)]);
      if (t > END) break;
    }
    const X1 = MONTHS[MONTHS.length - 1][0];
    const LONG = MONTHS.length > 13;
    const mlabel = d => d.toLocaleDateString('en-GB', LONG
      ? { month: 'short', year: '2-digit', timeZone: 'UTC' } : { month: 'short', timeZone: 'UTC' });

    // ── Events for the ticker ───────────────────────────────────────────────
    const EV = [];
    SITES.forEach(s => { if (isFinite(s.openD)) EV.push({ t: s.openD, col: '#C59A00', txt: `<b>${esc(s.name)}</b> opened to recruitment` }); });
    const marks = new Set([10, 25, 50]);
    for (let k = 100; k <= Math.max(TRIAL_TARGET, P.length); k += 100) marks.add(k);
    if (PILOT) marks.add(+PILOT.target);
    if (TRIAL_TARGET) marks.add(TRIAL_TARGET);
    P.forEach((p, i) => {
      const k = i + 1;
      EV.push({ t: p.m[0], col: '#00ACA9', txt: `<b>${esc(p.id)}</b> randomised at ${esc(p.site.name)}` });
      if (marks.has(k)) EV.push({ t: p.m[0] + 1e-4, col: '#1B1B1B',
        txt: k === TRIAL_TARGET ? `Milestone: <b>recruitment target of ${k}</b> reached`
           : PILOT && k === +PILOT.target ? `Milestone: <b>${k} participants</b>, the internal pilot target`
           : `Milestone: <b>${k} participants</b> randomised` });
      if (p.x >= 0 && isFinite(p.xa)) {
        const e = EXITS[p.x];
        EV.push({ t: p.xa, col: e.col, txt: `<b>${esc(p.id)}</b> discontinued (${esc(e.label)})` });
      }
      if (NS > 1 && isFinite(p.m[LAST]) && !(p.x >= 0 && p.xa < p.m[LAST]))
        EV.push({ t: p.m[LAST], col: '#00788E', txt: `<b>${esc(p.id)}</b> ${esc(STAGES[LAST].done || 'completed follow-up')}` });
      if (isFinite(p.pa)) EV.push({ t: p.pa, col: '#F07F3C', txt: `<b>${esc(p.id)}</b> part withdrawal, still in some follow-up` });
    });
    EV.sort((a, b) => a.t - b.t);

    // ── State ───────────────────────────────────────────────────────────────
    let t = END, playing = false, speed = 14, colourBy = 'status', hl = null, hover = null;
    let lastDay = -1, lastEv = -1, last = performance.now(), frameN = 0, alive = true, raceAll = false;
    let groups = [], lanes = [], reached = [], nNow = 0;

    function stageAt(p, t) {
      if (p.m[0] > t) return -1;
      if (p.x >= 0 && p.xa <= t) return 100 + p.x;
      for (let k = LAST; k > 0; k--) if (p.m[k] <= t) return k;
      return 0;
    }

    // ── Canvas ──────────────────────────────────────────────────────────────
    const pipe = $('.tr-pipe'), cv = pipe.querySelector('canvas'), ctx = cv.getContext('2d'), tip = $('.tr-tip');
    const GAP = 26, HAS_LANE = EXITS.length > 0, H = HAS_LANE ? 430 : 320;
    let W = 0, L = null, C = {}, F = 'system-ui, sans-serif';

    function readColours() {
      const cs = getComputedStyle(root), g = n => cs.getPropertyValue(n).trim();
      C = { ink: g('--tr-ink'), muted: g('--tr-muted'), line: g('--tr-line'), box: g('--tr-box'),
            dot: g('--tr-dot'), done: g('--tr-done'), doneBg: g('--tr-done-bg'), doneLine: g('--tr-done-line'),
            laneBg: g('--tr-lane-bg'), laneLine: g('--tr-lane-line'), laneInk: g('--tr-lane-ink'),
            laneSub: g('--tr-lane-sub'), arrow: g('--tr-arrow'), part: g('--tr-amber'), hover: g('--tr-hover'),
            navy: g('--tr-navy') };
      F = cs.fontFamily || F;
    }

    function layout() {
      W = pipe.clientWidth;
      if (W < 60) { L = null; return; }
      const d = window.devicePixelRatio || 1;
      cv.width = Math.round(W * d); cv.height = Math.round(H * d); cv.style.height = H + 'px';
      ctx.setTransform(d, 0, 0, d, 0, 0);
      const laneH = HAS_LANE ? 108 : 0, boxH = H - laneH - (HAS_LANE ? 22 : 0);
      const bw = (W - GAP * (NS - 1)) / NS;
      L = { boxes: STAGES.map((_, i) => ({ x: i * (bw + GAP), y: 0, w: bw, h: boxH })) };
      if (HAS_LANE) {
        L.lane = { x: 0, y: boxH + 22, w: W, h: laneH };
        const lx = Math.min(150, W * 0.18), sw = (W - lx) / EXITS.length;
        L.sub = EXITS.map((_, i) => ({ x: lx + i * sw, y: L.lane.y, w: sw, h: laneH }));
      }
    }

    function place(list, area, top, padX) {
      const aw = area.w - padX * 2, ah = area.h - top - 10;
      let S = 13, cols = Math.max(1, Math.floor(aw / S));
      if (Math.ceil(list.length / cols) * S > ah) {
        S = Math.max(5, Math.floor(Math.sqrt(aw * ah / list.length)));
        cols = Math.max(1, Math.floor(aw / S));
      }
      list.forEach((p, i) => {
        p.tx = area.x + padX + S / 2 + (i % cols) * S;
        p.ty = area.y + top + S / 2 + Math.floor(i / cols) * S;
        p.r = Math.min(4.6, S * 0.36);
      });
    }

    function update(dt) {
      groups = STAGES.map(() => []); lanes = EXITS.map(() => []); reached = STAGES.map(() => 0); nNow = 0;
      SITES.forEach(s => { s.n = 0; });
      const b0 = L.boxes[0];
      for (const p of P) {
        if (p.m[0] <= t) {
          nNow++; p.site.n++;
          for (let k = 0; k < NS; k++) if (p.m[k] <= t && !(p.x >= 0 && p.xa < p.m[k])) reached[k]++;
        }
        const s = stageAt(p, t), prev = p.stage;
        if (s !== prev) {
          if ((prev === undefined || prev === -1) && s !== -1) { p.px = b0.x + 6; p.py = b0.y + b0.h * 0.6; p.pulse = 1; }
          else if (s >= 100 && !(prev >= 100)) p.pulse = 1;
          p.stage = s;
        }
        if (s === -1) { p.ta = 0; continue; }
        p.ta = 1;
        (s >= 100 ? lanes[s - 100] : groups[s]).push(p);
      }
      groups.forEach((g, k) => { g.sort((a, b) => a.m[k] - b.m[k]); place(g, L.boxes[k], 76, 12); });
      if (HAS_LANE) lanes.forEach((g, k) => { g.sort((a, b) => a.xa - b.xa); place(g, L.sub[k], 46, 12); });
      // Frame-rate independent easing (≈0.14 per frame at 60 fps)
      const k = RM ? 1 : 1 - Math.pow(1e-4, dt), ka = RM ? 1 : 1 - Math.pow(5e-5, dt), kp = Math.pow(0.05, dt);
      for (const p of P) {
        if (p.px === undefined) continue;
        if (p.ta) { p.px += (p.tx - p.px) * k; p.py += (p.ty - p.py) * k; }
        p.a = (p.a || 0) + (p.ta - (p.a || 0)) * ka;
        p.pulse = (p.pulse || 0) * kp;
      }
    }

    function rr(x, y, w, h, r) {
      ctx.beginPath(); ctx.moveTo(x + r, y);
      ctx.arcTo(x + w, y, x + w, y + h, r); ctx.arcTo(x + w, y + h, x, y + h, r);
      ctx.arcTo(x, y + h, x, y, r); ctx.arcTo(x, y, x + w, y, r); ctx.closePath();
    }
    // Ellipsise to fit — canvas maxWidth would squash the glyphs instead.
    // Measures with whatever font / letterSpacing is currently set.
    function fit(txt, maxW) {
      txt = String(txt);
      if (!(maxW > 0) || ctx.measureText(txt).width <= maxW) return txt;
      while (txt.length > 1 && ctx.measureText(txt + '…').width > maxW) txt = txt.slice(0, -1);
      return txt.trimEnd() + '…';
    }
    // Up to two lines, ellipsising the second (Discontinued lane labels)
    function wrap2(txt, maxW) {
      if (ctx.measureText(txt).width <= maxW) return [txt];
      const words = String(txt).split(' ');
      let first = words.shift();
      while (words.length && ctx.measureText(first + ' ' + words[0]).width <= maxW) first += ' ' + words.shift();
      return words.length ? [fit(first, maxW), fit(words.join(' '), maxW)] : [fit(first, maxW)];
    }
    function capLabel(txt, x, y, maxW) {
      ctx.font = `600 10px ${F}`; ctx.fillStyle = C.muted; ctx.letterSpacing = '0.7px';
      ctx.fillText(fit(txt.toUpperCase(), maxW), x, y); ctx.letterSpacing = '0px';
    }

    function draw() {
      ctx.clearRect(0, 0, W, H);
      ctx.lineCap = 'round'; ctx.lineJoin = 'round'; ctx.textBaseline = 'alphabetic';
      L.boxes.forEach((b, i) => {
        const done = NS > 1 && i === LAST;
        rr(b.x + 0.5, b.y + 0.5, b.w - 1, b.h - 1, 10);
        ctx.fillStyle = done ? C.doneBg : C.box; ctx.fill();
        ctx.strokeStyle = done ? C.doneLine : C.line; ctx.lineWidth = 1; ctx.stroke();
        // Narrow boxes: short stage name and "here now" instead of the long description
        const narrow = b.w < 170, st = STAGES[i];
        const lbl = narrow && st.short && st.short.length < st.label.length ? st.short : st.label;
        ctx.textAlign = 'left'; capLabel(lbl, b.x + 12, b.y + 20, b.w - 24);
        ctx.font = `700 22px ${F}`; ctx.fillStyle = C.navy; ctx.fillText(reached[i], b.x + 12, b.y + 46);
        ctx.font = `500 11px ${F}`; ctx.fillStyle = C.muted;
        ctx.fillText(fit(`${groups[i].length} ${narrow ? 'here now' : st.now}`, b.w - 24), b.x + 12, b.y + 63);
        if (i < LAST) {
          const cx = b.x + b.w + GAP / 2, cy = b.y + b.h / 2;
          ctx.beginPath(); ctx.moveTo(cx - 4, cy - 7); ctx.lineTo(cx + 3, cy); ctx.lineTo(cx - 4, cy + 7);
          ctx.strokeStyle = C.arrow; ctx.lineWidth = 2; ctx.stroke();
        }
      });

      if (HAS_LANE) {
        const ln = L.lane, ex = lanes.reduce((a, l) => a + l.length, 0);
        rr(ln.x + 0.5, ln.y + 0.5, ln.w - 1, ln.h - 1, 10);
        ctx.fillStyle = C.laneBg; ctx.fill(); ctx.strokeStyle = C.laneLine; ctx.lineWidth = 1; ctx.stroke();
        ctx.textAlign = 'left'; capLabel('Discontinued', ln.x + 14, ln.y + 22, L.sub[0].x - 20);
        ctx.font = `700 22px ${F}`; ctx.fillStyle = C.laneInk; ctx.fillText(ex, ln.x + 14, ln.y + 50);
        ctx.font = `500 11px ${F}`; ctx.fillStyle = C.laneSub;
        ctx.fillText(fit(nNow ? `${(ex / nNow * 100).toFixed(1)}% of randomised` : 'none yet', L.sub[0].x - 20), ln.x + 14, ln.y + 68);
        L.sub.forEach((s, i) => {
          ctx.beginPath(); ctx.moveTo(s.x + 0.5, s.y + 12); ctx.lineTo(s.x + 0.5, s.y + s.h - 12);
          ctx.strokeStyle = C.laneLine; ctx.lineWidth = 1; ctx.stroke();
          ctx.beginPath(); ctx.arc(s.x + 16, s.y + 18, 4, 0, 7); ctx.fillStyle = EXITS[i].col; ctx.fill();
          ctx.textAlign = 'left'; ctx.font = `600 11.5px ${F}`; ctx.fillStyle = C.ink;
          wrap2(EXITS[i].label, s.w - 64).forEach((line, j) => ctx.fillText(line, s.x + 26, s.y + 22 + j * 13));
          ctx.textAlign = 'right'; ctx.font = `700 13px ${F}`; ctx.fillStyle = C.laneInk;
          ctx.fillText(lanes[i].length, s.x + s.w - 12, s.y + 22);
        });
      }

      for (const p of P) {
        if (!p.a || p.a < 0.01 || p.px === undefined) continue;
        const dim = hl && p.site !== hl, r = p.r || 4.6;
        const col = colourBy === 'site' ? p.site.col
          : p.stage >= 100 ? EXITS[p.stage - 100].col
          : (NS > 1 && p.stage === LAST) ? C.done : C.dot;
        ctx.globalAlpha = p.a * (dim ? 0.12 : 1);
        ctx.beginPath(); ctx.arc(p.px, p.py, r, 0, 7); ctx.fillStyle = col; ctx.fill();
        if (isFinite(p.pa) && t >= p.pa && p.stage >= 0 && p.stage < 100) {
          ctx.beginPath(); ctx.arc(p.px, p.py, r + 2.2, 0, 7); ctx.strokeStyle = C.part; ctx.lineWidth = 1.6; ctx.stroke();
        }
        if (p.pulse > 0.03 && !dim) {
          ctx.globalAlpha = p.pulse * 0.55;
          ctx.beginPath(); ctx.arc(p.px, p.py, 5 + (1 - p.pulse) * 16, 0, 7); ctx.strokeStyle = col; ctx.lineWidth = 1.5; ctx.stroke();
        }
        if (p === hover) {
          ctx.globalAlpha = 1;
          ctx.beginPath(); ctx.arc(p.px, p.py, r + 3.5, 0, 7); ctx.strokeStyle = C.hover; ctx.lineWidth = 1.5; ctx.stroke();
        }
      }
      ctx.globalAlpha = 1;
    }

    // ── Tooltip + linked highlighting ───────────────────────────────────────
    function tipHtml(p) {
      const row = (k, v) => `<div><span class="tr-k">${k}</span>${v}</div>`;
      let h = `<b>${esc(p.id)}</b>` + row('Site', esc(p.site.name)) + row('Randomised', fmt(p.m[0], 1));
      for (let k = 1; k < NS; k++) {
        if (!(p.m[k] <= t) || (p.x >= 0 && p.xa < p.m[k])) continue;
        const stay = STAGES[k].key === 'dis' && OP_IDX >= 0 && isFinite(p.m[OP_IDX])
          ? ` (${Math.round(p.m[k] - p.m[OP_IDX])}-day stay)` : '';
        h += row(esc(STAGES[k].short), fmt(p.m[k], 1) + stay);
      }
      if (isFinite(p.pa) && t >= p.pa) h += row('Part w/d', fmt(p.pa, 1));
      const st = p.stage >= 100 ? `${esc(EXITS[p.stage - 100].label)}, ${fmt(p.xa, 1)}` : esc(STAGES[p.stage].status);
      return h + row('Status', `<b class="tr-tip-status">${st}</b>`);
    }
    function setHl(s) { hl = s; SITES.forEach(x => x.el.classList.toggle('dim', !!s && x !== s)); }
    cv.addEventListener('mousemove', e => {
      const r = cv.getBoundingClientRect(), mx = e.clientX - r.left, my = e.clientY - r.top;
      let best = null, bd = 81;
      for (const p of P) {
        if (!p.a || p.a < 0.5) continue;
        const d = (p.px - mx) ** 2 + (p.py - my) ** 2;
        if (d < bd) { bd = d; best = p; }
      }
      if (best !== hover) { hover = best; setHl(hover ? hover.site : null); }
      if (!hover) { tip.classList.remove('show'); return; }
      tip.innerHTML = tipHtml(hover); tip.classList.add('show');
      const tw = tip.offsetWidth, th = tip.offsetHeight;
      tip.style.left = Math.max(0, Math.min(mx + 14, W - tw - 4)) + 'px';
      tip.style.top = (my + 14 + th > H ? my - th - 10 : my + 14) + 'px';
    });
    cv.addEventListener('mouseleave', () => { hover = null; tip.classList.remove('show'); setHl(null); });

    // ── Race ────────────────────────────────────────────────────────────────
    const race = $('.tr-race'), moreBtn = $('.tr-race-more'), ROW = 34;
    SITES.forEach(s => {
      const el = document.createElement('div');
      el.className = 'tr-row';
      el.innerHTML = `<span class="tr-rname"><i style="background:${s.col}"></i><span>${esc(s.name)}</span></span><div class="tr-rtrack"><div class="tr-rfill"></div><div class="tr-rexp"></div></div><span class="tr-rval"></span>`;
      el.title = s.name;
      race.appendChild(el);
      Object.assign(s, { el, fill: el.querySelector('.tr-rfill'), exp: el.querySelector('.tr-rexp'), val: el.querySelector('.tr-rval') });
      el.addEventListener('mouseenter', () => setHl(s));
      el.addEventListener('mouseleave', () => setHl(null));
    });
    moreBtn.addEventListener('click', () => { raceAll = !raceAll; renderRace(); });

    function renderRace() {
      SITES.forEach(s => { s.expected = s.openD <= t ? (t - s.openD) / 30.44 * s.rate : 0; });
      const open = SITES.filter(s => s.openD <= t).sort((a, b) => b.n - a.n || a.openD - b.openD);
      const soon = SITES.filter(s => !(s.openD <= t)).sort((a, b) => a.openD - b.openD);
      const brief = [...open.slice(0, RACE_TOP), ...soon.slice(0, 3)];
      const all = [...open, ...soon];
      const shown = raceAll ? all : brief;
      const scale = Math.max(8, ...SITES.map(s => Math.max(s.n, s.expected))) * 1.12;
      SITES.forEach(s => { s.el.hidden = !shown.includes(s); });
      shown.forEach((s, i) => {
        const isOpen = s.openD <= t;
        s.el.style.transform = `translateY(${i * ROW}px)`;
        s.el.classList.toggle('closed', !isOpen);
        s.fill.style.width = (s.n / scale * 100) + '%';
        s.fill.classList.toggle('behind', isOpen && s.n < s.expected * 0.85);
        s.exp.style.left = (s.expected / scale * 100) + '%';
        s.exp.hidden = !isOpen;
        s.val.textContent = isOpen ? s.n : (isFinite(s.openD) ? `opens ${fmt(s.openD)}` : 'not open');
      });
      race.style.height = Math.max(1, shown.length) * ROW - 6 + 'px';
      moreBtn.hidden = all.length <= brief.length;
      moreBtn.textContent = raceAll ? 'Show fewer sites' : `Show all ${all.length} sites`;
    }

    // ── Pace chart ──────────────────────────────────────────────────────────
    function niceStep(max) {
      const raw = max / 4, p = Math.pow(10, Math.floor(Math.log10(raw))), f = raw / p;
      return Math.max(1, (f <= 1 ? 1 : f <= 2 ? 2 : f <= 5 ? 5 : 10) * p);
    }
    function renderPace() {
      const el = $('.tr-pace'), w = el.clientWidth;
      let c = 0;
      for (const p of P) { if (p.m[0] > t) break; c++; }
      const tg = targetAt(t);
      if (w < 60) return tg;
      const h = 210, ml = 34, mr = 18, mt = 14, mb = 24;
      const yRaw = Math.max(10, P.length, TGT ? targetAt(X1) : 0), step = niceStep(yRaw), ymax = Math.ceil(yRaw / step) * step;
      const x = d => ml + d / X1 * (w - ml - mr), y = v => mt + (1 - v / ymax) * (h - mt - mb);
      const every = Math.ceil(MONTHS.length / 8);
      let g = '';
      for (let v = 0; v <= ymax + 1e-9; v += step)
        g += `<line x1="${ml}" x2="${w - mr}" y1="${y(v)}" y2="${y(v)}" class="tr-grid-line"/><text x="${ml - 6}" y="${y(v) + 3.5}" text-anchor="end" class="tr-ax">${Math.round(v)}</text>`;
      MONTHS.forEach(([d, dt], i) => {
        if (i % every) return;
        const last = i === MONTHS.length - 1;
        g += `<text x="${x(d)}" y="${h - 6}" text-anchor="${last ? 'end' : i === 0 ? 'start' : 'middle'}" class="tr-ax">${mlabel(dt)}</text>`;
      });
      if (TGT) {
        const pts = TGT.filter(([d]) => d <= X1).map(([d, v]) => `${x(d).toFixed(1)},${y(v).toFixed(1)}`);
        if (TGT[TGT.length - 1][0] > X1) pts.push(`${x(X1).toFixed(1)},${y(targetAt(X1)).toFixed(1)}`);
        g += `<polyline points="${pts.join(' ')}" class="tr-target-line"/>`;
      }
      let path = `M${x(0)},${y(0)}`, k = 0;
      for (const p of P) { if (p.m[0] > t) break; path += `H${x(p.m[0]).toFixed(1)}V${y(++k).toFixed(1)}`; }
      path += `H${x(t).toFixed(1)}`;
      g += `<path d="${path}V${y(0)}H${x(0)}Z" class="tr-actual-area"/><path d="${path}" class="tr-actual-line"/>`;
      const xt = x(t), right = xt > w - 130;
      g += `<line x1="${xt}" x2="${xt}" y1="${mt}" y2="${h - mb}" class="tr-now-line"/>`;
      if (tg !== null) g += `<circle cx="${xt}" cy="${y(tg)}" r="4" class="tr-target-dot"/>`;
      g += `<circle cx="${xt}" cy="${y(c)}" r="5" class="tr-actual-dot"/>`;
      g += `<text x="${xt + (right ? -10 : 10)}" y="${y(c) + (tg !== null && c < tg ? 16 : -8)}" text-anchor="${right ? 'end' : 'start'}" class="tr-actual-lbl">${c} randomised</text>`;
      g += `<g class="tr-pace-key"><line x1="${ml + 8}" x2="${ml + 26}" y1="${mt + 6}" y2="${mt + 6}" class="tr-actual-line"/><text x="${ml + 31}" y="${mt + 10}">Actual</text>`;
      if (TGT) g += `<line x1="${ml + 80}" x2="${ml + 98}" y1="${mt + 6}" y2="${mt + 6}" class="tr-target-line"/><text x="${ml + 103}" y="${mt + 10}">Target schedule</text>`;
      g += '</g>';
      el.innerHTML = `<svg width="${w}" height="${h}" role="img" aria-label="${c} randomised${tg !== null ? ' against a target of ' + Math.round(tg) : ''} on ${fmt(t, 1)}">${g}</svg>`;
      const dl = $('.tr-delta');
      dl.hidden = tg === null;
      if (tg !== null) {
        const diff = c - Math.round(tg);
        dl.className = 'tr-delta ' + (diff < 0 ? 'behind' : 'ahead');
        dl.textContent = diff < 0 ? `${-diff} behind target` : diff === 0 ? 'On target' : `${diff} ahead of target`;
      }
      return tg;
    }

    // ── KPIs ────────────────────────────────────────────────────────────────
    const kpis = $('.tr-kpis');
    function renderKpis(tg) {
      const openN = SITES.filter(s => s.openD <= t).length;
      const active = groups.slice(0, NS > 1 ? LAST : NS).reduce((a, g) => a + g.length, 0);
      const ex = lanes.reduce((a, g) => a + g.length, 0);
      const kpi = (l, v, s, bar) => `<div class="tr-kpi"><div class="tr-kpi-l">${l}</div><div class="tr-kpi-v">${v}</div>` +
        (bar !== null ? `<div class="tr-kpi-bar"><i style="width:${Math.min(100, bar)}%"></i></div>` : '') +
        `<div class="tr-kpi-s">${s}</div></div>`;
      let sub;
      if (tg !== null) {
        const diff = nNow - Math.round(tg);
        sub = `Target by now ${Math.round(tg)} · <span class="${diff < 0 ? 'tr-down' : 'tr-up'}">${diff < 0 ? '▼ ' + (-diff) + ' behind' : diff === 0 ? 'on target' : '▲ ' + diff + ' ahead'}</span>`;
      } else sub = TRIAL_TARGET ? `${(nNow / TRIAL_TARGET * 100).toFixed(1)}% of target` : 'participants';
      let h = kpi('Randomised', TRIAL_TARGET ? `${nNow} <small>/ ${TRIAL_TARGET}</small>` : nNow, sub,
                  TRIAL_TARGET ? nNow / TRIAL_TARGET * 100 : null);
      if (PILOT) h += kpi('Internal pilot', `${Math.min(nNow, +PILOT.target)} <small>/ ${+PILOT.target}</small>`,
                          PILOT.sites ? `${Math.min(openN, +PILOT.sites)} of ${+PILOT.sites} pilot sites open` : 'pilot target',
                          nNow / PILOT.target * 100);
      h += kpi('Sites open', openN, D.planned_sites ? `of ${+D.planned_sites} planned sites` : 'open to recruitment', null);
      if (NS > 1) {
        const due = P.filter(p => p.m[LAST] <= t).length;
        h += kpi('In active follow-up', active, 'still moving through the pathway', null);
        h += kpi(`Reached ${esc(STAGES[LAST].short)}`, reached[LAST],
                 due ? `${Math.round(reached[LAST] / due * 100)}% of the ${due} due` : 'none due yet', null);
      }
      if (HAS_LANE) h += kpi('Discontinued', ex, nNow ? `${(ex / nNow * 100).toFixed(1)}% of randomised` : 'none yet', null);
      kpis.innerHTML = h;
    }

    // ── Ticker ──────────────────────────────────────────────────────────────
    const ticker = $('.tr-ticker');
    function renderTicker() {
      let k = 0;
      while (k < EV.length && EV[k].t <= t) k++;
      if (k === lastEv) return;
      const forward = k > lastEv && lastEv !== -1;
      lastEv = k;
      const items = EV.slice(Math.max(0, k - 7), k).reverse();
      ticker.innerHTML = items.length
        ? items.map((e, i) => `<li class="${i === 0 && forward ? 'new' : ''}"><span class="tr-tdot" style="background:${e.col}"></span><span class="tr-tdate">${fmt(e.t)}</span><span>${e.txt}</span></li>`).join('')
        : '<li class="tr-tempty">Nothing yet. Press play.</li>';
    }

    // ── Timeline controls ───────────────────────────────────────────────────
    const range = $('.tr-range'), playBtn = $('.tr-play');
    range.max = END;
    const bin = Math.max(7, Math.ceil(END / 60));
    const counts = Array(Math.floor(END / bin) + 1).fill(0);
    P.forEach(p => { const i = Math.floor(p.m[0] / bin); if (i >= 0 && i < counts.length) counts[i]++; });
    const cmax = Math.max(...counts, 1);
    $('.tr-spark').innerHTML = counts.map(c => `<i style="height:${Math.max(6, c / cmax * 100)}%"></i>`).join('');
    const sparkEls = [...$('.tr-spark').children];
    const mEvery = Math.ceil(MONTHS.filter(([d]) => d <= END).length / 9);
    $('.tr-mlabels').innerHTML = MONTHS.filter(([d]) => d <= END).filter((_, i) => i % mEvery === 0)
      .map(([d, dt]) => `<span style="left:${d / END * 100}%">${mlabel(dt)}</span>`).join('');

    function setPlaying(v) {
      if (v && t >= END - 0.01) { t = 0; lastEv = -1; }
      playing = v;
      playBtn.innerHTML = v ? ICON_PAUSE : ICON_PLAY;
      playBtn.setAttribute('aria-label', v ? 'Pause' : 'Play');
    }
    playBtn.addEventListener('click', () => setPlaying(!playing));
    range.addEventListener('input', () => { t = +range.value; setPlaying(false); });
    $('.tr-today').addEventListener('click', () => { t = END; setPlaying(false); });
    root.querySelectorAll('.tr-speed button').forEach(b => b.addEventListener('click', () => {
      speed = +b.dataset.s;
      root.querySelectorAll('.tr-speed button').forEach(x => x.classList.toggle('on', x === b));
    }));
    root.querySelectorAll('.tr-colour button').forEach(b => b.addEventListener('click', () => {
      colourBy = b.dataset.c;
      root.querySelectorAll('.tr-colour button').forEach(x => x.classList.toggle('on', x === b));
    }));
    $('.tr-legend-done span').textContent = NS > 1 ? `Reached ${STAGES[LAST].short}` : '';
    $('.tr-legend-done').hidden = NS <= 1;
    $('.tr-legend-part').hidden = !HAS_PART;
    $('.tr-foot').textContent = D.note || '';

    // ── DOM refresh (twice per replay day) ──────────────────────────────────
    function updateDom() {
      const dt = new Date(START + t * DAY);
      const month = (dt.getUTCFullYear() - d0.getUTCFullYear()) * 12 + dt.getUTCMonth() - d0.getUTCMonth() + 1;
      $('.tr-now-date').textContent = fmt(t, 1);
      $('.tr-now-sub').textContent = `Month ${month} of the replay${t >= END - 0.01 ? ' · today' : ''}`;
      range.value = t;
      sparkEls.forEach((el, i) => el.classList.toggle('on', i * bin <= t));
      renderKpis(renderPace());
      renderRace();
      renderTicker();
      cv.setAttribute('aria-label', `Participant flow on ${fmt(t, 1)}: ` +
        STAGES.map((s, i) => `${s.label} ${reached[i]}`).join(', ') +
        (HAS_LANE ? `, discontinued ${lanes.reduce((a, l) => a + l.length, 0)}` : ''));
      if (hover && hover.a > 0.5) tip.innerHTML = tipHtml(hover);
    }

    // ── Main loop (pauses while the Overview tab is hidden) ─────────────────
    const ro = new ResizeObserver(() => { layout(); lastDay = -1; });
    ro.observe(pipe); ro.observe($('.tr-pace'));
    function frame(now) {
      if (!alive) return;
      if (!root.isConnected) { alive = false; ro.disconnect(); return; }
      const dt = Math.min(0.25, (now - last) / 1000); last = now;
      if (root.offsetParent !== null) {
        if (!L) layout();
        if (L) {
          if (playing) { t += dt * speed; if (t >= END) { t = END; setPlaying(false); } }
          if (frameN++ % 30 === 0) readColours();
          update(dt);
          const day = Math.floor(t * 2);
          if (day !== lastDay) { lastDay = day; updateDom(); }
          draw();
        }
      }
      requestAnimationFrame(frame);
    }
    readColours();
    setPlaying(false);
    requestAnimationFrame(frame);
  }

  function mount(root) {
    if (root.dataset.mounted) return;
    const src = document.getElementById(root.id + '-data');
    if (!src) return;
    let data;
    try { data = JSON.parse(src.textContent); }
    catch (e) { console.error('Trial replay: could not read data', e); return; }
    root.dataset.mounted = '1';
    try { Replay(root, data); }
    catch (e) { console.error('Trial replay failed', e); root.textContent = 'The trial replay could not be drawn for this data.'; }
  }
  function scan() { document.querySelectorAll('.tr-root:not([data-mounted])').forEach(mount); }

  new MutationObserver(scan).observe(document.documentElement, { childList: true, subtree: true });
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', scan);
  else scan();
  window.TrialReplay = { mount, scan };
})();

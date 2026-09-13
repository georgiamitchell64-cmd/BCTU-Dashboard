// ─────────────────────────────────────────────────────────────────────────────
// Guided tours and help pop-ups.
//
// Tours: step-by-step walkthroughs written in functions/tutorial.R and
// embedded in the page as JSON (#bctu-tours). The server starts one with
// session$sendCustomMessage('tour_start', {tour: 'home'}) the first time
// someone signs in (and 'trial' the first time they open a trial); anyone
// can replay one from the account menu (bctuTour.replay()). Finishing or
// skipping reports back as input$tour_done so it isn't shown again.
// Each step: {target, title, body, go, tab, requires, eyebrow, next, skip}.
//
// Help pop-ups: a "?" after the section headings listed in #bctu-help-tips,
// added as the page renders.
// Styles: www/tour.css.
// ─────────────────────────────────────────────────────────────────────────────
(function () {
  'use strict';

  var reduced = window.matchMedia && window.matchMedia('(prefers-reduced-motion: reduce)').matches;

  function readJson(id) {
    var el = document.getElementById(id);
    if (!el) return null;
    try { return JSON.parse(el.textContent); } catch (e) { return null; }
  }

  // A Shiny output wrapper has no box of its own (display: contents), so
  // measure it by what it holds
  function rectOf(el) {
    var r = el.getBoundingClientRect();
    if (r.width > 0 && r.height > 0) return r;
    var L = Infinity, T = Infinity, R = -Infinity, B = -Infinity, any = false;
    for (var i = 0; i < el.children.length; i++) {
      var k = el.children[i].getBoundingClientRect();
      if (k.width > 0 && k.height > 0) {
        any = true;
        L = Math.min(L, k.left); T = Math.min(T, k.top); R = Math.max(R, k.right); B = Math.max(B, k.bottom);
      }
    }
    return any ? { left: L, top: T, right: R, bottom: B, width: R - L, height: B - T } : r;
  }

  function visible(el) {
    if (!el) return false;
    var r = rectOf(el);
    return r.width > 0 && r.height > 0 && getComputedStyle(el).visibility !== 'hidden';
  }

  // First visible match; a step's target can list fallbacks
  function find(sel) {
    if (!sel) return null;
    var list = Array.isArray(sel) ? sel : [sel];
    for (var i = 0; i < list.length; i++) {
      var nodes = document.querySelectorAll(list[i]);
      for (var j = 0; j < nodes.length; j++) if (visible(nodes[j])) return nodes[j];
    }
    return null;
  }

  function waitFor(sel, ms) {
    return new Promise(function (resolve) {
      if (!sel) { resolve(null); return; }
      var t0 = Date.now();
      (function poll() {
        var el = find(sel);
        if (el || Date.now() - t0 > ms) resolve(el); else setTimeout(poll, 120);
      })();
    });
  }

  // ── Tours ──────────────────────────────────────────────────────────────────
  var T = null;   // the running tour: {name, def, steps, i, el, token, returnFocus}
  var ui = null;

  function buildUi() {
    var block = document.createElement('div');
    block.className = 'tour-block';
    var spot = document.createElement('div');
    spot.className = 'tour-spot off';
    var card = document.createElement('div');
    card.className = 'tour-card';
    card.setAttribute('role', 'dialog');
    card.setAttribute('aria-modal', 'true');
    card.setAttribute('aria-labelledby', 'tour-title');
    card.setAttribute('aria-describedby', 'tour-body');
    card.innerHTML =
      '<div class="tour-top"><span class="tour-eyebrow" id="tour-count"></span>' +
      '<button type="button" class="tour-x" aria-label="Close the tour">&times;</button></div>' +
      '<div class="tour-bar" aria-hidden="true"><i></i></div>' +
      '<h3 class="tour-title" id="tour-title"></h3>' +
      '<div class="tour-body" id="tour-body" aria-live="polite"></div>' +
      '<div class="tour-foot">' +
      '<button type="button" class="tour-skip"></button><span class="tour-sp"></span>' +
      '<button type="button" class="tour-back">Back</button>' +
      '<button type="button" class="tour-next"></button></div>';
    document.body.appendChild(block);
    document.body.appendChild(spot);
    document.body.appendChild(card);
    card.querySelector('.tour-x').addEventListener('click', function () { finish(false); });
    card.querySelector('.tour-skip').addEventListener('click', function () { finish(false); });
    card.querySelector('.tour-back').addEventListener('click', function () { if (T) go(T.i - 1); });
    card.querySelector('.tour-next').addEventListener('click', function () { if (T) go(T.i + 1); });
    // The page underneath can't be clicked while the tour is open
    block.addEventListener('click', function (e) { e.preventDefault(); e.stopPropagation(); });
    return { block: block, spot: spot, card: card };
  }

  function start(name) {
    if (T) return;                                  // one tour at a time
    var def = (readJson('bctu-tours') || {})[name];
    if (!def || !def.steps || !def.steps.length) return;
    // Never over the sign-in screen: wait until it has gone
    var w = document.getElementById('welcome_screen');
    if (w && visible(w)) { setTimeout(function () { start(name); }, 500); return; }
    var steps = def.steps.filter(function (s) { return !s.requires || find(s.requires); });
    T = { name: name, def: def, steps: steps, i: 0, el: null, token: 0,
          returnFocus: document.activeElement };
    ui = buildUi();
    document.body.classList.add('tour-on');
    document.addEventListener('keydown', onKey, true);
    window.addEventListener('resize', onMove);
    window.addEventListener('scroll', onMove, true);
    go(0);
  }

  function go(i) {
    if (!T) return;
    if (i < 0) i = 0;
    if (i >= T.steps.length) { finish(true); return; }
    var s = T.steps[i], token = ++T.token;
    T.i = i;
    if (s.go && window.Shiny) {
      Shiny.setInputValue(s.go, Math.random(), { priority: 'event' });
      if (s.tab && typeof window.setActiveTab === 'function') window.setActiveTab(s.tab);
    }
    ui.card.classList.add('busy');
    waitFor(s.target, s.go ? 5000 : 2000).then(function (el) {
      if (!T || token !== T.token) return;
      ui.card.classList.remove('busy');
      render(s, el);
    });
  }

  function render(s, el) {
    var n = T.steps.length, i = T.i, c = ui.card;
    c.querySelector('#tour-count').textContent = s.eyebrow || ('Step ' + (i + 1) + ' of ' + n);
    c.querySelector('.tour-bar i').style.width = (100 * (i + 1) / n) + '%';
    c.querySelector('#tour-title').textContent = s.title || '';
    c.querySelector('#tour-body').innerHTML = s.body || '';   // written in functions/tutorial.R
    var back = c.querySelector('.tour-back'), next = c.querySelector('.tour-next'),
        skip = c.querySelector('.tour-skip');
    back.hidden = i === 0;
    next.textContent = s.next || (i === n - 1 ? 'Finish' : 'Next');
    skip.textContent = s.skip || 'Skip tour';
    skip.hidden = i === n - 1;
    c.classList.toggle('tour-center', !el);
    T.el = el;
    if (el) {
      var box = el.getBoundingClientRect().width ? el : (el.firstElementChild || el);
      box.scrollIntoView({ block: 'center', inline: 'nearest', behavior: reduced ? 'auto' : 'smooth' });
      setTimeout(place, reduced ? 0 : 380);
    }
    place();
    next.focus({ preventScroll: true });
  }

  // Spotlight the target and put the card beside it: below, above, right, left
  function place() {
    if (!T || !ui) return;
    var c = ui.card, el = T.el, pad = 6, gap = 14;
    var vw = window.innerWidth, vh = window.innerHeight;
    if (!el || !visible(el)) {                      // no target: centred by CSS
      ui.spot.classList.add('off');
      c.classList.add('is-centred');
      c.style.left = ''; c.style.top = '';
      return;
    }
    ui.spot.classList.remove('off');
    c.classList.remove('is-centred');
    var cw = c.offsetWidth, ch = c.offsetHeight;
    var r = rectOf(el);
    var x = Math.max(4, r.left - pad), y = Math.max(4, r.top - pad);
    var w = Math.min(vw - 4, r.right + pad) - x, h = Math.min(vh - 4, r.bottom + pad) - y;
    ui.spot.style.left = x + 'px'; ui.spot.style.top = y + 'px';
    ui.spot.style.width = Math.max(0, w) + 'px'; ui.spot.style.height = Math.max(0, h) + 'px';
    var left, top;
    if (vh - (y + h) >= ch + gap)  { top = y + h + gap; left = x + w / 2 - cw / 2; }
    else if (y >= ch + gap)        { top = y - gap - ch; left = x + w / 2 - cw / 2; }
    else if (vw - (x + w) >= cw + gap) { left = x + w + gap; top = y + h / 2 - ch / 2; }
    else if (x >= cw + gap)        { left = x - gap - cw; top = y + h / 2 - ch / 2; }
    else                           { left = vw - cw - 16; top = vh - ch - 16; }
    c.style.left = Math.min(vw - cw - 12, Math.max(12, left)) + 'px';
    c.style.top = Math.min(vh - ch - 12, Math.max(12, top)) + 'px';
  }

  var moveQueued = false;
  function onMove() {
    if (moveQueued) return;
    moveQueued = true;
    requestAnimationFrame(function () { moveQueued = false; place(); });
  }

  function onKey(e) {
    if (!T || !ui) return;
    if (e.key === 'Escape') { e.preventDefault(); finish(false); return; }
    var inCard = ui.card.contains(document.activeElement);
    if (e.key === 'ArrowRight' && !e.altKey) { e.preventDefault(); go(T.i + 1); return; }
    if (e.key === 'ArrowLeft' && !e.altKey && T.i > 0) { e.preventDefault(); go(T.i - 1); return; }
    if (e.key === 'Tab') {                          // keep focus inside the card
      var f = Array.prototype.filter.call(ui.card.querySelectorAll('button'), function (b) { return !b.hidden; });
      if (!f.length) return;
      var k = f.indexOf(document.activeElement);
      if (!inCard || (e.shiftKey && k <= 0) || (!e.shiftKey && k === f.length - 1)) {
        e.preventDefault();
        f[e.shiftKey ? f.length - 1 : 0].focus();
      }
    }
  }

  function finish(completed) {
    if (!T) return;
    var name = T.name, def = T.def, back = T.returnFocus;
    T = null;
    document.body.classList.remove('tour-on');
    document.removeEventListener('keydown', onKey, true);
    window.removeEventListener('resize', onMove);
    window.removeEventListener('scroll', onMove, true);
    if (ui) { ui.block.remove(); ui.spot.remove(); ui.card.remove(); ui = null; }
    if (window.Shiny) {
      // Leave the person somewhere sensible (the trial tour ends on Overview)
      if (def.end_go) {
        Shiny.setInputValue(def.end_go, Math.random(), { priority: 'event' });
        if (def.end_tab && typeof window.setActiveTab === 'function') window.setActiveTab(def.end_tab);
      }
      Shiny.setInputValue('tour_done', { tour: name, completed: !!completed, n: Math.random() },
                          { priority: 'event' });
    }
    if (back && back.focus && document.contains(back)) back.focus({ preventScroll: true });
  }

  function registerHandler() {
    if (!window.Shiny || !Shiny.addCustomMessageHandler) { setTimeout(registerHandler, 200); return; }
    Shiny.addCustomMessageHandler('tour_start', function (m) { start(m && m.tour); });
  }
  registerHandler();

  window.bctuTour = {
    start: start,
    // The tour for wherever the person is: home screen or inside a trial
    replay: function () {
      document.querySelectorAll('.topbar-account.open, .userchip.open').forEach(function (m) { m.classList.remove('open'); });
      if (window.jQuery) jQuery('.modal').modal('hide');
      setTimeout(function () {
        start(document.body.classList.contains('home-mode') ? 'home' : 'trial');
      }, 250);
    }
  };

  // ── Help pop-ups ───────────────────────────────────────────────────────────
  var HEADS = '.th-section-head h3, .data-panel-title, .data-safety-title, .pov-card-head h3, ' +
              '.pov-card-head h2, .ov-card-head h2, .ov-card-head h3, .rt-title, .sec-head2 h2';
  var tips = null, floatEl = null, pinned = null;

  function ownText(el) {
    var t = '';
    for (var i = 0; i < el.childNodes.length; i++)
      if (el.childNodes[i].nodeType === 3) t += el.childNodes[i].textContent;
    t = t.replace(/\s+/g, ' ').trim();
    return t || el.textContent.replace(/\s+/g, ' ').trim();
  }

  function showTip(btn) {
    if (!floatEl) {
      floatEl = document.createElement('div');
      floatEl.id = 'htip2-float';
      floatEl.setAttribute('role', 'tooltip');
      document.body.appendChild(floatEl);
    }
    floatEl.textContent = btn.getAttribute('data-tip');
    floatEl.hidden = false;
    var r = btn.getBoundingClientRect(), fw = floatEl.offsetWidth, fh = floatEl.offsetHeight;
    var left = Math.min(window.innerWidth - fw - 12, Math.max(12, r.left + r.width / 2 - fw / 2));
    var top = r.bottom + 8;
    if (top + fh > window.innerHeight - 12) top = r.top - fh - 8;
    floatEl.style.left = left + 'px';
    floatEl.style.top = Math.max(12, top) + 'px';
    btn.setAttribute('aria-describedby', 'htip2-float');
  }
  function hideTip(btn) {
    if (btn && pinned === btn) return;
    if (floatEl) floatEl.hidden = true;
    if (btn) btn.removeAttribute('aria-describedby');
  }

  function tipButton(t) {
    var b = document.createElement('button');
    b.type = 'button';
    b.className = 'htip2';
    b.textContent = '?';
    b.setAttribute('aria-label', 'About ' + t.heading);
    b.setAttribute('data-tip', t.text);
    b.addEventListener('mouseenter', function () { showTip(b); });
    b.addEventListener('mouseleave', function () { hideTip(b); });
    b.addEventListener('focus', function () { showTip(b); });
    b.addEventListener('blur', function () { if (pinned === b) pinned = null; hideTip(b); });
    b.addEventListener('click', function (e) {
      e.preventDefault(); e.stopPropagation();
      if (pinned === b) { pinned = null; hideTip(b); } else { pinned = b; showTip(b); }
    });
    return b;
  }

  function addTips() {
    if (!tips) tips = readJson('bctu-help-tips') || [];
    if (!tips.length) return;
    var heads = document.querySelectorAll(HEADS);
    for (var i = 0; i < heads.length; i++) {
      var h = heads[i];
      if (h.hasAttribute('data-htip')) continue;
      var text = ownText(h).toLowerCase();
      for (var j = 0; j < tips.length; j++) {
        var t = tips[j];
        if (t.heading.toLowerCase() !== text) continue;
        if (t.scope && !h.closest(t.scope)) continue;
        h.setAttribute('data-htip', '1');
        h.appendChild(tipButton(t));
        break;
      }
    }
  }

  function closeTip() { pinned = null; if (floatEl) floatEl.hidden = true; }
  document.addEventListener('click', function (e) { if (!e.target.closest('.htip2')) closeTip(); });
  window.addEventListener('scroll', function () { if (floatEl && !floatEl.hidden) closeTip(); }, true);

  var queued = null;
  function scheduleTips() {
    if (queued) return;
    queued = setTimeout(function () {
      queued = null;
      addTips();
      // A heading that redraws while its pop-up is open takes the button with it
      if (floatEl && !floatEl.hidden) {
        var b = document.querySelector('.htip2[aria-describedby="htip2-float"]');
        if (!b || !visible(b)) closeTip();
      }
    }, 300);
  }
  function initTips() {
    addTips();
    new MutationObserver(scheduleTips).observe(document.body, { childList: true, subtree: true });
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', initTips);
  else initTips();
})();

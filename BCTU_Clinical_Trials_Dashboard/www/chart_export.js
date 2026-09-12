// ─────────────────────────────────────────────────────────────────────────────
// Chart export — an "Image" button on every card that holds a chart, saving
// the card as a PNG or JPEG. Covers the echarts charts (canvas) and the
// dashboard's own SVG/HTML charts: the card is copied with its styles inlined,
// canvases and images are swapped for data URLs, the Manrope font is embedded,
// and the copy is drawn at 2x through an SVG foreignObject. Maps are skipped —
// their tiles come from another site and can't be drawn into an image.
// Styles: www/data_kpis.css (.ce-*).
// ─────────────────────────────────────────────────────────────────────────────
(function () {
  'use strict';

  // Card types that can hold a chart: the card, its header, and its title
  var CARDS = [
    { card: '.th-section', head: '.th-section-head', title: 'h3' },
    { card: '.pov-card',   head: '.pov-card-head',   title: 'h3' },
    { card: '.data-panel', head: '.data-panel-head', title: '.data-panel-title' },
    { card: '.pf-panel',   head: '.pf-panel-head',   title: '.pf-panel-title' },
    { card: '.ov-card',    head: '.ov-card-head',    title: 'h3' }
  ];
  var SCALE = 2;
  // Charts drawn in plain HTML (bars made of divs) — out of hours by site,
  // monthly recruitment per site, change of status, demographics, portfolio
  // recruitment bars — plus anything marked data-chart.
  var HTML_CHARTS = '.oh-rows, .sm-rows, .cs-grid, .dm-grid, .dm-kpis, .pf-rec-list, .pf-cat-bar, [data-chart]';

  // An SVG counts as a chart when it is drawn wider than an icon
  function isChartSvg(svg) {
    if (svg.closest('button, .ce-wrap')) return false;
    var vb = svg.viewBox && svg.viewBox.baseVal;
    var w = (vb && vb.width) || parseFloat(svg.getAttribute('width')) || 0;
    return w >= 60;
  }
  function hasChart(card) {
    if (card.querySelector('.leaflet-container')) return false;
    if (card.querySelector('canvas') || card.querySelector(HTML_CHARTS)) return true;
    return Array.prototype.some.call(card.querySelectorAll('svg'), isChartSvg);
  }

  function slug(s) {
    return (s || '').toLowerCase().replace(/[^a-z0-9]+/g, '-').replace(/^-+|-+$/g, '').slice(0, 60) || 'chart';
  }
  function today() {
    var d = new Date();
    return d.getFullYear() + '-' + ('0' + (d.getMonth() + 1)).slice(-2) + '-' + ('0' + d.getDate()).slice(-2);
  }
  function notify(msg) {
    if (window.Shiny && Shiny.notifications) Shiny.notifications.show({ html: msg, type: 'error', duration: 6000 });
    else window.alert(msg);
  }
  function blobToDataUrl(blob) {
    return new Promise(function (resolve, reject) {
      var r = new FileReader();
      r.onload = function () { resolve(r.result); };
      r.onerror = reject;
      r.readAsDataURL(blob);
    });
  }

  // The page's Google font, with its latin files embedded, fetched once. An
  // image drawn from an SVG can't load fonts itself; without this it falls
  // back to the system font.
  var fontCssPromise = null;
  function fontCss() {
    if (fontCssPromise) return fontCssPromise;
    var link = document.querySelector('link[href*="fonts.googleapis.com/css"]');
    if (!link || !window.fetch) return (fontCssPromise = Promise.resolve(''));
    fontCssPromise = fetch(link.href)
      .then(function (r) { return r.text(); })
      .then(function (css) {
        var faces = css.split('@font-face').slice(1).map(function (b) { return '@font-face' + b; });
        var latin = faces.filter(function (b) { return /U\+0000-00FF/.test(b); });
        return Promise.all((latin.length ? latin : faces).map(function (b) {
          var m = b.match(/url\((['"]?)([^'")]+)\1\)/);
          if (!m) return b;
          return fetch(m[2]).then(function (r) { return r.blob(); }).then(blobToDataUrl)
            .then(function (du) { return b.replace(m[0], 'url(' + du + ')'); });
        }));
      })
      .then(function (parts) { return parts.join('\n'); })
      .catch(function () { return ''; });
    return fontCssPromise;
  }

  // Copy every computed style onto the clone, so it renders without the
  // page's stylesheets.
  function inlineStyles(src, dst) {
    var cs = window.getComputedStyle(src), out = [];
    for (var i = 0; i < cs.length; i++) out.push(cs[i] + ':' + cs.getPropertyValue(cs[i]));
    dst.setAttribute('style', out.join(';'));
    var s = src.children, d = dst.children;
    for (var j = 0; j < s.length; j++) if (d[j]) inlineStyles(s[j], d[j]);
  }

  // <img> tags become data URLs (cross-site ones are dropped); then each
  // <canvas> becomes an <img> of itself. Images first, so indexes line up.
  function embedPictures(card, clone) {
    var oi = card.querySelectorAll('img'), ci = clone.querySelectorAll('img');
    for (var i = 0; i < oi.length; i++) {
      if (!ci[i] || /^data:/.test(oi[i].src) || !oi[i].naturalWidth) continue;
      try {
        var c = document.createElement('canvas');
        c.width = oi[i].naturalWidth; c.height = oi[i].naturalHeight;
        c.getContext('2d').drawImage(oi[i], 0, 0);
        ci[i].setAttribute('src', c.toDataURL('image/png'));
      } catch (e) { ci[i].removeAttribute('src'); }
    }
    var oc = card.querySelectorAll('canvas'), cc = clone.querySelectorAll('canvas');
    for (var k = 0; k < oc.length; k++) {
      if (!cc[k]) continue;
      var img = document.createElement('img');
      img.setAttribute('src', oc[k].toDataURL('image/png'));   // throws if the canvas is cross-site
      img.setAttribute('style', cc[k].getAttribute('style') || '');
      cc[k].parentNode.replaceChild(img, cc[k]);
    }
  }

  function render(card, type) {
    var rect = card.getBoundingClientRect();
    var w = Math.ceil(rect.width), h = Math.ceil(rect.height);
    if (!w || !h) return Promise.reject(new Error('the chart is not on screen'));
    var clone = card.cloneNode(true);
    try {
      inlineStyles(card, clone);
      Array.prototype.forEach.call(clone.querySelectorAll('.ce-wrap'), function (n) { n.remove(); });
      embedPictures(card, clone);
    } catch (e) { return Promise.reject(e); }
    clone.style.margin = '0';
    clone.style.width = w + 'px';
    clone.style.height = h + 'px';
    var bg = window.getComputedStyle(card).backgroundColor;
    if (!bg || bg === 'transparent' || /rgba\(.*,\s*0\)$/.test(bg)) bg = '#ffffff';

    return fontCss().then(function (fonts) {
      var svg = '<svg xmlns="http://www.w3.org/2000/svg" width="' + w + '" height="' + h + '">' +
                '<style>' + fonts + '</style>' +
                '<foreignObject x="0" y="0" width="' + w + '" height="' + h + '">' +
                new XMLSerializer().serializeToString(clone) + '</foreignObject></svg>';
      return new Promise(function (resolve, reject) {
        var img = new Image();
        img.onload = function () {
          try {
            var c = document.createElement('canvas');
            c.width = w * SCALE; c.height = h * SCALE;
            var ctx = c.getContext('2d');
            ctx.fillStyle = bg;
            ctx.fillRect(0, 0, c.width, c.height);
            ctx.scale(SCALE, SCALE);
            ctx.drawImage(img, 0, 0, w, h);
            c.toBlob(function (b) { if (b) resolve(b); else reject(new Error('the image came out empty')); },
                     type === 'jpeg' ? 'image/jpeg' : 'image/png', 0.95);
          } catch (e) { reject(e); }
        };
        img.onerror = function () { reject(new Error('the chart could not be drawn')); };
        img.src = 'data:image/svg+xml;charset=utf-8,' + encodeURIComponent(svg);
      });
    });
  }

  function save(blob, name) {
    var url = URL.createObjectURL(blob), a = document.createElement('a');
    a.href = url; a.download = name;
    document.body.appendChild(a); a.click();
    setTimeout(function () { URL.revokeObjectURL(url); a.remove(); }, 1500);
  }

  function closeMenus() {
    Array.prototype.forEach.call(document.querySelectorAll('.ce-menu'), function (m) { m.hidden = true; });
    Array.prototype.forEach.call(document.querySelectorAll('.ce-btn'), function (b) { b.setAttribute('aria-expanded', 'false'); });
  }

  function addButton(card, head, titleSel) {
    var wrap = document.createElement('div');
    wrap.className = 'ce-wrap';
    wrap.innerHTML =
      '<button type="button" class="ce-btn" aria-haspopup="true" aria-expanded="false" title="Download this chart as an image">' +
        '<svg viewBox="0 0 16 16" width="13" height="13" aria-hidden="true"><path d="M8 2v8m0 0L5 7m3 3 3-3M3 13h10" fill="none" stroke="currentColor" stroke-width="1.6" stroke-linecap="round" stroke-linejoin="round"/></svg>' +
        '<span>Image</span></button>' +
      '<div class="ce-menu" role="menu" hidden>' +
        '<button type="button" role="menuitem" data-type="png">PNG <small>sharp, for slides</small></button>' +
        '<button type="button" role="menuitem" data-type="jpeg">JPEG <small>smaller file</small></button>' +
      '</div>';
    if (/flex/.test(window.getComputedStyle(head).display)) {
      head.appendChild(wrap);
    } else {
      wrap.classList.add('ce-abs');
      if (window.getComputedStyle(card).position === 'static') card.style.position = 'relative';
      card.appendChild(wrap);
    }
    var btn = wrap.querySelector('.ce-btn'), menu = wrap.querySelector('.ce-menu'), label = btn.querySelector('span');
    btn.addEventListener('click', function (e) {
      e.stopPropagation();
      var opening = menu.hidden;
      closeMenus();
      menu.hidden = !opening;
      btn.setAttribute('aria-expanded', String(opening));
    });
    menu.addEventListener('click', function (e) {
      var item = e.target.closest('[data-type]');
      if (!item) return;
      e.stopPropagation();
      closeMenus();
      var type = item.getAttribute('data-type');
      var t = card.querySelector(titleSel);
      var name = slug(t ? t.textContent : '') + '_' + today() + (type === 'jpeg' ? '.jpg' : '.png');
      btn.disabled = true; label.textContent = 'Saving…';
      render(card, type)
        .then(function (b) { save(b, name); })
        .catch(function (err) { notify('This chart couldn’t be saved as an image (' + ((err && err.message) || 'unknown error') + ').'); })
        .then(function () { btn.disabled = false; label.textContent = 'Image'; });
    });
  }

  function scan() {
    CARDS.forEach(function (c) {
      Array.prototype.forEach.call(document.querySelectorAll(c.card), function (card) {
        if (card.getAttribute('data-ce') || !hasChart(card)) return;
        var head = card.querySelector(c.head);
        if (!head) return;
        card.setAttribute('data-ce', '1');
        addButton(card, head, c.title);
      });
    });
  }

  var timer = null;
  function schedule() { clearTimeout(timer); timer = setTimeout(scan, 300); }
  function start() {
    new MutationObserver(schedule).observe(document.body, { childList: true, subtree: true });
    document.addEventListener('click', closeMenus);
    document.addEventListener('keydown', function (e) { if (e.key === 'Escape') closeMenus(); });
    scan();
  }
  if (document.readyState === 'loading') document.addEventListener('DOMContentLoaded', start);
  else start();
})();

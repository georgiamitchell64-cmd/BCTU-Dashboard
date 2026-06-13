reports_tab_ui <- function() {
  tabPanel("reports",

    # ── Design-matched styles (scoped to .rb-app) ──────────────────────────
    tags$style(HTML("
      .rb-app {
        --rb-navy:#1B4F6B; --rb-navy-dk:#143D54; --rb-navy-lt:#2A6584;
        --rb-teal:#2EC4A5; --rb-teal-dk:#0FA88E;
        --rb-amber:#F59E0B; --rb-red:#EF4444;
        --rb-ink:#0F1A24; --rb-ink-2:#27384A;
        --rb-muted:#64748B; --rb-muted-2:#94A3B8;
        --rb-line:#E2E8EE; --rb-line-2:#EEF2F6;
        --rb-bg:#F4F6F9; --rb-rail:#F8FAFC;
        --rb-doc:'Source Serif 4', Charter, Georgia, serif;
        --rb-mono:'JetBrains Mono', ui-monospace, Menlo, monospace;
        font-size:13px; color:var(--rb-ink);
        background:var(--rb-bg); margin:-20px -22px;
        height:calc(100vh - 96px); overflow:hidden;
      }
      .rb-work {
        display:grid; grid-template-columns:300px 1fr 360px;
        height:100%;
      }
      @media (max-width:1280px){
        .rb-work { grid-template-columns:260px 1fr 320px; }
      }

      /* ── Left rail ── */
      .rb-rail {
        background:var(--rb-rail); border-right:1px solid var(--rb-line);
        padding:14px 14px 24px; overflow-y:auto; height:100%;
      }
      .rb-rail h4 {
        font-size:10.5px; font-weight:600; color:var(--rb-muted);
        text-transform:uppercase; letter-spacing:.7px;
        margin:14px 4px 8px;
      }
      .rb-rail h4:first-of-type { margin-top:4px; }
      .rb-trial-card {
        background:#fff; border:1px solid var(--rb-line); border-radius:8px;
        padding:10px 12px; display:flex; align-items:center; gap:11px;
        margin-bottom:14px;
      }
      .rb-trial-mark {
        width:36px; height:36px; border-radius:7px;
        display:flex; align-items:center; justify-content:center;
        color:#fff; font-weight:700; font-size:12px; letter-spacing:-.3px;
        background:linear-gradient(135deg,var(--rb-navy),var(--rb-teal));
        flex-shrink:0;
      }
      .rb-trial-meta-k {
        font-size:9.5px; font-weight:600; color:var(--rb-muted);
        text-transform:uppercase; letter-spacing:.6px;
      }
      .rb-trial-meta-name {
        font-size:14px; font-weight:700; color:var(--rb-ink);
        letter-spacing:-.2px; line-height:1.1; margin-top:1px;
      }
      .rb-trial-meta-sub {
        font-size:10.5px; color:var(--rb-muted); margin-top:2px;
        font-variant-numeric:tabular-nums;
      }
      .rb-seg {
        display:flex; background:#fff; border:1px solid var(--rb-line);
        border-radius:6px; padding:2px; gap:2px;
      }
      .rb-seg button {
        flex:1; padding:6px 8px; background:transparent; border:none;
        border-radius:4px; font-size:11.5px; color:var(--rb-muted);
        font-weight:500; cursor:pointer; font-family:inherit;
      }
      .rb-seg button.on {
        background:var(--rb-navy); color:#fff; font-weight:600;
      }
      .rb-rail-input {
        width:100%; padding:7px 10px; border:1px solid var(--rb-line);
        border-radius:6px; background:#fff; font-size:12px;
        color:var(--rb-ink); font-family:inherit;
      }
      .rb-rail-input:focus {
        outline:none; border-color:var(--rb-teal);
        box-shadow:0 0 0 2px rgba(46,196,165,.15);
      }
      .rb-tdesc {
        font-size:11px; color:var(--rb-muted); margin-top:6px;
        line-height:1.45;
      }
      .rb-stat-row {
        display:grid; grid-template-columns:1fr 1fr; gap:6px; margin-top:6px;
      }
      .rb-stat-tile {
        background:#fff; border:1px solid var(--rb-line); border-radius:6px;
        padding:8px 10px;
      }
      .rb-stat-label {
        font-size:9.5px; font-weight:600; color:var(--rb-muted);
        text-transform:uppercase; letter-spacing:.5px;
      }
      .rb-stat-value {
        font-size:16px; font-weight:700; color:var(--rb-navy);
        font-variant-numeric:tabular-nums; letter-spacing:-.4px; margin-top:1px;
      }
      .rb-stat-sub { font-size:10px; color:var(--rb-muted); margin-top:1px; }

      /* ── Centre canvas ── */
      .rb-canvas {
        background:var(--rb-bg); overflow-y:auto; overflow-x:hidden;
        display:flex; flex-direction:column; align-items:center;
        min-width:0; height:100%;
      }
      .rb-toolbar {
        position:sticky; top:0; z-index:10;
        background:rgba(244,246,249,.92); backdrop-filter:blur(8px);
        width:100%; border-bottom:1px solid var(--rb-line);
        padding:10px 22px;
        display:flex; align-items:center; gap:12px; flex-shrink:0;
      }
      .rb-tb-title {
        font-size:13px; font-weight:600; color:var(--rb-ink);
      }
      .rb-tb-meta {
        font-size:11.5px; color:var(--rb-muted);
        font-family:var(--rb-mono); font-variant-numeric:tabular-nums;
      }
      .rb-tb-spacer { flex:1; }
      .rb-tb-btn {
        display:inline-flex; align-items:center; gap:6px;
        padding:6px 12px; border-radius:6px;
        border:1px solid var(--rb-line); background:#fff;
        color:var(--rb-ink-2); font-size:12px; font-weight:500;
        font-family:inherit; cursor:pointer;
      }
      .rb-tb-btn:hover { border-color:#cbd6df; }
      .rb-tb-btn.primary {
        background:var(--rb-navy); color:#fff; border-color:var(--rb-navy);
        font-weight:600; padding:7px 14px;
      }
      .rb-tb-btn.primary:hover { background:var(--rb-navy-lt); }

      .rb-pages {
        display:flex; flex-direction:column; align-items:center;
        gap:24px; padding:28px 22px 80px; width:100%;
        transition: transform .15s ease;
      }
      .rb-page {
        width:794px;  /* A4 width at ~96dpi */
        min-height:1123px; /* A4 height at ~96dpi */
        background:#fff;
        box-shadow:0 1px 4px rgba(15,26,36,.08),0 8px 28px rgba(15,26,36,.10);
        border-radius:2px;
        position:relative;
        overflow:hidden;
      }
      .rb-page-inner { padding:48px 64px 40px; min-height:1050px; }
      .rb-page-marker {
        font-family:var(--rb-mono); font-size:10px;
        color:var(--rb-muted-2); letter-spacing:1px; text-transform:uppercase;
        display:flex; align-items:center; gap:10px; width:794px; max-width:100%;
      }
      .rb-page-marker .ln {
        flex:1; height:1px; background:var(--rb-line);
      }
      .rb-page-foot {
        padding:0 64px 24px; display:flex; justify-content:space-between;
        align-items:center; font-size:9.5px; color:var(--rb-muted);
      }

      /* Document typography */
      .rb-doc { font-family:var(--rb-doc); color:var(--rb-ink);
                font-size:12.5px; line-height:1.6; }
      .rb-doc h1 { font-size:30px; font-weight:600; line-height:1.15;
                   letter-spacing:-.4px; color:var(--rb-ink); margin:0; }
      .rb-doc h2 { font-size:18px; font-weight:600; color:var(--rb-navy);
                   margin:0 0 4px; letter-spacing:-.1px; line-height:1.25; }
      .rb-doc p { margin:0 0 9px; }
      .rb-doc strong { font-weight:600; color:var(--rb-ink); }
      .rb-doc .sec-num {
        font-family:'Inter',sans-serif; font-size:10px; font-weight:600;
        color:var(--rb-teal-dk); text-transform:uppercase; letter-spacing:1.5px;
        margin-bottom:3px;
      }
      .rb-doc .sec-rule {
        height:1px; background:var(--rb-line); margin:0 0 12px;
      }
      .rb-doc .secblock { margin-bottom:18px; }
      .rb-doc .secblock-head {
        display:flex; align-items:baseline; gap:14px;
        margin-bottom:8px; padding-bottom:6px;
        border-bottom:1px solid var(--rb-line);
      }
      .rb-doc .secblock-head .n {
        font-family:'Inter',sans-serif; font-size:10px; font-weight:700;
        color:var(--rb-teal-dk); font-variant-numeric:tabular-nums;
        letter-spacing:.5px;
      }
      .rb-doc .secblock-head h2 { margin:0; flex:1; }

      /* Title page */
      .rb-titlepage {
        display:flex; flex-direction:column; min-height:1000px;
        padding:0; position:relative;
      }
      .rb-tp-stripe {
        position:absolute; left:0; top:0; bottom:0; width:8px;
        background:linear-gradient(180deg,var(--rb-navy),var(--rb-teal));
      }
      .rb-tp-top {
        display:flex; align-items:flex-start; justify-content:space-between;
        gap:20px; padding:40px 64px 0;
      }
      .rb-tp-mark { display:flex; align-items:center; gap:10px; }
      .rb-tp-mark .lm {
        width:34px; height:34px; border-radius:7px;
        display:flex; align-items:center; justify-content:center;
        color:#fff; font-weight:700; font-size:14px; letter-spacing:-.5px;
        background:linear-gradient(135deg,var(--rb-navy),var(--rb-teal));
      }
      .rb-tp-mark .ln-a {
        font-family:'Inter',sans-serif; font-size:11.5px;
        font-weight:600; color:var(--rb-ink);
      }
      .rb-tp-mark .ln-b {
        font-family:'Inter',sans-serif; font-size:9.5px;
        color:var(--rb-muted); letter-spacing:.5px; text-transform:uppercase;
      }
      .rb-tp-spons {
        font-family:'Inter',sans-serif; text-align:right; font-size:9.5px;
        color:var(--rb-muted); letter-spacing:.4px; text-transform:uppercase;
      }
      .rb-tp-spons strong {
        display:block; font-size:10.5px; color:var(--rb-ink-2);
        margin-top:2px; letter-spacing:.2px; text-transform:none;
        font-weight:600;
      }
      .rb-tp-mid {
        flex:1; padding:60px 64px 40px;
        display:flex; flex-direction:column; justify-content:center;
      }
      .rb-tp-eyebrow {
        font-family:'Inter',sans-serif; font-size:11px; font-weight:600;
        color:var(--rb-teal-dk); letter-spacing:2px; text-transform:uppercase;
        margin-bottom:18px; display:flex; align-items:center; gap:10px;
      }
      .rb-tp-eyebrow .bar {
        width:32px; height:2px; background:var(--rb-teal-dk);
      }
      .rb-tp-title {
        font-family:var(--rb-doc); font-size:48px; font-weight:600;
        line-height:1.05; letter-spacing:-1.5px; color:var(--rb-ink);
        margin:0 0 14px;
      }
      .rb-tp-sub {
        font-family:var(--rb-doc); font-size:18px; line-height:1.4;
        color:var(--rb-ink-2); font-style:italic; margin:0 0 20px;
        max-width:540px;
      }
      .rb-tp-period {
        font-family:'Inter',sans-serif; font-size:13px;
        color:var(--rb-ink-2); margin-bottom:6px;
      }
      .rb-tp-period strong { color:var(--rb-ink); font-weight:600; }
      .rb-tp-meta {
        display:grid; grid-template-columns:1fr 1fr; gap:22px 36px;
        margin-top:36px; padding-top:22px;
        border-top:1px solid var(--rb-line);
      }
      .rb-tp-meta .item { display:flex; flex-direction:column; gap:2px; }
      .rb-tp-meta .k {
        font-family:'Inter',sans-serif; font-size:9.5px; font-weight:600;
        color:var(--rb-muted); text-transform:uppercase; letter-spacing:.7px;
      }
      .rb-tp-meta .v {
        font-family:var(--rb-doc); font-size:13.5px;
        color:var(--rb-ink); font-weight:500;
      }

      /* ── Refined single-canvas shell ── */
      .rb-shell {
        position:relative; height:100%;
        display:flex; flex-direction:column;
      }
      .rb-main {
        position:relative; flex:1; display:flex; min-height:0;
      }
      /* Stretch the canvas to fill the full width of the shell — the
         old 3-column grid gave it a 1fr track; now we need flex:1. */
      .rb-main > .rb-canvas { flex: 1 1 auto; min-width: 0; width: 100%; }
      /* Trial chip in toolbar */
      .rb-trial-chip {
        display:inline-flex; align-items:center; gap:8px;
        padding:6px 12px; border-radius:6px; background:#fff;
        border:1px solid var(--rb-line); border-left:3px solid var(--rb-navy);
        font-size:12px; color:var(--rb-ink); line-height:1;
      }
      .rb-trial-chip .rb-trial-code {
        font-weight:700; letter-spacing:.2px; color:var(--rb-ink);
        font-variant-numeric:tabular-nums;
      }
      .rb-trial-chip .rb-trial-sep { color:var(--rb-muted-2); }
      .rb-trial-chip .rb-trial-type { color:var(--rb-muted); font-weight:500; }

      /* Slide-over panel (replaces the old right rail) */
      .rb-panel {
        position:absolute; top:0; right:0; bottom:0;
        width:420px; max-width:90vw;
        background:#fff; border-left:1px solid var(--rb-line);
        box-shadow:-12px 0 32px rgba(15,26,36,.08);
        display:flex; flex-direction:column;
        z-index:20;
        transform:translateX(100%);
        transition:transform .2s ease-out;
      }
      .rb-panel.open { transform:translateX(0); }
      .rb-panel-head {
        display:flex; align-items:center; justify-content:space-between;
        padding:14px 18px; border-bottom:1px solid var(--rb-line-2);
      }
      .rb-panel-head h3 {
        margin:0; font-size:13px; font-weight:600; color:var(--rb-ink);
        text-transform:capitalize;
      }
      .rb-panel-close {
        background:none; border:none; font-size:18px;
        color:var(--rb-muted); cursor:pointer; padding:4px 6px;
        line-height:1;
      }
      .rb-panel-close:hover { color:var(--rb-ink); }
      .rb-panel-body { flex:1; overflow-y:auto; padding:14px 18px 28px; }
      .rb-panel-body h4 {
        font-size:10.5px; font-weight:600; color:var(--rb-muted);
        text-transform:uppercase; letter-spacing:.7px;
        margin:14px 4px 8px;
      }
      .rb-panel-body h4:first-child { margin-top:0; }

      /* Toolbar tab toggle buttons (Sections / Narrative / Meeting / Amendments) */
      .rb-tb-btn.rb-btab {
        background:#fff;
      }
      .rb-tb-btn.rb-btab.active {
        background:var(--rb-rail); color:var(--rb-navy);
        border-color:var(--rb-navy); font-weight:600;
      }
      .rb-tb-sep {
        width:1px; height:20px; background:var(--rb-line); margin:0 4px;
      }
      .rb-tb-meta-pill {
        font-size:11px; color:var(--rb-muted);
        font-variant-numeric:tabular-nums;
        padding:4px 8px; background:var(--rb-rail);
        border-radius:4px;
      }
      .rb-bbody { padding:14px 14px 28px; }
      .rb-bbody h4 {
        font-size:10.5px; font-weight:600; color:var(--rb-muted);
        text-transform:uppercase; letter-spacing:.7px;
        margin:6px 0 8px;
      }
      .rb-bbody h4:not(:first-child) { margin-top:18px; }
      .rb-section-list {
        display:flex; flex-direction:column; gap:5px;
      }
      .rb-sec-item {
        display:flex; align-items:center; gap:8px;
        padding:8px 10px; background:#fff;
        border:1px solid var(--rb-line); border-radius:6px;
        transition:border-color .12s;
      }
      .rb-sec-item:hover { border-color:#cbd6df; }
      .rb-sec-handle {
        color:var(--rb-muted-2); flex-shrink:0; font-size:11px;
        line-height:1; cursor:grab; user-select:none;
      }
      .rb-sec-toggle {
        width:14px; height:14px; border:1.5px solid var(--rb-line);
        border-radius:3px; cursor:pointer; flex-shrink:0;
        display:flex; align-items:center; justify-content:center;
        background:#fff;
      }
      .rb-sec-toggle.on {
        background:var(--rb-navy); border-color:var(--rb-navy);
      }
      .rb-sec-toggle.on::after {
        content:''; width:6px; height:3px;
        border-left:1.5px solid #fff; border-bottom:1.5px solid #fff;
        transform:rotate(-45deg) translate(0,-1px);
      }
      .rb-sec-meta { flex:1; min-width:0; }
      .rb-sec-title {
        font-size:12px; color:var(--rb-ink);
        font-weight:500; line-height:1.2;
      }
      .rb-sec-group {
        font-size:10px; color:var(--rb-muted); margin-top:1px;
      }
      .rb-sec-arrows {
        display:flex; flex-direction:column; gap:1px; flex-shrink:0;
      }
      .rb-sec-arrows button {
        background:transparent; border:none; color:var(--rb-muted);
        font-size:8px; line-height:1; padding:1px 4px;
        cursor:pointer; font-family:inherit;
      }
      .rb-sec-arrows button:hover { color:var(--rb-navy); }
      .rb-sec-page {
        font-size:10.5px; color:var(--rb-muted);
        font-variant-numeric:tabular-nums;
        font-family:var(--rb-mono); flex-shrink:0;
      }
      .rb-chips { display:flex; flex-wrap:wrap; gap:5px; }
      .rb-chip {
        padding:4px 9px; border-radius:14px;
        border:1px solid var(--rb-line); background:#fff;
        font-size:11px; color:var(--rb-ink-2);
        cursor:pointer; font-family:inherit;
      }
      .rb-chip:hover {
        border-color:var(--rb-navy); color:var(--rb-navy);
      }
      .rb-ta {
        width:100%; padding:8px 10px;
        border:1px solid var(--rb-line); border-radius:6px;
        background:#fff; font-size:12px; color:var(--rb-ink);
        font-family:inherit; resize:vertical; line-height:1.5;
      }
      .rb-ta:focus {
        outline:none; border-color:var(--rb-teal);
        box-shadow:0 0 0 2px rgba(46,196,165,.15);
      }
      .rb-field {
        display:flex; flex-direction:column; gap:5px; margin-bottom:10px;
      }
      .rb-field label {
        font-size:11px; font-weight:600; color:var(--rb-ink-2);
      }
      .rb-amend {
        border:1px solid var(--rb-line); border-radius:6px;
        padding:8px 10px; background:#fff; margin-bottom:6px;
      }
      .rb-amend-head {
        display:flex; align-items:center; justify-content:space-between;
        margin-bottom:4px; gap:8px;
      }
      .rb-amend-ref {
        font-family:var(--rb-mono); font-size:10.5px;
        color:var(--rb-navy); font-weight:600;
      }
      .rb-amend-status {
        font-size:9.5px; text-transform:uppercase; letter-spacing:.4px;
        font-weight:600; padding:2px 6px; border-radius:3px;
      }
      .rb-amend-status.sub { background:#FEE2E2; color:#991B1B; }
      .rb-amend-status.nonsub { background:#E0F2FE; color:#075985; }
      .rb-amend-desc {
        font-size:11.5px; color:var(--rb-ink-2); line-height:1.4;
      }
      .rb-add-btn {
        width:100%; padding:7px;
        background:transparent; border:1px dashed var(--rb-line);
        border-radius:6px; color:var(--rb-muted); font-size:11.5px;
        font-weight:500; cursor:pointer; font-family:inherit;
      }
      .rb-add-btn:hover {
        color:var(--rb-navy); border-color:var(--rb-navy);
      }

      /* ─── Portfolio Review (Trial Update Summary v4.0) ──────────────── */
      .pf-banner{background:#7030A0;color:#fff;font-size:15px;font-weight:700;
                 letter-spacing:.04em;text-transform:uppercase;text-align:center;
                 padding:10px 16px;border-radius:4px 4px 0 0;font-variant:small-caps;
                 font-family:'Inter',sans-serif;}
      .pf-header-block{border:1px solid #E2E8EE;border-radius:4px;overflow:hidden;
                       margin-bottom:0;background:#fff;}
      .pf-info-grid{display:grid;grid-template-columns:1fr 1fr;
                    border-top:1px solid #E2E8EE;font-family:'Inter',sans-serif;}
      .pf-info-row{display:grid;grid-template-columns:160px 1fr;
                   border-bottom:1px solid #EEF2F6;}
      .pf-info-cell{padding:5px 10px;font-size:11px;line-height:1.4;}
      .pf-info-cell.label{font-weight:600;color:#0F1A24;background:#F8FAFC;}
      .pf-info-cell.value{color:#27384A;}

      .pf-status-row{display:flex;gap:18px;padding:8px 10px;
                     border:1px solid #E2E8EE;border-top:0;background:#fff;
                     font-family:'Inter',sans-serif;}
      .pf-check-item{display:flex;align-items:center;gap:6px;font-size:11px;
                     color:#27384A;}
      .pf-check-item.small{font-size:10.5px;}
      .pf-checkbox{width:14px;height:14px;border:1.5px solid #E2E8EE;
                   border-radius:3px;display:inline-flex;align-items:center;
                   justify-content:center;font-size:9px;color:#fff;
                   flex-shrink:0;background:#fff;line-height:1;}
      .pf-checkbox.sm{width:12px;height:12px;font-size:8px;}
      .pf-checkbox.checked{background:#7030A0;border-color:#7030A0;}

      .pf-summary{border:1px solid #E2E8EE;border-top:0;padding:8px 10px;
                  background:#fff;border-radius:0 0 4px 4px;margin-bottom:10px;
                  font-family:'Inter',sans-serif;}
      .pf-summary-label{font-size:10.5px;font-weight:600;color:#0F1A24;
                        margin-bottom:4px;}
      .pf-summary-text{font-family:'Source Serif 4',Georgia,serif;font-size:11px;
                       color:#27384A;line-height:1.55;}

      .pf-section{margin-bottom:10px;font-family:'Inter',sans-serif;}
      .pf-section-banner{background:#7030A0;color:#fff;font-size:11.5px;
                         font-weight:700;letter-spacing:.04em;padding:6px 12px;
                         border-radius:4px 4px 0 0;text-transform:uppercase;}
      .pf-section-banner.alert{background:#7F1D1D;}

      .pf-progress-grid{border:1px solid #E2E8EE;border-top:0;
                        border-radius:0 0 4px 4px;background:#fff;}
      .pf-yn-row{display:grid;grid-template-columns:200px 1fr;
                 border-bottom:1px solid #EEF2F6;align-items:center;}
      .pf-yn-row:last-child{border-bottom:0;}
      .pf-yn-label{font-size:10.5px;font-weight:500;color:#0F1A24;
                   padding:6px 10px;background:#F8FAFC;}
      .pf-yn-answer{display:flex;align-items:center;gap:14px;padding:6px 10px;}
      .pf-yn-date{font-size:10.5px;color:#64748B;margin-left:8px;}
      .pf-yn-date strong{color:#0F1A24;font-weight:600;}
      .pf-divider{height:1px;background:#E2E8EE;}
      .pf-meeting-row{display:grid;grid-template-columns:200px 1fr;
                      border-bottom:1px solid #EEF2F6;align-items:center;}
      .pf-meeting-label{font-size:10.5px;font-weight:500;color:#0F1A24;
                        padding:6px 10px;background:#F8FAFC;}
      .pf-meeting-dates{display:flex;gap:28px;padding:6px 10px;font-size:10.5px;
                        color:#64748B;}
      .pf-meeting-dates strong{color:#0F1A24;font-weight:600;
                               font-family:'JetBrains Mono',ui-monospace,Menlo,monospace;
                               font-size:10px;}
      .pf-further-row{display:grid;grid-template-columns:200px 1fr;align-items:start;}
      .pf-further-label{font-size:10.5px;font-weight:500;color:#0F1A24;
                        padding:6px 10px;background:#F8FAFC;}
      .pf-further-text{font-size:10.5px;color:#27384A;line-height:1.55;
                       padding:6px 10px;}

      .pf-rag-grid{border:1px solid #E2E8EE;border-top:0;
                   border-radius:0 0 4px 4px;background:#fff;
                   display:flex;flex-direction:column;}
      .pf-rag-item{display:flex;align-items:center;gap:10px;padding:7px 12px;
                   border-bottom:1px solid #EEF2F6;}
      .pf-rag-item:last-child{border-bottom:0;}
      .pf-rag-dot{width:10px;height:10px;border-radius:50%;flex-shrink:0;}
      .pf-rag-text{font-size:10.5px;color:#64748B;line-height:1.4;}

      .pf-chart-container{border:1px solid #E2E8EE;border-top:0;
                          border-radius:0 0 4px 4px;background:#fff;
                          padding:12px 14px;}
      .pf-chart-placeholder{display:flex;flex-direction:column;align-items:center;
                            justify-content:center;height:200px;
                            border:2px dashed #E2E8EE;border-radius:6px;
                            background:#F8FAFC;gap:8px;}
      .pf-chart-placeholder-icon{font-size:28px;color:#94A3B8;}
      .pf-chart-placeholder-text{font-size:12px;color:#64748B;font-weight:500;}
      .pf-chart-placeholder-sub{font-size:10.5px;color:#94A3B8;}
      .pf-recruit-stats{display:grid;grid-template-columns:repeat(4,1fr);
                        margin-top:8px;border-top:1px solid #E2E8EE;
                        border-bottom:1px solid #E2E8EE;}
      .pf-recruit-stat{padding:7px 10px 7px 0;display:flex;flex-direction:column;
                       gap:1px;border-right:1px solid #EEF2F6;}
      .pf-recruit-stat:last-child{border-right:0;}
      .pf-recruit-stat:not(:first-child){padding-left:10px;}
      .pf-recruit-stat .k{font-size:9px;font-weight:600;color:#64748B;
                          text-transform:uppercase;letter-spacing:.5px;}
      .pf-recruit-stat .v{font-size:16px;font-weight:700;color:#0F1A24;
                          font-variant-numeric:tabular-nums;letter-spacing:-.3px;}

      .pf-table{width:100%;border-collapse:collapse;font-size:10.5px;
                border:1px solid #E2E8EE;border-top:0;
                border-radius:0 0 4px 4px;overflow:hidden;
                font-family:'Inter',sans-serif;}
      .pf-table thead th{font-size:9px;font-weight:600;color:#64748B;
                         text-transform:uppercase;letter-spacing:.6px;
                         text-align:left;padding:7px 10px;
                         border-bottom:1.5px solid #0F1A24;background:#F8FAFC;}
      .pf-table tbody td{padding:6px 10px;border-bottom:1px solid #EEF2F6;
                         color:#27384A;line-height:1.4;background:#fff;}
      .pf-table tbody td.mono{font-family:'JetBrains Mono',ui-monospace,Menlo,monospace;
                              font-size:10px;}
      .pf-pill{display:inline-flex;align-items:center;font-size:9.5px;
               font-weight:600;padding:1px 7px;border-radius:10px;
               text-transform:uppercase;letter-spacing:.3px;}
      .pf-pill.green{background:#D1FAE5;color:#065F46;}
      .pf-pill.amber{background:#FEF3C7;color:#92400E;}
      .pf-pill.grey{background:#EEF2F6;color:#27384A;}
      .pf-pill.red{background:#FEE2E2;color:#991B1B;}
      .pf-data-capture{display:flex;align-items:center;gap:8px;}
      .pf-dc-bar{flex:1;height:6px;background:#EEF2F6;border-radius:3px;
                 overflow:hidden;max-width:120px;}
      .pf-dc-fill{height:100%;background:#7030A0;border-radius:3px;}
      .pf-dc-label{font-family:'JetBrains Mono',ui-monospace,Menlo,monospace;
                   font-size:10.5px;color:#0F1A24;font-weight:600;}

      .pf-kv-grid{border:1px solid #E2E8EE;border-top:0;
                  border-radius:0 0 4px 4px;background:#fff;}
      .pf-kv-row{display:grid;grid-template-columns:240px 1fr;
                 border-bottom:1px solid #EEF2F6;}
      .pf-kv-row:last-child{border-bottom:0;}
      .pf-kv-label{font-size:10.5px;font-weight:500;color:#0F1A24;
                   padding:5px 10px;background:#F8FAFC;
                   font-family:'Inter',sans-serif;}
      .pf-kv-value{font-family:'JetBrains Mono',ui-monospace,Menlo,monospace;
                   font-size:10.5px;color:#27384A;padding:5px 10px;}

      .pf-staffing-grid{border:1px solid #E2E8EE;border-top:0;
                        border-radius:0 0 4px 4px;background:#fff;}
      .pf-staff-row{display:grid;grid-template-columns:240px 1fr;
                    border-bottom:1px solid #EEF2F6;align-items:start;}
      .pf-staff-row:last-child{border-bottom:0;}
      .pf-staff-label{font-size:10.5px;font-weight:500;color:#0F1A24;
                      padding:6px 10px;background:#F8FAFC;
                      font-family:'Inter',sans-serif;}
      .pf-staff-value{font-size:10.5px;color:#27384A;padding:6px 10px;
                      line-height:1.5;font-family:'Inter',sans-serif;}

      .pf-issues-block{border:1px solid #E2E8EE;border-top:0;
                       border-radius:0 0 4px 4px;background:#fff;
                       display:grid;grid-template-columns:1fr 1fr;}
      .pf-issues-col{padding:8px 12px;border-right:1px solid #EEF2F6;
                     font-family:'Inter',sans-serif;}
      .pf-issues-col:last-child{border-right:0;}
      .pf-issues-heading{font-size:10px;font-weight:600;color:#0F1A24;
                         text-transform:uppercase;letter-spacing:.5px;
                         margin-bottom:6px;padding-bottom:4px;
                         border-bottom:1px solid #EEF2F6;}
      .pf-issues-list{margin:0;padding:0 0 0 14px;font-size:10.5px;
                      color:#27384A;line-height:1.55;display:flex;
                      flex-direction:column;gap:6px;}
      .pf-issues-list li::marker{color:#94A3B8;}

      /* Portfolio page — slightly different padding than generic rb-page */
      .rb-page.pf-page .rb-page-inner{padding:18px 48px 14px;}
      .rb-page.pf-page .pf-page-meta{display:flex;justify-content:space-between;
                                     align-items:center;font-size:9.5px;
                                     color:#64748B;font-family:'Inter',sans-serif;
                                     margin-bottom:12px;letter-spacing:.3px;
                                     font-weight:500;}
    ")),
    tags$link(href = paste0("https://fonts.googleapis.com/css2",
                            "?family=Inter:wght@400;500;600;700",
                            "&family=Source+Serif+4:wght@400;600;700",
                            "&family=JetBrains+Mono:wght@400;500&display=swap"),
              rel = "stylesheet"),

    div(class = "rb-app",
      div(class = "rb-shell",

        # ─── TOP TOOLBAR ─────────────────────────────────────────────────
        div(class = "rb-toolbar",
            # Left cluster: trial chip + report-type segmented + meta
            div(class = "rb-trial-chip",
                textOutput("rb_canvas_title", inline = TRUE)),
            uiOutput("rb_template_seg", inline = TRUE),
            div(class = "rb-tb-meta-pill",
                textOutput("rb_canvas_meta", inline = TRUE)),

            div(class = "rb-tb-spacer"),

            # Zoom controls (kept as before)
            tags$button(class = "rb-tb-btn", type = "button",
                        onclick = "var p=$('.rb-pages'); var z=parseFloat(p.data('zoom')||1); z=Math.max(0.3,z-0.1); p.data('zoom',z).css('transform','scale('+z+')').css('transform-origin','top center'); $('#rb_zoom_val').text(Math.round(z*100)+'%');",
                        HTML("&minus;")),
            span(id = "rb_zoom_val",
                 style = "font-size:12px;font-weight:600;color:#1B4F6B;min-width:40px;text-align:center;",
                 "100%"),
            tags$button(class = "rb-tb-btn", type = "button",
                        onclick = "var p=$('.rb-pages'); var z=parseFloat(p.data('zoom')||1); z=Math.min(1.5,z+0.1); p.data('zoom',z).css('transform','scale('+z+')').css('transform-origin','top center'); $('#rb_zoom_val').text(Math.round(z*100)+'%');",
                        HTML("+")),

            div(class = "rb-tb-sep"),

            # Panel toggles — Sections / Narrative / Meeting / Amendments
            tags$button(id = "rb_btab_sections",
                        class = "rb-tb-btn rb-btab action-button",
                        type = "button", "Sections"),
            tags$button(id = "rb_btab_narrative",
                        class = "rb-tb-btn rb-btab action-button",
                        type = "button", "Narrative"),
            tags$button(id = "rb_btab_meeting",
                        class = "rb-tb-btn rb-btab action-button",
                        type = "button", "Meeting"),
            tags$button(id = "rb_btab_amend",
                        class = "rb-tb-btn rb-btab action-button",
                        type = "button", "Amendments"),
            tags$button(id = "rb_btab_portfolio",
                        class = "rb-tb-btn rb-btab action-button",
                        type = "button", "Portfolio"),

            div(class = "rb-tb-sep"),

            # Preview · Print
            tags$button(class = "rb-tb-btn", type = "button",
                        onclick = "window.print();",
                        HTML("Preview &middot; Print")),

            # Generate report (primary)
            actionButton("rb_open_generate_modal",
                         HTML("&#x2193; Generate report"),
                         class = "rb-tb-btn primary",
                         style = "background:#1B4F6B;color:#fff;")
        ),

        # ─── MAIN AREA ───────────────────────────────────────────────────
        div(class = "rb-main",
          # Full-width document canvas (template unchanged)
          tags$main(class = "rb-canvas",
            div(class = "rb-pages",
                uiOutput("rb_document_preview"))
          ),

          # Slide-over panel — hidden by default; opened via toolbar tabs
          tags$aside(id = "rb_panel", class = "rb-panel",
            div(class = "rb-panel-head",
              tags$h3(id = "rb_panel_title", "Sections"),
              tags$button(id = "rb_panel_close",
                          class = "rb-panel-close action-button",
                          type = "button",
                          HTML("&times;"))
            ),
            div(class = "rb-panel-body",
              uiOutput("rb_builder_body")
            )
          )
        )
      ),

      # Preserve server bindings that used to live in the left rail.
      # These outputs are still produced by reports_server; keeping the
      # uiOutput targets in the DOM (hidden) prevents Shiny binding errors
      # without surfacing the old controls visually.
      div(style = "display:none;",
          uiOutput("rb_trial_card"),
          uiOutput("rb_template_desc"),
          uiOutput("rb_summary_tiles"),
          dateRangeInput("rpt_dates", label = NULL,
                         start = floor_date(Sys.Date() %m-% months(2), "month"),
                         end   = Sys.Date(),
                         format = "d M yyyy"),
          tags$button(id = "rb_scope_filtered", class = "on action-button",
                      type = "button", "Filtered"),
          tags$button(id = "rb_scope_full", class = "action-button",
                      type = "button", "Full trial")
      )
    ),

    # JS: toolbar tab → slide-over open/close + scope segment buttons
    tags$script(HTML("
      var RB_PANEL_TITLES = {
        sections:  'Sections',
        narrative: 'Narrative',
        meeting:   'Meeting details',
        amend:     'Amendments',
        portfolio: 'Portfolio review'
      };
      $(document).on('click', '.rb-btab', function(){
        var $btn = $(this);
        var id   = $btn.attr('id');
        var key  = id.replace('rb_btab_','');
        var $panel = $('#rb_panel');
        var alreadyActive = $btn.hasClass('active');
        $('.rb-btab').removeClass('active');
        if (alreadyActive) {
          $panel.removeClass('open');
          Shiny.setInputValue('rb_active_btab', null, {priority:'event'});
        } else {
          $btn.addClass('active');
          $('#rb_panel_title').text(RB_PANEL_TITLES[key] || key);
          $panel.addClass('open');
          Shiny.setInputValue('rb_active_btab', key, {priority:'event'});
        }
      });
      $(document).on('click', '#rb_panel_close', function(){
        $('.rb-btab').removeClass('active');
        $('#rb_panel').removeClass('open');
        Shiny.setInputValue('rb_active_btab', null, {priority:'event'});
      });
      $(document).on('click', '#rb_scope_filtered, #rb_scope_full', function(){
        $('#rb_scope_filtered, #rb_scope_full').removeClass('on');
        $(this).addClass('on');
        var key = $(this).attr('id') === 'rb_scope_full' ? 'full' : 'filtered';
        Shiny.setInputValue('rb_scope', key, {priority:'event'});
      });
    "))
  )
}

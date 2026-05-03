tonic_theme <- bs_theme(
  version = 5,
  primary = "#1B4F6B",
  secondary = "#2EC4A5",
  success = "#10B981",
  warning = "#F59E0B",
  danger = "#EF4444",
  info = "#3B82F6",
  bg = "#EEF3F8",
  fg = "#1E293B",
  base_font = font_google("Outfit"),
  heading_font = font_google("Outfit", wght = "700"),
  "card-border-radius" = "8px",
  "card-cap-bg" = "#F8FAFD",
  "card-border-color" = "#DDE5EE",
  "input-border-radius" = "5px",
  "input-border-color" = "#DDE5EE",
  "input-focus-border-color" = "#2EC4A5",
  "input-focus-box-shadow" = "0 0 0 0.2rem rgba(46,196,165,.2)",
  "btn-border-radius" = "5px",
  "sidebar-bg" = "#1B4F6B",
  "sidebar-fg" = "rgba(255,255,255,0.85)",
  "sidebar-width" = "240px"
)

tonic_css <- "
@import url('https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700&display=swap');
@import url('https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.5.0/css/all.min.css');

:root {
  --navy:#1B4F6B;--navy-dk:#143D54;--teal:#2EC4A5;--teal-dk:#0FA88E;
  --teal-lt:#E0F7F3;--amber:#F59E0B;--red:#EF4444;--blue:#3B82F6;
  --bg:#EEF3F8;--card:#fff;--border:#DDE5EE;--text:#1E293B;--muted:#64748B;
  --font:'Outfit',sans-serif;
}
body, .shiny-app { font-family: 'Outfit', sans-serif !important; }

/* TOPBAR */
.topbar{background:var(--navy);height:52px;display:flex;align-items:center;justify-content:space-between;padding:0 22px;box-shadow:0 2px 8px rgba(0,0,0,.12);flex-shrink:0;margin:-20px -22px 20px -22px;width:calc(100% + 44px)}
.topbar-title{color:#fff;font-size:14px;font-weight:600}
.topbar-badge{background:rgba(46,196,165,.2);color:var(--teal);font-size:10px;font-weight:600;padding:3px 9px;border-radius:20px;border:1px solid rgba(46,196,165,.3);margin-left:10px}
.topbar-user{color:rgba(255,255,255,.6);font-size:12px;display:flex;align-items:center;gap:8px}
.role-badge{font-size:10px;padding:3px 9px;border-radius:20px;font-weight:600;background:#FEF3C7;color:#92400E}

/* SIDEBAR */
.sidebar, .bslib-sidebar-layout[style] > .sidebar { background: #1B4F6B !important; }
.sidebar-logo{padding:16px 16px 12px;border-bottom:1px solid rgba(255,255,255,.1);display:flex;align-items:center;gap:10px}
.sidebar-logo img{height:34px;object-fit:contain;filter:brightness(0) invert(1)}
.sidebar-logo-info{display:flex;flex-direction:column}
.sidebar-user-name{color:#fff;font-size:12px;font-weight:500}
.sidebar-user-role{color:rgba(255,255,255,.45);font-size:10px;letter-spacing:.5px}
.nav-section-label{color:rgba(255,255,255,.3);font-size:9px;font-weight:600;letter-spacing:2px;text-transform:uppercase;padding:14px 16px 4px;display:block}

/* NAV BUTTONS */
.sidebar-nav-btn{display:flex!important;align-items:center!important;gap:10px!important;width:100%!important;text-align:left!important;padding:11px 18px!important;margin-bottom:1px!important;background:rgba(255,255,255,.06)!important;border:none!important;border-left:3px solid transparent!important;color:rgba(255,255,255,.65)!important;font-family:'Outfit',sans-serif!important;font-size:13px!important;font-weight:500!important;border-radius:0!important;transition:all .18s!important;box-shadow:none!important}
.sidebar-nav-btn:hover{background:rgba(255,255,255,.07)!important;color:#fff!important;border-left-color:rgba(46,196,165,.4)!important}
.sidebar-nav-btn.active-nav{background:rgba(255,255,255,.12)!important;color:#fff!important;border-left:3px solid #2EC4A5!important;font-weight:600!important}
.sidebar-nav-btn .fa{opacity:.65;font-size:14px;width:18px;text-align:center}
.sidebar-nav-btn.active-nav .fa,.sidebar-nav-btn:hover .fa{opacity:1;color:#2EC4A5!important}

/* MEETING BAR */
.meeting-bar{background:#fff;border:1px solid var(--border);border-radius:7px;padding:10px 16px;margin-bottom:14px;display:flex;align-items:center;gap:14px;flex-wrap:wrap;box-shadow:0 1px 3px rgba(0,0,0,.05)}

/* VALUE BOXES */
.tonic-vbox{background:var(--card);border:1px solid var(--border);border-radius:8px;padding:16px 18px;position:relative;overflow:hidden;box-shadow:0 1px 3px rgba(0,0,0,.05);transition:all .2s;border-top:3px solid var(--teal);height:100%}
.tonic-vbox:hover{box-shadow:0 4px 16px rgba(27,79,107,.12);transform:translateY(-1px)}
.tonic-vbox-icon{width:34px;height:34px;border-radius:8px;display:flex;align-items:center;justify-content:center;font-size:15px;margin-bottom:10px}
.tonic-vbox-label{font-size:10px;font-weight:600;text-transform:uppercase;letter-spacing:.7px;color:var(--muted);margin-bottom:3px}
.tonic-vbox-value{font-size:28px;font-weight:700;color:var(--navy);letter-spacing:-1px;line-height:1}
.tonic-vbox-sub{font-size:11px;color:var(--muted);margin-top:3px}
.delta-badge{font-size:11px;font-weight:700;color:#059669;background:#D1FAE5;padding:1px 7px;border-radius:12px;margin-left:6px;display:inline-block}
.delta-neg{color:#DC2626!important;background:#FEE2E2!important}

/* CARDS */
.card{background:var(--card)!important;border:1px solid var(--border)!important;border-radius:8px!important;box-shadow:0 1px 3px rgba(0,0,0,.05)!important;overflow:hidden;margin-bottom:15px;border-top:3px solid var(--teal)!important}
.card-header{padding:12px 16px!important;border-bottom:1px solid var(--border)!important;font-weight:600!important;color:#1B4F6B!important;font-family:'Outfit',sans-serif!important;font-size:13px!important}
.card-header-amber{border-top-color:#F59E0B!important}
.nav-hidden > .nav{display:none!important}
.demo-card { padding:4px 0; }
.demo-headline { font-size:22px; font-weight:700; color:#1B4F6B; }
.demo-row { display:flex; justify-content:space-between; align-items:center; padding:5px 0; border-bottom:1px solid #F1F5F9; }
.demo-row:last-child { border-bottom:none; }
.demo-label { font-size:12px; color:#64748B; }
.demo-val { font-size:13px; font-weight:600; color:#1B4F6B; }

/* PILLS */
.pill{display:inline-flex;align-items:center;gap:4px;padding:2px 9px;border-radius:20px;font-size:11px;font-weight:600}
.pr{background:#D1FAE5;color:#065F46}
.po{background:#DBEAFE;color:#1E40AF}
.ps{background:#FEF3C7;color:#92400E}
.pi{background:#F1F5F9;color:#475569}
.pc{background:#FEE2E2;color:#991B1B}
.spill{display:inline-flex;align-items:center;gap:4px;padding:2px 9px;border-radius:20px;font-size:11px;font-weight:600}
.sp-r{background:#D1FAE5;color:#065F46}
.sp-o{background:#DBEAFE;color:#1E40AF}
.sp-s{background:#FEF3C7;color:#92400E}
.sp-i{background:#F1F5F9;color:#475569}
.sp-c{background:#FEE2E2;color:#991B1B}

/* PROGRESS */
.prog-wrap{display:flex;align-items:center;gap:6px}
.prog-track{flex:1;background:#E2EAF0;border-radius:4px;height:6px;overflow:hidden;min-width:60px}
.prog-fill{height:6px;border-radius:4px;background:linear-gradient(90deg,var(--teal-dk),var(--teal))}
.prog-lbl{font-size:11px;color:var(--muted);white-space:nowrap}

/* RAND BUTTONS */
.rand-cell{display:flex;align-items:center;gap:8px;justify-content:center}
.rnum{font-size:17px;font-weight:700;min-width:28px;text-align:center;color:var(--navy)}
.rbtn{width:28px;height:28px;border-radius:50%;border:2px solid var(--teal);background:#fff;color:var(--teal);font-size:17px;font-weight:600;cursor:pointer;display:inline-flex;align-items:center;justify-content:center;transition:all .15s;line-height:1}
.rbtn:hover{background:var(--teal);color:#fff}
.rbtn-minus,.rbtn.minus{border-color:var(--red);color:var(--red)}
.rbtn-minus:hover,.rbtn.minus:hover{background:var(--red);color:#fff}

/* TABLES */
.reactable{font-family:'Outfit',sans-serif!important}
.sid{font-weight:600;color:var(--navy)}
.editable-num{width:58px;padding:4px 7px;border:1px solid var(--border);border-radius:4px;font-family:'Outfit',sans-serif;font-size:12px;text-align:center;color:var(--navy);font-weight:600;outline:none}
.editable-num:focus{border-color:var(--teal)}

/* GRIDS */
.grid-2{display:grid;grid-template-columns:1fr 1fr;gap:15px;margin-bottom:15px}
.grid-8-4{display:grid;grid-template-columns:2fr 1fr;gap:15px;margin-bottom:15px}

/* HEATMAP */
.hm-table{width:100%;border-collapse:collapse;font-size:12px}
.hm-table thead th{background:#F8FAFD;color:var(--navy);font-size:9px;font-weight:600;text-transform:uppercase;letter-spacing:.5px;padding:8px 10px;text-align:center;border-bottom:2px solid var(--border);white-space:nowrap}
.hm-table th.site-td{text-align:left;min-width:140px}
.hm-table td{padding:5px 8px;border:1px solid #EEF3F8;text-align:center}
.hm-table td.site-td{text-align:left;font-weight:600;color:var(--navy);background:#F8FAFD;padding:6px 12px}

/* COMPLETION */
.comp-tbl { overflow-x:auto; }
.comp-tbl table { width:100%; border-collapse:collapse; font-size:12px; font-family:'Outfit',sans-serif; min-width:900px; }
.comp-tbl th, .comp-tbl td { padding:10px 14px; border-bottom:1px solid #EEF3F8; }
.comp-tbl thead tr:first-child th { font-size:11px; font-weight:600; letter-spacing:.5px; text-transform:uppercase; text-align:center; }
.comp-tbl thead tr:nth-child(2) th { font-size:11px; font-weight:600; text-align:center; text-transform:uppercase; letter-spacing:.3px; }
.comp-tbl tbody td { vertical-align:middle; text-align:center; }
.comp-tbl tbody tr:hover { background:rgba(46,196,165,.04); }
.tp-baseline { background:#F5F3FF; color:#6D28D9; border-bottom:3px solid #7C3AED !important; }
.tp-baseline-sub { background:#FAF8FF; color:#7C3AED; }
.tp-discharge { background:#EFF6FF; color:#1E40AF; border-bottom:3px solid #3B82F6 !important; }
.tp-discharge-sub { background:#F8FBFF; color:#2563EB; }
.tp-d30 { background:#ECFDF5; color:#065F46; border-bottom:3px solid #2EC4A5 !important; }
.tp-d30-sub { background:#F5FDFB; color:#0D7A5F; }
.tp-d90 { background:#ECFDF5; color:#065F46; border-bottom:3px solid #059669 !important; }
.tp-d90-sub { background:#F5FDF9; color:#065F46; }
.col-bl { background:rgba(124,58,237,.03); }
.col-dc { background:rgba(37,99,235,.03); }
.col-d30 { background:rgba(46,196,165,.03); }
.col-d90 { background:rgba(5,150,105,.04); }
.tp-div-bl { border-left:3px solid #7C3AED; }
.tp-div-dc { border-left:3px solid #3B82F6; }
.tp-div-d30 { border-left:3px solid #2EC4A5; }
.tp-div-d90 { border-left:3px solid #059669; }
.tp-border-r { border-right:2px solid #EEF3F8; }
.c-complete { color:#059669; font-weight:700; font-size:15px; }
.c-partial { color:#D97706; font-weight:700; font-size:14px; }
.c-unverified { color:#D97706; font-weight:600; font-size:14px; }
.c-none { color:#CBD5E1; font-size:13px; }

/* FILTER BAR */
.fbar{background:#F8FAFD;border:1px solid var(--border);border-radius:7px;padding:12px 16px;margin-bottom:14px;display:flex;gap:14px;align-items:flex-end;flex-wrap:wrap}
.fg{display:flex;flex-direction:column;gap:4px}
.fg label{font-size:10px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.6px}

/* INFO BOXES */
.info-box-tonic{background:#F0F9F7;border:1px solid #C4EBE4;border-left:3px solid var(--teal-dk);border-radius:5px;padding:10px 14px;font-size:12px;color:var(--muted);line-height:1.6;margin-bottom:14px}
.info-box-amber{background:#FFFBEB;border:1px solid #FDE68A;border-left:3px solid var(--amber);border-radius:5px;padding:10px 14px;font-size:12px;color:var(--muted);line-height:1.6;margin-bottom:14px}

/* SECTION HEADINGS */
.section-heading{font-size:13px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.8px;margin-bottom:10px;padding-bottom:6px;border-bottom:2px solid var(--teal);display:flex;align-items:center;gap:8px}
.section-heading.amber{border-bottom-color:var(--amber)}

/* STATUS GRID */
.status-grid{display:grid;grid-template-columns:1fr 1fr;gap:10px;margin-bottom:14px}
.status-card{background:#F8FAFD;border:1px solid var(--border);border-radius:6px;padding:12px 14px}
.status-card-label{font-size:9px;font-weight:600;color:var(--navy);text-transform:uppercase;letter-spacing:.8px;margin-bottom:6px}
.status-card-val{font-size:20px;font-weight:700;color:var(--navy);line-height:1}
.status-card-sub{font-size:11px;color:var(--muted);margin-top:2px}

/* FILE LIST */
.file-row{display:flex;align-items:center;justify-content:space-between;padding:9px 12px;border-bottom:1px solid #F1F5FA;font-size:12px}
.file-row:last-child{border-bottom:none}
.file-name{font-weight:500;color:var(--navy)}
.file-meta{color:var(--muted);font-size:11px}
.file-loaded{background:#DCFCE7;color:#166534;padding:2px 9px;border-radius:12px;font-size:10px;font-weight:600}

/* FORMS */
.form-grid{display:grid;gap:12px;margin-bottom:12px}
.form-grid.g5{grid-template-columns:repeat(5,1fr)}
.form-grid.g4{grid-template-columns:repeat(4,1fr)}
.form-grid.g3{grid-template-columns:repeat(3,1fr)}
.form-field label{display:block;font-size:10px;font-weight:600;color:var(--muted);text-transform:uppercase;letter-spacing:.7px;margin-bottom:5px}
.form-field input,.form-field select{width:100%;padding:8px 11px;border:1px solid var(--border);border-radius:5px;font-family:var(--font);font-size:13px;color:var(--text);outline:none;background:#fff}
.form-field input:focus,.form-field select:focus{border-color:var(--teal)}

/* PERMISSIONS TABLE */
.perm-table{width:100%;border-collapse:collapse;font-size:12px}
.perm-table th{background:#F8FAFD;color:var(--navy);font-size:10px;font-weight:600;text-transform:uppercase;letter-spacing:.5px;padding:8px 12px;border-bottom:2px solid var(--border);text-align:center}
.perm-table th:first-child{text-align:left}
.perm-table td{padding:8px 12px;border-bottom:1px solid #F1F5FA;text-align:center;font-size:12px}
.perm-table td:first-child{text-align:left;font-weight:600}

/* MISC */
.shiny-notification{font-family:'Outfit',sans-serif!important;font-size:12px!important;border-radius:8px!important}
.shiny-spinner-output-container .load-container .circle{border-top-color:#2EC4A5!important}
.leaflet-popup-content-wrapper{font-family:Outfit,sans-serif!important;border-radius:8px!important;border-top:3px solid #2EC4A5!important}
::-webkit-scrollbar{width:5px}::-webkit-scrollbar-track{background:transparent}::-webkit-scrollbar-thumb{background:#CBD5E1;border-radius:4px}

/* TRIAL SELECTOR */
.trial-select-card{background:var(--card,#fff);border:1px solid var(--border,#DDE5EE);border-radius:12px;padding:28px 22px 22px;cursor:pointer;transition:all .22s cubic-bezier(.4,0,.2,1);text-align:center;min-width:220px;max-width:280px;box-shadow:0 1px 3px rgba(0,0,0,.05)}
.trial-select-card:hover{transform:translateY(-4px);box-shadow:0 12px 32px rgba(27,79,107,.15);border-color:var(--teal,#2EC4A5)}
.trial-select-card:active{transform:translateY(-1px)}
"


# ═══════════════════════════════════════════════════════════════════════════
# LOGIN SCREEN — targets actual shinymanager DOM:
#   div.panel-auth  (outer wrapper, centred automatically)
#     div.row
#       div.col-sm-4.offset-md-4.col-sm-offset-4  (the form column)
#         form elements: #auth-user_id, #auth-user_pwd, #auth-go_auth
# ═══════════════════════════════════════════════════════════════════════════

auth_head <- tagList(
  tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
  tags$link(rel = "preconnect", href = "https://fonts.gstatic.com", crossorigin = NA),
  tags$link(rel = "stylesheet",
            href = "https://fonts.googleapis.com/css2?family=Outfit:wght@300;400;500;600;700;800&display=swap"),

  tags$style(HTML("
    /* ── Use body-level class (added by JS) for maximum compatibility ─── */
    html.tonic-login, body.tonic-login {
      height: 100%; margin: 0; padding: 0;
      font-family: 'Outfit', system-ui, sans-serif;
      color: #1B4F6B;
      background: linear-gradient(135deg,
        #1B4F6B 0%, #2EC4A5 35%, #7DDCC6 60%, #FFC773 85%, #F98D6F 100%) !important;
      background-size: 400% 400% !important;
      background-attachment: fixed !important;
      animation: tonicGrad 18s ease infinite !important;
      min-height: 100vh !important;
      overflow-x: hidden !important;
    }

    @keyframes tonicGrad { 0%,100%{background-position:0% 50%} 50%{background-position:100% 50%} }
    @keyframes tonicFloat1 { 0%,100%{transform:translate(0,0) scale(1)} 50%{transform:translate(-50px,70px) scale(1.1)} }
    @keyframes tonicFloat2 { 0%,100%{transform:translate(0,0) scale(1)} 50%{transform:translate(60px,-50px) scale(1.2)} }
    @keyframes tonicCardEnter { from{opacity:0;transform:translateY(24px) scale(0.96)} to{opacity:1;transform:translateY(0) scale(1)} }
    @keyframes tonicLogoPulse {
      0%,100% { transform:scale(1); box-shadow:0 14px 34px rgba(46,196,165,0.45), 0 0 0 6px rgba(46,196,165,0.10); }
      50%     { transform:scale(1.05); box-shadow:0 18px 42px rgba(249,141,111,0.45), 0 0 0 10px rgba(255,199,115,0.18); }
    }
    @keyframes tonicFadeUp { from{opacity:0;transform:translateY(10px)} to{opacity:1;transform:translateY(0)} }
    @keyframes tonicSpin { to { transform: rotate(360deg); } }

    /* Floating background blobs */
    body.tonic-login::before, body.tonic-login::after {
      content:''; position:fixed; border-radius:50%;
      filter:blur(80px); pointer-events:none; z-index:0;
    }
    body.tonic-login::before {
      width:520px; height:520px; background:#F98D6F;
      opacity:0.35; top:-140px; right:-100px;
      animation: tonicFloat1 16s ease-in-out infinite;
    }
    body.tonic-login::after {
      width:480px; height:480px; background:#FFC773;
      opacity:0.30; bottom:-120px; left:-80px;
      animation: tonicFloat2 20s ease-in-out infinite;
    }

    /* Centre the .panel-auth wrapper on the page */
    body.tonic-login .panel-auth {
      min-height: 100vh !important;
      width: 100vw !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      padding: 20px !important;
      position: relative !important;
      z-index: 10 !important;
    }

    /* Override the bootstrap row — it's our card wrapper */
    body.tonic-login .panel-auth .row {
      margin: 0 !important;
      width: 100% !important;
      max-width: 440px !important;
      display: flex !important;
      justify-content: center !important;
    }

    /* The bootstrap column is our actual card — style it */
    body.tonic-login .panel-auth .row > div {
      background: rgba(255,252,247,0.97) !important;
      backdrop-filter: blur(24px) saturate(130%);
      -webkit-backdrop-filter: blur(24px) saturate(130%);
      border: 1.5px solid rgba(255,255,255,0.6) !important;
      border-radius: 28px !important;
      box-shadow:
        0 30px 80px rgba(14,53,73,0.30),
        0 10px 30px rgba(27,79,107,0.18),
        0 0 0 1px rgba(255,255,255,0.5) inset !important;
      padding: 42px !important;
      width: 100% !important;
      max-width: 440px !important;
      margin: 0 !important;
      flex: 0 0 100% !important;
      position: relative;
      animation: tonicCardEnter 0.8s cubic-bezier(0.22,1,0.36,1) both;
    }

    /* Hide any BR tags polluting the .panel-auth */
    body.tonic-login .panel-auth > br { display: none !important; }

    /* Hide the default 'Please authenticate' h3 */
    body.tonic-login #auth-shinymanager-auth-head,
    body.tonic-login .panel-auth h3 { display: none !important; }

    /* Inject logo + welcome as first-child of the card column */
    body.tonic-login .panel-auth .row > div::before {
      content: 'Clinical Trials Dashboard';
      display: block; text-align: center;
      font-size: 22px; font-weight: 700; color: #1B4F6B;
      letter-spacing: -0.4px; line-height: 1.3;
      margin: 12px 0 4px;
      padding-top: 92px;
      background-image:
        url(\"data:image/svg+xml;utf8,<svg xmlns='http://www.w3.org/2000/svg' viewBox='0 0 24 24' fill='none' stroke='white' stroke-width='2.3' stroke-linecap='round' stroke-linejoin='round'><path d='M12 2L2 7l10 5 10-5-10-5z'/><path d='M2 17l10 5 10-5'/><path d='M2 12l10 5 10-5'/></svg>\"),
        linear-gradient(135deg, #1B4F6B 0%, #2EC4A5 100%);
      background-repeat: no-repeat, no-repeat;
      background-position: top 0 center, top 0 center;
      background-size: 40px 40px, 78px 78px;
      animation: tonicFadeUp 0.9s ease 0.15s both;
    }

    /* Tagline */
    body.tonic-login .panel-auth .row > div::after {
      content: 'Log in to start ✨';
      display: block; text-align: center;
      font-size: 14px; font-weight: 400; color: #6c7a86;
      margin: 0 0 24px;
      animation: tonicFadeUp 0.9s ease 0.25s both;
    }

    /* Wrap order: ::before, h3 (hidden), form-groups, button, ::after */
    /* That gives us: welcome heading, form, tagline at bottom. Move ::after up via grid */
    body.tonic-login .panel-auth .row > div {
      display: flex !important;
      flex-direction: column !important;
    }
    body.tonic-login .panel-auth .row > div::before { order: -3; }
    body.tonic-login .panel-auth .row > div::after  { order: -1; }
    body.tonic-login #auth-shinymanager-auth-head { order: -2; }

    /* Form labels */
    body.tonic-login .panel-auth label,
    body.tonic-login .panel-auth .control-label {
      font-size: 11.5px !important;
      font-weight: 600 !important;
      color: #1B4F6B !important;
      text-transform: uppercase !important;
      letter-spacing: 0.8px !important;
      margin-bottom: 7px !important;
      display: block !important;
    }

    /* Inputs */
    body.tonic-login #auth-user_id,
    body.tonic-login #auth-user_pwd,
    body.tonic-login .panel-auth .form-control {
      background: #FAF6EF !important;
      border: 2px solid #E8E0D2 !important;
      border-radius: 14px !important;
      padding: 14px 18px !important;
      font-size: 15px !important;
      color: #1B4F6B !important;
      height: auto !important;
      font-family: 'Outfit', sans-serif !important;
      font-weight: 500 !important;
      transition: all 0.22s cubic-bezier(0.4,0,0.2,1) !important;
      box-shadow: none !important;
      width: 100% !important;
      box-sizing: border-box !important;
    }
    body.tonic-login #auth-user_id::placeholder,
    body.tonic-login #auth-user_pwd::placeholder {
      color: #B5A894 !important; font-weight: 400 !important;
    }
    body.tonic-login #auth-user_id:focus,
    body.tonic-login #auth-user_pwd:focus,
    body.tonic-login .panel-auth .form-control:focus {
      background: white !important;
      border-color: #2EC4A5 !important;
      box-shadow: 0 0 0 5px rgba(46,196,165,0.18), 0 4px 14px rgba(46,196,165,0.10) !important;
      outline: none !important;
      transform: translateY(-1px);
    }
    body.tonic-login .panel-auth .form-group { margin-bottom: 18px !important; }

    /* Password eye toggle */
    body.tonic-login .tonic-pwd-wrap { position: relative; }
    body.tonic-login .tonic-pwd-wrap input { padding-right: 50px !important; }
    body.tonic-login .tonic-eye-btn {
      position: absolute; right: 14px; top: 50%;
      transform: translateY(-50%);
      background: none; border: none; cursor: pointer;
      padding: 6px; color: #B5A894;
      display: flex; align-items: center; justify-content: center;
      border-radius: 8px;
      transition: color 0.15s, background 0.15s;
      z-index: 5;
    }
    body.tonic-login .tonic-eye-btn:hover { color: #2EC4A5; background: rgba(46,196,165,0.08); }
    body.tonic-login .tonic-eye-btn svg { width: 19px; height: 19px; }

    /* Remember-me row */
    body.tonic-login .tonic-remember {
      display: flex; align-items: center; gap: 10px;
      margin: -4px 0 22px;
    }
    body.tonic-login .tonic-remember input[type='checkbox'] {
      width: 17px; height: 17px;
      accent-color: #2EC4A5;
      cursor: pointer; margin: 0;
    }
    body.tonic-login .tonic-remember label {
      margin: 0 !important;
      font-size: 13.5px !important;
      font-weight: 500 !important;
      color: #5c6b77 !important;
      text-transform: none !important;
      letter-spacing: 0 !important;
      cursor: pointer;
    }

    /* Submit button */
    body.tonic-login #auth-go_auth,
    body.tonic-login .panel-auth .btn,
    body.tonic-login .panel-auth button.action-button {
      background: linear-gradient(135deg, #1B4F6B 0%, #2EC4A5 55%, #F98D6F 100%) !important;
      background-size: 200% 200% !important;
      border: none !important;
      color: white !important;
      font-weight: 600 !important;
      font-size: 15.5px !important;
      padding: 14px !important;
      width: 100% !important;
      border-radius: 14px !important;
      letter-spacing: 0.3px !important;
      font-family: 'Outfit', sans-serif !important;
      transition: all 0.3s cubic-bezier(0.4,0,0.2,1) !important;
      box-shadow: 0 8px 20px rgba(46,196,165,0.35), 0 3px 10px rgba(27,79,107,0.15) !important;
      position: relative !important;
      overflow: hidden !important;
      cursor: pointer !important;
      animation: tonicGrad 6s ease infinite !important;
      text-shadow: none !important;
      margin-top: 8px !important;
    }
    body.tonic-login #auth-go_auth:hover,
    body.tonic-login .panel-auth .btn:hover {
      transform: translateY(-2px) !important;
      box-shadow: 0 14px 32px rgba(249,141,111,0.4), 0 6px 14px rgba(46,196,165,0.3) !important;
      background-position: right center !important;
    }

    /* Hide language/where selectors */
    body.tonic-login #auth-language,
    body.tonic-login [id*='shinymanager_where'],
    body.tonic-login [id*='shinymanager_language'],
    body.tonic-login #language-selectized,
    body.tonic-login [class*='selectize'] { display: none !important; }

    /* Error alerts */
    body.tonic-login .panel-auth .alert {
      background: #FFF0EC !important;
      border: 1.5px solid #FFCFBF !important;
      color: #B8421F !important;
      border-radius: 12px !important;
      padding: 11px 15px !important;
      font-size: 13.5px !important;
      font-weight: 500 !important;
      margin-top: 14px !important;
    }

    /* Footer */
    body.tonic-login .panel-auth::after {
      content: 'Birmingham Clinical Trials Unit · University of Birmingham';
      position: fixed;
      bottom: 20px; left: 0; right: 0;
      text-align: center;
      font-size: 12px;
      color: rgba(255,255,255,0.9);
      font-weight: 500; letter-spacing: 0.2px;
      text-shadow: 0 2px 10px rgba(0,0,0,0.25);
      z-index: 5;
      pointer-events: none;
    }

    body.tonic-login .btn-pwd, body.tonic-login .btn-copy-cred { display: none !important; }
  ")),

  # JavaScript enhancement - adds .tonic-login class so CSS applies
  tags$script(HTML("
    (function() {
      function enhance() {
        var userInput = document.getElementById('auth-user_id');
        var pwdInput  = document.getElementById('auth-user_pwd');
        var submitBtn = document.getElementById('auth-go_auth');

        if (!userInput || !pwdInput || !submitBtn) {
          setTimeout(enhance, 100); return;
        }
        if (document.body.dataset.tonic) return;
        document.body.dataset.tonic = '1';
        document.body.classList.add('tonic-login');
        document.documentElement.classList.add('tonic-login');

        userInput.setAttribute('placeholder', 'Your username');
        userInput.setAttribute('autocomplete', 'username');
        try {
          var saved = localStorage.getItem('tonic_remember_user');
          if (saved) userInput.value = saved;
        } catch(e) {}

        pwdInput.setAttribute('placeholder', 'Your password');
        pwdInput.setAttribute('autocomplete', 'current-password');

        if (!pwdInput.parentNode.classList.contains('tonic-pwd-wrap')) {
          var wrap = document.createElement('div');
          wrap.className = 'tonic-pwd-wrap';
          pwdInput.parentNode.insertBefore(wrap, pwdInput);
          wrap.appendChild(pwdInput);

          var eyeOpen  = '<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M1 12s4-8 11-8 11 8 11 8-4 8-11 8-11-8-11-8z\"/><circle cx=\"12\" cy=\"12\" r=\"3\"/></svg>';
          var eyeClose = '<svg viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"><path d=\"M17.94 17.94A10.07 10.07 0 0 1 12 20c-7 0-11-8-11-8a18.45 18.45 0 0 1 5.06-5.94M9.9 4.24A9.12 9.12 0 0 1 12 4c7 0 11 8 11 8a18.5 18.5 0 0 1-2.16 3.19m-6.72-1.07a3 3 0 1 1-4.24-4.24\"/><line x1=\"1\" y1=\"1\" x2=\"23\" y2=\"23\"/></svg>';

          var toggle = document.createElement('button');
          toggle.type = 'button';
          toggle.className = 'tonic-eye-btn';
          toggle.innerHTML = eyeOpen;
          toggle.addEventListener('click', function() {
            if (pwdInput.type === 'password') {
              pwdInput.type = 'text'; toggle.innerHTML = eyeClose;
            } else {
              pwdInput.type = 'password'; toggle.innerHTML = eyeOpen;
            }
          });
          wrap.appendChild(toggle);
        }

        if (!document.querySelector('.tonic-remember')) {
          var row = document.createElement('div');
          row.className = 'tonic-remember';
          row.innerHTML =
            '<input type=\"checkbox\" id=\"tonic_remember\"/>' +
            '<label for=\"tonic_remember\">Remember my username</label>';
          submitBtn.parentNode.insertBefore(row, submitBtn);

          try {
            if (localStorage.getItem('tonic_remember_user')) {
              document.getElementById('tonic_remember').checked = true;
            }
          } catch(e) {}
        }

        if (!submitBtn.dataset.tonicBound) {
          submitBtn.dataset.tonicBound = '1';
          submitBtn.addEventListener('click', function() {
            try {
              var rm = document.getElementById('tonic_remember');
              var u  = userInput.value || '';
              if (rm && rm.checked && u) {
                localStorage.setItem('tonic_remember_user', u);
              } else {
                localStorage.removeItem('tonic_remember_user');
              }
            } catch(e) {}
          });
        }

        setTimeout(function() {
          if (!userInput.value) userInput.focus();
          else pwdInput.focus();
        }, 400);
      }

      if (document.readyState === 'loading') {
        document.addEventListener('DOMContentLoaded', enhance);
      } else {
        enhance();
      }
    })();
  "))
)

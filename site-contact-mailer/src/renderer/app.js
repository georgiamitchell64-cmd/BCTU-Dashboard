'use strict';

/* global api */
// Renderer. No Node access here — everything privileged goes through the
// `api` bridge defined in preload.js.

const state = {
  sites: [],
  settings: {},
  templates: [],
  builtInFields: [],
  encryptionAvailable: false,
  mode: 'combined',
  search: '',
  statusFilter: new Set(),
  expanded: new Set(),
  planned: [],
  previewIndex: 0,
  importDraft: null,
};

const $ = (id) => document.getElementById(id);

// ── Helpers ───────────────────────────────────────────────────────────────

function escapeHtml(text) {
  return String(text ?? '')
    .replace(/&/g, '&amp;')
    .replace(/</g, '&lt;')
    .replace(/>/g, '&gt;')
    .replace(/"/g, '&quot;');
}

let toastTimer = null;
function toast(message, kind = '') {
  const el = $('toast');
  el.textContent = message;
  el.className = `toast ${kind}`;
  clearTimeout(toastTimer);
  toastTimer = setTimeout(() => el.classList.add('hidden'), kind === 'error' ? 7000 : 3500);
}

/** Unwrap the `{ok, data}` envelope every IPC handler returns. */
async function call(promise, { silent = false } = {}) {
  const result = await promise;
  if (!result) return null;
  if (result.ok) return result.data;
  if (!silent) toast(result.error || 'Something went wrong', 'error');
  return null;
}

function plural(count, singular, pluralForm) {
  return `${count} ${count === 1 ? singular : pluralForm || `${singular}s`}`;
}

function openModal(id) { $(id).classList.remove('hidden'); }
function closeModal(id) { $(id).classList.add('hidden'); }

// ── Selection ─────────────────────────────────────────────────────────────

function selectedSites() {
  return state.sites.filter((s) => s.selected && s.contacts.some((c) => c.selected !== false));
}

function activeContactCount(site) {
  return site.contacts.filter((c) => c.selected !== false).length;
}

function matchesFilters(site) {
  if (state.statusFilter.size > 0 && !state.statusFilter.has(site.status || 'Unknown')) return false;
  const query = state.search.trim().toLowerCase();
  if (!query) return true;
  const haystack = [
    site.siteName, site.siteId, site.status,
    ...site.contacts.map((c) => `${c.name} ${c.email} ${c.role}`),
  ].join(' ').toLowerCase();
  return haystack.includes(query);
}

let saveTimer = null;
function persistSites() {
  clearTimeout(saveTimer);
  // The contact tick boxes are edited rapidly; coalesce the writes.
  saveTimer = setTimeout(() => {
    api.saveSites(state.sites.map((site) => ({
      ...site,
      // `selected` on the site is a transient UI choice, not saved state.
      selected: undefined,
    })));
  }, 400);
}

// ── Site list ─────────────────────────────────────────────────────────────

function renderStatusFilters() {
  const counts = new Map();
  for (const site of state.sites) {
    const key = site.status || 'Unknown';
    counts.set(key, (counts.get(key) || 0) + 1);
  }
  const box = $('statusFilters');
  box.innerHTML = '';
  if (counts.size <= 1) return;

  // Statuses come from the spreadsheet, so colour the ones we recognise and
  // leave anything else neutral. Set as a class, not an inline style, because
  // the content security policy forbids inline styles.
  const knownStatuses = {
    recruiting: 'recruiting', open: 'recruiting', setup: 'setup', identified: 'setup',
    paused: 'paused', onhold: 'paused', closed: 'closed', completed: 'closed',
  };
  const dotClass = (status) => {
    const key = String(status).toLowerCase().replace(/[^a-z]/g, '');
    return knownStatuses[key] ? ` chip-dot--${knownStatuses[key]}` : '';
  };

  for (const [status, count] of [...counts.entries()].sort((a, b) => b[1] - a[1])) {
    const chip = document.createElement('button');
    chip.className = `chip${state.statusFilter.has(status) ? ' on' : ''}`;
    const dot = document.createElement('span');
    dot.className = `chip-dot${dotClass(status)}`;
    chip.append(dot, document.createTextNode(`${status} (${count})`));
    chip.addEventListener('click', () => {
      if (state.statusFilter.has(status)) state.statusFilter.delete(status);
      else state.statusFilter.add(status);
      renderSites();
    });
    box.appendChild(chip);
  }
}

function renderSites() {
  const list = $('siteList');
  const empty = $('sitesEmpty');

  if (state.sites.length === 0) {
    list.classList.add('hidden');
    empty.classList.remove('hidden');
    $('siteCount').textContent = '';
    renderRecipientBar();
    return;
  }
  list.classList.remove('hidden');
  empty.classList.add('hidden');

  const visible = state.sites.filter(matchesFilters);
  list.innerHTML = '';

  for (const site of visible) {
    const contacts = activeContactCount(site);
    const row = document.createElement('div');
    row.className = `site${site.selected ? ' is-selected' : ''}`;
    row.setAttribute('role', 'listitem');

    const main = document.createElement('div');
    main.className = 'site-main';

    const box = document.createElement('input');
    box.type = 'checkbox';
    box.checked = Boolean(site.selected);
    box.setAttribute('aria-label', `Select ${site.siteName}`);
    box.addEventListener('click', (event) => {
      event.stopPropagation();
      site.selected = box.checked;
      renderSites();
    });

    const text = document.createElement('div');
    text.className = 'site-text';
    const subParts = [site.siteId !== site.siteName ? site.siteId : '', site.status].filter(Boolean);
    text.innerHTML = `<div class="site-name">${escapeHtml(site.siteName)}</div>`
      + `<div class="site-sub">${escapeHtml(subParts.join(' · '))}</div>`;

    const badge = document.createElement('span');
    badge.className = `site-badge${contacts === 0 ? ' warn' : ''}`;
    badge.textContent = contacts === 0 ? 'no contacts' : plural(contacts, 'contact');

    const disclose = document.createElement('button');
    disclose.className = 'disclose';
    disclose.textContent = state.expanded.has(site.key) ? '▾' : '▸';
    disclose.setAttribute('aria-label', `Show contacts for ${site.siteName}`);
    disclose.addEventListener('click', (event) => {
      event.stopPropagation();
      if (state.expanded.has(site.key)) state.expanded.delete(site.key);
      else state.expanded.add(site.key);
      renderSites();
    });

    main.append(box, text, badge, disclose);
    main.addEventListener('click', () => {
      site.selected = !site.selected;
      renderSites();
    });
    row.appendChild(main);

    if (state.expanded.has(site.key)) {
      const holder = document.createElement('div');
      holder.className = 'contacts';
      if (site.contacts.length === 0) {
        holder.innerHTML = '<p class="muted">No email addresses were found for this site.</p>';
      }
      for (const contact of site.contacts) {
        const line = document.createElement('label');
        line.className = 'contact';
        const tick = document.createElement('input');
        tick.type = 'checkbox';
        tick.checked = contact.selected !== false;
        tick.addEventListener('change', () => {
          contact.selected = tick.checked;
          persistSites();
          renderSites();
        });
        const label = document.createElement('span');
        label.innerHTML = `${contact.name ? `${escapeHtml(contact.name)} ` : ''}`
          + `<span class="contact-email">${escapeHtml(contact.email)}</span>`
          + `${contact.role ? ` <span class="contact-role">${escapeHtml(contact.role)}</span>` : ''}`;
        line.append(tick, label);
        holder.appendChild(line);
      }
      row.appendChild(holder);
    }

    list.appendChild(row);
  }

  const ticked = state.sites.filter((s) => s.selected).length;
  const shown = visible.length === state.sites.length
    ? plural(state.sites.length, 'site')
    : `${visible.length} of ${state.sites.length} sites`;
  $('siteCount').textContent = ticked ? `${shown} · ${ticked} selected` : shown;

  renderStatusFilters();
  renderRecipientBar();
}

// ── Recipient summary and send button ─────────────────────────────────────

function renderRecipientBar() {
  const bar = $('recipientBar');
  const chosen = selectedSites();
  const button = $('btnSend');

  // Ticked, but every address is either missing or unticked, so nothing can
  // be sent to them. Say so rather than quietly leaving them out.
  const skipped = state.sites.filter((s) => s.selected && !chosen.includes(s));
  const skippedNote = skipped.length
    ? ` · <span class="warn-text">${plural(skipped.length, 'selected site has', 'selected sites have')} `
      + `no contacts and will be skipped</span>`
    : '';

  if (chosen.length === 0) {
    bar.innerHTML = skipped.length
      ? `<span class="warn-text">${plural(skipped.length, 'selected site has', 'selected sites have')} no email addresses, so there is nothing to send.</span>`
      : '<span class="muted">Select one or more sites to address this email.</span>';
    button.disabled = true;
    button.textContent = 'Select sites first';
    return;
  }

  const recipients = new Set();
  for (const site of chosen) {
    for (const contact of site.contacts) {
      if (contact.selected !== false) recipients.add(contact.email.toLowerCase());
    }
  }

  if (state.mode === 'merge') {
    const perContact = $('perContact').checked;
    const count = perContact ? recipients.size : chosen.length;
    bar.innerHTML = `<span class="pill">TO</span>`
      + `<strong>${plural(count, 'separate email')}</strong> — `
      + `${perContact ? 'one to each contact' : 'one per site, addressed to that site\'s contacts'}, `
      + `each with its own subject and message.${skippedNote}`;
    button.textContent = state.settings.deliveryMethod === 'smtp'
      ? `Send ${count}` : `Prepare ${count}`;
  } else {
    const useBcc = chosen.length > 1 || state.settings.forceBcc;
    const self = state.settings.senderAddress;
    if (useBcc) {
      bar.innerHTML = `<span class="pill bcc">BCC</span>`
        + `<strong>${plural(recipients.size, 'address', 'addresses')}</strong> across `
        + `${plural(chosen.length, 'site')}`
        + (state.settings.putSelfInTo && self
          ? ` · <span class="pill">TO</span>${escapeHtml(self)}`
          : ' · <span class="warn-text">no To address set — add yours in Settings</span>')
        + skippedNote;
    } else {
      bar.innerHTML = `<span class="pill">TO</span>`
        + `<strong>${plural(recipients.size, 'address', 'addresses')}</strong> at `
        + `${escapeHtml(chosen[0].siteName)}${skippedNote}`;
    }
    button.textContent = 'Send';
  }
  button.disabled = false;
}

// ── Compose ───────────────────────────────────────────────────────────────

function currentTemplate() {
  return { subject: $('subject').value, body: $('body').value };
}

function refreshPlaceholderPicker() {
  const picker = $('placeholderPicker');
  const extra = new Set();
  for (const site of state.sites) {
    for (const key of Object.keys(site.fields || {})) extra.add(key);
  }
  const builtIn = state.builtInFields.map((f) => f.key);
  for (const key of builtIn) extra.delete(key);

  picker.innerHTML = '<option value="">Insert field…</option>';
  const addGroup = (label, items) => {
    if (items.length === 0) return;
    const group = document.createElement('optgroup');
    group.label = label;
    for (const item of items) {
      const option = document.createElement('option');
      option.value = item.key;
      option.textContent = item.label;
      group.appendChild(option);
    }
    picker.appendChild(group);
  };
  addGroup('Standard', state.builtInFields);
  addGroup('From your spreadsheet', [...extra].sort().map((key) => ({ key, label: key })));
}

function insertAtCursor(field, text) {
  const start = field.selectionStart ?? field.value.length;
  const end = field.selectionEnd ?? field.value.length;
  field.value = field.value.slice(0, start) + text + field.value.slice(end);
  field.selectionStart = field.selectionEnd = start + text.length;
  field.focus();
}

function refreshTemplatePicker() {
  const picker = $('templatePicker');
  picker.innerHTML = '<option value="">Templates…</option>';
  for (const template of state.templates) {
    const option = document.createElement('option');
    option.value = template.id;
    option.textContent = template.name;
    picker.appendChild(option);
  }
}

function setMode(mode) {
  state.mode = mode;
  for (const button of document.querySelectorAll('.mode-btn')) {
    const active = button.dataset.mode === mode;
    button.classList.toggle('is-active', active);
    button.setAttribute('aria-checked', String(active));
  }
  $('mergeOptions').classList.toggle('hidden', mode !== 'merge');
  renderRecipientBar();
}

function sendPayload() {
  return {
    sites: selectedSites(),
    template: currentTemplate(),
    mode: state.mode,
    options: { perContact: $('perContact').checked },
  };
}

/** Block obvious mistakes before anything reaches a mail client. */
function validateBeforeSend() {
  const problems = [];
  const chosen = selectedSites();
  if (chosen.length === 0) problems.push('No sites are selected.');
  if (!$('subject').value.trim()) problems.push('The subject is empty.');
  if (!$('body').value.trim()) problems.push('The message is empty.');

  const combinedBcc = state.mode === 'combined' && (chosen.length > 1 || state.settings.forceBcc);
  if (combinedBcc && state.settings.putSelfInTo && !state.settings.senderAddress) {
    problems.push('Everyone will be bcc\'d but no To address is set. Add your email address in Settings.');
  }
  if (state.mode === 'combined' && $('body').value.includes('{{first_name}}')) {
    problems.push('{{first_name}} only works on a per-site email — on one combined email it will come out blank. Use "One per site", or {{first_name|colleagues}}.');
  }
  return problems;
}

function showComposeWarnings(messages) {
  const box = $('composeWarnings');
  if (messages.length === 0) {
    box.classList.add('hidden');
    box.innerHTML = '';
    return;
  }
  box.classList.remove('hidden');
  box.innerHTML = `<ul>${messages.map((m) => `<li>${escapeHtml(m)}</li>`).join('')}</ul>`;
}

async function doSend() {
  const problems = validateBeforeSend();
  showComposeWarnings(problems);
  if (problems.length > 0) {
    toast('Fix the highlighted problems first', 'error');
    return;
  }

  const payload = sendPayload();
  const method = $('deliveryMethod').value;
  const planned = await call(api.planMessages(payload));
  if (!planned) return;

  const missing = [...new Set(planned.flatMap((m) => m.missing))];
  if (missing.length > 0) {
    const ok = window.confirm(
      `These fields are empty for at least one site and will appear blank:\n\n`
      + `${missing.map((f) => `  {{${f}}}`).join('\n')}\n\n`
      + `Give them a fallback like {{${missing[0]}|the team}} to avoid this.\n\nCarry on anyway?`,
    );
    if (!ok) return;
  }

  if (planned.length > 5) {
    const verb = method === 'smtp' ? 'send' : 'prepare';
    if (!window.confirm(`This will ${verb} ${planned.length} separate emails. Continue?`)) return;
  }
  if (method === 'smtp' && !window.confirm(
    `This sends ${plural(planned.length, 'email')} immediately, with no chance to review them in your email app. Continue?`,
  )) return;

  $('btnSend').disabled = true;
  try {
    if (method === 'mailto') {
      const risky = planned.filter((m) => m.mailto.risky);
      if (risky.length && !window.confirm(
        'This message is long. Some email programs cut off long compose links, which would '
        + 'silently truncate your text.\n\nUse "Draft in my email app" instead for a reliable copy.\n\nOpen it anyway?',
      )) return;
      const result = await call(api.sendViaMailto(payload));
      if (result) toast(`Opened ${plural(result.opened, 'compose window')}`, 'success');
    } else if (method === 'eml') {
      const result = await call(api.createDrafts(payload));
      if (result && !result.cancelled) {
        toast(result.written === 1
          ? 'Draft opened in your email app — review it and press Send'
          : `${result.written} drafts saved to ${result.folder}`, 'success');
      }
    } else {
      const result = await call(api.sendViaSmtp(payload));
      if (result) {
        const message = result.failed
          ? `Sent ${result.sent}, failed ${result.failed}. See the list below.`
          : `Sent ${plural(result.sent, 'email')}`;
        toast(message, result.failed ? 'error' : 'success');
        if (result.failed) {
          showComposeWarnings(result.results.filter((r) => !r.ok).map((r) => `${r.site}: ${r.error}`));
        }
      }
    }
  } finally {
    $('btnSend').disabled = false;
    renderRecipientBar();
  }
}

// ── Preview ───────────────────────────────────────────────────────────────

function renderPreview() {
  const messages = state.planned;
  const index = Math.min(state.previewIndex, messages.length - 1);
  const message = messages[index];
  if (!message) return;

  $('previewPosition').textContent = `Email ${index + 1} of ${messages.length}`
    + (message.siteName ? ` — ${message.siteName}` : '');
  $('prevMessage').disabled = index === 0;
  $('nextMessage').disabled = index >= messages.length - 1;

  const line = (key, contacts) => (contacts && contacts.length
    ? `<div class="preview-header-row"><span class="k">${key}</span>`
      + `<span class="v">${escapeHtml(contacts.map((c) => (c.name ? `${c.name} <${c.email}>` : c.email)).join('; '))}</span></div>`
    : '');

  $('previewContent').innerHTML = `
    <div class="preview-headers">
      ${line('To', message.to)}
      ${line('Cc', message.cc)}
      ${line('Bcc', message.bcc)}
      <div class="preview-header-row"><span class="k">Subject</span>
        <span class="v">${escapeHtml(message.subject) || '<em>(empty)</em>'}</span></div>
    </div>
    <div class="preview-body">${escapeHtml(message.body)}</div>`;
}

async function openPreview() {
  const planned = await call(api.planMessages(sendPayload()));
  if (!planned || planned.length === 0) {
    toast('Nothing to preview — select a site first', 'error');
    return;
  }
  state.planned = planned;
  state.previewIndex = 0;
  renderPreview();
  openModal('previewModal');
}

// ── Import wizard ─────────────────────────────────────────────────────────

function currentSheet() {
  const draft = state.importDraft;
  return draft.workbook.sheets.find((s) => s.name === draft.sheetName);
}

function renderMappingControls() {
  const sheet = currentSheet();
  const mapping = state.importDraft.mapping;
  const headers = sheet.headers;

  $('layoutPicker').value = mapping.layout;
  $('layoutHint').textContent = mapping.layout === 'per-site'
    ? 'Each row is one site. Every email column below becomes a contact for that site.'
    : 'Each row is one person. Rows sharing a site name or ID are grouped into one site.';

  const select = (label, key, options, allowNone = true) => {
    const field = document.createElement('div');
    field.className = 'field';
    const id = `map_${key}`;
    field.innerHTML = `<label for="${id}">${escapeHtml(label)}</label>`;
    const picker = document.createElement('select');
    picker.id = id;
    if (allowNone) picker.innerHTML = '<option value="">— none —</option>';
    for (const header of options) {
      const option = document.createElement('option');
      option.value = header;
      option.textContent = header;
      if (mapping[key] === header) option.selected = true;
      picker.appendChild(option);
    }
    picker.addEventListener('change', () => {
      mapping[key] = picker.value || null;
      rebuildImportPreview();
    });
    field.appendChild(picker);
    return field;
  };

  const grid = $('mappingFields');
  grid.innerHTML = '';
  grid.append(
    select('Site ID / number', 'siteId', headers),
    select('Site name', 'siteName', headers),
    select('Site status (optional)', 'status', headers),
  );
  if (mapping.layout === 'per-contact') {
    grid.append(
      select('Contact name', 'contactName', headers),
      select('First name', 'firstName', headers),
      select('Surname', 'lastName', headers),
      select('Role / job title', 'role', headers),
    );
  }

  renderEmailColumns(headers);
}

function renderEmailColumns(headers) {
  const mapping = state.importDraft.mapping;
  const box = $('emailColumnBox');
  box.innerHTML = '<h3>Email columns</h3>';

  if (mapping.emailColumns.length === 0) {
    box.innerHTML += '<p class="hint">No column looked like an email address. '
      + 'Pick one below — a cell may hold several addresses separated by semicolons.</p>';
  }

  const rows = document.createElement('div');
  for (const spec of mapping.emailColumns) {
    const row = document.createElement('div');
    row.className = 'email-col-row';

    const tick = document.createElement('input');
    tick.type = 'checkbox';
    tick.checked = spec.enabled !== false;
    tick.title = 'Include this column';
    tick.addEventListener('change', () => { spec.enabled = tick.checked; rebuildImportPreview(); });

    const name = document.createElement('span');
    name.className = 'col-name';
    name.textContent = spec.column;

    row.append(tick, name);

    if (mapping.layout === 'per-site') {
      const role = document.createElement('input');
      role.type = 'text';
      role.value = spec.role || '';
      role.placeholder = 'Role';
      role.addEventListener('input', () => { spec.role = role.value; });

      const namePicker = document.createElement('select');
      namePicker.innerHTML = '<option value="">— name column —</option>';
      for (const header of headers) {
        const option = document.createElement('option');
        option.value = header;
        option.textContent = header;
        if (spec.nameColumn === header) option.selected = true;
        namePicker.appendChild(option);
      }
      namePicker.addEventListener('change', () => {
        spec.nameColumn = namePicker.value || null;
        rebuildImportPreview();
      });
      row.append(role, namePicker);
    }
    rows.appendChild(row);
  }
  box.appendChild(rows);

  // Let the user add a column the detector missed.
  const adder = document.createElement('select');
  adder.innerHTML = '<option value="">Add another email column…</option>';
  const used = new Set(mapping.emailColumns.map((c) => c.column));
  for (const header of headers.filter((h) => !used.has(h))) {
    const option = document.createElement('option');
    option.value = header;
    option.textContent = header;
    adder.appendChild(option);
  }
  adder.addEventListener('change', () => {
    if (!adder.value) return;
    mapping.emailColumns.push({
      column: adder.value,
      role: mapping.layout === 'per-site' ? adder.value.replace(/e-?mail/gi, '').trim() : null,
      nameColumn: null,
      enabled: true,
    });
    rebuildImportPreview();
  });
  adder.className = 'column-adder';
  box.appendChild(adder);
}

async function rebuildImportPreview() {
  const draft = state.importDraft;
  const result = await call(api.buildSites({ sheetName: draft.sheetName, mapping: draft.mapping }));
  if (!result) return;
  draft.result = result;

  const totalContacts = result.sites.reduce((sum, s) => sum + s.contacts.length, 0);
  $('importStats').textContent = `${plural(result.sites.length, 'site')}, ${plural(totalContacts, 'contact')}`;

  const table = document.createElement('table');
  table.innerHTML = '<thead><tr><th>Site</th><th>ID</th><th>Contacts</th></tr></thead>';
  const tbody = document.createElement('tbody');
  for (const site of result.sites.slice(0, 60)) {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${escapeHtml(site.siteName)}</td><td>${escapeHtml(site.siteId)}</td>`
      + `<td class="emails">${escapeHtml(site.contacts.map((c) => c.email).join(', ')) || '<span class="muted">none</span>'}</td>`;
    tbody.appendChild(tr);
  }
  table.appendChild(tbody);
  const holder = $('importPreview');
  holder.innerHTML = '';
  holder.appendChild(table);
  if (result.sites.length > 60) {
    const note = document.createElement('p');
    note.className = 'hint hint-inset';
    note.textContent = `Showing the first 60 of ${result.sites.length} sites.`;
    holder.appendChild(note);
  }

  const box = $('importWarnings');
  if (result.warnings.length === 0) {
    box.classList.add('hidden');
  } else {
    box.classList.remove('hidden');
    const shown = result.warnings.slice(0, 12);
    box.innerHTML = `<strong>${plural(result.warnings.length, 'row needs', 'rows need')} a look</strong>`
      + `<ul>${shown.map((w) => `<li>Row ${w.row}: ${escapeHtml(w.message)}</li>`).join('')}</ul>`
      + (result.warnings.length > shown.length ? `<p>…and ${result.warnings.length - shown.length} more.</p>` : '');
  }
  $('btnConfirmImport').disabled = result.sites.length === 0;
}

async function startImport() {
  const workbook = await call(api.chooseWorkbook());
  if (workbook) await openImportWizard(workbook);
}

async function openImportWizard(workbook) {
  state.importDraft = {
    workbook,
    sheetName: workbook.sheets[0].name,
    mapping: JSON.parse(JSON.stringify(workbook.sheets[0].mapping)),
  };

  const picker = $('sheetPicker');
  picker.innerHTML = '';
  for (const sheet of workbook.sheets) {
    const option = document.createElement('option');
    option.value = sheet.name;
    option.textContent = `${sheet.name} (${plural(sheet.rowCount, 'row')})`;
    picker.appendChild(option);
  }
  picker.disabled = workbook.sheets.length === 1;

  $('mergeExisting').checked = false;
  $('mergeExisting').parentElement.classList.toggle('hidden', state.sites.length === 0);

  renderMappingControls();
  await rebuildImportPreview();
  openModal('importModal');
}

async function confirmImport() {
  const draft = state.importDraft;
  if (!draft || !draft.result) return;

  const merged = await call(api.commitImport({
    sites: draft.result.sites,
    strategy: $('mergeExisting').checked ? 'merge' : 'replace',
    mapping: draft.mapping,
    meta: {
      fileName: draft.workbook.fileName,
      sheet: draft.sheetName,
      importedAt: new Date().toISOString(),
      siteCount: draft.result.sites.length,
    },
  }));
  if (!merged) return;

  state.sites = merged.map((site) => ({ ...site, selected: false }));
  state.expanded.clear();
  state.statusFilter.clear();
  closeModal('importModal');
  refreshPlaceholderPicker();
  renderSites();
  updateImportSummary({
    fileName: draft.workbook.fileName,
    siteCount: state.sites.length,
    importedAt: new Date().toISOString(),
  });
  toast(`Imported ${plural(state.sites.length, 'site')}`, 'success');
}

function updateImportSummary(meta) {
  if (!meta) {
    $('importSummary').textContent = 'No contact list imported';
    return;
  }
  const when = meta.importedAt ? new Date(meta.importedAt).toLocaleDateString('en-GB') : '';
  const contacts = state.sites.reduce((sum, s) => sum + s.contacts.length, 0);
  $('importSummary').textContent = `${meta.fileName || 'Saved list'} — `
    + `${plural(state.sites.length, 'site')}, ${plural(contacts, 'contact')}${when ? ` · ${when}` : ''}`;
}

// ── Settings ──────────────────────────────────────────────────────────────

function fillSettingsForm() {
  const s = state.settings;
  $('senderAddress').value = s.senderAddress || '';
  $('forceBcc').checked = Boolean(s.forceBcc);
  $('putSelfInTo').checked = s.putSelfInTo !== false;
  $('bodyFormat').value = s.bodyFormat || 'plain';
  $('smtpHost').value = (s.smtp && s.smtp.host) || '';
  $('smtpPort').value = (s.smtp && s.smtp.port) || 587;
  $('smtpSecure').checked = Boolean(s.smtp && s.smtp.secure);
  $('smtpUser').value = (s.smtp && s.smtp.user) || '';
  $('smtpPassword').value = '';
  $('smtpPasswordHint').textContent = !state.encryptionAvailable
    ? 'This system has no secure keychain available, so the password cannot be saved. You will need to re-enter it each session.'
    : (s.smtp && s.smtp.hasPassword)
      ? 'A password is saved, encrypted with your operating system keychain.'
      : 'Saved encrypted with your operating system keychain.';
}

async function saveSettings() {
  const patch = {
    senderAddress: $('senderAddress').value.trim(),
    forceBcc: $('forceBcc').checked,
    putSelfInTo: $('putSelfInTo').checked,
    bodyFormat: $('bodyFormat').value,
    deliveryMethod: $('deliveryMethod').value,
    smtp: {
      host: $('smtpHost').value.trim(),
      port: Number($('smtpPort').value) || 587,
      secure: $('smtpSecure').checked,
      user: $('smtpUser').value.trim(),
    },
  };
  const updated = await call(api.updateSettings(patch));
  if (!updated) return;

  state.settings = updated;

  const password = $('smtpPassword').value;
  if (password) {
    const result = await call(api.setSmtpPassword(password));
    if (result && !result.stored && result.reason === 'no-encryption') {
      toast('Settings saved, but the password could not be stored securely on this system', 'error');
    }
    // Storing the password flips smtp.hasPassword, so pick that up.
    const reloaded = await call(api.loadState(), { silent: true });
    if (reloaded) state.settings = reloaded.settings;
  }
  closeModal('settingsModal');
  renderRecipientBar();
  toast('Settings saved', 'success');
}

// ── Wiring ────────────────────────────────────────────────────────────────

function wireEvents() {
  $('btnImport').addEventListener('click', startImport);
  $('btnImportEmpty').addEventListener('click', startImport);
  api.onImportRequested(startImport);

  $('btnSettings').addEventListener('click', () => { fillSettingsForm(); openModal('settingsModal'); });
  $('btnSaveSettings').addEventListener('click', saveSettings);
  $('btnVerifySmtp').addEventListener('click', async () => {
    await saveSettingsQuietly();
    const result = await call(api.verifySmtp());
    if (result) toast('Connected to the mail server', 'success');
  });

  for (const button of document.querySelectorAll('[data-close]')) {
    button.addEventListener('click', () => closeModal(button.dataset.close));
  }
  document.addEventListener('keydown', (event) => {
    if (event.key !== 'Escape') return;
    for (const modal of document.querySelectorAll('.modal:not(.hidden)')) modal.classList.add('hidden');
  });

  $('siteSearch').addEventListener('input', (event) => {
    state.search = event.target.value;
    renderSites();
  });
  $('btnSelectAll').addEventListener('click', () => {
    for (const site of state.sites) site.selected = true;
    renderSites();
  });
  $('btnSelectNone').addEventListener('click', () => {
    for (const site of state.sites) site.selected = false;
    renderSites();
  });
  $('btnSelectFiltered').addEventListener('click', () => {
    for (const site of state.sites) if (matchesFilters(site)) site.selected = true;
    renderSites();
  });

  for (const button of document.querySelectorAll('.mode-btn')) {
    button.addEventListener('click', () => setMode(button.dataset.mode));
  }
  $('perContact').addEventListener('change', renderRecipientBar);

  $('placeholderPicker').addEventListener('change', (event) => {
    if (!event.target.value) return;
    insertAtCursor($('body'), `{{${event.target.value}}}`);
    event.target.value = '';
  });

  $('templatePicker').addEventListener('change', (event) => {
    const template = state.templates.find((t) => t.id === event.target.value);
    if (!template) return;
    $('subject').value = template.subject;
    $('body').value = template.body;
    event.target.value = '';
    toast(`Loaded "${template.name}"`);
  });

  $('btnSaveTemplate').addEventListener('click', async () => {
    const name = window.prompt('Save this subject and message as a template called:');
    if (!name) return;
    const saved = await call(api.saveTemplate({ name, ...currentTemplate() }));
    if (!saved) return;
    const reloaded = await call(api.loadState(), { silent: true });
    if (reloaded) state.templates = reloaded.templates;
    refreshTemplatePicker();
    toast(`Saved "${saved.name}"`, 'success');
  });

  $('deliveryMethod').addEventListener('change', async (event) => {
    state.settings.deliveryMethod = event.target.value;
    await call(api.updateSettings({ deliveryMethod: event.target.value }), { silent: true });
    renderRecipientBar();
  });

  $('btnSend').addEventListener('click', doSend);
  $('btnPreview').addEventListener('click', openPreview);
  $('btnSendFromPreview').addEventListener('click', () => {
    closeModal('previewModal');
    doSend();
  });
  $('prevMessage').addEventListener('click', () => {
    state.previewIndex = Math.max(0, state.previewIndex - 1);
    renderPreview();
  });
  $('nextMessage').addEventListener('click', () => {
    state.previewIndex = Math.min(state.planned.length - 1, state.previewIndex + 1);
    renderPreview();
  });

  $('btnCopy').addEventListener('click', async () => {
    const chosen = selectedSites();
    if (chosen.length === 0) { toast('Select some sites first', 'error'); return; }
    const addresses = [...new Set(chosen.flatMap((s) => s.contacts
      .filter((c) => c.selected !== false)
      .map((c) => c.email)))];
    await call(api.copyToClipboard(addresses.join('; ')));
    toast(`Copied ${plural(addresses.length, 'address', 'addresses')}`, 'success');
  });

  $('btnExport').addEventListener('click', async () => {
    if (state.sites.length === 0) { toast('Nothing to export', 'error'); return; }
    const rows = [['Site ID', 'Site name', 'Status', 'Contact name', 'Email', 'Role']];
    for (const site of state.sites) {
      for (const contact of site.contacts) {
        rows.push([site.siteId, site.siteName, site.status || '', contact.name, contact.email, contact.role]);
      }
    }
    const result = await call(api.exportCsv({ rows, suggestedName: 'site-contacts.csv' }));
    if (result && !result.cancelled) toast('Exported', 'success');
  });

  $('sheetPicker').addEventListener('change', (event) => {
    const draft = state.importDraft;
    draft.sheetName = event.target.value;
    const sheet = currentSheet();
    draft.mapping = JSON.parse(JSON.stringify(sheet.mapping));
    renderMappingControls();
    rebuildImportPreview();
  });

  $('layoutPicker').addEventListener('change', (event) => {
    const draft = state.importDraft;
    draft.mapping.layout = event.target.value;
    // Roles come from the column headers in the per-site layout and from a
    // role column in the per-contact layout, so reset them on the switch.
    for (const spec of draft.mapping.emailColumns) {
      spec.role = event.target.value === 'per-site'
        ? spec.column.replace(/e-?mail\s*(address)?/gi, '').trim() || 'Contact'
        : null;
    }
    renderMappingControls();
    rebuildImportPreview();
  });

  $('btnConfirmImport').addEventListener('click', confirmImport);

  api.onSendProgress(({ done, total, site }) => {
    $('btnSend').textContent = `Sending ${done}/${total}…`;
    if (site && done === total) toast(`Finished sending ${total}`, 'success');
  });
}

/** Persist the settings form without closing the dialog (used by Test). */
async function saveSettingsQuietly() {
  await call(api.updateSettings({
    senderAddress: $('senderAddress').value.trim(),
    smtp: {
      host: $('smtpHost').value.trim(),
      port: Number($('smtpPort').value) || 587,
      secure: $('smtpSecure').checked,
      user: $('smtpUser').value.trim(),
    },
  }), { silent: true });
  if ($('smtpPassword').value) await call(api.setSmtpPassword($('smtpPassword').value), { silent: true });
}

async function init() {
  wireEvents();

  const loaded = await call(api.loadState());
  if (loaded) {
    state.settings = loaded.settings;
    state.templates = loaded.templates || [];
    state.builtInFields = loaded.builtInFields || [];
    state.encryptionAvailable = loaded.encryptionAvailable;
    state.sites = (loaded.sites || []).map((site) => ({ ...site, selected: false }));
    $('deliveryMethod').value = state.settings.deliveryMethod || 'eml';
    updateImportSummary(loaded.lastImport);
  }

  refreshTemplatePicker();
  refreshPlaceholderPicker();
  setMode('combined');
  renderSites();

  const info = await call(api.appInfo(), { silent: true });
  if (info) $('appInfo').textContent = `Version ${info.version} · data stored in ${info.userData}`;
}

init();

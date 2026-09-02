'use strict';

/* global api, scmHtml, RichTextEditor */
// Renderer. No Node access here — everything privileged goes through the
// `api` bridge defined in preload.js.

const state = {
  sites: [],
  settings: {},
  templates: [],
  drafts: [],
  activeDraftId: null,
  builtInFields: [],
  builtInTemplates: [],
  recruitmentFields: [],
  recruitment: null,
  recruitmentImport: null,
  recruitmentDraft: null,
  encryptionAvailable: false,
  // 'one' | 'site' | 'person' drives the UI switch; `mode`/`perContact` are
  // the shape the backend (shared/compose.js) actually expects.
  sendMode: 'one',
  mode: 'combined',
  perContact: false,
  search: '',
  statusFilter: new Set(),
  // Empty means "everyone at the selected sites". Adding roles narrows the
  // send to just those people — e.g. only PIs, or only R&D.
  roleFilter: new Set(),
  expanded: new Set(),
  attachments: [],
  planned: [],
  previewIndex: 0,
  previewTab: 'formatted',
  importDraft: null,
  drawerOpen: true,
};

let editor = null;

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

/**
 * Ask for a single line of text.
 *
 * Electron does not implement window.prompt() — it throws "prompt() is and
 * will not be supported" — so every such request goes through this dialog.
 *
 * @returns {Promise<string|null>} the trimmed text, or null if cancelled.
 */
function askText(message, defaultValue = '', options = {}) {
  const { title = 'Site Contact Mailer', okLabel = 'OK' } = options;
  return new Promise((resolve) => {
    const modal = $('promptModal');
    const input = $('promptInput');
    const cancels = [...modal.querySelectorAll('[data-prompt-cancel]')];
    $('promptTitle').textContent = title;
    $('promptMessage').textContent = message;
    $('promptOk').textContent = okLabel;
    input.value = defaultValue;

    const finish = (value) => {
      modal.classList.add('hidden');
      $('promptOk').removeEventListener('click', onOk);
      input.removeEventListener('keydown', onKey);
      for (const button of cancels) button.removeEventListener('click', onCancel);
      resolve(value);
    };
    const onOk = () => {
      const value = input.value.trim();
      finish(value === '' ? null : value);
    };
    const onCancel = () => finish(null);
    const onKey = (event) => {
      if (event.key === 'Enter') { event.preventDefault(); onOk(); }
      if (event.key === 'Escape') { event.preventDefault(); onCancel(); }
    };

    $('promptOk').addEventListener('click', onOk);
    input.addEventListener('keydown', onKey);
    for (const button of cancels) button.addEventListener('click', onCancel);

    modal.classList.remove('hidden');
    input.focus();
    input.select();
  });
}

// The editor is a separate script and needs the same dialog.
window.scmPrompt = askText;

const STATUS_DOTS = {
  recruiting: '#12a47b', open: '#12a47b', setup: '#838190', identified: '#838190',
  paused: '#d99a22', onhold: '#d99a22', closed: '#7c8b96', completed: '#7c8b96',
};
function statusDot(status) {
  const key = String(status || '').toLowerCase().replace(/[^a-z]/g, '');
  return STATUS_DOTS[key] || '#9c9aa8';
}

// ── Selection ─────────────────────────────────────────────────────────────

function roleOf(contact) {
  return contact.roleGroup || contact.role || 'Unspecified';
}

/** Does this contact fall inside the current role filter and stay ticked? */
function contactIncluded(contact) {
  if (contact.selected === false) return false;
  if (state.roleFilter.size === 0) return true;
  return state.roleFilter.has(roleOf(contact));
}

function activeContacts(site) {
  return site.contacts.filter(contactIncluded);
}

function activeContactCount(site) {
  return activeContacts(site).length;
}

/** Sites that are ticked and still have someone to write to. */
function selectedSites() {
  return state.sites.filter((s) => s.selected && activeContactCount(s) > 0);
}

/**
 * The site list as the send should see it: only contacts inside the role
 * filter. The filter is a per-send choice, so it is applied to a copy rather
 * than by unticking people in the stored list.
 */
function payloadSites() {
  return selectedSites().map((site) => ({
    ...site,
    contacts: activeContacts(site).map((c) => ({ ...c, selected: true })),
  }));
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

let saveSitesTimer = null;
function persistSites() {
  clearTimeout(saveSitesTimer);
  // The contact tick boxes are edited rapidly; coalesce the writes.
  saveSitesTimer = setTimeout(() => {
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

  const dotClass = (status) => {
    const key = String(status).toLowerCase().replace(/[^a-z]/g, '');
    const known = ['recruiting', 'open', 'setup', 'identified', 'paused', 'onhold', 'closed', 'completed'];
    return known.includes(key) ? ` chip-dot--${{ open: 'recruiting', identified: 'setup', onhold: 'paused', completed: 'closed' }[key] || key}` : '';
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

function renderRoleFilters() {
  const box = $('roleFilters');
  const counts = new Map();
  for (const site of state.sites) {
    for (const contact of site.contacts) {
      if (contact.selected === false) continue;
      const role = roleOf(contact);
      counts.set(role, (counts.get(role) || 0) + 1);
    }
  }

  box.innerHTML = '';
  const roles = [...counts.entries()].sort((a, b) => b[1] - a[1] || a[0].localeCompare(b[0]));
  if (roles.length <= 1) {
    // Nothing to choose between; hide the control rather than show one chip.
    $('roleFilters').closest('.role-filter').classList.add('hidden');
    return;
  }
  $('roleFilters').closest('.role-filter').classList.remove('hidden');

  for (const [role, count] of roles) {
    const on = state.roleFilter.has(role);
    const chip = document.createElement('button');
    chip.className = `chip${on ? ' role-on' : ''}`;
    chip.title = on ? `Only writing to ${role}` : `Include ${role}`;
    chip.textContent = `${role} (${count})`;
    chip.addEventListener('click', () => {
      if (on) state.roleFilter.delete(role);
      else state.roleFilter.add(role);
      renderSites();
      scheduleDraftSave();
    });
    box.appendChild(chip);
  }

  $('btnAllRoles').classList.toggle('hidden', state.roleFilter.size === 0);
}

function renderSites() {
  const list = $('siteList');
  const empty = $('sitesEmpty');

  if (state.sites.length === 0) {
    list.classList.add('hidden');
    empty.classList.remove('hidden');
    renderRecipients();
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
      scheduleDraftSave();
    });

    const text = document.createElement('div');
    text.className = 'site-text';
    const subParts = [site.siteId !== site.siteName ? site.siteId : '', site.status].filter(Boolean);
    text.innerHTML = `<div class="site-name">${escapeHtml(site.siteName)}</div>`
      + `<div class="site-sub">${escapeHtml(subParts.join(' · '))}</div>`;

    const badge = document.createElement('span');
    badge.className = `site-badge${contacts === 0 ? ' warn' : ''}`;
    if (contacts > 0) {
      badge.textContent = plural(contacts, 'contact');
    } else if (state.roleFilter.size > 0 && site.contacts.some((c) => c.selected !== false)) {
      // It has contacts, just nobody in the roles being written to — saying
      // "no contacts" here would be wrong and alarming.
      badge.textContent = `no ${[...state.roleFilter].join('/')}`;
      badge.title = 'This site has contacts, but none in the roles you are sending to.';
    } else {
      badge.textContent = 'no contacts';
    }

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
      scheduleDraftSave();
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
        // Ticked but outside the role filter: shown dimmed so it is obvious
        // why they are not receiving this particular email.
        const excludedByRole = contact.selected !== false && !contactIncluded(contact);
        line.className = `contact${excludedByRole ? ' is-role-excluded' : ''}`;
        if (excludedByRole) line.title = `Not in the "Send to" selection (${roleOf(contact)})`;

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

  renderStatusFilters();
  renderRoleFilters();
  renderRecipients();
}

// ── Recipient summary, right rail and the send button ─────────────────────

function computeRecipientState() {
  const chosen = selectedSites();
  const ticked = state.sites.filter((s) => s.selected);
  const skipped = ticked.filter((s) => !chosen.includes(s));
  const recipients = new Set();
  for (const site of chosen) {
    for (const contact of activeContacts(site)) recipients.add(contact.email.toLowerCase());
  }
  const useBcc = state.sendMode === 'one' && (chosen.length > 1 || state.settings.forceBcc);
  return { chosen, ticked, skipped, recipients, useBcc };
}

/** The To/Bcc chips above the subject line — what will this send address? */
function renderRecipientChips(ticked, useBcc) {
  const bar = $('recipientSummary');
  bar.innerHTML = '';

  if (state.sites.length === 0) {
    const span = document.createElement('span');
    span.className = 'recipient-empty';
    span.textContent = 'Import a contact list to begin.';
    bar.appendChild(span);
    return;
  }

  if (ticked.length > 0) {
    const isTo = state.sendMode !== 'one' || !useBcc;
    const pill = document.createElement('span');
    pill.className = `addr-pill ${isTo ? 'to' : 'bcc'}`;
    pill.textContent = isTo ? 'TO' : 'BCC';
    bar.appendChild(pill);
  }

  const chipsBox = document.createElement('div');
  chipsBox.className = 'recipient-chips';
  if (ticked.length === 0) {
    const span = document.createElement('span');
    span.className = 'recipient-empty';
    span.textContent = 'Select one or more sites to address this email.';
    chipsBox.appendChild(span);
  } else {
    for (const site of ticked) {
      const n = activeContactCount(site);
      const chip = document.createElement('span');
      chip.className = `recipient-chip${n === 0 ? ' no-address' : ''}`;

      const dot = document.createElement('span');
      dot.className = 'dot';
      dot.style.background = statusDot(site.status);

      const label = document.createElement('span');
      label.textContent = `${site.siteId} · ${site.siteName.split(',')[0]}`;

      const tail = document.createElement('span');
      tail.className = 'tail';
      tail.textContent = n === 0 ? 'no address' : `×${n}`;

      const drop = document.createElement('span');
      drop.className = 'drop';
      drop.textContent = '×';
      drop.title = `Remove ${site.siteName}`;
      drop.addEventListener('click', (event) => {
        event.stopPropagation();
        site.selected = false;
        renderSites();
        scheduleDraftSave();
      });

      chip.append(dot, label, tail, drop);
      chipsBox.appendChild(chip);
    }
  }
  bar.appendChild(chipsBox);

  const toggle = document.createElement('button');
  toggle.type = 'button';
  toggle.className = `drawer-toggle${state.drawerOpen ? ' is-open' : ''}`;
  toggle.innerHTML = `${state.drawerOpen ? 'Hide recipients' : (ticked.length ? '+ Add sites' : 'Choose recipients')} `
    + '<span class="caret">▾</span>';
  toggle.addEventListener('click', () => {
    state.drawerOpen = !state.drawerOpen;
    $('recipientDrawer').classList.toggle('is-collapsed', !state.drawerOpen);
    renderRecipientChips(ticked, useBcc);
  });
  bar.appendChild(toggle);
}

/** The "Who gets this" stats and addressing note in the right rail. */
function renderSummaryRail(chosen, skipped, recipients, useBcc) {
  $('statSites').textContent = chosen.length;
  $('statPeople').textContent = recipients.size;

  const note = $('addressingNote');
  const self = state.settings.senderAddress;
  const roleNote = state.roleFilter.size
    ? ` · <span>${escapeHtml([...state.roleFilter].join(', '))} only</span>` : '';

  if (state.sites.length === 0) {
    note.innerHTML = '<span class="muted">Import a contact list to begin.</span>';
  } else if (chosen.length === 0) {
    note.innerHTML = '<span class="muted">Select sites to see how this will be addressed.</span>';
  } else if (state.sendMode !== 'one') {
    const count = state.sendMode === 'person' ? recipients.size : chosen.length;
    note.innerHTML = `<span class="addr-pill to">TO</span>${plural(count, 'separate email')} — each addressed to `
      + `${state.sendMode === 'person' ? 'one person' : "that site's own contacts"}.${roleNote}`;
  } else if (useBcc) {
    note.innerHTML = `<span class="addr-pill bcc">BCC</span>${plural(recipients.size, 'address', 'addresses')} across `
      + `${plural(chosen.length, 'site')}`
      + (state.settings.putSelfInTo && self
        ? ' · your address is in To.'
        : ' · <span style="color: var(--amber-ink);">no To address set — add yours in Settings.</span>')
      + roleNote;
  } else {
    note.innerHTML = `<span class="addr-pill to">TO</span>${plural(recipients.size, 'address', 'addresses')} at `
      + `${escapeHtml(chosen[0].siteName)}.${roleNote}`;
  }

  const skipEl = $('skipWarning');
  if (skipped.length === 0) {
    skipEl.classList.add('hidden');
    skipEl.innerHTML = '';
  } else {
    skipEl.classList.remove('hidden');
    const reason = state.roleFilter.size ? 'nobody in those roles' : 'no usable address';
    skipEl.innerHTML = '<svg width="14" height="14" viewBox="0 0 24 24" fill="none" stroke="currentColor" '
      + 'stroke-width="1.9" stroke-linecap="round" stroke-linejoin="round"><path d="M12 3.5 21 19.5H3z"></path>'
      + '<path d="M12 9.5v4M12 16.5h.01"></path></svg>'
      + `<span>${plural(skipped.length, 'selected site has', 'selected sites have')} ${reason} and will be skipped `
      + `(${escapeHtml(skipped.map((s) => s.siteName.split(',')[0]).join(', '))}).</span>`;
  }
}

function updateSendButton(chosen, recipients) {
  const button = $('btnSend');
  const modeCount = $('modeCount');
  if (chosen.length === 0) {
    button.disabled = true;
    button.textContent = 'Select sites first';
    modeCount.textContent = '';
    return;
  }
  button.disabled = false;
  if (state.sendMode === 'one') {
    button.textContent = 'Send';
    modeCount.textContent = '';
  } else {
    const count = state.sendMode === 'person' ? recipients.size : chosen.length;
    button.textContent = state.settings.deliveryMethod === 'smtp' ? `Send ${count}` : `Prepare ${count}`;
    modeCount.textContent = `${plural(count, 'email')} to prepare`;
  }
}

function renderRecipients() {
  const { chosen, ticked, skipped, recipients, useBcc } = computeRecipientState();
  renderRecipientChips(ticked, useBcc);
  renderSummaryRail(chosen, skipped, recipients, useBcc);
  updateSendButton(chosen, recipients);
  scheduleLivePreview();
}

// ── Live preview (right rail) ──────────────────────────────────────────────

let livePreviewTimer = null;
function scheduleLivePreview() {
  clearTimeout(livePreviewTimer);
  livePreviewTimer = setTimeout(renderLivePreview, 350);
}

async function renderLivePreview() {
  const box = $('livePreview');
  const pos = $('livePreviewPos');
  const chosen = selectedSites();
  if (chosen.length === 0) {
    pos.textContent = '';
    box.innerHTML = '<p class="muted" style="padding: 12px;">Select a site to see a live preview here.</p>';
    return;
  }

  const planned = await call(api.planMessages(sendPayload()), { silent: true });
  if (!planned || planned.length === 0) return;
  const message = planned[0];
  pos.textContent = planned.length === 1 ? '1 of 1' : `1 of ${planned.length}`;

  const addressed = (message.to && message.to.length) ? message.to : (message.bcc || []);
  const label = (message.to && message.to.length) ? 'To' : 'Bcc';
  const names = addressed.map((c) => (c.name ? `${c.name} <${c.email}>` : c.email));
  const addressLine = names.length > 2
    ? `${names.slice(0, 2).join(', ')}, +${names.length - 2}`
    : (names.join(', ') || '—');

  const body = (!message.bodyHtml)
    ? `<div class="live-preview-body">${escapeHtml(message.body).split('\n\n').map((p) => `<p>${p}</p>`).join('')}</div>`
    : `<div class="live-preview-body">${scmHtml.sanitizeHtml(message.bodyHtml)}</div>`;

  box.innerHTML = `
    <div class="live-preview-headers">
      <div class="row"><span class="k">Subject</span> ${escapeHtml(message.subject) || '<em>(empty)</em>'}</div>
      <div class="row"><span class="k">${label}</span> ${escapeHtml(addressLine)}</div>
    </div>
    ${body}`;
}

// ── Compose ───────────────────────────────────────────────────────────────

function currentTemplate() {
  const bodyHtml = editor.getHtml();
  return {
    subject: $('subject').value,
    bodyHtml,
    // The plain-text alternative always travels with the HTML.
    body: editor.getText(),
  };
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
  // Only offered once there is data behind them, so the menu never advertises
  // a chart that would come out blank.
  if (state.recruitment && state.recruitment.sites && state.recruitment.sites.length) {
    addGroup('Recruitment', state.recruitmentFields);
  }
  addGroup('From your spreadsheet', [...extra].sort().map((key) => ({ key, label: key })));
}

function renderTemplateList() {
  const list = $('templateList');
  list.innerHTML = '';

  // `state.builtInTemplates` only arrives non-empty once there is recruitment
  // data to fill its charts — see availableBuiltIns() in shared/templates.js.
  const addGroup = (label, templates, deletable) => {
    if (templates.length === 0) return;
    const heading = document.createElement('div');
    heading.className = 'template-group-label';
    heading.textContent = label;
    list.appendChild(heading);

    for (const template of templates) {
      const item = document.createElement('div');
      item.className = 'template-item';

      const name = document.createElement('span');
      name.textContent = template.name;
      item.appendChild(name);

      if (deletable) {
        const remove = document.createElement('button');
        remove.className = 'template-remove';
        remove.textContent = '×';
        remove.title = `Delete "${template.name}"`;
        remove.addEventListener('click', async (event) => {
          event.stopPropagation();
          if (!window.confirm(`Delete template "${template.name}"?`)) return;
          const remaining = await call(api.deleteTemplate(template.id));
          if (!remaining) return;
          state.templates = remaining;
          renderTemplateList();
        });
        item.appendChild(remove);
      }

      item.addEventListener('click', () => {
        $('subject').value = template.subject;
        // Templates saved before rich text carry only a plain body.
        editor.setHtml(template.bodyHtml || scmHtml.textToHtmlFragment(template.body || ''));
        renderRecipients();
        scheduleDraftSave();
        toast(`Loaded "${template.name}"`);
      });
      list.appendChild(item);
    }
  };

  addGroup('Ready-made', state.builtInTemplates, false);
  addGroup('Saved', state.templates, true);
  $('templatesEmpty').classList.toggle('hidden', state.builtInTemplates.length > 0 || state.templates.length > 0);
}

/** `one` | `site` | `person` — the design's three-way switch. Internally
 *  this still drives `mode`/`perContact` the way shared/compose.js expects:
 *  a combined email, or a merge queue with one message per site or per
 *  contact. */
function setSendMode(key, { skipSave = false } = {}) {
  state.sendMode = key;
  state.mode = key === 'one' ? 'combined' : 'merge';
  state.perContact = key === 'person';
  for (const button of document.querySelectorAll('.mode-btn')) {
    const active = button.dataset.mode === key;
    button.classList.toggle('is-active', active);
    button.setAttribute('aria-checked', String(active));
  }
  renderRecipients();
  if (!skipSave) scheduleDraftSave();
}

function sendPayload() {
  return {
    sites: payloadSites(),
    template: currentTemplate(),
    mode: state.mode,
    options: {
      perContact: state.perContact,
      roles: [...state.roleFilter],
      cc: $('ccField').value,
      attachments: state.attachments,
    },
  };
}

/** Block obvious mistakes before anything reaches a mail client. */
function validateBeforeSend() {
  const problems = [];
  const chosen = selectedSites();
  if (chosen.length === 0) problems.push('No sites are selected.');
  if (!$('subject').value.trim()) problems.push('The subject is empty.');
  if (editor.isEmpty()) problems.push('The message is empty.');

  const combinedBcc = state.mode === 'combined' && (chosen.length > 1 || state.settings.forceBcc);
  if (combinedBcc && state.settings.putSelfInTo && !state.settings.senderAddress) {
    problems.push('Everyone will be bcc\'d but no To address is set. Add your email address in Settings.');
  }
  const bodyText = editor.getText();
  if (state.sendMode === 'one' && bodyText.includes('{{first_name}}')) {
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

/** A send is done with this draft — drop it from the sidebar and start fresh,
 *  the way sending an Outlook draft clears it out of the Drafts folder. */
async function retireActiveDraft() {
  if (state.activeDraftId) {
    const remaining = await call(api.deleteDraft(state.activeDraftId), { silent: true });
    if (remaining) state.drafts = remaining;
  }
  await startNewDraft();
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
      if (result) {
        toast(`Opened ${plural(result.opened, 'compose window')}`, 'success');
        await retireActiveDraft();
      }
    } else if (method === 'eml') {
      const result = await call(api.createDrafts(payload));
      if (result && !result.cancelled) {
        toast(result.written === 1
          ? 'Draft opened in your email app — review it and press Send'
          : `${result.written} drafts saved to ${result.folder}`, 'success');
        await retireActiveDraft();
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
        } else {
          await retireActiveDraft();
        }
      }
    }
  } finally {
    $('btnSend').disabled = false;
    renderRecipients();
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

  const attachmentLine = state.attachments.length
    ? `<div class="preview-header-row"><span class="k">Attached</span><span class="v">`
      + `${escapeHtml(state.attachments.map((a) => a.fileName).join(', '))}</span></div>`
    : '';

  // An HTML message is shown as it will render, with the plain-text
  // alternative one click away — that is what recipients on restricted
  // clients will actually see.
  const showText = state.previewTab === 'text' || !message.bodyHtml;
  const tabs = message.bodyHtml
    ? `<div class="preview-tabs">
         <button class="preview-tab${showText ? '' : ' is-on'}" data-tab="formatted">Formatted</button>
         <button class="preview-tab${showText ? ' is-on' : ''}" data-tab="text">Plain text</button>
       </div>`
    : '';

  const body = showText
    ? `<div class="preview-body">${escapeHtml(message.body)}</div>`
    : `<div class="preview-body is-html">${scmHtml.sanitizeHtml(message.bodyHtml)}</div>`;

  $('previewContent').innerHTML = `
    <div class="preview-headers">
      ${line('To', message.to)}
      ${line('Cc', message.cc)}
      ${line('Bcc', message.bcc)}
      <div class="preview-header-row"><span class="k">Subject</span>
        <span class="v">${escapeHtml(message.subject) || '<em>(empty)</em>'}</span></div>
      ${attachmentLine}
    </div>
    ${tabs}
    ${body}`;

  for (const tab of $('previewContent').querySelectorAll('.preview-tab')) {
    tab.addEventListener('click', () => {
      state.previewTab = tab.dataset.tab;
      renderPreview();
    });
  }
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

// ── Drafts ──────────────────────────────────────────────────────────────

function draftPayloadFromState() {
  return {
    id: state.activeDraftId,
    subject: $('subject').value,
    bodyHtml: editor.getHtml(),
    body: editor.getText(),
    cc: $('ccField').value,
    sendMode: state.sendMode,
    roles: [...state.roleFilter],
    siteKeys: state.sites.filter((s) => s.selected).map((s) => s.key),
  };
}

let draftSaveTimer = null;
function scheduleDraftSave() {
  clearTimeout(draftSaveTimer);
  draftSaveTimer = setTimeout(saveActiveDraftNow, 700);
}

/** Persist the draft immediately, bypassing the debounce — used before
 *  switching away from it, so nothing typed in the last second is lost. */
async function flushDraftSave() {
  if (!draftSaveTimer) return;
  clearTimeout(draftSaveTimer);
  draftSaveTimer = null;
  await saveActiveDraftNow();
}

async function saveActiveDraftNow() {
  const payload = draftPayloadFromState();
  const isEmpty = !payload.subject.trim() && editor.isEmpty() && payload.siteKeys.length === 0;
  // Don't create a record out of a message nobody has actually started.
  if (!state.activeDraftId && isEmpty) return;
  const saved = await call(api.saveDraft(payload), { silent: true });
  if (!saved) return;
  state.activeDraftId = saved.id;
  const index = state.drafts.findIndex((d) => d.id === saved.id);
  if (index >= 0) state.drafts[index] = saved;
  else state.drafts.unshift(saved);
  renderDrafts();
}

function renderDrafts() {
  const list = $('draftList');
  list.innerHTML = '';
  const sorted = [...state.drafts].sort((a, b) => new Date(b.updatedAt) - new Date(a.updatedAt));

  for (const draft of sorted) {
    const item = document.createElement('div');
    item.className = `draft-item${draft.id === state.activeDraftId ? ' is-active' : ''}`;

    const title = document.createElement('div');
    title.className = 'draft-title';
    title.textContent = draft.subject.trim() || 'New email';

    const sub = document.createElement('div');
    sub.className = 'draft-sub';
    const when = new Date(draft.updatedAt).toLocaleDateString('en-GB', { day: 'numeric', month: 'short' });
    sub.textContent = `${plural(draft.siteKeys.length, 'site')} · ${when}`;

    const remove = document.createElement('button');
    remove.className = 'draft-remove';
    remove.textContent = '×';
    remove.title = 'Delete draft';
    remove.addEventListener('click', (event) => removeDraft(draft.id, event));

    item.append(title, sub, remove);
    item.addEventListener('click', () => openDraft(draft.id));
    list.appendChild(item);
  }
  $('draftsEmpty').classList.toggle('hidden', sorted.length > 0);
}

/** Clear the compose area for a message that has not been saved as a draft
 *  yet — it becomes one the moment there is something worth keeping. */
async function startNewDraft() {
  await flushDraftSave();
  state.activeDraftId = null;
  $('subject').value = '';
  $('ccField').value = '';
  editor.setHtml('');
  if (editor.sourceMode) $('sourceBody').value = '';
  state.attachments = [];
  renderAttachments();
  for (const site of state.sites) site.selected = false;
  state.roleFilter.clear();
  state.expanded.clear();
  showComposeWarnings([]);
  setSendMode('one', { skipSave: true });
  renderSites();
  renderDrafts();
  $('subject').focus();
}

async function openDraft(id) {
  if (id === state.activeDraftId) return;
  await flushDraftSave();
  const draft = state.drafts.find((d) => d.id === id);
  if (!draft) return;

  state.activeDraftId = draft.id;
  $('subject').value = draft.subject || '';
  $('ccField').value = draft.cc || '';
  editor.setHtml(draft.bodyHtml || scmHtml.textToHtmlFragment(draft.body || ''));
  state.roleFilter = new Set(draft.roles || []);
  const keys = new Set(draft.siteKeys || []);
  for (const site of state.sites) site.selected = keys.has(site.key);
  state.expanded.clear();
  showComposeWarnings([]);
  setSendMode(draft.sendMode || 'one', { skipSave: true });
  renderSites();
  renderDrafts();
}

async function removeDraft(id, event) {
  event.stopPropagation();
  if (!window.confirm('Delete this draft?')) return;
  const remaining = await call(api.deleteDraft(id));
  if (!remaining) return;
  state.drafts = remaining;
  if (state.activeDraftId === id) {
    state.activeDraftId = null;
    const next = [...state.drafts].sort((a, b) => new Date(b.updatedAt) - new Date(a.updatedAt))[0];
    if (next) await openDraft(next.id);
    else await startNewDraft();
  } else {
    renderDrafts();
  }
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

// ── Recruitment data ──────────────────────────────────────────────────────

function recruitmentSheet() {
  const draft = state.recruitmentDraft;
  return draft.workbook.sheets.find((s) => s.name === draft.sheetName);
}

function renderRecruitmentMapping() {
  const draft = state.recruitmentDraft;
  const mapping = draft.mapping;
  const headers = recruitmentSheet().headers;
  $('recLayoutPicker').value = mapping.layout;

  const select = (label, key, options) => {
    const field = document.createElement('div');
    field.className = 'field';
    const id = `rec_${key}`;
    field.innerHTML = `<label for="${id}">${escapeHtml(label)}</label>`;
    const picker = document.createElement('select');
    picker.id = id;
    picker.innerHTML = '<option value="">— none —</option>';
    for (const header of options) {
      const option = document.createElement('option');
      option.value = header;
      option.textContent = header;
      if (mapping[key] === header) option.selected = true;
      picker.appendChild(option);
    }
    picker.addEventListener('change', () => {
      mapping[key] = picker.value || null;
      rebuildRecruitmentPreview();
    });
    field.appendChild(picker);
    return field;
  };

  const grid = $('recMappingFields');
  grid.innerHTML = '';
  grid.append(select('Site name', 'siteName', headers), select('Site ID (optional)', 'siteId', headers));

  if (mapping.layout === 'participant') {
    grid.append(select('Randomisation date', 'date', headers));
  } else if (mapping.layout === 'site-month') {
    grid.append(select('Month', 'month', headers), select('Number randomised', 'count', headers));
  } else if (mapping.layout === 'site-total') {
    grid.append(select('Number randomised', 'count', headers));
  } else {
    grid.append(select('Total column (optional)', 'count', headers),
      select('Date opened (optional)', 'opened', headers));
  }
  grid.append(select('Target (optional)', 'target', headers));

  if (mapping.layout === 'site-month-wide') {
    const note = document.createElement('p');
    note.className = 'hint';
    const found = (mapping.monthColumns || []).length;
    note.textContent = found
      ? `${found} month columns detected: ${mapping.monthColumns.map((c) => c.column).join(', ')}`
      : 'No month columns detected — check the layout is right.';
    grid.appendChild(note);
  }
}

async function rebuildRecruitmentPreview() {
  const draft = state.recruitmentDraft;
  const result = await call(api.buildRecruitment({ sheetName: draft.sheetName, mapping: draft.mapping }));
  if (!result) return;
  draft.result = result;

  $('recStats').textContent = `${plural(result.sites.length, 'site')}, `
    + `${result.totals.randomised} randomised`
    + (result.months.length ? `, ${plural(result.months.length, 'month')}` : '');

  const ranked = [...result.sites].sort((a, b) => a.rank - b.rank);
  const table = document.createElement('table');
  table.innerHTML = '<thead><tr><th>#</th><th>Site</th><th>Randomised</th><th>Target</th></tr></thead>';
  const tbody = document.createElement('tbody');
  for (const site of ranked.slice(0, 40)) {
    const tr = document.createElement('tr');
    tr.innerHTML = `<td>${site.rank}</td><td>${escapeHtml(site.siteName)}</td>`
      + `<td>${site.randomised}</td><td>${site.target ?? '<span class="muted">—</span>'}</td>`;
    tbody.appendChild(tr);
  }
  table.appendChild(tbody);
  $('recPreview').innerHTML = '';
  $('recPreview').appendChild(table);

  const warn = $('recWarnings');
  if (result.warnings.length === 0) {
    warn.classList.add('hidden');
  } else {
    warn.classList.remove('hidden');
    const shown = result.warnings.slice(0, 10);
    warn.innerHTML = `<strong>${plural(result.warnings.length, 'row needs', 'rows need')} a look</strong>`
      + `<ul>${shown.map((w) => `<li>Row ${w.row}: ${escapeHtml(w.message)}</li>`).join('')}</ul>`;
  }

  // Recruitment rows are matched to the contact list by site ID then name;
  // an unmatched site silently gets no chart, so it is called out here.
  const match = $('recMatch');
  const unmatched = (result.match && result.match.unmatched) || [];
  const noTarget = result.sites.every((s) => !s.target);
  const notes = [];
  if (unmatched.length) {
    notes.push(`${plural(unmatched.length, 'site')} in your contact list have no recruitment row and `
      + `will get no charts: ${unmatched.slice(0, 6).join(', ')}${unmatched.length > 6 ? '…' : ''}`);
  }
  if (noTarget) {
    notes.push('No target column found. The progress-against-target chart will fall back to a '
      + '"Target" column in your contact list, if there is one.');
  }
  if (notes.length === 0) {
    match.classList.add('hidden');
  } else {
    match.classList.remove('hidden');
    match.innerHTML = `<ul>${notes.map((n) => `<li>${escapeHtml(n)}</li>`).join('')}</ul>`;
  }

  $('btnConfirmRecruitment').disabled = result.sites.length === 0;
}

async function chooseRecruitmentFile() {
  const workbook = await call(api.chooseRecruitment());
  if (!workbook) return;

  state.recruitmentDraft = {
    workbook,
    sheetName: workbook.sheets[0].name,
    mapping: JSON.parse(JSON.stringify(workbook.sheets[0].mapping)),
  };

  const picker = $('recSheetPicker');
  picker.innerHTML = '';
  for (const sheet of workbook.sheets) {
    const option = document.createElement('option');
    option.value = sheet.name;
    option.textContent = `${sheet.name} (${plural(sheet.rowCount, 'row')})`;
    picker.appendChild(option);
  }
  picker.disabled = workbook.sheets.length === 1;
  $('recruitmentIntro').textContent = `Reading ${workbook.fileName}.`;

  renderRecruitmentMapping();
  await rebuildRecruitmentPreview();
}

function updateRecruitmentSummary() {
  const meta = state.recruitmentImport;
  const button = $('btnImportRecruitment');
  if (state.recruitment && state.recruitment.sites && state.recruitment.sites.length) {
    button.textContent = `Recruitment: ${state.recruitment.sites.length} sites`;
    button.title = meta && meta.fileName
      ? `From ${meta.fileName}. Click to replace.`
      : 'Click to replace the imported recruitment data.';
  } else {
    button.textContent = 'Recruitment data…';
    button.title = 'Import randomisation data for the recruitment charts';
  }
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
  renderRecipients();
  toast('Settings saved', 'success');
}

// ── Wiring ────────────────────────────────────────────────────────────────

// ── Editor, toolbar and attachments ───────────────────────────────────────

function renderAttachments() {
  const box = $('attachmentList');
  box.innerHTML = '';
  for (const [index, file] of state.attachments.entries()) {
    const chip = document.createElement('span');
    chip.className = 'attachment';
    const kb = file.size / 1024;
    const size = kb >= 1024 ? `${(kb / 1024).toFixed(1)} MB` : `${Math.max(1, Math.round(kb))} KB`;
    chip.append(
      document.createTextNode(file.fileName),
      Object.assign(document.createElement('span'), { className: 'attachment-size', textContent: size }),
    );
    const remove = document.createElement('button');
    remove.textContent = '×';
    remove.title = `Remove ${file.fileName}`;
    remove.addEventListener('click', () => {
      state.attachments.splice(index, 1);
      renderAttachments();
    });
    chip.appendChild(remove);
    box.appendChild(chip);
  }
}

/** Reflect the caret's current formatting in the toolbar buttons. */
function syncToolbarState() {
  for (const button of document.querySelectorAll('.tb-btn[data-command]')) {
    const command = button.dataset.command;
    button.classList.toggle('is-on', editor.queryState(command));
  }
}

function wireEditor() {
  editor = new RichTextEditor($('editorBody'), {
    onChange: () => {
      renderRecipients();
      scheduleDraftSave();
    },
  });

  document.addEventListener('selectionchange', () => {
    if (document.activeElement === $('editorBody')) syncToolbarState();
  });

  // Formatting buttons are declared in the HTML with data-command, so one
  // handler covers all of them.
  for (const button of document.querySelectorAll('.tb-btn[data-command]')) {
    button.addEventListener('mousedown', (event) => event.preventDefault()); // keep the selection
    button.addEventListener('click', () => {
      editor.exec(button.dataset.command);
      syncToolbarState();
    });
  }

  for (const picker of document.querySelectorAll('.tb-select[data-style]')) {
    picker.addEventListener('change', () => {
      if (!picker.value) return;
      editor.exec(picker.dataset.style, picker.value);
      picker.value = '';
    });
  }

  $('foreColour').addEventListener('input', (event) => editor.exec('foreColor', event.target.value));
  $('backColour').addEventListener('input', (event) => editor.exec('hiliteColor', event.target.value));
  $('btnLink').addEventListener('click', () => editor.promptForLink());

  $('btnSourceMode').addEventListener('click', () => {
    const goingToSource = !editor.sourceMode;
    editor.setSourceMode(goingToSource, $('sourceBody'));
    $('editorBody').classList.toggle('hidden', goingToSource);
    $('sourceBody').classList.toggle('hidden', !goingToSource);
    $('btnSourceMode').classList.toggle('is-on', goingToSource);
    for (const control of document.querySelectorAll('.tb-btn[data-command], .tb-select, .tb-colour')) {
      control.classList.toggle('hidden', goingToSource);
    }
    editor.focus();
  });

  $('sourceBody').addEventListener('input', () => {
    renderRecipients();
    scheduleDraftSave();
  });

  $('btnLoadHtml').addEventListener('click', async () => {
    const loaded = await call(api.loadHtmlBody());
    if (!loaded) return;
    if (!editor.isEmpty()
      && !window.confirm('Replace the message you have written with the contents of this file?')) return;
    editor.setHtml(loaded.html);
    if (editor.sourceMode) $('sourceBody').value = loaded.html;
    toast(`Loaded ${loaded.fileName}`, 'success');
  });

  $('btnAttach').addEventListener('click', async () => {
    const files = await call(api.chooseAttachments());
    if (!files || files.length === 0) return;
    state.attachments.push(...files);
    renderAttachments();
    toast(`Attached ${plural(files.length, 'file')}`, 'success');
  });
}

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

  $('btnNewDraft').addEventListener('click', startNewDraft);

  $('siteSearch').addEventListener('input', (event) => {
    state.search = event.target.value;
    renderSites();
  });
  $('btnSelectAll').addEventListener('click', () => {
    for (const site of state.sites) site.selected = true;
    renderSites();
    scheduleDraftSave();
  });
  $('btnSelectNone').addEventListener('click', () => {
    for (const site of state.sites) site.selected = false;
    renderSites();
    scheduleDraftSave();
  });
  $('btnSelectFiltered').addEventListener('click', () => {
    for (const site of state.sites) if (matchesFilters(site)) site.selected = true;
    renderSites();
    scheduleDraftSave();
  });

  for (const button of document.querySelectorAll('.mode-btn')) {
    button.addEventListener('click', () => setSendMode(button.dataset.mode));
  }

  $('subject').addEventListener('input', () => {
    renderRecipients();
    scheduleDraftSave();
  });
  $('ccField').addEventListener('input', () => {
    renderRecipients();
    scheduleDraftSave();
  });

  $('placeholderPicker').addEventListener('change', (event) => {
    if (!event.target.value) return;
    editor.insertPlaceholder(event.target.value);
    event.target.value = '';
  });

  $('btnAllRoles').addEventListener('click', () => {
    state.roleFilter.clear();
    renderSites();
    scheduleDraftSave();
  });

  $('btnSaveTemplate').addEventListener('click', async () => {
    const name = await askText('Save this subject and message as a template called:', '', {
      title: 'Save template', okLabel: 'Save',
    });
    if (!name) return;
    const saved = await call(api.saveTemplate({ name, ...currentTemplate() }));
    if (!saved) return;
    const reloaded = await call(api.loadState(), { silent: true });
    if (reloaded) state.templates = reloaded.templates;
    renderTemplateList();
    toast(`Saved "${saved.name}"`, 'success');
  });

  $('deliveryMethod').addEventListener('change', async (event) => {
    state.settings.deliveryMethod = event.target.value;
    await call(api.updateSettings({ deliveryMethod: event.target.value }), { silent: true });
    renderRecipients();
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
    const addresses = [...new Set(chosen.flatMap((s) => activeContacts(s).map((c) => c.email)))];
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

  $('btnImportRecruitment').addEventListener('click', async () => {
    openModal('recruitmentModal');
    if (!state.recruitmentDraft) await chooseRecruitmentFile();
  });
  $('btnChooseRecruitment').addEventListener('click', chooseRecruitmentFile);
  $('recSheetPicker').addEventListener('change', (event) => {
    const draft = state.recruitmentDraft;
    draft.sheetName = event.target.value;
    draft.mapping = JSON.parse(JSON.stringify(recruitmentSheet().mapping));
    renderRecruitmentMapping();
    rebuildRecruitmentPreview();
  });
  $('recLayoutPicker').addEventListener('change', (event) => {
    state.recruitmentDraft.mapping.layout = event.target.value;
    renderRecruitmentMapping();
    rebuildRecruitmentPreview();
  });
  $('btnConfirmRecruitment').addEventListener('click', async () => {
    const draft = state.recruitmentDraft;
    if (!draft || !draft.result) return;
    const { match, ...dataset } = draft.result;
    const saved = await call(api.commitRecruitment({
      dataset,
      meta: {
        fileName: draft.workbook.fileName,
        sheet: draft.sheetName,
        importedAt: new Date().toISOString(),
      },
    }));
    if (!saved) return;
    state.recruitment = saved.recruitment;
    state.recruitmentImport = { fileName: draft.workbook.fileName, importedAt: new Date().toISOString() };
    closeModal('recruitmentModal');
    refreshPlaceholderPicker();
    updateRecruitmentSummary();
    toast(`Recruitment data loaded for ${plural(saved.recruitment.sites.length, 'site')}`, 'success');
  });
  $('btnClearRecruitment').addEventListener('click', async () => {
    if (!window.confirm('Remove the imported recruitment data? Charts will stop working until you import again.')) return;
    await call(api.clearRecruitment());
    state.recruitment = null;
    state.recruitmentImport = null;
    state.recruitmentDraft = null;
    closeModal('recruitmentModal');
    refreshPlaceholderPicker();
    updateRecruitmentSummary();
    toast('Recruitment data removed');
  });

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
  wireEditor();
  wireEvents();

  const loaded = await call(api.loadState());
  if (loaded) {
    state.settings = loaded.settings;
    state.templates = loaded.templates || [];
    state.drafts = loaded.drafts || [];
    state.builtInFields = loaded.builtInFields || [];
    state.recruitmentFields = loaded.recruitmentFields || [];
    state.builtInTemplates = loaded.builtInTemplates || [];
    state.recruitment = loaded.recruitment || null;
    state.recruitmentImport = loaded.recruitmentImport || null;
    state.encryptionAvailable = loaded.encryptionAvailable;
    state.sites = (loaded.sites || []).map((site) => ({ ...site, selected: false }));
    $('deliveryMethod').value = state.settings.deliveryMethod || 'eml';
    updateImportSummary(loaded.lastImport);
  }

  renderTemplateList();
  refreshPlaceholderPicker();
  renderAttachments();
  updateRecruitmentSummary();
  renderDrafts();

  // Resume the most recently touched draft, the way Outlook reopens on
  // whatever you were last writing, rather than a blank compose window.
  const mostRecent = [...state.drafts].sort((a, b) => new Date(b.updatedAt) - new Date(a.updatedAt))[0];
  if (mostRecent) {
    await openDraft(mostRecent.id);
  } else {
    setSendMode('one', { skipSave: true });
    renderSites();
  }

  const info = await call(api.appInfo(), { silent: true });
  if (info) $('appInfo').textContent = `Version ${info.version} · data stored in ${info.userData}`;
}

init();

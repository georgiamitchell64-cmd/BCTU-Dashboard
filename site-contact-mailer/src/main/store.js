'use strict';

// Small JSON-file store in the per-user app data directory. Holds the imported
// site list, saved templates and settings, so the spreadsheet only has to be
// imported once.

const fs = require('fs');
const path = require('path');

const {
  applyCompletenessHistory, snapshotCompleteness, HISTORY_LIMIT,
} = require('../shared/completeness');

const DEFAULT_SETTINGS = {
  senderAddress: '',
  // Combined sends put everyone in Bcc once more than one site is selected;
  // this forces Bcc even for a single site.
  forceBcc: false,
  // With everyone in Bcc the message still needs a To, so the sender's own
  // address goes there.
  putSelfInTo: true,
  bodyFormat: 'html',
  deliveryMethod: 'eml',
  // Sites are named in the ranked charts. Turn this on to show everyone but
  // the recipient as "Site A", "Site B" — for a trial that does not publish a
  // named league table.
  anonymiseOtherSites: false,
  // 'all' | 'top3' (name the leading three only) | 'none'.
  completenessNaming: 'all',
  draftFolder: '',
  smtp: {
    host: '',
    port: 587,
    secure: false,
    user: '',
    // The password is never written here in the clear; see credentials.enc.
    hasPassword: false,
  },
};

// 2: rich-text messages. Stores written by version 1 default bodyFormat to
// 'plain', which would now silently strip the formatting the user just typed.
// 3: named sites in the ranked charts. Version 2 stored the old default of
// true, which was never exposed in the UI and so was never a choice.
const CURRENT_VERSION = 3;

const DEFAULT_STATE = {
  version: CURRENT_VERSION,
  // Imported randomisation data, normalised by shared/recruitment.js.
  recruitment: null,
  recruitmentImport: null,
  // Imported return-rate data, normalised by shared/completeness.js, plus the
  // snapshots that make "up two places since last month" possible.
  completeness: null,
  completenessImport: null,
  completenessHistory: [],
  settings: DEFAULT_SETTINGS,
  sites: [],
  templates: [],
  drafts: [],
  lastImport: null,
  lastMapping: null,
};

class Store {
  /**
   * @param {string} directory Usually `app.getPath('userData')`.
   * @param {object} [safeStorage] Electron's safeStorage, when available.
   */
  constructor(directory, safeStorage = null) {
    this.directory = directory;
    this.safeStorage = safeStorage;
    this.file = path.join(directory, 'data.json');
    this.credentialsFile = path.join(directory, 'credentials.enc');
    this.state = this.load();
  }

  load() {
    try {
      const raw = fs.readFileSync(this.file, 'utf8');
      const parsed = JSON.parse(raw);
      const state = {
        ...DEFAULT_STATE,
        ...parsed,
        settings: {
          ...DEFAULT_SETTINGS,
          ...(parsed.settings || {}),
          smtp: { ...DEFAULT_SETTINGS.smtp, ...((parsed.settings || {}).smtp || {}) },
        },
      };

      if (!parsed.version || parsed.version < 2) {
        // The old default was plain text, chosen when there was no formatting
        // to lose. Carrying it forward would throw away every rich message.
        state.settings.bodyFormat = 'html';
      }
      if (!parsed.version || parsed.version < 3) {
        state.settings.anonymiseOtherSites = false;
      }
      state.version = CURRENT_VERSION;
      return state;
    } catch (error) {
      if (error.code !== 'ENOENT') {
        // A corrupt file should not stop the app opening; keep the bad copy
        // aside so nothing the user imported is silently destroyed.
        try {
          fs.renameSync(this.file, `${this.file}.corrupt-${Date.now()}`);
        } catch { /* nothing more we can do */ }
      }
      return JSON.parse(JSON.stringify(DEFAULT_STATE));
    }
  }

  save() {
    fs.mkdirSync(this.directory, { recursive: true });
    // Write-then-rename so an interrupted write cannot truncate the store.
    const temporary = `${this.file}.tmp`;
    fs.writeFileSync(temporary, JSON.stringify(this.state, null, 2), 'utf8');
    fs.renameSync(temporary, this.file);
  }

  getSettings() {
    return this.state.settings;
  }

  updateSettings(patch) {
    this.state.settings = {
      ...this.state.settings,
      ...patch,
      smtp: { ...this.state.settings.smtp, ...(patch.smtp || {}) },
    };
    this.save();
    return this.state.settings;
  }

  getSites() {
    return this.state.sites;
  }

  setSites(sites, meta = null) {
    this.state.sites = sites;
    if (meta) this.state.lastImport = meta;
    this.save();
    return this.state.sites;
  }

  setLastMapping(mapping) {
    this.state.lastMapping = mapping;
    this.save();
  }

  getRecruitment() {
    return this.state.recruitment;
  }

  setRecruitment(dataset, meta = null) {
    this.state.recruitment = dataset;
    this.state.recruitmentImport = meta;
    this.save();
    return this.state.recruitment;
  }

  clearRecruitment() {
    this.state.recruitment = null;
    this.state.recruitmentImport = null;
    this.save();
  }

  getCompleteness() {
    return this.state.completeness;
  }

  /**
   * Store a completeness dataset, appending it to the history first so the
   * movement fields compare against the *previous* import, not this one.
   */
  setCompleteness(dataset, meta = null) {
    const history = this.state.completenessHistory || [];
    const importedAt = (meta && meta.importedAt) || new Date().toISOString();
    const annotated = applyCompletenessHistory(dataset, history);
    const snapshot = snapshotCompleteness(annotated, importedAt);
    this.state.completenessHistory = [...history, snapshot].slice(-HISTORY_LIMIT);
    this.state.completeness = annotated;
    this.state.completenessImport = meta;
    this.save();
    return this.state.completeness;
  }

  clearCompleteness({ keepHistory = true } = {}) {
    this.state.completeness = null;
    this.state.completenessImport = null;
    if (!keepHistory) this.state.completenessHistory = [];
    this.save();
  }

  getTemplates() {
    return this.state.templates;
  }

  saveTemplate(template) {
    const id = template.id || `tpl_${Date.now()}`;
    const record = {
      id,
      name: template.name || 'Untitled',
      subject: template.subject || '',
      // `body` is the plain-text alternative; `bodyHtml` is the formatted
      // message. Templates saved before rich text have only the former.
      body: template.body || '',
      bodyHtml: template.bodyHtml || '',
      updatedAt: new Date().toISOString(),
    };
    const index = this.state.templates.findIndex((t) => t.id === id);
    if (index >= 0) this.state.templates[index] = record;
    else this.state.templates.push(record);
    this.save();
    return record;
  }

  deleteTemplate(id) {
    this.state.templates = this.state.templates.filter((t) => t.id !== id);
    this.save();
    return this.state.templates;
  }

  getDrafts() {
    return this.state.drafts || [];
  }

  /**
   * Create or update an in-progress message, the way Outlook's Drafts folder
   * holds several messages you haven't sent yet. `id` is supplied by the
   * renderer so rapid autosaves of the same draft update one record instead
   * of piling up new ones.
   */
  saveDraft(draft) {
    if (!this.state.drafts) this.state.drafts = [];
    const id = draft.id || `draft_${Date.now()}`;
    const record = {
      id,
      subject: draft.subject || '',
      bodyHtml: draft.bodyHtml || '',
      body: draft.body || '',
      cc: draft.cc || '',
      sendMode: draft.sendMode || 'one',
      roles: Array.isArray(draft.roles) ? draft.roles : [],
      siteKeys: Array.isArray(draft.siteKeys) ? draft.siteKeys : [],
      createdAt: draft.createdAt || new Date().toISOString(),
      updatedAt: new Date().toISOString(),
    };
    const index = this.state.drafts.findIndex((d) => d.id === id);
    if (index >= 0) this.state.drafts[index] = record;
    else this.state.drafts.unshift(record);
    this.save();
    return record;
  }

  deleteDraft(id) {
    this.state.drafts = (this.state.drafts || []).filter((d) => d.id !== id);
    this.save();
    return this.state.drafts;
  }

  /**
   * SMTP password, encrypted with the OS keychain when Electron can. Without
   * safeStorage the password is not persisted at all rather than being written
   * in the clear.
   */
  setSmtpPassword(password) {
    if (!password) {
      try { fs.unlinkSync(this.credentialsFile); } catch { /* already absent */ }
      this.updateSettings({ smtp: { hasPassword: false } });
      return { stored: false, encrypted: false };
    }
    if (!this.safeStorage || !this.safeStorage.isEncryptionAvailable()) {
      return { stored: false, encrypted: false, reason: 'no-encryption' };
    }
    fs.mkdirSync(this.directory, { recursive: true });
    fs.writeFileSync(this.credentialsFile, this.safeStorage.encryptString(password));
    this.updateSettings({ smtp: { hasPassword: true } });
    return { stored: true, encrypted: true };
  }

  getSmtpPassword() {
    if (!this.safeStorage || !this.safeStorage.isEncryptionAvailable()) return '';
    try {
      return this.safeStorage.decryptString(fs.readFileSync(this.credentialsFile));
    } catch {
      return '';
    }
  }
}

module.exports = { Store, DEFAULT_SETTINGS, DEFAULT_STATE };

'use strict';

/* global scmHtml */
// Rich-text editor built on a contenteditable region.
//
// It uses document.execCommand, which is formally deprecated but remains the
// only built-in way to do rich-text editing and is fully supported in the
// Chromium that Electron ships. The alternative — a third-party editor —
// would mean bundling a large dependency for a toolbar this modest.

(function initEditor(global) {
  const { cleanPastedHtml, sanitizeHtml, htmlToText, textToHtmlFragment, isEmptyHtml } = global.scmHtml;

  const FONT_SIZES = [
    ['1', '8pt'], ['2', '10pt'], ['3', '11pt'],
    ['4', '14pt'], ['5', '18pt'], ['6', '24pt'], ['7', '36pt'],
  ];

  class RichTextEditor {
    constructor(element, { onChange } = {}) {
      this.el = element;
      this.onChange = onChange || (() => {});
      this.sourceMode = false;
      this.el.contentEditable = 'true';
      this.el.spellcheck = true;

      // Produce <span style> rather than the legacy <font> element, which
      // Outlook and most webmail handle far more predictably.
      try {
        document.execCommand('styleWithCSS', false, true);
      } catch { /* older engines simply ignore this */ }

      this.el.addEventListener('input', () => this.onChange());
      this.el.addEventListener('paste', (event) => this.handlePaste(event));
      this.el.addEventListener('drop', (event) => this.handleDrop(event));
      this.el.addEventListener('keydown', (event) => this.handleKeydown(event));
    }

    /**
     * Paste keeps formatting, but only after the markup has been cleaned of
     * Office cruft and stripped of anything executable. Shift+Ctrl+V is left
     * to the browser, which pastes plain text.
     */
    handlePaste(event) {
      const clipboard = event.clipboardData;
      if (!clipboard) return;

      const html = clipboard.getData('text/html');
      if (html) {
        event.preventDefault();
        const clean = sanitizeHtml(cleanPastedHtml(html));
        document.execCommand('insertHTML', false, clean);
        this.onChange();
        return;
      }

      // Plain text still needs the line breaks turning into markup, or the
      // whole message collapses into one paragraph.
      const text = clipboard.getData('text/plain');
      if (text && /\n/.test(text)) {
        event.preventDefault();
        document.execCommand('insertHTML', false, textToHtmlFragment(text));
        this.onChange();
      }
    }

    handleDrop(event) {
      // Dropping a file into the body would replace the message with a file
      // URL; attachments have their own control.
      if (event.dataTransfer && event.dataTransfer.files && event.dataTransfer.files.length) {
        event.preventDefault();
      }
    }

    handleKeydown(event) {
      const ctrl = event.ctrlKey || event.metaKey;
      if (!ctrl) return;
      const key = event.key.toLowerCase();
      // Ctrl+B/I/U are handled natively; these are the additions.
      if (key === 'k') {
        event.preventDefault();
        this.promptForLink();
      }
    }

    exec(command, value = null) {
      if (this.sourceMode) return;
      this.el.focus();
      try {
        document.execCommand(command, false, value);
      } catch { /* unsupported command on this engine */ }
      this.onChange();
    }

    /** True when the caret currently sits inside the given formatting. */
    queryState(command) {
      if (this.sourceMode) return false;
      try {
        return document.queryCommandState(command);
      } catch {
        return false;
      }
    }

    /**
     * Electron has no window.prompt(), so this uses the app's own dialog.
     * The selection is captured before the dialog opens, because showing it
     * moves focus out of the editor and the range would otherwise be lost.
     */
    async promptForLink() {
      const selection = window.getSelection();
      const hasSelection = selection && String(selection).trim() !== '';
      const range = hasSelection && selection.rangeCount ? selection.getRangeAt(0).cloneRange() : null;

      const url = await global.scmPrompt('Link address:', 'https://', { title: 'Insert link' });
      if (!url) return;

      if (range) {
        const restored = window.getSelection();
        restored.removeAllRanges();
        restored.addRange(range);
        this.el.focus();
        this.exec('createLink', url);
        return;
      }

      const label = await global.scmPrompt('Text to show:', url, { title: 'Insert link' }) || url;
      this.insertHtml(`<a href="${scmHtml.escapeHtml(url)}">${scmHtml.escapeHtml(label)}</a>`);
    }

    insertHtml(html) {
      if (this.sourceMode) {
        // In source mode the textarea is the source of truth.
        const field = this.sourceField;
        const start = field.selectionStart ?? field.value.length;
        field.value = field.value.slice(0, start) + html + field.value.slice(field.selectionEnd ?? start);
        field.selectionStart = field.selectionEnd = start + html.length;
        this.onChange();
        return;
      }
      this.el.focus();
      document.execCommand('insertHTML', false, html);
      this.onChange();
    }

    /** Placeholders are inserted as plain text so they survive as {{field}}. */
    insertPlaceholder(field) {
      this.insertHtml(scmHtml.escapeHtml(`{{${field}}}`));
    }

    getHtml() {
      if (this.sourceMode) return this.sourceField.value;
      return this.el.innerHTML;
    }

    setHtml(html) {
      const clean = sanitizeHtml(String(html || ''));
      if (this.sourceMode) this.sourceField.value = clean;
      else this.el.innerHTML = clean;
      this.onChange();
    }

    getText() {
      return htmlToText(this.getHtml());
    }

    isEmpty() {
      return isEmptyHtml(this.getHtml());
    }

    /**
     * Swap between the formatted view and raw HTML, so a message written
     * elsewhere can be pasted in as source.
     */
    setSourceMode(on, sourceField) {
      this.sourceField = sourceField;
      if (on === this.sourceMode) return;
      if (on) {
        sourceField.value = this.el.innerHTML;
        this.sourceMode = true;
      } else {
        this.el.innerHTML = sanitizeHtml(sourceField.value);
        this.sourceMode = false;
      }
      this.onChange();
    }

    focus() {
      (this.sourceMode ? this.sourceField : this.el).focus();
    }
  }

  global.RichTextEditor = RichTextEditor;
  global.EDITOR_FONT_SIZES = FONT_SIZES;
}(window));

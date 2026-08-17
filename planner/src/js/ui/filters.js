/* ============================================================================
 * filters.js — the shared filter toolbar
 * ----------------------------------------------------------------------------
 * Trial, urgency, tag, sort and free text. Every view that lists tasks uses
 * the same bar so the controls are always in the same place.
 * ========================================================================== */

import { debounce, h, icon } from './dom.js';
import { store } from '../state/store.js';
import { ui, filterIsActive } from '../state/ui.js';
import { rerender } from '../bus.js';
import { allTags, emptyFilter, SORTS } from '../core/select.js';
import { URGENCIES } from '../core/parse.js';

export function filterBar({ showSort = true, showSearch = true, extra = null } = {}) {
  const filter = ui.filter;
  const trials = store.trials.filter((t) => t.active || t.id === filter.trialId);
  const tags = allTags(store.tasks);

  const onSearch = debounce((value) => { filter.query = value; rerender(); }, 220);

  const trialChips = h('div.row.wrap', { style: { gap: '5px' } },
    h('button.chip.chip-btn', {
      type: 'button',
      class: filter.trialId === 'all' ? 'on' : '',
      onclick: () => { filter.trialId = 'all'; rerender(); }
    }, 'All trials'),
    trials.map((trial) => h('button.chip.chip-btn', {
      type: 'button',
      class: filter.trialId === trial.id ? 'on' : '',
      title: trial.name,
      style: filter.trialId === trial.id
        ? { background: `color-mix(in srgb, ${trial.colour} 16%, transparent)`, color: trial.colour, borderColor: trial.colour }
        : {},
      onclick: () => { filter.trialId = filter.trialId === trial.id ? 'all' : trial.id; rerender(); }
    }, h('i.chip-dot', { style: { background: trial.colour } }), trial.code)),
    h('button.chip.chip-btn', {
      type: 'button',
      class: filter.trialId === 'none' ? 'on' : '',
      onclick: () => { filter.trialId = filter.trialId === 'none' ? 'all' : 'none'; rerender(); }
    }, 'No trial'));

  return h('div.toolbar',
    trialChips,

    h('select.select', {
      style: { width: 'auto' },
      'aria-label': 'Filter by urgency',
      onchange: (event) => { filter.urgency = event.target.value; rerender(); }
    },
    h('option', { value: 'all', selected: filter.urgency === 'all' }, 'Any urgency'),
    URGENCIES.map((urgency) => h('option', {
      value: urgency.id, selected: filter.urgency === urgency.id
    }, urgency.label))),

    tags.length ? h('select.select', {
      style: { width: 'auto' },
      'aria-label': 'Filter by tag',
      onchange: (event) => { filter.tag = event.target.value; rerender(); }
    },
    h('option', { value: 'all', selected: filter.tag === 'all' }, 'Any tag'),
    tags.map(({ tag, count }) => h('option', {
      value: tag, selected: filter.tag === tag
    }, `#${tag} (${count})`))) : null,

    showSort ? h('select.select', {
      style: { width: 'auto' },
      'aria-label': 'Sort tasks',
      onchange: (event) => { filter.sort = event.target.value; rerender(); }
    }, SORTS.map((sort) => h('option', {
      value: sort.id, selected: filter.sort === sort.id
    }, `Sort: ${sort.label}`))) : null,

    h('label.row.tiny.muted', { style: { gap: '7px', cursor: 'pointer' } },
      h('span.switch',
        h('input', {
          type: 'checkbox',
          checked: filter.showDone,
          onchange: (event) => { filter.showDone = event.target.checked; rerender(); }
        }),
        h('span')),
      'Show done'),

    extra,
    h('span.grow'),

    showSearch ? h('div.search',
      icon('search', { size: 14 }),
      h('input.input', {
        type: 'search',
        placeholder: 'Search tasks…',
        value: filter.query,
        'data-keep': 'filter-search',
        'aria-label': 'Search tasks',
        oninput: (event) => onSearch(event.target.value)
      })) : null,

    filterIsActive() ? h('button.btn.btn-sm.btn-ghost', {
      type: 'button',
      onclick: () => { ui.filter = { ...emptyFilter(), sort: filter.sort }; rerender(); }
    }, icon('close', { size: 12 }), 'Clear') : null);
}

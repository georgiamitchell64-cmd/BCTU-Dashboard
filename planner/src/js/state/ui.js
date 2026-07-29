/* ============================================================================
 * ui.js — ephemeral view state
 * ----------------------------------------------------------------------------
 * Which view is open, which day/week/month is being looked at, and the current
 * filter. Deliberately not persisted with the data file: only the last view is
 * remembered, and that lives in settings.
 * ========================================================================== */

import { emptyFilter } from '../core/select.js';
import { monthKey, todayKey } from '../core/dates.js';

export const ui = {
  view: 'today',
  /** Day cursor for Today and Week. */
  cursor: todayKey(),
  /** Month cursor for the Month view. */
  month: monthKey(todayKey()),
  filter: emptyFilter(),
  rail: false,
  /** Month cells expanded past the "+n more" cut-off. */
  expandedDays: new Set()
};

export const resetCursor = () => {
  ui.cursor = todayKey();
  ui.month = monthKey(todayKey());
};

export const filterIsActive = () => {
  const { query, trialId, urgency, tag, showDone } = ui.filter;
  return Boolean(query) || trialId !== 'all' || urgency !== 'all' || tag !== 'all' || !showDone;
};

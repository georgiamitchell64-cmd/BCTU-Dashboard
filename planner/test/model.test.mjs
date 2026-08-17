import assert from 'node:assert/strict';
import { test } from 'node:test';

import { createTask, defaultSettings, defaultState, migrate, normaliseTask } from '../src/js/state/model.js';
import {
  applyFilter, completionsOn, countByTrial, matchesFilter, progress,
  quadrantOf, sortTasks, streak, workload
} from '../src/js/core/select.js';

const settings = defaultSettings();

test('a new task gets sane defaults', () => {
  const task = createTask({ title: '  Book the TMG  ' }, settings);
  assert.equal(task.title, 'Book the TMG');
  assert.equal(task.scope, 'day');
  assert.equal(task.urgency, 'medium');
  assert.equal(task.done, false);
  assert.equal(task.week, null);
  assert.ok(task.id.startsWith('t_'));
});

test('scope decides which bucket field is filled', () => {
  const week = createTask({ title: 'a', scope: 'week', date: '2026-09-09' }, settings);
  assert.equal(week.week, '2026-09-07');
  assert.equal(week.date, null);

  const month = createTask({ title: 'a', scope: 'month', date: '2026-09-09' }, settings);
  assert.equal(month.month, '2026-09');
  assert.equal(month.date, null);
});

test('normalising rejects junk without throwing', () => {
  const task = normaliseTask({
    title: '', urgency: 'catastrophic', scope: 'fortnight',
    tags: ['Alpha', 'alpha', 42, ''], subtasks: [{ title: 'ok' }, null, { nope: 1 }],
    time: '99:99', estimate: 'lots', done: true
  }, settings);

  assert.equal(task.title, 'Untitled task');
  assert.equal(task.urgency, 'medium');
  assert.equal(task.scope, 'day');
  assert.deepEqual(task.tags, ['alpha']);
  assert.equal(task.subtasks.length, 1);
  assert.equal(task.time, null);
  assert.equal(task.estimate, null);
  assert.ok(task.completedAt, 'a done task gets a completion timestamp');
});

test('migrate copes with nothing, rubbish and partial data', () => {
  assert.deepEqual(migrate(null).tasks, []);
  assert.deepEqual(migrate('nonsense').trials.length, 1);
  assert.equal(migrate({ settings: { theme: 'neon', weekStart: 9 } }).settings.theme, 'system');
  assert.equal(migrate({ settings: { weekStart: 0 } }).settings.weekStart, 0);
});

test('migrate drops references to trials that no longer exist', () => {
  const state = migrate({
    trials: [{ id: 'tr_1', code: 'tonic', name: 'TONIC' }],
    tasks: [
      { id: 't1', title: 'kept', trialId: 'tr_1' },
      { id: 't2', title: 'orphan', trialId: 'tr_gone' }
    ],
    settings: { defaultTrial: 'tr_gone' }
  });

  assert.equal(state.tasks[0].trialId, 'tr_1');
  assert.equal(state.tasks[1].trialId, null);
  assert.equal(state.settings.defaultTrial, null);
  assert.equal(state.trials[0].code, 'TONIC');
});

test('migrate keeps only well-formed day notes', () => {
  const state = migrate({ notes: { '2026-09-09': 'ok', 'not-a-date': 'x', '2026-09-10': '   ' } });
  assert.deepEqual(Object.keys(state.notes), ['2026-09-09']);
});

test('a default state is stable through a save/load round trip', () => {
  const before = defaultState();
  const after = migrate(JSON.parse(JSON.stringify(before)));
  assert.equal(after.trials.length, before.trials.length);
  assert.equal(after.profile.name, before.profile.name);
});

/* ── Selectors ────────────────────────────────────────────────────────────── */

const trials = [{ id: 'tr_1', code: 'TONIC', name: 'TONIC', colour: '#2EC4A5', active: true }];
const make = (overrides) => normaliseTask({
  id: overrides.id || Math.random().toString(36).slice(2),
  createdAt: '2026-09-01T09:00:00.000Z',
  ...overrides
}, settings);

test('filtering matches on trial, urgency, tag and free text', () => {
  const tasks = [
    make({ title: 'SAE follow up', trialId: 'tr_1', urgency: 'critical', tags: ['safety'] }),
    make({ title: 'Order stationery', urgency: 'low' }),
    make({ title: 'Done thing', done: true })
  ];

  assert.equal(applyFilter(tasks, { trialId: 'tr_1', showDone: true }, trials).length, 1);
  assert.equal(applyFilter(tasks, { urgency: 'low', showDone: true }, trials).length, 1);
  assert.equal(applyFilter(tasks, { tag: 'safety', showDone: true }, trials).length, 1);
  assert.equal(applyFilter(tasks, { showDone: false }, trials).length, 2);
  assert.equal(applyFilter(tasks, { query: 'sae', showDone: true }, trials).length, 1);
  // Searching by trial code finds the task even though the code is not in the title.
  assert.equal(applyFilter(tasks, { query: 'tonic', showDone: true }, trials).length, 1);
  // All words must match.
  assert.equal(applyFilter(tasks, { query: 'sae stationery', showDone: true }, trials).length, 0);
});

test('search ignores case and matches subtasks', () => {
  const task = make({ title: 'Prep', subtasks: [{ id: 's1', title: 'Print consent forms' }] });
  assert.ok(matchesFilter(task, { query: 'CONSENT', showDone: true }, trials));
});

test('sorting by urgency puts critical first', () => {
  const tasks = [
    make({ title: 'c', urgency: 'low', order: 0 }),
    make({ title: 'a', urgency: 'critical', order: 1 }),
    make({ title: 'b', urgency: 'medium', order: 2 })
  ];
  assert.deepEqual(sortTasks(tasks, 'urgency').map((t) => t.title), ['a', 'b', 'c']);
});

test('sorting by time puts untimed tasks last', () => {
  const tasks = [
    make({ title: 'late', time: '16:00' }),
    make({ title: 'none' }),
    make({ title: 'early', time: '08:00' })
  ];
  assert.deepEqual(sortTasks(tasks, 'time').map((t) => t.title), ['early', 'late', 'none']);
});

test('progress and workload add up', () => {
  const tasks = [
    make({ title: 'a', done: true }),
    make({ title: 'b', estimate: 30 }),
    make({ title: 'c', estimate: 60 }),
    make({ title: 'd' })
  ];
  assert.deepEqual(progress(tasks), { total: 4, done: 1, remaining: 3, pct: 25 });
  assert.deepEqual(workload(tasks), { minutes: 90, unestimated: 1, count: 3 });
});

test('empty progress does not divide by zero', () => {
  assert.deepEqual(progress([]), { total: 0, done: 0, remaining: 0, pct: 0 });
});

test('completion history drives the streak', () => {
  const tasks = [make({ title: 'repeat', history: ['2026-09-08', '2026-09-07'] })];
  assert.equal(completionsOn(tasks, '2026-09-08').length, 1);
  // Today being untouched does not break a run that is otherwise intact.
  assert.equal(streak(tasks, '2026-09-09'), 2);
  assert.equal(streak(tasks, '2026-09-11'), 0);
});

test('the matrix places tasks by urgency and importance', () => {
  const today = '2026-09-09';
  assert.equal(quadrantOf(make({ title: 'a', urgency: 'critical' }), today), 'do');
  assert.equal(quadrantOf(make({ title: 'b', urgency: 'high' }), today), 'delegate');
  assert.equal(quadrantOf(make({ title: 'c', urgency: 'low', important: true, date: '2026-12-01' }), today), 'plan');
  assert.equal(quadrantOf(make({ title: 'd', urgency: 'low', date: '2026-12-01' }), today), 'drop');
  // Anything due today or tomorrow counts as urgent whatever its label says.
  assert.equal(quadrantOf(make({ title: 'e', urgency: 'low', date: today }), today), 'delegate');
});

test('untagged tasks get their own row in the trial breakdown', () => {
  const rows = countByTrial([
    make({ title: 'a', trialId: 'tr_1', done: true }),
    make({ title: 'b' })
  ], trials);
  assert.equal(rows.length, 2);
  assert.ok(rows.some((row) => row.trial.id === 'none'));
});

# Clinical Trials Dashboard — Platform Architecture

**Status:** Proposal for review
**Scope:** Evolving the BCTU dashboard from a REDCap-shaped multi-trial app into a
source-independent clinical trial dashboard platform.

---

## 1. Executive summary

The dashboard should be split into three strictly separated layers, connected by
two contracts:

```
┌─────────────────────────────────────────────────────────────────────┐
│  SOURCE LAYER      REDCap / MedSciNet / Panacea / CSV / Excel / API │
│                    (one adapter per source family)                  │
└──────────────────────────────┬──────────────────────────────────────┘
                               │  Contract 1: Source Package
                               │  (schema metadata + tidy raw tables)
┌──────────────────────────────▼──────────────────────────────────────┐
│  MAPPING LAYER     concept registry · mapping suggestions ·         │
│                    saved trial mapping · value decodes ·            │
│                    transformation · validation                      │
└──────────────────────────────┬──────────────────────────────────────┘
                               │  Contract 2: Canonical Trial Dataset
                               │  (typed domain tables, no source terms)
┌──────────────────────────────▼──────────────────────────────────────┐
│  DASHBOARD LAYER   module registry · availability engine ·          │
│                    recruitment / safety / completeness / … modules  │
└─────────────────────────────────────────────────────────────────────┘
```

The three most important decisions in this document:

1. **Adapters normalise *shape*, not *meaning*.** Every adapter emits the same
   source-neutral package (field metadata + tidy tables). Semantic mapping to
   dashboard concepts happens once, in a single shared mapping engine, driven by
   per-trial configuration. This is the refinement to the adapter idea in the
   brief: if each adapter mapped straight to the internal model, every adapter
   would re-implement mapping, suggestion, and validation logic.

2. **The canonical model is a hybrid: typed domain tables + one long
   observations table.** Not a fully generic entity–attribute–value store
   (dashboard queries become unreadable and slow) and not full CDISC SDTM
   (too heavy for a monitoring dashboard). We borrow SDTM's *domain* idea —
   participants, visits, dispositions, adverse events — and keep everything
   else as long-format observations.

3. **Modules declare what they need; the platform decides what to show.**
   Each dashboard module declares required and optional canonical concepts.
   Availability is computed from the trial's mapping + data, replacing the
   manual `features` flags with automatic, explainable enablement
   ("Safety tab hidden: no adverse-event concept mapped").

---

## 2. Where the current architecture actually stands

An honest audit of the code, because it determines the migration path:

**What already exists and is worth keeping:**

- Multi-trial config (`trials/<code>/config.R` + `overrides.json`), with logical
  roles for fields and events (`redcap_fields`, `redcap_events`) and `fld()` /
  `evt()` lookups (`globals/trial_config.R`).
- CSV auto-detection with candidate lists and pattern matching
  (`functions/csv_autodetect.R`), with a review/amend modal
  (`functions/autodetect_modal.R`, `modules/upload_server.R`) that persists
  confirmed mappings to `overrides.json`.
- Feature flags per trial, per-trial SQLite databases, a permissions/accounts
  system, and a large set of working analytics modules.

**What blocks the platform goal:**

- **The dashboard consumes the raw REDCap export.** `process_redcap()`
  (`functions/helpers.R`) produces only a thin `participants` summary and
  updated `sites`; everything else reads `rv$raw_redcap` — the untransformed
  wide REDCap frame — at render time. `fld()` is called from ~10 files
  (33 times in `functions/safety_events.R` alone), and REDCap conventions are
  baked into module logic: `redcap_event_name` filtering, `*_complete == 2`
  semantics, DAG-based site derivation, longitudinal same-variable-per-event
  counting (`modules/participants_server.R`). Supporting MedSciNet today would
  mean touching every module. The config layer makes *field names*
  configurable; it does not make the dashboard *source-independent*.
- **Mapping happens at point-of-use, not at import.** Because translation is
  deferred to render time via `fld()`, every module must handle missing
  mappings, loose date parsing (`.parse_date_loose()` exists precisely because
  raw values reach the UI), and coded-value decoding independently.
- **Configuration is executable R code.** `config.R` is sourced at discovery
  time. That is a poor fit for non-technical users, cannot be safely edited
  from the UI, cannot be schema-validated, and is an arbitrary-code-execution
  surface.
- **Process-wide globals hold per-session state.** `apply_trial_globals()`
  writes `.TRIAL_CFG`, `DATA_DIR`, `DB_PATH` with `<<-`. The comment in
  `app.R`'s `onSessionEnded` already acknowledges cross-session leakage. A
  multi-trial, multi-user platform cannot be built on process globals.
- **No metadata ingestion.** Auto-detection sniffs the *data* only; the REDCap
  data dictionary (labels, types, choices, forms, branching) is unused.
- **No validation stage.** Bad data (duplicate IDs, unparseable dates,
  unmapped events) surfaces as broken charts, not as import warnings.

---

## 3. Design principles (and where this diverges from the brief)

The brief's instincts are largely right. Points of agreement are stated
briefly; disagreements get the argument.

1. **Adapter architecture — yes, but two-stage.** (See §1, decision 1.)
   Adapters are *syntactic* (parse this vendor's export into tidy tables +
   field metadata). The mapping engine is *semantic* and shared. New source =
   one new adapter, zero new mapping code.

2. **Never rely on field names — but rely on metadata first, data second.**
   Suggestion quality comes from labels, types, choices and form context far
   more than from variable-name string similarity. `rand_dt` is opaque;
   "Randomisation Date" + field type `date_dmy` on form "Randomisation" is
   nearly conclusive.

3. **Don't invent the canonical model from scratch.** The brief says "there is
   no standard". For *EDC exports* that's true; for *trial data semantics* it
   isn't — CDISC SDTM has been the regulatory standard for 20 years. Adopting
   SDTM wholesale would be over-engineering for a monitoring dashboard, but the
   canonical model should *align its vocabulary* with SDTM domains (DM, SV, DS,
   AE, DV…) so that (a) concept boundaries are battle-tested, (b) a future
   "export to SDTM-ish" feature is cheap, and (c) trial statisticians recognise
   the concepts immediately.

4. **Deterministic, explainable mapping suggestions first; ML/LLM assist
   later, never silently.** A trials unit must be able to audit why the system
   suggested a mapping. A transparent scoring function over metadata features
   (plus a synonym library that grows from confirmed mappings) gets ~90% of the
   value. An LLM can be added later as one more scorer, but suggestions are
   always confirmed by a human and recorded in the trial config.

5. **Configuration is data, not code.** One versioned JSON document per trial,
   schema-validated, editable through the UI, diffable, portable. `config.R`
   survives only as a legacy shim during migration.

6. **Fail at import, not at render.** All parsing, decoding and validation
   happens in the pipeline. Modules receive typed, validated tables and may
   assume them. `.parse_date_loose()`-style defensiveness disappears from the
   dashboard layer.

7. **Keep the platform boring.** R + Shiny + per-trial SQLite remain. Nothing
   in this design requires a rewrite, a server framework change, or new
   infrastructure. The design is a re-layering, not a re-platforming.

---

## 4. Contract 1: the Source Package

Every adapter — regardless of vendor — produces the same structure:

```r
source_package <- list(
  source = list(
    adapter        = "redcap_csv",          # which adapter produced this
    adapter_version= "1.0.0",
    files          = c("TONIC_export_2026-07-14.csv",
                       "TONIC_DataDictionary_2026-07-01.csv"),
    fingerprint    = "sha256:…",            # for drift detection (§9)
    exported_at    = as.POSIXct("2026-07-14 09:00")
  ),

  # ── Schema: one row per source field ─────────────────────────────────────
  # Populated from the data dictionary when available, inferred from data
  # otherwise. `label`, `type`, `choices` are what the mapping UI shows and
  # what the suggestion engine scores on.
  schema = tibble(
    field_name   = "rand_dttm_s",
    label        = "Date and time of randomisation",
    form         = "randomisation",
    form_label   = "Randomisation",
    section      = NA_character_,
    type         = "datetime",           # normalised: id|text|integer|numeric|
                                         # date|datetime|categorical|checkbox|
                                         # boolean|file|calc|notes
    choices      = list(NULL),           # list-col: tibble(code, label) or NULL
    required     = TRUE,
    branching    = NA_character_,        # raw expression, informational
    inferred     = FALSE                 # TRUE if guessed from data, not dict
  ),

  # ── Structure: events/visits and forms as the source defines them ────────
  events = tibble(unique_name = "baseline_arm_1", label = "Baseline",
                  order = 1L, forms = list(c("randomisation", "consent"))),
  forms  = tibble(name = "randomisation", label = "Randomisation",
                  repeating = FALSE),

  # ── Data: tidy tables, source column names preserved ─────────────────────
  # `records`: one row per participant-event(-repeat) in the source's own
  # vocabulary. Wide is fine here; the transformer consumes it via mapping.
  data = list(records = tibble(...))
)
```

Key properties:

- **Source column names are preserved** into the package; renaming happens in
  the transformer. This keeps adapters trivial and makes the mapping UI able to
  show "Randomisation Date `(rand_dttm_s)`".
- **The schema is mandatory, the dictionary is optional.** When no dictionary
  is supplied, the adapter infers `type` (date detection, small-cardinality
  categorical detection, numeric detection) and sets `inferred = TRUE` so the
  UI can flag lower-confidence metadata.
- **`fingerprint`** hashes the schema (field names + types + choices), not the
  data — it drives "your project changed since last configuration" detection.

### Adapter interface

S3 generics keep this idiomatic and dependency-free:

```r
# functions/adapters/adapter_api.R
adapter_detect   <- function(adapter, paths) UseMethod("adapter_detect")
  # → confidence 0–1 that these files belong to this adapter, + reasons
adapter_inputs   <- function(adapter) UseMethod("adapter_inputs")
  # → declares expected inputs for the upload UI
  #   e.g. redcap: data export (required) + data dictionary (recommended)
adapter_read     <- function(adapter, paths, options) UseMethod("adapter_read")
  # → source_package (above); throws typed errors with user-facing messages
```

Registered adapters live in `functions/adapters/<name>.R` and self-register in
a registry list. Upload flow: user drops files → every adapter's
`adapter_detect()` runs → best match pre-selected, user can override.
Detection is cheap and reliable in practice: a REDCap raw export has
`redcap_event_name`/`*_complete` signature columns; a REDCap dictionary has the
fixed 18-column header; MedSciNet and Panacea exports have their own
signatures; generic CSV/Excel is the fallback that always matches at low
confidence.

**Planned adapters, in order:**

| Adapter | Metadata source | Notes |
|---|---|---|
| `redcap_csv` | Data dictionary CSV/XLSX (§7) | Port of current reader; first implementation |
| `generic_tabular` | Inferred from data | CSV/Excel, incl. multi-sheet workbooks; **build second — it proves the abstraction** |
| `medscinet_export` | Export structure + optional study spec | When first MedSciNet trial onboards |
| `panacea_export` | Export structure | When first Panacea trial onboards |
| `redcap_api` | Metadata API | Same schema output as `redcap_csv`; only the acquisition differs |

Deliberate sequencing point: **do not build three vendor adapters up front.**
Build REDCap + generic tabular. If generic tabular works end-to-end through the
same mapping engine, the abstraction is proven; vendor adapters then get
written when a real trial needs them, against real export samples rather than
guessed formats.

---

## 5. Contract 2: the Canonical Trial Dataset

The only structure the dashboard layer may read. No source vocabulary
(no `redcap_event_name`, no `_complete`, no DAGs) appears at or below this
line.

### 5.1 Core tables (typed, always present — possibly empty)

```
participants   participant_id · site_id · enrolled_at · randomised_at ·
               arm · status (screened|randomised|withdrawn|completed|died) ·
               status_date · cohort            # ~SDTM DM (+DS summary)

sites          site_id · name · status · open_date · target ·
               monthly_target · geo (lat/lon) · source ("data"|"manual")

visits         participant_id · visit_id · visit_date ·
               due_date · window_open · window_close · status
                                               # ~SDTM SV; schedule from config

form_status    participant_id · visit_id · form_concept ·
               status (complete|partial|missing|not_expected) · updated_at
                                               # generalises *_complete == 2

observations   participant_id · visit_id · concept · value ·
               value_num · value_date · value_code · value_label
                                               # long/tidy; everything mapped
                                               # that isn't a core column
```

### 5.2 Domain tables (present when the trial maps them)

```
status_changes  participant_id · type (withdrawal|death|lost|no_op|other) ·
                subtype_code · subtype_label · date · reason      # ~SDTM DS
safety_events   participant_id · type (sae|ae) · term · onset_date ·
                report_date · severity · relatedness · expectedness ·
                outcome · narrative · fatal                       # ~SDTM AE
deviations      participant_id · term · date · report_date ·
                category · narrative                              # ~SDTM DV
queries         participant_id · field_concept · raised_at · status ·
                resolved_at                    # data-management queries
proms           participant_id · visit_id · instrument · status ·
                sent_at · returned_at          # postal/PROM tracking
```

### 5.3 Why hybrid rather than pure EAV or pure wide

- *Pure long/EAV* ("one observations table for everything") is maximally
  generic but pushes pivoting, typing and decoding into every chart, which is
  exactly the per-module defensiveness we're eliminating. Recruitment,
  safety and disposition queries are 90% of dashboard load and deserve typed
  tables.
- *Pure wide canonical* ("one row per participant with standard columns")
  cannot represent longitudinal instruments (TONIC's `post_operation_complete`
  collected at both day-30 and day-90 already broke this shape — see the
  workaround note in `trials/tonic/config.R`).
- The hybrid gives typed tables for the hot paths and `observations` +
  `form_status` for the long tail (baseline characteristics tables, custom
  breakdowns, completeness heatmaps) without schema changes.

### 5.4 Canonical vocabularies

Labels are not enough; cross-trial modules need canonical *codes*. For a small
set of enumerations the platform defines its own vocabulary and each trial's
config supplies a decode map:

- `participant.status`, `status_change.type`, `safety_event.severity`,
  `safety_event.relatedness`, `sex`, `form_status.status`.

Example: TONIC `cos_type` code `4` → canonical `status_change.type =
"withdrawal", subtype = "complete"`. The original code and label are kept
(`subtype_code`, `subtype_label`) so nothing is lost. Everything outside this
small set stays trial-specific (code + label carried through `observations`)
— do not attempt a universal medical vocabulary; that way lies MedDRA
licensing and a mapping burden nobody asked for.

### 5.5 Persistence

The canonical dataset is written to the trial's existing SQLite database at
import (tables prefixed `canon_`), alongside an `imports` audit table (when,
by whom, which files, adapter, validation summary). Benefits: the dashboard
can open instantly from the last import without re-parsing CSVs; reports and a
future API read the same store; imports become auditable history rather than
ephemeral session state.

---

## 6. The concept registry and the mapping engine

### 6.1 Concept registry

A single platform-owned table of every concept a module can ask for. This is
the contract between the mapping UI and the module layer — new dashboard
capability = new registry entries + a module that consumes them.

```yaml
# concepts.yaml (illustrative entries)
- id: participant.id
  label: Participant ID
  target: participants.participant_id
  required: platform            # dashboard cannot run without it
  expect: {types: [id, text, integer], unique_per: participant}
  synonyms: [participant id, study id, record id, patid, subject,
             screening number, unique participant identifier, trial no]

- id: randomisation.datetime
  label: Randomisation date/time
  target: participants.randomised_at
  required: module              # required by whichever modules declare it
  expect: {types: [date, datetime]}
  synonyms: [randomisation date, randomization date, date randomised, rand]
  form_hints: [randomisation, rando, treatment allocation]

- id: safety.sae.onset_date
  label: SAE onset date
  target: safety_events.onset_date
  group: safety.sae             # concepts confirm/decline as a group
  expect: {types: [date, datetime]}
  form_hints: [sae, serious adverse event, safety]
```

Registry entries carry everything the suggestion engine and mapping UI need:
expected types, uniqueness constraints, synonym lists, form-name hints, and
which canonical column they land in. The current `redcap_fields` roles in
`trials/tonic/config.R` enumerate the initial registry almost completely —
this is a formalisation, not an invention.

### 6.2 Suggestion engine

Deterministic scoring per (concept × source field), combining independent
evidence:

| Signal | Source | Example |
|---|---|---|
| Label similarity | dictionary label vs concept label + synonyms (normalised token match) | "Date and time of randomisation" → `randomisation.datetime` |
| Name similarity | variable name vs synonyms | `patid` → `participant.id` |
| Type compatibility | dictionary validation / inferred type vs `expect.types` | `date_dmy` boosts date concepts, hard-vetoes ID concepts |
| Form context | form/section name vs `form_hints` | field on form "Serious Adverse Event" boosts `safety.*` |
| Value shape | uniqueness, cardinality, date-parse rate, code sets | 100% unique non-null → boosts `participant.id` |
| Structural priors | vendor conventions, applied by the adapter as schema annotations | REDCap: first field is the record ID; `*_complete` fields are form statuses; DAG ≈ site |
| Prior confirmations | synonym library grown from previously confirmed mappings across trials (stored centrally in `shared.sqlite`) | after two trials map `centre_name` → `site.name`, the third gets it at high confidence |

Output per concept: ranked candidates with score and *reasons* — the UI shows
"✓ Randomisation Date `(rand_dt)` — 95% · label match · date type · form
'Randomisation'". Thresholds: ≥ high → pre-selected; middle band → suggested,
unconfirmed; below → left unmapped. Human confirmation is always the final
step, and confirmations feed the synonym library (concept 4 of the brief's
"previous trial configurations", made concrete).

This replaces `functions/csv_autodetect.R`'s hardcoded candidate vectors: those
lists become the seed synonym data, and the engine becomes one testable
function with a fixture suite of real (anonymised) dictionaries.

### 6.3 Mapping document

The confirmed output is a declarative, versioned document — the heart of a
trial's configuration:

```jsonc
{
  "mapping_version": 3,
  "source_fingerprint": "sha256:…",
  "concepts": {
    "participant.id":          {"field": "record_id",   "confirmed_by": "gm", "confirmed_at": "2026-07-14"},
    "site.name":               {"field": "site_name"},
    "randomisation.datetime":  {"field": "rand_dttm_s"},
    "safety.sae.onset_date":   {"field": "sae_onset_dt"}
  },
  "events": {
    "baseline":  {"source_events": ["baseline_arm_1"]},
    "day_30":    {"source_events": ["day_30_arm_1"], "offset_days": 30, "window": [-7, 14]}
  },
  "forms": {
    "eq5d": {"source_form": "eq5d", "timepoints": ["baseline","discharge","day_30","day_90"]}
  },
  "decodes": {
    "status_change.type": {"1": "death", "2": "no_op", "3": "withdrawal", "4": "withdrawal", "5": "lost"}
  }
}
```

### 6.4 Transformer

One shared, source-agnostic function:

```r
build_canonical(source_package, trial_config) → list(dataset, issues)
```

It resolves concepts to source fields, parses/types values *once* (dates,
numerics, code decoding — centralising what `.parse_date_loose()` does ad hoc
today), derives `form_status` from the source's completion representation
(adapter annotates *how* completion is expressed; REDCap: `*_complete == 2`),
populates domain tables, and emits a structured issue list for the validation
report. It contains **zero vendor conditionals** — anything vendor-specific
must have been normalised by the adapter or expressed as schema annotations.

---

## 7. Using the REDCap data dictionary properly

The dictionary is the highest-value, lowest-cost improvement in this whole
design and should land early.

- **Ingestion:** accept the CSV or XLSX dictionary alongside (or before) the
  data export. Columns map directly into the source schema: `field_name`,
  `field_label` → `label`, `form_name` → `form`, `field_type` +
  `text_validation_type` → normalised `type`, `select_choices_or_calculations`
  → parsed `choices` (the `"1, Mild | 2, Moderate | 3, Severe"` format),
  `branching_logic` kept raw for display, `section_header` → `section`.
- **Pre-data configuration:** because the dictionary describes the whole
  project, a coordinator can configure a trial *before any data exists* —
  suggestion engine runs on metadata alone, the mapping UI shows human labels
  grouped by form, and choice labels replace hand-typed label vectors like
  TONIC's 19-entry `ethnicity_labels` (which the dictionary already contains).
- **Consistency check:** when data arrives, dictionary vs export are
  cross-validated (fields in data but not dictionary and vice versa, choice
  codes in data absent from the dictionary) — this feeds the validation report
  and the drift detector.
- **Codebook variant:** the printable codebook (HTML/PDF) is *not* a target
  format; the CSV/XLSX dictionary and the API metadata endpoint carry the same
  information in parseable form. The UI should ask for "the data dictionary
  (Project Setup → Data Dictionary → download)".

MedSciNet and Panacea equivalents (study definition exports) slot into the
same `schema` structure when those adapters are written; when a source has no
metadata artefact, inference from data fills `schema` with `inferred = TRUE`.

---

## 8. Validation

A distinct pipeline stage between transformation and publish, producing a
report the user sees *before* the dashboard renders. Three severities:

- **Blocking** — dashboard will not load: `participant.id` unmapped; duplicate
  participant IDs within an event; zero rows.
- **Warning** — dashboard loads, banner shown: unparseable dates (with count
  and examples), participants with no site, codes present in data but missing
  from decode maps, mapped events absent from the data, randomisation dates in
  the future.
- **Info** — configuration hints: unmapped source fields that scored highly
  for some concept ("`dev_dt` looks like a deviation date — map it to enable
  the Deviations panel"), modules that would enable with one more mapping.

Rules are data-driven (a rule registry keyed by canonical table/column), so
new checks are additions, not pipeline changes. The validation result is
persisted with the import (§5.5) and re-shown from the Data tab.

---

## 9. Trial configuration lifecycle

One declarative document per trial replaces `config.R`:

```
trials/<code>/trial.json
├─ meta        code · name · target · category · branding · report defaults
├─ source      adapter · input expectations · data folder binding
├─ mapping     the §6.3 document (concepts · events · forms · decodes)
├─ schedule    visit timepoints, offsets, windows (drives `visits`)
└─ modules     per-module settings/overrides (NOT on/off flags — §10)
```

- **Schema-validated** (JSON Schema) on load and on save; the UI edits it
  through forms, power users can edit the file. Versioned with a
  `config_version` and migrated forward automatically.
- **Configure once, reuse forever:** each import compares the new source
  package's `fingerprint` against `mapping.source_fingerprint`. Match → import
  silently with saved config. Mismatch → a *drift review* showing exactly what
  changed ("2 new fields, 1 removed, `sae_severity` choices changed") with
  suggestions only for the delta — never a full re-mapping.
- **Migration shim:** a one-off converter reads existing `config.R` +
  `overrides.json` and writes `trial.json` (TONIC becomes the first migrated
  trial and the regression fixture). During transition, discovery prefers
  `trial.json` and falls back to `config.R`.
- **Templates:** a new trial can start from a saved template ("BCTU surgical
  RCT, REDCap") that pre-seeds schedule shape and module settings — replacing
  `create_trial_template()`'s string-built R code.

---

## 10. The module layer

### 10.1 Module manifest

Every dashboard module registers a manifest:

```r
register_module(
  id       = "safety",
  title    = "Safety",
  requires = c("participant.id", "safety.sae.onset_date"),   # concepts/domains
  enhanced_by = c("safety.sae.severity", "safety.sae.narrative",
                  "safety.sae.relatedness"),
  ui       = safety_ui,
  server   = safety_server                                    # receives (id, trial)
)
```

### 10.2 Availability engine

After each import, the platform computes per-module status from the manifest
against the trial's mapping and canonical dataset:

- **enabled** — all `requires` mapped and populated;
- **degraded** — enabled, but some `enhanced_by` missing (module renders,
  hides the affected panels; the Data tab can say why);
- **hidden** — `requires` unmet, with the reason surfaced in trial settings
  ("Safety: map *SAE onset date* to enable") — which doubles as discoverable
  onboarding.

Manual override remains possible (`modules.<id>.enabled: false` in
`trial.json` force-hides a module a unit doesn't want), but the default is
computed. This subsumes the current `features` flags and fixes their failure
mode: flags that promise pages the data can't support.

### 10.3 Module data access

Modules receive a `trial` handle — canonical tables plus config-derived
context (targets, schedule, branding) — and **may not** reach into raw source
data. The `x__<header>` extra-columns mechanism in `safety_events.R` (user-
mapped extra detail columns) generalises properly here: extra columns become
additional *observations* mapped to ad-hoc concepts under a trial-local
namespace, and drill-down tables render them generically.

---

## 11. State and session hygiene

Prerequisite plumbing, not optional polish:

- **Eliminate `apply_trial_globals()` / `.TRIAL_CFG` / `DATA_DIR` / `DB_PATH`
  process globals.** The trial handle (config + canonical dataset + paths)
  lives in session state (`state$trial`) and is passed explicitly. `fld()` and
  `evt()` survive the migration only as functions of an explicit `cfg`
  argument, and disappear from the dashboard layer entirely once modules read
  canonical tables.
- **Import pipeline is pure:** `paths → adapter → mapping → validate →
  canonical dataset` with no reactive values or globals touched until the
  final publish step swaps `state$trial`. This makes the pipeline unit-testable
  end-to-end with fixture files — which is how adapters and the suggestion
  engine should be developed (a fixture suite of real, anonymised exports and
  dictionaries per source).

---

## 12. Proposed source layout

```
functions/
  adapters/
    adapter_api.R            # S3 generics + registry
    redcap_csv.R             # export + dictionary reader
    generic_tabular.R        # CSV/Excel fallback
    (medscinet.R, panacea.R when needed)
  pipeline/
    concepts.R               # registry loader (concepts.yaml)
    suggest.R                # scoring engine + synonym library
    transform.R              # build_canonical()
    validate.R               # rule registry + runner
    canonical_store.R        # SQLite read/write of canon_* tables
    drift.R                  # fingerprint + delta review
  ...existing analytic helpers, progressively rewritten against canon tables
modules/
  registry.R                 # register_module() + availability engine
  setup/                     # mapping studio UI (replaces autodetect modal)
  ...existing modules, migrated one at a time
concepts.yaml
trials/<code>/trial.json
```

---

## 13. Migration plan

Each phase ships independently and keeps TONIC working throughout — TONIC is
the regression baseline, not a casualty.

**Phase 1 — Canonical core behind the current importer.**
Extend `process_redcap()` to *also* emit `participants`, `sites`, `visits`,
`form_status`, `status_changes`, `safety_events` canonical tables (it already
computes most of the raw ingredients), publish them via the trial handle, and
persist to SQLite. Migrate the modules with the deepest raw coupling first —
`safety_events.R` (33 `fld()` calls) and `participants_server.R` — because
they gate everything else; then overview/sites/randomisations. Success
criterion: `rv$raw_redcap` has zero readers outside the pipeline. *No user-
visible change.*

**Phase 2 — Metadata + mapping engine.**
Dictionary ingestion; concept registry seeded from today's `redcap_fields`
roles + `csv_autodetect.R` candidate lists; scoring engine; mapping studio UI
(labelled fields grouped by form, confidence + reasons, replaces the
autodetect modal); `trial.json` + converter from `config.R`/`overrides.json`.
*User-visible: dramatically better setup experience for REDCap trials.*

**Phase 3 — Adapter seam + validation + module availability.**
Formalise `adapter_read()` and move the REDCap reader behind it; build
`generic_tabular` (the abstraction proof); validation stage + report UI;
module manifests + availability engine replacing `features` flags; retire the
trial-config globals. *User-visible: non-REDCap trials via CSV/Excel;
import-time quality report; self-configuring navigation.*

**Phase 4 — Growth.**
MedSciNet/Panacea adapters against real exports; drift review on re-upload;
cross-trial synonym learning; REDCap API adapter; further modules (queries,
protocol deviations, PROMs) as pure consumers of the canonical model.

---

## 14. Alternatives considered and rejected

- **Full CDISC SDTM as the internal model** — regulatory-grade but heavy
  (controlled terminology, per-domain variable conventions); a monitoring
  dashboard needs its shape, not its ceremony. Alignment, not adoption (§3.3).
- **Pure EAV canonical store** — maximal flexibility, unacceptable query
  ergonomics for the hot dashboard paths (§5.3).
- **Per-adapter mapping straight to canonical** (the brief's diagram, read
  literally) — duplicates mapping/suggestion/validation logic per vendor and
  couples the mapping UX to each adapter; rejected for the two-stage design
  (§4).
- **LLM-first field mapping** — opaque, unauditable for a trials unit, and
  unnecessary: metadata + synonyms are highly discriminative. Kept as a
  possible *additional* scorer behind the same confirm-first UX (§3.4).
- **Rewriting off Shiny** — nothing here requires it; the constraint is
  layering, not framework. A rewrite would burn the working analytics modules
  for zero architectural gain.

---

## 15. Summary of what changes for whom

| Audience | Today | After |
|---|---|---|
| Trial coordinator | Edits `config.R` in R, or accepts a REDCap-only autodetect modal | Uploads dictionary + export, confirms suggested mappings against human-readable labels, once |
| New trial on MedSciNet/CSV | Impossible without code changes | Generic tabular adapter + same mapping UI |
| Module developer | Reads `rv$raw_redcap`, defends against missing fields and raw codes everywhere | Declares required concepts, receives typed canonical tables |
| Platform maintainer | REDCap conventions in ~12 files; process globals; code-as-config | Vendor logic isolated in adapters; pure testable pipeline; declarative validated config |

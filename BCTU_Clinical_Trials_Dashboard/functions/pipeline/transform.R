# =============================================================================
# Canonical transformer
# =============================================================================
# build_canonical(source_package, cfg) → Contract 2: the Canonical Trial
# Dataset (docs/ARCHITECTURE.md §5). The ONLY structure the dashboard layer
# should read. Contains zero vendor conditionals: anything vendor-specific
# must arrive normalised in the source package (adapter output) or be
# expressed in the trial's mapping.
#
# Dataset tables:
#   participants    one row per participant (id, site, randomised_at, arm,
#                   status, status_date)
#   form_status     participant × visit × form completion
#   status_changes  withdrawals / deaths / etc (~SDTM DS)
#   safety_events   SAEs (~SDTM AE)
#   deviations      protocol deviations (~SDTM DV)
#   observations    long table of every other mapped concept value
#
# All parsing and decoding happens HERE, once — modules receive typed
# columns and may assume them.
# =============================================================================

#' Parse date-ish strings to Date. ISO first, then UK d/m/Y; time discarded.
parse_date_std <- function(x) {
  if (is.null(x)) return(as.Date(character(0)))
  if (inherits(x, "Date")) return(x)
  if (inherits(x, "POSIXt")) return(as.Date(x))
  x_chr <- trimws(as.character(x))
  x_chr[!nzchar(x_chr)] <- NA_character_
  x_chr <- sub("[ T].*$", "", x_chr)
  out <- suppressWarnings(as.Date(x_chr, format = "%Y-%m-%d"))
  uk  <- is.na(out) & !is.na(x_chr)
  if (any(uk))
    out[uk] <- suppressWarnings(as.Date(x_chr[uk], format = "%d/%m/%Y"))
  out
}

#' Resolve the trial's concept → source-field mapping.
#' Prefers the declarative mapping (cfg$mapping$concepts, trial.json style);
#' falls back to translating legacy cfg$redcap_fields via each concept's
#' legacy_role. @return named chr: concept_id → field_name
resolve_concept_mapping <- function(cfg, registry = concept_registry()) {
  out <- character(0)
  mapped <- cfg$mapping$concepts
  if (!is.null(mapped) && length(mapped)) {
    for (cid in names(mapped)) {
      f <- mapped[[cid]]$field %||% mapped[[cid]]
      if (is.character(f) && length(f) == 1 && nzchar(f)) out[[cid]] <- f
    }
  }
  legacy <- cfg$redcap_fields %||% list()
  for (cid in names(registry)) {
    if (cid %in% names(out)) next
    lr <- registry[[cid]]$legacy_role
    if (!is.na(lr %||% NA_character_) && !is.null(legacy[[lr]])) {
      f <- legacy[[lr]]
      if (is.character(f) && length(f) == 1 && nzchar(f)) out[[cid]] <- f
    }
  }
  out
}

#' Resolve logical event roles → source event names (list role → chr vector).
resolve_event_mapping <- function(cfg) {
  m <- cfg$mapping$events
  if (!is.null(m) && length(m)) {
    out <- lapply(m, function(e) e$source_events %||% e)
    return(Filter(function(v) is.character(v) && length(v), out))
  }
  Filter(function(v) is.character(v) && length(v),
         cfg$redcap_events %||% list())
}

# Canonical status_change.type vocabulary and the default decode heuristic
# (used when the trial supplies no explicit decode map). Matches label text.
.STATUS_CHANGE_TYPES <- c("death", "withdrawal", "lost", "no_intervention", "other")

.decode_status_type <- function(code, decodes, labels) {
  code <- trimws(as.character(code))
  if (!nzchar(code) || is.na(code)) return(NA_character_)
  if (!is.null(decodes[[code]])) return(decodes[[code]])
  lbl <- tolower(labels[[code]] %||% "")
  if (grepl("death|died", lbl))            return("death")
  if (grepl("withdraw", lbl))              return("withdrawal")
  if (grepl("lost", lbl))                  return("lost")
  if (grepl("no operation|not receive|no intervention", lbl))
    return("no_intervention")
  "other"
}

#' Look up choice labels for a field from the source schema.
.choices_for <- function(schema, field) {
  if (is.null(field) || is.null(schema) || !nrow(schema)) return(NULL)
  i <- match(field, schema$field_name)
  if (is.na(i)) return(NULL)
  schema$choices[[i]]
}

.decode_value <- function(x, choices) {
  if (is.null(choices) || !nrow(choices)) return(as.character(x))
  lbl <- choices$label[match(trimws(as.character(x)), choices$code)]
  ifelse(is.na(lbl), as.character(x), lbl)
}

#' Build the Canonical Trial Dataset.
#'
#' @param source_package adapter output (Contract 1)
#' @param cfg trial config (legacy config.R list or trial.json list)
#' @return list(dataset = list(<tables>), issues = tibble(severity, code,
#'   message, count))
build_canonical <- function(source_package, cfg,
                            registry = concept_registry()) {
  records <- source_package$data$records
  schema  <- source_package$schema
  conv    <- source_package$conventions %||% list()
  issues  <- list()
  note <- function(severity, code, message, count = NA_integer_) {
    issues[[length(issues) + 1]] <<- tibble::tibble(
      severity = severity, code = code, message = message,
      count = as.integer(count))
  }

  cmap <- resolve_concept_mapping(cfg, registry)
  emap <- resolve_event_mapping(cfg)

  fld_of <- function(concept_id) {
    if (!(concept_id %in% names(cmap))) return(NULL)
    f <- cmap[[concept_id]]
    if (is.null(f) || !nzchar(f) || !(f %in% names(records))) NULL else f
  }
  col_of <- function(concept_id) {
    f <- fld_of(concept_id)
    if (is.null(f)) rep(NA_character_, nrow(records))
    else trimws(as.character(records[[f]]))
  }

  # ── Identity ──────────────────────────────────────────────────────────────
  id_field <- fld_of("participant.id")
  if (is.null(id_field)) {
    note("blocking", "missing_participant_id",
         "No participant identifier is mapped (concept participant.id).")
    return(list(dataset = NULL, issues = dplyr::bind_rows(issues)))
  }
  pid <- trimws(as.character(records[[id_field]]))
  keep <- !is.na(pid) & nzchar(pid) & pid != "NA"
  if (any(!keep))
    note("warning", "blank_ids",
         "Rows dropped because the participant identifier is blank.",
         sum(!keep))
  rec <- records[keep, , drop = FALSE]
  pid <- pid[keep]
  if (!nrow(rec)) {
    note("blocking", "no_rows", "No usable rows after removing blank IDs.")
    return(list(dataset = NULL, issues = dplyr::bind_rows(issues)))
  }

  gcol <- function(concept_id) {
    f <- fld_of(concept_id)
    if (is.null(f)) rep(NA_character_, nrow(rec))
    else trimws(as.character(rec[[f]]))
  }

  ev_field <- conv$event_field
  visit <- if (!is.null(ev_field) && !is.na(ev_field) && ev_field %in% names(rec)) {
    trimws(as.character(rec[[ev_field]]))
  } else rep(NA_character_, nrow(rec))

  # Logical visit id: map source event names → role names where configured
  visit_role <- visit
  for (role in names(emap)) {
    visit_role[visit %in% emap[[role]]] <- role
  }

  # ── participants ─────────────────────────────────────────────────────────
  site_v <- gcol("site.name")
  rand_v <- gcol("randomisation.datetime")
  arm_f  <- fld_of("randomisation.arm")
  arm_v  <- if (!is.null(arm_f))
    .decode_value(rec[[arm_f]], .choices_for(schema, arm_f))
  else rep(NA_character_, nrow(rec))

  per_participant <- function(pid, v) {
    # first non-empty value per participant, aligned to unique(pid) order
    ok <- !is.na(v) & nzchar(v)
    first_ok <- tapply(seq_along(v)[ok], pid[ok], function(ix) ix[1])
    out <- stats::setNames(rep(NA_character_, length(unique(pid))), unique(pid))
    out[names(first_ok)] <- v[unlist(first_ok)]
    out
  }

  u_pid  <- unique(pid)
  p_site <- per_participant(pid, site_v)
  p_rand <- per_participant(pid, rand_v)
  p_arm  <- per_participant(pid, arm_v)

  participants <- tibble::tibble(
    participant_id = u_pid,
    site_name      = unname(p_site[u_pid]),
    randomised_at  = parse_date_std(unname(p_rand[u_pid])),
    arm            = unname(p_arm[u_pid]),
    status         = ifelse(is.na(parse_date_std(unname(p_rand[u_pid]))),
                            "screened", "randomised"),
    status_date    = as.Date(NA)
  )

  n_bad_rand <- sum(!is.na(p_rand) & nzchar(p_rand) &
                      is.na(parse_date_std(unname(p_rand))))
  if (n_bad_rand > 0)
    note("warning", "unparseable_randomisation_dates",
         "Randomisation dates could not be parsed.", n_bad_rand)

  # ── form_status ──────────────────────────────────────────────────────────
  comp <- conv$completion_fields %||% character(0)
  done_vals <- as.character(conv$completion_done %||% character(0))
  fs_rows <- list()
  for (form in names(comp)) {
    fcol <- comp[[form]]
    if (!(fcol %in% names(rec))) next
    v <- trimws(as.character(rec[[fcol]]))
    has <- !is.na(v) & nzchar(v)
    if (!any(has)) next
    fs_rows[[form]] <- tibble::tibble(
      participant_id = pid[has],
      visit_id       = visit_role[has],
      form_concept   = form,
      status         = ifelse(v[has] %in% done_vals, "complete", "partial")
    )
  }
  form_status <- if (length(fs_rows)) dplyr::distinct(dplyr::bind_rows(fs_rows))
  else tibble::tibble(participant_id = character(), visit_id = character(),
                      form_concept = character(), status = character())

  # ── status_changes ───────────────────────────────────────────────────────
  sc_type_f <- fld_of("status_change.type")
  decodes <- cfg$mapping$decodes$`status_change.type` %||% list()
  # Code → label map: trial config wins, else the source schema's choices
  labels <- as.list(cfg$cos_type_labels %||% character(0))
  if (!length(labels)) {
    ch <- .choices_for(schema, sc_type_f)
    if (!is.null(ch)) labels <- as.list(stats::setNames(ch$label, ch$code))
  }
  status_changes <- if (!is.null(sc_type_f)) {
    v <- trimws(as.character(rec[[sc_type_f]]))
    has <- !is.na(v) & nzchar(v)
    tibble::tibble(
      participant_id = pid[has],
      type    = vapply(v[has], .decode_status_type, character(1),
                       decodes = decodes, labels = labels),
      subtype_code  = v[has],
      subtype_label = vapply(v[has], function(cd)
        as.character(labels[[cd]] %||% cd), character(1)),
      date    = parse_date_std(gcol("status_change.date")[has]),
      reason  = gcol("status_change.reason")[has]
    )
  } else {
    tibble::tibble(participant_id = character(), type = character(),
                   subtype_code = character(), subtype_label = character(),
                   date = as.Date(character()), reason = character())
  }

  # Reflect terminal dispositions back onto participants.status
  if (nrow(status_changes)) {
    term <- status_changes |>
      dplyr::filter(type %in% c("death", "withdrawal", "lost")) |>
      dplyr::arrange(date) |>
      dplyr::distinct(participant_id, .keep_all = TRUE)
    i <- match(term$participant_id, participants$participant_id)
    ok <- !is.na(i)
    participants$status[i[ok]] <- ifelse(term$type[ok] == "death", "died",
                                         term$type[ok])
    participants$status_date[i[ok]] <- term$date[ok]
  }

  # ── safety_events ────────────────────────────────────────────────────────
  sev_f <- fld_of("safety.sae.severity")
  rel_f <- fld_of("safety.sae.relatedness")
  out_f <- fld_of("safety.sae.status")
  sae_term  <- gcol("safety.sae.term")
  sae_onset <- gcol("safety.sae.onset_date")
  has_sae <- (!is.na(sae_term) & nzchar(sae_term)) |
             (!is.na(sae_onset) & nzchar(sae_onset))
  safety_events <- tibble::tibble(
    participant_id = pid[has_sae],
    type        = "sae",
    term        = sae_term[has_sae],
    onset_date  = parse_date_std(sae_onset[has_sae]),
    report_date = parse_date_std(gcol("safety.sae.report_date")[has_sae]),
    severity    = if (!is.null(sev_f))
      .decode_value(rec[[sev_f]][has_sae], .choices_for(schema, sev_f))
      else rep(NA_character_, sum(has_sae)),
    relatedness = if (!is.null(rel_f))
      .decode_value(rec[[rel_f]][has_sae], .choices_for(schema, rel_f))
      else rep(NA_character_, sum(has_sae)),
    outcome     = if (!is.null(out_f))
      .decode_value(rec[[out_f]][has_sae], .choices_for(schema, out_f))
      else rep(NA_character_, sum(has_sae)),
    narrative   = gcol("safety.sae.narrative")[has_sae]
  )

  # ── deviations ───────────────────────────────────────────────────────────
  dev_term <- gcol("deviation.term")
  dev_date <- gcol("deviation.date")
  has_dev <- (!is.na(dev_term) & nzchar(dev_term)) |
             (!is.na(dev_date) & nzchar(dev_date))
  deviations <- tibble::tibble(
    participant_id = pid[has_dev],
    term        = dev_term[has_dev],
    date        = parse_date_std(dev_date[has_dev]),
    report_date = parse_date_std(gcol("deviation.report_date")[has_dev]),
    narrative   = gcol("deviation.narrative")[has_dev]
  )

  # ── observations: every other mapped concept, long format ───────────────
  core_ids <- c("participant.id", "site.name", "event.name",
                "randomisation.datetime", "randomisation.arm",
                "status_change.type", "status_change.date", "status_change.reason",
                "safety.sae.term", "safety.sae.onset_date", "safety.sae.report_date",
                "safety.sae.severity", "safety.sae.relatedness",
                "safety.sae.status", "safety.sae.narrative",
                "deviation.term", "deviation.date", "deviation.report_date",
                "deviation.narrative")
  obs_rows <- list()
  for (cid in setdiff(names(cmap), core_ids)) {
    f <- fld_of(cid)
    if (is.null(f)) next
    v <- trimws(as.character(rec[[f]]))
    has <- !is.na(v) & nzchar(v)
    if (!any(has)) next
    ch <- .choices_for(schema, f)
    obs_rows[[cid]] <- tibble::tibble(
      participant_id = pid[has],
      visit_id    = visit_role[has],
      concept     = cid,
      value       = v[has],
      value_num   = suppressWarnings(as.numeric(v[has])),
      value_date  = parse_date_std(v[has]),
      value_code  = if (!is.null(ch)) v[has] else NA_character_,
      value_label = .decode_value(v[has], ch)
    )
  }
  observations <- if (length(obs_rows)) dplyr::bind_rows(obs_rows)
  else tibble::tibble(participant_id = character(), visit_id = character(),
                      concept = character(), value = character(),
                      value_num = numeric(), value_date = as.Date(character()),
                      value_code = character(), value_label = character())

  # ── visits (derived: participant × logical visit seen in data) ──────────
  seen <- !is.na(visit_role) & nzchar(visit_role)
  visits <- if (any(seen)) {
    dplyr::distinct(tibble::tibble(
      participant_id = pid[seen], visit_id = visit_role[seen]))
  } else {
    tibble::tibble(participant_id = character(), visit_id = character())
  }

  # Unmapped-event note (unmapped source events pass through under their raw
  # names, so compare against the mapped source names, not visit_role)
  unmapped_ev <- setdiff(unique(visit[!is.na(visit) & nzchar(visit)]),
                         unlist(emap))
  if (length(unmapped_ev))
    note("info", "unmapped_events",
         paste0("Source events not mapped to a visit role: ",
                paste(unmapped_ev, collapse = ", ")),
         length(unmapped_ev))

  list(
    dataset = list(
      participants   = participants,
      visits         = visits,
      form_status    = form_status,
      status_changes = status_changes,
      safety_events  = safety_events,
      deviations     = deviations,
      observations   = observations,
      meta = list(
        built_at    = Sys.time(),
        source      = source_package$source,
        concept_map = cmap,
        event_map   = emap
      )
    ),
    issues = if (length(issues)) dplyr::bind_rows(issues)
    else tibble::tibble(severity = character(), code = character(),
                        message = character(), count = integer())
  )
}

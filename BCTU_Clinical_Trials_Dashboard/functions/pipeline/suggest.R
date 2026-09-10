# =============================================================================
# Mapping suggestion engine
# =============================================================================
# Deterministic, explainable scoring of (concept × source field) pairs
# (docs/ARCHITECTURE.md §6.2). Every suggestion carries its reasons so a
# trials unit can audit why the system proposed a mapping. Replaces the
# hardcoded candidate lists in functions/csv_autodetect.R (those lists now
# seed the concept registry's synonyms).
#
# Evidence combined per candidate:
#   name/label similarity   vs concept synonyms (+ learned synonyms)
#   type compatibility      schema type vs concept's accepted types
#   form context            form name vs concept form_hints
#   value shape             uniqueness for ID concepts (from data, if present)
#   structural priors       adapter conventions (id_field_first, …)
# =============================================================================

#' Normalise a name/label for matching: lowercase, strip punctuation,
#' collapse whitespace/underscores.
.norm_token <- function(x) {
  x <- tolower(trimws(as.character(x %||% "")))
  x <- gsub("[^a-z0-9]+", " ", x)
  trimws(gsub("\\s+", " ", x))
}

#' Similarity of a field name/label against one synonym, in [0, 1].
#' Exact normalised match = 1; token containment scores by coverage.
.token_sim <- function(value, synonym) {
  v <- .norm_token(value); s <- .norm_token(synonym)
  if (!nzchar(v) || !nzchar(s)) return(0)
  if (v == s) return(1)
  vt <- strsplit(v, " ")[[1]]; st <- strsplit(s, " ")[[1]]
  shared <- length(intersect(vt, st))
  if (!shared) return(0)
  # coverage of the synonym's tokens, damped by extra tokens in the value
  cov <- shared / length(st)
  pen <- shared / length(vt)
  0.9 * cov * (0.5 + 0.5 * pen)
}

.best_sim <- function(value, synonyms) {
  if (!length(synonyms)) return(0)
  max(vapply(synonyms, function(s) .token_sim(value, s), numeric(1)))
}

#' Score every source field against every concept.
#'
#' @param source_package adapter output (schema + data + conventions)
#' @param registry       concept_registry()
#' @param learned        learned_synonyms() — confirmed mappings from other trials
#' @return tibble(concept_id, field_name, label, score, reasons), best-first
#'   within each concept, scores in [0, 1].
suggest_mappings <- function(source_package, registry = concept_registry(),
                             learned = learned_synonyms()) {
  schema  <- source_package$schema
  records <- source_package$data$records
  conv    <- source_package$conventions %||% list()
  if (is.null(schema) || !nrow(schema)) return(
    tibble::tibble(concept_id = character(), field_name = character(),
                   label = character(), score = numeric(), reasons = character()))

  # Pre-compute value-shape stats once per field (only for fields present in data)
  shape <- new.env(parent = emptyenv())
  field_stats <- function(fn) {
    st <- shape[[fn]]
    if (!is.null(st)) return(st)
    st <- list(n = 0, unique_frac = NA_real_)
    if (!is.null(records) && fn %in% names(records)) {
      v <- records[[fn]]
      v <- trimws(as.character(v)); v <- v[!is.na(v) & nzchar(v)]
      st$n <- length(v)
      if (length(v)) st$unique_frac <- length(unique(v)) / length(v)
    }
    shape[[fn]] <- st
    st
  }

  out <- vector("list", length(registry))
  names(out) <- names(registry)

  for (cid in names(registry)) {
    cc <- registry[[cid]]
    syns <- unique(c(cc$synonyms, tolower(learned[[cid]] %||% character(0))))

    scores  <- numeric(nrow(schema))
    reasons <- character(nrow(schema))

    for (i in seq_len(nrow(schema))) {
      fn  <- schema$field_name[i]
      lbl <- schema$label[i]
      typ <- schema$type[i]
      frm <- schema$form[i]
      r   <- character(0)

      # Hard veto: incompatible type (when the concept restricts types and the
      # schema type is trusted, i.e. came from a dictionary)
      type_ok <- identical(cc$types, "any") || typ %in% cc$types
      if (!type_ok && !schema$inferred[i]) { scores[i] <- 0; next }

      s_name  <- .best_sim(fn,  syns)
      s_label <- .best_sim(lbl, syns)
      s <- 0.55 * max(s_name, s_label) + 0.15 * min(s_name, s_label)
      if (s_name  >= 0.9) r <- c(r, "name match")
      else if (s_name > 0.3) r <- c(r, "partial name match")
      if (s_label >= 0.9) r <- c(r, "label match")
      else if (s_label > 0.3) r <- c(r, "partial label match")

      if (type_ok && !identical(cc$types, "any")) {
        s <- s + 0.15
        r <- c(r, paste0(typ, " type"))
      } else if (!type_ok) {
        s <- s * 0.4   # inferred type mismatch: damp, don't veto
      }

      if (length(cc$form_hints) && !is.na(frm) && nzchar(frm)) {
        fh <- .norm_token(frm)
        if (any(vapply(cc$form_hints, function(h) grepl(h, fh, fixed = TRUE),
                       logical(1)))) {
          s <- s + 0.15
          r <- c(r, paste0("form '", frm, "'"))
        }
      }

      if (isTRUE(cc$unique)) {
        st <- field_stats(fn)
        if (st$n > 10 && !is.na(st$unique_frac)) {
          if (st$unique_frac > 0.6) { s <- s + 0.15; r <- c(r, "values unique") }
          else                      { s <- s * 0.5;  r <- c(r, "values repeat") }
        }
        # Structural prior: first column is conventionally the record ID
        if (isTRUE(conv$id_field_first) && i == 1 && s > 0) {
          s <- s + 0.1; r <- c(r, "first column")
        }
      }

      scores[i]  <- min(1, s)
      reasons[i] <- paste(r, collapse = " · ")
    }

    keep <- which(scores >= 0.3)
    if (length(keep)) {
      out[[cid]] <- tibble::tibble(
        concept_id = cid,
        field_name = schema$field_name[keep],
        label      = schema$label[keep],
        score      = round(scores[keep], 2),
        reasons    = reasons[keep]
      )
    }
  }

  res <- dplyr::bind_rows(out)
  if (!nrow(res)) return(
    tibble::tibble(concept_id = character(), field_name = character(),
                   label = character(), score = numeric(), reasons = character()))
  res |>
    dplyr::group_by(concept_id) |>
    dplyr::arrange(dplyr::desc(score), .by_group = TRUE) |>
    dplyr::slice_head(n = 5) |>
    dplyr::ungroup()
}

#' Collapse suggestions to one pre-selected candidate per concept.
#' @param min_score auto-select threshold; below it the concept is suggested
#'   but left unconfirmed (the UI shows it unticked).
#' @return tibble(concept_id, field_name, label, score, reasons, auto)
top_suggestions <- function(suggestions, min_score = 0.6) {
  if (!nrow(suggestions)) return(dplyr::mutate(suggestions, auto = logical(0)))
  suggestions |>
    dplyr::group_by(concept_id) |>
    dplyr::slice_max(score, n = 1, with_ties = FALSE) |>
    dplyr::ungroup() |>
    dplyr::mutate(auto = score >= min_score)
}

# ── Event-role suggestions ───────────────────────────────────────────────────
# Events are matched by pattern, as csv_autodetect did, but against the
# adapter-provided event table rather than raw data.

.EVENT_ROLE_PATTERNS <- list(
  baseline  = c("baseline", "enrol", "screen", "randomi"),
  discharge = c("discharge", "day_0", "post_op", "postop"),
  day_30    = c("day.?30", "month.?1[^0-9]", "4.?week", "30.?day", "follow.?up.?1"),
  day_90    = c("day.?90", "month.?3[^0-9]", "12.?week", "90.?day", "follow.?up.?2"),
  sub_forms = c("sae", "adverse", "safety", "ad.?hoc", "sub.?form",
                "protocol.?dev", "change.?of.?status", "withdrawal", "serious")
)

#' Suggest source events for each logical event role.
#' @return named list role → character vector of source event names
suggest_events <- function(source_package) {
  evs <- source_package$events$unique_name %||% character(0)
  if (!length(evs)) return(list())
  lows <- tolower(evs)
  out <- list()
  for (role in names(.EVENT_ROLE_PATTERNS)) {
    pats <- .EVENT_ROLE_PATTERNS[[role]]
    hit <- evs[vapply(lows, function(e) any(vapply(pats, grepl, logical(1), x = e)),
                      logical(1))]
    if (length(hit)) {
      out[[role]] <- if (role == "sub_forms") hit else hit[1]
    }
  }
  out
}

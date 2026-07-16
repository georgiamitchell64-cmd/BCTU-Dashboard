# =============================================================================
# Trial Templates — Pre-configured structures for common trial types
# =============================================================================
# These templates provide sensible defaults so users don't start from scratch.
# The CSV auto-detection will refine these based on actual data.
# =============================================================================

trial_templates <- list(

  # ── NELA Surgery (emergency laparotomy — TONIC-like) ────────────────────
  nela_surgery = list(
    label       = "Emergency Laparotomy (NELA)",
    description = "Emergency abdominal surgery trial with NELA-eligible patients. Typically includes CCI, EQ-5D, QoR-15, and 30/90-day follow-up.",
    icon        = "&#x1FA7A;",

    colors = list(primary = "#1B4F6B", secondary = "#2EC4A5", accent = "#F59E0B"),

    typical_events = list(
      baseline  = "baseline_arm_1",
      discharge = "discharge_arm_1",
      day_30    = "day_30_arm_1",
      day_90    = "day_90_arm_1",
      sub_forms = c("sub_forms_arm_1", "ad_hoc_arm_1")
    ),

    typical_instruments = c("EQ-5D", "QoR-15", "CCI", "PRODIGI",
                            "Patient Satisfaction", "HRUQ", "Barthel"),

    features = list(
      postal_tracking = TRUE, return_rates = TRUE, projections = TRUE,
      pilot_criteria = TRUE, consort_flow = TRUE, baseline_table = TRUE
    ),

    help_text = "Based on the TONIC trial structure. Includes nutrition, morbidity scoring (NELA), and patient-reported outcomes at discharge, 30 days, and 90 days."
  ),

  # ── Elective Surgery ────────────────────────────────────────────────────
  elective_surgery = list(
    label       = "Elective / Planned Surgery",
    description = "Planned surgical procedures with pre-op baseline, post-op assessment, and follow-up at 6 weeks and 6 months.",
    icon        = "&#x1F3E5;",

    colors = list(primary = "#312E81", secondary = "#8B5CF6", accent = "#F59E0B"),

    typical_events = list(
      baseline  = "baseline_arm_1",
      discharge = "discharge_arm_1",
      day_30    = "6_week_arm_1",
      day_90    = "6_month_arm_1",
      sub_forms = c("sub_forms_arm_1")
    ),

    typical_instruments = c("EQ-5D", "SF-36", "VAS Pain", "Clavien-Dindo",
                            "Patient Satisfaction", "HRUQ"),

    features = list(
      postal_tracking = FALSE, return_rates = TRUE, projections = TRUE,
      pilot_criteria = FALSE, consort_flow = TRUE, baseline_table = TRUE
    ),

    help_text = "For planned procedures with pre-operative baseline assessment. Follow-up typically at 6 weeks and 6 months rather than 30/90 days."
  ),

  # ── Colorectal Surgery ─────────────────────────────────────────────────
  colorectal = list(
    label       = "Colorectal Surgery",
    description = "Colorectal procedures with stoma-specific outcomes, bowel function, and quality of life measures.",
    icon        = "&#x1F9EC;",

    colors = list(primary = "#064E3B", secondary = "#10B981", accent = "#FBBF24"),

    typical_events = list(
      baseline  = "baseline_arm_1",
      discharge = "discharge_arm_1",
      day_30    = "day_30_arm_1",
      day_90    = "day_90_arm_1",
      sub_forms = c("sub_forms_arm_1", "ad_hoc_arm_1")
    ),

    typical_instruments = c("EQ-5D", "EORTC QLQ-C30", "EORTC QLQ-CR29",
                            "Wexner Score", "LARS Score", "CCI",
                            "Patient Satisfaction"),

    features = list(
      postal_tracking = TRUE, return_rates = TRUE, projections = TRUE,
      pilot_criteria = FALSE, consort_flow = TRUE, baseline_table = TRUE
    ),

    help_text = "Includes colorectal-specific instruments (LARS, Wexner) alongside general surgical outcomes."
  ),

  # ── Generic / Minimal ──────────────────────────────────────────────────
  generic = list(
    label       = "Generic Trial",
    description = "Minimal assumptions. Just recruitment tracking, site management, and basic follow-up. Best if your trial doesn't fit the surgical templates.",
    icon        = "&#x1F4CB;",

    colors = list(primary = "#334155", secondary = "#06B6D4", accent = "#F97316"),

    typical_events = list(
      baseline  = "baseline_arm_1",
      discharge = "discharge_arm_1"
    ),

    typical_instruments = c("EQ-5D"),

    features = list(
      postal_tracking = FALSE, return_rates = FALSE, projections = TRUE,
      pilot_criteria = FALSE, consort_flow = FALSE, baseline_table = FALSE
    ),

    help_text = "Start here if you're unsure. You can enable features later in Trial Settings."
  )
)

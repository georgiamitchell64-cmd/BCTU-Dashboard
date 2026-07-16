empty_sites <- tibble(
  site_id = character(), site_name = character(), city = character(),
  region = character(), country = character(),
  status = character(), site_open_date = as.Date(character()),
  siv_booked = logical(), siv_date = as.Date(character()),
  monthly_target = integer(), target = integer(), randomised = integer(),
  lat = numeric(), lon = numeric(),
  # "auto"   = created from a REDCap export DAG (process_redcap)
  # "manual" = added by hand (Add site / bulk import)
  source = character()
)
empty_log <- tibble(
  timestamp = as.POSIXct(character()), site_id = character(),
  action = character(), note = character()
)
empty_participants <- tibble(record_id = character(), event_type = character(),
                             site_dag = character(), work_package = integer())

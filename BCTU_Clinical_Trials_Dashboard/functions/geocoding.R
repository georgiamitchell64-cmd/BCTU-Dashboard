# ── geocoding.R ──────────────────────────────────────────────────────────────
#
# Worldwide geocoding for sites using OpenStreetMap Nominatim.
# Free, no API key. Cached in memory to avoid repeat calls.
# Fall-back chain:  UK hospital table  →  cached lookup  →  Nominatim
#
# Rate limit: 1 request/second — we add a small sleep to stay polite.
# ─────────────────────────────────────────────────────────────────────────────

# In-memory cache for the session
.geocode_cache <- new.env(parent = emptyenv())

#' Geocode a location (city + optional country) to lat/lon.
#' @param city     City or hospital name
#' @param country  Country name (optional but recommended)
#' @return list(lat, lon) — or NAs if no match
geocode_location <- function(city, country = NULL) {
  city_clean <- trimws(city %||% "")
  if (!nzchar(city_clean))
    return(list(lat = NA_real_, lon = NA_real_))

  ctry_clean <- trimws(country %||% "")

  # Try the UK hospital fast-path first if no country given or country is UK
  if (!nzchar(ctry_clean) || tolower(ctry_clean) %in% c("uk", "united kingdom", "gb", "great britain")) {
    uk <- tryCatch(hospital_latlon(city_clean), error = function(e) NULL)
    if (!is.null(uk) && !is.na(uk$lat)) return(uk)
  }

  # Air-gap mode: skip Nominatim entirely if the user has disabled outbound
  # geocoding. Set ALLOW_REMOTE_GEOCODING <- FALSE in any sourced file (or in
  # globals/constants.R) to suppress the call. Default is FALSE for safety.
  if (!isTRUE(getOption("BCTU_ALLOW_REMOTE_GEOCODING",
                        if (exists("ALLOW_REMOTE_GEOCODING"))
                          ALLOW_REMOTE_GEOCODING else FALSE))) {
    return(list(lat = NA_real_, lon = NA_real_))
  }

  # Cache key
  key <- tolower(paste(city_clean, ctry_clean, sep = "|"))
  if (exists(key, envir = .geocode_cache)) {
    return(get(key, envir = .geocode_cache))
  }

  # Build Nominatim query
  q <- if (nzchar(ctry_clean)) paste(city_clean, ctry_clean, sep = ", ") else city_clean

  result <- tryCatch({
    Sys.sleep(1)    # respect 1 req/sec rate limit
    url <- paste0(
      "https://nominatim.openstreetmap.org/search?format=json&limit=1&q=",
      utils::URLencode(q, reserved = TRUE)
    )

    con <- url(url, headers = c("User-Agent" = "TONIC-Dashboard/1.0 (BCTU)"))
    txt <- tryCatch(paste(readLines(con, warn = FALSE), collapse = ""),
                    finally = close(con))

    if (!nzchar(txt) || txt == "[]") {
      list(lat = NA_real_, lon = NA_real_)
    } else {
      # Parse without jsonlite dependency — use a regex on the simple response
      lat_m <- regmatches(txt, regexpr('"lat"\\s*:\\s*"[^"]+"', txt))
      lon_m <- regmatches(txt, regexpr('"lon"\\s*:\\s*"[^"]+"', txt))
      if (length(lat_m) == 0 || length(lon_m) == 0) {
        list(lat = NA_real_, lon = NA_real_)
      } else {
        list(
          lat = as.numeric(sub('.*"([^"]+)"', "\\1", lat_m)),
          lon = as.numeric(sub('.*"([^"]+)"', "\\1", lon_m))
        )
      }
    }
  }, error = function(e) {
    message("geocode_location error: ", e$message)
    list(lat = NA_real_, lon = NA_real_)
  })

  assign(key, result, envir = .geocode_cache)
  result
}

#' Common countries for the site form dropdown.
site_countries <- c(
  "United Kingdom", "Ireland",
  "France", "Germany", "Netherlands", "Belgium", "Switzerland",
  "Spain", "Italy", "Portugal", "Denmark", "Sweden", "Norway", "Finland",
  "Poland", "Austria", "Greece", "Czechia",
  "United States", "Canada",
  "Australia", "New Zealand",
  "Japan", "Singapore", "Hong Kong",
  "India", "South Africa",
  "Brazil", "Mexico", "Argentina",
  "Other"
)

# scripts/production/phase1/config.R
# PRODUCTION Phase 1 config -- full-span daily docket collection.
# Separate data/log roots from the pilot so the two can never collide.
#
# No secrets here: the whitelisted GUID is read from the environment.

suppressPackageStartupMessages({ library(fs) })

# ---- Credential ------------------------------------------------------------
OSCN_UA_GUID <- Sys.getenv("OSCN_UA_GUID", unset = NA_character_)
if (is.na(OSCN_UA_GUID) || !nzchar(OSCN_UA_GUID)) {
  stop("OSCN_UA_GUID is not set. Add it to ~/.Renviron and restart R.\n",
       "  OSCN_UA_GUID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx", call. = FALSE)
}
USER_AGENT <- OSCN_UA_GUID   # webmaster whitelisted the GUID as the UA string

# ---- Scope -----------------------------------------------------------------
COUNTIES <- c("adair", "canadian", "cleveland", "comanche", "ellis",
              "garfield", "logan", "oklahoma", "payne", "pushmataha",
              "rogermills", "rogers", "tulsa")

CASE_TYPE_IDS <- c(CF = 31L, CM = 32L, TR = 18L)   # confirmed empirically
CASE_TYPES    <- names(CASE_TYPE_IDS)

# ---- Study span ------------------------------------------------------------
HB2259_DATE <- as.Date("2023-11-01")   # pre/post boundary (approved 2023-05-15)
FULL_START  <- as.Date("2022-01-01")
FULL_END    <- as.Date("2026-06-30")
WEEKDAYS_ONLY <- TRUE

# ---- Rate limiting (1 req/sec agreement) -----------------------------------
MIN_INTERVAL_S <- 1.0
JITTER_MAX_S   <- 0.4

# ---- Paths (PRODUCTION roots -- distinct from pilot) -----------------------
PROJECT_ROOT <- if (requireNamespace("here", quietly = TRUE)) here::here() else normalizePath(getwd())
PROD_ROOT    <- fs::path(PROJECT_ROOT, "data", "production")
CHUNK_ROOT   <- fs::path(PROD_ROOT, "docket_listings")
LOG_DIR      <- fs::path(PROJECT_ROOT, "logs", "production", "phase1")
fs::dir_create(CHUNK_ROOT); fs::dir_create(LOG_DIR)

# ---- Date grid -------------------------------------------------------------
# Weekdays across the full span, each labeled pre/post the HB 2259 boundary.
build_date_grid <- function(start = FULL_START, end = FULL_END) {
  d <- seq(as.Date(start), as.Date(end), by = "day")
  g <- data.frame(date = d, period = ifelse(d < HB2259_DATE, "pre", "post"))
  if (WEEKDAYS_ONLY) g <- g[as.integer(format(g$date, "%u")) <= 5, , drop = FALSE]
  g[order(g$date), , drop = FALSE]
}

# ---- Auto-run guard --------------------------------------------------------
.oscn_should_autorun <- function() {
  !interactive() && !isTRUE(getOption("oscn.autorun.suppress"))
}

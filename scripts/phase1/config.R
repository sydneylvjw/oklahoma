# scripts/phase1/config.R
# Central configuration for the Phase 1 (90-day pre/post HB 2259) OSCN pull.
#
# No secrets live in this file. The whitelisted GUID is read from the
# environment so it never appears in source, git, chat, or logs.

suppressPackageStartupMessages({
  library(fs)
})

# ---- Credential (read from environment, never hardcoded) -------------------
OSCN_UA_GUID <- Sys.getenv("OSCN_UA_GUID", unset = NA_character_)
if (is.na(OSCN_UA_GUID) || !nzchar(OSCN_UA_GUID)) {
  stop(
    "OSCN_UA_GUID is not set. Add it to ~/.Renviron (kept out of the repo):\n",
    "  OSCN_UA_GUID=xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx\n",
    "then restart R. Never paste the GUID into a script or chat.",
    call. = FALSE
  )
}

# The webmaster whitelisted the User-Agent string itself (the GUID), sent via
# the standard User-Agent header -- no custom header.
# >>> If they instead whitelisted a specific descriptive UA string, set that
#     string here instead of the bare GUID. <<<
USER_AGENT <- OSCN_UA_GUID

# ---- Scope -----------------------------------------------------------------
# All 13 OCIS counties (OSCN db codes = lowercase county name, no spaces).
# NOTE: "rogermills" is a best guess for the two-word county -- VERIFY with
# verify_counties() before the full pull (the others match known-good codes).
COUNTIES <- c("adair", "canadian", "cleveland", "comanche", "ellis",
              "garfield", "logan", "oklahoma", "payne", "pushmataha",
              "rogermills", "rogers", "tulsa")

# OSCN CaseTypeID integers for the WebJudicialDocketCaseTypeAll report.
# The URL takes the numeric ID; the label is used for paths/columns.
# Confirmed empirically against OSCN docket pages.
CASE_TYPE_IDS <- c(CF = 31L, CM = 32L, TR = 18L)
CASE_TYPES <- names(CASE_TYPE_IDS)

# HB 2259 took effect 2022-11-01. 90 calendar days on each side:
PRE_START  <- as.Date("2022-08-03")
PRE_END    <- as.Date("2022-10-31")
POST_START <- as.Date("2022-11-01")
POST_END   <- as.Date("2023-01-29")

WEEKDAYS_ONLY <- TRUE   # ability-to-pay dockets don't sit on weekends

# ---- Rate limiting (honoring the 1 req/sec agreement) ----------------------
MIN_INTERVAL_S <- 1.0   # hard floor between request starts
JITTER_MAX_S   <- 0.4   # jitter added on top, so effective rate is < 1/sec

# ---- Paths (match existing repo layout) ------------------------------------
# Machine-independent: anchor to the repo root (finds .git / .Rproj), never a
# hardcoded path -- so it's correct on any machine and safe to commit.
PROJECT_ROOT <- if (requireNamespace("here", quietly = TRUE)) here::here() else normalizePath(getwd())
CHUNK_ROOT   <- fs::path(PROJECT_ROOT, "data", "pilot_chunks")
LOG_DIR      <- fs::path(PROJECT_ROOT, "logs", "phase1")
fs::dir_create(LOG_DIR)

# ---- Date grid -------------------------------------------------------------
build_date_grid <- function() {
  pre  <- seq(PRE_START,  PRE_END,  by = "day")
  post <- seq(POST_START, POST_END, by = "day")
  d <- data.frame(
    date   = c(pre, post),
    period = c(rep("pre", length(pre)), rep("post", length(post)))
  )
  if (WEEKDAYS_ONLY) {
    wd <- as.integer(format(d$date, "%u"))  # 1=Mon ... 7=Sun
    d <- d[wd <= 5, , drop = FALSE]
  }
  d[order(d$date), , drop = FALSE]
}

# ---- Auto-run guard --------------------------------------------------------
# TRUE only when a script is the top-level Rscript job AND auto-run hasn't been
# suppressed. Wrapper scripts set options(oscn.autorun.suppress = TRUE) before
# sourcing worker files so they can control execution order without those files
# firing their own runs at source time.
.oscn_should_autorun <- function() {
  !interactive() && !isTRUE(getOption("oscn.autorun.suppress"))
}

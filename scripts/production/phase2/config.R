# scripts/production/phase2/config.R
# PRODUCTION Phase 2 paths. Sources production Phase 1 config for the
# credential, throttle, and study span.

source("scripts/production/phase1/config.R")   # USER_AGENT, rate limits, PROD_ROOT, HB2259_DATE
source("scripts/production/phase1/http.R")     # oscn_get(), is_challenge()

suppressPackageStartupMessages({ library(fs) })

PHASE1_CHUNK_ROOT <- CHUNK_ROOT                                  # read-only input
DETAIL_ROOT <- fs::path(PROD_ROOT, "case_details")               # raw HTML output
DOCKET_ROOT <- fs::path(PROD_ROOT, "case_docket")                # parsed parquet
CLASSIFIED_ROOT <- fs::path(PROD_ROOT, "case_docket_classified") # classified (regenerable)
DETAIL_LOG  <- fs::path(PROJECT_ROOT, "logs", "production", "phase2")
PARSE_LOG   <- fs::path(PROJECT_ROOT, "logs", "production", "phase2_parse")
fs::dir_create(c(DETAIL_ROOT, DOCKET_ROOT, DETAIL_LOG, PARSE_LOG))

CASE_NUMBER_COL <- "case_number"
DICT_PATH <- "scripts/production/phase2/lfo_codes.csv"   # canonical dictionary

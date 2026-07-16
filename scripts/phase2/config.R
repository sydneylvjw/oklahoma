# scripts/phase2/config.R
# Config for Phase 2 (case-detail fetch). Reuses Phase 1's whitelisted HTTP
# layer and rate limit -- same GUID, same 1/sec, same challenge tripwire.

source("scripts/phase1/config.R")   # USER_AGENT, throttle consts, CHUNK_ROOT, PROJECT_ROOT
source("scripts/phase1/http.R")     # oscn_get(), is_challenge()

suppressPackageStartupMessages({ library(fs) })

PHASE1_CHUNK_ROOT <- CHUNK_ROOT                              # read-only input
DETAIL_ROOT <- fs::path(PROJECT_ROOT, "data", "case_details")   # raw HTML output
DETAIL_LOG  <- fs::path(PROJECT_ROOT, "logs", "phase2")
fs::dir_create(DETAIL_ROOT); fs::dir_create(DETAIL_LOG)

CASE_NUMBER_COL <- "case_number"   # confirmed from inspect_chunks.R

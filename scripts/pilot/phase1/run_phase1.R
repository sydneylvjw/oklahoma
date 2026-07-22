# scripts/pilot/phase1/run_phase1.R
# Full Phase 1 pull: all four counties x CF/CM/TR x the 90-day pre/post window.
# Resume-safe -- rerun after any interruption and it picks up from the manifest.
#
# Run from the project root:  Rscript scripts/pilot/phase1/run_phase1.R

source("scripts/pilot/phase1/pipeline.R")

# Guard: don't launch the ~1,500-request run half-configured.
if (any(is.na(CASE_TYPE_IDS))) {
  stop("CASE_TYPE_IDS has NA values -- fill CF/CM/TR in config.R first.",
       call. = FALSE)
}

grid <- build_date_grid()
full <- make_combos(COUNTIES, CASE_TYPES, grid$date)

if (identical(environment(), globalenv())) {
  message(sprintf("Phase 1: %d counties x %d case types x %d weekdays = %d combos",
                  length(COUNTIES), length(CASE_TYPES), nrow(grid), nrow(full)))
  run_combos(full, "phase1")
}

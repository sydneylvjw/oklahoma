# scripts/production/phase1/run_phase1.R
# PRODUCTION Phase 1: all counties x CF/CM/TR x every weekday in the full span.
# Resume-safe -- rerun after any interruption and it continues from the manifest.
#
# Run inside tmux so a dropped terminal can't kill it:
#   tmux new -s scrape
#   caffeinate -i Rscript scripts/production/phase1/run_phase1.R
#   (detach: Ctrl-b then d   |   reattach: tmux attach -t scrape)

options(oscn.autorun.suppress = TRUE)
source("scripts/production/phase1/pipeline.R")

if (any(is.na(CASE_TYPE_IDS)))
  stop("CASE_TYPE_IDS has NA values -- fix config.R first.", call. = FALSE)

grid <- build_date_grid()
full <- make_combos(COUNTIES, CASE_TYPES, grid$date)

if (!interactive()) {
  message(sprintf("PRODUCTION Phase 1: %d counties x %d case types x %d weekdays (%s to %s) = %s combos",
                  length(COUNTIES), length(CASE_TYPES), nrow(grid),
                  format(min(grid$date)), format(max(grid$date)),
                  format(nrow(full), big.mark = ",")))
  run_combos(full, "phase1")
}

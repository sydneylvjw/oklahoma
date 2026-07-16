# scripts/phase1/run_smoke.R
# Smoke test: one county x one case type x a few days across the 2022-11-01
# boundary. Validates fetch -> parse -> immutable-write before the full run.
#
# Run from the project root:  Rscript scripts/phase1/run_smoke.R

source("scripts/phase1/pipeline.R")

smoke_dates <- as.Date(c("2022-10-27", "2022-10-28", "2022-10-31",
                         "2022-11-01", "2022-11-02", "2022-11-03"))
smoke <- make_combos("tulsa", "CF", smoke_dates)

if (identical(environment(), globalenv())) {
  run_combos(smoke, "smoke")
}
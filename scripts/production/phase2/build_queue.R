# scripts/production/phase2/build_queue.R
# Build the Phase 2 work queue: unique (county, case_type, case_number) from the
# Phase 1 listings. Read-only. Prints the REQUEST BUDGET -- i.e. exactly how many
# detail fetches (and therefore how many seconds) Phase 2 will take.
#
# Run:  Rscript scripts/production/phase2/build_queue.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(arrow); library(dplyr) })
source("scripts/production/phase2/config.R")

build_queue <- function() {
  ds   <- open_dataset(PHASE1_CHUNK_ROOT)
  need <- c("county", "case_type", CASE_NUMBER_COL)
  miss <- setdiff(need, ds$schema$names)
  if (length(miss)) stop("Phase 1 data missing: ", paste(miss, collapse = ", "))

  ds |>
    select(all_of(need)) |>
    collect() |>
    rename(case_number = all_of(CASE_NUMBER_COL)) |>
    mutate(across(everything(), trimws)) |>
    filter(!is.na(case_number), case_number != "") |>
    distinct(county, case_type, case_number) |>          # (county, case_number) is the true key
    arrange(county, case_type, case_number)
}

if (!interactive()) {
  q <- build_queue()
  secs <- nrow(q) * (MIN_INTERVAL_S + JITTER_MAX_S / 2)
  cat(sprintf("\nPHASE 2 REQUEST BUDGET: %s unique cases\n", format(nrow(q), big.mark = ",")))
  cat(sprintf("Estimated fetch time: %.1f hours (%.1f days continuous) at ~%.1fs/req\n",
              secs / 3600, secs / 86400, MIN_INTERVAL_S + JITTER_MAX_S / 2))
  cat(sprintf("Estimated raw HTML: ~%.1f GB gzipped (at ~40KB/case)\n\n",
              nrow(q) * 40e3 / 1e9))
  print(as.data.frame(count(q, county, case_type)), row.names = FALSE)
}

# scripts/pilot/phase2/build_queue.R
# Build the Phase 2 work queue: unique (county, case_type, case_number) from the
# Phase 1 chunks. Read-only. Prints the request budget (unique case count).

suppressPackageStartupMessages({ library(arrow); library(dplyr) })
source("scripts/pilot/phase2/config.R")

build_queue <- function() {
  ds   <- open_dataset(PHASE1_CHUNK_ROOT)
  need <- c("county", "case_type", CASE_NUMBER_COL)
  miss <- setdiff(need, ds$schema$names)
  if (length(miss)) stop("Phase 1 data missing columns: ", paste(miss, collapse = ", "))
  
  ds |>
    select(all_of(need)) |>
    collect() |>
    rename(case_number = all_of(CASE_NUMBER_COL)) |>
    mutate(across(everything(), trimws)) |>
    filter(!is.na(case_number), case_number != "") |>
    distinct(county, case_type, case_number) |>
    arrange(county, case_type, case_number)
}

# No auto-run: build_queue() is a function. Call it explicitly, e.g.
#   q <- build_queue(); nrow(q)
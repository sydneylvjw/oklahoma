# scripts/pilot/phase1/screening_report.R
# OPTION A: pre/post case-VOLUME screen across all Phase 1 counties.
# Read-only. This is the county-selection table for volume/coverage, and the
# sampling frame for Option B.
# Run:  Rscript scripts/pilot/phase1/screening_report.R

suppressPackageStartupMessages({ library(arrow); library(dplyr); library(tidyr) })
source("scripts/pilot/phase1/config.R")

screening_report <- function() {
  d <- open_dataset(CHUNK_ROOT) |>
    select(county, case_type, case_number, query_date) |>
    collect() |>
    mutate(period = ifelse(query_date <= PRE_END, "pre", "post"))

  by_type <- d |>
    distinct(county, case_type, period, case_number) |>
    count(county, case_type, period, name = "n") |>
    pivot_wider(names_from = period, values_from = n, values_fill = 0) |>
    mutate(total = pre + post,
           pct_change = round(100 * (post - pre) / pmax(pre, 1), 1)) |>
    arrange(county, case_type)

  by_county <- by_type |>
    group_by(county) |>
    summarise(pre = sum(pre), post = sum(post), total = sum(total),
              pct_change = round(100 * (post - pre) / pmax(pre, 1), 1),
              .groups = "drop") |>
    arrange(desc(total))

  list(by_county = by_county, by_type = by_type)
}

if (!interactive()) {
  r <- screening_report()
  cat("\n=== Pre/post unique cases by COUNTY ===\n"); print(r$by_county, n = Inf)
  cat("\n=== By county x case type ===\n"); print(r$by_type, n = Inf)
}

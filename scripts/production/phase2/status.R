# scripts/production/phase2/status.R
# Read-only progress: queued vs fetched vs parsed, per county. Safe to run in
# another terminal during the fetch.
# Run:  Rscript scripts/production/phase2/status.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(arrow); library(dplyr); library(fs); library(tidyr) })
source("scripts/production/phase2/config.R")

if (!interactive()) {
  fp <- fs::path(DETAIL_LOG, "manifest.csv")
  if (!fs::file_exists(fp)) { cat("No Phase 2 fetch manifest yet.\n"); quit(status = 0) }
  m <- read.csv(fp, stringsAsFactors = FALSE)

  cat("Fetched:", format(nrow(m), big.mark = ","), "cases\n")
  cat("\nStatus:\n"); print(table(m$status))
  cat("\nRaw HTML on disk:",
      sprintf("%.2f GB", sum(fs::file_size(fs::dir_ls(DETAIL_ROOT, recurse = TRUE,
                                                      glob = "*.html.gz"))) / 1e9), "\n")

  parsed <- if (length(dir(DOCKET_ROOT))) {
    open_dataset(DOCKET_ROOT) |> select(county, case_number) |> collect() |>
      distinct(county, case_number) |> count(county, name = "parsed")
  } else tibble(county = character(), parsed = integer())

  cat("\nBy county (fetched vs parsed):\n")
  m |> count(county, name = "fetched") |>
    full_join(parsed, by = "county") |>
    mutate(across(c(fetched, parsed), ~ tidyr::replace_na(.x, 0L)),
           gap = fetched - parsed) |>
    arrange(desc(fetched)) |> as.data.frame() |> print(row.names = FALSE)

  if (nrow(m) > 1) {
    ts <- as.POSIXct(m$scraped_at, format = "%Y-%m-%dT%H:%M:%S")
    el <- as.numeric(difftime(max(ts), min(ts), units = "secs"))
    if (el > 0) cat(sprintf("\nFetch rate: %.2f s/case\n", el / nrow(m)))
  }
}

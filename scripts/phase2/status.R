# scripts/phase2/status.R
# Reproducible pipeline status: how many cases are fetched vs parsed, per county,
# so you can see exactly where the sample got to. Read-only.
# Run:  Rscript scripts/phase2/status.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(fs); library(tidyr)
})
source("scripts/phase2/config.R")   # DETAIL_ROOT, PROJECT_ROOT
DOCKET_ROOT <- file.path(PROJECT_ROOT, "data", "case_docket")

pipeline_status <- function() {
  # fetched: raw .html.gz on disk, county taken from the folder (ground truth)
  files <- as.character(fs::dir_ls(DETAIL_ROOT, recurse = TRUE, glob = "*.html.gz"))
  fetched <- tibble(path = files) |>
    mutate(county = fs::path_file(fs::path_dir(fs::path_dir(path)))) |>
    count(county, name = "fetched")

  # parsed: distinct cases in case_docket, grouped by the county COLUMN
  # (this is what exposes NA-county rows if any slipped through)
  parsed <- if (dir.exists(DOCKET_ROOT) && length(dir(DOCKET_ROOT))) {
    open_dataset(DOCKET_ROOT) |>
      select(county, case_number) |> collect() |>
      distinct(county, case_number) |>
      count(county, name = "parsed")
  } else {
    tibble(county = character(), parsed = integer())
  }

  full_join(fetched, parsed, by = "county") |>
    mutate(across(c(fetched, parsed), ~ tidyr::replace_na(.x, 0L)),
           gap = fetched - parsed) |>
    arrange(desc(fetched))
}

if (!interactive()) {
  s <- pipeline_status()
  print(s, n = Inf)
  cat("\nfetched total:", sum(s$fetched), " parsed total:", sum(s$parsed), "\n")
  if (any(is.na(s$county) | s$county == "NA"))
    cat("NOTE: an NA-county row means parsed rows whose county column is NA.\n")
}

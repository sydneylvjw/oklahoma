# scripts/production/phase1/status.R
# Read-only progress check -- safe to run in another terminal while the scrape
# is going. Run:  Rscript scripts/production/phase1/status.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(dplyr) })
source("scripts/production/phase1/config.R")

if (!interactive()) {
  p <- file.path(LOG_DIR, "manifest.csv")
  if (!file.exists(p)) { cat("No manifest yet -- run hasn't started.\n"); quit(status = 0) }
  m <- read.csv(p, stringsAsFactors = FALSE)

  grid  <- build_date_grid()
  total <- length(COUNTIES) * length(CASE_TYPES) * nrow(grid)

  cat(sprintf("Progress: %s / %s combos (%.1f%%)\n",
              format(nrow(m), big.mark = ","), format(total, big.mark = ","),
              100 * nrow(m) / total))
  cat("\nStatus counts:\n"); print(table(m$status))
  cat("\nRows collected:", format(sum(m$n_rows, na.rm = TRUE), big.mark = ","), "\n")
  cat("Date reached:  ", max(m$date), "of", format(max(grid$date)), "\n")

  cat("\nBy county:\n")
  m |> group_by(county) |>
    summarise(combos = n(), rows = sum(n_rows, na.rm = TRUE), .groups = "drop") |>
    arrange(desc(rows)) |> as.data.frame() |> print(row.names = FALSE)

  if (nrow(m) > 1) {
    ts <- as.POSIXct(m$ts, format = "%Y-%m-%dT%H:%M:%S")
    el <- as.numeric(difftime(max(ts), min(ts), units = "secs"))
    if (el > 0) {
      rate <- el / nrow(m); rem <- (total - nrow(m)) * rate
      cat(sprintf("\nRate: %.2f s/req | est. remaining: %.1f hours\n", rate, rem / 3600))
    }
  }
}

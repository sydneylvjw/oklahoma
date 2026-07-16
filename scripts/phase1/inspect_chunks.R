# scripts/phase1/inspect_chunks.R
# Read-only profiling of the scraped parquet chunks, to plan cleaning.
# Writes nothing. Run from project root:  Rscript scripts/phase1/inspect_chunks.R
# (or source() it in RStudio and then View(df) for a spreadsheet view.)

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(stringr); library(purrr); library(tibble)
})

source("scripts/phase1/config.R")   # CHUNK_ROOT
stopifnot(dir.exists(CHUNK_ROOT))

# open_dataset unions all chunks; it THROWS if any chunk's schema disagrees,
# which is the cross-county schema-drift tripwire.
ds <- open_dataset(CHUNK_ROOT)
cat("Schema (", length(ds$schema$names), " columns):\n", sep = "")
print(ds$schema)

df <- ds |> collect()
cat("\nTotal rows:", nrow(df), "\n")

if (all(c("county", "case_type") %in% names(df))) {
  cat("\nRows per county/case_type:\n")
  df |> count(county, case_type) |> arrange(county, case_type) |> print(n = Inf)
}

# ---- per-column profile: NA/blank/whitespace/newline/entity + example ------
profile_col <- function(x, nm) {
  chr <- as.character(x)
  tibble(
    column     = nm,
    type       = class(x)[1],
    pct_na     = round(mean(is.na(chr)) * 100, 1),
    pct_blank  = round(mean(!is.na(chr) & trimws(chr) == "") * 100, 1),
    n_distinct = dplyr::n_distinct(chr),
    ws_edges   = sum(!is.na(chr) & chr != trimws(chr)),        # stray leading/trailing ws
    newline    = any(grepl("\n", chr), na.rm = TRUE),          # multi-line cell -> needs flattening
    html_ent   = any(grepl("&[a-zA-Z]+;|&#\\d+;", chr), na.rm = TRUE),  # &nbsp; &amp; etc.
    example    = { v <- chr[!is.na(chr) & trimws(chr) != ""]; if (length(v)) substr(v[1], 1, 45) else NA }
  )
}
cat("\n--- Column profile ---\n")
imap_dfr(df, profile_col) |> print(n = Inf, width = Inf)

# ---- date-like columns: format consistency --------------------------------
is_datey <- function(x) any(grepl("\\d{1,4}[-/]\\d{1,2}[-/]\\d{1,4}", as.character(x)), na.rm = TRUE)
date_cols <- names(df)[map_lgl(df, is_datey)]
if (length(date_cols)) {
  cat("\n--- Date-like columns (watch for mixed formats) ---\n")
  for (nm in date_cols) {
    v <- as.character(df[[nm]])
    fmt <- ifelse(grepl("^\\d{4}-\\d{2}-\\d{2}", v), "YYYY-MM-DD",
           ifelse(grepl("^\\d{1,2}/\\d{1,2}/\\d{4}", v), "M/D/YYYY", "other"))
    cat(nm, ":\n"); print(table(fmt, useNA = "ifany"))
  }
}

# ---- case-number column + cross-date duplication --------------------------
has_case <- function(x) any(grepl("\\b(CF|CM|TR)-\\d{4}-\\d+", as.character(x)), na.rm = TRUE)
case_cols <- names(df)[map_lgl(df, has_case)]
if (length(case_cols)) {
  cn <- case_cols[1]
  cat("\n--- Case-number column:", cn, "---\n")
  cat("Unique cases:", n_distinct(df[[cn]]), " | total rows:", nrow(df), "\n")
  cat("Exact duplicate rows:", sum(duplicated(df)), "\n")

  # A case can have docket events on many days, so the SAME case appears under
  # multiple query dates. That's expected for daily-docket scraping -- but it
  # means "one row = one case-day", not "one row = one case". Plan dedup for
  # any case-level analysis in Phase 2.
  dcol <- intersect(c("query_date", "filed_date", "date"), names(df))
  if (length(dcol)) {
    multi <- df |> distinct(.data[[cn]], .data[[dcol[1]]]) |>
      count(.data[[cn]], name = "n_dates") |> filter(n_dates > 1)
    cat("Cases appearing on >1 query date:", nrow(multi),
        "(", round(100 * nrow(multi) / n_distinct(df[[cn]]), 1), "% of cases)\n")
  }
}

cat("\nDone. Read-only -- nothing was written.\n")

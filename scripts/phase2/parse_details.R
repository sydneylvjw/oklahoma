# scripts/phase2/parse_details.R
# Stage 2 driver: parse every stored case-detail page into immutable parquet
# chunks. Local, no network, resume-safe (manifest keyed on source file).
# Fault-tolerant: a bad page is logged as parse_error, never aborts the batch.
#
# Run from project root:  Rscript scripts/phase2/parse_details.R
# Parallel (optional):    R -e 'source("scripts/phase2/parse_details.R"); library(furrr); plan(multisession); run_parse(parallel=TRUE)'

suppressPackageStartupMessages({ library(arrow); library(fs); library(purrr); library(dplyr) })
source("scripts/phase2/config.R")
source("scripts/phase2/parse_case_detail.R")

DOCKET_ROOT <- fs::path(PROJECT_ROOT, "data", "case_docket")
PARSE_LOG   <- fs::path(PROJECT_ROOT, "logs", "phase2_parse")
fs::dir_create(DOCKET_ROOT); fs::dir_create(PARSE_LOG)
MANI_COLS <- c("source_file","county","case_type","case_number","n_rows","status","chunk")

pm_path     <- function() fs::path(PARSE_LOG, "manifest.csv")
load_parsed <- function() {
  p <- pm_path(); if (!fs::file_exists(p)) return(character(0))
  utils::read.csv(p, stringsAsFactors = FALSE)$source_file
}
record_parsed <- function(rows) {
  p <- pm_path(); exists <- fs::file_exists(p)
  utils::write.table(rows[, MANI_COLS], p, sep = ",", row.names = FALSE,
                     col.names = !exists, append = exists)
}

chunk_seq <- function(county, case_type) {
  d <- fs::path(DOCKET_ROOT, county, case_type); fs::dir_create(d)
  ex <- fs::dir_ls(d, glob = "*.parquet")
  if (!length(ex)) return(1L)
  max(as.integer(sub("^chunk_(\\d+)\\.parquet$", "\\1", fs::path_file(ex))), na.rm = TRUE) + 1L
}
write_docket_chunk <- function(df, county, case_type) {
  d <- fs::path(DOCKET_ROOT, county, case_type); fs::dir_create(d)
  path <- fs::path(d, sprintf("chunk_%04d.parquet", chunk_seq(county, case_type)))
  tmp <- paste0(path, ".tmp"); arrow::write_parquet(df, tmp); fs::file_move(tmp, path)
  path
}

run_parse <- function(batch_size = 300, parallel = FALSE) {
  done   <- load_parsed()
  mapper <- if (parallel && requireNamespace("furrr", quietly = TRUE)) furrr::future_map else purrr::map
  safe   <- purrr::possibly(parse_case_file, otherwise = NULL)

  counties <- fs::path_file(fs::dir_ls(DETAIL_ROOT, type = "directory"))
  for (cty in counties) {
    ctypes <- fs::path_file(fs::dir_ls(fs::path(DETAIL_ROOT, cty), type = "directory"))
    for (ct in ctypes) {
      files <- as.character(fs::dir_ls(fs::path(DETAIL_ROOT, cty, ct), glob = "*.html.gz"))
      todo  <- files[!(fs::path_file(files) %in% done)]
      if (!length(todo)) next
      message(sprintf("[parse] %s/%s: %d to parse", cty, ct, length(todo)))
      batches <- split(todo, ceiling(seq_along(todo) / batch_size))
      for (fb in batches) {
        parsed <- mapper(fb, safe)
        ok <- !vapply(parsed, is.null, logical(1))
        if (any(ok)) {
          df   <- dplyr::bind_rows(parsed[ok])
          path <- write_docket_chunk(df, cty, ct)          # write chunk first
          man  <- df |> count(source_file, county, case_type, case_number, name = "n_rows") |>
                        mutate(status = "ok", chunk = as.character(path))
          record_parsed(man)                                # then record -> resume-safe
        }
        if (any(!ok)) {
          record_parsed(data.frame(
            source_file = fs::path_file(fb[!ok]), county = cty, case_type = ct,
            case_number = NA_character_, n_rows = 0L, status = "parse_error",
            chunk = NA_character_, stringsAsFactors = FALSE))
        }
        message(sprintf("   +%d ok, %d failed", sum(ok), sum(!ok)))
      }
    }
  }
  message("[parse] done")
}

if (!interactive()) run_parse()

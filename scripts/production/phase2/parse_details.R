# scripts/production/phase2/parse_details.R
# PRODUCTION Phase 2 stage 2: parse stored pages -> immutable parquet chunks.
# Local, no network, resume-safe, fault-tolerant (a bad page is logged, never
# aborts the batch). County/case_type/case_number are stamped from the folder +
# filename (ground truth), never trusted from page metadata.
#
# Run:      Rscript scripts/production/phase2/parse_details.R
# Parallel: Rscript -e 'source("scripts/production/phase2/parse_details.R"); library(furrr); plan(multisession); run_parse(parallel=TRUE)'

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(arrow); library(fs); library(purrr); library(dplyr) })
source("scripts/production/phase2/config.R")
source("scripts/production/phase2/parse_case_detail.R")

MANI_COLS <- c("source_file","county","case_type","case_number","n_rows","status","chunk")
pm_path <- function() fs::path(PARSE_LOG, "manifest.csv")

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
  path <- fs::path(d, sprintf("chunk_%05d.parquet", chunk_seq(county, case_type)))
  tmp <- paste0(path, ".tmp"); arrow::write_parquet(df, tmp); fs::file_move(tmp, path)
  path
}

run_parse <- function(batch_size = 500, parallel = FALSE) {
  done   <- load_parsed()
  mapper <- if (parallel && requireNamespace("furrr", quietly = TRUE)) furrr::future_map else purrr::map
  safe   <- purrr::possibly(parse_case_file, otherwise = NULL)
  t0 <- Sys.time(); n_done <- 0L

  for (cty in fs::path_file(fs::dir_ls(DETAIL_ROOT, type = "directory"))) {
    for (ct in fs::path_file(fs::dir_ls(fs::path(DETAIL_ROOT, cty), type = "directory"))) {
      files <- as.character(fs::dir_ls(fs::path(DETAIL_ROOT, cty, ct), glob = "*.html.gz"))
      todo  <- files[!(fs::path_file(files) %in% done)]
      if (!length(todo)) next
      message(sprintf("[parse] %s/%s: %s to parse", cty, ct, format(length(todo), big.mark = ",")))

      for (fb in split(todo, ceiling(seq_along(todo) / batch_size))) {
        parsed <- mapper(fb, safe)
        ok <- !vapply(parsed, is.null, logical(1))
        if (any(ok)) {
          df <- dplyr::bind_rows(parsed[ok])
          # authoritative provenance from folder + filename
          df$county    <- cty
          df$case_type <- ct
          df$case_number <- ifelse(is.na(df$case_number) | df$case_number == "",
                                   sub("\\.html\\.gz$", "", df$source_file), df$case_number)
          path <- write_docket_chunk(df, cty, ct)
          record_parsed(df |> count(source_file, county, case_type, case_number, name = "n_rows") |>
                          mutate(status = "ok", chunk = as.character(path)))
        }
        if (any(!ok)) record_parsed(data.frame(
          source_file = fs::path_file(fb[!ok]), county = cty, case_type = ct,
          case_number = NA_character_, n_rows = 0L, status = "parse_error",
          chunk = NA_character_, stringsAsFactors = FALSE))
        n_done <- n_done + length(fb)
        message(sprintf("   +%d ok, %d failed | %s parsed | %.1f min elapsed",
                        sum(ok), sum(!ok), format(n_done, big.mark = ","),
                        as.numeric(difftime(Sys.time(), t0, units = "mins"))))
      }
    }
  }
  message("[parse] done -- ", format(n_done, big.mark = ","), " files")
}

if (.oscn_should_autorun()) run_parse()

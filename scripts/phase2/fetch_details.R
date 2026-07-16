# scripts/phase2/fetch_details.R
# Phase 2, stage 1: fetch each case's detail page and store the RAW HTML,
# write-once (one gzipped file per case). Parsing/classifying is a separate
# later pass over these files -- so this rate-limited run happens exactly once.
#
# Resume-safe via an append-only manifest keyed on (county, case_number).
# CONFIRM the detail URL with a single-case test before the full run (see chat).
# Run from project root:  Rscript scripts/phase2/fetch_details.R

suppressPackageStartupMessages({ library(httr2); library(fs) })
source("scripts/phase2/config.R")
source("scripts/phase2/build_queue.R")

detail_url <- function(county, case_number) {
  sprintf("https://www.oscn.net/dockets/GetCaseInformation.aspx?db=%s&number=%s",
          county, utils::URLencode(case_number, reserved = TRUE))
}

detail_path <- function(county, case_type, case_number) {
  fs::path(DETAIL_ROOT, county, case_type, paste0(case_number, ".html.gz"))
}

manifest2_path <- function() fs::path(DETAIL_LOG, "manifest.csv")
d_key <- function(county, case_number) paste(county, case_number, sep = "|")

load_done2 <- function() {
  p <- manifest2_path()
  if (!fs::file_exists(p)) return(character(0))
  utils::read.csv(p, stringsAsFactors = FALSE)$key
}

record2 <- function(county, case_type, case_number, status, bytes, path) {
  p <- manifest2_path(); exists <- fs::file_exists(p)
  row <- data.frame(
    key = d_key(county, case_number), county = county, case_type = case_type,
    case_number = case_number, status = status, bytes = bytes,
    path = if (is.null(path)) NA_character_ else as.character(path),
    scraped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = FALSE
  )
  utils::write.table(row, p, sep = ",", row.names = FALSE,
                     col.names = !exists, append = exists)
}

# write-once: temp file, then atomic rename (never modify an existing file)
write_html_gz <- function(html, path) {
  fs::dir_create(fs::path_dir(path))
  tmp <- paste0(path, ".tmp")
  con <- gzfile(tmp, "wb"); writeBin(charToRaw(html), con); close(con)
  fs::file_move(tmp, path)
  path
}

fetch_one_detail <- function(county, case_type, case_number) {
  url  <- detail_url(county, case_number)
  resp <- tryCatch(oscn_get(url), error = function(e) e)
  if (inherits(resp, "error")) return(list(status = "error", html = NULL))
  status <- httr2::resp_status(resp)
  html   <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  if (is_challenge(status, html)) {
    stop("Turnstile/403 on detail fetch despite the whitelist -- stopping.\nURL: ",
         url, call. = FALSE)
  }
  if (status >= 400) return(list(status = "error", html = NULL))
  if (is.na(html) || nchar(html) < 500) return(list(status = "empty", html = NULL))
  list(status = "ok", html = html)
}

run_details <- function(queue = build_queue()) {
  done <- load_done2()
  message(sprintf("[phase2] %d cases; %d already done", nrow(queue),
                  sum(d_key(queue$county, queue$case_number) %in% done)))
  for (i in seq_len(nrow(queue))) {
    r <- queue[i, ]
    if (d_key(r$county, r$case_number) %in% done) next
    res  <- fetch_one_detail(r$county, r$case_type, r$case_number)  # may stop() on challenge
    path <- NULL; bytes <- 0L
    if (identical(res$status, "ok")) {
      path  <- write_html_gz(res$html, detail_path(r$county, r$case_type, r$case_number))
      bytes <- nchar(res$html)
    }
    record2(r$county, r$case_type, r$case_number, res$status, bytes, path)
    if (i %% 100 == 0) message(sprintf("  ... %d/%d", i, nrow(queue)))
  }
  message("[phase2] done")
}

# Auto-run ONLY as a script (Rscript); never on an interactive source().
if (!interactive()) {
  run_details()
}
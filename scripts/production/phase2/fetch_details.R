# scripts/production/phase2/fetch_details.R
# PRODUCTION Phase 2 stage 1: fetch each case's detail page, store RAW HTML
# write-once. Parsing/classifying happen later, offline -- so this long,
# rate-limited run happens exactly once even if the parser changes.
#
# Resume-safe: manifest keyed on (county, case_number). Safe to stop/restart.
# This run may take DAYS -- launch detached:
#   nohup caffeinate -i Rscript scripts/production/phase2/fetch_details.R \
#     > logs/production/phase2/run.log 2>&1 &

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(httr2); library(fs) })
source("scripts/production/phase2/config.R")
source("scripts/production/phase2/build_queue.R")

detail_url <- function(county, case_number)
  sprintf("https://www.oscn.net/dockets/GetCaseInformation.aspx?db=%s&number=%s",
          county, utils::URLencode(case_number, reserved = TRUE))

detail_path <- function(county, case_type, case_number)
  fs::path(DETAIL_ROOT, county, case_type, paste0(case_number, ".html.gz"))

manifest2_path <- function() fs::path(DETAIL_LOG, "manifest.csv")
d_key <- function(county, case_number) paste(county, case_number, sep = "|")

load_done2 <- function() {
  p <- manifest2_path()
  if (!fs::file_exists(p)) return(character(0))
  utils::read.csv(p, stringsAsFactors = FALSE)$key
}

record2 <- function(county, case_type, case_number, status, bytes, path) {
  p <- manifest2_path(); exists <- fs::file_exists(p)
  utils::write.table(data.frame(
    key = d_key(county, case_number), county = county, case_type = case_type,
    case_number = case_number, status = status, bytes = bytes,
    path = if (is.null(path)) NA_character_ else as.character(path),
    scraped_at = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), stringsAsFactors = FALSE),
    p, sep = ",", row.names = FALSE, col.names = !exists, append = exists)
}

write_html_gz <- function(html, path) {
  fs::dir_create(fs::path_dir(path))
  tmp <- paste0(path, ".tmp")
  con <- gzfile(tmp, "wb"); writeBin(charToRaw(html), con); close(con)
  fs::file_move(tmp, path)   # atomic: never a partial file
  path
}

fetch_one_detail <- function(county, case_number) {
  url  <- detail_url(county, case_number)
  resp <- tryCatch(oscn_get(url), error = function(e) e)
  if (inherits(resp, "error")) return(list(status = "error", html = NULL))
  st   <- httr2::resp_status(resp)
  html <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  if (is_challenge(st, html))
    stop("Turnstile/403 despite the whitelist -- STOPPING.\nURL: ", url, call. = FALSE)
  if (st >= 400) return(list(status = "error", html = NULL))
  if (is.na(html) || nchar(html) < 500) return(list(status = "empty", html = NULL))
  list(status = "ok", html = html)
}

.fmt_hms <- function(s) { s <- max(0, round(s))
  sprintf("%02d:%02d:%02d", s %/% 3600, (s %% 3600) %/% 60, s %% 60) }

run_details <- function(queue = build_queue(), report_every = 200L) {
  done <- load_done2()
  todo <- queue[!(d_key(queue$county, queue$case_number) %in% done), , drop = FALSE]
  message(sprintf("[phase2] %s cases | %s done | %s to fetch | est %s",
                  format(nrow(queue), big.mark = ","),
                  format(nrow(queue) - nrow(todo), big.mark = ","),
                  format(nrow(todo), big.mark = ","),
                  .fmt_hms(nrow(todo) * (MIN_INTERVAL_S + JITTER_MAX_S / 2))))
  if (!nrow(todo)) { message("[phase2] nothing to do."); return(invisible(NULL)) }

  t0 <- Sys.time(); tally <- c(ok = 0L, empty = 0L, error = 0L); bytes_total <- 0
  for (i in seq_len(nrow(todo))) {
    r <- todo[i, ]
    res <- fetch_one_detail(r$county, r$case_number)   # stops on challenge
    path <- NULL; b <- 0L
    if (identical(res$status, "ok")) {
      path <- write_html_gz(res$html, detail_path(r$county, r$case_type, r$case_number))
      b <- nchar(res$html); bytes_total <- bytes_total + b
    }
    record2(r$county, r$case_type, r$case_number, res$status, b, path)
    tally[res$status] <- tally[res$status] + 1L

    if (i %% report_every == 0 || i == nrow(todo)) {
      el <- as.numeric(difftime(Sys.time(), t0, units = "secs")); rate <- el / i
      message(sprintf(
        "  [phase2] %s/%s (%.2f%%) | %s elapsed | ETA %s | ok %s empty %s err %s | ~%.1f GB raw | %s",
        format(i, big.mark = ","), format(nrow(todo), big.mark = ","),
        100 * i / nrow(todo), .fmt_hms(el), .fmt_hms(rate * (nrow(todo) - i)),
        format(tally[["ok"]], big.mark = ","), format(tally[["empty"]], big.mark = ","),
        format(tally[["error"]], big.mark = ","), bytes_total / 1e9, r$county))
    }
  }
  message("[phase2] complete in ",
          .fmt_hms(as.numeric(difftime(Sys.time(), t0, units = "secs"))))
}

if (!interactive()) run_details()

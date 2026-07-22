# scripts/production/phase1/pipeline.R
# Shared production pipeline: URL construction, fetch+parse, provenance, and the
# resume-safe orchestration loop with progress/ETA reporting.

suppressPackageStartupMessages({ library(httr2) })

source("scripts/production/phase1/config.R")
source("scripts/production/phase1/http.R")
source("scripts/production/phase1/chunk_writer.R")
source("scripts/parser.R")   # validated listing parser: parse_oscn_page()

build_url <- function(county, case_type_id, date) {
  sprintf(
    paste0("https://www.oscn.net/applications/oscn/report.asp",
           "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
           "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
           "&GeneralNumber=1&generalnumber1=1"),
    county, case_type_id, format(as.Date(date), "%Y-%m-%d")
  )
}

# Pipeline (not the parser) is authoritative for provenance.
stamp_provenance <- function(df, county, case_type_label, case_type_id,
                             query_date, scraped_at, run_label) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df$county       <- county
  df$case_type    <- case_type_label
  df$case_type_id <- as.integer(case_type_id)
  df$query_date   <- as.Date(query_date)
  df$period       <- ifelse(as.Date(query_date) < HB2259_DATE, "pre", "post")
  df$scraped_at   <- scraped_at
  df$scrape_run   <- run_label
  df
}

# Returns list(status, data); status: ok | empty | error | parse_error.
fetch_and_parse <- function(county, case_type_label, date, run_label = NA_character_) {
  id <- CASE_TYPE_IDS[[case_type_label]]
  if (is.na(id)) stop("CASE_TYPE_IDS[['", case_type_label, "']] is NA.", call. = FALSE)
  url <- build_url(county, id, date)

  scraped_at <- Sys.time()
  resp <- tryCatch(oscn_get(url), error = function(e) e)
  if (inherits(resp, "error")) return(list(status = "error", data = NULL))

  status <- httr2::resp_status(resp)
  body   <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")

  if (is_challenge(status, body)) {
    stop("OSCN bot-challenge (HTTP ", status, ") despite the whitelist -- STOPPING.\n",
         "Re-verify the GUID/User-Agent with the webmaster.\nURL: ", url, call. = FALSE)
  }
  if (status >= 400) return(list(status = "error", data = NULL))
  if (is.na(body) || nchar(body) < 500) return(list(status = "empty", data = NULL))

  parsed <- tryCatch(parse_oscn_page(body, query_date = as.Date(date), county = county),
                     error = function(e) NULL)
  if (is.null(parsed)) return(list(status = "parse_error", data = NULL))

  list(status = "ok",
       data = stamp_provenance(parsed, county, case_type_label, id, date,
                               scraped_at, run_label))
}

make_combos <- function(counties, case_types, dates) {
  g <- expand.grid(county = counties, case_type = case_types,
                   date = dates, stringsAsFactors = FALSE)
  g$date <- as.Date(g$date, origin = "1970-01-01")
  g$key  <- mapply(manifest_key, g$county, g$case_type, g$date)
  # date-ordered: an interrupted run leaves coverage balanced across counties
  g[order(g$date, g$county, g$case_type), , drop = FALSE]
}

.fmt_hms <- function(secs) {
  secs <- max(0, round(secs))
  sprintf("%02d:%02d:%02d", secs %/% 3600, (secs %% 3600) %/% 60, secs %% 60)
}

# Progress every `report_every` requests, with rate and ETA -- essential on a
# 15+ hour run where silence is otherwise indistinguishable from a hang.
run_combos <- function(combos, label, report_every = 100L) {
  completed <- load_completed()
  todo <- combos[!(combos$key %in% completed), , drop = FALSE]
  message(sprintf("[%s] %d combos total | %d done | %d to fetch | est %s at ~%.1fs/req",
                  label, nrow(combos), nrow(combos) - nrow(todo), nrow(todo),
                  .fmt_hms(nrow(todo) * (MIN_INTERVAL_S + JITTER_MAX_S / 2)),
                  MIN_INTERVAL_S + JITTER_MAX_S / 2))
  if (!nrow(todo)) { message("[", label, "] nothing to do."); return(invisible(NULL)) }

  t0 <- Sys.time(); n_rows_total <- 0L; tally <- c(ok = 0L, empty = 0L, error = 0L, parse_error = 0L)

  for (i in seq_len(nrow(todo))) {
    r   <- todo[i, ]
    res <- fetch_and_parse(r$county, r$case_type, r$date, run_label = label)
    n   <- if (is.null(res$data)) 0L else nrow(res$data)
    p   <- if (n > 0) write_chunk(res$data, r$county, r$case_type) else NULL
    record_completed(r$county, r$case_type, r$date, res$status, n, p)

    tally[res$status] <- tally[res$status] + 1L
    n_rows_total <- n_rows_total + n

    if (i %% report_every == 0 || i == nrow(todo)) {
      el   <- as.numeric(difftime(Sys.time(), t0, units = "secs"))
      rate <- el / i
      message(sprintf(
        "  [%s] %d/%d (%.1f%%) | %s elapsed | ETA %s | rows %s | ok %d empty %d err %d parse_err %d | at %s",
        label, i, nrow(todo), 100 * i / nrow(todo), .fmt_hms(el),
        .fmt_hms(rate * (nrow(todo) - i)), format(n_rows_total, big.mark = ","),
        tally[["ok"]], tally[["empty"]], tally[["error"]], tally[["parse_error"]],
        format(r$date)))
    }
  }
  message(sprintf("[%s] complete in %s | %s rows",
                  label, .fmt_hms(as.numeric(difftime(Sys.time(), t0, units = "secs"))),
                  format(n_rows_total, big.mark = ",")))
}

# scripts/pilot/phase1/pipeline.R
# Shared pipeline for the Phase 1 OSCN pull.
# Sourced by run_smoke.R and run_phase1.R -- no side effects beyond defining
# functions and sourcing the (idempotent) config/http/writer/parser.

suppressPackageStartupMessages({ library(httr2); library(tibble) })

source("scripts/pilot/phase1/config.R")
source("scripts/pilot/phase1/http.R")
source("scripts/pilot/phase1/chunk_writer.R")
source("scripts/parser.R")   # validated parser: parse_oscn_page()

# ---- OSCN request construction ---------------------------------------------
build_url <- function(county, case_type_id, date) {
  sprintf(
    paste0("https://www.oscn.net/applications/oscn/report.asp",
           "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
           "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
           "&GeneralNumber=1&generalnumber1=1"),
    county, case_type_id, format(as.Date(date), "%Y-%m-%d")
  )
}

# ---- Provenance -------------------------------------------------------------
# The pipeline -- not the parser -- is authoritative for provenance, so every
# row carries what it was scraped under regardless of parser internals. Cheap
# now, impossible to backfill once this points at multiple years.
stamp_provenance <- function(df, county, case_type_label, case_type_id,
                             query_date, scraped_at, run_label) {
  if (is.null(df) || nrow(df) == 0) return(df)
  df$county       <- county
  df$case_type    <- case_type_label
  df$case_type_id <- as.integer(case_type_id)   # exact CaseTypeID param queried
  df$query_date   <- as.Date(query_date)        # the StartDate this pull used
  df$scraped_at   <- scraped_at                 # when OSCN was hit (page state)
  df$scrape_run   <- run_label                  # which pass: smoke | phase1 | ...
  df
}

# Returns list(status, data). status in: ok | empty | error | parse_error.
# A challenge (403/Turnstile) HALTS the run -- with a working whitelist that
# means the whitelist failed, and you want to stop, not scrape challenges.
fetch_and_parse <- function(county, case_type_label, date,
                            run_label = NA_character_) {
  id <- CASE_TYPE_IDS[[case_type_label]]
  if (is.na(id)) {
    stop("CASE_TYPE_IDS[['", case_type_label, "']] is NA -- fill it in config.R.",
         call. = FALSE)
  }
  url <- build_url(county, id, date)
  
  scraped_at <- Sys.time()                       # captured at request time
  resp <- tryCatch(oscn_get(url), error = function(e) e)
  if (inherits(resp, "error")) {
    message("  net error ", county, "/", case_type_label, "/", date, ": ",
            conditionMessage(resp))
    return(list(status = "error", data = NULL))
  }
  
  status <- httr2::resp_status(resp)
  body   <- tryCatch(httr2::resp_body_string(resp), error = function(e) "")
  
  if (is_challenge(status, body)) {
    stop("OSCN returned a bot-challenge (HTTP ", status, " / Turnstile) despite ",
         "the whitelist.\nStop and re-verify the GUID / User-Agent with the ",
         "webmaster.\nURL: ", url, call. = FALSE)
  }
  if (status >= 400) {
    message("  HTTP ", status, " ", county, "/", case_type_label, "/", date)
    return(list(status = "error", data = NULL))
  }
  if (is.na(body) || nchar(body) < 500) {
    return(list(status = "empty", data = NULL))
  }
  
  parsed <- tryCatch(
    parse_oscn_page(body, query_date = as.Date(date), county = county),
    error = function(e) { message("  parser error: ", conditionMessage(e)); NULL }
  )
  if (is.null(parsed)) return(list(status = "parse_error", data = NULL))
  
  parsed <- stamp_provenance(parsed, county, case_type_label, id,
                             date, scraped_at, run_label)
  list(status = "ok", data = parsed)
}

# ---- Orchestration ---------------------------------------------------------
make_combos <- function(counties, case_types, dates) {
  g <- expand.grid(county = counties, case_type = case_types,
                   date = dates, stringsAsFactors = FALSE)
  g$date <- as.Date(g$date, origin = "1970-01-01")
  g$key  <- mapply(manifest_key, g$county, g$case_type, g$date)
  g[order(g$date, g$county, g$case_type), , drop = FALSE]
}

run_combos <- function(combos, label) {
  completed <- load_completed()
  message(sprintf("[%s] %d combos; %d already complete",
                  label, nrow(combos), sum(combos$key %in% completed)))
  for (i in seq_len(nrow(combos))) {
    row <- combos[i, ]
    if (row$key %in% completed) next
    res  <- fetch_and_parse(row$county, row$case_type, row$date, run_label = label)
    df   <- res$data
    n    <- if (is.null(df)) 0L else nrow(df)
    path <- if (n > 0) write_chunk(df, row$county, row$case_type) else NULL
    record_completed(row$county, row$case_type, row$date, res$status, n, path)
    message(sprintf("  %-11s %s -> %d rows", res$status, row$key, n))
  }
}
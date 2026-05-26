# fetch_one_combo.R — scrape one (county, case_type) combination
# 
# Writes one parquet file per N successful fetches (a "chunk") to:
#   data/pilot_chunks/{county_lower}_{case_type}/chunk_{NNNN}.parquet
# 
# Each chunk file is written ONCE and never modified. Resume-safe.
# 
# Usage (called from pilot.R or directly for testing):
#   fetch_one_combo(county = "Tulsa", case_type_id = 31, case_type_label = "CF",
#                   start_date = as.Date("2022-09-01"), 
#                   end_date = as.Date("2022-12-30"))

library(httr2)
library(dplyr)
library(arrow)
library(stringr)
library(fs)

source("scripts/parser.R")

fetch_one_combo <- function(county, case_type_id, case_type_label,
                            start_date, end_date,
                            chunk_size = 10,
                            pause_minutes_initial = 60,
                            pause_minutes_max = 240,
                            max_consecutive_403s = 5) {
  
  chunk_dir <- file.path("data", "pilot_chunks", tolower(county), case_type_label)
  dir_create(chunk_dir, recurse = TRUE)
  
  message("\n=== Starting ", county, "/", case_type_label, " ===")
  message("Chunk directory: ", chunk_dir)
  
  # ---- Helpers (local closures) ----
  build_url <- function(date) {
    sprintf(
      paste0("https://www.oscn.net/applications/oscn/report.asp",
             "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
             "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
             "&GeneralNumber=1&generalnumber1=1"),
      county, case_type_id, format(date, "%Y-%m-%d")
    )
  }
  
  fetch_and_parse <- function(date) {
    url <- build_url(date)
    message("[", county, "/", case_type_label, "] Fetching ", date)
    
    resp <- tryCatch(
      request(url) |>
        req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15") |>
        req_retry(max_tries = 2, backoff = ~ 2) |>
        req_timeout(30) |>
        req_perform(),
      error = function(e) {
        msg <- e$message
        message("  failed: ", msg)
        if (grepl("403", msg)) return("RATE_LIMITED")
        return(NULL)
      }
    )
    
    if (identical(resp, "RATE_LIMITED")) return("RATE_LIMITED")
    if (is.null(resp)) {
      return(tibble(query_date = as.character(date), county = county, 
                    case_type = case_type_label, error = TRUE))
    }
    
    body <- resp_body_string(resp)
    
    # NEW: Detect Cloudflare Turnstile challenge page
    if (grepl("OSCN Turnstile|cf-turnstile|challenges\\.cloudflare\\.com", body)) {
      message("  ⚠ Cloudflare Turnstile detected — treating as rate limit")
      return("RATE_LIMITED")
    }
    
    # Empty body check
    if (is.na(body) || nchar(body) < 500) {
      return(tibble(query_date = as.character(date), county = county,
                    case_type = case_type_label, empty = TRUE,
                    body_length = nchar(body)))
    }
    
    parsed <- tryCatch(
      parse_oscn_page(body, query_date = date, county = county),
      error = function(e) {
        message("  parser error: ", e$message)
        tibble(query_date = as.character(date), county = county,
               case_type = case_type_label, parse_error = TRUE)
      }
    )
    parsed$case_type <- case_type_label
    parsed
  }
  
  write_chunk <- function(results_list, chunk_num) {
    chunk_data <- bind_rows(results_list)
    if (nrow(chunk_data) == 0) return(invisible(NULL))
    
    chunk_path <- file.path(chunk_dir, sprintf("chunk_%04d.parquet", chunk_num))
    
    # Defensive: verify by reading back immediately
    write_parquet(chunk_data, chunk_path)
    verify <- read_parquet(chunk_path)
    
    if (nrow(verify) != nrow(chunk_data)) {
      stop("CRITICAL: write/read mismatch for chunk ", chunk_num,
           ". Wrote ", nrow(chunk_data), " rows, read back ", nrow(verify))
    }
    
    message("  ✓ Chunk ", chunk_num, " written: ", basename(chunk_path),
            " (", nrow(chunk_data), " rows, ", ncol(chunk_data), " cols, ",
            round(file_size(chunk_path) / 1024, 1), " KB)")
    
    chunk_path
  }
  
  # ---- Determine which dates need scraping ----
  all_dates <- seq(start_date, end_date, by = "day")
  all_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
  
  # Read all existing chunks to find done dates
  existing_chunks <- dir_ls(chunk_dir, glob = "*.parquet")
  done_dates <- if (length(existing_chunks) > 0) {
    existing_chunks |>
      purrr::map_dfr(\(f) read_parquet(f) |> select(query_date)) |>
      pull(query_date) |>
      unique() |>
      as.Date()
  } else {
    as.Date(character(0))
  }
  
  dates_to_fetch <- all_dates[!all_dates %in% done_dates]
  message("Date range: ", start_date, " to ", end_date,
          " (", length(all_dates), " weekdays)")
  message("Already done: ", length(done_dates))
  message("To fetch: ", length(dates_to_fetch))
  
  if (length(dates_to_fetch) == 0) {
    message("=== ", county, "/", case_type_label, ": already complete ===")
    return(invisible())
  }
  
  # ---- Determine next chunk number ----
  next_chunk_num <- if (length(existing_chunks) > 0) {
    existing_nums <- str_match(basename(existing_chunks), "chunk_(\\d+)\\.parquet")[, 2] |>
      as.integer()
    max(existing_nums, na.rm = TRUE) + 1
  } else {
    1
  }
  
  # ---- Main scrape loop ----
  results <- list()
  consecutive_403s <- 0
  current_pause <- pause_minutes_initial
  
  i <- 1
  while (i <= length(dates_to_fetch)) {
    result <- fetch_and_parse(dates_to_fetch[i])
    
    # 403 handling: write what we have, then pause
    if (identical(result, "RATE_LIMITED")) {
      if (length(results) > 0) {
        write_chunk(results, next_chunk_num)
        next_chunk_num <- next_chunk_num + 1
        results <- list()
      }
      
      consecutive_403s <- consecutive_403s + 1
      
      if (consecutive_403s > max_consecutive_403s) {
        message("\n!!! [", county, "/", case_type_label, "] Hit ",
                max_consecutive_403s, " consecutive 403s. Stopping.")
        return(invisible(FALSE))
      }
      
      pause_until <- Sys.time() + (current_pause * 60)
      message("\n!!! 403 (#", consecutive_403s, "). Pausing ", current_pause,
              " min, resuming at ", format(pause_until, "%H:%M:%S"))
      Sys.sleep(current_pause * 60)
      current_pause <- min(current_pause * 2, pause_minutes_max)
      next
    }
    
    # Successful fetch (even if empty-day)
    consecutive_403s <- 0
    current_pause <- pause_minutes_initial
    results[[length(results) + 1]] <- result
    
    Sys.sleep(runif(1, 12, 18))
    
    # Write chunk every chunk_size fetches
    if (length(results) >= chunk_size) {
      write_chunk(results, next_chunk_num)
      next_chunk_num <- next_chunk_num + 1
      results <- list()
      message("  Progress: ", i, "/", length(dates_to_fetch),
              " (", round(100 * i / length(dates_to_fetch), 1), "%)")
      gc()
    }
    
    i <- i + 1
  }
  
  # Flush any remaining
  if (length(results) > 0) {
    write_chunk(results, next_chunk_num)
  }
  
  message("=== Finished ", county, "/", case_type_label, " ===\n")
  invisible(TRUE)
}
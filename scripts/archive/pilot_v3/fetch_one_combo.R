# fetch_one_combo.R — v3 worker
#
# Pilot v3 changes vs v2:
# - Heavy-tailed sleep jitter (mimics human browsing rhythm)
# - Identifying User-Agent (signals "research, not abuse" to Cloudflare)
# - Between-combo cooldown
# - More browser-like request headers
# - Immutable chunk writes (one parquet per chunk, never overwritten)
# - Detects Cloudflare Turnstile pages and treats as rate limit

library(httr2)
library(dplyr)
library(arrow)
library(stringr)
library(fs)

source("scripts/parser.R")

# IMPORTANT: replace [your-email] with your actual email
RESEARCH_USER_AGENT <- "OSCN-research-bot/1.0 (Sydney Jones, Master's research on Rule 8 hearings; contact: sydneyjw@upenn.edu)"

# Browser-like headers — looks less like a bare scripted client
BROWSER_HEADERS <- list(
  "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
  "Accept-Language" = "en-US,en;q=0.9",
  "Accept-Encoding" = "gzip, deflate, br",
  "Connection" = "keep-alive",
  "Upgrade-Insecure-Requests" = "1"
)

fetch_one_combo <- function(county, case_type_id, case_type_label,
                            start_date, end_date,
                            chunk_size = 10,
                            pause_minutes_initial = 60,
                            pause_minutes_max = 240,
                            max_consecutive_403s = 5) {
  
  chunk_dir <- file.path("data", "pilot_v3_chunks", tolower(county), case_type_label)
  dir_create(chunk_dir, recurse = TRUE)
  
  message("\n=== Starting ", county, "/", case_type_label, " ===")
  message("Chunk directory: ", chunk_dir)
  
  # ---- Local helpers ----
  
  build_url <- function(date) {
    sprintf(
      paste0("https://www.oscn.net/applications/oscn/report.asp",
             "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
             "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
             "&GeneralNumber=1&generalnumber1=1"),
      county, case_type_id, format(date, "%Y-%m-%d")
    )
  }
  
  # Heavy-tailed sleep — humans don't browse at uniform rate
  human_sleep <- function() {
    r <- runif(1)
    delay <- if (r < 0.70) {
      runif(1, 8, 25)             # normal "read the page"
    } else if (r < 0.90) {
      runif(1, 30, 90)            # "got distracted briefly"
    } else {
      runif(1, 180, 600)          # "walked away from computer"
    }
    Sys.sleep(delay)
    invisible(delay)
  }
  
  fetch_and_parse <- function(date) {
    url <- build_url(date)
    message("[", county, "/", case_type_label, "] Fetching ", date)
    
    req <- request(url) |>
      req_user_agent(RESEARCH_USER_AGENT) |>
      req_headers(!!!BROWSER_HEADERS) |>
      req_retry(max_tries = 2, backoff = ~ 2) |>
      req_timeout(30)
    
    resp <- tryCatch(
      req_perform(req),
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
    
    # Detect Cloudflare Turnstile challenge page
    if (grepl("OSCN Turnstile|cf-turnstile|challenges\\.cloudflare\\.com", body)) {
      message("  ⚠ Cloudflare Turnstile detected — treating as rate limit")
      return("RATE_LIMITED")
    }
    
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
    
    write_parquet(chunk_data, chunk_path)
    
    # Defensive: verify by reading back
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
    return(invisible(TRUE))
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
    
    if (identical(result, "RATE_LIMITED")) {
      if (length(results) > 0) {
        write_chunk(results, next_chunk_num)
        next_chunk_num <- next_chunk_num + 1
        results <- list()
      }
      
      consecutive_403s <- consecutive_403s + 1
      
      if (consecutive_403s > max_consecutive_403s) {
        message("\n!!! [", county, "/", case_type_label, "] Hit ",
                max_consecutive_403s, " consecutive blocks. Stopping this combo.")
        return(invisible(FALSE))
      }
      
      pause_until <- Sys.time() + (current_pause * 60)
      message("\n!!! Block (#", consecutive_403s, "). Pausing ", current_pause,
              " min, resuming at ", format(pause_until, "%H:%M:%S"))
      Sys.sleep(current_pause * 60)
      current_pause <- min(current_pause * 2, pause_minutes_max)
      next
    }
    
    consecutive_403s <- 0
    current_pause <- pause_minutes_initial
    results[[length(results) + 1]] <- result
    
    human_sleep()
    
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
  
  if (length(results) > 0) {
    write_chunk(results, next_chunk_num)
  }
  
  message("=== Finished ", county, "/", case_type_label, " ===\n")
  invisible(TRUE)
}
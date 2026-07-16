# ocsn_scrape_multi.R — with checkpointing and 403 auto-pause
library(rvest); library(purrr); library(dplyr); library(httr2); library(arrow)

# ---- config ----
counties   <- c("Oklahoma", "Tulsa", "Cleveland")  # Comanche already done
case_type  <- 31
start_date <- as.Date("2022-01-01")
end_date   <- as.Date("2025-12-31")

# 403 backoff config
pause_minutes_initial <- 60
pause_minutes_max     <- 240
max_consecutive_403s  <- 5

# ---- helpers ----
build_url <- function(date, db) {
  sprintf(
    paste0("https://www.oscn.net/applications/oscn/report.asp",
           "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
           "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
           "&GeneralNumber=1&generalnumber1=1"),
    db, case_type, format(date, "%Y-%m-%d")
  )
}

fetch_docket <- function(date, db) {
  url <- build_url(date, db)
  message("[", db, "] Fetching ", date)
  
  resp <- tryCatch(
    request(url) |>
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15") |>
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
    return(tibble(query_date = as.character(date), county = db, error = TRUE))
  }
  
  page <- read_html(resp_body_string(resp))
  tables <- page |> html_elements("table") |> html_table(fill = TRUE)
  if (length(tables) == 0) {
    return(tibble(query_date = as.character(date), county = db, empty = TRUE))
  }
  
  docket <- tables |>
    keep(\(t) ncol(t) >= 3 && nrow(t) >= 1) |>
    purrr::pluck(which.max(map_int(tables, nrow)), .default = NULL)
  
  if (is.null(docket)) {
    return(tibble(query_date = as.character(date), county = db, empty = TRUE))
  }
  
  docket |>
    mutate(query_date = as.character(date), county = db) |>
    mutate(across(everything(), as.character))
}

save_checkpoint <- function(results, checkpoint_path) {
  new_data <- bind_rows(results)
  if (nrow(new_data) == 0) return(invisible())
  
  if (file.exists(checkpoint_path)) {
    prior <- read_parquet(checkpoint_path)
    combined <- bind_rows(prior, new_data)
  } else {
    combined <- new_data
  }
  write_parquet(combined, checkpoint_path)
  message("  Checkpoint saved — total rows on disk: ", nrow(combined))
}

# ---- date range (weekdays only) ----
all_dates <- seq(start_date, end_date, by = "day")
all_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]

# ---- outer loop: counties ----
for (cty in counties) {
  out_path   <- sprintf("data/oscn_%s_CT%d.parquet", tolower(cty), case_type)
  checkpoint <- sprintf("data/oscn_%s_CT%d_checkpoint.parquet", tolower(cty), case_type)
  
  if (file.exists(out_path)) {
    message("=== Skipping ", cty, " (already exists: ", out_path, ") ===")
    next
  }
  
  message("\n=== Starting ", cty, " ===\n")
  
  # Resume from this county's checkpoint if one exists
  done_dates <- if (file.exists(checkpoint)) {
    message("Resuming from checkpoint: ", checkpoint)
    prior <- read_parquet(checkpoint)
    as.Date(unique(prior$query_date))
  } else {
    as.Date(character(0))
  }
  
  dates <- all_dates[!all_dates %in% done_dates]
  message("Dates remaining for ", cty, ": ", length(dates))
  
  if (length(dates) == 0) {
    # All dates already done in checkpoint — finalize and move on
    file.rename(checkpoint, out_path)
    message("=== Finished ", cty, " (from existing checkpoint) → ", out_path, " ===\n")
    next
  }
  
  # ---- inner loop with 403 handling ----
  results <- list()
  consecutive_403s <- 0
  current_pause <- pause_minutes_initial
  gave_up <- FALSE
  
  i <- 1
  while (i <= length(dates)) {
    result <- fetch_docket(dates[i], cty)
    
    if (identical(result, "RATE_LIMITED")) {
      if (length(results) > 0) {
        save_checkpoint(results, checkpoint)
        results <- list()
      }
      
      consecutive_403s <- consecutive_403s + 1
      
      if (consecutive_403s > max_consecutive_403s) {
        message("\n!!! [", cty, "] Hit ", max_consecutive_403s, 
                " consecutive 403s after backoff.")
        message("!!! Stopping ", cty, " cleanly. Data saved to ", checkpoint)
        message("!!! Re-run later to resume this county.")
        gave_up <- TRUE
        break
      }
      
      pause_until <- Sys.time() + (current_pause * 60)
      message("\n!!! [", cty, "] 403 detected (failure #", consecutive_403s, ")")
      message("!!! Pausing for ", current_pause, " minutes, resuming at ",
              format(pause_until, "%H:%M:%S"))
      Sys.sleep(current_pause * 60)
      
      current_pause <- min(current_pause * 2, pause_minutes_max)
      next  # retry same date
    }
    
    # Success or non-403 failure — reset backoff
    consecutive_403s <- 0
    current_pause <- pause_minutes_initial
    
    results[[length(results) + 1]] <- result
    Sys.sleep(runif(1, 4, 7))
    
    if (length(results) >= 25) {
      save_checkpoint(results, checkpoint)
      results <- list()
      message("  [", cty, "] Progress: ", i, "/", length(dates),
              " (", round(100 * i / length(dates), 1), "%)")
      gc()
    }
    
    i <- i + 1
  }
  
  # Final save for this county
  if (length(results) > 0) save_checkpoint(results, checkpoint)
  
  if (!gave_up) {
    file.rename(checkpoint, out_path)
    message("=== Finished ", cty, " → ", out_path, " ===\n")
  } else {
    message("=== Stopped early on ", cty, ". Partial data in: ", checkpoint, " ===\n")
    # Continue to next county anyway — don't let one stuck county block the others
  }
}

message("\nAll counties processed.")
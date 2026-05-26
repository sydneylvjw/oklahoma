# pilot.R — 4 counties × 2 case types × ~85 days (HB 2259 straddle window)
# Each (county, case_type) gets its own parquet file with embedded county + case_type columns

library(httr2)
library(dplyr)
library(arrow)
library(stringr)

source("scripts/parser.R")

# ---- config ----
counties <- c("Oklahoma", "Tulsa", "Cleveland", "Comanche")
case_types <- list(
  list(id = 31, label = "CF"),
  list(id = 32, label = "CM")
)
start_date <- as.Date("2022-09-01")
end_date   <- as.Date("2022-12-30")

# 403 backoff config
pause_minutes_initial <- 60
pause_minutes_max     <- 240
max_consecutive_403s  <- 5

# ---- helpers ----
build_url <- function(date, db, case_type_id) {
  sprintf(
    paste0("https://www.oscn.net/applications/oscn/report.asp",
           "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
           "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
           "&GeneralNumber=1&generalnumber1=1"),
    db, case_type_id, format(date, "%Y-%m-%d")
  )
}

fetch_and_parse <- function(date, db, case_type_id, case_type_label) {
  url <- build_url(date, db, case_type_id)
  message("[", db, "/", case_type_label, "] Fetching ", date)
  
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
    return(tibble(query_date = as.character(date), county = db, 
                  case_type = case_type_label, error = TRUE))
  }
  
  body <- resp_body_string(resp)
  if (is.na(body) || nchar(body) < 500) {
    return(tibble(query_date = as.character(date), county = db,
                  case_type = case_type_label, empty = TRUE))
  }
  
  # Wrap parser in tryCatch so a bad page doesn't crash the whole run
  parsed <- tryCatch(
    parse_oscn_page(body, query_date = date, county = db),
    error = function(e) {
      message("  parser error: ", e$message)
      tibble(query_date = as.character(date), county = db,
             case_type = case_type_label, parse_error = TRUE)
    }
  )
  parsed$case_type <- case_type_label
  parsed
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

# ---- weekday dates ----
all_dates <- seq(start_date, end_date, by = "day")
all_dates <- all_dates[!weekdays(all_dates) %in% c("Saturday", "Sunday")]
message("Date range: ", start_date, " to ", end_date, " — ", length(all_dates), " weekdays")

# ---- outer loop: (county, case_type) combinations ----
for (cty in counties) {
  for (ct in case_types) {
    case_type_id    <- ct$id
    case_type_label <- ct$label
    
    out_path <- sprintf("data/pilot_%s_%s.parquet", 
                        tolower(cty), case_type_label)
    checkpoint <- sprintf("data/pilot_%s_%s_checkpoint.parquet", 
                          tolower(cty), case_type_label)
    
    if (file.exists(out_path)) {
      message("\n=== Skipping ", cty, "/", case_type_label, 
              " (already complete: ", out_path, ") ===")
      next
    }
    
    message("\n=== Starting ", cty, "/", case_type_label, " ===\n")
    
    # Resume from this checkpoint if it exists
    done_dates <- if (file.exists(checkpoint)) {
      message("Resuming from checkpoint: ", checkpoint)
      prior <- read_parquet(checkpoint)
      as.Date(unique(prior$query_date))
    } else {
      as.Date(character(0))
    }
    
    dates <- all_dates[!all_dates %in% done_dates]
    message("Dates remaining for ", cty, "/", case_type_label, ": ", length(dates))
    
    if (length(dates) == 0) {
      file.rename(checkpoint, out_path)
      message("=== Finished ", cty, "/", case_type_label, " (from checkpoint) ===")
      next
    }
    
    # ---- inner loop with 403 handling ----
    results <- list()
    consecutive_403s <- 0
    current_pause <- pause_minutes_initial
    gave_up <- FALSE
    
    i <- 1
    while (i <= length(dates)) {
      result <- fetch_and_parse(dates[i], cty, case_type_id, case_type_label)
      
      if (identical(result, "RATE_LIMITED")) {
        if (length(results) > 0) {
          save_checkpoint(results, checkpoint)
          results <- list()
        }
        
        consecutive_403s <- consecutive_403s + 1
        
        if (consecutive_403s > max_consecutive_403s) {
          message("\n!!! [", cty, "/", case_type_label, "] Hit ", 
                  max_consecutive_403s, " consecutive 403s. Stopping this combo.")
          gave_up <- TRUE
          break
        }
        
        pause_until <- Sys.time() + (current_pause * 60)
        message("\n!!! [", cty, "/", case_type_label, "] 403 detected (#", 
                consecutive_403s, ")")
        message("!!! Pausing ", current_pause, " min, resuming at ",
                format(pause_until, "%H:%M:%S"))
        Sys.sleep(current_pause * 60)
        
        current_pause <- min(current_pause * 2, pause_minutes_max)
        next
      }
      
      consecutive_403s <- 0
      current_pause <- pause_minutes_initial
      
      results[[length(results) + 1]] <- result
      Sys.sleep(runif(1, 4, 7))
      
      # Checkpoint every 10 days
      if (length(results) >= 10) {
        save_checkpoint(results, checkpoint)
        results <- list()
        message("  [", cty, "/", case_type_label, "] Progress: ", i, "/", 
                length(dates), " (", round(100 * i / length(dates), 1), "%)")
        gc()
      }
      
      i <- i + 1
    }
    
    if (length(results) > 0) save_checkpoint(results, checkpoint)
    
    if (!gave_up && file.exists(checkpoint)) {
      file.rename(checkpoint, out_path)
      message("=== Finished ", cty, "/", case_type_label, 
              " → ", out_path, " ===\n")
    } else {
      message("=== Stopped early on ", cty, "/", case_type_label, 
              ". Partial in ", checkpoint, " ===\n")
      # Continue to next combo
    }
  }
}

message("\nAll pilot combinations processed.")
# smoke_test.R — 1 county × 1 case type × 30 days
# Validates the full scrape → parse → save pipeline before launching the pilot

library(httr2)
library(dplyr)
library(arrow)
library(stringr)

# Load the validated parser
source("scripts/parser.R")

# ---- config ----
db          <- "Oklahoma"
case_type   <- 31           # CF
case_label  <- "CF"
start_date  <- as.Date("2022-09-01")
end_date    <- as.Date("2022-10-14")  # ~30 weekdays
out_path    <- sprintf("data/smoke_%s_%s.parquet", tolower(db), case_label)
checkpoint  <- sprintf("data/smoke_%s_%s_checkpoint.parquet", tolower(db), case_label)

# 403 backoff config
pause_minutes_initial <- 60
pause_minutes_max     <- 240
max_consecutive_403s  <- 5

# ---- helpers ----
build_url <- function(date, db, case_type) {
  sprintf(
    paste0("https://www.oscn.net/applications/oscn/report.asp",
           "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
           "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
           "&GeneralNumber=1&generalnumber1=1"),
    db, case_type, format(date, "%Y-%m-%d")
  )
}

fetch_and_parse <- function(date, db, case_type, county_label) {
  url <- build_url(date, db, case_type)
  message("[", county_label, "/", case_label, "] Fetching ", date)
  
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
    return(tibble(query_date = as.character(date), county = county_label, 
                  case_type = case_label, error = TRUE))
  }
  
  body <- resp_body_string(resp)
  if (is.na(body) || nchar(body) < 500) {
    return(tibble(query_date = as.character(date), county = county_label,
                  case_type = case_label, empty = TRUE))
  }
  
  # Parse using validated parser
  parsed <- parse_oscn_page(body, query_date = date, county = county_label)
  parsed$case_type <- case_label
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

# ---- date range ----
dates <- seq(start_date, end_date, by = "day")
dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

# Resume from checkpoint if it exists
done_dates <- if (file.exists(checkpoint)) {
  message("Resuming from checkpoint: ", checkpoint)
  prior <- read_parquet(checkpoint)
  as.Date(unique(prior$query_date))
} else {
  as.Date(character(0))
}

dates <- dates[!dates %in% done_dates]
message("Dates remaining: ", length(dates))

# ---- main loop with 403 handling ----
results <- list()
consecutive_403s <- 0
current_pause <- pause_minutes_initial

i <- 1
while (i <= length(dates)) {
  result <- fetch_and_parse(dates[i], db, case_type, db)
  
  if (identical(result, "RATE_LIMITED")) {
    if (length(results) > 0) {
      save_checkpoint(results, checkpoint)
      results <- list()
    }
    
    consecutive_403s <- consecutive_403s + 1
    
    if (consecutive_403s > max_consecutive_403s) {
      message("\n!!! Hit ", max_consecutive_403s, " consecutive 403s. Stopping.")
      break
    }
    
    pause_until <- Sys.time() + (current_pause * 60)
    message("\n!!! 403 detected (failure #", consecutive_403s, ")")
    message("!!! Pausing for ", current_pause, " minutes, resuming at ",
            format(pause_until, "%H:%M:%S"))
    Sys.sleep(current_pause * 60)
    
    current_pause <- min(current_pause * 2, pause_minutes_max)
    next
  }
  
  consecutive_403s <- 0
  current_pause <- pause_minutes_initial
  
  results[[length(results) + 1]] <- result
  Sys.sleep(runif(1, 4, 7))
  
  # Checkpoint every 10 days for the smoke test
  if (length(results) >= 10) {
    save_checkpoint(results, checkpoint)
    results <- list()
    message("  Progress: ", i, "/", length(dates), 
            " (", round(100 * i / length(dates), 1), "%)")
    gc()
  }
  
  i <- i + 1
}

if (length(results) > 0) save_checkpoint(results, checkpoint)

# Rename checkpoint to final output
if (i > length(dates) && file.exists(checkpoint)) {
  file.rename(checkpoint, out_path)
  message("\nDone: ", out_path)
}
# library(rvest)
# library(purrr)
# library(dplyr)
# library(httr2)
# 
# # ===== This first scrape pulls for Comanche County only, case type 31 (criminal felonies), court activity on each specified day between 2018 and early 2026 =====
# 
# # --- config ---
# db          <- "Comanche"
# case_type   <- 31
# start_date  <- as.Date("2022-01-01")
# end_date    <- as.Date("2025-12-31")  # adjust as needed
# out_path    <- "data/oscn_comanche_CF.parquet"
# 
# # --- helpers ---
# build_url <- function(date) {
#   # OSCN expects YYYY-M-D (no zero-padding on month/day in your example),
#   # but YYYY-MM-DD also works. Using padded form is safer.
#   sprintf(
#     paste0("https://www.oscn.net/applications/oscn/report.asp",
#            "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
#            "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
#            "&GeneralNumber=1&generalnumber1=1"),
#     db, case_type, format(date, "%Y-%m-%d")
#   )
# }
# 
# fetch_docket <- function(date) {
#   url <- build_url(date)
#   message("Fetching ", date)
#   
#   # be polite + handle transient failures
#   resp <- tryCatch(
#     request(url) |>
#       req_user_agent("research-scraper (sydney; academic use)") |>
#       req_retry(max_tries = 3, backoff = ~ 2) |>
#       req_timeout(30) |>
#       req_perform(),
#     error = function(e) { message("  failed: ", e$message); return(NULL) }
#   )
#   if (is.null(resp)) return(tibble(query_date = date, error = TRUE))
#   
#   page <- read_html(resp_body_string(resp))
#   
#   # OSCN renders results in HTML tables — grab them all, pick the docket one.
#   # On an empty day, html_table() may return zero or just header tables.
#   tables <- page |> html_elements("table") |> html_table(fill = TRUE)
#   if (length(tables) == 0) return(tibble(query_date = date, empty = TRUE))
#   
#   # Heuristic: the real docket table tends to be the largest one with
#   # a "Case Number" or similar column. Inspect the first run and adjust.
#   docket <- tables |>
#     keep(\(t) ncol(t) >= 3 && nrow(t) >= 1) |>
#     purrr::pluck(which.max(map_int(tables, nrow)), .default = NULL)
#   
#   if (is.null(docket)) return(tibble(query_date = date, empty = TRUE))
#   
#   docket |>
#     mutate(query_date = date) |>
#     mutate(across(everything(), as.character))  # safe binding across days
#   
#   # polite delay handled outside via Sys.sleep
# }
# 
# # --- run ---
# dates <- seq(start_date, end_date, by = "day")
# 
# results <- map(dates, \(d) {
#   out <- fetch_docket(d)
#   Sys.sleep(runif(1, 1.5, 3.0))  # jittered delay between requests
#   out
# })
# 
# combined <- bind_rows(results)
# 
# # --- write out (your usual DuckDB → parquet pattern works too) ---
# arrow::write_parquet(combined, out_path)


# # ocsn_scrape.R — with checkpointing
# library(rvest); library(purrr); library(dplyr); library(httr2); library(arrow)
# 
# db          <- "Comanche"
# case_type   <- 31
# start_date  <- as.Date("2022-01-01")
# end_date    <- as.Date("2025-12-31")
# out_path    <- "data/oscn_comanche_CT31.parquet"
# checkpoint  <- "data/oscn_comanche_CT31_checkpoint.parquet"
# 
# build_url <- function(date) {
#   sprintf(
#     paste0("https://www.oscn.net/applications/oscn/report.asp",
#            "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
#            "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
#            "&GeneralNumber=1&generalnumber1=1"),
#     db, case_type, format(date, "%Y-%m-%d")
#   )
# }
# 
# fetch_docket <- function(date) {
#   url <- build_url(date)
#   message("Fetching ", date)
#   
#   resp <- tryCatch(
#     request(url) |>
#       req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15") |>
#       req_retry(max_tries = 3, backoff = ~ 2) |>
#       req_timeout(30) |>
#       req_perform(),
#     error = function(e) { message("  failed: ", e$message); return(NULL) }
#   )
#   if (is.null(resp)) return(tibble(query_date = date, county = db, error = TRUE))
#   
#   page <- read_html(resp_body_string(resp))
#   tables <- page |> html_elements("table") |> html_table(fill = TRUE)
#   if (length(tables) == 0) return(tibble(query_date = date, county = db, empty = TRUE))
#   
#   docket <- tables |>
#     keep(\(t) ncol(t) >= 3 && nrow(t) >= 1) |>
#     purrr::pluck(which.max(map_int(tables, nrow)), .default = NULL)
#   
#   if (is.null(docket)) return(tibble(query_date = date, county = db, empty = TRUE))
#   
#   docket |>
#     mutate(query_date = date, county = db) |>
#     mutate(across(everything(), as.character))
# }
# 
# # --- resumable: load checkpoint if exists, skip dates already done ---
# dates <- seq(start_date, end_date, by = "day")
# dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]
# 
# done_dates <- if (file.exists(checkpoint)) {
#   message("Resuming from checkpoint: ", checkpoint)
#   prior <- read_parquet(checkpoint)
#   as.Date(unique(prior$query_date))
# } else {
#   as.Date(character(0))
# }
# 
# dates <- dates[!dates %in% done_dates]
# message("Dates remaining: ", length(dates))
# 
# # --- run with frequent checkpoints ---
# results <- list()
# for (i in seq_along(dates)) {
#   results[[i]] <- fetch_docket(dates[i])
#   Sys.sleep(runif(1, 4, 7))   # slower to avoid 403s
#   
#   # Checkpoint every 25 requests (~3 minutes of work, max)
#   if (i %% 25 == 0) {
#     new_data <- bind_rows(results)
#     if (file.exists(checkpoint)) {
#       prior <- read_parquet(checkpoint)
#       combined <- bind_rows(prior, new_data)
#     } else {
#       combined <- new_data
#     }
#     write_parquet(combined, checkpoint)
#     message("  Checkpoint: ", i, "/", length(dates), 
#             " — total rows on disk: ", nrow(combined))
#     results <- list()  # clear in-memory after writing
#     gc()
#   }
# }
# 
# # --- final write ---
# if (length(results) > 0) {
#   final_batch <- bind_rows(results)
#   if (file.exists(checkpoint)) {
#     prior <- read_parquet(checkpoint)
#     combined <- bind_rows(prior, final_batch)
#   } else {
#     combined <- final_batch
#   }
#   write_parquet(combined, checkpoint)
# }
# 
# # rename checkpoint to final
# file.rename(checkpoint, out_path)
# message("Done: ", out_path)



# ocsn_scrape.R — with checkpointing and 403 auto-pause ----
## ---- setup ----

library(rvest); library(purrr); library(dplyr); library(httr2); library(arrow)

db          <- "Comanche"
case_type   <- 31
start_date  <- as.Date("2022-01-01")
end_date    <- as.Date("2025-12-31")
out_path    <- "data/oscn_comanche_CT31.parquet"
checkpoint  <- "data/oscn_comanche_CT31_checkpoint.parquet"


# ---- functions ----
build_url <- function(date) {
  sprintf(
    paste0("https://www.oscn.net/applications/oscn/report.asp",
           "?report=WebJudicialDocketCaseTypeAll&errorcheck=true",
           "&database=&db=%s&CaseTypeID=%d&StartDate=%s",
           "&GeneralNumber=1&generalnumber1=1"),
    db, case_type, format(date, "%Y-%m-%d")
  )
}



fetch_docket <- function(date) {
  url <- build_url(date)
  message("Fetching ", date)
  
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

# ---- resumable: load checkpoint if exists, skip dates already done ----
dates <- seq(start_date, end_date, by = "day")
dates <- dates[!weekdays(dates) %in% c("Saturday", "Sunday")]

done_dates <- if (file.exists(checkpoint)) {
  message("Resuming from checkpoint: ", checkpoint)
  prior <- read_parquet(checkpoint)
  as.Date(unique(prior$query_date))
} else {
  as.Date(character(0))
}

dates <- dates[!dates %in% done_dates]
message("Dates remaining: ", length(dates))

# ---- run with frequent checkpoints ----
results <- list()
for (i in seq_along(dates)) {
  results[[i]] <- fetch_docket(dates[i])
  Sys.sleep(runif(1, 4, 7))   # slower to avoid 403s
  
  # Checkpoint every 25 requests (~3 minutes of work, max)
  if (i %% 25 == 0) {
    new_data <- bind_rows(results)
    if (file.exists(checkpoint)) {
      prior <- read_parquet(checkpoint)
      combined <- bind_rows(prior, new_data)
    } else {
      combined <- new_data
    }
    write_parquet(combined, checkpoint)
    message("  Checkpoint: ", i, "/", length(dates), 
            " — total rows on disk: ", nrow(combined))
    results <- list()  # clear in-memory after writing
    gc()
  }
}

# --- final write ---
if (length(results) > 0) {
  final_batch <- bind_rows(results)
  if (file.exists(checkpoint)) {
    prior <- read_parquet(checkpoint)
    combined <- bind_rows(prior, final_batch)
  } else {
    combined <- final_batch
  }
  write_parquet(combined, checkpoint)
}

# rename checkpoint to final
file.rename(checkpoint, out_path)
message("Done: ", out_path)

library(httr2)

# Same UA and minimal headers — what httr2 sends by default
test_one <- function(label, county, date, case_type_id = 31) {
  url <- sprintf(
    "https://www.oscn.net/applications/oscn/report.asp?report=WebJudicialDocketCaseTypeAll&errorcheck=true&database=&db=%s&CaseTypeID=%d&StartDate=%s&GeneralNumber=1&generalnumber1=1",
    county, case_type_id, date
  )
  
  resp <- tryCatch(
    request(url) |>
      req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.3.1 Safari/605.1.15") |>
      req_timeout(15) |>
      req_perform(),
    error = function(e) NULL
  )
  
  if (is.null(resp)) {
    cat(label, ": REQUEST FAILED\n")
    return(invisible())
  }
  
  body <- resp_body_string(resp)
  is_turnstile <- grepl("Turnstile|cf-turnstile", body)
  has_cases <- grepl("GetCaseInformation", body)
  
  cat(sprintf("%-30s status=%d  len=%d  turnstile=%s  cases=%s\n",
              label, resp_status(resp), nchar(body), 
              is_turnstile, has_cases))
}

# Test 1: Oklahoma County (the smoke test county) on a known-good date
test_one("Oklahoma 2022-09-01", "Oklahoma", "2022-09-01", 31)

# Wait so we're not pounding the server
Sys.sleep(15)

# Test 2: Tulsa County on the same date
test_one("Tulsa 2022-09-01", "Tulsa", "2022-09-01", 31)

Sys.sleep(15)

# Test 3: Cleveland County
test_one("Cleveland 2022-09-01", "Cleveland", "2022-09-01", 31)

Sys.sleep(15)

# Test 4: Garfield County
test_one("Garfield 2022-09-01", "Garfield", "2022-09-01", 31)
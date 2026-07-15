# test_one_fetch.R — single-fetch test before committing to full pilot
# Run this BEFORE launching pilot.R. If it returns Turnstile, your IP is flagged.

library(httr2)
source("scripts/pilot_v3/fetch_one_combo.R")

url <- "https://www.oscn.net/applications/oscn/report.asp?report=WebJudicialDocketCaseTypeAll&errorcheck=true&database=&db=Tulsa&CaseTypeID=31&StartDate=2022-10-17&GeneralNumber=1&generalnumber1=1"

resp <- request(url) |>
  req_user_agent(RESEARCH_USER_AGENT) |>
  req_headers(!!!BROWSER_HEADERS) |>
  req_perform()

body <- resp_body_string(resp)

cat("\n=== Pre-flight test ===\n")
cat("Status:", resp_status(resp), "\n")
cat("Body length:", nchar(body), "characters\n")
cat("Turnstile detected:", grepl("Turnstile|cf-turnstile", body), "\n")
cat("Cases on page:", grepl("GetCaseInformation", body), "\n\n")

if (grepl("Turnstile|cf-turnstile", body)) {
  cat("❌ FAIL: IP is currently blocked. Do NOT launch pilot from this network.\n")
  cat("   Switch networks (cellular hotspot, friend's WiFi) and retest.\n\n")
} else if (grepl("GetCaseInformation", body)) {
  cat("✅ PASS: Ready to launch pilot.\n\n")
} else {
  cat("⚠ UNEXPECTED: status OK and no Turnstile, but no case data found.\n")
  cat("   Investigate before launching.\n\n")
}
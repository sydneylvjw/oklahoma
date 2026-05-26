library(httr2)

url <- "https://www.oscn.net/applications/oscn/report.asp?report=WebJudicialDocketCaseTypeAll&errorcheck=true&database=&db=Tulsa&CaseTypeID=31&StartDate=2022-10-17&GeneralNumber=1&generalnumber1=1"

# ---- Patch attempt #1: Same UA, adds missing headers ----
# Use the same UA your browser shows. To find yours, visit https://www.whatsmyua.info/
# For now, using a modern Safari UA. Replace with yours if different.
safari_ua <- "Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.3.1 Safari/605.1.15"

resp <- request(url) |>
  req_user_agent(safari_ua) |>
  req_headers(
    "Accept" = "text/html,application/xhtml+xml,application/xml;q=0.9,image/webp,*/*;q=0.8",
    "Accept-Language" = "en-US,en;q=0.9",
    "Accept-Encoding" = "gzip, deflate, br",
    "Connection" = "keep-alive",
    "Upgrade-Insecure-Requests" = "1",
    "Sec-Fetch-Dest" = "document",
    "Sec-Fetch-Mode" = "navigate",
    "Sec-Fetch-Site" = "none",
    "Sec-Fetch-User" = "?1",
    "Cache-Control" = "max-age=0"
  ) |>
  req_perform()

body <- resp_body_string(resp)
cat("Status:", resp_status(resp), "\n")
cat("Body length:", nchar(body), "\n")
cat("Contains Turnstile?:", grepl("Turnstile|cf-turnstile", body), "\n")
cat("Contains case info?:", grepl("GetCaseInformation", body), "\n")
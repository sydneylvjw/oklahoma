library(httr2)
source("scripts/parser.R")

url <- "https://www.oscn.net/applications/oscn/report.asp?report=WebJudicialDocketCaseTypeAll&errorcheck=true&database=&db=Tulsa&CaseTypeID=31&StartDate=2022-10-17&GeneralNumber=1&generalnumber1=1"

resp <- request(url) |>
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15") |>
  req_perform()

body <- resp_body_string(resp)
cat("Status:", resp_status(resp), "\n")
cat("Body length:", nchar(body), "\n")
cat("Contains Turnstile?:", grepl("Turnstile|cf-turnstile", body), "\n")
cat("Contains case info?:", grepl("GetCaseInformation", body), "\n")
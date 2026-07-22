library(httr2)

url <- "https://www.oscn.net/applications/oscn/report.asp?report=WebJudicialDocketCaseTypeAll&errorcheck=true&database=&db=Tulsa&CaseTypeID=31&StartDate=2022-10-17&GeneralNumber=1&generalnumber1=1"

resp <- request(url) |>
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/26.3.1 Safari/605.1.15") |>
  req_perform()

body <- resp_body_string(resp)
cat("Status:", resp_status(resp), "\n")
cat("Length:", nchar(body), "\n")
cat("Turnstile:", grepl("Turnstile|cf-turnstile", body), "\n")
cat("Cases:", grepl("GetCaseInformation", body), "\n")
library(httr2)

# Same URL you just confirmed works in your browser
url <- "https://www.oscn.net/applications/oscn/report.asp?report=WebJudicialDocketCaseTypeAll&errorcheck=true&database=&db=Oklahoma&CaseTypeID=31&StartDate=2023-06-13&GeneralNumber=1&generalnumber1=1"

resp <- request(url) |>
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15 (KHTML, like Gecko) Version/17.0 Safari/605.1.15") |>
  req_timeout(30) |>
  req_perform()

cat("Status:", resp_status(resp), "\n")
cat("Body length:", nchar(resp_body_string(resp)), "characters\n")

# Save the raw HTML so we can inspect it
dir.create("data/diagnostic", showWarnings = FALSE)
writeLines(resp_body_string(resp), "data/diagnostic/oklahoma_2023-06-13.html")

cat("\nSaved to: data/diagnostic/oklahoma_2023-06-13.html\n")
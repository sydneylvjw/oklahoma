source("scripts/parser.R")
library(httr2)

url <- "https://www.oscn.net/applications/oscn/report.asp?report=WebJudicialDocketCaseTypeAll&errorcheck=true&database=&db=Oklahoma&CaseTypeID=31&StartDate=2023-06-13&GeneralNumber=1&generalnumber1=1"

resp <- request(url) |>
  req_user_agent("Mozilla/5.0 (Macintosh; Intel Mac OS X 10_15_7) AppleWebKit/605.1.15") |>
  req_perform()

parsed <- parse_oscn_page(resp_body_string(resp), 
                          query_date = as.Date("2023-06-13"), 
                          county = "Oklahoma")

parsed |> filter(hearing_code == "RCC") |> count(section)
parsed |> count(hearing_code, sort = TRUE) |> head(15)
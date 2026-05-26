source("scripts/parser.R")

html_text <- readLines("data/diagnostic/oklahoma_2023-06-13.html", warn = FALSE) |>
  paste(collapse = "\n")

parsed <- parse_oscn_page(html_text, 
                          query_date = as.Date("2023-06-13"), 
                          county = "Oklahoma")

cat("Total rows:", nrow(parsed), "\n")
cat("Rows with hearing_code populated:", sum(!is.na(parsed$hearing_code)), "\n")

cat("\nFull hearing code distribution:\n")
print(parsed |> count(hearing_code, sort = TRUE))

cat("\nBreakdown by section:\n")
print(parsed |> count(section, hearing_code) |> tidyr::pivot_wider(names_from = section, values_from = n))
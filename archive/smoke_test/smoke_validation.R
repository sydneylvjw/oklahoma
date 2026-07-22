library(arrow); library(dplyr)
smoke <- read_parquet("data/smoke_oklahoma_CF.parquet")

cat("Total rows:", nrow(smoke), "\n")
cat("Distinct dates:", n_distinct(smoke$query_date), "\n")
cat("Avg rows per day:", round(nrow(smoke) / n_distinct(smoke$query_date)), "\n")

smoke |> count(section)
smoke |> filter(hearing_code %in% c("RL8", "RCC")) |> count(hearing_code, section)

# Look at days near HB 2259 effective date (Nov 1, 2022)
smoke |> 
  filter(query_date >= "2022-10-25", query_date <= "2022-11-05") |>
  count(query_date, section) |>
  print()
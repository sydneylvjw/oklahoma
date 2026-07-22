library(arrow); library(dplyr); library(fs); library(purrr); library(stringr)

# Load all pilot parquets
pilot <- dir_ls("data/", glob = "*pilot_*.parquet") |>
  map_dfr(read_parquet)

cat("Total rows:", nrow(pilot), "\n")
cat("By county and case type:\n")
pilot |> count(county, case_type) |> print()

cat("\nHearing codes across pilot:\n")
pilot |> count(hearing_code, sort = TRUE) |> head(30) |> print()

cat("\nLFO-relevant codes:\n")
lfo_codes <- c("RL8", "RCC", "CCH", "CCR", "HAR", "REV", "MOD")
pilot |> filter(hearing_code %in% lfo_codes) |> 
  count(county, case_type, hearing_code) |> 
  print(n = 50)

# Pre/post HB 2259 comparison
cat("\nRule 8 hearings pre/post HB 2259 (Nov 1, 2022):\n")
pilot |> 
  filter(hearing_code == "RL8") |>
  mutate(period = if_else(as.Date(query_date) < "2022-11-01", "pre", "post")) |>
  count(county, case_type, period) |>
  tidyr::pivot_wider(names_from = period, values_from = n, values_fill = 0) |>
  print()
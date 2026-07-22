#---- Run prior to the full parser to try and map out how amounts are handled in the JSON tables. Unable to map that in the original 

source("scripts/pilot/phase2/parse_case_detail.R")
source("scripts/pilot/phase2/config.R")

files <- list.files(DETAIL_ROOT, pattern="\\.html\\.gz$", recursive=TRUE, full.names=TRUE)

# parse a batch and stack them
d <- purrr::map_dfr(files, parse_case_file)
cat(nrow(d), "docket rows from", length(unique(d$case_number)), "cases\n")

# 1) sanity: one case's parsed docket
d |> filter(case_number == d$case_number[1]) |>
  select(entry_date, docket_code, description, amount) |> print(n = 20)

# 2) THE key check — any populated amounts?
d |> filter(!is.na(amount)) |>
  select(case_number, entry_date, docket_code, amount, amount_raw, description) |>
  head(15) |> print(width = Inf)

# 3) LFO-looking rows (this is what the classifier will key on)
d |> filter(str_detect(toupper(description),
                       "COST|FEE|FINE|ABILITY TO PAY|RULE 8|CCH|RCC|INSTALLMENT|TIME TO PAY")) |>
  count(docket_code, sort = TRUE) |> head(20) |> print()
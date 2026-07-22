# test_tulsa_cf.R — run just Tulsa CF to verify the new architecture
source("scripts/pilot_v2/fetch_one_combo.R")

fetch_one_combo(
  county          = "Tulsa",
  case_type_id    = 31,
  case_type_label = "CF",
  start_date      = as.Date("2022-09-01"),
  end_date        = as.Date("2022-12-30"),
  chunk_size      = 10
)
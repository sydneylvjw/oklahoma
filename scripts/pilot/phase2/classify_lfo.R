# scripts/pilot/phase2/classify_lfo.R
# Stage 3: tag parsed docket rows against a per-(county,code) dictionary.
# OUTCOME-AGNOSTIC: emits row-level tagged data; you compute assessed / waived /
# net / event-count yourself. Regenerable -- re-run after editing the CSV; never
# re-parse. Includes a per-county coverage report for unmapped money-bearing codes.

suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(readr); library(fs); library(stringr)
})
source("scripts/pilot/phase2/config.R")

DOCKET_ROOT     <- fs::path(PROJECT_ROOT, "data", "case_docket")            # input (immutable)
CLASSIFIED_ROOT <- fs::path(PROJECT_ROOT, "data", "case_docket_classified") # output (regenerable)
DICT_PATH       <- "scripts/pilot/phase2/lfo_codes.csv"

load_dict <- function(path = DICT_PATH) {
  readr::read_csv(path, show_col_types = FALSE) |>
    mutate(county = tolower(trimws(county)), code = trimws(code))
}

classify_docket <- function(docket, dict) {
  docket |>
    mutate(county = tolower(county), docket_code = trimws(docket_code)) |>
    left_join(dict, by = c("county", "docket_code" = "code")) |>
    mutate(
      code_mapped            = !is.na(category),
      category               = coalesce(category, "UNMAPPED"),
      direction              = coalesce(direction, NA_character_),
      is_rule8               = coalesce(is_rule8, FALSE),
      is_conversion_artifact = coalesce(is_conversion_artifact, FALSE)
    )
}

# Read all parsed dockets, classify, write the regenerable classified dataset.
run_classify <- function() {
  dict <- load_dict()
  cl   <- open_dataset(DOCKET_ROOT) |> collect() |> classify_docket(dict)
  fs::dir_create(CLASSIFIED_ROOT)
  arrow::write_dataset(cl, CLASSIFIED_ROOT, partitioning = c("county", "case_type"),
                       existing_data_behavior = "delete_matching")
  message(sprintf("[classify] %d rows; %d unmapped (%d with a dollar amount)",
                  nrow(cl), sum(!cl$code_mapped),
                  sum(!cl$code_mapped & !is.na(cl$amount))))
  invisible(cl)
}

# --- Coverage: what's NOT in the dictionary, per county, ranked by money -----
# Edit lfo_codes.csv against this, then re-run run_classify(). No re-parse.
unmapped_codes <- function(cl) {
  cl |>
    filter(!code_mapped, !is.na(amount)) |>
    group_by(county, docket_code) |>
    summarise(n = n(), total = sum(amount, na.rm = TRUE),
              example = first(str_sub(description, 1, 60)), .groups = "drop") |>
    arrange(county, desc(abs(total)))
}

# --- Outcome-agnostic per-case components (compute ANY of the 4 from these) ---
summarise_cases <- function(cl) {
  cl |>
    group_by(county, case_type, case_number) |>
    summarise(
      total_debits   = sum(amount[amount > 0 & !is_conversion_artifact], na.rm = TRUE), # assessed
      total_credits  = sum(amount[amount < 0 & !is_conversion_artifact], na.rm = TRUE), # waived/credited (neg)
      net_balance    = sum(amount[!is_conversion_artifact], na.rm = TRUE),              # net
      conversion_amt = sum(amount[is_conversion_artifact], na.rm = TRUE),              # quarantined
      n_rule8_events = sum(is_rule8, na.rm = TRUE),                                     # event count
      n_lfo_rows     = sum(!is.na(amount)),
      first_rule8    = { d <- entry_date[is_rule8 %in% TRUE]; if (length(d)) min(d, na.rm = TRUE) else as.Date(NA) },
      last_rule8     = { d <- entry_date[is_rule8 %in% TRUE]; if (length(d)) max(d, na.rm = TRUE) else as.Date(NA) },
      n_unmapped     = sum(!code_mapped & !is.na(amount)),
      .groups = "drop"
    )
}

if (.oscn_should_autorun()) {
  cl <- run_classify()
  cat("\n--- Unmapped money-bearing codes (edit lfo_codes.csv) ---\n")
  print(unmapped_codes(cl), n = 40)
}

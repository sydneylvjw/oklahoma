# scripts/production/phase2/classify_lfo.R
# PRODUCTION Phase 2 stage 3: tag docket rows against the (county, code)
# dictionary. OUTCOME-AGNOSTIC -- emits row-level tagged data so assessed /
# waived / net / event-count are all computable downstream. Regenerable: re-run
# after editing the dictionary; never re-parse, never re-fetch.
#
# Run:  Rscript scripts/production/phase2/classify_lfo.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({
  library(arrow); library(dplyr); library(readr); library(fs); library(stringr)
})
source("scripts/production/phase2/config.R")

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
      is_rule8               = coalesce(is_rule8, FALSE),
      is_conversion_artifact = coalesce(is_conversion_artifact, FALSE),
      period                 = ifelse(entry_date < HB2259_DATE, "pre", "post")
    )
}

run_classify <- function() {
  cl <- open_dataset(DOCKET_ROOT) |> collect() |> classify_docket(load_dict())
  fs::dir_create(CLASSIFIED_ROOT)
  arrow::write_dataset(cl, CLASSIFIED_ROOT, partitioning = c("county", "case_type"),
                       existing_data_behavior = "delete_matching")
  message(sprintf("[classify] %s rows | %s unmapped (%s with a dollar amount)",
                  format(nrow(cl), big.mark = ","),
                  format(sum(!cl$code_mapped), big.mark = ","),
                  format(sum(!cl$code_mapped & !is.na(cl$amount)), big.mark = ",")))
  invisible(cl)
}

# Money-bearing codes with no dictionary entry, per county -- the coverage gate.
unmapped_codes <- function(cl) {
  cl |> filter(!code_mapped, !is.na(amount)) |>
    group_by(county, docket_code) |>
    summarise(n = n(), total = sum(amount, na.rm = TRUE),
              example = first(str_sub(description, 1, 60)), .groups = "drop") |>
    arrange(county, desc(abs(total)))
}

# Per-case components: compute ANY of the four outcomes from these.
summarise_cases <- function(cl) {
  cl |> group_by(county, case_type, case_number) |>
    summarise(
      total_debits   = sum(amount[amount > 0 & !is_conversion_artifact], na.rm = TRUE),
      total_credits  = sum(amount[amount < 0 & !is_conversion_artifact], na.rm = TRUE),
      net_balance    = sum(amount[!is_conversion_artifact], na.rm = TRUE),
      conversion_amt = sum(amount[is_conversion_artifact], na.rm = TRUE),
      n_rule8_events = sum(is_rule8, na.rm = TRUE),
      n_lfo_rows     = sum(!is.na(amount)),
      n_unmapped     = sum(!code_mapped & !is.na(amount)),
      .groups = "drop")
}

if (.oscn_should_autorun()) {
  cl <- run_classify()
  cat("\n--- Unmapped money-bearing codes (fill via build_dictionary_todo.R) ---\n")
  print(unmapped_codes(cl), n = 50)
}

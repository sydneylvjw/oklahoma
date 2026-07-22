# scripts/pilot/phase2/build_dictionary_todo.R
# Reproducible step 1 of dictionary maintenance:
# scaffold every money-bearing docket code NOT yet in the dictionary, per
# county, pre-filled with its own description + frequency/dollar totals so you
# can triage. Fill in `category`/`direction`/`is_rule8`, then run
# scripts/pilot/phase2/merge_dictionary.R to append the completed rows.
#
# Run:  Rscript scripts/pilot/phase2/build_dictionary_todo.R
# Reads:  data/case_docket/**            (all parsed dockets)
#         scripts/pilot/phase2/lfo_codes.csv   (current dictionary)
# Writes: scripts/pilot/phase2/lfo_codes_TODO.csv

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(arrow); library(dplyr); library(readr) })
source("scripts/pilot/phase2/classify_lfo.R")   # classify_docket(), load_dict(), unmapped_codes(), DOCKET_ROOT

TODO_PATH <- "scripts/pilot/phase2/lfo_codes_TODO.csv"

build_dictionary_todo <- function(todo_path = TODO_PATH) {
  cl <- open_dataset(DOCKET_ROOT) |> collect() |> classify_docket(load_dict())

  todo <- unmapped_codes(cl) |>                 # county, docket_code, n, total, example
    transmute(
      county,
      code                   = docket_code,
      category               = "",              # FILL: cost / fee / fine / assessment / rule8 / credit / etc.
      direction              = "",              # FILL: debit | credit | neutral
      is_rule8               = FALSE,           # set TRUE for Rule 8 / ability-to-pay codes
      is_conversion_artifact = FALSE,           # set TRUE for mainframe-conversion balances
      n_rows                 = n,               # triage aids (dropped on merge)
      total_amount           = round(total, 2),
      notes                  = example
    ) |>
    arrange(county, desc(abs(total_amount)))

  readr::write_csv(todo, todo_path)
  message("Wrote ", nrow(todo), " unmapped (county, code) rows to ", todo_path,
          " -- fill category/direction, then run merge_dictionary.R")
  invisible(todo)
}

if (!interactive()) build_dictionary_todo()

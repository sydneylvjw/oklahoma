# scripts/production/phase2/build_dictionary_todo.R
# Dictionary step 1: scaffold every money-bearing code NOT in the dictionary,
# per county, with frequency + dollar totals + its own description for triage.
# Fill category/direction, then run merge_dictionary.R.
#
# Run:  Rscript scripts/production/phase2/build_dictionary_todo.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(arrow); library(dplyr); library(readr) })
source("scripts/production/phase2/classify_lfo.R")

TODO_PATH <- "scripts/production/phase2/lfo_codes_TODO.csv"

build_dictionary_todo <- function(todo_path = TODO_PATH) {
  cl <- open_dataset(DOCKET_ROOT) |> collect() |> classify_docket(load_dict())
  todo <- unmapped_codes(cl) |>
    transmute(county, code = docket_code,
              category = "", direction = "", is_rule8 = FALSE,
              is_conversion_artifact = FALSE,
              n_rows = n, total_amount = round(total, 2), notes = example) |>
    arrange(county, desc(abs(total_amount)))
  readr::write_csv(todo, todo_path)
  message("Wrote ", nrow(todo), " unmapped (county, code) rows to ", todo_path)
  invisible(todo)
}

if (!interactive()) build_dictionary_todo()

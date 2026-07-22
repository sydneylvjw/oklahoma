# scripts/production/phase2/merge_dictionary.R
# Dictionary step 3: append filled TODO rows into lfo_codes.csv.
# Skips blank-category rows (they stay UNMAPPED and resurface), refuses
# duplicate (county, code) pairs, backs up before writing.
# Then re-run classify_lfo.R.
#
# Run:  Rscript scripts/production/phase2/merge_dictionary.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(dplyr); library(readr) })
source("scripts/production/phase2/config.R")

TODO_PATH <- "scripts/production/phase2/lfo_codes_TODO.csv"
DICT_COLS <- c("county","code","category","direction",
               "is_rule8","is_conversion_artifact","notes")

merge_dictionary <- function(dict_path = DICT_PATH, todo_path = TODO_PATH) {
  dict <- readr::read_csv(dict_path, show_col_types = FALSE)
  todo <- readr::read_csv(todo_path, show_col_types = FALSE) |>
    mutate(county = tolower(trimws(county)), code = trimws(code)) |>
    select(any_of(DICT_COLS))

  blank <- is.na(todo$category) | trimws(todo$category) == ""
  if (any(blank)) message("Skipping ", sum(blank), " row(s) with no category.")
  todo <- todo[!blank, , drop = FALSE]

  dup <- paste(todo$county, todo$code) %in%
         paste(tolower(trimws(dict$county)), trimws(dict$code))
  if (any(dup)) message("Skipping ", sum(dup), " already-present (county, code) pair(s).")
  todo <- todo[!dup, , drop = FALSE]

  if (!nrow(todo)) { message("Nothing new to add."); return(invisible(dict)) }
  readr::write_csv(dict, paste0(dict_path, ".bak"))
  merged <- bind_rows(dict, todo)
  readr::write_csv(merged, dict_path)
  message("Added ", nrow(todo), " code(s); dictionary now ", nrow(merged),
          " rows. Re-run classify_lfo.R to apply.")
  invisible(merged)
}

if (!interactive()) merge_dictionary()

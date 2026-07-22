# scripts/pilot/phase2/merge_dictionary.R
# Reproducible step 2 of dictionary maintenance:
# append completed rows from scripts/pilot/phase2/lfo_codes_TODO.csv into the
# dictionary scripts/pilot/phase2/lfo_codes.csv, while:
#   - skipping rows whose `category` is still blank (they stay UNMAPPED and
#     resurface in the next TODO, so nothing is silently half-mapped),
#   - refusing duplicate (county, code) pairs already in the dictionary,
#   - backing up the dictionary to lfo_codes.csv.bak before writing.
# After merging, re-run:  Rscript scripts/pilot/phase2/classify_lfo.R
#
# Run:  Rscript scripts/pilot/phase2/merge_dictionary.R

options(oscn.autorun.suppress = TRUE)
suppressPackageStartupMessages({ library(dplyr); library(readr) })

DICT_PATH <- "scripts/pilot/phase2/lfo_codes.csv"
TODO_PATH <- "scripts/pilot/phase2/lfo_codes_TODO.csv"
DICT_COLS <- c("county","code","category","direction",
               "is_rule8","is_conversion_artifact","notes")

merge_dictionary <- function(dict_path = DICT_PATH, todo_path = TODO_PATH) {
  dict <- readr::read_csv(dict_path, show_col_types = FALSE)
  todo <- readr::read_csv(todo_path, show_col_types = FALSE) |>
    mutate(county = tolower(trimws(county)), code = trimws(code)) |>
    select(any_of(DICT_COLS))                       # drop triage-only columns

  blank <- is.na(todo$category) | trimws(todo$category) == ""
  if (any(blank)) message("Skipping ", sum(blank), " row(s) with no category filled.")
  todo <- todo[!blank, , drop = FALSE]

  existing <- paste(tolower(trimws(dict$county)), trimws(dict$code))
  dup <- paste(todo$county, todo$code) %in% existing
  if (any(dup)) message("Skipping ", sum(dup), " (county, code) pair(s) already present.")
  todo <- todo[!dup, , drop = FALSE]

  if (nrow(todo) == 0) { message("Nothing new to add."); return(invisible(dict)) }

  readr::write_csv(dict, paste0(dict_path, ".bak"))  # backup before write
  merged <- bind_rows(dict, todo)
  readr::write_csv(merged, dict_path)
  message("Added ", nrow(todo), " code(s). Dictionary now ", nrow(merged),
          " rows. Backup at ", paste0(dict_path, ".bak"),
          ". Re-run classify_lfo.R to apply.")
  invisible(merged)
}

if (!interactive()) merge_dictionary()

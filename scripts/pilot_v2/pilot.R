# pilot.R — full pilot scrape across all counties × case types
# Calls fetch_one_combo() for each combination.
# Safe to re-run: skip-if-done logic at the chunk level handles resume.

source("scripts/pilot_v2/fetch_one_combo.R")

# ---- config ----
counties <- c("Tulsa", "Cleveland", "Garfield", "Comanche")
case_types <- list(
  list(id = 31, label = "CF"),
  list(id = 32, label = "CM")
)
start_date <- as.Date("2022-09-01")
end_date   <- as.Date("2022-12-30")

# ---- run all combinations ----
message("=== Pilot v2: ", length(counties), " counties × ", 
        length(case_types), " case types ===")
message("Window: ", start_date, " to ", end_date)
message("Expected combos: ", length(counties) * length(case_types))
message("")

for (cty in counties) {
  for (ct in case_types) {
    result <- tryCatch(
      fetch_one_combo(
        county          = cty,
        case_type_id    = ct$id,
        case_type_label = ct$label,
        start_date      = start_date,
        end_date        = end_date,
        chunk_size      = 10
      ),
      error = function(e) {
        message("\n!!! ERROR in ", cty, "/", ct$label, ": ", e$message)
        message("!!! Moving to next combo.\n")
        FALSE
      }
    )
  }
}

message("\n=== All pilot combinations processed ===")
message("Run scripts/pilot_v2/combine_chunks.R to build final parquet files.")
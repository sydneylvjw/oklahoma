# pilot.R — v3 driver
# Runs all (county, case_type) combos with cooldowns between them.

source("scripts/pilot_v3/fetch_one_combo.R")

# ---- config ----
counties <- c("Tulsa", "Cleveland", "Garfield", "Comanche")
case_types <- list(
  list(id = 31, label = "CF"),
  list(id = 32, label = "CM")
)
start_date <- as.Date("2022-09-01")
end_date   <- as.Date("2022-12-30")

# Cooldown between combos — 10-15 min looks more human, lets reputation rest
between_combo_cooldown <- function() {
  delay <- runif(1, 600, 900)
  message("\n>>> Cooldown: pausing ", round(delay / 60, 1), 
          " minutes before next combo. Resuming at ",
          format(Sys.time() + delay, "%H:%M:%S"), "\n")
  Sys.sleep(delay)
}

# ---- main loop ----
message("=== Pilot v3 ===")
message("Counties: ", paste(counties, collapse = ", "))
message("Case types: ", paste(sapply(case_types, \(x) x$label), collapse = ", "))
message("Date window: ", start_date, " to ", end_date)
message("Total combos: ", length(counties) * length(case_types))
message("")

combo_count <- 0
total_combos <- length(counties) * length(case_types)

for (cty in counties) {
  for (ct in case_types) {
    combo_count <- combo_count + 1
    
    message("\n##### Combo ", combo_count, "/", total_combos, ": ",
            cty, "/", ct$label, " #####")
    
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
        message("!!! Moving to next combo.")
        FALSE
      }
    )
    
    # Cooldown unless this was the last combo
    if (combo_count < total_combos) {
      between_combo_cooldown()
    }
  }
}

message("\n=== All combos processed ===")
message("Next: run scripts/pilot_v3/combine_chunks.R to build final files.")
# scripts/phase2/run_sample.R
# OPTION B, end to end and REPRODUCIBLE. A fixed seed + deterministic frame
# ordering means this draws the identical cases every run, on any machine.
#   draw (seeded) -> save auditable case list -> fetch -> parse -> classify ->
#   write PI markdown summary.
#
# Run:  caffeinate -i Rscript scripts/phase2/run_sample.R
#
# Change SAMPLE_SEED only if you deliberately want a different draw; the saved
# sample_draw.csv is the record of exactly which cases were pulled.

SAMPLE_SEED  <- 20221101L   # HB 2259 effective date, used as the seed
N_PER_PERIOD <- 75L         # 75 pre + 75 post per county (balanced screen)

# Suppress the worker files' own auto-runs so THIS script controls order.
options(oscn.autorun.suppress = TRUE)
source("scripts/phase2/sample_counties.R")   # frame / draw / fetch / summary
source("scripts/phase2/parse_details.R")     # run_parse()
source("scripts/phase2/classify_lfo.R")      # run_classify()

DRAW_RDS <- file.path(PROJECT_ROOT, "logs", "sample_draw.rds")
DRAW_CSV <- file.path(PROJECT_ROOT, "logs", "sample_draw.csv")

run_sample <- function(n_per_period = N_PER_PERIOD, seed = SAMPLE_SEED) {
  frame <- sample_frame()
  draw  <- draw_sample(frame, n_per_period = n_per_period, seed = seed)

  dir.create(dirname(DRAW_RDS), showWarnings = FALSE, recursive = TRUE)
  saveRDS(draw, DRAW_RDS)
  write.csv(draw, DRAW_CSV, row.names = FALSE)   # auditable, reproducible list
  message("Seed ", seed, " -> ", nrow(draw), " sampled case-strata saved to ",
          basename(DRAW_CSV))

  # Fetch (rate-limited, resume-safe; pages reuse the case_details tree so a
  # later full pull of chosen counties skips them).
  run_details(queue = sample_queue(draw))

  # Parse + classify the newly fetched sample, then write the PI summary.
  run_parse()
  run_classify()
  out <- write_pi_summary(draw)
  message("Done. PI summary: ", out)
  invisible(out)
}

# This wrapper is never sourced by others, so a plain check is safe here.
if (!interactive()) run_sample()

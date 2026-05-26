library(arrow); library(dplyr); library(tibble)
source("scripts/parser.R")

# === Since the parser drops all previous data prior to the final checkpoint where all dates are empty, trying to recreate the bug to diagnose the issue. Hypothesis is something funny is happening in the `bind_rows()` call with a potential change of column type that prevents the binding from just adding NAs into the cells and pushes it to actually drop the columns that aren't present in the final checkpoint dates (12-22-22 - 12-30-22, assuming courts are closed for the holiday therefor no cases) and collapse the columns from 17 to 4. ===

# ---- Test #1: Recreate the exact pattern: wide rows + narrow empty rows ----

## ---- A wide row, as parse_oscn_page would return for a non-empty day ----
wide <- tibble(
  query_date    = "2022-09-01",
  county        = "Oklahoma",
  section       = "scheduled",
  hearing_time  = "9:00 AM",
  case_number   = "CF-2022-1",
  casemasterid  = "1234567",
  judge         = "Smith, John",
  hearing_type  = "RULE 8 HEARING",
  hearing_code  = "RL8",
  plaintiff     = "STATE OF OKLAHOMA",
  defendants    = "DOE, JOHN",
  n_defendants  = 1L,
  attorney      = "Brown, Jane",
  charges       = "1. SOME CHARGE",
  bonds         = "(Bond: $500)",
  empty         = NA,
  case_type     = "CF"
)

## ---- A narrow empty-day row, as the empty branch returns ----
narrow_empty <- tibble(
  query_date = "2022-12-22",
  county     = "Oklahoma",
  empty      = TRUE,
  case_type  = "CF"
)

## Combine them — this is what bind_rows(results) does ----
combined <- bind_rows(wide, narrow_empty)
cat("Combined dimensions:", nrow(combined), "rows x", ncol(combined), "cols\n")
cat("Columns:", paste(names(combined), collapse = ", "), "\n\n")

## Write to parquet, read back ----
write_parquet(combined, "/tmp/test_combine.parquet")
roundtrip <- read_parquet("/tmp/test_combine.parquet")

cat("After parquet round-trip:\n")
cat("Rows:", nrow(roundtrip), "\n")
cat("Cols:", ncol(roundtrip), "\n")
cat("Names:", paste(names(roundtrip), collapse = ", "), "\n")
print(roundtrip)

## ---- Now simulate the actual bug: incremental save with prior wide-only checkpoint ----

### Simulation: prior checkpoint with 5 wide rows ----
prior <- bind_rows(replicate(5, wide, simplify = FALSE)) |>
  mutate(query_date = paste0("2022-09-", sprintf("%02d", 1:5)))
write_parquet(prior, "/tmp/test_checkpoint.parquet")

cat("\n--- Simulating the buggy save sequence ---\n")
cat("Prior checkpoint:", nrow(prior), "rows,", ncol(prior), "cols\n")

### Now the "new_data" buffer has only narrow empty rows ----
new_narrow_only <- bind_rows(replicate(7, narrow_empty, simplify = FALSE)) |>
  mutate(query_date = paste0("2022-12-", 22:28))

cat("New batch:", nrow(new_narrow_only), "rows,", ncol(new_narrow_only), "cols\n")

### What save_checkpoint does ----
prior_read <- read_parquet("/tmp/test_checkpoint.parquet")
combined2 <- bind_rows(prior_read, new_narrow_only)
cat("After bind_rows (before write):", nrow(combined2), "rows,", ncol(combined2), "cols\n")

write_parquet(combined2, "/tmp/test_checkpoint.parquet")
final <- read_parquet("/tmp/test_checkpoint.parquet")
cat("After parquet round-trip:", nrow(final), "rows,", ncol(final), "cols\n")
print(final)




# ---- Test #2: Checking for soft IP block by OCSN ----
library(arrow); library(dplyr); library(tibble)

  ## ---- Simulate: previous checkpoint has WIDE rows + narrow empty rows mixed ----
prior_mixed <- bind_rows(
  # 5 wide rows from early successful fetches
  tibble(
    query_date = paste0("2022-09-", sprintf("%02d", 1:5)),
    county = "Oklahoma",
    section = "scheduled",
    hearing_time = "9:00 AM",
    case_number = paste0("CF-2022-", 1:5),
    casemasterid = as.character(1000+1:5),
    judge = "Smith",
    hearing_type = "RULE 8",
    hearing_code = "RL8",
    plaintiff = "STATE",
    defendants = "DOE",
    n_defendants = 1L,
    attorney = "BROWN",
    charges = "1. CHARGE",
    bonds = "(Bond: $500)",
    empty = NA,
    case_type = "CF"
  ),
  # Some narrow empty rows mixed in
  tibble(
    query_date = paste0("2022-10-", sprintf("%02d", 1:3)),
    county = "Oklahoma",
    empty = TRUE,
    case_type = "CF"
  )
)

# Write and read back
write_parquet(prior_mixed, "/tmp/mixed.parquet")
back <- read_parquet("/tmp/mixed.parquet")

cat("Wrote", nrow(prior_mixed), "rows,", ncol(prior_mixed), "cols\n")
cat("Read back", nrow(back), "rows,", ncol(back), "cols\n")
print(back)

## Now do another round: append a few more narrow rows ----
new_batch <- tibble(
  query_date = paste0("2022-11-", sprintf("%02d", 1:7)),
  county = "Oklahoma",
  empty = TRUE,
  case_type = "CF"
)

combined3 <- bind_rows(back, new_batch)
write_parquet(combined3, "/tmp/mixed.parquet")
back3 <- read_parquet("/tmp/mixed.parquet")
cat("\nAfter appending 7 more narrow:\n")
cat("Rows:", nrow(back3), "Cols:", ncol(back3), "\n")
print(back3)

# Now check: are we losing data across MULTIPLE iterations?
## Simulate 5 sequential checkpoint saves, each adding only narrow empty rows ----
for (round in 1:5) {
  prior_read <- read_parquet("/tmp/mixed.parquet")
  new_batch_n <- tibble(
    query_date = paste0("2022-12-", sprintf("%02d", round*2 + 1:5)),
    county = "Oklahoma",
    empty = TRUE,
    case_type = "CF"
  )
  combined_n <- bind_rows(prior_read, new_batch_n)
  write_parquet(combined_n, "/tmp/mixed.parquet")
  
  back_n <- read_parquet("/tmp/mixed.parquet")
  cat("Round", round, ": rows=", nrow(back_n), "cols=", ncol(back_n), 
      "wide_rows=", sum(!is.na(back_n$section) & back_n$section != ""), "\n")
}

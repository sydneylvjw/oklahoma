library(arrow); library(dplyr); library(fs); library(purrr)

chunks <- dir_ls("data/pilot_chunks/tulsa/CF/", glob = "*.parquet")
cat("Number of chunks:", length(chunks), "\n")

tulsa_cf <- map_dfr(chunks, read_parquet)
cat("Total rows:", nrow(tulsa_cf), "\n")
cat("Columns:", ncol(tulsa_cf), "—", paste(names(tulsa_cf), collapse = ", "), "\n")
cat("Unique dates:", length(unique(tulsa_cf$query_date)), "\n")

# Sanity check: how many empty days?
cat("Empty days:", sum(tulsa_cf$empty %in% TRUE, na.rm = TRUE), "\n")
cat("Real cases:", sum(!is.na(tulsa_cf$case_number)), "\n")

# Hearing codes seen
tulsa_cf |> filter(!is.na(hearing_code)) |> count(hearing_code, sort = TRUE) |> head(20)

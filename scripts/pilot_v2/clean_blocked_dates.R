# clean_blocked_dates.R — remove Tulsa CF chunk rows for dates where
# the scrape was blocked (returned empty due to Turnstile, not real emptiness).
# After running this, re-run pilot.R to refetch only the missing dates.

library(arrow)
library(dplyr)
library(fs)
library(purrr)
library(stringr)

clean_empty_chunks <- function(chunks_root = "data/pilot_chunks",
                               county, case_type,
                               min_real_rows_per_day = 1) {
  
  chunk_dir <- file.path(chunks_root, county, case_type)
  if (!dir_exists(chunk_dir)) {
    message("No chunk dir at ", chunk_dir)
    return(invisible(NULL))
  }
  
  chunks <- dir_ls(chunk_dir, glob = "*.parquet")
  message("Processing ", length(chunks), " chunks for ", county, "/", case_type)
  
  # Read all chunks, identify dates where the scrape returned empty
  all_data <- map_dfr(chunks, read_parquet)
  
  empty_dates <- all_data |>
    filter(empty %in% TRUE) |>
    pull(query_date) |>
    unique()
  
  message("Empty dates to remove: ", length(empty_dates))
  
  if (length(empty_dates) == 0) {
    message("Nothing to clean.")
    return(invisible(NULL))
  }
  
  # Rewrite each chunk WITHOUT the empty-date rows
  total_removed <- 0
  for (chunk_path in chunks) {
    chunk <- read_parquet(chunk_path)
    n_before <- nrow(chunk)
    chunk_cleaned <- chunk |> filter(!(empty %in% TRUE))
    n_after <- nrow(chunk_cleaned)
    
    if (n_after < n_before) {
      if (n_after == 0) {
        # Chunk is entirely empty rows — delete the file
        file_delete(chunk_path)
        message("  Deleted empty chunk: ", basename(chunk_path),
                " (", n_before, " rows removed)")
      } else {
        write_parquet(chunk_cleaned, chunk_path)
        message("  Cleaned ", basename(chunk_path), ": ",
                n_before, " → ", n_after, " rows")
      }
      total_removed <- total_removed + (n_before - n_after)
    }
  }
  
  message("\nTotal rows removed: ", total_removed)
  message("Dates that will be re-fetched on next pilot run: ", length(empty_dates))
  invisible(empty_dates)
}

# Run if called directly
if (sys.nframe() == 0) {
  # Clean Tulsa CF (the test combo)
  clean_empty_chunks(county = "tulsa", case_type = "CF")
}
# combine_chunks.R — v3 assembler
# Builds final pilot_{county}_{case_type}.parquet files from chunks.
# Safe to re-run anytime.

library(arrow)
library(dplyr)
library(fs)
library(purrr)
library(stringr)

combine_chunks <- function(chunks_root = "data/pilot_v3_chunks",
                           output_root = "data",
                           verify = TRUE) {
  
  county_dirs <- dir_ls(chunks_root, type = "directory")
  
  if (length(county_dirs) == 0) {
    message("No chunk folders found in ", chunks_root)
    return(invisible(NULL))
  }
  
  summary_rows <- list()
  
  for (county_dir in county_dirs) {
    county <- basename(county_dir)
    case_dirs <- dir_ls(county_dir, type = "directory")
    
    for (case_dir in case_dirs) {
      case_type <- basename(case_dir)
      
      chunk_files <- dir_ls(case_dir, glob = "*.parquet")
      if (length(chunk_files) == 0) {
        message("Skipping empty combo: ", county, "/", case_type)
        next
      }
      
      message("\nCombining ", county, "/", case_type, 
              " (", length(chunk_files), " chunks)")
      
      combined <- map_dfr(chunk_files, read_parquet)
      
      n_before <- nrow(combined)
      combined <- combined |> distinct()
      n_after <- nrow(combined)
      if (n_before != n_after) {
        message("  De-duplicated: ", n_before - n_after, " duplicate rows removed")
      }
      
      out_path <- file.path(output_root, 
                            sprintf("pilot_v3_%s_%s.parquet", county, case_type))
      
      write_parquet(combined, out_path)
      
      if (verify) {
        check <- read_parquet(out_path)
        if (nrow(check) != nrow(combined)) {
          stop("CRITICAL: combine verify failed for ", out_path,
               " — wrote ", nrow(combined), " rows, read back ", nrow(check))
        }
      }
      
      message("  → ", basename(out_path), 
              " (", nrow(combined), " rows, ", ncol(combined), " cols, ",
              round(file_size(out_path) / 1024, 1), " KB)")
      
      summary_rows[[length(summary_rows) + 1]] <- tibble(
        county = county,
        case_type = case_type,
        n_chunks = length(chunk_files),
        n_rows = nrow(combined),
        n_cols = ncol(combined),
        n_dates = length(unique(combined$query_date)),
        size_kb = round(file_size(out_path) / 1024, 1),
        path = out_path
      )
    }
  }
  
  if (length(summary_rows) > 0) {
    summary_df <- bind_rows(summary_rows)
    message("\n=== Combine summary ===")
    print(summary_df, n = 50)
    invisible(summary_df)
  } else {
    invisible(NULL)
  }
}

if (sys.nframe() == 0) {
  combine_chunks()
}
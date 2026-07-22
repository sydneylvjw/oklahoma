# scripts/production/phase1/chunk_writer.R
# Write-once immutable chunks + append-only manifest. Assumes config.R sourced.

suppressPackageStartupMessages({ library(arrow); library(fs) })

chunk_dir <- function(county, case_type) {
  d <- fs::path(CHUNK_ROOT, county, case_type); fs::dir_create(d); d
}

next_chunk_index <- function(county, case_type) {
  ex <- fs::dir_ls(chunk_dir(county, case_type), glob = "*.parquet")
  if (!length(ex)) return(1L)
  max(as.integer(sub("^chunk_(\\d+)\\.parquet$", "\\1", fs::path_file(ex))), na.rm = TRUE) + 1L
}

# temp write + atomic rename: no partial/corrupt chunk even if killed mid-write
write_chunk <- function(df, county, case_type) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  path <- fs::path(chunk_dir(county, case_type),
                   sprintf("chunk_%05d.parquet", next_chunk_index(county, case_type)))
  tmp <- paste0(path, ".tmp")
  arrow::write_parquet(df, tmp)
  fs::file_move(tmp, path)
  path
}

# ---- Resume manifest -------------------------------------------------------
manifest_path <- function() fs::path(LOG_DIR, "manifest.csv")
manifest_key  <- function(county, case_type, date)
  sprintf("%s|%s|%s", county, case_type, format(as.Date(date), "%Y-%m-%d"))

load_completed <- function() {
  p <- manifest_path()
  if (!fs::file_exists(p)) return(character(0))
  m <- utils::read.csv(p, stringsAsFactors = FALSE)
  if (!nrow(m)) return(character(0))
  m$key
}

record_completed <- function(county, case_type, date, status, n_rows, chunk_path) {
  p <- manifest_path(); exists <- fs::file_exists(p)
  row <- data.frame(
    key = manifest_key(county, case_type, date), county = county,
    case_type = case_type, date = format(as.Date(date), "%Y-%m-%d"),
    status = status, n_rows = n_rows,
    chunk = if (is.null(chunk_path)) NA_character_ else as.character(chunk_path),
    ts = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"), stringsAsFactors = FALSE)
  utils::write.table(row, p, sep = ",", row.names = FALSE,
                     col.names = !exists, append = exists)
}

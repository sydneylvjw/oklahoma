# scripts/phase1/chunk_writer.R
# Write-once, immutable checkpoints + an append-only manifest for resumability
# and reproducibility. Files are never modified after they are written.
# Assumes config.R is already sourced (CHUNK_ROOT, LOG_DIR).

suppressPackageStartupMessages({
  library(arrow)
  library(fs)
})

chunk_dir <- function(county, case_type) {
  d <- fs::path(CHUNK_ROOT, county, case_type)
  fs::dir_create(d)
  d
}

next_chunk_index <- function(county, case_type) {
  d <- chunk_dir(county, case_type)
  existing <- fs::dir_ls(d, glob = "*.parquet")
  if (length(existing) == 0) return(1L)
  nums <- as.integer(sub("^chunk_(\\d+)\\.parquet$", "\\1",
                         fs::path_file(existing)))
  max(nums, na.rm = TRUE) + 1L
}

# Write to a temp name, then atomic rename -> no partial/corrupt chunks even if
# the process dies mid-write. Nothing here ever touches an existing file.
write_chunk <- function(df, county, case_type) {
  if (is.null(df) || nrow(df) == 0) return(NULL)
  idx  <- next_chunk_index(county, case_type)
  path <- fs::path(chunk_dir(county, case_type),
                   sprintf("chunk_%04d.parquet", idx))
  tmp  <- fs::path(paste0(path, ".tmp"))
  arrow::write_parquet(df, tmp)
  fs::file_move(tmp, path)
  path
}

# ---- Resume manifest (append-only audit trail) -----------------------------
manifest_path <- function() fs::path(LOG_DIR, "manifest.csv")

manifest_key <- function(county, case_type, date) {
  sprintf("%s|%s|%s", county, case_type, format(as.Date(date), "%Y-%m-%d"))
}

load_completed <- function() {
  p <- manifest_path()
  if (!fs::file_exists(p)) return(character(0))
  m <- utils::read.csv(p, stringsAsFactors = FALSE)
  if (nrow(m) == 0) return(character(0))
  m$key
}

record_completed <- function(county, case_type, date, status, n_rows, chunk_path) {
  p <- manifest_path()
  exists <- fs::file_exists(p)
  row <- data.frame(
    key       = manifest_key(county, case_type, date),
    county    = county,
    case_type = case_type,
    date      = format(as.Date(date), "%Y-%m-%d"),
    status    = status,
    n_rows    = n_rows,
    chunk     = if (is.null(chunk_path)) NA_character_ else as.character(chunk_path),
    ts        = format(Sys.time(), "%Y-%m-%dT%H:%M:%S"),
    stringsAsFactors = FALSE
  )
  utils::write.table(row, p, sep = ",", row.names = FALSE,
                     col.names = !exists, append = exists)
}

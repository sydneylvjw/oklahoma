# scripts/production/phase2/parse_case_detail.R
# Stage 2 parser: one stored case-detail page -> tidy docket rows + case metadata.
# Pure function of HTML; no network. Map it over the stored .html.gz files.

suppressPackageStartupMessages({
  library(rvest); library(xml2); library(stringr)
  library(dplyr); library(tibble); library(jsonlite); library(fs)
})

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a
.clean <- function(x) str_squish(str_replace_all(x %||% "", "\u00a0", " "))

parse_amount <- function(x) {
  x <- .clean(x)
  if (is.na(x) || x == "") return(NA_real_)
  suppressWarnings(as.numeric(str_replace_all(x, "[^0-9.\\-]", "")))
}
parse_mdy   <- function(x) as.Date(.clean(x), format = "%m-%d-%Y")
parse_slash <- function(x) as.Date(.clean(x), format = "%m/%d/%Y")

parse_case_detail <- function(html, source_file = NA_character_) {
  doc <- read_html(html)

  # ---- case-level metadata: prefer the embedded JSON blob ----
  js   <- html_text2(html_element(doc, "script#json_style"))
  meta <- tryCatch(jsonlite::fromJSON(js), error = function(e) list())
  case_number <- meta$casenumber %||% NA_character_
  court       <- meta$court      %||% NA_character_
  style       <- meta$style      %||% NA_character_
  cmid        <- meta$cmid       %||% NA_character_

  cap <- html_text2(html_element(doc, "table.caseStyle")) %||% ""
  filed  <- str_match(cap, "Filed:\\s*([0-9/]+)")[, 2]
  closed <- str_match(cap, "Closed:\\s*([0-9/]+)")[, 2]
  judge  <- .clean(str_match(cap, "Judge:\\s*([^\\n]+)")[, 2])
  left   <- html_text2(html_element(doc, "table.caseStyle td")) %||% ""
  defendant <- .clean(str_match(left, "v\\.\\s*(.+)")[, 2])

  # ---- docket rows ----
  rows <- html_elements(doc, "table.docketlist tr.docketRow")
  entries <- list()
  for (r in rows) {
    cls <- html_attr(r, "class") %||% ""
    tds <- html_elements(r, "td")
    if (length(tds) == 0) next
    txt <- vapply(tds, html_text2, character(1))
    txt <- c(txt, rep("", max(0, 6 - length(txt))))   # pad short rows to 6 cols
    desc <- .clean(txt[3])

    if (str_detect(cls, "addtl-text")) {              # wrapped continuation of prior entry
      n <- length(entries)
      if (n > 0 && nzchar(desc))
        entries[[n]]$description <- .clean(paste(entries[[n]]$description, desc))
      next
    }
    entries[[length(entries) + 1]] <- list(
      entry_date  = parse_mdy(txt[1]),
      docket_code = .clean(txt[2]),
      description = desc,
      count       = .clean(txt[4]),
      party       = .clean(txt[5]),
      amount      = parse_amount(txt[6]),
      amount_raw  = .clean(txt[6])
    )
  }

  docket <- if (length(entries) == 0) {
    tibble(entry_date = as.Date(character()), docket_code = character(),
           description = character(), count = character(),
           party = character(), amount = numeric(), amount_raw = character())
  } else bind_rows(lapply(entries, as_tibble))

  docket |>
    mutate(
      case_number = case_number,
      court       = court,
      county      = tolower(court),
      case_type   = str_extract(case_number, "^[A-Z]+"),
      style       = style,
      cmid        = cmid,
      filed_date  = parse_slash(filed),
      closed_date = parse_slash(closed),
      judge       = judge,
      defendant   = defendant,
      source_file = source_file,
      .before = 1
    )
}

# read one stored gzipped page and parse it
parse_case_file <- function(path) {
  con  <- gzfile(path, "rt", encoding = "UTF-8")
  html <- paste(readLines(con, warn = FALSE), collapse = "\n")
  close(con)
  parse_case_detail(html, source_file = fs::path_file(path))
}

# parser.R — OSCN docket parser
library(rvest)
library(xml2)
library(dplyr)
library(stringr)
library(purrr)
library(tibble)

# ---- functions -----

## ---- clean OSCN whitespace ----
clean_ws <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return(NA_character_)
  x |>
    str_replace_all("[\r\n\t]+", " ") |>
    str_replace_all("\\s+", " ") |>
    str_trim()
}

## ---- parse the parties blob into plaintiff & defendants ----
parse_parties <- function(x) {
  if (is.na(x) || x == "") {
    return(tibble(plaintiff = NA_character_, defendants = NA_character_, 
                  n_defendants = NA_integer_))
  }
  
  clean <- clean_ws(x)
  parts <- str_split(clean, "(?i)\\s*,?\\s*v\\.\\s*", n = 2)[[1]]
  
  plaintiff <- str_remove(parts[1], "(?i),?\\s*Plaintiff\\.?\\s*$") |> str_trim()
  
  if (length(parts) < 2) {
    return(tibble(plaintiff = plaintiff, defendants = NA_character_, 
                  n_defendants = 0L))
  }
  
  def_blob <- parts[2]
  def_strings <- str_split(def_blob, "(?i)Defendant,?\\s*and")[[1]] |>
    map_chr(\(s) str_remove(s, "(?i),?\\s*Defendant\\.?\\s*$") |> str_trim()) |>
    map_chr(\(s) str_remove(s, ",\\s*$") |> str_trim()) |>
    discard(\(s) s == "" | is.na(s))
  
  tibble(
    plaintiff = plaintiff,
    defendants = paste(def_strings, collapse = " | "),
    n_defendants = length(def_strings)
  )
}

## ---- parse hearing type string ----
# Two formats appear in OSCN HTML:
#   Scheduled section:  "RULE 8 HEARING (RL8)" -> code at end
#   Continued section:  "(RCC)COURT COST REVIEW" -> code at start
parse_hearing_type <- function(x) {
  if (is.na(x) || x == "") {
    return(tibble(hearing_type = NA_character_, hearing_code = NA_character_))
  }
  clean <- clean_ws(x)
  
  # Try trailing code first: "...DESCRIPTION (CODE)"
  trail_match <- str_match(clean, "\\(\\s*([A-Z0-9]+)\\s*\\)\\s*$")
  if (!is.na(trail_match[1,2])) {
    code <- str_trim(trail_match[1,2])
    desc <- str_remove(clean, "\\s*\\([^)]+\\)\\s*$") |> str_trim()
    return(tibble(hearing_type = desc, hearing_code = code))
  }
  
  # Strict leading: "(CODE)..." 
  lead_match <- str_match(clean, "^\\(([A-Z0-9]+)\\s*\\)\\s*(.*)$")
  if (!is.na(lead_match[1,2])) {
    return(tibble(hearing_type = str_trim(lead_match[1,3]), 
                  hearing_code = str_trim(lead_match[1,2])))
  }
  
  # No code found
  tibble(hearing_type = clean, hearing_code = NA_character_)
}

## ---- split charges blob into charge lines vs bond lines ----
split_charges_bonds <- function(td_node) {
  if (length(td_node) == 0) {
    return(list(charges = NA_character_, bonds = NA_character_))
  }
  
  raw <- as.character(td_node)
  lines <- str_split(raw, regex("<br\\s*/?>", ignore_case = TRUE))[[1]] |>
    map_chr(\(s) {
      s |>
        str_replace_all("<[^>]+>", "") |>
        clean_ws()
    }) |>
    discard(\(s) is.na(s) || s == "")
  
  lines <- str_remove_all(lines, "\\*+Prior Convictions\\*+") |> 
    str_trim() |> 
    discard(\(s) s == "")
  
  charge_lines <- lines[str_detect(lines, "^\\d+\\.")]
  bond_lines   <- lines[str_detect(lines, "^\\(.*Bond:")]
  
  list(
    charges = if (length(charge_lines) > 0) paste(charge_lines, collapse = " || ") else NA_character_,
    bonds   = if (length(bond_lines)   > 0) paste(bond_lines,   collapse = " || ") else NA_character_
  )
}

## ---- main: parse one OSCN docket page ----
parse_oscn_page <- function(html_text, query_date, county) {
  if (is.na(html_text) || nchar(html_text) < 500) {
    return(tibble(query_date = as.character(query_date), county = county, empty = TRUE))
  }
  
  page <- read_html(html_text)
  body <- page |> html_element("body")
  
  # Walk through time-block headers and case tables in document order
  nodes <- body |> html_elements(xpath = ".//font[@size='3']/b | .//table[@class='clspg']")
  
  current_time <- NA_character_
  cases <- list()
  
  for (node in nodes) {
    node_name <- xml_name(node)
    
    ## ---- Time block header ----
    if (node_name == "b") {
      txt <- clean_ws(html_text(node))
      if (!is.na(txt) && str_detect(txt, "\\d{1,2}:\\d{2}\\s*(AM|PM)")) {
        current_time <- str_extract(txt, "\\d{1,2}:\\d{2}\\s*(AM|PM)")
      }
      next
    }
    
    ## ---- Case table ----
    if (node_name == "table") {
      case_link <- node |> html_element("a[href*='GetCaseInformation']")
      if (length(case_link) == 0) next
      
      case_number  <- clean_ws(html_text(case_link))
      casemasterid <- str_extract(html_attr(case_link, "href"), "(?<=casemasterid=)\\d+")
      
      hearing_raw <- node |> html_element("font[color='RED']") |> html_text() |> clean_ws()
      all_text    <- clean_ws(html_text(node))
      
      ## ---- Judge: text between "Case Assigned to:" and the hearing description (in red font) ----
      judge <- NA_character_
      if (!is.na(all_text) && str_detect(all_text, "Case Assigned to:") && !is.na(hearing_raw)) {
        # The hearing description (in red) appears immediately after the judge in the text
        # Escape regex special characters in hearing_raw
        hearing_esc <- str_replace_all(hearing_raw, "([\\\\^$.|?*+(){}\\[\\]])", "\\\\\\1")
        judge_match <- str_match(all_text, 
                                 paste0("Case Assigned to:\\s+(.+?)\\s+", hearing_esc))
        if (!is.na(judge_match[1,2])) {
          judge <- str_trim(judge_match[1,2])
        }
      }
      
      ## ---- Attorney: "represented by ATTORNEY_NAME" ----
      attorney <- NA_character_
      att_match <- str_match(all_text, 
                             "represented by\\s+(.+?)(?=\\s+STATE OF|\\s+State (Of|of)|\\s+\\d+\\.|$)")
      if (!is.na(att_match[1,2])) {
        attorney <- str_trim(att_match[1,2])
        attorney <- str_remove(attorney, "\\s{2,}.*$") |> str_trim()
      }
      
      ## ---- Parties: bold text inside the 45%-width TD ----
      parties_raw <- node |> 
        html_element("td[width='45%'] font b") |> 
        html_text() |> clean_ws()
      
      ## Charges + bonds: from the LAST TR's VALIGN=TOP TD ----
      rows <- node |> html_elements("tr")
      charges_bonds <- list(charges = NA_character_, bonds = NA_character_)
      if (length(rows) >= 2) {
        last_row <- rows[[length(rows)]]
        charge_tds <- last_row |> html_elements("td[valign='TOP']")
        if (length(charge_tds) > 0) {
          charges_bonds <- split_charges_bonds(charge_tds[[1]])
        }
      }
      
      parties_parsed <- parse_parties(parties_raw)
      hearing_parsed <- parse_hearing_type(hearing_raw)
      
      cases[[length(cases) + 1]] <- tibble(
        query_date    = as.character(query_date),
        county        = county,
        section       = "scheduled",
        hearing_time  = current_time,
        case_number   = case_number,
        casemasterid  = casemasterid,
        judge         = judge,
        hearing_type  = hearing_parsed$hearing_type,
        hearing_code  = hearing_parsed$hearing_code,
        plaintiff     = parties_parsed$plaintiff,
        defendants    = parties_parsed$defendants,
        n_defendants  = parties_parsed$n_defendants,
        attorney      = attorney,
        charges       = charges_bonds$charges,
        bonds         = charges_bonds$bonds,
        empty         = NA
      )
    }
  }
  
  # ---- Continued/Rescheduled section ----
  # Find a 5-column table whose rows contain case-info links, that ISN'T a clspg table
  all_tables <- body |> html_elements("table")
  cont_table <- NULL
  
  for (tbl in all_tables) {
    cls <- html_attr(tbl, "class")
    if (!is.na(cls) && str_detect(cls, "clspg")) next
    
    first_tr <- tbl |> html_element("tr")
    if (length(first_tr) == 0) next
    
    first_tds <- first_tr |> html_elements("td")
    if (length(first_tds) != 5) next
    
    has_case_link <- length(first_tds[[2]] |> html_elements("a[href*='GetCaseInformation']")) > 0
    if (!has_case_link) next
    
    cont_table <- tbl
    break
  }
  
  if (!is.null(cont_table)) {
    cont_rows <- cont_table |> html_elements("tr")
    current_cont_time <- NA_character_
    
    for (r in cont_rows) {
      tds <- r |> html_elements("td")
      if (length(tds) < 5) next
      
      time_cell <- clean_ws(html_text(tds[[1]]))
      if (!is.na(time_cell) && time_cell != "") {
        current_cont_time <- time_cell
      }
      
      case_link <- tds[[2]] |> html_element("a[href*='GetCaseInformation']")
      if (length(case_link) == 0) next
      
      case_number  <- clean_ws(html_text(case_link))
      casemasterid <- str_extract(html_attr(case_link, "href"), "(?<=casemasterid=)\\d+")
      
      parties_raw <- clean_ws(html_text(tds[[4]]))
      hearing_raw <- clean_ws(html_text(tds[[5]]))
      
      parties_parsed <- parse_parties(parties_raw)
      hearing_parsed <- parse_hearing_type(hearing_raw)
      
      cases[[length(cases) + 1]] <- tibble(
        query_date    = as.character(query_date),
        county        = county,
        section       = "continued_rescheduled",
        hearing_time  = current_cont_time,
        case_number   = case_number,
        casemasterid  = casemasterid,
        judge         = NA_character_,
        hearing_type  = hearing_parsed$hearing_type,
        hearing_code  = hearing_parsed$hearing_code,
        plaintiff     = parties_parsed$plaintiff,
        defendants    = parties_parsed$defendants,
        n_defendants  = parties_parsed$n_defendants,
        attorney      = NA_character_,
        charges       = NA_character_,
        bonds         = NA_character_,
        empty         = NA
      )
    }
  }
  
  if (length(cases) == 0) {
    return(tibble(query_date = as.character(query_date), county = county, empty = TRUE))
  }
  
  bind_rows(cases)
}
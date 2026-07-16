# scripts/phase2/sample_counties.R
# OPTION B: capped, pre/post-balanced random sample of cases per county, to
# estimate Rule 8 / LFO activity for COUNTY SELECTION ONLY. Reuses the Phase 2
# fetch/parse/classify pipeline. Sampled pages land in the SAME case_details
# tree, so a later full pull of the chosen counties SKIPS them (no wasted fetch).
#
# Screening choices (deliberately different from the confirmatory analysis):
#  - Rule 8 detected by TEXT ("RULE 8"/"ABILITY TO PAY") OR dictionary code, so
#    it works across all 13 counties even before the dictionary is complete.
#  - LFO presence = any positive assessed amount (dictionary-independent).
# These are robust-but-approximate on purpose; the real analysis uses the codes.

suppressPackageStartupMessages({ library(arrow); library(dplyr); library(stringr) })
source("scripts/phase2/fetch_details.R")   # run_details(), detail fetch infra
source("scripts/phase1/config.R")          # CHUNK_ROOT, PRE_END, PROJECT_ROOT

# 1) Frame: unique (county, case_type, case_number, period) from Phase 1.
sample_frame <- function() {
  open_dataset(CHUNK_ROOT) |>
    select(county, case_type, case_number, query_date) |>
    collect() |>
    mutate(period = ifelse(query_date <= PRE_END, "pre", "post")) |>
    distinct(county, case_type, case_number, period)
}

# 2) Draw n_per_period cases per county per period (graceful if a stratum is
#    short: slice_sample takes all available). Logs achieved N.
draw_sample <- function(frame, n_per_period = 75, seed = 20221101) {
  # Reproducibility needs BOTH a fixed seed and a fixed input order -- parquet
  # read order isn't guaranteed, so sort deterministically before sampling.
  set.seed(seed, kind = "Mersenne-Twister", sample.kind = "Rejection")
  drawn <- frame |>
    arrange(county, period, case_type, case_number) |>
    group_by(county, period) |>
    slice_sample(n = n_per_period) |>   # takes all if a stratum has < n
    ungroup()
  achieved <- drawn |> count(county, period, name = "n_drawn") |>
    tidyr::pivot_wider(names_from = period, values_from = n_drawn, values_fill = 0)
  message("Achieved sample per county (pre/post):")
  print(achieved)
  drawn
}

# 3) Fetch queue (a case drawn in both periods is fetched once).
sample_queue <- function(drawn) {
  drawn |> distinct(county, case_type, case_number) |>
    arrange(county, case_type, case_number)
}

# 4) Per-county, per-period screening summary. Requires the sampled cases to be
#    parsed + classified first (run_parse(); run_classify()).
summarise_sample <- function(drawn,
                             classified_root = file.path(PROJECT_ROOT, "data", "case_docket_classified")) {
  cl <- open_dataset(classified_root) |> collect() |>
    mutate(
      ev_period  = ifelse(entry_date <= PRE_END, "pre", "post"),
      rule8_flag = is_rule8 | str_detect(toupper(description), "RULE 8|ABILITY TO PAY"),
      lfo_flag   = !is.na(amount) & amount > 0 & !is_conversion_artifact
    )
  case_flags <- cl |>
    group_by(county, case_number, ev_period) |>
    summarise(any_rule8 = any(rule8_flag, na.rm = TRUE),
              any_lfo   = any(lfo_flag,   na.rm = TRUE), .groups = "drop")
  
  drawn |>
    left_join(case_flags,
              by = c("county", "case_number", "period" = "ev_period")) |>
    group_by(county, period) |>
    summarise(
      n_sampled  = n(),
      rule8_rate = mean(any_rule8, na.rm = TRUE),
      lfo_rate   = mean(any_lfo,   na.rm = TRUE),
      .groups = "drop"
    ) |>
    mutate(rule8_ci = sqrt(rule8_rate * (1 - rule8_rate) / n_sampled) * 1.96) |>
    arrange(county, period)
}

# 5) PI-facing markdown summary: Option A (volume) + Option B (sampled rates)
#    in one readable document. Requires the sample to be parsed + classified.
.fmt_pct  <- function(x) ifelse(is.na(x), "\u2014", sprintf("%.1f%%", 100 * x))
.md_table <- function(df) {
  cols   <- names(df)
  header <- paste0("| ", paste(cols, collapse = " | "), " |")
  sep    <- paste0("|", paste(rep(" --- ", length(cols)), collapse = "|"), "|")
  body   <- apply(df, 1, function(r) paste0("| ", paste(trimws(r), collapse = " | "), " |"))
  paste(c(header, sep, body), collapse = "\n")
}

write_pi_summary <- function(drawn,
                             out_path = file.path(PROJECT_ROOT, "reports", "county_screening.md"),
                             classified_root = file.path(PROJECT_ROOT, "data", "case_docket_classified")) {
  
  suppressPackageStartupMessages({ library(tidyr) })
  dir.create(dirname(out_path), showWarnings = FALSE, recursive = TRUE)
  
  # --- Option A: pre/post volume by county ---
  vol <- open_dataset(CHUNK_ROOT) |>
    select(county, case_number, query_date) |>
    collect() |>
    mutate(period = ifelse(query_date <= PRE_END, "pre", "post")) |>
    distinct(county, period, case_number) |>
    count(county, period, name = "n") |>
    pivot_wider(names_from = period, values_from = n, values_fill = 0) |>
    mutate(total = pre + post, pct = round(100 * (post - pre) / pmax(pre, 1), 1)) |>
    arrange(desc(total))
  volA <- vol |>
    transmute(County = county, Pre = pre, Post = post, Total = total,
              `% Change` = sprintf("%+.1f%%", pct))
  
  # --- Option B: sampled Rule 8 / LFO rates, pre vs post side by side ---
  rates <- summarise_sample(drawn, classified_root)
  wide <- rates |>
    pivot_wider(names_from = period,
                values_from = c(n_sampled, rule8_rate, rule8_ci, lfo_rate))
  ratesB <- wide |>
    transmute(County = county,
              `N pre/post` = paste0(n_sampled_pre, "/", n_sampled_post),
              `Rule 8 pre` = .fmt_pct(rule8_rate_pre),
              `Rule 8 post` = .fmt_pct(rule8_rate_post),
              `Rule 8 chg` = sprintf("%+.1f pp", 100 * (rule8_rate_post - rule8_rate_pre)),
              `LFO pre` = .fmt_pct(lfo_rate_pre),
              `LFO post` = .fmt_pct(lfo_rate_post)) |>
    arrange(County)
  
  doc <- c(
    "# OCIS County Screening — HB 2259 Pre/Post",
    sprintf("_Generated %s. Pre: %s to %s. Post: %s to %s._",
            format(Sys.Date()), PRE_START, PRE_END, POST_START, POST_END),
    "",
    "## Option A — Case volume (all cases, unique)",
    "Structural screen for coverage; use to pick counties by size (e.g. 2 rural + 2 urban).",
    "", .md_table(volA), "",
    "## Option B — Sampled Rule 8 / LFO activity",
    sprintf("Balanced random sample (%d pre + %d post per county). Rates are estimates for _selection only_.",
            max(rates$n_sampled), max(rates$n_sampled)),
    "Rule 8 detected by text (\"RULE 8\"/\"ABILITY TO PAY\") or dictionary code; margins are 95%.",
    "", .md_table(ratesB), "",
    "## Reading these",
    "- **Volume ≠ treatment relevance.** Option A shows how much data exists; Option B shows how much of it is ability-to-pay activity.",
    "- **Sample is balanced 50/50 pre/post**, so it measures *rate/composition* change, not raw volume change. Volume change lives in Option A.",
    "- **Screening only.** These sampled rates guide county choice; the confirmatory analysis uses full data at natural volumes and code-based classification.",
    "- **Dictionary coverage is Cleveland-seeded**, so LFO rates in other counties are approximate until the code dictionary is completed."
  )
  writeLines(doc, out_path)
  message("Wrote ", out_path)
  invisible(out_path)
}
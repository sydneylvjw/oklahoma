# scripts/production/phase1/verify_counties.R
# Confirm every county db code resolves BEFORE launching the multi-hour run.
# One request per county. Writes nothing.
# Run:  Rscript scripts/production/phase1/verify_counties.R

options(oscn.autorun.suppress = TRUE)
source("scripts/production/phase1/pipeline.R")

verify_counties <- function(counties = COUNTIES,
                            probe_date = as.Date("2023-11-15"),
                            case_type = "CF") {
  id <- CASE_TYPE_IDS[[case_type]]
  do.call(rbind, lapply(counties, function(cty) {
    url  <- build_url(cty, id, probe_date)
    resp <- tryCatch(oscn_get(url), error = function(e) e)
    if (inherits(resp, "error"))
      return(data.frame(county = cty, status = "net_error", n_cases = NA_integer_,
                        note = conditionMessage(resp)))
    st   <- httr2::resp_status(resp)
    body <- httr2::resp_body_string(resp)
    if (is_challenge(st, body))
      return(data.frame(county = cty, status = "CHALLENGE", n_cases = NA_integer_,
                        note = "403/Turnstile -- whitelist problem"))
    hits <- regmatches(body, gregexpr("\\b[A-Z]{2}-\\d{4}-\\d+", body))[[1]]
    data.frame(county = cty,
               status  = if (st >= 400) paste0("HTTP_", st) else "ok",
               n_cases = length(unique(hits)),
               note    = if (is.na(body) || nchar(body) < 500) "short/empty body" else "")
  }))
}

if (!interactive()) {
  v <- verify_counties()
  print(v, row.names = FALSE)
  bad <- subset(v, status != "ok")
  if (nrow(bad)) {
    cat("\n*** ", nrow(bad), " county code(s) did NOT resolve -- fix before running. ***\n")
  } else {
    cat("\nAll ", nrow(v), " county codes resolved. Safe to launch run_phase1.R\n", sep = "")
  }
}

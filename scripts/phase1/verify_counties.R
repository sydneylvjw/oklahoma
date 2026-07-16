# scripts/phase1/verify_counties.R
# Confirm each county's OSCN db code resolves BEFORE committing the full pull.
# One request per county (CF, a busy weekday). Writes nothing.
# Run from project root:  Rscript scripts/phase1/verify_counties.R

source("scripts/phase1/pipeline.R")

verify_counties <- function(counties = COUNTIES,
                            probe_date = as.Date("2022-11-15"),
                            case_type = "CF") {
  id <- CASE_TYPE_IDS[[case_type]]
  rows <- lapply(counties, function(cty) {
    url  <- build_url(cty, id, probe_date)
    resp <- tryCatch(oscn_get(url), error = function(e) e)
    if (inherits(resp, "error"))
      return(data.frame(county = cty, status = "net_error",
                        n_cases = NA_integer_, note = conditionMessage(resp)))
    st   <- httr2::resp_status(resp)
    body <- httr2::resp_body_string(resp)
    if (is_challenge(st, body))
      return(data.frame(county = cty, status = "challenge",
                        n_cases = NA_integer_, note = "403/Turnstile"))
    hits <- regmatches(body, gregexpr("\\b[A-Z]{2}-\\d{4}-\\d+", body))[[1]]
    data.frame(
      county  = cty,
      status  = if (st >= 400) paste0("HTTP_", st) else "ok",
      n_cases = length(unique(hits)),
      note    = if (is.na(body) || nchar(body) < 500) "short/empty body" else ""
    )
  })
  do.call(rbind, rows)
}

if (!interactive()) print(verify_counties(), row.names = FALSE)

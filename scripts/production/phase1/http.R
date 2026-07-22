# scripts/production/phase1/http.R
# Rate-limited HTTP layer. Assumes config.R is sourced.

suppressPackageStartupMessages({ library(httr2) })

.oscn_last_request <- NULL

.throttle <- function() {
  if (!is.null(.oscn_last_request)) {
    elapsed <- as.numeric(difftime(Sys.time(), .oscn_last_request, units = "secs"))
    wait <- (MIN_INTERVAL_S - elapsed) + runif(1, 0, JITTER_MAX_S)
    if (wait > 0) Sys.sleep(wait)
  }
  assign(".oscn_last_request", Sys.time(), envir = .GlobalEnv)
}

# A challenge or 403 means the whitelist stopped working -- halt, don't grind.
is_challenge <- function(status, body) {
  isTRUE(status %in% c(201L, 403L)) ||
    grepl("OSCN Turnstile|cf-turnstile|challenges\\.cloudflare\\.com|Just a moment",
          body, ignore.case = TRUE)
}

# Does not throw on HTTP status (caller classifies); retries transient 429/5xx.
oscn_get <- function(url, max_tries = 3) {
  .throttle()
  httr2::request(url) |>
    httr2::req_user_agent(USER_AGENT) |>
    httr2::req_timeout(30) |>
    httr2::req_error(is_error = function(resp) FALSE) |>
    httr2::req_retry(
      max_tries = max_tries,
      backoff = function(i) min(30, 2^i),
      is_transient = function(resp)
        httr2::resp_status(resp) %in% c(429, 500, 502, 503, 504)
    ) |>
    httr2::req_perform()
}

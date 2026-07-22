# scripts/pilot/phase1/http.R
# Rate-limited HTTP layer for the whitelisted OSCN pull.
# Assumes config.R is already sourced (USER_AGENT, MIN_INTERVAL_S, JITTER_MAX_S).

suppressPackageStartupMessages({
  library(httr2)
})

.oscn_last_request <- NULL

.throttle <- function() {
  now <- Sys.time()
  if (!is.null(.oscn_last_request)) {
    elapsed <- as.numeric(difftime(now, .oscn_last_request, units = "secs"))
    wait <- (MIN_INTERVAL_S - elapsed) + runif(1, 0, JITTER_MAX_S)
    if (wait > 0) Sys.sleep(wait)
  }
  assign(".oscn_last_request", Sys.time(), envir = .GlobalEnv)
}

# A Cloudflare/Turnstile challenge or a 403/201 = the whitelist isn't working.
is_challenge <- function(status, body) {
  isTRUE(status %in% c(201L, 403L)) ||
    grepl("OSCN Turnstile|cf-turnstile|challenges\\.cloudflare\\.com|Just a moment",
          body, ignore.case = TRUE)
}

# Performs the request WITHOUT throwing on HTTP status codes, so the caller can
# classify (challenge / empty / error / ok). Only network/timeout errors throw.
# Transient 429/5xx are retried with backoff.
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

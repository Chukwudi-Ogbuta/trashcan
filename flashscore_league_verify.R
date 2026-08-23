# ============================================================
# FLASHSCORE LEAGUE VERIFICATION
# Re-verifies every is_league=TRUE entry in league_directory.rds
# by visiting the /standings/ page and confirming a real table exists.
# Adds `verified` column to the rds.
# ============================================================

library(rvest)
library(dplyr)
library(httr)
library(jsonlite)

BASE_PATH      <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
DIRECTORY_PATH <- file.path(BASE_PATH, "league_directory.rds")
BASE_URL       <- "https://www.flashscore.com"

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# SELENIUM (same as other scripts)
# ════════════════════════════════════════════════════════════

SELENIUM <- list(port = NULL, session_id = NULL, started = FALSE,
                  chromedriver_path = NULL, chrome_path = NULL)

find_free_port <- function(candidates = c(8888, 8889, 7777, 7778, 6666, 5555, 9876)) {
  cd <- SELENIUM$chromedriver_path
  for (i in 1:3) {
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE); Sys.sleep(0.5)
  }
  Sys.sleep(1)
  test_port <- function(port) {
    log_path <- tempfile(fileext = ".log")
    system2(cd, args = c(paste0("--port=", port),
                          paste0("--log-path=", log_path)), wait = FALSE)
    Sys.sleep(2)
    log_lines <- tryCatch(readLines(log_path, warn = FALSE), error = function(e) "")
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE); Sys.sleep(1)
    !any(grepl("SEVERE|ERR_ADDRESS_IN_USE", log_lines))
  }
  for (port in candidates) if (test_port(port)) return(port)
  set.seed(as.integer(Sys.time()))
  for (attempt in 1:20) {
    port <- sample(10000:60000, 1)
    if (test_port(port)) return(port)
  }
  stop("No free port found.")
}

start_selenium <- function(chromedriver_path = "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe",
                            chrome_path = "C:/Program Files/Google/Chrome/Application/chrome.exe") {
  message("[ SELENIUM ] Starting...")
  system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
         ignore.stderr = TRUE, wait = TRUE); Sys.sleep(2)
  SELENIUM$chromedriver_path <<- chromedriver_path
  SELENIUM$chrome_path       <<- chrome_path
  port <- find_free_port()
  SELENIUM$port <<- port
  system2(chromedriver_path, args = paste0("--port=", port), wait = FALSE)
  Sys.sleep(3)
  response <- tryCatch(POST(
    paste0("http://localhost:", port, "/session"),
    body = list(capabilities = list(alwaysMatch = list(
      browserName = "chrome",
      `goog:chromeOptions` = list(
        binary = chrome_path,
        args = list("--no-sandbox", "--disable-dev-shm-usage",
                    "--disable-blink-features=AutomationControlled",
                    "--disable-extensions", "--start-maximized"),
        excludeSwitches = list("enable-automation"),
        useAutomationExtension = FALSE)))),
    encode = "json", timeout(30)), error = function(e) NULL)
  if (is.null(response) || status_code(response) != 200) stop("Selenium failed")
  sd <- fromJSON(content(response, as = "text"))
  SELENIUM$session_id <<- sd$sessionId %||% sd$value$sessionId
  SELENIUM$started <<- TRUE
  message("[ SELENIUM ] Warming up flashscore.com...")
  selenium_navigate(paste0(BASE_URL, "/")); Sys.sleep(5)
  wait_for_page(max_wait = 30); dismiss_cookie_popup(); Sys.sleep(3)
  message("[ SELENIUM ] Ready.")
}

selenium_navigate <- function(url) {
  POST(paste0("http://localhost:", SELENIUM$port, "/session/",
               SELENIUM$session_id, "/url"),
       body = list(url = url), encode = "json", timeout(30))
}

selenium_get_source <- function() {
  res <- GET(paste0("http://localhost:", SELENIUM$port, "/session/",
                     SELENIUM$session_id, "/source"), timeout(30))
  html_raw <- fromJSON(content(res, as = "text"))$value
  tryCatch(read_html(html_raw), error = function(e) NULL)
}

dismiss_cookie_popup <- function() {
  body_json <- '{"script": "var b=document.querySelectorAll(\\"button,a\\");var k=[\\"Reject\\",\\"Decline\\",\\"Accept\\",\\"Agree\\"];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}", "args": []}'
  tryCatch({
    POST(paste0("http://localhost:", SELENIUM$port, "/session/",
                 SELENIUM$session_id, "/execute/sync"),
         body = body_json, encode = "raw",
         add_headers(`Content-Type` = "application/json"), timeout(10))
    Sys.sleep(1)
  }, error = function(e) invisible(NULL))
}

wait_for_page <- function(max_wait = 20) {
  body_json <- '{"script": "return document.title;", "args": []}'
  for (i in seq_len(max_wait)) {
    Sys.sleep(1)
    res <- tryCatch(POST(paste0("http://localhost:", SELENIUM$port, "/session/",
                                  SELENIUM$session_id, "/execute/sync"),
                          body = body_json, encode = "raw",
                          add_headers(`Content-Type` = "application/json"),
                          timeout(5)), error = function(e) NULL)
    if (!is.null(res)) {
      title <- tryCatch({ v <- fromJSON(content(res, as = "text"))$value
                          if (is.null(v)) "" else as.character(v)[1] },
                        error = function(e) "")
      if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
          nchar(title) > 0) return(TRUE)
    }
  }
  return(FALSE)
}

selenium_get_html <- function(url) {
  Sys.sleep(runif(1, 1, 2)); selenium_navigate(url); Sys.sleep(runif(1, 4, 6))
  wait_for_page(max_wait = 20); dismiss_cookie_popup(); Sys.sleep(1)
  selenium_get_source()
}

stop_selenium <- function() {
  if (SELENIUM$started) {
    tryCatch(DELETE(paste0("http://localhost:", SELENIUM$port, "/session/",
                             SELENIUM$session_id), timeout(10)),
             error = function(e) invisible(NULL))
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE)
    SELENIUM$started <<- FALSE
    message("[ SELENIUM ] Stopped.")
  }
}

# ════════════════════════════════════════════════════════════
# STANDINGS PAGE VERIFICATION
# ════════════════════════════════════════════════════════════

# Returns TRUE if standings page has a real table with ranked teams
verify_standings <- function(league_url) {
  standings_url <- paste0(league_url, "standings/")
  page <- tryCatch(selenium_get_html(standings_url), error = function(e) NULL)
  if (is.null(page)) return(NA)

  rows <- page %>% html_nodes("div.ui-table__row")
  if (length(rows) == 0) return(FALSE)

  # Confirm at least one row has a rank cell with a real number AND a team name
  valid_count <- 0
  for (r in rows) {
    rank_node <- r %>% html_node("div.tableCellRank")
    team_node <- r %>% html_node("a.tableCellParticipant__name")
    if (is.null(rank_node) || is.null(team_node)) next
    rank_str <- html_text(rank_node, trim = TRUE)
    team_name <- html_text(team_node, trim = TRUE)
    rank <- suppressWarnings(as.integer(gsub("\\.", "", rank_str)))
    if (!is.na(rank) && rank >= 1 && !is.na(team_name) && nchar(team_name) > 0) {
      valid_count <- valid_count + 1
    }
  }
  valid_count >= 1
}

# ════════════════════════════════════════════════════════════
# RUN
# ════════════════════════════════════════════════════════════

cat("\n=== FLASHSCORE LEAGUE VERIFICATION ===\n")

d <- readRDS(DIRECTORY_PATH)
cat("Total directory entries:", nrow(d), "\n")

# Target only entries currently flagged as leagues
to_verify <- which(d$is_league == TRUE & !is.na(d$is_league))
cat("Leagues to verify:", length(to_verify), "\n\n")

# Initialize verified column (preserve any existing values if rerun)
if (!"verified" %in% names(d)) d$verified <- NA

# Skip entries already verified successfully (in case of resume after crash)
already_done <- which(!is.na(d$verified))
cat("Already verified (from previous run):", length(already_done), "\n")

to_verify <- setdiff(to_verify, already_done)
cat("Remaining to verify:", length(to_verify), "\n\n")

start_selenium()

for (j in seq_along(to_verify)) {
  i <- to_verify[j]
  url <- d$league_url[i]
  cat("[", j, "/", length(to_verify), "] ", d$country[i], " — ",
      d$league_name[i], "... ", sep="")
  result <- tryCatch(verify_standings(url), error = function(e) NA)
  d$verified[i] <- result
  cat(if (isTRUE(result)) "OK\n"
      else if (isFALSE(result)) "FAIL\n"
      else "UNKNOWN\n")
  # Save every 10 entries
  if (j %% 10 == 0) saveRDS(d, DIRECTORY_PATH)
}

stop_selenium()
saveRDS(d, DIRECTORY_PATH)

cat("\n=== VERIFICATION SUMMARY ===\n")
cat("Total leagues checked:", length(to_verify) + length(already_done), "\n")
cat("Verified OK:          ", sum(d$verified == TRUE, na.rm = TRUE), "\n")
cat("Verification failed:  ", sum(d$verified == FALSE, na.rm = TRUE), "\n")
cat("Errors (NA):          ", sum(is.na(d$verified) & d$is_league == TRUE), "\n")
cat("\nFinal counts:\n")
verified_leagues <- d[d$verified == TRUE & !is.na(d$verified), ]
cat("Final verified leagues:", nrow(verified_leagues), "\n")
cat("Countries covered:    ", length(unique(verified_leagues$country)), "\n")

cat("\nTo inspect:\n")
cat("  d <- readRDS('", DIRECTORY_PATH, "')\n", sep="")
cat("  verified <- d[d$verified == TRUE & !is.na(d$verified), ]\n")
cat("  write.csv(verified, '", file.path(BASE_PATH, "verified_leagues.csv"),
    "', row.names=FALSE)\n", sep="")

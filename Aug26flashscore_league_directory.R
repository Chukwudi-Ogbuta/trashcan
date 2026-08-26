# ============================================================
# FLASHSCORE LEAGUE DIRECTORY — one-time global scrape
# Scrapes all countries → all leagues/cups → validates leagues by standings
# Output: league_directory.rds saved to New Projects 3/
# ============================================================

library(rvest)
library(dplyr)
library(httr)
library(jsonlite)

BASE_PATH      <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
if (!dir.exists(BASE_PATH)) dir.create(BASE_PATH, recursive = TRUE)
DIRECTORY_PATH <- file.path(BASE_PATH, "league_directory.rds")
BASE_URL       <- "https://www.flashscore.com"

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# SELENIUM (same as flashscore_demo.R)
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
  Sys.sleep(runif(1, 2, 4)); selenium_navigate(url); Sys.sleep(runif(1, 5, 8))
  wait_for_page(max_wait = 25); dismiss_cookie_popup(); Sys.sleep(2)
  selenium_get_source()
}

js_eval <- function(script_text) {
  body_json <- sprintf('{"script": %s, "args": []}',
                       toJSON(script_text, auto_unbox = TRUE))
  tryCatch({
    r <- POST(paste0("http://localhost:", SELENIUM$port, "/session/",
                     SELENIUM$session_id, "/execute/sync"),
              body = body_json, encode = "raw",
              add_headers(`Content-Type` = "application/json"),
              timeout(15))
    fromJSON(content(r, as = "text"))$value
  }, error = function(e) { message("JS err: ", e$message); NULL })
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
# COUNTRY + LEAGUE EXPANSION
# ════════════════════════════════════════════════════════════

# Click "Show more" on country list until it disappears (gets us all countries)
expand_country_list <- function() {
  message("[ EXPAND ] Clicking 'Show more' until all countries visible...")
  for (i in 1:20) {  # safety cap
    res <- js_eval(paste0(
      "var sm = document.querySelectorAll('span.lmc__itemMore, .lmc__itemMore');",
      "var clicked = 0;",
      "for (var i = 0; i < sm.length; i++) {",
      "  try { sm[i].scrollIntoView({block:'center'}); sm[i].click(); clicked++; } catch(e) {}",
      "}",
      "return clicked;"))
    Sys.sleep(2)
    if (is.null(res) || (is.numeric(res) && res == 0)) {
      message("[ EXPAND ] Done. No more 'Show more' buttons.")
      break
    }
    message("  Iter ", i, " clicked: ", res)
  }
}

# Click ALL country blocks to expand them so leagues become visible
expand_all_countries <- function() {
  message("[ EXPAND ] Expanding all country blocks...")
  res <- js_eval(paste0(
    "var blocks = document.querySelectorAll('a.lmc__element');",
    "var clicked = 0;",
    "for (var i = 0; i < blocks.length; i++) {",
    "  try {",
    "    if (!blocks[i].classList.contains('lmc__itemOpen')) {",
    "      blocks[i].click();",
    "      clicked++;",
    "    }",
    "  } catch(e) {}",
    "}",
    "return clicked;"))
  Sys.sleep(5)  # give DOM time to populate league entries
  message("[ EXPAND ] Clicked open ", res %||% 0, " country blocks.")
}

# Parse country + league structure from page source
parse_directory <- function(page) {
  blocks <- page %>% html_nodes("div.lmc__block")
  message("[ PARSE ] Found ", length(blocks), " country blocks")
  out <- data.frame()
  for (b in blocks) {
    country_link <- b %>% html_node("a.lmc__element")
    if (is.null(country_link)) next
    country_name <- country_link %>% html_node("span.lmc__elementName") %>%
      html_text(trim = TRUE)
    country_url  <- html_attr(country_link, "href")
    if (is.na(country_name) || is.na(country_url)) next
    
    league_nodes <- b %>% html_nodes("span.lmc__template a.lmc__templateHref")
    if (length(league_nodes) == 0) next
    
    for (ln in league_nodes) {
      name <- html_text(ln, trim = TRUE)
      url  <- html_attr(ln, "href")
      if (is.na(name) || is.na(url)) next
      out <- rbind(out, data.frame(
        country      = country_name,
        country_url  = paste0(BASE_URL, country_url),
        league_name  = name,
        league_url   = paste0(BASE_URL, url),
        stringsAsFactors = FALSE
      ))
    }
  }
  out
}

# ════════════════════════════════════════════════════════════
# KEYWORD CLASSIFICATION — fast pre-filter before standings check
# ════════════════════════════════════════════════════════════

# Keywords that strongly indicate CUP (knockout/non-league competition)
CUP_KEYWORDS <- c(
  "Cup", "Coupe", "Copa", "Coppa", "Pokal", "Beker",
  "Cupa", "Kupa", "Kup", "Kupp", "Taça", "Pohár", "Pokál",
  "Trophy", "Tröphie", "Shield", "Charity Shield",
  "Super Cup", "Supercup", "Supercoupe", "Supercopa", "Supercoppa",
  "Playoff", "Play-off", "Play Off",
  "Promotion", "Relegation",
  "Knockout", "Final",
  "FA ",        # FA Cup, FA Trophy (English non-league knockouts)
  "EFL Cup",
  "Tournament", "Tournoi", "Torneo",
  "Pucharu"      # Polish for "of the Cup"
)

# Keywords that strongly indicate LEAGUE
LEAGUE_KEYWORDS <- c(
  "League", "Liga", "Ligue", "Lega",
  "Bundesliga", "Eredivisie", "Eerste Divisie",
  "Serie A", "Serie B", "Serie C", "Serie D",
  "Premier", "Premiership", "Premiera",
  "Championship", "Champions",  # league-style, not "Cup"
  "Division", "Divisione", "Divizia",
  "Primera", "Segunda", "Tercera",
  "Allsvenskan", "Superettan",
  "Superliga", "Super Liga", "Superligaen",
  "Ekstraklasa",
  "Kategoria", "Kategorie",
  "Mestaruussarja", "Veikkausliiga",
  "1. ", "2. ", "3. ",   # "1. HNL", "2. Bundesliga" etc
  "I Liga", "II Liga", "III Liga",
  "Eliteserien", "OBOS",
  "A-League",
  "MLS",
  "PFL", "FNL",  # Russian
  "J1", "J2", "J3",  # Japan
  "K League"
)

classify_by_name <- function(name) {
  if (is.na(name) || nchar(name) == 0) return("UNKNOWN")
  # Check cup keywords first (more specific / dangerous if missed)
  for (kw in CUP_KEYWORDS) {
    if (grepl(kw, name, ignore.case = TRUE)) return("CUP")
  }
  for (kw in LEAGUE_KEYWORDS) {
    if (grepl(kw, name, ignore.case = TRUE)) return("LEAGUE")
  }
  "UNKNOWN"
}

# ════════════════════════════════════════════════════════════
# LEAGUE VALIDATION — check if URL has a real standings table
# ════════════════════════════════════════════════════════════

# Returns TRUE if standings page has a proper table with at least 1 ranked team row
is_league <- function(league_url) {
  standings_url <- paste0(league_url, "standings/")
  page <- tryCatch(selenium_get_html(standings_url), error = function(e) NULL)
  if (is.null(page)) return(NA)
  
  rows <- page %>% html_nodes("div.ui-table__row")
  if (length(rows) == 0) return(FALSE)
  
  # Check at least one row has a rank cell with a number
  for (r in rows) {
    rank_node <- r %>% html_node("div.tableCellRank")
    if (is.null(rank_node)) next
    rank_str <- html_text(rank_node, trim = TRUE)
    rank <- suppressWarnings(as.integer(gsub("\\.", "", rank_str)))
    if (!is.na(rank) && rank >= 1) return(TRUE)
  }
  FALSE
}

# ════════════════════════════════════════════════════════════
# RUN
# ════════════════════════════════════════════════════════════

cat("\n=== FLASHSCORE LEAGUE DIRECTORY ===\n")

start_selenium()

# Step 1: load homepage with country list
message("\n[ STEP 1 ] Loading homepage...")
selenium_navigate(paste0(BASE_URL, "/"))
Sys.sleep(6); wait_for_page(max_wait = 25); dismiss_cookie_popup(); Sys.sleep(2)

# Step 2: expand country list (click "Show more")
message("\n[ STEP 2 ] Expanding country list...")
expand_country_list()

# Step 3: expand each country to reveal its leagues
message("\n[ STEP 3 ] Expanding all country blocks...")
expand_all_countries()

# Step 4: parse the now-fully-expanded directory
message("\n[ STEP 4 ] Parsing directory...")
page <- selenium_get_source()
raw_directory <- parse_directory(page)
cat("Total entries scraped (leagues + cups):", nrow(raw_directory), "\n")
cat("Unique countries:", length(unique(raw_directory$country)), "\n\n")

# Step 5a: keyword classification (fast, no page loads)
message("\n[ STEP 5a ] Classifying by name keywords...")
raw_directory$classification <- sapply(raw_directory$league_name, classify_by_name)
class_counts <- table(raw_directory$classification)
cat("LEAGUE (by keyword):  ", class_counts["LEAGUE"]  %||% 0, "\n")
cat("CUP (by keyword):     ", class_counts["CUP"]     %||% 0, "\n")
cat("UNKNOWN (need check): ", class_counts["UNKNOWN"] %||% 0, "\n\n")

# Step 5b: standings validation ONLY for UNKNOWN entries
message("[ STEP 5b ] Standings validation for ambiguous entries only...\n")
raw_directory$is_league <- NA
# Auto-fill from keyword classification
raw_directory$is_league[raw_directory$classification == "LEAGUE"] <- TRUE
raw_directory$is_league[raw_directory$classification == "CUP"]    <- FALSE

unknown_idx <- which(raw_directory$classification == "UNKNOWN")
for (j in seq_along(unknown_idx)) {
  i <- unknown_idx[j]
  url <- raw_directory$league_url[i]
  cat("[", j, "/", length(unknown_idx), "] (row ", i, ") ",
      raw_directory$country[i], " — ", raw_directory$league_name[i], "... ", sep="")
  result <- tryCatch(is_league(url), error = function(e) NA)
  raw_directory$is_league[i] <- result
  cat(if (isTRUE(result)) "LEAGUE\n"
      else if (isFALSE(result)) "CUP/OTHER\n"
      else "UNKNOWN\n")
  if (j %% 10 == 0) saveRDS(raw_directory, DIRECTORY_PATH)
}

stop_selenium()

# Final save
saveRDS(raw_directory, DIRECTORY_PATH)

cat("\n=== SUMMARY ===\n")
cat("Total entries:", nrow(raw_directory), "\n")
cat("Leagues:      ", sum(raw_directory$is_league == TRUE, na.rm = TRUE), "\n")
cat("Cups/other:   ", sum(raw_directory$is_league == FALSE, na.rm = TRUE), "\n")
cat("Unknown:      ", sum(is.na(raw_directory$is_league)), "\n")
cat("\nSaved to:", DIRECTORY_PATH, "\n")
cat("\nTo inspect leagues only:\n")
cat("  d <- readRDS('", DIRECTORY_PATH, "')\n", sep="")
cat("  leagues <- d[d$is_league == TRUE & !is.na(d$is_league), ]\n")
cat("  View(leagues)\n")





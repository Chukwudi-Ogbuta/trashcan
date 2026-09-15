# ============================================================
# M.O.A.B BetKing Auto-Bettor (Hybrid: API + Selenium)
# API resolves fixtures + market fingerprints.
# Selenium clicks odds cells, generates booking code, validates slip.
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readxl)
  library(httr)
  library(jsonlite)
  library(stringdist)
  library(rvest)
})

# ════════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════════
BASE_PATH         <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
MAP_PATH          <- file.path(BASE_PATH, "sporty_mapper.rds")
LOG_DIR           <- file.path(BASE_PATH, "auto_bettor_logs")
SLIP_BACKUP_DIR   <- file.path(BASE_PATH, "slip_backups")
if (!dir.exists(LOG_DIR)) dir.create(LOG_DIR, recursive = TRUE)
if (!dir.exists(SLIP_BACKUP_DIR)) dir.create(SLIP_BACKUP_DIR, recursive = TRUE)

CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"

BETKING_BASE     <- "https://www.betking.com"
BETKING_API      <- "https://sportsapicdn-desktop.betking.com/api/feeds/prematch"
BETKING_LEAGUE   <- paste0(BETKING_API, "/en/4/")
BETKING_EVENT    <- paste0(BETKING_API, "/lite/event/ungrouped/en/4/")

EMAIL_FROM       <- "chukwudiogbuta@gmail.com"
EMAIL_TO         <- "chukwudiogbuta@gmail.com"
EMAIL_APP_PASS   <- "jkhryasdthhxiuts"
EMAIL_SMTP_HOST  <- "smtp.gmail.com"
EMAIL_SMTP_PORT  <- 587
BK_MAX_CUM_ODDS  <- 440000   # safely below observed ~445k crash point

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

batch_sizes <- function(n, max_per_slip = 50, min_remainder = 15) {
  if (n <= max_per_slip) return(n)
  full_slips <- n %/% max_per_slip
  remainder  <- n %% max_per_slip
  sizes <- rep(max_per_slip, full_slips)
  if (remainder >= min_remainder) sizes <- c(sizes, remainder)
  sizes
}

# ════════════════════════════════════════════════════════════
# MARKET FINGERPRINTS (for API-side lookup + DOM matching)
# For each market, we know:
# - Where it lives in the API response (fixture vs event)
# - The market panel name text on the BetKing DOM (for click)
# - The outcome cell text for the click
# ════════════════════════════════════════════════════════════
BK_MARKET_FINGERPRINT <- list(
  o15    = list(source = "event",   panel = "Total Goals 1.5",               outcome = "Over 1.5"),
  o25    = list(source = "event",   panel = "Total Goals 2.5",               outcome = "Over 2.5"),
  btts   = list(source = "event",   panel = "GG/NG",                         outcome = "GG"),
  dc_1x  = list(source = "fixture", panel = "Double Chance",                 outcome = "1X"),
  fh_o05 = list(source = "event",   panel = "1st Half - Total Goals 0.5",    outcome = "Over 0.5"),
  sh_o05 = list(source = "event",   panel = "2nd Half - Total Goals 0.5",    outcome = "Over 0.5"),
  drw_10 = list(source = "event",   panel = "10 Minutes - 1X2 From 1 To 10", outcome = "X"),
  drw_15 = list(source = "event",   panel = "15 Minutes - 1X2 From 1 To 15", outcome = "X")
)

BK_MARKET_DISPLAY_TO_KEY <- list(
  "Over 1.5"                 = "o15",
  "Over 2.5"                 = "o25",
  "BTTS"                     = "btts",
  "Double Chance 1X"         = "dc_1x",
  "1H Over 0.5"              = "fh_o05",
  "2H Over 0.5"              = "sh_o05",
  "Draw up to min 10"        = "drw_10",
  "Draw up to min 15"        = "drw_15"
)

# ════════════════════════════════════════════════════════════
# API — fixtures + team match
# ════════════════════════════════════════════════════════════
bk_headers <- function() {
  add_headers(
    `accept`          = "application/json, text/plain, */*",
    `accept-language` = "en-US,en;q=0.9",
    `origin`          = BETKING_BASE,
    `referer`         = paste0(BETKING_BASE, "/"),
    `user-agent`      = "Mozilla/5.0"
  )
}

fetch_league_fixtures <- function(league_id) {
  url <- paste0(BETKING_LEAGUE, league_id, "/0/0")
  resp <- tryCatch(GET(url, bk_headers(), timeout(30)), error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  tryCatch(fromJSON(content(resp, "text", encoding = "UTF-8"),
                    simplifyVector = FALSE), error = function(e) NULL)
}

extract_fixtures <- function(league_payload) {
  if (is.null(league_payload) || is.null(league_payload$AreaMatches)) return(list())
  if (length(league_payload$AreaMatches) == 0) return(list())
  am <- league_payload$AreaMatches[[1]]
  am$Items %||% list()
}

bk_team_match <- function(pred_home, pred_away, fixtures) {
  if (length(fixtures) == 0) return(NULL)
  ph <- tolower(trimws(pred_home))
  pa <- tolower(trimws(pred_away))
  best <- NULL; best_score <- Inf
  for (fx in fixtures) {
    if (is.null(fx$Teams) || length(fx$Teams) < 2) next
    fh <- tolower(trimws(fx$Teams[[1]]$Name %||% ""))
    fa <- tolower(trimws(fx$Teams[[2]]$Name %||% ""))
    d_h <- stringdist(ph, fh, method = "jw", p = 0.1)
    d_a <- stringdist(pa, fa, method = "jw", p = 0.1)
    if (max(d_h, d_a) > 0.35) next
    if (min(d_h, d_a) > 0.15) next
    tot <- d_h + d_a
    if (tot < best_score) { best_score <- tot; best <- fx }
  }
  best
}

# ════════════════════════════════════════════════════════════
# SELENIUM
# ════════════════════════════════════════════════════════════
SEL <- new.env(parent = emptyenv())

kill_chromedriver <- function() {
  system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
         ignore.stderr = TRUE, wait = TRUE)
  Sys.sleep(0.5)
}

start_browser <- function() {
  kill_chromedriver()
  port <- 9998
  system2(CHROMEDRIVER_PATH, args = paste0("--port=", port), wait = FALSE)
  Sys.sleep(2)
  resp <- POST(paste0("http://localhost:", port, "/session"),
               body = list(capabilities = list(alwaysMatch = list(
                 browserName = "chrome",
                 `goog:chromeOptions` = list(
                   binary = CHROME_PATH,
                   args = list("--no-sandbox", "--disable-dev-shm-usage",
                               "--disable-blink-features=AutomationControlled",
                               "--window-size=1400,900"))))),
               encode = "json", timeout(60))
  if (status_code(resp) >= 400) stop("ChromeDriver failed to start")
  body <- content(resp, "parsed", "application/json")
  SEL$port <- port; SEL$session <- body$value$sessionId
  SEL$base <- paste0("http://localhost:", port, "/session/", SEL$session)
  invisible(TRUE)
}

stop_browser <- function() {
  tryCatch({ if (!is.null(SEL$base)) DELETE(SEL$base, timeout(10)) },
           error = function(e) NULL)
  kill_chromedriver()
}

selenium_get <- function(url) {
  POST(paste0(SEL$base, "/url"), body = list(url = url),
       encode = "json", timeout(60)); Sys.sleep(2)
}

selenium_js <- function(script, args = list()) {
  args_body <- if (length(args) == 0) list() else unname(args)
  body_json <- jsonlite::toJSON(list(script = script, args = args_body),
                                auto_unbox = TRUE, null = "null")
  resp <- POST(paste0(SEL$base, "/execute/sync"),
               body = body_json, encode = "raw",
               content_type_json(), timeout(30))
  content(resp, "parsed", "application/json")$value
}

# Slugify BetKing URL parts (lowercase, hyphenated)
bk_slug <- function(s) {
  s <- tolower(as.character(s %||% ""))
  s <- gsub("\\.", "", s)
  s <- gsub("[^a-z0-9]+", "-", s)
  gsub("^-+|-+$", "", s)
}

# Build league URL: /sports/s/event/p/football/{country-slug}/{league-slug}/0/0
build_league_url <- function(country_name, league_name) {
  paste0(BETKING_BASE, "/sports/s/event/p/football/",
         bk_slug(country_name), "/", bk_slug(league_name), "/0/0")
}

# Click the odds cell for a given market panel + outcome text
# Handles both fixture-row cells (default markets) and expanded cells
# (via more-odds click). Returns TRUE if odds cell clicked.
bk_click_pick <- function(match_id, panel_text, outcome_text, source, log_fn = function(x) NULL) {
  if (source == "fixture") {
    result <- selenium_js(paste0(
      "var mid = arguments[0]; var ot = arguments[1];",
      "var headers = document.querySelectorAll('th.headers[title]');",
      "var colIdx = -1;",
      "for (var i = 0; i < headers.length; i++) {",
      "  if (headers[i].getAttribute('title') === ot) { colIdx = i; break; }",
      "}",
      "if (colIdx < 0) return JSON.stringify({status:'header_not_found'});",
      "var row = document.getElementById('match_' + mid);",
      "if (!row) return JSON.stringify({status:'row_not_found'});",
      "var cells = row.querySelectorAll('td.oddItem');",
      "if (colIdx >= cells.length) return JSON.stringify({status:'col_out_of_range'});",
      "var cell = cells[colIdx];",
      "var a = cell.querySelector('a.clickable');",
      "if (!a) return JSON.stringify({status:'no_link'});",
      "var oddTxt = (a.textContent || '').replace(/[^0-9.]/g,'');",
      "var odd = parseFloat(oddTxt);",
      "return JSON.stringify({status:'found', odd: odd, elId: null});"),
      list(as.character(match_id), outcome_text))
    parsed <- tryCatch(jsonlite::fromJSON(result %||% '{"status":"parse_err"}'),
                       error = function(e) list(status = "parse_err"))
    if (!identical(parsed$status, "found")) {
      return(list(ok = FALSE, reason = paste0("dom: ", parsed$status), odd = NA_real_))
    }
    odd <- suppressWarnings(as.numeric(parsed$odd))
    if (is.na(odd) || odd <= 1) {
      return(list(ok = FALSE, reason = "dom: bad_odd", odd = NA_real_))
    }
    # caller decides whether to click; we return the odd only
    return(list(ok = NA, reason = "peek", odd = odd, source = "fixture",
                match_id = match_id, outcome_text = outcome_text))
  }
  
  # source == "event": expand, then scroll-and-poll to LOCATE (not click yet)
  expanded <- selenium_js(paste0(
    "var mid = arguments[0];",
    "var cells = document.querySelectorAll('td.moreOdds[data-eventid=\"' + mid + '\"]');",
    "if (cells.length === 0) return 'no_expand_cell';",
    "cells[0].click(); return 'expanded';"),
    list(as.character(match_id)))
  if (!identical(expanded, "expanded")) {
    return(list(ok = FALSE, reason = paste0("expand: ", expanded), odd = NA_real_))
  }
  Sys.sleep(2.5)
  
  MAX_SCROLLS <- 30
  last_scroll_top <- -1L
  for (attempt in seq_len(MAX_SCROLLS + 1)) {
    peek <- selenium_js(paste0(
      "var pt = arguments[0]; var ot = arguments[1];",
      "var containers = document.querySelectorAll('div.event-container.opened');",
      "for (var i = 0; i < containers.length; i++) {",
      "  var hdr = containers[i].querySelector('.subHeader .headerText span');",
      "  if (!hdr) continue;",
      "  if (hdr.textContent.trim() !== pt) continue;",
      "  var content = containers[i].querySelector('.content');",
      "  if (!content) continue;",
      "  var items = content.querySelectorAll('.inner-content');",
      "  for (var j = 0; j < items.length; j++) {",
      "    var lbl = items[j].querySelector('span');",
      "    if (!lbl) continue;",
      "    if (lbl.textContent.trim() !== ot) continue;",
      "    var btn = items[j].querySelector('.match-odd');",
      "    if (!btn) continue;",
      "    var oddEl = btn.querySelector('.oddBorder');",
      "    var oddTxt = oddEl ? (oddEl.textContent || '').replace(/[^0-9.]/g,'') : '';",
      "    var odd = parseFloat(oddTxt);",
      "    if (!btn.id) { btn.setAttribute('data-bkclick', '1'); }",
      "    return JSON.stringify({status:'found', odd: odd, elId: btn.id || null, marker: btn.id ? null : '1'});",
      "  }",
      "}",
      "return JSON.stringify({status:'not_found'});"),
      list(panel_text, outcome_text))
    parsed <- tryCatch(jsonlite::fromJSON(peek %||% '{"status":"parse_err"}'),
                       error = function(e) list(status = "parse_err"))
    if (identical(parsed$status, "found")) {
      odd <- suppressWarnings(as.numeric(parsed$odd))
      if (is.na(odd) || odd <= 1) {
        return(list(ok = FALSE, reason = "dom: bad_odd", odd = NA_real_))
      }
      return(list(ok = NA, reason = "peek", odd = odd, source = "event",
                  el_id = parsed$elId, marker = parsed$marker %||% NULL))
    }
    
    scroll_state <- selenium_js(paste0(
      "function findScrollParent(el) {",
      "  var p = el.parentElement;",
      "  while (p) {",
      "    var st = window.getComputedStyle(p);",
      "    if ((st.overflowY === 'auto' || st.overflowY === 'scroll') && p.scrollHeight > p.clientHeight) return p;",
      "    p = p.parentElement;",
      "  }",
      "  return document.scrollingElement || document.documentElement;",
      "}",
      "var any = document.querySelector('div.event-container');",
      "if (!any) return JSON.stringify({top: -1, max: -1});",
      "var sp = findScrollParent(any);",
      "var before = sp.scrollTop;",
      "sp.scrollTop = before + Math.max(300, sp.clientHeight - 100);",
      "return JSON.stringify({top: sp.scrollTop, max: sp.scrollHeight - sp.clientHeight});"))
    st <- tryCatch(jsonlite::fromJSON(scroll_state %||% '{"top":-1,"max":-1}'),
                   error = function(e) list(top = -1, max = -1))
    Sys.sleep(0.4)
    if (is.null(st$top) || st$top < 0) break
    if (st$top == last_scroll_top) break
    last_scroll_top <- st$top
  }
  list(ok = FALSE, reason = "dom: not_found (scrolled to bottom)", odd = NA_real_)
}

# ════════════════════════════════════════════════════════════
# NEW helper: commit a peeked pick (click it)
# ════════════════════════════════════════════════════════════
bk_commit_pick <- function(peek) {
  if (identical(peek$source, "fixture")) {
    result <- selenium_js(paste0(
      "var mid = arguments[0]; var ot = arguments[1];",
      "var headers = document.querySelectorAll('th.headers[title]');",
      "var colIdx = -1;",
      "for (var i = 0; i < headers.length; i++) {",
      "  if (headers[i].getAttribute('title') === ot) { colIdx = i; break; }",
      "}",
      "if (colIdx < 0) return 'header_gone';",
      "var row = document.getElementById('match_' + mid);",
      "if (!row) return 'row_gone';",
      "var cells = row.querySelectorAll('td.oddItem');",
      "if (colIdx >= cells.length) return 'col_gone';",
      "var a = cells[colIdx].querySelector('a.clickable');",
      "if (!a) return 'no_link';",
      "a.click(); return 'clicked';"),
      list(as.character(peek$match_id), peek$outcome_text))
    return(identical(result, "clicked"))
  }
  # event: click by id or by data-bkclick marker
  sel <- if (!is.null(peek$el_id) && nchar(peek$el_id) > 0) {
    paste0("document.getElementById('", peek$el_id, "')")
  } else {
    "document.querySelector('.match-odd[data-bkclick=\"1\"]')"
  }
  result <- selenium_js(paste0(
    "var el = ", sel, ";",
    "if (!el) return 'gone';",
    "el.removeAttribute('data-bkclick');",
    "el.click(); return 'clicked';"))
  identical(result, "clicked")
}

# ════════════════════════════════════════════════════════════
# NEW helper: read current cumulative odds from slip
# ════════════════════════════════════════════════════════════
bk_read_cum_odds <- function() {
  txt <- selenium_js(paste0(
    "var spans = document.querySelectorAll('.totals-container span');",
    "for (var i = 0; i < spans.length; i++) {",
    "  if (spans[i].textContent.trim() === 'Total Odds' && spans[i+1]) {",
    "    return spans[i+1].textContent.trim();",
    "  }",
    "}",
    "return '';"))
  n <- suppressWarnings(as.numeric(gsub("[^0-9.]", "", txt %||% "")))
  if (is.na(n)) 0 else n
}

# How many picks are on the slip right now
bk_slip_count <- function() {
  txt <- selenium_js(paste0(
    "var el = document.querySelector('.selections-counter');",
    "return el ? el.textContent.trim() : '0';"))
  n <- suppressWarnings(as.integer(gsub("[^0-9]", "", txt %||% "0")))
  if (is.na(n)) 0 else n
}

# Clear the slip (remove all picks)
bk_clear_slip <- function() {
  selenium_js(paste0(
    "var b = document.querySelector('.removeAll, .clearCoupon, button.clearAll');",
    "if (b) b.click();"))
  Sys.sleep(0.8)
  selenium_js(paste0(
    "var b = document.querySelector('.confirm-yes, .btn-danger, .yesButton');",
    "if (b) b.click();"))
  Sys.sleep(0.5)
}

# Click Book Bet button and read the returned code
bk_generate_code <- function() {
  clicked <- selenium_js(paste0(
    "var b = document.querySelector('button.bookBet');",
    "if (b && !b.disabled) { b.click(); return true; } return false;"))
  if (!isTRUE(clicked)) return(NA_character_)
  Sys.sleep(8)
  code <- selenium_js(paste0(
    "var el = document.querySelector('#couponBookedCode span:last-child');",
    "return el ? el.textContent.trim() : '';"))
  if (is.null(code) || nchar(code) == 0) return(NA_character_)
  code
}

# Read the slip confirmation table for validation
bk_read_slip_events <- function() {
  data <- selenium_js(paste0(
    "var rows = document.querySelectorAll('#couponBookedEvents tr.eventsContent');",
    "var out = [];",
    "for (var i = 0; i < rows.length; i++) {",
    "  var cells = rows[i].querySelectorAll('td');",
    "  if (cells.length < 5) continue;",
    "  out.push({",
    "    smart_code: cells[0].textContent.trim(),",
    "    match_name: cells[1].textContent.trim(),",
    "    date: cells[2].textContent.trim(),",
    "    market: cells[4].querySelectorAll('span')[0] ? cells[4].querySelectorAll('span')[0].textContent.replace(':','').trim() : '',",
    "    selection: cells[4].querySelectorAll('span')[1] ? cells[4].querySelectorAll('span')[1].textContent.trim() : ''",
    "  });",
    "}",
    "return JSON.stringify(out);"))
  if (is.null(data) || nchar(data) == 0) return(data.frame())
  parsed <- tryCatch(fromJSON(data), error = function(e) NULL)
  if (is.null(parsed) || length(parsed) == 0) return(data.frame())
  as.data.frame(parsed, stringsAsFactors = FALSE)
}

# Validate slip: compare booked selections against intended
bk_validate_slip <- function(code, intended_items, log_fn) {
  slip <- bk_read_slip_events()
  if (nrow(slip) == 0) {
    log_fn("  slip validation: no events read")
    return(data.frame())
  }
  mismatches <- list()
  for (it in intended_items) {
    fp <- BK_MARKET_FINGERPRINT[[it$market_internal]]
    if (is.null(fp)) next
    # Match by team names (fuzzy)
    match_row <- NULL
    for (i in seq_len(nrow(slip))) {
      mn <- slip$match_name[i]
      parts <- strsplit(mn, " - ", fixed = TRUE)[[1]]
      if (length(parts) < 2) next
      d_h <- stringdist(tolower(it$home), tolower(trimws(parts[1])),
                        method = "jw", p = 0.1)
      d_a <- stringdist(tolower(it$away), tolower(trimws(parts[2])),
                        method = "jw", p = 0.1)
      if (d_h < 0.2 && d_a < 0.2) { match_row <- slip[i, ]; break }
    }
    if (is.null(match_row)) next
    # Check selection matches outcome text
    if (!identical(as.character(match_row$selection), fp$outcome) &&
        !grepl(fp$outcome, match_row$selection, fixed = TRUE)) {
      mismatches[[length(mismatches) + 1]] <- data.frame(
        code = code, home = it$home, away = it$away,
        intended = it$market_internal,
        issue = paste0("expected '", fp$outcome, "' got '",
                       match_row$selection, "' on market '",
                       match_row$market, "'"),
        stringsAsFactors = FALSE)
    }
  }
  if (length(mismatches) == 0) {
    log_fn(paste0("  \u2713 all ", nrow(slip), " picks validated for ", code))
    return(data.frame())
  }
  df <- do.call(rbind, mismatches)
  log_fn(paste0("  \u26A0 ", nrow(df), " MISMATCHES for ", code))
  for (i in seq_len(nrow(df))) {
    log_fn(paste0("    - ", df$home[i], " v ", df$away[i], ": ", df$issue[i]))
  }
  df
}

# Save slip modal screenshot (basic — full modal screenshot)
bk_save_slip_image <- function(code, log_fn) {
  b64 <- selenium_js(paste0(
    "var el = document.querySelector('#bookedCoupon');",
    "if (!el) return null;",
    "return html2canvas ? 'html2canvas_available' : 'no_screenshot_lib';"))
  # Fallback: use the /screenshot endpoint at page level
  resp <- tryCatch(GET(paste0(SEL$base, "/screenshot"), timeout(10)),
                   error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) {
    log_fn("  slip screenshot failed"); return(NA)
  }
  parsed <- content(resp, "parsed", "application/json")
  b64_data <- parsed$value %||% ""
  if (nchar(b64_data) == 0) {
    log_fn("  slip screenshot empty"); return(NA)
  }
  ts <- format(Sys.time(), "%Y-%m-%d_%H%M%S")
  path <- file.path(SLIP_BACKUP_DIR, paste0(ts, "_", code, "_BK.png"))
  writeBin(jsonlite::base64_dec(b64_data), path)
  log_fn(paste0("  slip image saved: ", basename(path)))
  path
}

# Close the booking modal so we can start a new batch
bk_close_modal <- function() {
  selenium_js(paste0(
    "var b = document.querySelector('.ngdialog-close, button.close, ",
    "#bookedCoupon .close-icon, .booked-close-button');",
    "if (b) b.click();"))
  Sys.sleep(0.8)
}

# ════════════════════════════════════════════════════════════
# CORE FLOW
# ════════════════════════════════════════════════════════════
process_market_bk <- function(preds_df, market_key, market_internal, log_fn) {
  log_fn(paste0("\n=== ", market_key, " ==="))
  normalize_market <- function(m) {
    switch(as.character(m),
           "1hover05"   = "1H Over 0.5",
           "2hover05"   = "2H Over 0.5",
           "dc12"       = "Double Chance 12",
           "dc1x"       = "Double Chance 1X",
           "dcx2"       = "Double Chance X2",
           "drawupto10" = "Draw up to min 10",
           "drawupto15" = "Draw up to min 15",
           "over_15_ft" = "Over 1.5",
           as.character(m))
  }
  preds_df$market <- vapply(preds_df$market, normalize_market, character(1))
  sub <- preds_df[preds_df$market == market_key, , drop = FALSE]
  if (nrow(sub) == 0) {
    log_fn("  no picks for this market")
    return(list(codes = c(), failed = data.frame()))
  }
  if ("confidence" %in% names(sub))
    sub <- sub[order(-as.numeric(sub$confidence)), , drop = FALSE]
  sub$lg_key <- paste0(sub$betking_country_name, "::", sub$betking_league_name, "::",
                       sub$betking_league_id)
  groups <- split(sub, sub$lg_key)
  
  fp <- BK_MARKET_FINGERPRINT[[market_internal]]
  bookable <- list(); failed <- data.frame()
  
  for (gkey in names(groups)) {
    g <- groups[[gkey]]
    ct  <- as.character(g$betking_country_name[1])
    lg  <- as.character(g$betking_league_name[1])
    lgi <- as.integer(g$betking_league_id[1])
    log_fn(paste0("\n[", ct, " / ", lg, "] ", nrow(g), " picks"))
    league_payload <- fetch_league_fixtures(lgi)
    if (is.null(league_payload)) {
      failed <- rbind(failed, cbind(g, reason = "league fetch failed")); next
    }
    fixtures <- extract_fixtures(league_payload)
    if (length(fixtures) == 0) {
      failed <- rbind(failed, cbind(g, reason = "no fixtures")); next
    }
    for (i in seq_len(nrow(g))) {
      p <- g[i, ]
      fx <- bk_team_match(p$home, p$away, fixtures)
      if (is.null(fx)) {
        failed <- rbind(failed, cbind(p, reason = "team match failed")); next
      }
      bookable[[paste0(gkey, "::", i)]] <- list(
        pick = p, match_id = as.integer(fx$ItemID),
        bk_country = ct, bk_league = lg,
        market_internal = market_internal,
        home = p$home, away = p$away)
    }
  }
  
  log_fn(paste0("\nBookable: ", length(bookable), " | Failed: ", nrow(failed)))
  if (length(bookable) == 0) return(list(codes = c(), failed = failed))
  
  # Group bookable items by league — one page load per league — then batch overall
  codes <- c(); batch_no <- 0
  keys_remaining <- names(bookable)
  
  while (length(keys_remaining) > 0) {
    batch_no <- batch_no + 1
    log_fn(paste0("\n--- Cocktail batch ", batch_no, " (", length(keys_remaining), " picks remaining) ---"))
    bk_clear_slip(); Sys.sleep(1)
    cum_odds <- 1
    batch_items <- list()
    added <- 0
    
    # Group remaining by league for page-load efficiency
    by_league <- split(bookable[keys_remaining],
                       sapply(bookable[keys_remaining],
                              function(x) paste0(x$bk_country, "::", x$bk_league)))
    
    batch_full <- FALSE
    for (lg_key in names(by_league)) {
      if (batch_full) break
      items <- by_league[[lg_key]]
      lg_url <- build_league_url(items[[1]]$bk_country, items[[1]]$bk_league)
      selenium_get(lg_url); Sys.sleep(3)
      for (k in names(items)) {
        it <- items[[k]]
        peek <- bk_click_pick(it$match_id, fp$panel, fp$outcome, fp$source, log_fn)
        if (!identical(peek$reason, "peek")) {
          log_fn(paste0("  ! skipped [", it$market_internal, "] (", peek$reason,
                        "): ", it$home, " v ", it$away))
          keys_remaining <- setdiff(keys_remaining, k)
          Sys.sleep(0.5); next
        }
        projected <- cum_odds * peek$odd
        if (added >= 50 || projected > BK_MAX_CUM_ODDS) {
          log_fn(paste0("  # batch full (cum=", round(cum_odds,2),
                        ", next=", peek$odd, ", projected=", round(projected,2),
                        ") — deferring: ", it$home, " v ", it$away))
          batch_full <- TRUE; break
        }
        if (!bk_commit_pick(peek)) {
          log_fn(paste0("  ! click failed [", it$market_internal, "]: ",
                        it$home, " v ", it$away))
          keys_remaining <- setdiff(keys_remaining, k)
          Sys.sleep(0.5); next
        }
        cum_odds <- projected
        added <- added + 1
        batch_items[[k]] <- it
        keys_remaining <- setdiff(keys_remaining, k)
        log_fn(paste0("  + [", it$market_internal, "] ", it$home, " v ", it$away,
                      " @", peek$odd, " (cum=", round(cum_odds,2), ")"))
        Sys.sleep(0.5)
      }
    }
    
    Sys.sleep(1)
    actual <- bk_slip_count()
    log_fn(paste0("  slip size: ", actual, " (expected ", added,
                  ", cum odds ~", round(cum_odds,2), ")"))
    if (actual < 1) { log_fn("  slip empty — skipping booking"); next }
    code <- bk_generate_code()
    if (is.na(code)) { log_fn("  booking code not retrieved"); next }
    log_fn(paste0("  booked: ", code))
    codes <- c(codes, code)
    Sys.sleep(1)
    bk_save_slip_image(code, log_fn)
    intended <- lapply(batch_items, function(x) {
      list(market_internal = x$market_internal, home = x$home, away = x$away)
    })
    bk_validate_slip(code, intended, log_fn)
    bk_close_modal(); Sys.sleep(1)
  }
  list(codes = codes, failed = failed)
}

process_cocktail_bk <- function(preds_df, log_fn) {
  log_fn("\n=== COCKTAIL ===")
  normalize_market <- function(m) {
    switch(as.character(m),
           "1hover05"   = "1H Over 0.5",
           "2hover05"   = "2H Over 0.5",
           "dc12"       = "Double Chance 12",
           "dc1x"       = "Double Chance 1X",
           "dcx2"       = "Double Chance X2",
           "drawupto10" = "Draw up to min 10",
           "drawupto15" = "Draw up to min 15",
           "over_15_ft" = "Over 1.5",
           as.character(m))
  }
  preds_df$market <- vapply(preds_df$market, normalize_market, character(1))
  preds_df$mkt_internal <- vapply(as.character(preds_df$market), function(m) {
    k <- BK_MARKET_DISPLAY_TO_KEY[[m]]; if (is.null(k)) NA_character_ else k
  }, character(1))
  drop <- is.na(preds_df$mkt_internal)
  if (any(drop)) {
    log_fn(paste0("  dropping ", sum(drop), " picks not bookable on BetKing"))
  }
  sub <- preds_df[!drop, , drop = FALSE]
  if (nrow(sub) == 0) {
    log_fn("  no bookable picks"); return(list(codes = c(), failed = data.frame()))
  }
  if ("confidence" %in% names(sub))
    sub <- sub[order(-as.numeric(sub$confidence)), , drop = FALSE]
  sub$lg_key <- paste0(sub$betking_country_name, "::", sub$betking_league_name, "::",
                       sub$betking_league_id)
  groups <- split(sub, sub$lg_key)
  
  bookable <- list(); failed <- data.frame()
  for (gkey in names(groups)) {
    g <- groups[[gkey]]
    ct  <- as.character(g$betking_country_name[1])
    lg  <- as.character(g$betking_league_name[1])
    lgi <- as.integer(g$betking_league_id[1])
    log_fn(paste0("\n[", ct, " / ", lg, "] ", nrow(g), " picks"))
    league_payload <- fetch_league_fixtures(lgi)
    if (is.null(league_payload)) {
      failed <- rbind(failed, cbind(g, reason = "league fetch failed")); next
    }
    fixtures <- extract_fixtures(league_payload)
    if (length(fixtures) == 0) {
      failed <- rbind(failed, cbind(g, reason = "no fixtures")); next
    }
    for (i in seq_len(nrow(g))) {
      p <- g[i, ]
      fx <- bk_team_match(p$home, p$away, fixtures)
      if (is.null(fx)) {
        failed <- rbind(failed, cbind(p, reason = "team match failed")); next
      }
      bookable[[paste0(gkey, "::", i)]] <- list(
        pick = p, match_id = as.integer(fx$ItemID),
        bk_country = ct, bk_league = lg,
        market_internal = p$mkt_internal,
        home = p$home, away = p$away)
    }
  }
  
  log_fn(paste0("\nBookable: ", length(bookable), " | Failed: ", nrow(failed)))
  if (length(bookable) == 0) return(list(codes = c(), failed = failed))
  
  codes <- c(); batch_no <- 0
  keys_remaining <- names(bookable)
  
  while (length(keys_remaining) > 0) {
    batch_no <- batch_no + 1
    log_fn(paste0("\n--- Cocktail batch ", batch_no, " (", length(keys_remaining), " picks remaining) ---"))
    bk_clear_slip(); Sys.sleep(1)
    cum_odds <- 1
    batch_items <- list()
    added <- 0
    
    # Group remaining by league for page-load efficiency
    by_league <- split(bookable[keys_remaining],
                       sapply(bookable[keys_remaining],
                              function(x) paste0(x$bk_country, "::", x$bk_league)))
    
    batch_full <- FALSE
    for (lg_key in names(by_league)) {
      if (batch_full) break
      items <- by_league[[lg_key]]
      lg_url <- build_league_url(items[[1]]$bk_country, items[[1]]$bk_league)
      selenium_get(lg_url); Sys.sleep(3)
      for (k in names(items)) {
        it <- items[[k]]
        fp <- BK_MARKET_FINGERPRINT[[it$market_internal]]
        if (is.null(fp)) {
          log_fn(paste0("  ! no fingerprint for ", it$market_internal))
          keys_remaining <- setdiff(keys_remaining, k); next
        }
        peek <- bk_click_pick(it$match_id, fp$panel, fp$outcome, fp$source, log_fn)
        if (!identical(peek$reason, "peek")) {
          log_fn(paste0("  ! skipped [", it$market_internal, "] (", peek$reason,
                        "): ", it$home, " v ", it$away))
          keys_remaining <- setdiff(keys_remaining, k)
          Sys.sleep(0.5); next
        }
        projected <- cum_odds * peek$odd
        if (added >= 50 || projected > BK_MAX_CUM_ODDS) {
          log_fn(paste0("  # batch full (cum=", round(cum_odds,2),
                        ", next=", peek$odd, ", projected=", round(projected,2),
                        ") — deferring: ", it$home, " v ", it$away))
          batch_full <- TRUE; break
        }
        if (!bk_commit_pick(peek)) {
          log_fn(paste0("  ! click failed [", it$market_internal, "]: ",
                        it$home, " v ", it$away))
          keys_remaining <- setdiff(keys_remaining, k)
          Sys.sleep(0.5); next
        }
        cum_odds <- projected
        added <- added + 1
        batch_items[[k]] <- it
        keys_remaining <- setdiff(keys_remaining, k)
        log_fn(paste0("  + [", it$market_internal, "] ", it$home, " v ", it$away,
                      " @", peek$odd, " (cum=", round(cum_odds,2), ")"))
        Sys.sleep(0.5)
      }
    }
    
    Sys.sleep(1)
    actual <- bk_slip_count()
    log_fn(paste0("  slip size: ", actual, " (expected ", added,
                  ", cum odds ~", round(cum_odds,2), ")"))
    if (actual < 1) { log_fn("  slip empty — skipping booking"); next }
    code <- bk_generate_code()
    if (is.na(code)) { log_fn("  booking code not retrieved"); next }
    log_fn(paste0("  booked: ", code))
    codes <- c(codes, code)
    Sys.sleep(1)
    bk_save_slip_image(code, log_fn)
    intended <- lapply(batch_items, function(x) {
      list(market_internal = x$market_internal, home = x$home, away = x$away)
    })
    bk_validate_slip(code, intended, log_fn)
    bk_close_modal(); Sys.sleep(1)
  }
  list(codes = codes, failed = failed)
}

# ════════════════════════════════════════════════════════════
# EMAIL (same as before, red theme)
# ════════════════════════════════════════════════════════════
build_bk_email_html <- function(market_codes) {
  total_codes <- sum(sapply(market_codes, length))
  rows <- ""
  for (mkt in names(market_codes)) {
    codes <- market_codes[[mkt]]
    if (length(codes) == 0) next
    code_blocks <- paste0(vapply(codes, function(c) {
      paste0('<div style="margin:10px 0; padding:18px 22px; ',
             'background:linear-gradient(135deg,#0F1115 0%,#1A1D24 100%); ',
             'border:2px dashed #DC2626; border-radius:6px; text-align:center;">',
             '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:10px; ',
             'font-weight:600; letter-spacing:3px; text-transform:uppercase; ',
             'color:#7C8290; margin-bottom:6px;">BetKing Booking Code</div>',
             '<div style="font-family:Consolas,monospace; font-size:26px; ',
             'font-weight:900; letter-spacing:5px; color:#DC2626;">', c,
             '</div></div>')
    }, character(1)), collapse = "")
    rows <- paste0(rows, '<div style="margin:28px 0;">',
                   '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:13px; ',
                   'font-weight:700; letter-spacing:2px; text-transform:uppercase; ',
                   'color:#EAEAEA; margin-bottom:12px;">', mkt, ' \u00b7 ',
                   length(codes), ' code', if (length(codes) == 1) '' else 's',
                   '</div>', code_blocks, '</div>')
  }
  paste0('<!doctype html><html><body style="margin:0;padding:0;background:#0A0B0E;',
         'font-family:\'Segoe UI\',Arial,sans-serif;">',
         '<table cellpadding="0" cellspacing="0" width="100%" ',
         'style="background:#0A0B0E;padding:32px 16px;"><tr><td align="center">',
         '<table cellpadding="0" cellspacing="0" width="640" ',
         'style="background:#15181E;border-radius:10px;overflow:hidden;',
         'border:1px solid #2A2E37;">',
         '<tr><td style="background:linear-gradient(135deg,#0F1115 0%,#7F1D1D 50%,#DC2626 100%);',
         'padding:32px 36px;">',
         '<div style="color:rgba(255,255,255,0.75);font-size:11px;font-weight:700;',
         'letter-spacing:4px;text-transform:uppercase;margin-bottom:6px;">',
         'M.O.A.B \u00b7 BetKing Auto-Bettor</div>',
         '<div style="font-family:Georgia,serif;color:#FFFFFF;font-size:32px;',
         'font-weight:900;line-height:1.1;">Codes ',
         '<span style="color:#FCA5A5;font-style:italic;">Ready</span></div>',
         '<div style="color:rgba(255,255,255,0.7);font-size:13px;margin-top:10px;">',
         format(Sys.time(), "%A, %d %B %Y &middot; %H:%M"), '</div>',
         '<div style="margin-top:14px;background:rgba(220,38,38,0.15);',
         'border:1px solid #DC2626;padding:6px 14px;border-radius:14px;',
         'font-family:Consolas,monospace;font-size:11px;color:#FCA5A5;',
         'font-weight:700;display:inline-block;">', total_codes,
         ' TOTAL CODES</div></td></tr>',
         '<tr><td style="padding:28px 36px;">', rows, '</td></tr>',
         '<tr><td style="background:#0F1115;padding:18px 36px;border-top:1px solid #2A2E37;',
         'font-size:10px;letter-spacing:3px;text-transform:uppercase;color:#7C8290;',
         'text-align:center;">Load each code on BetKing to view and stake</td></tr>',
         '</table></td></tr></table></body></html>')
}

send_bk_email <- function(market_codes, log_fn) {
  if (length(market_codes) == 0 || all(sapply(market_codes, length) == 0)) {
    log_fn("No codes to email"); return(FALSE)
  }
  if (!requireNamespace("emayili", quietly = TRUE)) {
    log_fn("emayili package not installed"); return(FALSE)
  }
  msg <- emayili::envelope()
  msg <- emayili::from(msg, EMAIL_FROM)
  msg <- emayili::to(msg, EMAIL_TO)
  msg <- emayili::subject(msg, paste0("BetKing codes \u00b7 ",
                                      format(Sys.time(), "%d %b %H:%M")))
  msg <- emayili::html(msg, build_bk_email_html(market_codes))
  smtp <- emayili::server(host = EMAIL_SMTP_HOST, port = EMAIL_SMTP_PORT,
                          username = EMAIL_FROM, password = EMAIL_APP_PASS)
  tryCatch({ smtp(msg, verbose = FALSE); TRUE },
           error = function(e) {
             log_fn(paste0("Email error: ", e$message)); FALSE })
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════
THEME_CSS <- "
  :root { --ivory: #FAF7F2; --ivory-2: #F1ECE3; --red: #DC2626;
          --red-dark: #991B1B; --border: #E5E0D5; --good: #2E7D5B; }
  body { background: var(--ivory-2) !important; color: #1A1A1A !important;
         font-family: 'Inter', 'Segoe UI', system-ui, sans-serif; margin: 0; }
  .container-fluid { max-width: 880px; margin: 32px auto; padding: 0 16px; }
  .ab-header { padding: 0 0 24px; border-bottom: 2px solid var(--red);
               margin-bottom: 28px; }
  .ab-eyebrow { font-size: 10px; font-weight: 700; letter-spacing: 3px;
                text-transform: uppercase; color: var(--red); }
  .ab-title { font-size: 26px; font-weight: 600; color: var(--red-dark);
              margin-top: 6px; }
  .ab-card { background: var(--ivory); border: 1px solid var(--border);
             border-radius: 8px; padding: 22px 24px; margin-bottom: 16px; }
  .ab-step { display: flex; align-items: center; gap: 10px; margin-bottom: 14px; }
  .ab-step-num { width: 22px; height: 22px; border-radius: 50%;
                 background: var(--red); color: var(--ivory);
                 font-weight: 700; font-size: 12px;
                 display: flex; align-items: center; justify-content: center; }
  .ab-step-label { font-size: 11px; font-weight: 700; letter-spacing: 2px;
                   text-transform: uppercase; color: var(--red-dark); }
  .ab-btn { background: var(--red); color: var(--ivory); border: none;
            border-radius: 6px; padding: 12px 22px; font-weight: 700;
            font-size: 13px; letter-spacing: 1px; text-transform: uppercase;
            cursor: pointer; }
  .ab-btn:hover { background: var(--red-dark); }
  .ab-log { background: #FFFFFF; border: 1px solid var(--border);
            border-radius: 6px; padding: 14px 16px;
            font-family: 'JetBrains Mono', Consolas, monospace;
            font-size: 12px; color: #4A4A4A; line-height: 1.6;
            max-height: 420px; overflow-y: auto; white-space: pre-wrap; }
  .ab-status { font-size: 12px; color: #888; margin-top: 10px; }
  .ab-status-ok { color: var(--good); }
"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono&display=swap"),
    tags$style(HTML(THEME_CSS))
  ),
  div(class = "ab-header",
      div(class = "ab-eyebrow", "M.O.A.B \u00b7 BetKing"),
      div(class = "ab-title", "Auto-Bettor")),
  div(class = "ab-card",
      div(class = "ab-step", div(class = "ab-step-num", "1"),
          div(class = "ab-step-label", "Upload predictions XLSX")),
      fileInput("preds_file", NULL, accept = ".xlsx", width = "100%"),
      uiOutput("pred_status")),
  div(class = "ab-card",
      div(class = "ab-step", div(class = "ab-step-num", "2"),
          div(class = "ab-step-label", "Match against BetKing")),
      actionButton("fetch_btn", "Load mapper & match", class = "ab-btn"),
      uiOutput("match_status")),
  div(class = "ab-card",
      div(class = "ab-step", div(class = "ab-step-num", "3"),
          div(class = "ab-step-label", "Market")),
      selectInput("market_choice", NULL,
                  choices = list(
                    "Over / Under" = list("Over 1.5" = "Over 1.5", "Over 2.5" = "Over 2.5"),
                    "BTTS" = list("BTTS" = "BTTS"),
                    "Double Chance" = list("Double Chance 1X" = "Double Chance 1X"),
                    "Halves" = list("1H Over 0.5" = "1H Over 0.5",
                                    "2H Over 0.5" = "2H Over 0.5"),
                    "Timed markets" = list("Draw up to min 10" = "Draw up to min 10",
                                           "Draw up to min 15" = "Draw up to min 15"),
                    "Cocktail" = list("Cocktail (all markets in one ticket)" = "Cocktail")
                  ),
                  selected = "Over 1.5", width = "100%"),
      uiOutput("market_help")),
  div(class = "ab-card",
      div(class = "ab-step", div(class = "ab-step-num", "4"),
          div(class = "ab-step-label", "Generate codes")),
      actionButton("run_btn", "Generate codes and email", class = "ab-btn"),
      tags$br(), tags$br(),
      div(class = "ab-step-label", "Activity log"),
      tags$div(style = "height:8px;"),
      tags$pre(class = "ab-log", textOutput("log_out", inline = TRUE)))
)

server <- function(input, output, session) {
  rv <- reactiveValues(preds = NULL, matched = NULL, unmatched = NULL, log = "")
  log_msg <- function(msg) {
    rv$log <- paste0(rv$log, "[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n")
  }
  output$log_out <- renderText({ rv$log })
  
  observeEvent(input$preds_file, {
    req(input$preds_file)
    sheets <- excel_sheets(input$preds_file$datapath)
    sh <- if ("All Picks" %in% sheets) "All Picks" else sheets[1]
    df <- tryCatch(read_excel(input$preds_file$datapath, sheet = sh),
                   error = function(e) NULL)
    if (is.null(df)) { showNotification("Could not read file", type = "error"); return() }
    rv$preds <- as.data.frame(df)
    log_msg(paste0("Loaded ", nrow(df), " picks"))
  })
  
  output$pred_status <- renderUI({
    if (is.null(rv$preds)) return(div(class = "ab-status", "No file uploaded yet."))
    n_mkt <- table(rv$preds$market)
    n_cty <- length(unique(rv$preds$country))
    div(class = "ab-status ab-status-ok",
        paste0(nrow(rv$preds), " picks \u00b7 ", n_cty, " countries \u00b7 ",
               paste(paste0(names(n_mkt), " ", n_mkt), collapse = " / ")))
  })
  
  observeEvent(input$fetch_btn, {
    req(rv$preds)
    if (!file.exists(MAP_PATH)) {
      log_msg("ERROR: sporty_mapper.rds not found."); return()
    }
    log_msg("Loading mapper...")
    tryCatch({
      m <- readRDS(MAP_PATH)
      bk_ready <- m[!is.na(m$status) & m$status == "Verified" &
                      !is.na(m$betking_league_id) &
                      !is.na(m$betking_reviewed_on) &
                      !is.na(m$fs_country) & !is.na(m$fs_league), , drop = FALSE]
      if (nrow(bk_ready) == 0) {
        log_msg("No FS<->BetKing pairs verified."); return()
      }
      log_msg(paste0("Mapper: ", nrow(bk_ready), " FS<->BetKing pairs"))
      
      preds <- rv$preds
      preds$country <- as.character(preds$country)
      preds$league  <- as.character(preds$league)
      key_pred <- paste0(tolower(preds$country), "||", tolower(preds$league))
      key_map  <- paste0(tolower(bk_ready$fs_country), "||",
                         tolower(bk_ready$fs_league))
      idx <- match(key_pred, key_map)
      matched_df <- preds[!is.na(idx), , drop = FALSE]
      unmatched_df <- preds[is.na(idx), , drop = FALSE]
      if (nrow(matched_df) > 0) {
        matched_idx <- idx[!is.na(idx)]
        matched_df$betking_league_id   <- bk_ready$betking_league_id[matched_idx]
        matched_df$betking_country_name <- bk_ready$betking_country_name[matched_idx]
        matched_df$betking_league_name  <- bk_ready$betking_league_name[matched_idx]
      }
      rv$matched <- matched_df
      rv$unmatched <- unmatched_df
      log_msg(paste0("Matched ", nrow(matched_df), " / ", nrow(preds),
                     " predictions to BetKing via mapper"))
    }, error = function(e) log_msg(paste0("Error: ", e$message)))
  })
  
  output$match_status <- renderUI({
    if (is.null(rv$matched))
      return(div(class = "ab-status", "Click Fetch to see matchable picks."))
    by_mkt <- table(rv$matched$market)
    div(class = "ab-status ab-status-ok",
        paste0("\u2713 ", nrow(rv$matched), " bookable / ",
               nrow(rv$unmatched), " not on BetKing \u00b7 ",
               paste(paste0(names(by_mkt), " ", by_mkt), collapse = " / ")))
  })
  
  output$market_help <- renderUI({
    choice <- input$market_choice %||% ""
    if (identical(choice, "Cocktail"))
      return(div(class = "ab-status", "Cocktail: one slip mixing all markets."))
    div(class = "ab-status", paste0("Will book ", choice, " picks only."))
  })
  
  observeEvent(input$run_btn, {
    req(rv$matched, input$market_choice)
    choice <- input$market_choice
    if (nrow(rv$matched) == 0) {
      showNotification("Nothing matched. Click Fetch first.", type = "error"); return()
    }
    rv$log <- ""
    log_msg("Starting browser...")
    tryCatch({
      start_browser()
      log_msg("Browser ready.")
      market_codes <- list(); all_failed <- data.frame()
      if (identical(choice, "Cocktail")) {
        res <- process_cocktail_bk(rv$matched, log_msg)
        market_codes[["Cocktail"]] <- res$codes
        if (nrow(res$failed) > 0)
          all_failed <- rbind(all_failed, cbind(res$failed, market = "Cocktail"))
      } else {
        mkt_internal <- BK_MARKET_DISPLAY_TO_KEY[[choice]]
        if (is.null(mkt_internal)) {
          log_msg(paste0("Unknown market: ", choice)); stop_browser(); return()
        }
        res <- process_market_bk(rv$matched, choice, mkt_internal, log_msg)
        market_codes[[choice]] <- res$codes
        if (nrow(res$failed) > 0)
          all_failed <- rbind(all_failed, cbind(res$failed, market = choice))
      }
      log_msg("\n=== EMAIL ===")
      ok <- send_bk_email(market_codes, log_msg)
      log_msg(if (ok) "Email sent." else "Email NOT sent.")
      log_msg("\n=== SUMMARY ===")
      for (m in names(market_codes)) {
        log_msg(paste0(m, ": ", length(market_codes[[m]]), " codes \u2192 ",
                       paste(market_codes[[m]], collapse = ", ")))
      }
      if (nrow(all_failed) > 0) {
        out_path <- file.path(LOG_DIR, paste0("betking_failed_",
                                              format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
        write.csv(all_failed, out_path, row.names = FALSE)
        log_msg(paste0("Failed picks saved to: ", basename(out_path)))
      }
      stop_browser(); log_msg("Done.")
    }, error = function(e) {
      log_msg(paste0("ERROR: ", e$message)); stop_browser()
    })
  })
  session$onSessionEnded(function() { try(stop_browser(), silent = TRUE) })
}

shinyApp(ui, server)
# ============================================================
# M.O.A.B Auto-Bettor — SportyBet booking-code generator
# Uses SportyBet API to fetch country/league directory in one shot,
# then navigates directly to league pages (no menu hover/click chain).
# Booking-code mode only. No login required.
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(dplyr)
  library(readxl)
  library(rvest)
  library(httr)
  library(jsonlite)
  library(stringdist)
})

# ════════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════════
BASE_PATH         <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
DIRECTORY_PATH    <- file.path(BASE_PATH, "sportybet_directory.rds")
LOG_DIR           <- file.path(BASE_PATH, "auto_bettor_logs")
SB_HELPERS_PATH   <- file.path(BASE_PATH, "sportybet_helpers.R")
if (!dir.exists(LOG_DIR)) dir.create(LOG_DIR, recursive = TRUE)

CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"

SPORTYBET_BASE    <- "https://www.sportybet.com"
SPORTYBET_REGION  <- "/ng"
SPORTYBET_FOOTBALL <- paste0(SPORTYBET_BASE, SPORTYBET_REGION, "/sport/football/")
SPORTYBET_DIRECTORY_API <- paste0(SPORTYBET_BASE,
                                  "/api/ng/factsCenter/popularAndSportList?sportId=sr:sport:1&productId=3")

# Load shared SportyBet helpers (fuzzy country/league matching with aliases).
# Falls back to NULL stubs if file missing so the rest of the code still runs.
if (file.exists(SB_HELPERS_PATH)) {
  source(SB_HELPERS_PATH, local = FALSE)
} else {
  filter_to_sportybet_df <- function(df, sb_dir = NULL, ...) list(df = df, matched = rep(TRUE, nrow(df)), diagnostic = NULL, sb_dir = sb_dir)
  match_one_league <- function(...) list(matched = FALSE, sb_row = NULL, score = 0, reason = "helpers_missing")
}

EMAIL_FROM        <- "chukwudiogbuta@gmail.com"
EMAIL_TO          <- "chukwudiogbuta@gmail.com"
EMAIL_APP_PASS    <- "jkhryasdthhxiuts"
EMAIL_SMTP_HOST   <- "smtp.gmail.com"
EMAIL_SMTP_PORT   <- 587

batch_sizes <- function(n) {
  if (n < 10)  return(n)
  if (n < 30)  return(rep(10, ceiling(n / 10))[seq_len(ceiling(n / 10))])
  if (n < 40)  return(rep(15, ceiling(n / 15))[seq_len(ceiling(n / 15))])
  if (n < 100) return(rep(20, ceiling(n / 20))[seq_len(ceiling(n / 20))])
  rep(30, ceiling(n / 30))[seq_len(ceiling(n / 30))]
}

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# SPORTYBET DIRECTORY API
# ════════════════════════════════════════════════════════════
fetch_sportybet_directory <- function() {
  resp <- GET(SPORTYBET_DIRECTORY_API,
              add_headers(`User-Agent` = "Mozilla/5.0"), timeout(30))
  if (status_code(resp) != 200) stop("SportyBet API returned ", status_code(resp))
  body <- content(resp, "parsed", "application/json")
  if (is.null(body$data) || is.null(body$data$sportList)) stop("Unexpected API shape")
  football <- NULL
  for (sp in body$data$sportList) {
    if (identical(sp$id, "sr:sport:1")) { football <- sp; break }
  }
  if (is.null(football)) stop("Football not in API response")
  rows <- list()
  for (cat in football$categories) {
    country <- cat$name; cat_id <- cat$id
    for (trn in cat$tournaments) {
      rows[[length(rows) + 1]] <- data.frame(
        country = country, league = trn$name,
        category_id = cat_id, tournament_id = trn$id,
        event_size = trn$eventSize %||% NA,
        league_url = paste0(SPORTYBET_BASE, SPORTYBET_REGION,
                            "/sport/football/", cat_id, "/", trn$id),
        stringsAsFactors = FALSE)
    }
  }
  do.call(rbind, rows)
}

match_predictions_to_sportybet <- function(pred_df, dir_df) {
  pred_df$country <- as.character(pred_df$country)
  pred_df$league  <- as.character(pred_df$league)
  
  # Use the shared fuzzy matcher (handles country aliases + Jaro-Winkler league matching)
  in_df <- data.frame(country = pred_df$country, league = pred_df$league,
                      stringsAsFactors = FALSE)
  filt <- tryCatch(filter_to_sportybet_df(in_df, dir_df, fuzzy_threshold = 0.75),
                   error = function(e) NULL)
  
  if (is.null(filt) || is.null(filt$diagnostic)) {
    # Fallback: exact lowercase match (legacy behaviour)
    dir_key  <- paste0(tolower(dir_df$country), "||", tolower(dir_df$league))
    pred_key <- paste0(tolower(pred_df$country), "||", tolower(pred_df$league))
    pred_df$.sb_idx <- match(pred_key, dir_key)
    matched   <- pred_df[!is.na(pred_df$.sb_idx), , drop = FALSE]
    unmatched <- pred_df[is.na(pred_df$.sb_idx), , drop = FALSE]
    if (nrow(matched) > 0) {
      matched$category_id   <- dir_df$category_id[matched$.sb_idx]
      matched$tournament_id <- dir_df$tournament_id[matched$.sb_idx]
      matched$league_url    <- dir_df$league_url[matched$.sb_idx]
      matched$.sb_idx       <- NULL
    }
    return(list(matched = matched, unmatched = unmatched))
  }
  
  # Resolve each matched fixture back to its SB directory row
  diag <- filt$diagnostic
  matched_rows <- which(diag$matched)
  
  # Attach SB IDs to the matched predictions
  matched_df <- pred_df[matched_rows, , drop = FALSE]
  if (nrow(matched_df) > 0) {
    cat_id <- character(nrow(matched_df))
    trn_id <- character(nrow(matched_df))
    lg_url <- character(nrow(matched_df))
    for (i in seq_len(nrow(matched_df))) {
      sb_country <- diag$sb_country[matched_rows[i]]
      sb_league  <- diag$sb_league[matched_rows[i]]
      if (!is.na(sb_country) && !is.na(sb_league)) {
        idx <- which(tolower(dir_df$country) == tolower(sb_country) &
                     tolower(dir_df$league)  == tolower(sb_league))[1]
        if (!is.na(idx)) {
          cat_id[i] <- dir_df$category_id[idx]
          trn_id[i] <- dir_df$tournament_id[idx]
          lg_url[i] <- dir_df$league_url[idx]
        }
      }
    }
    matched_df$category_id   <- cat_id
    matched_df$tournament_id <- trn_id
    matched_df$league_url    <- lg_url
  }
  
  unmatched_df <- pred_df[!diag$matched, , drop = FALSE]
  list(matched = matched_df, unmatched = unmatched_df)
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
  port <- 9999
  system2(CHROMEDRIVER_PATH, args = paste0("--port=", port), wait = FALSE)
  Sys.sleep(2)
  resp <- POST(paste0("http://localhost:", port, "/session"),
               body = list(capabilities = list(alwaysMatch = list(
                 browserName = "chrome",
                 `goog:chromeOptions` = list(
                   binary = CHROME_PATH,
                   args = list("--no-sandbox", "--disable-dev-shm-usage",
                               "--disable-blink-features=AutomationControlled",
                               "--disable-extensions", "--window-size=1400,900"))))),
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

selenium_source <- function() {
  content(GET(paste0(SEL$base, "/source"), timeout(30)),
          "parsed", "application/json")$value
}

selenium_js <- function(script, args = list()) {
  # ChromeDriver requires args to be a JSON ARRAY. R's named lists serialize as
  # objects; we must use auto_unbox=FALSE and ensure it's an unnamed list.
  args_body <- if (length(args) == 0) list() else unname(args)
  body_json <- jsonlite::toJSON(list(script = script, args = args_body),
                                auto_unbox = TRUE, null = "null")
  resp <- POST(paste0(SEL$base, "/execute/sync"),
               body = body_json, encode = "raw",
               content_type_json(), timeout(15))
  parsed <- content(resp, "parsed", "application/json")
  parsed$value
}

selenium_find_element <- function(css) {
  resp <- POST(paste0(SEL$base, "/element"),
               body = list(using = "css selector", value = css),
               encode = "json", timeout(15))
  content(resp, "parsed", "application/json")$value
}

selenium_text <- function(el_ref) {
  if (is.null(el_ref)) return("")
  tryCatch({
    ref_id <- if (is.list(el_ref)) el_ref[[1]] else el_ref
    content(GET(paste0(SEL$base, "/element/", ref_id, "/text"), timeout(10)),
            "parsed", "application/json")$value %||% ""
  }, error = function(e) "")
}

wait_for <- function(css, max_wait = 12) {
  for (i in seq_len(max_wait * 2)) {
    if (!is.null(selenium_find_element(css))) return(TRUE)
    Sys.sleep(0.5)
  }
  FALSE
}

# ════════════════════════════════════════════════════════════
# LEAGUE PAGE SCRAPING
# ════════════════════════════════════════════════════════════
scrape_league_fixtures <- function(league_url, country_name, league_name) {
  selenium_get(league_url)
  wait_for("div.m-content-row.match-row", 25)
  Sys.sleep(3)
  pg <- read_html(selenium_source())
  rows <- pg %>% html_nodes("div.m-table-row")
  cur_date <- NA; out <- data.frame()
  for (r in rows) {
    cls <- html_attr(r, "class") %||% ""
    if (grepl("date-row", cls)) {
      d_node <- r %>% html_node(".date")
      if (!is.null(d_node)) cur_date <- html_text(d_node, trim = TRUE)
      next
    }
    if (grepl("match-row", cls)) {
      clock <- r %>% html_node(".clock-time") %>% html_text(trim = TRUE)
      mid_node <- r %>% html_node(".m-market[data-op*='sr:match:']")
      sr_id <- NA
      if (!is.null(mid_node)) {
        attr_val <- html_attr(mid_node, "data-op") %||% ""
        m <- regmatches(attr_val, regexpr("sr:match:[0-9]+", attr_val))
        if (length(m) > 0) sr_id <- m
      }
      home <- r %>% html_node(".home-team") %>% html_text(trim = TRUE)
      away <- r %>% html_node(".away-team") %>% html_text(trim = TRUE)
      out <- rbind(out, data.frame(
        date_str = cur_date, clock = clock, sr_id = sr_id,
        home = home, away = away,
        country = country_name, league = league_name,
        stringsAsFactors = FALSE))
    }
  }
  out
}

# Slugify a string for SportyBet URL: spaces and punctuation to underscores,
# preserve case (SportyBet uses CamelCase in URLs).
sb_slug <- function(s) {
  if (is.null(s) || is.na(s)) return("")
  s <- gsub("[^A-Za-z0-9]+", "_", s)
  gsub("^_+|_+$", "", s)
}

# Build the canonical SportyBet match URL using the exact pattern observed.
build_match_url <- function(country, league, home, away, sr_id) {
  if (is.na(sr_id) || is.na(country) || is.na(league) || is.na(home) || is.na(away))
    return(NA)
  paste0(SPORTYBET_BASE, SPORTYBET_REGION,
         "/sport/football/", sb_slug(country), "/", sb_slug(league),
         "/", sb_slug(home), "_vs_", sb_slug(away),
         "/", sr_id)
}

# ════════════════════════════════════════════════════════════
# FUZZY MATCHING
# ════════════════════════════════════════════════════════════
team_name_match <- function(a, b) {
  if (is.na(a) || is.na(b)) return(1.0)
  stringdist::stringdist(tolower(trimws(a)), tolower(trimws(b)),
                         method = "jw", p = 0.1)
}

find_sportybet_match <- function(pred, sb_fixtures) {
  if (nrow(sb_fixtures) == 0) return(NA)
  pred_md <- substr(as.character(pred$fixture_date), 6, 10)
  pred_md <- paste0(substr(pred_md, 4, 5), "/", substr(pred_md, 1, 2))
  pred_time <- as.character(pred$match_time)
  sb_fixtures$date_md <- sapply(sb_fixtures$date_str, function(s) {
    if (is.na(s)) return(NA); strsplit(s, " ")[[1]][1]
  })
  same_day <- sb_fixtures[sb_fixtures$date_md == pred_md, , drop = FALSE]
  if (nrow(same_day) == 0) return(NA)
  same_time <- same_day[trimws(same_day$clock) == pred_time, , drop = FALSE]
  pool <- if (nrow(same_time) > 0) same_time else same_day
  best <- NULL; best_score <- Inf
  for (i in seq_len(nrow(pool))) {
    hd <- team_name_match(pred$home, pool$home[i])
    ad <- team_name_match(pred$away, pool$away[i])
    if (hd > 0.25 || ad > 0.25) next
    if (hd > 0.10 && ad > 0.10) next
    if (nchar(pred$home) <= 5 && hd > 0.05) next
    if (nchar(pred$away) <= 5 && ad > 0.05) next
    score <- hd + ad
    if (score < best_score) { best_score <- score; best <- pool[i, ] }
  }
  if (is.null(best)) return(NA)
  best$sr_id
}

# ════════════════════════════════════════════════════════════
# SLIP OPERATIONS
# ════════════════════════════════════════════════════════════
# Market dispatch table: internal_key -> (panel_header_substring, outcome_label).
# Panel header match is a SUBSTRING check, outcome label is EXACT trim match.
MARKET_MAP <- list(
  ht_draw       = list(panel = "1st Half - 1X2",         outcome = "Draw"),
  fh_o05        = list(panel = "1st Half - Over/Under",  outcome = "Over 0.5"),
  o15           = list(panel = "Over/Under",             outcome = "Over 1.5"),
  o25           = list(panel = "Over/Under",             outcome = "Over 2.5"),
  btts          = list(panel = "GG/NG",                  outcome = "Yes"),
  u45           = list(panel = "Over/Under",             outcome = "Under 4.5"),
  o15_2h        = list(panel = "2nd Half - Over/Under",  outcome = "Over 1.5"),
  btts_2h       = list(panel = "2nd Half - GG/NG",       outcome = "Yes"),
  home_cs       = list(panel = "Home Team Clean Sheet",  outcome = "Yes"),
  away_cs       = list(panel = "Away Team Clean Sheet",  outcome = "Yes"),
  home_wtn      = list(panel = "Home Team to Win to Nil",outcome = "Yes"),
  away_wtn      = list(panel = "Away Team to Win to Nil",outcome = "Yes"),
  bh_u15        = list(panel = "Both Halves Under 1.5",  outcome = "Yes")
)

# Display name -> internal key.
MARKET_DISPLAY_TO_KEY <- list(
  "HT Draw"               = "ht_draw",
  "FH Over 0.5"           = "fh_o05",
  "Over 1.5 FT"           = "o15",
  "Over 2.5 FT"           = "o25",
  "BTTS"                  = "btts",
  "Under 4.5 FT"          = "u45",
  "2H Over 1.5"           = "o15_2h",
  "2H BTTS"               = "btts_2h",
  "Home Clean Sheet"      = "home_cs",
  "Away Clean Sheet"      = "away_cs",
  "Home Win to Nil"       = "home_wtn",
  "Away Win to Nil"       = "away_wtn",
  "Both Halves Under 1.5" = "bh_u15"
)

# Markets not yet on SportyBet (placeholder)
MARKETS_PLACEHOLDER <- c(
  "Corners Over 9.5", "Home Corners Over 4.5", "Away Corners Over 3.5",
  "Match Cards Over 3.5", "Home Bookings Over 1.5", "Away Bookings Over 1.5",
  "Total Shots Over 21.5", "SOT Over 7.5"
)

add_pick_to_slip <- function(market_key) {
  spec <- MARKET_MAP[[market_key]]
  if (is.null(spec))
    return(list(ok = FALSE, reason = paste0("unknown market key: ", market_key)))
  panel_label <- spec$panel
  outcome_label <- spec$outcome
  result <- selenium_js(paste0(
    "var pl = arguments[0]; var ol = arguments[1];",
    "var w = document.querySelectorAll('.m-table__wrapper');",
    "var sawPanel = false;",
    "for (var i = 0; i < w.length; i++) {",
    "  var h = w[i].querySelector('.m-table-header-title');",
    "  if (!h || h.textContent.indexOf(pl) < 0) continue;",
    "  sawPanel = true;",
    "  var c = w[i].querySelectorAll('.m-table-row.m-outcome .m-table-cell');",
    "  for (var j = 0; j < c.length; j++) {",
    "    var s = c[j].querySelector('.m-table-cell-item');",
    "    if (s && s.textContent.trim() === ol) { c[j].click(); return 'clicked'; }",
    "  }",
    "}",
    "return sawPanel ? 'panel_found_no_outcome' : 'panel_not_found';"),
    list(panel_label, outcome_label))
  if (identical(result, "panel_not_found"))
    return(list(ok = FALSE, reason = "market not offered for this fixture"))
  if (identical(result, "panel_found_no_outcome"))
    return(list(ok = FALSE, reason = paste0(outcome_label, " line not offered")))
  if (!identical(result, "clicked"))
    return(list(ok = FALSE, reason = "click failed"))
  Sys.sleep(0.6)
  verified <- selenium_js(paste0(
    "var pl = arguments[0]; var ol = arguments[1];",
    "var w = document.querySelectorAll('.m-table__wrapper');",
    "for (var i = 0; i < w.length; i++) {",
    "  var h = w[i].querySelector('.m-table-header-title');",
    "  if (!h || h.textContent.indexOf(pl) < 0) continue;",
    "  var c = w[i].querySelectorAll('.m-table-row.m-outcome .m-table-cell');",
    "  for (var j = 0; j < c.length; j++) {",
    "    var s = c[j].querySelector('.m-table-cell-item');",
    "    if (s && s.textContent.trim() === ol) {",
    "      return c[j].className.indexOf('m-table-cell--checked') >= 0;",
    "    }",
    "  }",
    "}",
    "return false;"), list(panel_label, outcome_label))
  if (isTRUE(verified)) list(ok = TRUE, reason = "added")
  else list(ok = FALSE, reason = "click did not register")
}

slip_count <- function() {
  el <- selenium_find_element(".m-bet-count")
  if (is.null(el)) return(0)
  txt <- selenium_text(el)
  n <- suppressWarnings(as.integer(gsub("[^0-9]", "", txt)))
  if (is.na(n)) 0 else n
}

clear_slip <- function() {
  selenium_js("var b = document.querySelector('.m-text-min'); if (b) b.click();")
  Sys.sleep(0.5)
  selenium_js("var b = document.querySelector('.af-button--primary'); if (b) b.click();")
  Sys.sleep(0.5)
}

generate_booking_code <- function() {
  clicked <- selenium_js(paste0(
    "var s = document.querySelectorAll('.m-share--wrapper span');",
    "for (var i = 0; i < s.length; i++) {",
    "  if (s[i].textContent.indexOf('Book Bet') >= 0) { s[i].click(); return true; }",
    "}",
    "return false;"))
  if (!isTRUE(clicked)) return(NA)
  Sys.sleep(3)
  txt <- selenium_js(paste0(
    "var el = document.querySelector('.m-code .m-value');",
    "return el ? el.textContent.trim() : '';"))
  if (is.null(txt) || nchar(txt) == 0) return(NA)
  txt
}

# ════════════════════════════════════════════════════════════
# CORE FLOW
# ════════════════════════════════════════════════════════════
process_market <- function(preds_df, market_key, market_internal, log_fn) {
  log_fn(paste0("\n=== ", market_key, " ==="))
  sub <- preds_df[preds_df$market == market_key, , drop = FALSE]
  if (nrow(sub) == 0) {
    log_fn("  no picks for this market")
    return(list(codes = c(), failed = data.frame()))
  }
  if ("confidence" %in% names(sub))
    sub <- sub[order(-as.numeric(sub$confidence)), , drop = FALSE]
  sub$cl_key <- paste0(sub$country, "::", sub$league)
  groups <- split(sub, sub$cl_key)
  
  bookable <- list(); failed <- data.frame()
  for (gkey in names(groups)) {
    g <- groups[[gkey]]
    country <- as.character(g$country[1])
    league  <- as.character(g$league[1])
    league_url <- as.character(g$league_url[1])
    if (is.na(league_url) || nchar(league_url) == 0) {
      failed <- rbind(failed, cbind(g, reason = "no league_url")); next
    }
    log_fn(paste0("\n[", country, " / ", league, "] ", nrow(g), " picks"))
    sb_fix <- tryCatch(scrape_league_fixtures(league_url, country, league),
                       error = function(e) data.frame())
    if (nrow(sb_fix) == 0) {
      failed <- rbind(failed, cbind(g, reason = "no fixtures on SportyBet page")); next
    }
    for (i in seq_len(nrow(g))) {
      sr <- find_sportybet_match(g[i, ], sb_fix)
      if (is.na(sr)) {
        failed <- rbind(failed, cbind(g[i, ], reason = "no fuzzy match")); next
      }
      # Capture the SportyBet-side row that matched (we need its exact team names
      # for the canonical URL)
      sb_row <- sb_fix[sb_fix$sr_id == sr, , drop = FALSE][1, ]
      bookable[[paste0(gkey, "::", i)]] <- list(
        pick = g[i, ], sr_id = sr,
        sb_country = country, sb_league = league,
        sb_home = sb_row$home, sb_away = sb_row$away)
    }
  }
  
  log_fn(paste0("\nBookable: ", length(bookable), " | Failed: ", nrow(failed)))
  if (length(bookable) == 0) return(list(codes = c(), failed = failed))
  
  sizes <- batch_sizes(length(bookable))
  codes <- c(); idx <- 1; batch_no <- 0
  for (sz in sizes) {
    batch_no <- batch_no + 1
    items <- bookable[idx:min(idx + sz - 1, length(bookable))]
    idx <- idx + sz
    log_fn(paste0("\n--- Batch ", batch_no, ": ", length(items), " picks ---"))
    clear_slip(); Sys.sleep(1)
    added <- 0
    batch_skipped <- data.frame()
    for (it in items) {
      mu <- build_match_url(it$sb_country, it$sb_league,
                            it$sb_home, it$sb_away, it$sr_id)
      if (is.na(mu)) {
        log_fn(paste0("  bad URL: ", it$pick$home, " v ", it$pick$away))
        batch_skipped <- rbind(batch_skipped, cbind(it$pick, reason = "bad URL"))
        next
      }
      selenium_get(mu); Sys.sleep(2)
      res <- add_pick_to_slip(market_internal)
      if (isTRUE(res$ok)) {
        added <- added + 1
        log_fn(paste0("  + ", it$pick$home, " v ", it$pick$away))
      } else {
        log_fn(paste0("  ! skipped (", res$reason, "): ",
                      it$pick$home, " v ", it$pick$away))
        batch_skipped <- rbind(batch_skipped, cbind(it$pick, reason = res$reason))
      }
    }
    if (nrow(batch_skipped) > 0) failed <- rbind(failed, batch_skipped)
    Sys.sleep(1)
    actual <- slip_count()
    log_fn(paste0("  slip size: ", actual, " (expected ", added, ")"))
    if (actual != added) { log_fn("  size mismatch \u2014 skipping booking"); next }
    code <- generate_booking_code()
    if (is.na(code)) { log_fn("  booking code not retrieved"); next }
    log_fn(paste0("  booked: ", code))
    codes <- c(codes, code)
  }
  list(codes = codes, failed = failed)
}

# Cocktail: every prediction is its own (fixture, market) pick. Each row of
# preds_df has a `market` column whose value maps via MARKET_DISPLAY_TO_KEY to
# an internal market key. We process all picks together, group by league for
# efficient scraping, then build batches of up to 30 (or 1 batch if n<30).
# Disabled/placeholder markets are silently dropped.
process_cocktail <- function(preds_df, log_fn) {
  log_fn("\n=== COCKTAIL ===")
  preds_df$mkt_internal <- vapply(as.character(preds_df$market), function(m) {
    k <- MARKET_DISPLAY_TO_KEY[[m]]; if (is.null(k)) NA_character_ else k
  }, character(1))
  drop <- is.na(preds_df$mkt_internal)
  if (any(drop)) {
    log_fn(paste0("  dropping ", sum(drop),
                  " picks with markets not yet bookable on SportyBet"))
  }
  sub <- preds_df[!drop, , drop = FALSE]
  if (nrow(sub) == 0) {
    log_fn("  no bookable picks")
    return(list(codes = c(), failed = data.frame()))
  }
  if ("confidence" %in% names(sub))
    sub <- sub[order(-as.numeric(sub$confidence)), , drop = FALSE]
  
  # Group by league so we scrape each league page just once.
  sub$cl_key <- paste0(sub$country, "::", sub$league)
  groups <- split(sub, sub$cl_key)
  bookable <- list(); failed <- data.frame()
  
  for (gkey in names(groups)) {
    g <- groups[[gkey]]
    country <- as.character(g$country[1])
    league  <- as.character(g$league[1])
    league_url <- as.character(g$league_url[1])
    if (is.na(league_url) || nchar(league_url) == 0) {
      failed <- rbind(failed, cbind(g, reason = "no league_url")); next
    }
    log_fn(paste0("\n[", country, " / ", league, "] ", nrow(g), " picks"))
    sb_fix <- tryCatch(scrape_league_fixtures(league_url, country, league),
                       error = function(e) data.frame())
    if (nrow(sb_fix) == 0) {
      failed <- rbind(failed, cbind(g, reason = "no fixtures on SportyBet page")); next
    }
    for (i in seq_len(nrow(g))) {
      sr <- find_sportybet_match(g[i, ], sb_fix)
      if (is.na(sr)) {
        failed <- rbind(failed, cbind(g[i, ], reason = "no fuzzy match")); next
      }
      sb_row <- sb_fix[sb_fix$sr_id == sr, , drop = FALSE][1, ]
      bookable[[paste0(gkey, "::", i)]] <- list(
        pick = g[i, ], sr_id = sr,
        sb_country = country, sb_league = league,
        sb_home = sb_row$home, sb_away = sb_row$away,
        market_internal = g$mkt_internal[i])
    }
  }
  
  log_fn(paste0("\nBookable: ", length(bookable), " | Failed: ", nrow(failed)))
  if (length(bookable) == 0) return(list(codes = c(), failed = failed))
  
  # Cocktail batching: 1 ticket if n<30, else batches of 30
  n <- length(bookable)
  sizes <- if (n < 30) n else rep(30, ceiling(n / 30))[seq_len(ceiling(n / 30))]
  codes <- c(); idx <- 1; batch_no <- 0
  for (sz in sizes) {
    batch_no <- batch_no + 1
    items <- bookable[idx:min(idx + sz - 1, length(bookable))]
    idx <- idx + sz
    log_fn(paste0("\n--- Cocktail batch ", batch_no, ": ", length(items), " picks ---"))
    clear_slip(); Sys.sleep(1)
    added <- 0; batch_skipped <- data.frame()
    for (it in items) {
      mu <- build_match_url(it$sb_country, it$sb_league,
                            it$sb_home, it$sb_away, it$sr_id)
      if (is.na(mu)) {
        log_fn(paste0("  bad URL: ", it$pick$home, " v ", it$pick$away))
        batch_skipped <- rbind(batch_skipped, cbind(it$pick, reason = "bad URL"))
        next
      }
      selenium_get(mu); Sys.sleep(2)
      res <- add_pick_to_slip(it$market_internal)
      if (isTRUE(res$ok)) {
        added <- added + 1
        log_fn(paste0("  + [", it$pick$market, "] ",
                      it$pick$home, " v ", it$pick$away))
      } else {
        log_fn(paste0("  ! skipped [", it$pick$market, "] (", res$reason, "): ",
                      it$pick$home, " v ", it$pick$away))
        batch_skipped <- rbind(batch_skipped, cbind(it$pick, reason = res$reason))
      }
    }
    if (nrow(batch_skipped) > 0) failed <- rbind(failed, batch_skipped)
    Sys.sleep(1)
    actual <- slip_count()
    log_fn(paste0("  slip size: ", actual, " (expected ", added, ")"))
    if (actual != added) { log_fn("  size mismatch \u2014 skipping booking"); next }
    code <- generate_booking_code()
    if (is.na(code)) { log_fn("  booking code not retrieved"); next }
    log_fn(paste0("  booked: ", code))
    codes <- c(codes, code)
  }
  list(codes = codes, failed = failed)
}

# ════════════════════════════════════════════════════════════
# EMAIL — emayili via namespace (no library() at top level)
# ════════════════════════════════════════════════════════════
build_email_html <- function(market_codes) {
  # Compute totals
  total_codes <- sum(sapply(market_codes, length))
  active_markets <- names(market_codes)[sapply(market_codes, length) > 0]
  
  # Build per-market sections
  rows <- ""
  for (mkt in names(market_codes)) {
    codes <- market_codes[[mkt]]
    if (length(codes) == 0) next
    code_blocks <- paste0(vapply(codes, function(c) {
      paste0(
        '<div style="margin:10px 0; padding:18px 22px; background:linear-gradient(135deg,#0F1115 0%,#1A1D24 100%); ',
        'border:2px dashed #C9A14A; border-radius:6px; text-align:center;">',
        '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:10px; font-weight:600; ',
        'letter-spacing:3px; text-transform:uppercase; color:#7C8290; margin-bottom:6px;">',
        'Booking Code</div>',
        '<div style="font-family:Consolas,\'Courier New\',monospace; font-size:26px; font-weight:900; ',
        'letter-spacing:5px; color:#C9A14A; text-shadow:0 0 10px rgba(201,161,74,0.3);">',
        c, '</div></div>')
    }, character(1)), collapse = "")
    rows <- paste0(rows,
                   '<div style="margin:28px 0;">',
                   '<div style="display:flex; align-items:center; gap:12px; margin-bottom:12px; ',
                   'padding-bottom:8px; border-bottom:1px solid #2A2E37;">',
                   '<div style="width:4px; height:18px; background:#C9A14A;"></div>',
                   '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:13px; font-weight:700; ',
                   'letter-spacing:2px; text-transform:uppercase; color:#EAEAEA;">',
                   mkt, '</div>',
                   '<div style="font-family:Consolas,monospace; font-size:11px; font-weight:600; ',
                   'color:#1E3A8A; background:#DBEAFE; padding:3px 10px; border-radius:10px;">',
                   length(codes), ' code', if (length(codes) == 1) '' else 's', '</div>',
                   '</div>', code_blocks, '</div>')
  }
  
  # Summary chips at top
  summary_chips <- paste0(
    '<div style="display:flex; gap:10px; flex-wrap:wrap; margin-top:18px;">',
    '<div style="background:rgba(201,161,74,0.15); border:1px solid #C9A14A; ',
    'padding:6px 14px; border-radius:14px; font-family:Consolas,monospace; font-size:11px; color:#C9A14A; font-weight:700;">',
    total_codes, ' TOTAL CODES</div>',
    '<div style="background:rgba(30,58,138,0.15); border:1px solid #1E3A8A; ',
    'padding:6px 14px; border-radius:14px; font-family:Consolas,monospace; font-size:11px; color:#93C5FD; font-weight:700;">',
    length(active_markets), ' MARKET', if (length(active_markets) == 1) '' else 'S',
    '</div></div>')
  
  paste0(
    '<!doctype html><html><body style="margin:0;padding:0;background:#0A0B0E;font-family:\'Segoe UI\',Arial,sans-serif;">',
    '<table cellpadding="0" cellspacing="0" width="100%" style="background:#0A0B0E;padding:32px 16px;">',
    '<tr><td align="center">',
    '<table cellpadding="0" cellspacing="0" width="640" ',
    'style="background:#15181E;border-radius:10px;overflow:hidden;border:1px solid #2A2E37;',
    'box-shadow:0 8px 32px rgba(0,0,0,0.5);">',
    
    # HEADER with gradient
    '<tr><td style="background:linear-gradient(135deg,#0F1115 0%,#1E3A8A 50%,#C9A14A 100%);',
    'padding:32px 36px;">',
    '<div style="font-family:\'Segoe UI\',Arial,sans-serif;color:rgba(255,255,255,0.75);',
    'font-size:11px;font-weight:700;letter-spacing:4px;text-transform:uppercase;margin-bottom:6px;">',
    'M.O.A.B Auto-Bettor</div>',
    '<div style="font-family:Georgia,serif;color:#FFFFFF;font-size:32px;font-weight:900;',
    'line-height:1.1;letter-spacing:-0.5px;">Booking Codes <span style="color:#FCD34D;font-style:italic;">Ready</span></div>',
    '<div style="font-family:\'Segoe UI\',Arial,sans-serif;color:rgba(255,255,255,0.7);',
    'font-size:13px;margin-top:10px;letter-spacing:0.5px;">',
    format(Sys.time(), "%A, %d %B %Y &middot; %H:%M"), '</div>',
    summary_chips,
    '</td></tr>',
    
    # BODY with codes
    '<tr><td style="padding:28px 36px 12px;">', rows, '</td></tr>',
    
    # CTA / instructions block
    '<tr><td style="padding:8px 36px 28px;">',
    '<div style="background:linear-gradient(135deg,rgba(30,58,138,0.15) 0%,rgba(201,161,74,0.1) 100%);',
    'border-left:4px solid #C9A14A; padding:16px 20px; border-radius:4px;">',
    '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:12px; font-weight:700; ',
    'letter-spacing:2px; text-transform:uppercase; color:#C9A14A; margin-bottom:6px;">How to use</div>',
    '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:13px; color:#C0C5CF; line-height:1.6;">',
    'Enter each booking code on the SportyBet app or website to load the slip, set your stake, then place the bet. ',
    'Codes are valid until the first fixture in the slip kicks off.',
    '</div></div>',
    '</td></tr>',
    
    # FOOTER
    '<tr><td style="background:#0F1115; padding:18px 36px; border-top:1px solid #2A2E37;">',
    '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:10px; letter-spacing:3px; ',
    'text-transform:uppercase; color:#7C8290; text-align:center;">',
    'M.O.A.B &middot; ', format(Sys.Date(), "%Y"), ' &middot; Auto-Bettor Pipeline',
    '</div></td></tr>',
    
    '</table></td></tr></table></body></html>')
}

send_codes_email <- function(market_codes, log_fn) {
  if (length(market_codes) == 0 || all(sapply(market_codes, length) == 0)) {
    log_fn("No codes to email"); return(FALSE)
  }
  if (!requireNamespace("emayili", quietly = TRUE)) {
    log_fn("emayili package not installed"); return(FALSE)
  }
  html_body <- build_email_html(market_codes)
  msg <- emayili::envelope()
  msg <- emayili::from(msg, EMAIL_FROM)
  msg <- emayili::to(msg, EMAIL_TO)
  msg <- emayili::subject(msg, paste0("M.O.A.B codes \u00b7 ", format(Sys.time(), "%d %b %H:%M")))
  msg <- emayili::html(msg, html_body)
  smtp <- emayili::server(host = EMAIL_SMTP_HOST, port = EMAIL_SMTP_PORT,
                          username = EMAIL_FROM, password = EMAIL_APP_PASS)
  tryCatch({ smtp(msg, verbose = FALSE); TRUE },
           error = function(e) { log_fn(paste0("Email error: ", e$message)); FALSE })
}

# ════════════════════════════════════════════════════════════
# UI — dark theme, gold accents
# ════════════════════════════════════════════════════════════
THEME_CSS <- "
  :root {
    --ivory: #FAF7F2;
    --ivory-2: #F1ECE3;
    --cobalt: #1E3A8A;
    --cobalt-dark: #152a66;
    --gold: #C9A14A;
    --text-1: #1A1A1A;
    --text-2: #4A4A4A;
    --text-3: #888;
    --border: #E5E0D5;
    --good: #2E7D5B;
    --bad: #C44536;
  }
  body { background: var(--ivory-2) !important; color: var(--text-1) !important;
         font-family: 'Inter', 'Segoe UI', system-ui, sans-serif; margin: 0; }
  .container-fluid { max-width: 880px; margin: 32px auto; padding: 0 16px; }
  .ab-header { padding: 0 0 24px; border-bottom: 2px solid var(--gold);
               margin-bottom: 28px; }
  .ab-eyebrow { font-size: 10px; font-weight: 700; letter-spacing: 3px;
                text-transform: uppercase; color: var(--gold); }
  .ab-title { font-size: 26px; font-weight: 600; color: var(--cobalt);
              margin-top: 6px; letter-spacing: -0.01em; }
  .ab-card { background: var(--ivory); border: 1px solid var(--border);
             border-radius: 8px; padding: 22px 24px; margin-bottom: 16px; }
  .ab-step { display: flex; align-items: center; gap: 10px; margin-bottom: 14px; }
  .ab-step-num { width: 22px; height: 22px; border-radius: 50%;
                 background: var(--cobalt); color: var(--ivory);
                 font-weight: 700; font-size: 12px;
                 display: flex; align-items: center; justify-content: center; }
  .ab-step-label { font-size: 11px; font-weight: 700; letter-spacing: 2px;
                   text-transform: uppercase; color: var(--cobalt); }
  .form-control, input[type=file], input[type=text] {
    background: var(--ivory-2) !important; border: 1px solid var(--border) !important;
    color: var(--text-1) !important; border-radius: 6px;
  }
  .ab-status { font-size: 12px; color: var(--text-3); margin-top: 10px; }
  .ab-status-ok { color: var(--good); }
  .checkbox label, .checkbox-inline label {
    color: var(--text-1) !important; font-weight: 500;
  }
  .ab-btn { background: var(--cobalt); color: var(--ivory); border: none;
            border-radius: 6px; padding: 12px 22px; font-weight: 700;
            font-size: 13px; letter-spacing: 1px; text-transform: uppercase;
            cursor: pointer; transition: all .15s; }
  .ab-btn:hover { background: var(--cobalt-dark); }
  .ab-log { background: #FFFFFF; border: 1px solid var(--border);
            border-radius: 6px; padding: 14px 16px;
            font-family: 'JetBrains Mono', Consolas, monospace;
            font-size: 12px; color: var(--text-2); line-height: 1.6;
            max-height: 420px; overflow-y: auto; white-space: pre-wrap;
            margin: 0; }
  .ab-log::-webkit-scrollbar { width: 6px; }
  .ab-log::-webkit-scrollbar-thumb { background: var(--gold); border-radius: 3px; }
"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono&display=swap"),
    tags$style(HTML(THEME_CSS))
  ),
  div(class = "ab-header",
      div(class = "ab-eyebrow", "M.O.A.B"),
      div(class = "ab-title", "Auto-Bettor")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "1"),
          div(class = "ab-step-label", "Upload predictions XLSX")),
      fileInput("predictions_file", NULL, accept = ".xlsx", width = "100%"),
      uiOutput("pred_status")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "2"),
          div(class = "ab-step-label", "Match against SportyBet")),
      actionButton("fetch_dir_btn", "Fetch SportyBet directory", class = "ab-btn"),
      uiOutput("match_status")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "3"),
          div(class = "ab-step-label", "Market")),
      selectInput("market_choice", NULL,
                  choices = list(
                    "Single market" = list(
                      "HT Draw"               = "HT Draw",
                      "FH Over 0.5"           = "FH Over 0.5",
                      "Over 1.5 FT"           = "Over 1.5 FT",
                      "Over 2.5 FT"           = "Over 2.5 FT",
                      "BTTS"                  = "BTTS",
                      "Under 4.5 FT"          = "Under 4.5 FT",
                      "2H Over 1.5"           = "2H Over 1.5",
                      "2H BTTS"               = "2H BTTS",
                      "Home Clean Sheet"      = "Home Clean Sheet",
                      "Away Clean Sheet"      = "Away Clean Sheet",
                      "Home Win to Nil"       = "Home Win to Nil",
                      "Away Win to Nil"       = "Away Win to Nil",
                      "Both Halves Under 1.5" = "Both Halves Under 1.5"
                    ),
                    "Cocktail" = list("Cocktail (all selected markets in one ticket)" = "Cocktail"),
                    "Not yet available on SportyBet" = list(
                      "Corners Over 9.5"        = "_disabled_Corners Over 9.5",
                      "Home Corners Over 4.5"   = "_disabled_Home Corners Over 4.5",
                      "Away Corners Over 3.5"   = "_disabled_Away Corners Over 3.5",
                      "Match Cards Over 3.5"    = "_disabled_Match Cards Over 3.5",
                      "Home Bookings Over 1.5"  = "_disabled_Home Bookings Over 1.5",
                      "Away Bookings Over 1.5"  = "_disabled_Away Bookings Over 1.5",
                      "Total Shots Over 21.5"   = "_disabled_Total Shots Over 21.5",
                      "SOT Over 7.5"            = "_disabled_SOT Over 7.5"
                    )
                  ),
                  selected = "HT Draw", width = "100%"),
      uiOutput("market_help")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "4"),
          div(class = "ab-step-label", "Generate codes")),
      actionButton("run_btn", "Generate codes and email", class = "ab-btn"),
      tags$br(), tags$br(),
      div(class = "ab-step-label", "Activity log"),
      tags$div(style = "height:8px;"),
      tags$pre(class = "ab-log", textOutput("log_out", inline = TRUE)))
)

server <- function(input, output, session) {
  rv <- reactiveValues(preds = NULL, sb_dir = NULL, matched = NULL,
                       unmatched = NULL, log = "")
  log_msg <- function(msg) {
    rv$log <- paste0(rv$log, "[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n")
  }
  
  observeEvent(input$predictions_file, {
    req(input$predictions_file)
    sheets <- excel_sheets(input$predictions_file$datapath)
    sh <- if ("All Picks" %in% sheets) "All Picks" else sheets[1]
    df <- tryCatch(read_excel(input$predictions_file$datapath, sheet = sh),
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
  
  observeEvent(input$fetch_dir_btn, {
    req(rv$preds)
    log_msg("Fetching SportyBet directory...")
    tryCatch({
      rv$sb_dir <- fetch_sportybet_directory()
      log_msg(paste0("Directory: ", nrow(rv$sb_dir), " (country, league) entries"))
      res <- match_predictions_to_sportybet(rv$preds, rv$sb_dir)
      rv$matched <- res$matched; rv$unmatched <- res$unmatched
      log_msg(paste0("Matched ", nrow(res$matched), " / ", nrow(rv$preds),
                     " predictions to SportyBet"))
    }, error = function(e) log_msg(paste0("Error: ", e$message)))
  })
  
  output$match_status <- renderUI({
    if (is.null(rv$matched))
      return(div(class = "ab-status", "Click Fetch to see matchable picks."))
    by_mkt <- table(rv$matched$market)
    div(class = "ab-status ab-status-ok",
        paste0("\u2713 ", nrow(rv$matched), " bookable / ",
               nrow(rv$unmatched), " not on SportyBet \u00b7 ",
               paste(paste0(names(by_mkt), " ", by_mkt), collapse = " / ")))
  })
  
  output$log_out <- renderText({ rv$log })
  
  output$market_help <- renderUI({
    choice <- input$market_choice %||% ""
    if (startsWith(choice, "_disabled_")) {
      mkt <- sub("^_disabled_", "", choice)
      return(div(class = "ab-status",
                 style = "color:#C44536;",
                 paste0("\u26A0 ", mkt,
                        " is not yet available on SportyBet. Check again when top-league football resumes.")))
    }
    if (identical(choice, "Cocktail")) {
      return(div(class = "ab-status",
                 "Cocktail: one slip mixing all markets. Splits into batches of 30 if more than 30 picks."))
    }
    div(class = "ab-status", paste0("Will book ", choice, " picks only."))
  })
  
  observeEvent(input$run_btn, {
    req(rv$matched, input$market_choice)
    choice <- input$market_choice
    if (startsWith(choice, "_disabled_")) {
      showNotification("That market isn't on SportyBet yet.", type = "error")
      return()
    }
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
        log_msg("Mode: COCKTAIL (mixed markets in one slip)")
        res <- process_cocktail(rv$matched, log_msg)
        market_codes[["Cocktail"]] <- res$codes
        if (nrow(res$failed) > 0)
          all_failed <- rbind(all_failed, cbind(res$failed, market = "Cocktail"))
      } else {
        mkt_internal <- MARKET_DISPLAY_TO_KEY[[choice]]
        if (is.null(mkt_internal)) {
          log_msg(paste0("Unknown market: ", choice)); stop_browser(); return()
        }
        res <- process_market(rv$matched, choice, mkt_internal, log_msg)
        market_codes[[choice]] <- res$codes
        if (nrow(res$failed) > 0)
          all_failed <- rbind(all_failed, cbind(res$failed, market = choice))
      }
      
      log_msg("\n=== EMAIL ===")
      ok <- send_codes_email(market_codes, log_msg)
      log_msg(if (ok) "Email sent." else "Email NOT sent.")
      log_msg("\n=== SUMMARY ===")
      for (m in names(market_codes)) {
        log_msg(paste0(m, ": ", length(market_codes[[m]]), " codes \u2192 ",
                       paste(market_codes[[m]], collapse = ", ")))
      }
      if (nrow(all_failed) > 0) {
        out_path <- file.path(LOG_DIR, paste0("failed_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".csv"))
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
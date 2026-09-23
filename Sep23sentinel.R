# ============================================================
# Sentinel v2 — Flashscore DOM change detector
#
# Primary output: GAP DETECTOR — groups selectors by the field
# they extract (goal_times_home, form_icons, stat_value, etc).
# For every sampled match/URL, at least ONE of a field's selectors
# must return >0 nodes. If ANY sample returned 0 nodes from EVERY
# selector for a field, that field goes RED with sample URLs.
#
# Secondary: per-selector view (legacy). A backfall selector that
# never fires while its siblings work is ORANGE "outdated" only —
# not a serious flag.
#
# Samples: 3-5 finished matches (from /results/) and 3-5 upcoming
# matches (from /fixtures/) per active league. Each match hits:
# summary, stats, h2h — every relevant page.
#
# UI tabs (in order):
#   - Gap Detector      : PRIMARY. Red = data-loss risk.
#   - Per-selector view : legacy, individual selector health.
#   - Selectors         : editable list (with page + field + purpose)
#   - Leagues           : editable list
# ============================================================

library(shiny)
library(DT)
library(rvest)
library(httr)
library(jsonlite)
library(future)

# ════════════════════════════════════════════════════════════
# CONFIG — matches moab.R
# ════════════════════════════════════════════════════════════

SENTINEL_DIR      <- file.path(getwd(), "sentinel_data")
if (!dir.exists(SENTINEL_DIR)) dir.create(SENTINEL_DIR, recursive = TRUE)
SELECTORS_PATH    <- file.path(SENTINEL_DIR, "selectors.rds")
LEAGUES_PATH      <- file.path(SENTINEL_DIR, "leagues.rds")
LAST_REPORT_PATH  <- file.path(SENTINEL_DIR, "last_report.rds")

CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
BASE_URL          <- "https://www.flashscore.com"

N_FINISHED_PER_LEAGUE <- 4L   # per league, from /results/
N_UPCOMING_PER_LEAGUE <- 4L   # per league, from /fixtures/ (list-page-only pages)
N_PARALLEL_WORKERS    <- 6L   # parallel chromedriver workers (matches moab.R)

WORKER_PROGRESS_DIR <- file.path(SENTINEL_DIR, "worker_progress")
if (!dir.exists(WORKER_PROGRESS_DIR)) dir.create(WORKER_PROGRESS_DIR, recursive = TRUE)

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a) || is.list(a)) return(a)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# DEFAULT SELECTORS — every selector moab.R touches
# Columns: page, selector, field, purpose
#
# `field` = the thing being extracted; multiple selectors sharing
# a field are alternates (new DOM + legacy fallback). Gap detector
# groups by (page, field): a gap is when NO selector for that
# (page, field) group finds anything on a sampled URL.
# ════════════════════════════════════════════════════════════

default_selectors <- function() {
  rbind(
    # ── Fixtures page (upcoming matches) ────────────────────
    data.frame(page = "fixtures", selector = "div.event__match",
               field = "match_row",
               purpose = "One row per upcoming match", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "span.event__stageTime",
               field = "kickoff_time",
               purpose = "Kickoff date and time", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "div.event__time",
               field = "kickoff_time",
               purpose = "Kickoff time (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "div.event__homeParticipant img",
               field = "home_team",
               purpose = "Home team name (from logo alt)", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "div.event__awayParticipant img",
               field = "away_team",
               purpose = "Away team name (from logo alt)", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "a.eventRowLink",
               field = "match_link",
               purpose = "Clickable link to each match", stringsAsFactors = FALSE),
    
    # ── Results page (finished matches) ─────────────────────
    data.frame(page = "results", selector = "div.event__match",
               field = "match_row",
               purpose = "One row per finished match", stringsAsFactors = FALSE),
    data.frame(page = "results", selector = "a.eventRowLink",
               field = "match_link",
               purpose = "Link to each finished match (used to click through)", stringsAsFactors = FALSE),
    
    # ── Standings page ──────────────────────────────────────
    data.frame(page = "standings", selector = "div.ui-table__row",
               field = "team_row",
               purpose = "One row per team in table", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "div.tableCellRank",
               field = "team_rank",
               purpose = "Team's league position", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "a.tableCellParticipant__name",
               field = "team_name",
               purpose = "Team name in table", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "span.table__cell--value",
               field = "team_stats_numbers",
               purpose = "MP / W / D / L / GF:GA / GD / Pts numbers", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "div.tableCellFormIcon div.wcl-badgeform_AKaAR",
               field = "form_icon",
               purpose = "W/D/L form icons for last games (legacy \u2014 div parent)", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "a.tableCellFormIcon div.wcl-badgeform_AKaAR",
               field = "form_icon",
               purpose = "W/D/L form icons for last games (new DOM \u2014 anchor parent)", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "div[data-testid^='wcl-badgeForm-']",
               field = "form_icon",
               purpose = "W/D/L form badge via data-testid (most stable)", stringsAsFactors = FALSE),
    
    # ── Match summary page (goal times + HT/FT) ─────────────
    data.frame(page = "summary", selector = "div.smv__participantRow.smv__homeParticipant",
               field = "home_event_row",
               purpose = "Home team goal/card/sub event row (legacy \u2014 div)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "li.smv__participantRow.smv__homeParticipant",
               field = "home_event_row",
               purpose = "Home team event row (new DOM \u2014 li)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.smv__participantRow.smv__awayParticipant",
               field = "away_event_row",
               purpose = "Away team goal/card/sub event row (legacy \u2014 div)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "li.smv__participantRow.smv__awayParticipant",
               field = "away_event_row",
               purpose = "Away team event row (new DOM \u2014 li)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.smv__incidentIcon",
               field = "event_icon_wrapper",
               purpose = "Wrapper around the goal/card/sub icon", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "svg[data-testid='wcl-icon-incidents-goal-soccer']",
               field = "goal_ball_icon",
               purpose = "The goal ball icon (used to filter goal events only)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.smv__timeBox",
               field = "goal_minute",
               purpose = "The minute a goal happened (e.g. 45+2')", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "[data-testid='wcl-headerSection-text']",
               field = "half_score_block",
               purpose = "HT / 2H mini-score blocks", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "span[data-testid='wcl-scores-overline-02']",
               field = "half_label_score",
               purpose = "Half label and score inside the mini-blocks", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.detailScore__wrapper",
               field = "match_score",
               purpose = "Final score text on summary page (used to tell a real 0-0 apart from a scrape miss)", stringsAsFactors = FALSE),
    
    # ── Match stats page ────────────────────────────────────
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics']",
               field = "stat_row",
               purpose = "One row per stat category (possession, shots, etc)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-row_2oCpS",
               field = "stat_row",
               purpose = "Stat row (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics-category'] span",
               field = "stat_name",
               purpose = "Name of each stat (legacy \u2014 pre-DOM change)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-category_6sT1J span",
               field = "stat_name",
               purpose = "Stat name (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "span[data-testid='wcl-scores-simple-text-01'].wcl-name_2lXWg",
               field = "stat_name",
               purpose = "Stat name (new DOM \u2014 small rows)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "h4[data-testid='wcl-scores-heading-04']",
               field = "stat_name",
               purpose = "Stat name (new DOM \u2014 large rows)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics-value']",
               field = "stat_value",
               purpose = "The home/away value for each stat (legacy)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-value_XJG99",
               field = "stat_value",
               purpose = "Stat value (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-value_Ywp3J",
               field = "stat_value",
               purpose = "Stat value (new DOM \u2014 home and away, in order)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-labelRow_42JBQ",
               field = "stat_value_container",
               purpose = "Row holding home value, stat name, away value (new DOM)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-awayValue_smmfR",
               field = "stat_away_wrapper",
               purpose = "Wrapper that marks the away value (new DOM)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.detailScore__wrapper",
               field = "match_score_header",
               purpose = "Final score header on the stats page", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "span.detailScore__matchResult",
               field = "match_score_header",
               purpose = "Final score (alternate header)", stringsAsFactors = FALSE),
    
    # ── H2H page ────────────────────────────────────────────
    data.frame(page = "h2h", selector = "div.h2h__section",
               field = "h2h_section",
               purpose = "One section for home form, away form, and past H2H", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.wcl-scores-overline-02_bpqU7",
               field = "h2h_section_title",
               purpose = "Section title (Home form / Away form / H2H)", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "a.h2h__row",
               field = "h2h_match_row",
               purpose = "One past-match row inside a section", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__date",
               field = "h2h_match_date",
               purpose = "Date of that past match (legacy)", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span[data-testid='wcl-stageTime']",
               field = "h2h_match_date",
               purpose = "Date of that past match (new DOM fallback)", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__event",
               field = "h2h_match_competition",
               purpose = "Competition name for that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "div.h2h__homeParticipant img",
               field = "h2h_match_home_team",
               purpose = "Home team of that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "div.h2h__awayParticipant img",
               field = "h2h_match_away_team",
               purpose = "Away team of that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__result > span[data-testid='wcl-tableScore']",
               field = "h2h_match_score",
               purpose = "Score of that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__result > span",
               field = "h2h_match_score",
               purpose = "Score of that past match (fallback)", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "button.wclButtonLink--h2h",
               field = "h2h_show_more_button",
               purpose = "'Show more' button (clicked via JS to expand H2H list)", stringsAsFactors = FALSE)
  )
}

default_leagues <- function() {
  data.frame(
    name = c("Eredivisie", "La Liga", "Premier League",
             "Serie A", "Belgian Pro League", "MLS",
             "Ecuador Liga Pro", "Rwanda Premier League"),
    url  = c("https://www.flashscore.com/football/netherlands/eredivisie/",
             "https://www.flashscore.com/football/spain/laliga/",
             "https://www.flashscore.com/football/england/premier-league/",
             "https://www.flashscore.com/football/italy/serie-a/",
             "https://www.flashscore.com/football/belgium/jupiler-pro-league/",
             "https://www.flashscore.com/football/usa/mls/",
             "https://www.flashscore.com/football/ecuador/liga-pro/",
             "https://www.flashscore.com/football/rwanda/premier-league/"),
    active = TRUE, stringsAsFactors = FALSE
  )
}

load_selectors <- function() {
  if (!file.exists(SELECTORS_PATH)) return(default_selectors())
  loaded <- tryCatch(readRDS(SELECTORS_PATH), error = function(e) default_selectors())
  # Auto-migrate: if `field` column missing (old file), merge in defaults
  if (!"field" %in% names(loaded)) {
    def <- default_selectors()
    merged <- merge(loaded, def[, c("page", "selector", "field")],
                    by = c("page", "selector"), all.x = TRUE, sort = FALSE)
    merged$field[is.na(merged$field)] <- "unknown"
    loaded <- merged[, c("page", "selector", "field", "purpose")]
  }
  loaded
}
save_selectors <- function(df) {
  tryCatch(saveRDS(df, SELECTORS_PATH), error = function(e) NULL)
}
load_leagues <- function() {
  if (!file.exists(LEAGUES_PATH)) return(default_leagues())
  tryCatch(readRDS(LEAGUES_PATH), error = function(e) default_leagues())
}
save_leagues <- function(df) {
  tryCatch(saveRDS(df, LEAGUES_PATH), error = function(e) NULL)
}

# ════════════════════════════════════════════════════════════
# SELENIUM — kill helper (used before/after parallel run)
# ════════════════════════════════════════════════════════════

kill_chromedrivers <- function() {
  for (i in 1:3) {
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE); Sys.sleep(0.5)
  }
  Sys.sleep(1)
}

# ════════════════════════════════════════════════════════════
# CHECK ENGINE
# ════════════════════════════════════════════════════════════

# For each selector on a page, count nodes returned.
# Also captures matched text for lightweight fields (e.g. match_score) so
# the Gap Detector can tell a genuine 0-0/no-card match apart from a scrape miss.
check_page <- function(page_html, selectors_for_page) {
  if (is.null(page_html)) {
    return(data.frame(selector = selectors_for_page$selector,
                      field    = selectors_for_page$field,
                      purpose  = selectors_for_page$purpose,
                      n_nodes  = NA_integer_,
                      sample_text = NA_character_, stringsAsFactors = FALSE))
  }
  out <- lapply(seq_len(nrow(selectors_for_page)), function(i) {
    sel <- selectors_for_page$selector[i]
    nodes <- tryCatch(page_html %>% html_nodes(sel), error = function(e) NULL)
    n <- if (is.null(nodes)) NA_integer_ else length(nodes)
    txt <- tryCatch({
      if (is.null(nodes) || length(nodes) == 0) NA_character_
      else paste(html_text(nodes, trim = TRUE), collapse = " | ")
    }, error = function(e) NA_character_)
    data.frame(selector = sel,
               field   = selectors_for_page$field[i],
               purpose = selectors_for_page$purpose[i],
               n_nodes = n,
               sample_text = txt, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# ════════════════════════════════════════════════════════════
# PARALLEL WORKER — each gets its own chromedriver
#   (mirrors moab.R's run_parallel_worker pattern)
# ════════════════════════════════════════════════════════════

run_sentinel_worker <- function(worker_id, league_chunk, selectors_df,
                                chrome_path, chromedriver_path,
                                base_url, progress_dir,
                                n_finished_per_league,
                                retry_urls = NULL) {
  # retry_urls (optional): data.frame(page_type, url, league) — when
  # supplied, this worker skips the normal per-league crawl and instead
  # re-fetches just these specific pages (used by run_sentinel's retry
  # waves to chase down intermittent misses without re-scraping everything).
  # Re-import libs inside worker process (future::multisession)
  library(rvest); library(httr); library(jsonlite)
  
  port <- 20000 + worker_id * 137 + sample(1:999, 1)
  session_id <- NULL
  
  # ── Local helpers (self-contained, no global state) ──────
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.data.frame(a) || is.list(a)) return(a)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }
  
  navigate <- function(url) {
    tryCatch(POST(paste0("http://localhost:", port, "/session/", session_id, "/url"),
                  body = list(url = url), encode = "json", timeout(30)),
             error = function(e) NULL)
  }
  get_source <- function() {
    res <- tryCatch(GET(paste0("http://localhost:", port, "/session/", session_id, "/source"),
                        timeout(30)), error = function(e) NULL)
    if (is.null(res)) return(NULL)
    html_raw <- tryCatch(fromJSON(content(res, as = "text"))$value, error = function(e) NULL)
    if (is.null(html_raw)) return(NULL)
    tryCatch(read_html(html_raw), error = function(e) NULL)
  }
  js_eval <- function(script_text) {
    body_json <- sprintf('{"script": %s, "args": []}', toJSON(script_text, auto_unbox = TRUE))
    tryCatch({
      r <- POST(paste0("http://localhost:", port, "/session/", session_id, "/execute/sync"),
                body = body_json, encode = "raw",
                add_headers(`Content-Type` = "application/json"), timeout(15))
      fromJSON(content(r, as = "text"))$value
    }, error = function(e) NULL)
  }
  dismiss_cookie <- function() {
    js_eval("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
    Sys.sleep(0.5)
  }
  wait_for_page <- function(max_wait = 20) {
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      title <- js_eval("return document.title;")
      title <- if (is.null(title)) "" else as.character(title)[1]
      if (length(title) != 1 ||
          grepl("Just a moment|Checking", title, ignore.case = TRUE) ||
          nchar(title) == 0) next
      return(TRUE)
    }
    FALSE
  }
  
  # ── Wait for actual content to render (SPA async load) ──
  # This fixes the PL/Eredivisie/La Liga gap issue: Cloudflare clears
  # and title is set, but match rows haven't loaded yet via XHR.
  # (Reverted from a stability-check version that required the node count
  # to hold steady for 2 checks in a row — that caused results/standings
  # pages to come back completely empty, likely because those pages have
  # ticking/live content that never truly goes still. Back to the simple
  # "return as soon as we see any node" version, with a longer ceiling.)
  wait_for_content <- function(css_selector, max_wait = 30) {
    js_check <- sprintf("return document.querySelectorAll('%s').length;", css_selector)
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      n <- js_eval(js_check)
      if (!is.null(n) && is.numeric(n) && n > 0) return(TRUE)
    }
    FALSE
  }
  
  get_html <- function(url, content_selector = NULL) {
    Sys.sleep(runif(1, 0.5, 1.5))
    navigate(url)
    Sys.sleep(runif(1, 2, 3))
    wait_for_page(20)
    dismiss_cookie()
    # Wait for actual content to appear in the DOM (fixes high-traffic league pages)
    if (!is.null(content_selector)) {
      wait_for_content(content_selector, max_wait = 30)
    } else {
      Sys.sleep(2)  # generic fallback
    }
    get_source()
  }
  
  click_show_more_h2h <- function() {
    for (attempt in 1:2) {
      n <- js_eval(paste0(
        "var btns = document.querySelectorAll('button.wclButtonLink--h2h');",
        "var clicked = 0;",
        "for (var i = 0; i < btns.length; i++) {",
        "  try { btns[i].scrollIntoView({block:'center'}); btns[i].click(); clicked++; } catch(e) {}",
        "}",
        "return clicked;"))
      Sys.sleep(2)
      if (is.null(n) || (is.numeric(n) && n == 0)) break
    }
  }
  
  # ── Re-fetch ONE already-known URL for a retry wave ──────
  # page_type + url fully determine how to fetch a page (its content
  # selector, how sub-urls are derived, which selector table to check).
  # This mirrors the per-league loop below but for a single URL, so a
  # retry wave can re-check just the handful of URLs that came up as
  # gaps on the first pass instead of re-crawling every league.
  fetch_one_gap_url <- function(page_type, url) {
    if (page_type == "results") {
      page <- get_html(url, content_selector = "div.event__match, a.eventRowLink")
      sel <- selectors_df[selectors_df$page == "results", , drop = FALSE]
    } else if (page_type == "fixtures") {
      page <- get_html(url, content_selector = "div.event__match, a.eventRowLink")
      sel <- selectors_df[selectors_df$page == "fixtures", , drop = FALSE]
    } else if (page_type == "standings") {
      page <- get_html(url, content_selector = "div.ui-table__row, div.table__row")
      sel <- selectors_df[selectors_df$page == "standings", , drop = FALSE]
    } else if (page_type == "summary") {
      page <- get_html(url,
                       content_selector = "li.smv__participantRow, div.smv__participantRow, div.detailScore__wrapper")
      sel <- selectors_df[selectors_df$page == "summary", , drop = FALSE]
    } else if (page_type == "stats") {
      page <- get_html(url,
                       content_selector = "div[data-testid='wcl-statistics'], div.wcl-row_2oCpS")
      sel <- selectors_df[selectors_df$page == "stats", , drop = FALSE]
    } else if (page_type == "h2h") {
      navigate(url)
      Sys.sleep(3)
      wait_for_page(20)
      dismiss_cookie()
      wait_for_content("div.h2h__section, a.h2h__row", max_wait = 30)
      Sys.sleep(1)
      click_show_more_h2h()
      page <- get_source()
      sel <- selectors_df[selectors_df$page == "h2h", , drop = FALSE]
    } else {
      return(NULL)
    }
    check_page(page, sel)
  }
  
  write_progress <- function(msg) {
    pf <- file.path(progress_dir, paste0("sentinel_worker_", worker_id, "_progress.txt"))
    tryCatch(writeLines(msg, pf), error = function(e) NULL)
  }
  
  # ── Start chromedriver (with retry, like moab.R) ─────────
  for (attempt in 1:3) {
    system2(chromedriver_path, args = paste0("--port=", port), wait = FALSE)
    Sys.sleep(3)
    resp <- tryCatch(POST(
      paste0("http://localhost:", port, "/session"),
      body = list(capabilities = list(alwaysMatch = list(
        browserName = "chrome",
        `goog:chromeOptions` = list(
          binary = chrome_path,
          args = list("--no-sandbox", "--disable-dev-shm-usage",
                      "--disable-blink-features=AutomationControlled",
                      "--disable-extensions"),
          excludeSwitches = list("enable-automation"),
          useAutomationExtension = FALSE)))),
      encode = "json", timeout(30)), error = function(e) NULL)
    if (!is.null(resp) && status_code(resp) == 200) {
      sd <- fromJSON(content(resp, as = "text"))
      session_id <- sd$sessionId %||% sd$value$sessionId
      break
    }
    port <- port + 1
  }
  if (is.null(session_id)) {
    write_progress("DONE|FAILED to start chromedriver")
    return(list(worker_id = worker_id, success = FALSE, error = "Failed to start chromedriver",
                results = data.frame()))
  }
  
  # Warm up — visit base URL, clear Cloudflare, dismiss cookies
  navigate(paste0(base_url, "/"))
  Sys.sleep(4)
  wait_for_page(30)
  dismiss_cookie()
  Sys.sleep(2)
  
  results <- list()
  
  # ── Retry mode: re-fetch only the specific gap URLs handed in ──
  if (!is.null(retry_urls) && nrow(retry_urls) > 0) {
    total_retry <- nrow(retry_urls)
    for (ri in seq_len(total_retry)) {
      write_progress(paste0("retry ", ri, "/", total_retry, " | ",
                            retry_urls$page_type[ri], " | ", retry_urls$league[ri]))
      checks <- tryCatch(
        fetch_one_gap_url(retry_urls$page_type[ri], retry_urls$url[ri]),
        error = function(e) NULL)
      if (!is.null(checks) && nrow(checks) > 0) {
        checks$league <- retry_urls$league[ri]
        checks$page_type <- retry_urls$page_type[ri]
        checks$url <- retry_urls$url[ri]
        results[[length(results) + 1]] <- checks
      }
    }
    
    tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                    timeout(10)), error = function(e) invisible())
    write_progress("DONE")
    return(list(worker_id = worker_id, success = TRUE, error = NULL,
                results = if (length(results) > 0) do.call(rbind, results) else data.frame()))
  }
  
  # ── Process assigned leagues ─────────────────────────────
  total_leagues <- nrow(league_chunk)
  
  for (li in seq_len(total_leagues)) {
    league_name <- league_chunk$name[li]
    league_url  <- sub("/$", "", league_chunk$url[li])
    write_progress(paste0(li, "/", total_leagues, " | ", league_name))
    
    # Results page
    results_url <- paste0(league_url, "/results/")
    page <- get_html(results_url,
                     content_selector = "div.event__match, a.eventRowLink")
    sel_res <- selectors_df[selectors_df$page == "results", , drop = FALSE]
    checks <- check_page(page, sel_res)
    checks$league <- league_name; checks$page_type <- "results"; checks$url <- results_url
    results[[length(results) + 1]] <- checks
    
    finished_urls <- character(0)
    if (!is.null(page)) {
      links <- page %>% html_nodes("a.eventRowLink") %>% html_attr("href")
      links <- links[!is.na(links) & nzchar(links)]
      finished_urls <- head(links, n_finished_per_league)
    }
    
    # Fixtures page
    fixtures_url <- paste0(league_url, "/fixtures/")
    page <- get_html(fixtures_url,
                     content_selector = "div.event__match, a.eventRowLink")
    sel_fx <- selectors_df[selectors_df$page == "fixtures", , drop = FALSE]
    checks <- check_page(page, sel_fx)
    checks$league <- league_name; checks$page_type <- "fixtures"; checks$url <- fixtures_url
    results[[length(results) + 1]] <- checks
    
    # Standings page
    standings_url <- paste0(league_url, "/standings/")
    page <- get_html(standings_url,
                     content_selector = "div.ui-table__row, div.table__row")
    sel_st <- selectors_df[selectors_df$page == "standings", , drop = FALSE]
    checks <- check_page(page, sel_st)
    checks$league <- league_name; checks$page_type <- "standings"; checks$url <- standings_url
    results[[length(results) + 1]] <- checks
    
    # Per-match pages (finished only)
    for (mu in finished_urls) {
      if (!startsWith(mu, "http")) mu <- paste0(base_url, mu)
      
      # Summary
      page <- get_html(mu,
                       content_selector = "li.smv__participantRow, div.smv__participantRow, div.detailScore__wrapper")
      sel_sm <- selectors_df[selectors_df$page == "summary", , drop = FALSE]
      checks <- check_page(page, sel_sm)
      checks$league <- league_name; checks$page_type <- "summary"; checks$url <- mu
      results[[length(results) + 1]] <- checks
      
      # Stats — overall tab
      stats_url <- sub("(\\?mid=)", "summary/stats/overall/\\1", mu)
      page <- get_html(stats_url,
                       content_selector = "div[data-testid='wcl-statistics'], div.wcl-row_2oCpS")
      sel_ss <- selectors_df[selectors_df$page == "stats", , drop = FALSE]
      checks <- check_page(page, sel_ss)
      checks$league <- league_name; checks$page_type <- "stats"; checks$url <- stats_url
      results[[length(results) + 1]] <- checks
      
      # H2H
      h2h_url <- sub("(\\?mid=)", "h2h/overall/\\1", mu)
      navigate(h2h_url)
      Sys.sleep(3)
      wait_for_page(20)
      dismiss_cookie()
      wait_for_content("div.h2h__section, a.h2h__row", max_wait = 30)
      Sys.sleep(1)
      click_show_more_h2h()
      page <- get_source()
      sel_h2h <- selectors_df[selectors_df$page == "h2h", , drop = FALSE]
      checks <- check_page(page, sel_h2h)
      checks$league <- league_name; checks$page_type <- "h2h"; checks$url <- h2h_url
      results[[length(results) + 1]] <- checks
    }
  }
  
  # Cleanup: close chromedriver session
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  
  write_progress("DONE")
  list(worker_id = worker_id, success = TRUE, error = NULL,
       results = if (length(results) > 0) do.call(rbind, results) else data.frame())
}

# ════════════════════════════════════════════════════════════
# ORCHESTRATOR — distributes leagues across parallel workers
# ════════════════════════════════════════════════════════════

# Runs one batch of parallel workers and returns their combined results.
# Used both for the initial full crawl (league_chunks, retry_chunks = NULL)
# and for a retry wave (retry_chunks set, league_chunks = NULL).
run_worker_batch <- function(n_workers, selectors_df, progress_cb,
                             league_chunks = NULL, retry_chunks = NULL,
                             wave_label = "") {
  old_files <- list.files(WORKER_PROGRESS_DIR, full.names = TRUE)
  if (length(old_files) > 0) file.remove(old_files)
  
  plan(multisession, workers = n_workers)
  
  chrome_p <- CHROME_PATH; cd_p <- CHROMEDRIVER_PATH
  base_u <- BASE_URL; prog_dir <- WORKER_PROGRESS_DIR
  n_fin <- N_FINISHED_PER_LEAGUE
  sel_df <- selectors_df
  
  futures <- list()
  for (i in seq_len(n_workers)) {
    local_chunk <- if (!is.null(league_chunks)) league_chunks[[i]] else data.frame()
    local_retry <- if (!is.null(retry_chunks)) retry_chunks[[i]] else NULL
    futures[[i]] <- future({
      run_sentinel_worker(i, local_chunk, sel_df,
                          chrome_p, cd_p, base_u, prog_dir, n_fin,
                          retry_urls = local_retry)
    }, seed = TRUE,
    globals = list(i = i, local_chunk = local_chunk, local_retry = local_retry,
                   sel_df = sel_df,
                   chrome_p = chrome_p, cd_p = cd_p, base_u = base_u,
                   prog_dir = prog_dir, n_fin = n_fin,
                   run_sentinel_worker = run_sentinel_worker,
                   check_page = check_page))
  }
  
  done <- rep(FALSE, n_workers)
  while (!all(done)) {
    Sys.sleep(2)
    status_parts <- character()
    for (i in seq_len(n_workers)) {
      pf <- file.path(WORKER_PROGRESS_DIR, paste0("sentinel_worker_", i, "_progress.txt"))
      if (file.exists(pf)) {
        txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
        if (grepl("DONE", txt)) done[i] <- TRUE
        status_parts <- c(status_parts, paste0("W", i, ":", txt))
      } else {
        status_parts <- c(status_parts, paste0("W", i, ":starting"))
      }
    }
    if (!is.null(progress_cb)) {
      frac <- sum(done) / n_workers
      progress_cb(frac, paste0(wave_label, paste(status_parts, collapse = "  ")))
    }
  }
  
  all_results <- list()
  errors <- character()
  for (i in seq_len(n_workers)) {
    worker_result <- tryCatch(value(futures[[i]]), error = function(e) {
      list(worker_id = i, success = FALSE, error = e$message, results = data.frame())
    })
    if (worker_result$success && nrow(worker_result$results) > 0) {
      all_results[[length(all_results) + 1]] <- worker_result$results
    } else if (!worker_result$success) {
      errors <- c(errors, paste0("Worker ", i, ": ", worker_result$error))
    }
  }
  
  plan(sequential)
  
  list(results = if (length(all_results) > 0) do.call(rbind, all_results) else data.frame(),
       errors = errors)
}

# Given the full results data frame, returns a data.frame(page_type, url,
# league) of every (page_type, url) page that has AT LEAST ONE field-level
# gap on it — same per-field "did any selector in this field's group fire"
# test compute_gaps uses, just rolled up to the page instead of the field.
# We retry the whole page (can't re-fetch a single field) whenever any
# field on it looks empty, including a page that's otherwise fine but
# missing just one lagging element (e.g. goal_minute loaded slower than
# the rest of the summary page) — that's exactly the intermittent-miss
# pattern we're chasing, not just total page failures.
find_retry_targets <- function(results) {
  if (is.null(results) || nrow(results) == 0) return(data.frame())
  r <- results[!is.na(results$field) & results$field != "unknown", , drop = FALSE]
  if (nrow(r) == 0) return(data.frame())
  key <- paste(r$page_type, r$field, r$url, sep = "\u0001")
  any_hit <- tapply(r$n_nodes, key, function(x) any(x > 0, na.rm = TRUE))
  first_league <- tapply(r$league, key, function(x) x[1])
  bad <- names(any_hit)[!any_hit]
  if (length(bad) == 0) return(data.frame())
  parts <- do.call(rbind, strsplit(bad, "\u0001", fixed = TRUE))
  page_url <- data.frame(page_type = parts[, 1], url = parts[, 3],
                         league = as.character(first_league[bad]),
                         stringsAsFactors = FALSE)
  unique(page_url)
}

run_sentinel <- function(leagues_df, selectors_df, progress_cb = NULL,
                         max_retry_waves = 2) {
  active_leagues <- leagues_df[isTRUE(leagues_df$active) | leagues_df$active == TRUE, , drop = FALSE]
  if (nrow(active_leagues) == 0) stop("No active leagues")
  
  n_workers <- min(N_PARALLEL_WORKERS, nrow(active_leagues))
  
  # Distribute leagues round-robin across workers
  assignments <- cut(seq_len(nrow(active_leagues)), n_workers, labels = FALSE)
  chunks <- split(active_leagues, assignments)
  
  batch <- run_worker_batch(n_workers, selectors_df, progress_cb,
                            league_chunks = chunks, wave_label = "[pass 1] ")
  all_results <- batch$results
  errors <- batch$errors
  
  if ((is.null(all_results) || nrow(all_results) == 0)) {
    if (length(errors) > 0) stop(paste(errors, collapse = "; "))
    stop("No results collected from any worker")
  }
  
  # ── Retry waves: re-fetch only the pages that came back fully empty ──
  # (every field's selectors returned 0 nodes) — almost always a page
  # that didn't finish rendering before we grabbed it, not a real gap.
  for (wave in seq_len(max_retry_waves)) {
    targets <- find_retry_targets(all_results)
    if (nrow(targets) == 0) break  # nothing left to retry
    
    n_retry_workers <- min(N_PARALLEL_WORKERS, nrow(targets))
    retry_assignments <- cut(seq_len(nrow(targets)), n_retry_workers, labels = FALSE)
    retry_chunks <- split(targets, retry_assignments)
    # Pad to n_retry_workers chunks so run_worker_batch's indexing lines up
    if (length(retry_chunks) < n_retry_workers) {
      for (k in (length(retry_chunks) + 1):n_retry_workers) retry_chunks[[k]] <- data.frame()
    }
    
    retry_batch <- run_worker_batch(n_retry_workers, selectors_df, progress_cb,
                                    retry_chunks = retry_chunks,
                                    wave_label = paste0("[retry ", wave, "/", max_retry_waves, "] "))
    if (is.null(retry_batch$results) || nrow(retry_batch$results) == 0) next
    
    # Replace the old (page_type, url) rows with the fresh retry rows —
    # whatever the retry found (hit or still-a-gap) is more up to date
    # than what the earlier pass saw.
    retried_key <- paste(retry_batch$results$page_type, retry_batch$results$url, sep = "\u0001")
    all_key <- paste(all_results$page_type, all_results$url, sep = "\u0001")
    all_results <- all_results[!(all_key %in% unique(retried_key)), , drop = FALSE]
    all_results <- rbind(all_results, retry_batch$results)
  }
  
  all_results
}

# ════════════════════════════════════════════════════════════
# GAP DETECTOR — the primary output
# ════════════════════════════════════════════════════════════
# For each (page_type, field) group and each unique URL sampled,
# check if AT LEAST ONE selector in the group returned > 0 nodes.
# If ZERO of a field's selectors matched anything on a given URL,
# that URL is a "gap" for that field.
# ════════════════════════════════════════════════════════════

compute_gaps <- function(results) {
  if (is.null(results) || nrow(results) == 0) return(NULL)
  
  # Fields where a "gap" (0 nodes) can be a LEGITIMATE outcome, not a scrape
  # miss, when the match had no goals (0-0). We only suppress these two —
  # cards/subs can still happen in a 0-0 game, so home/away_event_row and
  # event_icon_wrapper are NOT suppressed.
  GOAL_ONLY_FIELDS <- c("goal_ball_icon", "goal_minute")
  
  # Per-URL: does the summary page's match_score selector read as 0-0 /
  # 0:0 (any separator)? Built once, before filtering out "match_score"
  # rows below (that field itself isn't a thing we gap-check).
  score_rows <- results[results$page_type == "summary" &
                          results$field == "match_score", , drop = FALSE]
  zero_zero_url <- character(0)
  if (nrow(score_rows) > 0) {
    is_00 <- !is.na(score_rows$sample_text) &
      grepl("^\\s*0\\s*[-:]\\s*0\\s*$", score_rows$sample_text)
    zero_zero_url <- unique(score_rows$url[is_00])
  }
  
  # Only look at rows where field is set (defaults will always have it;
  # user-added rows without field get "unknown" and are shown but ignored here)
  results <- results[!is.na(results$field) & results$field != "unknown", , drop = FALSE]
  if (nrow(results) == 0) return(NULL)
  
  # For each (page_type, field, url) — did any selector fire?
  key <- paste(results$page_type, results$field, results$url, sep = "\u0001")
  any_hit <- tapply(results$n_nodes, key, function(x) any(x > 0, na.rm = TRUE))
  first_league <- tapply(results$league, key, function(x) x[1])
  
  parts <- do.call(rbind, strsplit(names(any_hit), "\u0001", fixed = TRUE))
  per_url <- data.frame(
    page_type = parts[, 1],
    field     = parts[, 2],
    url       = parts[, 3],
    league    = as.character(first_league),
    any_hit   = as.logical(any_hit),
    stringsAsFactors = FALSE
  )
  
  # Suppress false gaps: goal-only field, zero nodes, but the match's own
  # score reads 0-0 — nothing to have shown in the first place.
  suppress <- per_url$field %in% GOAL_ONLY_FIELDS &
    !per_url$any_hit &
    per_url$url %in% zero_zero_url
  per_url$any_hit[suppress] <- TRUE
  per_url$suppressed_00 <- suppress
  
  # Aggregate to (page_type, field): urls sampled, urls with gap
  agg <- aggregate(any_hit ~ page_type + field, data = per_url,
                   FUN = function(x) c(sampled = length(x),
                                       hits    = sum(x),
                                       gaps    = sum(!x)))
  gap_summary <- data.frame(
    page_type = agg$page_type,
    field     = agg$field,
    urls_sampled = as.integer(agg$any_hit[, "sampled"]),
    urls_ok      = as.integer(agg$any_hit[, "hits"]),
    urls_gap     = as.integer(agg$any_hit[, "gaps"]),
    stringsAsFactors = FALSE
  )
  gap_summary$status <- ifelse(gap_summary$urls_gap == 0, "OK", "GAP")
  
  # For each field with gaps, collect up to 5 sample gap URLs
  sample_urls_for <- function(pg, fd) {
    rows <- per_url[per_url$page_type == pg & per_url$field == fd & !per_url$any_hit, , drop = FALSE]
    if (nrow(rows) == 0) return("")
    paste(head(paste0("[", rows$league, "] ", rows$url), 5), collapse = " | ")
  }
  gap_summary$sample_gap_urls <- mapply(sample_urls_for,
                                        gap_summary$page_type,
                                        gap_summary$field)
  
  # Sort: gaps first, then by page then field
  gap_summary <- gap_summary[order(gap_summary$status != "GAP",
                                   gap_summary$page_type,
                                   gap_summary$field), , drop = FALSE]
  list(summary = gap_summary, per_url = per_url)
}

# ════════════════════════════════════════════════════════════
# PER-SELECTOR SUMMARY (legacy view)
# Adds "outdated" tag: selector returned 0 on all URLs while another
# selector for the same (page, field) group hit at least one URL.
# ════════════════════════════════════════════════════════════

summarize_selectors <- function(results) {
  if (is.null(results) || nrow(results) == 0) return(NULL)
  
  # Per (page_type, selector, field, purpose): how many URLs did it hit?
  key <- paste(results$page_type, results$selector, results$field, results$purpose,
               sep = "\u0001")
  n_ok <- tapply(results$n_nodes, key, function(x) sum(x > 0, na.rm = TRUE))
  n_total <- tapply(results$n_nodes, key, length)
  n_na <- tapply(results$n_nodes, key, function(x) sum(is.na(x)))
  
  parts <- do.call(rbind, strsplit(names(n_ok), "\u0001", fixed = TRUE))
  s <- data.frame(
    page_type = parts[, 1],
    selector  = parts[, 2],
    field     = parts[, 3],
    purpose   = parts[, 4],
    ok_urls   = as.integer(n_ok),
    total_urls= as.integer(n_total),
    na_urls   = as.integer(n_na),
    stringsAsFactors = FALSE
  )
  
  # For each (page, field) group, does ANY sibling hit anything?
  sibling_hits <- aggregate(ok_urls ~ page_type + field, data = s, FUN = sum)
  names(sibling_hits)[3] <- "field_total_hits"
  s <- merge(s, sibling_hits, by = c("page_type", "field"), all.x = TRUE, sort = FALSE)
  
  # Status logic:
  # OK        — this selector hit all URLs
  # PARTIAL   — hit some but not all URLs (and >0)
  # OUTDATED  — hit ZERO URLs, but a sibling in same field DID hit something
  # BROKEN    — hit ZERO URLs, and NO sibling helped either (or no siblings — this is bad)
  s$status <- ifelse(s$ok_urls == s$total_urls, "OK",
                     ifelse(s$ok_urls > 0, "PARTIAL",
                            ifelse(s$field_total_hits > 0, "OUTDATED", "BROKEN")))
  
  s[order(match(s$status, c("BROKEN", "PARTIAL", "OUTDATED", "OK")),
          s$page_type, s$field), , drop = FALSE]
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,700;9..144,900&family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML("
      *,*::before,*::after{margin:0;padding:0;box-sizing:border-box;}
      :root{
        --ivory:#FAF7F2;--ivory-2:#F5EEDF;
        --cobalt:#1E3A8A;--cobalt-deep:#0F1F4D;
        --gold:#C9A14A;--gold-deep:#8C6A1E;
        --text:#0F1F4D;--text-2:#5c5448;--text-3:#8e8678;
        --border:#e8dfc9;--white:#fff;
        --success:#0F6E56;--success-bg:#E1F5EE;
        --danger:#A32D2D;--danger-bg:#FCEBEB;
        --warning:#854F0B;--warning-bg:#FAEEDA;
        --fh:'Fraunces',Georgia,serif;--fb:'Inter',system-ui,sans-serif;--fm:'JetBrains Mono',monospace;
      }
      body{background:var(--ivory);color:var(--text);font-family:var(--fb);}
      .hd{background:var(--cobalt);color:var(--ivory);padding:24px 36px;
          display:flex;align-items:center;justify-content:space-between;
          border-bottom:3px solid var(--gold);}
      .brand{display:flex;align-items:baseline;gap:18px;}
      .logo{font-family:var(--fh);font-size:32px;font-weight:900;letter-spacing:-0.5px;line-height:1;}
      .logo .dot{color:var(--gold);}
      .sub{font-family:var(--fb);font-size:10px;font-weight:600;letter-spacing:4px;
           text-transform:uppercase;color:rgba(250,247,242,0.75);}
      .pg{max-width:1500px;margin:0 auto;padding:32px 36px;}
      .nav-tabs{border:none!important;border-bottom:1px solid var(--border)!important;margin-bottom:28px!important;}
      .nav-tabs>li>a{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
                     letter-spacing:2.5px!important;text-transform:uppercase!important;
                     color:var(--text-3)!important;border:none!important;padding:14px 28px!important;
                     border-bottom:2px solid transparent!important;background:transparent!important;}
      .nav-tabs>li.active>a{color:var(--cobalt)!important;border-bottom-color:var(--gold)!important;}
      .card{background:var(--white);border:1px solid var(--border);border-radius:6px;
            padding:24px 28px;margin-bottom:16px;position:relative;
            box-shadow:0 1px 3px rgba(15,31,77,0.04);}
      .card::before{content:'';position:absolute;top:0;left:0;width:48px;height:2px;background:var(--gold);}
      .eyebrow{font-family:var(--fb);font-size:9px;letter-spacing:3px;text-transform:uppercase;
               color:var(--gold-deep);font-weight:700;margin-bottom:12px;}
      .section-lab{font-family:var(--fh);font-size:24px;font-weight:700;color:var(--cobalt-deep);
                    letter-spacing:-0.5px;margin-bottom:6px;}
      .section-sub{font-family:var(--fb);font-size:13px;color:var(--text-3);margin-bottom:20px;}
      .btn-primary-sentinel{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
                             letter-spacing:2px!important;text-transform:uppercase!important;
                             background:var(--cobalt)!important;color:var(--ivory)!important;
                             border:none!important;border-radius:3px!important;padding:12px 28px!important;}
      .btn-outline-sentinel{font-family:var(--fb)!important;font-size:10px!important;font-weight:600!important;
                              letter-spacing:2px!important;text-transform:uppercase!important;
                              background:transparent!important;color:var(--cobalt)!important;
                              border:1px solid var(--cobalt)!important;border-radius:3px!important;
                              padding:9px 22px!important;}
      .btn-danger-sentinel{font-family:var(--fb)!important;font-size:10px!important;font-weight:600!important;
                             letter-spacing:2px!important;text-transform:uppercase!important;
                             background:var(--danger)!important;color:var(--white)!important;
                             border:none!important;border-radius:3px!important;padding:9px 22px!important;}
      table.dataTable thead th{background:var(--ivory-2)!important;font-size:10px!important;
                                font-weight:700!important;letter-spacing:2px!important;
                                text-transform:uppercase!important;color:var(--text-2)!important;
                                padding:12px 14px!important;border-bottom:1px solid var(--border)!important;}
      table.dataTable tbody td{font-size:12.5px!important;padding:10px 14px!important;
                                border-bottom:1px solid var(--border)!important;
                                font-family:var(--fb)!important;}
      .status-ok{background:var(--success-bg)!important;color:var(--success)!important;
                  font-weight:600;padding:3px 10px;border-radius:12px;font-size:11px;}
      .status-gap, .status-broken{background:var(--danger-bg)!important;color:var(--danger)!important;
                     font-weight:600;padding:3px 10px;border-radius:12px;font-size:11px;}
      .status-partial{background:var(--warning-bg)!important;color:var(--warning)!important;
                       font-weight:600;padding:3px 10px;border-radius:12px;font-size:11px;}
      .status-outdated{background:#FFEDD5!important;color:#9A3412!important;
                        font-weight:600;padding:3px 10px;border-radius:12px;font-size:11px;}
      .kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:20px;}
      .kpi-card{background:var(--white);border:1px solid var(--border);border-radius:6px;padding:20px 24px;
                position:relative;}
      .kpi-card::before{content:'';position:absolute;top:0;left:0;width:32px;height:2px;background:var(--cobalt);}
      .kpi-num{font-family:var(--fh);font-size:36px;font-weight:900;line-height:1;
               color:var(--cobalt);letter-spacing:-1px;}
      .kpi-lab{font-family:var(--fm);font-size:9px;font-weight:500;letter-spacing:2.5px;
                text-transform:uppercase;color:var(--text-3);margin-top:10px;}
      .kpi-num.danger{color:var(--danger);}
      .kpi-num.warning{color:var(--warning);}
      .kpi-num.success{color:var(--success);}
      .log-pane{background:var(--cobalt-deep);border-radius:4px;padding:16px 18px;
                font-family:var(--fm);font-size:11px;color:var(--gold);height:280px;
                overflow-y:auto;white-space:pre-wrap;line-height:1.8;}
      .health-banner{padding:18px 24px;border-radius:6px;margin-bottom:20px;
                      font-family:var(--fh);font-size:22px;font-weight:700;}
      .health-ok{background:var(--success-bg);color:var(--success);
                  border-left:4px solid var(--success);}
      .health-bad{background:var(--danger-bg);color:var(--danger);
                   border-left:4px solid var(--danger);}
      .health-none{background:var(--ivory-2);color:var(--text-3);
                    border-left:4px solid var(--text-3);}
    "))
  ),
  div(class = "hd",
      div(class = "brand",
          div(class = "logo", "Sentinel", tags$span(class = "dot", ".")),
          div(class = "sub", "Flashscore DOM watchdog")),
      div(style = "font-family:var(--fm);font-size:11px;color:rgba(250,247,242,0.8);",
          textOutput("hdr_status", inline = TRUE))
  ),
  div(class = "pg",
      tabsetPanel(id = "main_tabs",
                  
                  # ────────── GAP DETECTOR (PRIMARY) ──────────
                  tabPanel("Gap Detector",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Gap Detector"),
                               div(class = "section-sub",
                                   "One row per field being scraped. A field is RED if any sampled URL had ZERO selectors find data for it \u2014 meaning that URL has a new DOM you haven't covered. Sample URLs are shown so you can inspect."),
                               div(class = "card",
                                   div(class = "eyebrow", "Controls"),
                                   div(style = "display:flex;gap:14px;align-items:center;margin-top:12px;",
                                       actionButton("run_btn", "Run sentinel", class = "btn-primary-sentinel"),
                                       actionButton("stop_btn", "Force stop", class = "btn-outline-sentinel"),
                                       div(style = "flex:1;"),
                                       tags$span(style = "font-family:var(--fm);font-size:11px;color:var(--text-3);",
                                                 textOutput("run_status_line", inline = TRUE)))
                               ),
                               uiOutput("health_banner"),
                               uiOutput("gap_kpis"),
                               div(class = "card",
                                   div(class = "eyebrow", "Fields (grouped selectors)"),
                                   div(style = "font-family:var(--fb);font-size:12px;color:var(--text-3);margin-bottom:8px;",
                                       "Each row is a distinct thing being extracted. A GAP means at least one sample URL had no selector fire for this field."),
                                   DTOutput("gap_table")
                               ),
                               div(class = "card",
                                   div(class = "eyebrow", "Activity log"),
                                   div(class = "log-pane", textOutput("log_text"))
                               )
                           )
                  ),
                  
                  # ────────── PER-SELECTOR VIEW (LEGACY) ──────────
                  tabPanel("Per-selector view",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Per-selector view"),
                               div(class = "section-sub",
                                   "Individual selector health. OUTDATED (orange) means this selector hit nothing but a sibling for the same field did \u2014 the legacy fallback isn't needed on the sampled URLs. BROKEN (red) means neither this selector nor any sibling worked \u2014 real problem."),
                               uiOutput("selector_kpis"),
                               div(class = "card",
                                   div(class = "eyebrow", "Selector status"),
                                   DTOutput("selector_table")
                               ),
                               div(class = "card",
                                   div(class = "eyebrow", "Per-URL detail"),
                                   div(style = "font-family:var(--fb);font-size:12px;color:var(--text-3);margin-bottom:8px;",
                                       "Every selector on every URL: how many nodes it found."),
                                   DTOutput("detail_table")
                               )
                           )
                  ),
                  
                  # ────────── SELECTORS ──────────
                  tabPanel("Selectors",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Selector list"),
                               div(class = "section-sub",
                                   "Every selector MOAB uses. Edit cells inline. `field` groups alternates together \u2014 selectors sharing a field are alternates for the same job (new DOM + fallback)."),
                               div(class = "card",
                                   div(class = "eyebrow", "Actions"),
                                   div(style = "display:flex;gap:12px;align-items:center;margin-top:12px;flex-wrap:wrap;",
                                       actionButton("add_selector_btn", "Add selector", class = "btn-outline-sentinel"),
                                       actionButton("delete_selector_btn", "Delete selected", class = "btn-danger-sentinel"),
                                       actionButton("save_selectors_btn", "Save changes", class = "btn-primary-sentinel"),
                                       actionButton("reset_selectors_btn", "Reset to defaults", class = "btn-outline-sentinel"),
                                       div(style = "flex:1;"),
                                       tags$span(style = "font-family:var(--fm);font-size:11px;color:var(--text-3);",
                                                 textOutput("selectors_status", inline = TRUE)))
                               ),
                               div(class = "card",
                                   div(class = "eyebrow", "Selectors"),
                                   div(style = "font-family:var(--fb);font-size:12px;color:var(--text-3);margin-bottom:8px;",
                                       "page = which URL type. field = what's being extracted (siblings = alternates). Double-click a cell to edit."),
                                   DTOutput("selectors_edit_table")
                               )
                           )
                  ),
                  
                  # ────────── LEAGUES ──────────
                  tabPanel("Leagues",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Reference leagues"),
                               div(class = "section-sub",
                                   "Leagues sentinel samples matches from. Mix of top and lower tiers keeps checks unbiased."),
                               div(class = "card",
                                   div(class = "eyebrow", "Actions"),
                                   div(style = "display:flex;gap:12px;align-items:center;margin-top:12px;flex-wrap:wrap;",
                                       actionButton("add_league_btn", "Add league", class = "btn-outline-sentinel"),
                                       actionButton("delete_league_btn", "Delete selected", class = "btn-danger-sentinel"),
                                       actionButton("save_leagues_btn", "Save changes", class = "btn-primary-sentinel"),
                                       actionButton("reset_leagues_btn", "Reset to defaults", class = "btn-outline-sentinel"),
                                       div(style = "flex:1;"),
                                       tags$span(style = "font-family:var(--fm);font-size:11px;color:var(--text-3);",
                                                 textOutput("leagues_status", inline = TRUE)))
                               ),
                               div(class = "card",
                                   div(class = "eyebrow", "Leagues"),
                                   DTOutput("leagues_edit_table")
                               )
                           )
                  )
      )
  )
)

# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    log = "Ready.\n",
    selectors = load_selectors(),
    leagues = load_leagues(),
    results = if (file.exists(LAST_REPORT_PATH))
      tryCatch(readRDS(LAST_REPORT_PATH), error = function(e) NULL) else NULL,
    gap_report = NULL,
    selector_report = NULL,
    running = FALSE,
    status_line = "",
    selectors_status = "",
    leagues_status = ""
  )
  
  observe({
    if (!is.null(rv$results)) {
      rv$gap_report <- compute_gaps(rv$results)
      rv$selector_report <- summarize_selectors(rv$results)
    }
  })
  
  log_msg <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log <- paste0(rv$log, "[", ts, "] ", msg, "\n")
  }
  
  output$log_text <- renderText({ rv$log })
  output$run_status_line <- renderText({ rv$status_line })
  output$hdr_status <- renderText({
    n_sel <- nrow(rv$selectors)
    n_lg <- sum(rv$leagues$active == TRUE, na.rm = TRUE)
    paste0(n_sel, " selectors \u00b7 ", n_lg, " active leagues")
  })
  
  # ═══ RUN ═══
  observeEvent(input$run_btn, {
    if (rv$running) { showNotification("Already running", type = "warning"); return() }
    rv$running <- TRUE
    rv$log <- "Sentinel starting...\n"
    log_msg("Killing any existing chromedrivers")
    tryCatch(kill_chromedrivers(), error = function(e) log_msg(paste0("kill failed: ", e$message)))
    
    n_active <- sum(rv$leagues$active == TRUE, na.rm = TRUE)
    n_w <- min(N_PARALLEL_WORKERS, n_active)
    log_msg(paste0("Spawning ", n_w, " parallel workers for ", n_active, " leagues"))
    
    withProgress(message = "Sentinel running", value = 0.02, {
      progress_cb <- function(frac, msg) {
        setProgress(value = frac, detail = msg)
        rv$status_line <- msg
      }
      res <- tryCatch(run_sentinel(rv$leagues, rv$selectors, progress_cb),
                      error = function(e) { log_msg(paste0("Run FAILED: ", e$message)); NULL })
      if (!is.null(res)) {
        rv$results <- res
        rv$gap_report <- compute_gaps(res)
        rv$selector_report <- summarize_selectors(res)
        tryCatch(saveRDS(res, LAST_REPORT_PATH), error = function(e) NULL)
        n_gaps <- if (!is.null(rv$gap_report))
          sum(rv$gap_report$summary$status == "GAP") else 0
        log_msg(paste0("Done. ", nrow(rv$gap_report$summary %||% data.frame()),
                       " fields checked. ", n_gaps, " with GAPS."))
      }
    })
    
    log_msg("Cleaning up chromedrivers")
    tryCatch(kill_chromedrivers(), error = function(e) log_msg(paste0("cleanup failed: ", e$message)))
    rv$running <- FALSE
    rv$status_line <- "Done"
    showNotification("Sentinel finished", type = "message")
  })
  
  observeEvent(input$stop_btn, {
    log_msg("Force-stopping all chromedrivers")
    tryCatch(kill_chromedrivers(), error = function(e) NULL)
    rv$running <- FALSE
    rv$status_line <- "Stopped"
  })
  
  # ═══ GAP DETECTOR TAB ═══
  output$health_banner <- renderUI({
    gr <- rv$gap_report
    if (is.null(gr) || nrow(gr$summary) == 0)
      return(div(class = "health-banner health-none",
                 "No run yet. Click Run sentinel."))
    n_gaps <- sum(gr$summary$status == "GAP")
    if (n_gaps == 0)
      return(div(class = "health-banner health-ok",
                 paste0("HEALTHY \u00b7 all ", nrow(gr$summary),
                        " fields captured on every sampled URL")))
    div(class = "health-banner health-bad",
        paste0("PROBLEM DETECTED \u00b7 ", n_gaps, " field(s) with gaps \u00b7 check table below"))
  })
  
  output$gap_kpis <- renderUI({
    gr <- rv$gap_report
    if (is.null(gr) || nrow(gr$summary) == 0) return(NULL)
    s <- gr$summary
    n_fields <- nrow(s)
    n_ok <- sum(s$status == "OK")
    n_gap <- sum(s$status == "GAP")
    n_urls <- length(unique(gr$per_url$url))
    div(class = "kpi-grid",
        div(class = "kpi-card", div(class = "kpi-num", n_fields),
            div(class = "kpi-lab", "Fields checked")),
        div(class = "kpi-card", div(class = "kpi-num success", n_ok),
            div(class = "kpi-lab", "Fully captured")),
        div(class = "kpi-card", div(class = "kpi-num danger", n_gap),
            div(class = "kpi-lab", "With gaps")),
        div(class = "kpi-card", div(class = "kpi-num", n_urls),
            div(class = "kpi-lab", "URLs sampled"))
    )
  })
  
  output$gap_table <- renderDT({
    gr <- rv$gap_report
    if (is.null(gr) || nrow(gr$summary) == 0) {
      return(datatable(data.frame(Message = "Click Run sentinel to check for gaps."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    s <- gr$summary
    disp <- data.frame(
      status = paste0("<span class='status-", tolower(s$status), "'>", s$status, "</span>"),
      page = s$page_type,
      field = s$field,
      coverage = paste0(s$urls_ok, " / ", s$urls_sampled, " URLs"),
      gaps = s$urls_gap,
      sample_gap_urls = ifelse(nchar(s$sample_gap_urls) > 0,
                               paste0("<code style='font-size:10px;'>",
                                      htmltools::htmlEscape(s$sample_gap_urls), "</code>"),
                               ""),
      stringsAsFactors = FALSE
    )
    datatable(disp, escape = FALSE, filter = "top", rownames = FALSE,
              options = list(pageLength = 50, scrollX = TRUE, dom = "lftip",
                             order = list(list(0, "asc"), list(1, "asc"))))
  })
  
  # ═══ PER-SELECTOR TAB ═══
  output$selector_kpis <- renderUI({
    sr <- rv$selector_report
    if (is.null(sr) || nrow(sr) == 0) return(NULL)
    n_ok <- sum(sr$status == "OK")
    n_partial <- sum(sr$status == "PARTIAL")
    n_outdated <- sum(sr$status == "OUTDATED")
    n_broken <- sum(sr$status == "BROKEN")
    div(class = "kpi-grid",
        div(class = "kpi-card", div(class = "kpi-num success", n_ok),
            div(class = "kpi-lab", "Working")),
        div(class = "kpi-card", div(class = "kpi-num warning", n_partial),
            div(class = "kpi-lab", "Partial")),
        div(class = "kpi-card", div(class = "kpi-num", n_outdated,
                                    style = "color:#9A3412;"),
            div(class = "kpi-lab", "Outdated (fallback)")),
        div(class = "kpi-card", div(class = "kpi-num danger", n_broken),
            div(class = "kpi-lab", "Broken (real problem)"))
    )
  })
  
  output$selector_table <- renderDT({
    sr <- rv$selector_report
    if (is.null(sr) || nrow(sr) == 0) {
      return(datatable(data.frame(Message = "Click Run sentinel to populate."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    disp <- data.frame(
      status = paste0("<span class='status-", tolower(sr$status), "'>", sr$status, "</span>"),
      page = sr$page_type,
      field = sr$field,
      selector = paste0("<code>", htmltools::htmlEscape(sr$selector), "</code>"),
      purpose = sr$purpose,
      urls_ok = paste0(sr$ok_urls, " / ", sr$total_urls),
      stringsAsFactors = FALSE
    )
    datatable(disp, escape = FALSE, filter = "top", rownames = FALSE,
              options = list(pageLength = 50, scrollX = TRUE, dom = "lftip",
                             order = list(list(0, "asc"), list(1, "asc"))))
  })
  
  output$detail_table <- renderDT({
    r <- rv$results
    if (is.null(r) || nrow(r) == 0) {
      return(datatable(data.frame(Message = "Run sentinel to populate."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    r$found <- ifelse(is.na(r$n_nodes), "ERROR",
                      ifelse(r$n_nodes > 0, paste0("\u2705 ", r$n_nodes), "\u274c 0"))
    disp <- r[, c("page_type", "league", "field", "selector", "purpose", "found"),
              drop = FALSE]
    datatable(disp, filter = "top", rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE, dom = "lftip"))
  })
  
  # ═══ SELECTORS TAB ═══
  # DT pagination fix: render ONCE with isolate; NEVER re-render on cell edit.
  # Cell edits update rv$selectors silently — DT's `editable = TRUE` already
  # updates the visible cell client-side. We only proxy-replace when the
  # data changes from OUTSIDE the table (Add row / Delete row / Reset).
  output$selectors_edit_table <- renderDT({
    datatable(isolate(rv$selectors),
              editable = list(target = "cell"),
              rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE, dom = "lftip"),
              selection = "multiple")
  }, server = FALSE)
  
  selectors_proxy <- dataTableProxy("selectors_edit_table")
  
  # Only fire proxy replace when a "structural" change happens (add/delete/reset)
  rv_selectors_signal <- reactiveVal(0)
  
  observe({
    # depend on the signal, not on rv$selectors directly
    rv_selectors_signal()
    isolate({
      replaceData(selectors_proxy, rv$selectors, resetPaging = FALSE, rownames = FALSE)
    })
  })
  
  observeEvent(input$selectors_edit_table_cell_edit, {
    info <- input$selectors_edit_table_cell_edit
    # info$col is 0-indexed; with rownames=FALSE, cell (i, col+1)
    rv$selectors[info$row, info$col + 1] <- info$value
    # DO NOT trigger signal — the DT client already shows the new value.
    # If we replaced data here, pagination would reset.
  })
  
  observeEvent(input$add_selector_btn, {
    rv$selectors <- rbind(rv$selectors,
                          data.frame(page = "fixtures", selector = "new.selector",
                                     field = "unknown",
                                     purpose = "describe what this finds", stringsAsFactors = FALSE))
    rv_selectors_signal(rv_selectors_signal() + 1)
  })
  
  observeEvent(input$delete_selector_btn, {
    sel <- input$selectors_edit_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select rows to delete first", type = "warning"); return()
    }
    rv$selectors <- rv$selectors[-sel, , drop = FALSE]
    rv$selectors_status <- paste0("Removed ", length(sel), " row(s). Click Save.")
    rv_selectors_signal(rv_selectors_signal() + 1)
  })
  
  observeEvent(input$save_selectors_btn, {
    save_selectors(rv$selectors)
    rv$selectors_status <- paste0("Saved ", nrow(rv$selectors), " selectors at ",
                                  format(Sys.time(), "%H:%M:%S"))
    showNotification("Selectors saved", type = "message")
  })
  
  observeEvent(input$reset_selectors_btn, {
    showModal(modalDialog(title = "Reset to defaults?",
                          "This replaces your edited selector list with the built-in defaults.",
                          footer = tagList(modalButton("Cancel"),
                                           actionButton("reset_selectors_confirm", "Reset",
                                                        class = "btn-danger-sentinel")),
                          easyClose = TRUE))
  })
  observeEvent(input$reset_selectors_confirm, {
    rv$selectors <- default_selectors()
    save_selectors(rv$selectors)
    removeModal()
    rv$selectors_status <- "Reset to defaults"
    rv_selectors_signal(rv_selectors_signal() + 1)
    showNotification("Selectors reset", type = "message")
  })
  
  output$selectors_status <- renderText({ rv$selectors_status })
  
  # ═══ LEAGUES TAB ═══ — same pagination-safe pattern
  output$leagues_edit_table <- renderDT({
    datatable(isolate(rv$leagues),
              editable = list(target = "cell"),
              rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE, dom = "lftip"),
              selection = "multiple")
  }, server = FALSE)
  
  leagues_proxy <- dataTableProxy("leagues_edit_table")
  rv_leagues_signal <- reactiveVal(0)
  
  observe({
    rv_leagues_signal()
    isolate({
      replaceData(leagues_proxy, rv$leagues, resetPaging = FALSE, rownames = FALSE)
    })
  })
  
  observeEvent(input$leagues_edit_table_cell_edit, {
    info <- input$leagues_edit_table_cell_edit
    val <- info$value
    if (info$col + 1 == 3) val <- as.logical(val)  # active column
    rv$leagues[info$row, info$col + 1] <- val
  })
  
  observeEvent(input$add_league_btn, {
    rv$leagues <- rbind(rv$leagues,
                        data.frame(name = "New league",
                                   url = "https://www.flashscore.com/football/country/league/",
                                   active = TRUE, stringsAsFactors = FALSE))
    rv_leagues_signal(rv_leagues_signal() + 1)
  })
  
  observeEvent(input$delete_league_btn, {
    sel <- input$leagues_edit_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select rows to delete first", type = "warning"); return()
    }
    rv$leagues <- rv$leagues[-sel, , drop = FALSE]
    rv$leagues_status <- paste0("Removed ", length(sel), " row(s). Click Save.")
    rv_leagues_signal(rv_leagues_signal() + 1)
  })
  
  observeEvent(input$save_leagues_btn, {
    save_leagues(rv$leagues)
    rv$leagues_status <- paste0("Saved ", nrow(rv$leagues), " leagues at ",
                                format(Sys.time(), "%H:%M:%S"))
    showNotification("Leagues saved", type = "message")
  })
  
  observeEvent(input$reset_leagues_btn, {
    showModal(modalDialog(title = "Reset to defaults?",
                          "This replaces your edited league list with the built-in defaults.",
                          footer = tagList(modalButton("Cancel"),
                                           actionButton("reset_leagues_confirm", "Reset",
                                                        class = "btn-danger-sentinel")),
                          easyClose = TRUE))
  })
  observeEvent(input$reset_leagues_confirm, {
    rv$leagues <- default_leagues()
    save_leagues(rv$leagues)
    removeModal()
    rv$leagues_status <- "Reset to defaults"
    rv_leagues_signal(rv_leagues_signal() + 1)
    showNotification("Leagues reset", type = "message")
  })
  
  output$leagues_status <- renderText({ rv$leagues_status })
  
  session$onSessionEnded(function() {
    tryCatch(kill_chromedrivers(), error = function(e) NULL)
  })
}

shinyApp(ui, server)
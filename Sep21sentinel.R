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
      purpose = "W/D/L form icons for last games (legacy — div parent)", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "a.tableCellFormIcon div.wcl-badgeform_AKaAR",
      field = "form_icon",
      purpose = "W/D/L form icons for last games (new DOM — anchor parent)", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "div[data-testid^='wcl-badgeForm-']",
      field = "form_icon",
      purpose = "W/D/L form badge via data-testid (most stable)", stringsAsFactors = FALSE),

    # ── Match summary page (goal times + HT/FT) ─────────────
    data.frame(page = "summary", selector = "div.smv__participantRow.smv__homeParticipant",
      field = "home_event_row",
      purpose = "Home team goal/card/sub event row (legacy — div)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "li.smv__participantRow.smv__homeParticipant",
      field = "home_event_row",
      purpose = "Home team event row (new DOM — li)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.smv__participantRow.smv__awayParticipant",
      field = "away_event_row",
      purpose = "Away team goal/card/sub event row (legacy — div)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "li.smv__participantRow.smv__awayParticipant",
      field = "away_event_row",
      purpose = "Away team event row (new DOM — li)", stringsAsFactors = FALSE),
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

    # ── Match stats page ────────────────────────────────────
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics']",
      field = "stat_row",
      purpose = "One row per stat category (possession, shots, etc)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-row_2oCpS",
      field = "stat_row",
      purpose = "Stat row (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics-category'] span",
      field = "stat_name",
      purpose = "Name of each stat (legacy — pre-DOM change)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-category_6sT1J span",
      field = "stat_name",
      purpose = "Stat name (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "span[data-testid='wcl-scores-simple-text-01'].wcl-name_2lXWg",
      field = "stat_name",
      purpose = "Stat name (new DOM — small rows)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "h4[data-testid='wcl-scores-heading-04']",
      field = "stat_name",
      purpose = "Stat name (new DOM — large rows)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics-value']",
      field = "stat_value",
      purpose = "The home/away value for each stat (legacy)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-value_XJG99",
      field = "stat_value",
      purpose = "Stat value (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-value_Ywp3J",
      field = "stat_value",
      purpose = "Stat value (new DOM — home and away, in order)", stringsAsFactors = FALSE),
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
# SELENIUM — single-worker
# ════════════════════════════════════════════════════════════

SEL <- list(port = NULL, session_id = NULL)

kill_chromedrivers <- function() {
  for (i in 1:3) {
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE); Sys.sleep(0.5)
  }
  system("taskkill /F /IM chrome.exe", ignore.stdout = TRUE,
         ignore.stderr = TRUE, wait = TRUE)
  Sys.sleep(1)
}

start_selenium <- function(port = 8888) {
  kill_chromedrivers()
  system2(CHROMEDRIVER_PATH, args = paste0("--port=", port), wait = FALSE)
  Sys.sleep(2)
  response <- tryCatch(POST(
    paste0("http://localhost:", port, "/session"),
    body = list(capabilities = list(alwaysMatch = list(
      browserName = "chrome",
      `goog:chromeOptions` = list(
        binary = CHROME_PATH,
        args = list("--no-sandbox", "--disable-dev-shm-usage",
                    "--disable-blink-features=AutomationControlled",
                    "--disable-extensions"),
        excludeSwitches = list("enable-automation"),
        useAutomationExtension = FALSE)))),
    encode = "json", timeout(30)), error = function(e) NULL)
  if (is.null(response) || status_code(response) != 200)
    stop("Failed to start chromedriver on port ", port)
  sd <- fromJSON(content(response, as = "text"))
  session_id <- sd$sessionId %||% sd$value$sessionId
  SEL$port       <<- port
  SEL$session_id <<- session_id
  sel_navigate(paste0(BASE_URL, "/"))
  Sys.sleep(3)
  sel_dismiss_cookie()
  Sys.sleep(1)
}

stop_selenium <- function() {
  if (!is.null(SEL$session_id)) {
    tryCatch(DELETE(paste0("http://localhost:", SEL$port, "/session/", SEL$session_id),
                    timeout(10)), error = function(e) invisible())
  }
  kill_chromedrivers()
  SEL$port       <<- NULL
  SEL$session_id <<- NULL
}

sel_navigate <- function(url) {
  tryCatch(POST(paste0("http://localhost:", SEL$port, "/session/", SEL$session_id, "/url"),
                body = list(url = url), encode = "json", timeout(30)),
           error = function(e) NULL)
}

sel_source <- function() {
  res <- tryCatch(GET(paste0("http://localhost:", SEL$port, "/session/", SEL$session_id,
                             "/source"), timeout(30)), error = function(e) NULL)
  if (is.null(res)) return(NULL)
  html_raw <- tryCatch(fromJSON(content(res, as = "text"))$value, error = function(e) NULL)
  if (is.null(html_raw)) return(NULL)
  tryCatch(read_html(html_raw), error = function(e) NULL)
}

sel_js <- function(script_text) {
  body_json <- sprintf('{"script": %s, "args": []}',
                       toJSON(script_text, auto_unbox = TRUE))
  tryCatch({
    r <- POST(paste0("http://localhost:", SEL$port, "/session/", SEL$session_id,
                     "/execute/sync"),
              body = body_json, encode = "raw",
              add_headers(`Content-Type` = "application/json"),
              timeout(15))
    fromJSON(content(r, as = "text"))$value
  }, error = function(e) NULL)
}

sel_dismiss_cookie <- function() {
  sel_js("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
  Sys.sleep(0.5)
}

sel_wait_for_page <- function(max_wait = 20) {
  for (i in seq_len(max_wait)) {
    Sys.sleep(1)
    title <- sel_js("return document.title;")
    title <- if (is.null(title)) "" else as.character(title)[1]
    if (length(title) != 1 ||
        grepl("Just a moment|Checking", title, ignore.case = TRUE) ||
        nchar(title) == 0) next
    return(TRUE)
  }
  FALSE
}

sel_get_html <- function(url) {
  Sys.sleep(runif(1, 0.5, 1.5))
  sel_navigate(url)
  Sys.sleep(runif(1, 2, 3))
  sel_wait_for_page(20)
  sel_dismiss_cookie()
  Sys.sleep(0.5)
  sel_source()
}

sel_click_show_more_h2h <- function() {
  for (attempt in 1:2) {
    n <- sel_js(paste0(
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

# ════════════════════════════════════════════════════════════
# CHECK ENGINE
# ════════════════════════════════════════════════════════════

# For each selector on a page, count nodes returned
check_page <- function(page_html, selectors_for_page) {
  if (is.null(page_html)) {
    return(data.frame(selector = selectors_for_page$selector,
                       field    = selectors_for_page$field,
                       purpose  = selectors_for_page$purpose,
                       n_nodes  = NA_integer_, stringsAsFactors = FALSE))
  }
  out <- lapply(seq_len(nrow(selectors_for_page)), function(i) {
    sel <- selectors_for_page$selector[i]
    n <- tryCatch(length(page_html %>% html_nodes(sel)), error = function(e) NA_integer_)
    data.frame(selector = sel,
                field   = selectors_for_page$field[i],
                purpose = selectors_for_page$purpose[i],
                n_nodes = n, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Full sentinel run
run_sentinel <- function(leagues_df, selectors_df, progress_cb = NULL) {
  active_leagues <- leagues_df[isTRUE(leagues_df$active) | leagues_df$active == TRUE, , drop = FALSE]
  if (nrow(active_leagues) == 0) stop("No active leagues")

  results <- list()

  # Steps: per league: 1 results + 1 fixtures + 1 standings +
  #        N_FINISHED * 3 (summary/stats/h2h)
  n_steps <- nrow(active_leagues) * (3 + N_FINISHED_PER_LEAGUE * 3)
  step <- 0
  bump <- function(msg) {
    step <<- step + 1
    if (!is.null(progress_cb)) progress_cb(min(1, step / n_steps), msg)
  }

  for (li in seq_len(nrow(active_leagues))) {
    league_name <- active_leagues$name[li]
    league_url  <- sub("/$", "", active_leagues$url[li])

    # ── Results page (finished matches list) ────────────────
    bump(paste0(league_name, " \u00b7 results"))
    results_url <- paste0(league_url, "/results/")
    page <- sel_get_html(results_url)
    sel_res <- selectors_df[selectors_df$page == "results", , drop = FALSE]
    checks <- check_page(page, sel_res)
    checks$league <- league_name; checks$page_type <- "results"; checks$url <- results_url
    results[[length(results) + 1]] <- checks

    finished_urls <- character(0)
    if (!is.null(page)) {
      links <- page %>% html_nodes("a.eventRowLink") %>% html_attr("href")
      links <- links[!is.na(links) & nzchar(links)]
      finished_urls <- head(links, N_FINISHED_PER_LEAGUE)
    }

    # ── Fixtures page (upcoming matches list) ───────────────
    bump(paste0(league_name, " \u00b7 fixtures"))
    fixtures_url <- paste0(league_url, "/fixtures/")
    page <- sel_get_html(fixtures_url)
    sel_fx <- selectors_df[selectors_df$page == "fixtures", , drop = FALSE]
    checks <- check_page(page, sel_fx)
    checks$league <- league_name; checks$page_type <- "fixtures"; checks$url <- fixtures_url
    results[[length(results) + 1]] <- checks

    # ── Standings page ─────────────────────────────────────
    bump(paste0(league_name, " \u00b7 standings"))
    standings_url <- paste0(league_url, "/standings/")
    page <- sel_get_html(standings_url)
    sel_st <- selectors_df[selectors_df$page == "standings", , drop = FALSE]
    checks <- check_page(page, sel_st)
    checks$league <- league_name; checks$page_type <- "standings"; checks$url <- standings_url
    results[[length(results) + 1]] <- checks

    # ── Per-match pages (finished only — they have goal times,
    # stats, HT/FT to check; upcoming pages don't have that data) ─
    for (mu in finished_urls) {
      if (!startsWith(mu, "http")) mu <- paste0(BASE_URL, mu)

      # Summary page
      bump(paste0(league_name, " \u00b7 summary"))
      page <- sel_get_html(mu)
      sel_sm <- selectors_df[selectors_df$page == "summary", , drop = FALSE]
      checks <- check_page(page, sel_sm)
      checks$league <- league_name; checks$page_type <- "summary"; checks$url <- mu
      results[[length(results) + 1]] <- checks

      # Stats page — overall tab
      bump(paste0(league_name, " \u00b7 stats"))
      stats_url <- sub("(\\?mid=)", "summary/stats/overall/\\1", mu)
      page <- sel_get_html(stats_url)
      sel_ss <- selectors_df[selectors_df$page == "stats", , drop = FALSE]
      checks <- check_page(page, sel_ss)
      checks$league <- league_name; checks$page_type <- "stats"; checks$url <- stats_url
      results[[length(results) + 1]] <- checks

      # H2H page
      bump(paste0(league_name, " \u00b7 h2h"))
      h2h_url <- sub("(\\?mid=)", "h2h/overall/\\1", mu)
      sel_navigate(h2h_url)
      Sys.sleep(3)
      sel_wait_for_page(20)
      sel_dismiss_cookie()
      Sys.sleep(1)
      sel_click_show_more_h2h()
      page <- sel_source()
      sel_h2h <- selectors_df[selectors_df$page == "h2h", , drop = FALSE]
      checks <- check_page(page, sel_h2h)
      checks$league <- league_name; checks$page_type <- "h2h"; checks$url <- h2h_url
      results[[length(results) + 1]] <- checks
    }
  }

  do.call(rbind, results)
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
              "One row per field being scraped. A field is RED if any sampled URL had ZERO selectors find data for it — meaning that URL has a new DOM you haven't covered. Sample URLs are shown so you can inspect."),
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
              "Individual selector health. OUTDATED (orange) means this selector hit nothing but a sibling for the same field did — the legacy fallback isn't needed on the sampled URLs. BROKEN (red) means neither this selector nor any sibling worked — real problem."),
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
              "Every selector MOAB uses. Edit cells inline. `field` groups alternates together — selectors sharing a field are alternates for the same job (new DOM + fallback)."),
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

    log_msg("Starting Selenium")
    start_ok <- tryCatch({ start_selenium(); TRUE },
                          error = function(e) { log_msg(paste0("Selenium start FAILED: ", e$message)); FALSE })
    if (!start_ok) {
      rv$running <- FALSE
      showNotification("Failed to start Selenium", type = "error"); return()
    }
    log_msg("Selenium ready. Beginning checks.")

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

    log_msg("Stopping Selenium")
    tryCatch(stop_selenium(), error = function(e) log_msg(paste0("stop failed: ", e$message)))
    rv$running <- FALSE
    rv$status_line <- "Done"
    showNotification("Sentinel finished", type = "message")
  })

  observeEvent(input$stop_btn, {
    log_msg("Force-stopping Selenium")
    tryCatch(stop_selenium(), error = function(e) NULL)
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
                        ifelse(r$n_nodes > 0, paste0("\u2705 ", r$n_nodes), "\u274C 0"))
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
    tryCatch(stop_selenium(), error = function(e) NULL)
  })
}

shinyApp(ui, server)

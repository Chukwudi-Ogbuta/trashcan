# ============================================================
# Sentinel — Flashscore DOM change detector
#
# Loads the same Selenium setup as MOAB, hits a handful of matches
# across 6 leagues, and checks every scrape selector still returns
# nodes. Reports per-selector red/green across leagues so a single
# odd match doesn't cause a false alarm.
#
# UI has three tabs:
#   - Run              : the actual check
#   - Selectors        : editable list of what's being checked
#   - Leagues          : editable list of league base URLs
#
# Both editable lists persist to disk between sessions.
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

# Same paths as moab.R — will fall back to first found if wrong
CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
BASE_URL          <- "https://www.flashscore.com"

N_MATCHES_PER_LEAGUE <- 2  # how many finished matches per league to click into

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a) || is.list(a)) return(a)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# DEFAULT SELECTORS — every selector moab.R touches
# Grouped by page type. Each row: selector, purpose.
# ════════════════════════════════════════════════════════════

default_selectors <- function() {
  rbind(
    # ── Fixtures page (upcoming matches) ────────────────────
    data.frame(page = "fixtures", selector = "div.event__match",
      purpose = "One row per upcoming match", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "span.event__stageTime",
      purpose = "Kickoff date and time", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "div.event__time",
      purpose = "Kickoff time (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "div.event__homeParticipant img",
      purpose = "Home team name (from logo alt)", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "div.event__awayParticipant img",
      purpose = "Away team name (from logo alt)", stringsAsFactors = FALSE),
    data.frame(page = "fixtures", selector = "a.eventRowLink",
      purpose = "Clickable link to each match", stringsAsFactors = FALSE),

    # ── Results page (finished matches) — same DOM as fixtures ─
    data.frame(page = "results", selector = "div.event__match",
      purpose = "One row per finished match", stringsAsFactors = FALSE),
    data.frame(page = "results", selector = "a.eventRowLink",
      purpose = "Link to each finished match (used to click through)", stringsAsFactors = FALSE),

    # ── Standings page ──────────────────────────────────────
    data.frame(page = "standings", selector = "div.ui-table__row",
      purpose = "One row per team in table", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "div.tableCellRank",
      purpose = "Team's league position", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "a.tableCellParticipant__name",
      purpose = "Team name in table", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "span.table__cell--value",
      purpose = "MP / W / D / L / GF:GA / GD / Pts numbers", stringsAsFactors = FALSE),
    data.frame(page = "standings", selector = "div.tableCellFormIcon div.wcl-badgeform_AKaAR",
      purpose = "W/D/L form icons for last games", stringsAsFactors = FALSE),

    # ── Match summary page (goal times + HT/FT) ─────────────
    data.frame(page = "summary", selector = "div.smv__participantRow.smv__homeParticipant",
      purpose = "Home team goal/card/sub event row", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.smv__participantRow.smv__awayParticipant",
      purpose = "Away team goal/card/sub event row", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "li.smv__participantRow.smv__homeParticipant",
      purpose = "Home team event row (new DOM — try both)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "li.smv__participantRow.smv__awayParticipant",
      purpose = "Away team event row (new DOM — try both)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.smv__incidentIcon",
      purpose = "Wrapper around the goal/card/sub icon", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "svg[data-testid='wcl-icon-incidents-goal-soccer']",
      purpose = "The goal ball icon (used to filter goal events only)", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "div.smv__timeBox",
      purpose = "The minute a goal happened (e.g. 45+2')", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "[data-testid='wcl-headerSection-text']",
      purpose = "HT / 2H mini-score blocks", stringsAsFactors = FALSE),
    data.frame(page = "summary", selector = "span[data-testid='wcl-scores-overline-02']",
      purpose = "Half label and score inside the mini-blocks", stringsAsFactors = FALSE),

    # ── Match stats page ────────────────────────────────────
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics']",
      purpose = "One row per stat category (possession, shots, etc)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-row_2oCpS",
      purpose = "Stat row (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics-category'] span",
      purpose = "Name of each stat (Possession, Total shots, etc)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-category_6sT1J span",
      purpose = "Stat name (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div[data-testid='wcl-statistics-value']",
      purpose = "The home/away value for each stat", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.wcl-value_XJG99",
      purpose = "Stat value (legacy fallback)", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "div.detailScore__wrapper",
      purpose = "Final score header on the stats page", stringsAsFactors = FALSE),
    data.frame(page = "stats", selector = "span.detailScore__matchResult",
      purpose = "Final score (alternate header)", stringsAsFactors = FALSE),

    # ── H2H page ────────────────────────────────────────────
    data.frame(page = "h2h", selector = "div.h2h__section",
      purpose = "One section for home form, away form, and past H2H", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.wcl-scores-overline-02_bpqU7",
      purpose = "Section title (Home form / Away form / H2H)", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "a.h2h__row",
      purpose = "One past-match row inside a section", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__date",
      purpose = "Date of that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span[data-testid='wcl-stageTime']",
      purpose = "Date of that past match (new DOM fallback)", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__event",
      purpose = "Competition name for that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "div.h2h__homeParticipant img",
      purpose = "Home team of that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "div.h2h__awayParticipant img",
      purpose = "Away team of that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__result > span[data-testid='wcl-tableScore']",
      purpose = "Score of that past match", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "span.h2h__result > span",
      purpose = "Score of that past match (fallback)", stringsAsFactors = FALSE),
    data.frame(page = "h2h", selector = "button.wclButtonLink--h2h",
      purpose = "'Show more' button (clicked via JS to expand H2H list)", stringsAsFactors = FALSE)
  )
}

default_leagues <- function() {
  data.frame(
    name = c("Eredivisie", "La Liga", "Premier League",
             "Serie A", "Belgian Pro League", "MLS"),
    url  = c("https://www.flashscore.com/football/netherlands/eredivisie/",
             "https://www.flashscore.com/football/spain/laliga/",
             "https://www.flashscore.com/football/england/premier-league/",
             "https://www.flashscore.com/football/italy/serie-a/",
             "https://www.flashscore.com/football/belgium/jupiler-pro-league/",
             "https://www.flashscore.com/football/usa/mls/"),
    active = TRUE, stringsAsFactors = FALSE
  )
}

load_selectors <- function() {
  if (!file.exists(SELECTORS_PATH)) return(default_selectors())
  tryCatch(readRDS(SELECTORS_PATH), error = function(e) default_selectors())
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
# SELENIUM — mirrors moab.R exactly, single-worker (one Chrome
# window, N tabs unnecessary since sentinel is small)
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
  # Warm up on flashscore homepage and dismiss cookie
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

# Discover finished match URLs from a league's results page
discover_match_urls <- function(league_url, n = N_MATCHES_PER_LEAGUE) {
  results_url <- paste0(sub("/$", "", league_url), "/results/")
  page <- sel_get_html(results_url)
  if (is.null(page)) return(character(0))
  links <- page %>% html_nodes("a.eventRowLink") %>% html_attr("href")
  links <- links[!is.na(links) & nzchar(links)]
  head(links, n)
}

# For each selector on a page, count nodes returned
check_page <- function(page_html, selectors_for_page) {
  if (is.null(page_html)) {
    return(data.frame(selector = selectors_for_page$selector,
                       purpose  = selectors_for_page$purpose,
                       n_nodes  = NA_integer_, stringsAsFactors = FALSE))
  }
  out <- lapply(seq_len(nrow(selectors_for_page)), function(i) {
    sel <- selectors_for_page$selector[i]
    n <- tryCatch(length(page_html %>% html_nodes(sel)), error = function(e) NA_integer_)
    data.frame(selector = sel, purpose = selectors_for_page$purpose[i],
                n_nodes = n, stringsAsFactors = FALSE)
  })
  do.call(rbind, out)
}

# Full sentinel run
run_sentinel <- function(leagues_df, selectors_df, progress_cb = NULL) {
  active_leagues <- leagues_df[isTRUE(leagues_df$active) | leagues_df$active == TRUE, , drop = FALSE]
  if (nrow(active_leagues) == 0) stop("No active leagues")

  # results: page × selector × league -> n_nodes
  results <- list()

  page_types <- unique(selectors_df$page)
  n_steps <- nrow(active_leagues) * (2 + N_MATCHES_PER_LEAGUE * 3)  # results+fixtures+standings + each match(summary+stats+h2h)
  step <- 0
  bump <- function(msg) {
    step <<- step + 1
    if (!is.null(progress_cb)) progress_cb(step / n_steps, msg)
  }

  for (li in seq_len(nrow(active_leagues))) {
    league_name <- active_leagues$name[li]
    league_url  <- sub("/$", "", active_leagues$url[li])

    # ── Results page (also gives us match URLs) ────────────
    bump(paste0(league_name, " · results"))
    results_url <- paste0(league_url, "/results/")
    page <- sel_get_html(results_url)
    sel_res <- selectors_df[selectors_df$page == "results", , drop = FALSE]
    checks <- check_page(page, sel_res)
    checks$league <- league_name; checks$page_type <- "results"; checks$url <- results_url
    results[[length(results) + 1]] <- checks

    # Grab match URLs while we have the page
    match_urls <- character(0)
    if (!is.null(page)) {
      links <- page %>% html_nodes("a.eventRowLink") %>% html_attr("href")
      links <- links[!is.na(links) & nzchar(links)]
      match_urls <- head(links, N_MATCHES_PER_LEAGUE)
    }

    # ── Fixtures page ──────────────────────────────────────
    bump(paste0(league_name, " · fixtures"))
    fixtures_url <- paste0(league_url, "/fixtures/")
    page <- sel_get_html(fixtures_url)
    sel_fx <- selectors_df[selectors_df$page == "fixtures", , drop = FALSE]
    checks <- check_page(page, sel_fx)
    checks$league <- league_name; checks$page_type <- "fixtures"; checks$url <- fixtures_url
    results[[length(results) + 1]] <- checks

    # ── Standings page ─────────────────────────────────────
    standings_url <- paste0(league_url, "/standings/")
    page <- sel_get_html(standings_url)
    sel_st <- selectors_df[selectors_df$page == "standings", , drop = FALSE]
    checks <- check_page(page, sel_st)
    checks$league <- league_name; checks$page_type <- "standings"; checks$url <- standings_url
    results[[length(results) + 1]] <- checks

    # ── Per-match pages ────────────────────────────────────
    for (mu in match_urls) {
      # Normalize URL — hrefs from flashscore can be relative or absolute
      if (!startsWith(mu, "http")) mu <- paste0(BASE_URL, mu)

      # Summary page
      bump(paste0(league_name, " · summary"))
      page <- sel_get_html(mu)
      sel_sm <- selectors_df[selectors_df$page == "summary", , drop = FALSE]
      checks <- check_page(page, sel_sm)
      checks$league <- league_name; checks$page_type <- "summary"; checks$url <- mu
      results[[length(results) + 1]] <- checks

      # Stats page — overall tab
      bump(paste0(league_name, " · stats"))
      stats_url <- sub("(\\?mid=)", "summary/stats/overall/\\1", mu)
      page <- sel_get_html(stats_url)
      sel_st <- selectors_df[selectors_df$page == "stats", , drop = FALSE]
      checks <- check_page(page, sel_st)
      checks$league <- league_name; checks$page_type <- "stats"; checks$url <- stats_url
      results[[length(results) + 1]] <- checks

      # H2H page
      bump(paste0(league_name, " · h2h"))
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

# Summarize results: per selector, count leagues where n_nodes > 0
summarize_results <- function(results) {
  if (is.null(results) || nrow(results) == 0) return(NULL)
  # Per (page_type, selector), aggregate across leagues+urls
  agg <- aggregate(n_nodes ~ page_type + selector + purpose, data = results,
                    FUN = function(x) c(ok = sum(x > 0, na.rm = TRUE),
                                          n  = length(x),
                                          na = sum(is.na(x))))
  # aggregate returns a matrix in n_nodes column — split it out
  agg <- data.frame(
    page_type = agg$page_type,
    selector  = agg$selector,
    purpose   = agg$purpose,
    ok_urls   = agg$n_nodes[, "ok"],
    total_urls= agg$n_nodes[, "n"],
    na_urls   = agg$n_nodes[, "na"],
    stringsAsFactors = FALSE
  )
  agg$status <- ifelse(agg$ok_urls == agg$total_urls, "OK",
                ifelse(agg$ok_urls == 0, "BROKEN",
                ifelse(agg$ok_urls >= agg$total_urls - 1, "OK (1 miss)",
                                                               "PARTIAL")))
  agg[order(agg$page_type, agg$status != "BROKEN", -agg$ok_urls), , drop = FALSE]
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
      .pg{max-width:1400px;margin:0 auto;padding:32px 36px;}
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
      .status-broken{background:var(--danger-bg)!important;color:var(--danger)!important;
                     font-weight:600;padding:3px 10px;border-radius:12px;font-size:11px;}
      .status-partial{background:var(--warning-bg)!important;color:var(--warning)!important;
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

      # ────────── RUN ──────────
      tabPanel("Run",
        div(style = "padding-top:8px;",
          div(class = "section-lab", "Run check"),
          div(class = "section-sub",
              "Loads each active league's results, fixtures, standings, plus 1–3 finished matches, and checks every selector still finds nodes."),
          div(class = "card",
            div(class = "eyebrow", "Controls"),
            div(style = "display:flex;gap:14px;align-items:center;margin-top:12px;",
              actionButton("run_btn", "Run sentinel", class = "btn-primary-sentinel"),
              actionButton("stop_btn", "Force stop", class = "btn-outline-sentinel"),
              div(style = "flex:1;"),
              tags$span(style = "font-family:var(--fm);font-size:11px;color:var(--text-3);",
                        textOutput("run_status_line", inline = TRUE)))
          ),
          uiOutput("summary_kpis"),
          div(class = "card",
            div(class = "eyebrow", "Selector status"),
            DTOutput("summary_table")
          ),
          div(class = "card",
            div(class = "eyebrow", "Per-league detail"),
            div(style = "font-family:var(--fb);font-size:12px;color:var(--text-3);margin-bottom:8px;",
                "Shows nodes returned by each selector on each league. Use this to spot single-league flukes."),
            DTOutput("detail_table")
          ),
          div(class = "card",
            div(class = "eyebrow", "Activity log"),
            div(class = "log-pane", textOutput("log_text"))
          )
        )
      ),

      # ────────── SELECTORS ──────────
      tabPanel("Selectors",
        div(style = "padding-top:8px;",
          div(class = "section-lab", "Current selectors"),
          div(class = "section-sub",
              "This is the list of every selector MOAB scrapes. Edit any cell inline. Add a row for a new selector. Delete rows you no longer scrape."),
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
                "page = which URL type this selector belongs to. Double-click a cell to edit."),
            DTOutput("selectors_edit_table")
          )
        )
      ),

      # ────────── LEAGUES ──────────
      tabPanel("Leagues",
        div(style = "padding-top:8px;",
          div(class = "section-lab", "Reference leagues"),
          div(class = "section-sub",
              "The leagues sentinel uses to sample matches from. Uncheck 'active' to skip a league without deleting it."),
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
    summary = NULL,
    running = FALSE,
    status_line = "",
    selectors_status = "",
    leagues_status = ""
  )

  # Rebuild summary from results on startup
  observe({
    if (!is.null(rv$results)) rv$summary <- summarize_results(rv$results)
  })

  log_msg <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log <- paste0(rv$log, "[", ts, "] ", msg, "\n")
  }

  output$log_text <- renderText({ rv$log })
  output$run_status_line <- renderText({ rv$status_line })
  output$hdr_status <- renderText({
    n_sel <- nrow(rv$selectors); n_lg <- sum(rv$leagues$active == TRUE, na.rm = TRUE)
    paste0(n_sel, " selectors \u00b7 ", n_lg, " active leagues")
  })

  # ═══ RUN TAB ═══
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
      showNotification("Failed to start Selenium — check chromedriver path in Settings", type = "error")
      return()
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
        rv$summary <- summarize_results(res)
        tryCatch(saveRDS(res, LAST_REPORT_PATH), error = function(e) NULL)
        n_broken <- sum(rv$summary$status == "BROKEN")
        log_msg(paste0("Done. ", nrow(rv$summary), " unique selectors checked. ",
                        n_broken, " BROKEN."))
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

  output$summary_kpis <- renderUI({
    s <- rv$summary
    if (is.null(s) || nrow(s) == 0) {
      return(div(class = "kpi-grid",
        div(class = "kpi-card", div(class = "kpi-num", "\u2014"),
            div(class = "kpi-lab", "No run yet"))))
    }
    n_ok <- sum(s$status == "OK")
    n_partial <- sum(s$status %in% c("OK (1 miss)", "PARTIAL"))
    n_broken <- sum(s$status == "BROKEN")
    div(class = "kpi-grid",
      div(class = "kpi-card", div(class = "kpi-num success", n_ok),
          div(class = "kpi-lab", "Selectors working")),
      div(class = "kpi-card", div(class = "kpi-num warning", n_partial),
          div(class = "kpi-lab", "Partial / 1 miss")),
      div(class = "kpi-card", div(class = "kpi-num danger", n_broken),
          div(class = "kpi-lab", "Broken")),
      div(class = "kpi-card", div(class = "kpi-num", nrow(s)),
          div(class = "kpi-lab", "Selectors checked"))
    )
  })

  output$summary_table <- renderDT({
    s <- rv$summary
    if (is.null(s) || nrow(s) == 0) {
      return(datatable(data.frame(Message = "Click Run sentinel to check selectors."),
                        options = list(dom = "t"), rownames = FALSE))
    }
    disp <- data.frame(
      status = paste0("<span class='status-", tolower(gsub("[^a-z]", "", tolower(s$status))), "'>",
                       s$status, "</span>"),
      page = s$page_type,
      selector = paste0("<code>", htmltools::htmlEscape(s$selector), "</code>"),
      purpose = s$purpose,
      leagues_ok = paste0(s$ok_urls, " / ", s$total_urls),
      stringsAsFactors = FALSE
    )
    datatable(disp, escape = FALSE, filter = "top", rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE, dom = "lftip",
                              order = list(list(0, 'asc'), list(1, 'asc'))))
  })

  output$detail_table <- renderDT({
    r <- rv$results
    if (is.null(r) || nrow(r) == 0) {
      return(datatable(data.frame(Message = "Run sentinel to populate."),
                        options = list(dom = "t"), rownames = FALSE))
    }
    r$found <- ifelse(is.na(r$n_nodes), "ERROR",
                        ifelse(r$n_nodes > 0, paste0("\u2705 ", r$n_nodes), "\u274C 0"))
    disp <- r[, c("page_type", "league", "selector", "purpose", "found"),
                drop = FALSE]
    datatable(disp, filter = "top", rownames = FALSE,
              options = list(pageLength = 25, scrollX = TRUE, dom = "lftip"))
  })

  # ═══ SELECTORS TAB ═══
  output$selectors_edit_table <- renderDT({
    datatable(isolate(rv$selectors), editable = list(target = "cell"),
              rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE, dom = "lftip"),
              selection = "multiple")
  }, server = FALSE)
  
  selectors_proxy <- dataTableProxy("selectors_edit_table")
  
  observe({
    replaceData(selectors_proxy, rv$selectors, resetPaging = FALSE, rownames = FALSE)
  })

  observeEvent(input$selectors_edit_table_cell_edit, {
    info <- input$selectors_edit_table_cell_edit
    rv$selectors[info$row, info$col + 1] <- info$value  # +1 because rownames=FALSE
  })

  observeEvent(input$add_selector_btn, {
    rv$selectors <- rbind(rv$selectors,
      data.frame(page = "fixtures", selector = "new.selector",
                  purpose = "describe what this finds", stringsAsFactors = FALSE))
  })

  observeEvent(input$delete_selector_btn, {
    sel <- input$selectors_edit_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select rows to delete first", type = "warning"); return()
    }
    rv$selectors <- rv$selectors[-sel, , drop = FALSE]
    rv$selectors_status <- paste0("Removed ", length(sel), " row(s). Click Save.")
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
    showNotification("Selectors reset", type = "message")
  })

  output$selectors_status <- renderText({ rv$selectors_status })

  # ═══ LEAGUES TAB ═══
  output$leagues_edit_table <- renderDT({
    datatable(isolate(rv$leagues), editable = list(target = "cell"),
              rownames = FALSE, filter = "top",
              options = list(pageLength = 10, scrollX = TRUE, dom = "lftip"),
              selection = "multiple")
  }, server = FALSE)
  
  leagues_proxy <- dataTableProxy("leagues_edit_table")
  
  observe({
    replaceData(leagues_proxy, rv$leagues, resetPaging = FALSE, rownames = FALSE)
  })

  observeEvent(input$leagues_edit_table_cell_edit, {
    info <- input$leagues_edit_table_cell_edit
    val <- info$value
    # "active" column is logical
    if (info$col + 1 == 3) val <- as.logical(val)
    rv$leagues[info$row, info$col + 1] <- val
  })

  observeEvent(input$add_league_btn, {
    rv$leagues <- rbind(rv$leagues,
      data.frame(name = "New league",
                  url = "https://www.flashscore.com/football/country/league/",
                  active = TRUE, stringsAsFactors = FALSE))
  })

  observeEvent(input$delete_league_btn, {
    sel <- input$leagues_edit_table_rows_selected
    if (length(sel) == 0) {
      showNotification("Select rows to delete first", type = "warning"); return()
    }
    rv$leagues <- rv$leagues[-sel, , drop = FALSE]
    rv$leagues_status <- paste0("Removed ", length(sel), " row(s). Click Save.")
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
    showNotification("Leagues reset", type = "message")
  })

  output$leagues_status <- renderText({ rv$leagues_status })

  # Cleanup on session end
  session$onSessionEnded(function() {
    tryCatch(stop_selenium(), error = function(e) NULL)
  })
}

shinyApp(ui, server)

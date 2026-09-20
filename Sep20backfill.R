# ============================================================
# Backfill — Goal-times recovery
#
# Reads uploaded RDS files (R1/R2/R3, enriched_fixtures, cache),
# scans every fixture/form/H2H entry for missing goal_times,
# dedupes into a queue of unique match URLs, spins up 6 Chrome
# tabs to re-scrape those URLs (goal times + HT + FT), then
# patches the results into every source that referenced them.
#
# Fixed outputs land in ./backfilled/ next to the app — originals
# stay untouched.
#
# Uses the DOM-fixed selector: li.smv__participantRow.*
# with div.smv__participantRow.* as legacy fallback.
# ============================================================

library(shiny)
library(DT)
library(rvest)
library(httr)
library(jsonlite)

# ════════════════════════════════════════════════════════════
# CONFIG — mirrors MOAB
# ════════════════════════════════════════════════════════════

BASE_PATH         <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
BACKFILL_DIR      <- file.path(getwd(), "backfilled")
PROGRESS_DIR      <- file.path(getwd(), "backfill_progress")
UPLOADS_DIR       <- file.path(getwd(), "backfill_uploads")
if (!dir.exists(BACKFILL_DIR)) dir.create(BACKFILL_DIR, recursive = TRUE)
if (!dir.exists(PROGRESS_DIR)) dir.create(PROGRESS_DIR, recursive = TRUE)
if (!dir.exists(UPLOADS_DIR))  dir.create(UPLOADS_DIR,  recursive = TRUE)

CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
BASE_URL          <- "https://www.flashscore.com"
SB_HELPERS_PATH   <- file.path(BASE_PATH, "sportybet_helpers.R")
NO_GT_PATH        <- file.path(getwd(), "no_goaltimes_leagues.rds")

N_WORKERS         <- 6   # 6 Chrome windows for parallel scraping

# Source SportyBet helpers (has filter_to_sportybet_df / fetch_sportybet_directory)
if (file.exists(SB_HELPERS_PATH)) {
  source(SB_HELPERS_PATH, local = FALSE)
} else {
  filter_to_sportybet_df <- function(df, sb_dir = NULL, ...) list(df = df, matched = rep(TRUE, nrow(df)))
  fetch_sportybet_directory <- function() NULL
}

# Load persisted "leagues that never had goal times" set
load_no_gt_leagues <- function() {
  if (!file.exists(NO_GT_PATH)) return(character())
  tryCatch(as.character(readRDS(NO_GT_PATH)), error = function(e) character())
}
save_no_gt_leagues <- function(leagues) {
  tryCatch(saveRDS(unique(as.character(leagues)), NO_GT_PATH), error = function(e) NULL)
}

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a) || is.list(a)) return(a)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# GOAL-TIMES DETECTION
# ════════════════════════════════════════════════════════════

# Is a value "empty" in the goal-times sense?
gt_empty <- function(v) {
  if (is.null(v)) return(TRUE)
  if (length(v) == 0) return(TRUE)
  if (is.list(v) && length(v) == 0) return(TRUE)
  FALSE
}

# For a form/h2h entry: needs backfill if BOTH concerned AND opponent are empty
entry_needs_backfill <- function(m) {
  if (!is.list(m)) return(FALSE)
  # Must have a match_url to be scrapable
  mu <- m$match_url
  if (is.null(mu) || length(mu) == 0 || is.na(mu[1]) || !nzchar(mu[1])) return(FALSE)
  gt_empty(m$goal_times_concerned) && gt_empty(m$goal_times_opponent)
}

# For a fixture-level result row: needs backfill if BOTH sides empty AND FINAL
fixture_needs_backfill <- function(fx) {
  if (is.null(fx) || nrow(fx) == 0) return(FALSE)
  status <- as.character(fx$result_status[1] %||% "")
  if (status != "FINAL") return(FALSE)
  mu <- fx$match_url[1]
  if (is.null(mu) || is.na(mu) || !nzchar(mu)) return(FALSE)
  gth <- fx$result_goal_times_home[[1]]
  gta <- fx$result_goal_times_away[[1]]
  gt_empty(gth) && gt_empty(gta)
}

# Scan one source (list of enriched fixtures) and collect entries needing backfill
scan_source <- function(enriched, source_name) {
  needs <- list()  # each element: list(match_id, match_url, where = list of {src, fx_idx, kind, form_key/pos})
  for (fx_idx in seq_along(enriched)) {
    ef <- enriched[[fx_idx]]
    if (is.null(ef)) next

    # Fixture-level result page (goal_times_home / goal_times_away)
    if (!is.null(ef$fixture) && fixture_needs_backfill(ef$fixture)) {
      mid <- as.character(ef$fixture$match_id[1] %||% NA)
      mu  <- as.character(ef$fixture$match_url[1] %||% NA)
      if (!is.na(mid) && !is.na(mu)) {
        needs[[length(needs) + 1]] <- list(
          match_id = mid, match_url = mu,
          where = list(src = source_name, fx_idx = fx_idx, kind = "fixture")
        )
      }
    }

    # Form entries
    for (fk in c("home_form", "away_form")) {
      form <- ef[[fk]] %||% list()
      for (pos in seq_along(form)) {
        m <- form[[pos]]
        if (!entry_needs_backfill(m)) next
        mid <- as.character(m$match_id %||% NA)
        mu  <- as.character(m$match_url %||% NA)
        if (is.na(mid) || is.na(mu)) next
        needs[[length(needs) + 1]] <- list(
          match_id = mid, match_url = mu,
          where = list(src = source_name, fx_idx = fx_idx,
                        kind = "form", form_key = fk, pos = pos)
        )
      }
    }

    # H2H entries
    h2h <- ef$h2h %||% list()
    for (pos in seq_along(h2h)) {
      m <- h2h[[pos]]
      if (!entry_needs_backfill(m)) next
      mid <- as.character(m$match_id %||% NA)
      mu  <- as.character(m$match_url %||% NA)
      if (is.na(mid) || is.na(mu)) next
      needs[[length(needs) + 1]] <- list(
        match_id = mid, match_url = mu,
        where = list(src = source_name, fx_idx = fx_idx,
                      kind = "h2h", pos = pos)
      )
    }
  }
  needs
}

# Take the flat list of needs and build a deduplicated match-URL queue,
# each queue item carrying a list of "patch sites" that reference it.
build_queue <- function(all_needs) {
  by_id <- list()
  for (n in all_needs) {
    key <- n$match_id
    if (is.null(by_id[[key]])) {
      by_id[[key]] <- list(match_id = key, match_url = n$match_url, sites = list())
    }
    by_id[[key]]$sites[[length(by_id[[key]]$sites) + 1]] <- n$where
  }
  # Return as list, sorted by number of sites descending (biggest wins first)
  q <- unname(by_id)
  q[order(-vapply(q, function(x) length(x$sites), integer(1)))]
}

# ════════════════════════════════════════════════════════════
# LEAGUE-LEVEL "NEVER HAD GOAL TIMES" DETECTION (from cache)
# ════════════════════════════════════════════════════════════
# For each league seen in cache, check if ANY match ever had goal times.
# Leagues with 20+ matches and 0% goal-time coverage are treated as
# "never had" and skipped. Cache doesn't store league name directly on
# each record, so we cross-reference via match_url (skip if URL-based
# league detection isn't possible — fall back to only-persisted list).

# Build a league->coverage map from an enriched fixtures list (which
# DOES carry league on ef$fixture). Any fixture whose league already
# has coverage information keeps it aggregated.
detect_leagues_never_had_gt <- function(enriched_list, min_matches = 20) {
  if (length(enriched_list) == 0) return(character())
  # league -> list(total = int, with_gt = int)
  agg <- list()
  for (ef in enriched_list) {
    if (is.null(ef$fixture) || nrow(ef$fixture) == 0) next
    lg <- as.character(ef$fixture$country[1] %||% "")
    lg2 <- as.character(ef$fixture$league[1] %||% "")
    league_key <- paste0(lg, " :: ", lg2)
    if (!nzchar(lg2)) next
    # Count fixture-level goal times
    a <- agg[[league_key]] %||% list(total = 0L, with_gt = 0L)
    if (identical(as.character(ef$fixture$result_status[1] %||% ""), "FINAL")) {
      a$total <- a$total + 1L
      gth <- ef$fixture$result_goal_times_home[[1]]
      gta <- ef$fixture$result_goal_times_away[[1]]
      if (!gt_empty(gth) || !gt_empty(gta)) a$with_gt <- a$with_gt + 1L
    }
    agg[[league_key]] <- a
  }
  # Flag leagues with 20+ matches and 0 goal times
  bad <- character()
  for (k in names(agg)) {
    if (agg[[k]]$total >= min_matches && agg[[k]]$with_gt == 0L) {
      bad <- c(bad, k)
    }
  }
  bad
}


# ════════════════════════════════════════════════════════════
# SELENIUM POOL — mirrors MOAB structure
# ════════════════════════════════════════════════════════════

SEL_POOL <- list()

kill_chromedrivers <- function() {
  for (i in 1:3) {
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE); Sys.sleep(0.5)
  }
  system("taskkill /F /IM chrome.exe", ignore.stdout = TRUE,
         ignore.stderr = TRUE, wait = TRUE)
  Sys.sleep(1)
}

find_free_ports <- function(n) {
  bases <- c(8888, 8889, 7777, 7778, 6666, 5555, 9876, 9877, 9878, 9879)
  ports <- head(bases, n)
  while (length(ports) < n) {
    p <- sample(20000:60000, 1)
    if (!p %in% ports) ports <- c(ports, p)
  }
  ports
}

start_one_worker <- function(port) {
  system2(CHROMEDRIVER_PATH, args = paste0("--port=", port), wait = FALSE)
  Sys.sleep(2)
  resp <- tryCatch(POST(
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
  if (is.null(resp) || status_code(resp) != 200) stop("Worker start failed on port ", port)
  sd <- fromJSON(content(resp, as = "text"))
  session_id <- sd$sessionId %||% sd$value$sessionId
  r <- GET(paste0("http://localhost:", port, "/session/", session_id, "/window/handles"))
  handles <- fromJSON(content(r, as = "text"))$value
  list(port = port, session_id = session_id, handle = handles[1], busy = FALSE)
}

start_pool <- function() {
  kill_chromedrivers()
  ports <- find_free_ports(N_WORKERS)
  SEL_POOL <<- list()
  for (i in seq_along(ports)) {
    w <- tryCatch(start_one_worker(ports[i]), error = function(e) {
      message("Worker ", i, " failed: ", e$message); NULL
    })
    if (!is.null(w)) SEL_POOL[[length(SEL_POOL) + 1]] <<- w
    Sys.sleep(1)
  }
  if (length(SEL_POOL) == 0) stop("All workers failed to start")
  # Warm up all workers
  for (i in seq_along(SEL_POOL)) {
    w <- SEL_POOL[[i]]
    worker_navigate(w, paste0(BASE_URL, "/"))
    Sys.sleep(2)
    worker_dismiss_cookie(w)
  }
}

stop_pool <- function() {
  for (w in SEL_POOL) {
    tryCatch(DELETE(paste0("http://localhost:", w$port, "/session/", w$session_id),
                    timeout(10)), error = function(e) invisible())
  }
  kill_chromedrivers()
  SEL_POOL <<- list()
}

worker_navigate <- function(w, url) {
  tryCatch(POST(paste0("http://localhost:", w$port, "/session/", w$session_id, "/url"),
                body = list(url = url), encode = "json", timeout(30)),
           error = function(e) NULL)
}
worker_source <- function(w) {
  res <- tryCatch(GET(paste0("http://localhost:", w$port, "/session/", w$session_id,
                              "/source"), timeout(30)), error = function(e) NULL)
  if (is.null(res)) return(NULL)
  html_raw <- tryCatch(fromJSON(content(res, as = "text"))$value, error = function(e) NULL)
  if (is.null(html_raw)) return(NULL)
  tryCatch(read_html(html_raw), error = function(e) NULL)
}
worker_js <- function(w, script_text) {
  body_json <- sprintf('{"script": %s, "args": []}', toJSON(script_text, auto_unbox = TRUE))
  tryCatch({
    r <- POST(paste0("http://localhost:", w$port, "/session/", w$session_id, "/execute/sync"),
              body = body_json, encode = "raw",
              add_headers(`Content-Type` = "application/json"),
              timeout(15))
    fromJSON(content(r, as = "text"))$value
  }, error = function(e) NULL)
}
worker_dismiss_cookie <- function(w) {
  worker_js(w, "var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
  Sys.sleep(0.5)
}
worker_wait_for_page <- function(w, max_wait = 20) {
  for (i in seq_len(max_wait)) {
    Sys.sleep(1)
    title <- worker_js(w, "return document.title;")
    title <- if (is.null(title)) "" else as.character(title)[1]
    if (length(title) != 1 || grepl("Just a moment|Checking", title, ignore.case = TRUE) ||
        nchar(title) == 0) next
    is_match <- worker_js(w, "return window.location.pathname.startsWith('/match/');")
    if (isTRUE(is_match)) {
      score_txt <- worker_js(w, paste0(
        "var el = document.querySelector('div.detailScore__wrapper') || ",
        "         document.querySelector('span.detailScore__matchResult');",
        "return el ? el.innerText : '';"))
      score_txt <- if (is.null(score_txt)) "" else as.character(score_txt)[1]
      if (grepl("\\d", score_txt)) return(TRUE)
      next
    }
    return(TRUE)
  }
  FALSE
}
worker_get_html <- function(w, url) {
  Sys.sleep(runif(1, 0.1, 0.3))
  worker_navigate(w, url)
  Sys.sleep(runif(1, 0.5, 1))
  worker_wait_for_page(w, 10)
  worker_source(w)
}

# ════════════════════════════════════════════════════════════
# SCRAPE ONE MATCH URL — goal times + HT + FT
# WITH DOM FIX (li.smv__participantRow.*, fallback to div.)
# ════════════════════════════════════════════════════════════

scrape_goal_times <- function(w, match_url) {
  # Normalize URL
  if (!startsWith(match_url, "http")) match_url <- paste0(BASE_URL, match_url)
  page <- worker_get_html(w, match_url)
  if (is.null(page)) return(NULL)
  # New DOM (li) first, fall back to old (div)
  home_rows <- page %>% html_nodes("li.smv__participantRow.smv__homeParticipant, div.smv__participantRow.smv__homeParticipant")
  away_rows <- page %>% html_nodes("li.smv__participantRow.smv__awayParticipant, div.smv__participantRow.smv__awayParticipant")
  extract <- function(rows) {
    t <- character()
    for (n in rows) {
      goal_icon_div <- n %>% html_node("div.smv__incidentIcon")
      if (is.null(goal_icon_div) || length(goal_icon_div) == 0) next
      goal_svg <- goal_icon_div %>% html_node("svg[data-testid='wcl-icon-incidents-goal-soccer']")
      if (is.null(goal_svg) || length(goal_svg) == 0) next
      tm <- n %>% html_node("div.smv__timeBox") %>% html_text(trim = TRUE)
      if (!is.na(tm) && nchar(tm) > 0) t <- c(t, tm)
    }
    t
  }
  gt_home <- as.list(extract(home_rows))
  gt_away <- as.list(extract(away_rows))
  # HT from headerSection
  ht_home <- NA_integer_; ht_away <- NA_integer_
  blocks <- page %>% html_nodes("[data-testid='wcl-headerSection-text']")
  for (b in blocks) {
    spans <- b %>% html_nodes("span[data-testid='wcl-scores-overline-02']")
    if (length(spans) < 2) spans <- b %>% html_nodes("span")
    if (length(spans) < 2) next
    label <- html_text(spans[[1]], trim = TRUE)
    if (identical(label, "1st Half")) {
      score_txt <- html_text(spans[[2]], trim = TRUE)
      m <- regmatches(score_txt, regexec("([0-9]+)\\s*-\\s*([0-9]+)", score_txt))[[1]]
      if (length(m) >= 3) {
        ht_home <- as.integer(m[2]); ht_away <- as.integer(m[3])
      }
      break
    }
  }
  # FT from main header
  ft_home <- NA_integer_; ft_away <- NA_integer_
  hdr <- page %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
  if (length(hdr) > 0) {
    txt <- html_text(hdr[1], trim = TRUE)
    nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
    if (length(nums) >= 2) {
      ft_home <- as.integer(nums[1]); ft_away <- as.integer(nums[2])
    }
  }
  list(goal_times_home = gt_home, goal_times_away = gt_away,
       ht_home = ht_home, ht_away = ht_away,
       ft_home = ft_home, ft_away = ft_away)
}

# ════════════════════════════════════════════════════════════
# PATCH — apply scraped result to every site that referenced it
# ════════════════════════════════════════════════════════════

# Patch a single site (in a mutable list of sources)
patch_site <- function(sources, site, scraped) {
  src <- sources[[site$src]]
  fx  <- src[[site$fx_idx]]
  if (is.null(fx)) return(sources)

  if (site$kind == "fixture") {
    if (!is.null(fx$fixture) && nrow(fx$fixture) > 0) {
      fx$fixture$result_goal_times_home[[1]] <- scraped$goal_times_home
      fx$fixture$result_goal_times_away[[1]] <- scraped$goal_times_away
      # Also fill HT if it was missing
      if (is.na(fx$fixture$result_ht_home[1])) fx$fixture$result_ht_home[1] <- scraped$ht_home
      if (is.na(fx$fixture$result_ht_away[1])) fx$fixture$result_ht_away[1] <- scraped$ht_away
    }
  } else if (site$kind == "form") {
    m <- fx[[site$form_key]][[site$pos]]
    is_concerned_home <- isTRUE(m$concerned_was_home)
    m$goal_times_concerned <- if (is_concerned_home) scraped$goal_times_home else scraped$goal_times_away
    m$goal_times_opponent  <- if (is_concerned_home) scraped$goal_times_away else scraped$goal_times_home
    if (is.na(m$ht_for %||% NA))     m$ht_for     <- if (is_concerned_home) scraped$ht_home else scraped$ht_away
    if (is.na(m$ht_against %||% NA)) m$ht_against <- if (is_concerned_home) scraped$ht_away else scraped$ht_home
    if (is.na(m$ft_for %||% NA))     m$ft_for     <- if (is_concerned_home) scraped$ft_home else scraped$ft_away
    if (is.na(m$ft_against %||% NA)) m$ft_against <- if (is_concerned_home) scraped$ft_away else scraped$ft_home
    fx[[site$form_key]][[site$pos]] <- m
  } else if (site$kind == "h2h") {
    m <- fx$h2h[[site$pos]]
    is_concerned_home <- isTRUE(m$concerned_was_home)
    m$goal_times_concerned <- if (is_concerned_home) scraped$goal_times_home else scraped$goal_times_away
    m$goal_times_opponent  <- if (is_concerned_home) scraped$goal_times_away else scraped$goal_times_home
    if (is.na(m$ht_for %||% NA))     m$ht_for     <- if (is_concerned_home) scraped$ht_home else scraped$ht_away
    if (is.na(m$ht_against %||% NA)) m$ht_against <- if (is_concerned_home) scraped$ht_away else scraped$ht_home
    if (is.na(m$ft_for %||% NA))     m$ft_for     <- if (is_concerned_home) scraped$ft_home else scraped$ft_away
    if (is.na(m$ft_against %||% NA)) m$ft_against <- if (is_concerned_home) scraped$ft_away else scraped$ft_home
    fx$h2h[[site$pos]] <- m
  }
  src[[site$fx_idx]] <- fx
  sources[[site$src]] <- src
  sources
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
      .card{background:var(--white);border:1px solid var(--border);border-radius:6px;
            padding:24px 28px;margin-bottom:16px;position:relative;
            box-shadow:0 1px 3px rgba(15,31,77,0.04);}
      .card::before{content:'';position:absolute;top:0;left:0;width:48px;height:2px;background:var(--gold);}
      .eyebrow{font-family:var(--fb);font-size:9px;letter-spacing:3px;text-transform:uppercase;
               color:var(--gold-deep);font-weight:700;margin-bottom:12px;}
      .section-lab{font-family:var(--fh);font-size:24px;font-weight:700;color:var(--cobalt-deep);
                    letter-spacing:-0.5px;margin-bottom:6px;}
      .section-sub{font-family:var(--fb);font-size:13px;color:var(--text-3);margin-bottom:20px;}
      .btn-primary-b{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
                       letter-spacing:2px!important;text-transform:uppercase!important;
                       background:var(--cobalt)!important;color:var(--ivory)!important;
                       border:none!important;border-radius:3px!important;padding:12px 28px!important;}
      .btn-outline-b{font-family:var(--fb)!important;font-size:10px!important;font-weight:600!important;
                       letter-spacing:2px!important;text-transform:uppercase!important;
                       background:transparent!important;color:var(--cobalt)!important;
                       border:1px solid var(--cobalt)!important;border-radius:3px!important;
                       padding:9px 22px!important;}
      .btn-danger-b{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
                      letter-spacing:2px!important;text-transform:uppercase!important;
                      background:var(--danger)!important;color:var(--white)!important;
                      border:none!important;border-radius:3px!important;padding:12px 28px!important;}
      .kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:20px;}
      .kpi-card{background:var(--white);border:1px solid var(--border);border-radius:6px;padding:22px 24px;
                position:relative;}
      .kpi-card::before{content:'';position:absolute;top:0;left:0;width:32px;height:2px;background:var(--cobalt);}
      .kpi-num{font-family:var(--fh);font-size:40px;font-weight:900;line-height:1;
               color:var(--cobalt);letter-spacing:-1px;}
      .kpi-lab{font-family:var(--fm);font-size:9px;font-weight:500;letter-spacing:2.5px;
                text-transform:uppercase;color:var(--text-3);margin-top:10px;}
      .worker-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-top:16px;}
      .worker-card{background:var(--ivory-2);border:1px solid var(--border);border-radius:4px;
                    padding:14px 16px;}
      .worker-title{font-family:var(--fm);font-size:11px;font-weight:600;color:var(--cobalt);
                     letter-spacing:1px;}
      .worker-status{font-family:var(--fb);font-size:12px;color:var(--text-2);margin-top:6px;}
      .worker-detail{font-family:var(--fm);font-size:10px;color:var(--text-3);margin-top:4px;
                      word-break:break-all;}
      .progress-track{width:100%;height:8px;background:var(--ivory-2);border-radius:4px;
                       overflow:hidden;margin:12px 0;}
      .progress-fill{height:100%;background:linear-gradient(90deg,var(--cobalt) 0%,var(--gold) 100%);
                      transition:width 0.3s;}
      .log-pane{background:var(--cobalt-deep);border-radius:4px;padding:16px 18px;
                font-family:var(--fm);font-size:11px;color:var(--gold);height:260px;
                overflow-y:auto;white-space:pre-wrap;line-height:1.8;}
      table.dataTable thead th{background:var(--ivory-2)!important;font-size:10px!important;
                                font-weight:700!important;letter-spacing:2px!important;
                                text-transform:uppercase!important;color:var(--text-2)!important;
                                padding:12px 14px!important;}
      table.dataTable tbody td{font-size:12.5px!important;padding:10px 14px!important;
                                font-family:var(--fb)!important;}
    "))
  ),
  div(class = "hd",
      div(class = "brand",
          div(class = "logo", "Backfill", tags$span(class = "dot", ".")),
          div(class = "sub", "Goal-times recovery")),
      div(style = "font-family:var(--fm);font-size:11px;color:rgba(250,247,242,0.8);",
          textOutput("hdr_status", inline = TRUE))),
  div(class = "pg",
      # ─── 1. Load ───
      div(class = "section-lab", "1. Load source files"),
      div(class = "section-sub",
          "Upload each RDS file you want backfilled. R1/R2/R3 result files, enriched_fixtures.rds, and the MOAB cache if you want it patched too."),
      div(class = "card",
          div(class = "eyebrow", "Uploads"),
          div(style = "display:grid;grid-template-columns:repeat(2,1fr);gap:14px;",
              fileInput("upload_r1", "R1 result", accept = ".rds", width = "100%"),
              fileInput("upload_r2", "R2 result", accept = ".rds", width = "100%"),
              fileInput("upload_r3", "R3 result", accept = ".rds", width = "100%"),
              fileInput("upload_enriched", "enriched_fixtures.rds", accept = ".rds", width = "100%")),
          checkboxInput("also_cache", "Also patch match_cache.rds (from MOAB BASE_PATH)", value = TRUE),
          checkboxInput("sb_filter", "Only backfill SportyBet-verified leagues", value = TRUE),
          checkboxInput("skip_no_gt_leagues", "Skip leagues that never had goal times (auto-learn)", value = TRUE),
          div(style = "margin-top:8px;font-family:var(--fm);font-size:11px;color:var(--text-3);",
              "Cache path: ", tags$code(file.path(BASE_PATH, "match_cache.rds")))
      ),

      # ─── 2. Scan ───
      div(class = "section-lab", style = "margin-top:24px;", "2. Scan for missing data"),
      div(class = "section-sub",
          "Read each source and count entries with empty goal times. Nothing scrapes yet."),
      div(class = "card",
          div(class = "eyebrow", "Actions"),
          div(style = "display:flex;gap:14px;align-items:center;margin-top:12px;",
              actionButton("scan_btn", "Scan sources", class = "btn-primary-b"),
              div(style = "flex:1;"),
              tags$span(style = "font-family:var(--fm);font-size:11px;color:var(--text-3);",
                        textOutput("scan_status", inline = TRUE)))
      ),
      uiOutput("scan_summary"),

      # ─── 3. Run ───
      div(class = "section-lab", style = "margin-top:24px;", "3. Run backfill"),
      div(class = "section-sub",
          "Spin up 6 Chrome tabs, re-scrape each unique match URL once, patch every source that needed it. Backfilled RDS files go to ./backfilled/."),
      div(class = "card",
          div(class = "eyebrow", "Controls"),
          div(style = "display:flex;gap:14px;align-items:center;margin-top:12px;",
              actionButton("run_btn", "Start backfill", class = "btn-primary-b"),
              actionButton("stop_btn", "Force stop", class = "btn-danger-b"),
              div(style = "flex:1;"),
              tags$span(style = "font-family:var(--fm);font-size:11px;color:var(--text-3);",
                        textOutput("run_status", inline = TRUE)))
      ),
      uiOutput("live_kpis"),
      uiOutput("live_workers"),
      div(class = "card",
          div(class = "eyebrow", "Activity log"),
          div(class = "log-pane", textOutput("log_text"))
      )
  )
)

# ════════════════════════════════════════════════════════════
# SERVER
# ════════════════════════════════════════════════════════════

server <- function(input, output, session) {

  rv <- reactiveValues(
    log = "Ready. Upload files and click Scan.\n",
    sources = list(),      # named: R1, R2, R3, enriched, cache -> list of enriched fixtures
    needs = list(),        # per-source list of needs
    queue = list(),        # deduplicated queue
    scan_summary = NULL,   # data.frame for display
    running = FALSE,
    total_queue = 0,
    done_count = 0,
    fail_count = 0,
    started_at = NULL,
    worker_state = NULL,   # per-worker: list(idx, url, done_count)
    scan_status = "",
    run_status = ""
  )

  log_msg <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log <- paste0(rv$log, "[", ts, "] ", msg, "\n")
  }

  output$log_text   <- renderText({ rv$log })
  output$scan_status <- renderText({ rv$scan_status })
  output$run_status  <- renderText({ rv$run_status })

  output$hdr_status <- renderText({
    if (rv$running)
      paste0("running: ", rv$done_count, "/", rv$total_queue)
    else if (rv$total_queue > 0)
      paste0("queued: ", rv$total_queue, " matches")
    else
      "ready"
  })

  # ─── File uploads → cache into rv$sources ───────────────
  load_uploaded_rds <- function(file_input, name) {
    req(file_input)
    dest <- file.path(UPLOADS_DIR, file_input$name)
    file.copy(file_input$datapath, dest, overwrite = TRUE)
    data <- tryCatch(readRDS(dest), error = function(e) {
      log_msg(paste0("FAILED to read ", name, ": ", e$message)); NULL
    })
    if (is.null(data)) return()
    # Cache files are a NAMED list keyed by match_id (not the enriched-fixture list)
    # We treat them differently — see cache_needs_backfill below
    rv$sources[[name]] <- data
    log_msg(paste0("Loaded ", name, ": ", length(data), " top-level entries."))
  }

  observeEvent(input$upload_r1, { load_uploaded_rds(input$upload_r1, "R1") })
  observeEvent(input$upload_r2, { load_uploaded_rds(input$upload_r2, "R2") })
  observeEvent(input$upload_r3, { load_uploaded_rds(input$upload_r3, "R3") })
  observeEvent(input$upload_enriched, { load_uploaded_rds(input$upload_enriched, "enriched") })

  # ─── SCAN — compute needs per source ───────────────────
  # For cache: it's a named list of match records keyed by match_id.
  # A cache entry needs backfill if its goal_times_home AND goal_times_away are empty.
  scan_cache <- function(cache) {
    needs <- list()
    for (mid in names(cache)) {
      rec <- cache[[mid]]
      if (is.null(rec)) next
      if (gt_empty(rec$goal_times_home) && gt_empty(rec$goal_times_away)) {
        mu <- rec$match_url %||% NA
        if (is.na(mu) || !nzchar(mu)) next
        needs[[length(needs) + 1]] <- list(
          match_id = mid, match_url = as.character(mu),
          where = list(src = "cache", mid = mid, kind = "cache")
        )
      }
    }
    needs
  }

  observeEvent(input$scan_btn, {
    if (length(rv$sources) == 0 && !isTRUE(input$also_cache)) {
      showNotification("Upload at least one file first", type = "error"); return()
    }
    rv$scan_status <- "Scanning..."
    withProgress(message = "Scanning sources", value = 0.1, {
      all_needs <- list()
      per_source_counts <- list()

      # R1/R2/R3/enriched are lists of enriched-fixtures
      for (nm in c("R1", "R2", "R3", "enriched")) {
        if (is.null(rv$sources[[nm]])) next
        incProgress(0.15, detail = paste0("Scanning ", nm, "..."))
        src <- rv$sources[[nm]]
        n_needs <- scan_source(src, nm)
        rv$needs[[nm]] <- n_needs
        per_source_counts[[nm]] <- list(
          total_entries = 0,  # filled below
          needs = length(n_needs)
        )
        # Count total entries too (for %)
        total_entries <- 0
        for (ef in src) {
          if (!is.null(ef$fixture) && nrow(ef$fixture) > 0) total_entries <- total_entries + 1
          total_entries <- total_entries + length(ef$home_form %||% list())
          total_entries <- total_entries + length(ef$away_form %||% list())
          total_entries <- total_entries + length(ef$h2h %||% list())
        }
        per_source_counts[[nm]]$total_entries <- total_entries
        all_needs <- c(all_needs, n_needs)
      }

      # Cache (optional)
      if (isTRUE(input$also_cache)) {
        cache_path <- file.path(BASE_PATH, "match_cache.rds")
        if (file.exists(cache_path)) {
          incProgress(0.2, detail = "Loading cache...")
          cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
          if (!is.null(cache)) {
            rv$sources[["cache"]] <- cache
            c_needs <- scan_cache(cache)
            rv$needs[["cache"]] <- c_needs
            per_source_counts[["cache"]] <- list(
              total_entries = length(cache), needs = length(c_needs))
            # Only add cache needs for match_ids that are ALREADY referenced
            # in the other sources — no point backfilling cache-only ancient matches
            referenced_mids <- unique(vapply(all_needs, function(n) n$match_id, character(1)))
            c_needs_filtered <- Filter(function(n) n$match_id %in% referenced_mids, c_needs)
            per_source_counts[["cache"]]$filtered <- length(c_needs_filtered)
            all_needs <- c(all_needs, c_needs_filtered)
            log_msg(paste0("Cache: ", length(c_needs), " entries missing goal times; ",
                            length(c_needs_filtered), " overlap with other sources and will be patched."))
          }
        } else {
          log_msg("Cache file not found at MOAB path.")
        }
      }

      # ─── Auto-detect leagues that never had goal times ─────
      no_gt_leagues <- load_no_gt_leagues()
      if (isTRUE(input$skip_no_gt_leagues)) {
        incProgress(0.85, detail = "Learning no-goal-times leagues...")
        # Aggregate across all enriched sources (not cache — cache has no league on record)
        for (nm in c("R1", "R2", "R3", "enriched")) {
          if (is.null(rv$sources[[nm]])) next
          learned <- detect_leagues_never_had_gt(rv$sources[[nm]])
          no_gt_leagues <- unique(c(no_gt_leagues, learned))
        }
        save_no_gt_leagues(no_gt_leagues)
        log_msg(paste0("No-goal-times leagues: ", length(no_gt_leagues), " known."))
      }
      
      # ─── SportyBet filter — from curated mapper RDS ───────
      sb_verified <- NULL
      if (isTRUE(input$sb_filter)) {
        incProgress(0.88, detail = "Loading SportyBet mapper...")
        mapper_path <- file.path(BASE_PATH, "sporty_mapper.rds")
        if (!file.exists(mapper_path)) {
          log_msg("sporty_mapper.rds not found — filter disabled.")
        } else {
          mapper <- tryCatch(readRDS(mapper_path), error = function(e) NULL)
          if (is.null(mapper)) {
            log_msg("Failed to read sporty_mapper.rds — filter disabled.")
          } else {
            sb_verified <- mapper[!is.na(mapper$status) & mapper$status == "Verified" &
                                    !is.na(mapper$fs_country) & !is.na(mapper$fs_league), , drop = FALSE]
            log_msg(paste0("SportyBet mapper: ", nrow(sb_verified), " Verified leagues."))
            log_msg(paste0("Sample allowed keys: ",
                           paste(head(paste0(sb_verified$fs_country, " :: ", sb_verified$fs_league), 5),
                                 collapse = " | ")))
          }
        }
      }
      
      # Attach league info to each need (from the ef$fixture that referenced it)
      # so we can filter needs by league.
      # We rebuild all_needs with league_key set.
      annotate_needs <- function(needs, src_name) {
        src <- rv$sources[[src_name]]
        out <- lapply(needs, function(n) {
          ef <- src[[n$where$fx_idx]]
          if (!is.null(ef$fixture) && nrow(ef$fixture) > 0) {
            n$country <- as.character(ef$fixture$country[1] %||% NA)
            n$league  <- as.character(ef$fixture$league[1] %||% NA)
            n$league_key <- paste0(n$country, " :: ", n$league)
          } else {
            n$country <- NA; n$league <- NA; n$league_key <- NA
          }
          n
        })
        out
      }
      # Re-collect + annotate
      all_needs_annotated <- list()
      for (nm in c("R1", "R2", "R3", "enriched")) {
        if (is.null(rv$sources[[nm]])) next
        annotated <- annotate_needs(rv$needs[[nm]], nm)
        all_needs_annotated <- c(all_needs_annotated, annotated)
      }
      # Cache needs are trickier — cache records don't carry league.
      # Only pass cache needs whose match_id ALSO appears in an enriched source
      # (already done earlier). We inherit the annotation from a sibling need.
      if (!is.null(rv$needs$cache)) {
        mid_to_league <- list()
        for (n in all_needs_annotated) {
          if (!is.null(n$league_key) && !is.na(n$league_key))
            mid_to_league[[n$match_id]] <- n$league_key
        }
        referenced <- names(mid_to_league)
        cache_annotated <- lapply(
          Filter(function(n) n$match_id %in% referenced, rv$needs$cache),
          function(n) { n$league_key <- mid_to_league[[n$match_id]]; n })
        all_needs_annotated <- c(all_needs_annotated, cache_annotated)
      }
      
      # Apply filters
      before_filter <- length(all_needs_annotated)
      if (isTRUE(input$skip_no_gt_leagues) && length(no_gt_leagues) > 0) {
        all_needs_annotated <- Filter(function(n) {
          !isTRUE(n$league_key %in% no_gt_leagues)
        }, all_needs_annotated)
        log_msg(paste0("After no-goal-times filter: ", length(all_needs_annotated),
                       " (dropped ", before_filter - length(all_needs_annotated), ")"))
      }
      if (isTRUE(input$sb_filter) && !is.null(sb_verified)) {
        before_sb <- length(all_needs_annotated)
        sample_fx_keys <- unique(vapply(head(all_needs_annotated, 20),
                                        function(n) n$league_key %||% "NA",
                                        character(1)))
        log_msg(paste0("Sample fixture keys: ", paste(head(sample_fx_keys, 5), collapse = " | ")))
        all_fx_keys <- unique(vapply(all_needs_annotated,
                                     function(n) n$league_key %||% "NA",
                                     character(1)))
        log_msg(paste0("Total unique fixture keys: ", length(all_fx_keys)))
        # Count matches
        matched_count <- sum(all_fx_keys %in% paste0(sb_verified$fs_country, " :: ", sb_verified$fs_league))
        log_msg(paste0("Matched fixture keys: ", matched_count, " of ", length(all_fx_keys)))
        allowed_keys <- paste0(sb_verified$fs_country, " :: ", sb_verified$fs_league)
        all_needs_annotated <- Filter(function(n) {
          !is.na(n$league_key) && n$league_key %in% allowed_keys
        }, all_needs_annotated)
        log_msg(paste0("After SportyBet filter (mapper): ",
                       length(all_needs_annotated),
                       " (dropped ", before_sb - length(all_needs_annotated), ")"))
      }
      
      # Build queue
      incProgress(0.9, detail = "Deduplicating queue...")
      rv$queue <- build_queue(all_needs_annotated)
      rv$total_queue <- length(rv$queue)

      # Build display data
      rows <- list()
      for (nm in names(per_source_counts)) {
        row <- per_source_counts[[nm]]
        rows[[length(rows) + 1]] <- data.frame(
          Source = nm,
          `Total entries` = row$total_entries,
          `Missing goal times` = row$needs,
          `Filtered` = if (!is.null(row$filtered)) row$filtered else row$needs,
          stringsAsFactors = FALSE, check.names = FALSE
        )
      }
      rv$scan_summary <- do.call(rbind, rows)
    })
    rv$scan_status <- paste0("Scan complete. Queue: ", rv$total_queue, " unique matches.")
    log_msg(rv$scan_status)
  })

  output$scan_summary <- renderUI({
    df <- rv$scan_summary
    if (is.null(df) || nrow(df) == 0) return(NULL)
    total_needs <- sum(df$Filtered, na.rm = TRUE)
    tagList(
      div(class = "kpi-grid",
        div(class = "kpi-card",
          div(class = "kpi-num", nrow(df)),
          div(class = "kpi-lab", "Sources loaded")),
        div(class = "kpi-card",
          div(class = "kpi-num", sum(df$`Total entries`, na.rm = TRUE)),
          div(class = "kpi-lab", "Total entries")),
        div(class = "kpi-card",
          div(class = "kpi-num", total_needs),
          div(class = "kpi-lab", "Entries needing backfill")),
        div(class = "kpi-card",
          div(class = "kpi-num", rv$total_queue),
          div(class = "kpi-lab", "Unique matches to scrape"))
      ),
      div(class = "card",
        div(class = "eyebrow", "Per-source breakdown"),
        DTOutput("scan_breakdown_table")
      )
    )
  })

  output$scan_breakdown_table <- renderDT({
    df <- rv$scan_summary
    if (is.null(df)) return(NULL)
    datatable(df, options = list(dom = "t", pageLength = 10), rownames = FALSE)
  })

  # ─── LIVE PROGRESS UI ───────────────────────────────────
  live_tick <- reactiveTimer(2000)

  format_duration <- function(secs) {
    if (is.na(secs) || secs < 0 || !is.finite(secs)) return("--")
    h <- floor(secs / 3600); m <- floor((secs %% 3600) / 60); s <- floor(secs %% 60)
    if (h > 0) sprintf("%dh %02dm", h, m)
    else if (m > 0) sprintf("%dm %02ds", m, s)
    else sprintf("%ds", s)
  }

  output$live_kpis <- renderUI({
    if (rv$running) invalidateLater(2000, session)
    live_tick()
    if (!rv$running && rv$done_count == 0) return(NULL)
    elapsed_secs <- if (!is.null(rv$started_at))
      as.numeric(difftime(Sys.time(), rv$started_at, units = "secs")) else 0
    rate <- if (elapsed_secs > 0) rv$done_count / elapsed_secs else 0
    remaining <- max(0, rv$total_queue - rv$done_count)
    eta <- if (rate > 0) remaining / rate else NA
    pct <- if (rv$total_queue > 0) round(rv$done_count / rv$total_queue * 100, 1) else 0
    div(class = "card",
      div(class = "eyebrow", "Progress"),
      div(class = "progress-track",
        div(class = "progress-fill", style = paste0("width:", pct, "%;"))),
      div(style = "display:flex;justify-content:space-between;font-family:var(--fm);font-size:11px;color:var(--text-3);",
        tags$span(paste0(rv$done_count, " / ", rv$total_queue, " matches (", pct, "%)")),
        tags$span(paste0("Failures: ", rv$fail_count))),
      div(class = "kpi-grid", style = "margin-top:20px;",
        div(class = "kpi-card",
          div(class = "kpi-num", format_duration(elapsed_secs)),
          div(class = "kpi-lab", "Elapsed")),
        div(class = "kpi-card",
          div(class = "kpi-num", if (rate > 0) sprintf("%.1f", rate * 60) else "--"),
          div(class = "kpi-lab", "Matches / min")),
        div(class = "kpi-card",
          div(class = "kpi-num", format_duration(eta)),
          div(class = "kpi-lab", "ETA remaining")),
        div(class = "kpi-card",
          div(class = "kpi-num", style = "font-size:14px;padding-top:20px;",
            if (!is.na(eta)) format(Sys.time() + eta, "%a %H:%M") else "--"),
          div(class = "kpi-lab", "ETA finish"))
      )
    )
  })
  outputOptions(output, "live_kpis", suspendWhenHidden = FALSE, priority = 1000)

  output$live_workers <- renderUI({
    if (rv$running) invalidateLater(2000, session)
    live_tick()
    if (!rv$running) return(NULL)
    ws <- rv$worker_state
    if (is.null(ws)) return(NULL)
    cards <- lapply(seq_along(ws), function(i) {
      s <- ws[[i]]
      div(class = "worker-card",
        div(class = "worker-title", paste0("Worker ", i)),
        div(class = "worker-status", s$status %||% "idle"),
        div(class = "worker-detail", s$url %||% "")
      )
    })
    div(class = "card",
      div(class = "eyebrow", "Workers"),
      div(class = "worker-grid", do.call(tagList, cards))
    )
  })
  outputOptions(output, "live_workers", suspendWhenHidden = FALSE, priority = 1000)

  # ─── RUN BACKFILL ──────────────────────────────────────
  observeEvent(input$run_btn, {
    if (rv$total_queue == 0) {
      showNotification("Scan first to build a queue", type = "error"); return()
    }
    if (rv$running) { showNotification("Already running", type = "warning"); return() }

    rv$running <- TRUE
    rv$done_count <- 0
    rv$fail_count <- 0
    rv$started_at <- Sys.time()
    rv$worker_state <- lapply(seq_len(N_WORKERS),
                                function(i) list(status = "starting", url = ""))
    log_msg("Starting Chrome pool (6 workers)...")

    ok <- tryCatch({ start_pool(); TRUE },
                    error = function(e) { log_msg(paste0("Pool start FAILED: ", e$message)); FALSE })
    if (!ok || length(SEL_POOL) < N_WORKERS) {
      log_msg(paste0("Only ", length(SEL_POOL), " workers ready. Continuing."))
    }
    log_msg(paste0("Backfill starting. ", rv$total_queue, " matches queued."))

    # Process queue sequentially in rotation across workers.
    # (Simpler than true async — each iteration uses one worker in round-robin.)
    withProgress(message = "Backfilling", value = 0, {
      queue <- rv$queue
      sources <- rv$sources
      total <- length(queue)
      for (i in seq_along(queue)) {
        if (!rv$running) { log_msg("Stopped by user."); break }
        w_idx <- ((i - 1) %% length(SEL_POOL)) + 1
        w <- SEL_POOL[[w_idx]]
        item <- queue[[i]]
        rv$worker_state[[w_idx]] <- list(status = "scraping",
                                           url = basename(item$match_url))

        scraped <- tryCatch(scrape_goal_times(w, item$match_url),
                              error = function(e) { log_msg(paste0("SCRAPE ERR (", item$match_id, "): ", e$message)); NULL })

        if (is.null(scraped) ||
            (length(scraped$goal_times_home) == 0 && length(scraped$goal_times_away) == 0)) {
          rv$fail_count <- rv$fail_count + 1
          rv$worker_state[[w_idx]]$status <- "no goals found"
        } else {
          # Patch every site
          for (site in item$sites) {
            if (site$kind == "cache") {
              # Special: cache is a named list of match records
              rec <- sources$cache[[site$mid]]
              if (!is.null(rec)) {
                rec$goal_times_home <- scraped$goal_times_home
                rec$goal_times_away <- scraped$goal_times_away
                if (is.na(rec$ht_home %||% NA)) rec$ht_home <- scraped$ht_home
                if (is.na(rec$ht_away %||% NA)) rec$ht_away <- scraped$ht_away
                sources$cache[[site$mid]] <- rec
              }
            } else {
              sources <- patch_site(sources, site, scraped)
            }
          }
          rv$worker_state[[w_idx]]$status <- paste0("done: ",
                                                     length(scraped$goal_times_home), "H + ",
                                                     length(scraped$goal_times_away), "A")
        }
        rv$done_count <- i
        setProgress(value = i / total, detail = paste0(i, "/", total))
        # Persist every 25 scrapes
        if (i %% 25 == 0) {
          for (nm in names(sources)) {
            saveRDS(sources[[nm]], file.path(BACKFILL_DIR, paste0(nm, ".rds")))
          }
        }
      }
      # Final save
      for (nm in names(sources)) {
        saveRDS(sources[[nm]], file.path(BACKFILL_DIR, paste0(nm, ".rds")))
      }
      log_msg(paste0("Written all sources to ", BACKFILL_DIR))
    })

    log_msg("Stopping Chrome pool...")
    tryCatch(stop_pool(), error = function(e) NULL)
    rv$running <- FALSE
    rv$run_status <- paste0("Done. ", rv$done_count, " scraped, ", rv$fail_count, " failed.")
    log_msg(rv$run_status)
    showNotification(rv$run_status, type = "message", duration = 15)
  })

  observeEvent(input$stop_btn, {
    log_msg("Force-stop requested.")
    rv$running <- FALSE
    tryCatch(stop_pool(), error = function(e) NULL)
  })

  session$onSessionEnded(function() {
    tryCatch(stop_pool(), error = function(e) NULL)
  })
}

shinyApp(ui, server)

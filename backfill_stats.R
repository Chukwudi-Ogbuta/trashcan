# ============================================================
# Backfill STATS — recovers missing match stats
#
# Upload the ALREADY-BACKFILLED files from ./backfilled/:
#   R1.rds, R2.rds, R3.rds, enriched.rds
# App scans every fixture/form/H2H entry for missing stats,
# dedupes into a queue of unique match URLs, spins up 6 Chrome
# tabs to re-scrape stats page (overall + 1H + 2H tabs),
# patches results back into every source, and also into
# match_cache.rds where entries overlap.
#
# Uses DOM-fixed stats parser (new wcl-labelRow structure).
# Applies SportyBet mapper filter (verified leagues only).
# Auto-detects & persists leagues that never have stats.
#
# Outputs to ./backfilled_stats/ folder.
# ============================================================

library(shiny)
library(DT)
library(rvest)
library(httr)
library(jsonlite)

# ════════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════════

BASE_PATH         <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
BACKFILL_DIR      <- file.path(getwd(), "backfilled_stats")
PROGRESS_DIR      <- file.path(getwd(), "backfill_stats_progress")
UPLOADS_DIR       <- file.path(getwd(), "backfill_stats_uploads")
NO_STATS_PATH     <- file.path(BASE_PATH, "leagues_without_stats.rds")
if (!dir.exists(BACKFILL_DIR)) dir.create(BACKFILL_DIR, recursive = TRUE)
if (!dir.exists(PROGRESS_DIR)) dir.create(PROGRESS_DIR, recursive = TRUE)
if (!dir.exists(UPLOADS_DIR))  dir.create(UPLOADS_DIR,  recursive = TRUE)

CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
BASE_URL          <- "https://www.flashscore.com"

N_WORKERS         <- 6
NO_STATS_THRESHOLD <- 2L   # match MOAB

# Same wanted stats list MOAB uses (case-sensitive match against Flashscore)
WANTED_STATS <- c("Expected goals (xG)", "Ball possession", "Total shots",
                  "Shots on target", "Shots off target", "Blocked shots",
                  "Shots inside the box", "Shots outside the box",
                  "Big chances", "Corner kicks", "Touches in opposition box",
                  "Fouls", "Offsides", "Free kicks", "Throw ins",
                  "Yellow cards", "Red cards", "Goalkeeper saves")

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a) || is.list(a)) return(a)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

slug <- function(x) {
  x <- tolower(x)
  x <- gsub("[^a-z0-9]+", "_", x)
  x <- gsub("^_|_$", "", x)
  x
}

# ════════════════════════════════════════════════════════════
# STATS DETECTION
# ════════════════════════════════════════════════════════════

stats_empty <- function(v) {
  if (is.null(v)) return(TRUE)
  if (length(v) == 0) return(TRUE)
  if (is.list(v) && length(v) == 0) return(TRUE)
  FALSE
}

# For a form/h2h entry: needs backfill if stats is empty
entry_needs_stats <- function(m) {
  if (!is.list(m)) return(FALSE)
  mu <- m$match_url
  if (is.null(mu) || length(mu) == 0 || is.na(mu[1]) || !nzchar(mu[1])) return(FALSE)
  stats_empty(m$stats)
}

# For a fixture-level: check if it has result_stats or similar; adjust to your schema
fixture_needs_stats <- function(fx) {
  if (is.null(fx) || nrow(fx) == 0) return(FALSE)
  status <- as.character(fx$result_status[1] %||% "")
  if (status != "FINAL") return(FALSE)
  mu <- fx$match_url[1]
  if (is.null(mu) || is.na(mu) || !nzchar(mu)) return(FALSE)
  if ("result_stats_home" %in% names(fx)) {
    return(stats_empty(fx$result_stats_home[[1]]))
  }
  FALSE
}

scan_source_stats <- function(enriched, source_name) {
  needs <- list()
  for (fx_idx in seq_along(enriched)) {
    ef <- enriched[[fx_idx]]
    if (is.null(ef)) next
    
    # Fixture-level
    if (!is.null(ef$fixture) && fixture_needs_stats(ef$fixture)) {
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
        if (!entry_needs_stats(m)) next
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
      if (!entry_needs_stats(m)) next
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

build_queue <- function(all_needs) {
  by_id <- list()
  for (n in all_needs) {
    key <- n$match_id
    if (is.null(by_id[[key]])) {
      by_id[[key]] <- list(match_id = key, match_url = n$match_url, sites = list())
    }
    by_id[[key]]$sites[[length(by_id[[key]]$sites) + 1]] <- n$where
  }
  q <- unname(by_id)
  q[order(-vapply(q, function(x) length(x$sites), integer(1)))]
}

# ════════════════════════════════════════════════════════════
# NO-STATS LEAGUE LEARNING (persisted across runs)
# ════════════════════════════════════════════════════════════

load_no_stats_leagues <- function() {
  if (!file.exists(NO_STATS_PATH)) return(character())
  tryCatch(as.character(readRDS(NO_STATS_PATH)), error = function(e) character())
}
save_no_stats_leagues <- function(leagues) {
  tryCatch(saveRDS(unique(as.character(leagues)), NO_STATS_PATH), error = function(e) NULL)
}

# Learn from existing sources: leagues with 20+ FINAL matches and 0 stats coverage
detect_leagues_never_had_stats <- function(enriched_list, min_matches = 20) {
  if (length(enriched_list) == 0) return(character())
  agg <- list()
  for (ef in enriched_list) {
    if (is.null(ef$fixture) || nrow(ef$fixture) == 0) next
    lg <- as.character(ef$fixture$country[1] %||% "")
    lg2 <- as.character(ef$fixture$league[1] %||% "")
    league_key <- paste0(lg, " :: ", lg2)
    if (!nzchar(lg2)) next
    a <- agg[[league_key]] %||% list(total = 0L, with_stats = 0L)
    # Check form entries (they're the ones with stats)
    for (fk in c("home_form", "away_form")) {
      form <- ef[[fk]] %||% list()
      for (m in form) {
        if (!is.list(m)) next
        a$total <- a$total + 1L
        if (!stats_empty(m$stats)) a$with_stats <- a$with_stats + 1L
      }
    }
    agg[[league_key]] <- a
  }
  bad <- character()
  for (k in names(agg)) {
    if (agg[[k]]$total >= min_matches && agg[[k]]$with_stats == 0L) {
      bad <- c(bad, k)
    }
  }
  bad
}

# ════════════════════════════════════════════════════════════
# SELENIUM POOL
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
worker_wait_for_page <- function(w, max_wait = 10) {
  for (i in seq_len(max_wait)) {
    Sys.sleep(1)
    title <- worker_js(w, "return document.title;")
    title <- if (is.null(title)) "" else as.character(title)[1]
    if (length(title) != 1 || grepl("Just a moment|Checking", title, ignore.case = TRUE) ||
        nchar(title) == 0) next
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
# STATS PARSER — DOM-fixed (new wcl-labelRow structure)
# ════════════════════════════════════════════════════════════

parse_stats_tab <- function(page) {
  home <- list(); away <- list()
  if (is.null(page)) return(list(home = home, away = away))
  rows <- page %>% html_nodes("div[data-testid='wcl-statistics']")
  if (length(rows) == 0) rows <- page %>% html_nodes("div.wcl-row_2oCpS")
  for (r in rows) {
    cn <- r %>% html_node("span[data-testid='wcl-scores-simple-text-01'].wcl-name_2lXWg")
    if (is.null(cn)) cn <- r %>% html_node("h4[data-testid='wcl-scores-heading-04']")
    if (is.null(cn)) cn <- r %>% html_node("div[data-testid='wcl-statistics-category'] span")
    if (is.null(cn)) cn <- r %>% html_node("div.wcl-category_6sT1J span")
    if (is.null(cn)) next
    cat_name <- html_text(cn, trim = TRUE)
    if (!(cat_name %in% WANTED_STATS)) next
    home_val_txt <- NA; away_val_txt <- NA
    label_row <- r %>% html_node("div.wcl-labelRow_42JBQ")
    if (!is.null(label_row)) {
      away_val_node <- label_row %>% html_node("div.wcl-awayValue_smmfR div.wcl-value_Ywp3J")
      all_vals <- label_row %>% html_nodes("div.wcl-value_Ywp3J")
      home_val_node <- NULL
      for (v in all_vals) {
        parent_class <- html_attr(v %>% xml2::xml_parent(), "class") %||% ""
        if (!grepl("wcl-awayValue_smmfR", parent_class)) {
          home_val_node <- v; break
        }
      }
      if (!is.null(home_val_node)) home_val_txt <- html_text(home_val_node, trim = TRUE)
      if (!is.null(away_val_node)) away_val_txt <- html_text(away_val_node, trim = TRUE)
    }
    if (is.na(home_val_txt) || is.na(away_val_txt)) {
      val_divs <- r %>% html_nodes("div[data-testid='wcl-statistics-value']")
      if (length(val_divs) < 2) val_divs <- r %>% html_nodes("div.wcl-value_XJG99")
      if (length(val_divs) >= 2) {
        hv <- val_divs[[1]] %>% html_node("span")
        av <- val_divs[[2]] %>% html_node("span")
        if (!is.null(hv)) home_val_txt <- html_text(hv, trim = TRUE)
        if (!is.null(av)) away_val_txt <- html_text(av, trim = TRUE)
      }
    }
    if (is.na(home_val_txt) || is.na(away_val_txt)) next
    key <- slug(cat_name)
    home[[key]] <- home_val_txt
    away[[key]] <- away_val_txt
  }
  list(home = home, away = away)
}

# MOAB's exact stats-URL builder (path injection on ?mid= URLs)
build_stats_url <- function(match_url, half = "overall") {
  seg <- paste0("summary/stats/", half, "/")
  if (grepl("summary/stats/", match_url))
    return(sub("summary/stats/[^/]+/", seg, match_url))
  sub("(\\?mid=)", paste0(seg, "\\1"), match_url)
}

# Scrape a match's stats page — overall, 1st half, 2nd half
scrape_match_stats <- function(w, match_url) {
  if (!startsWith(match_url, "http")) match_url <- paste0(BASE_URL, match_url)
  urls <- list(
    overall = build_stats_url(match_url, "overall"),
    h1      = build_stats_url(match_url, "1st-half"),
    h2      = build_stats_url(match_url, "2nd-half")
  )
  
  out <- list(stats = list(home = list(), away = list()),
              stats_1h = list(home = list(), away = list()),
              stats_2h = list(home = list(), away = list()))
  
  for (tab_name in names(urls)) {
    page <- worker_get_html(w, urls[[tab_name]])
    if (is.null(page)) next
    parsed <- parse_stats_tab(page)
    if (tab_name == "overall") out$stats <- parsed
    else if (tab_name == "h1") out$stats_1h <- parsed
    else if (tab_name == "h2") out$stats_2h <- parsed
  }
  out
}

# ════════════════════════════════════════════════════════════
# PATCH
# ════════════════════════════════════════════════════════════

patch_stats_site <- function(sources, site, scraped) {
  src <- sources[[site$src]]
  fx  <- src[[site$fx_idx]]
  if (is.null(fx)) return(sources)
  
  if (site$kind == "fixture") {
    if (!is.null(fx$fixture) && nrow(fx$fixture) > 0) {
      if ("result_stats_home" %in% names(fx$fixture)) {
        fx$fixture$result_stats_home[[1]] <- scraped$stats$home
        fx$fixture$result_stats_away[[1]] <- scraped$stats$away
      }
      if ("result_stats_home_1h" %in% names(fx$fixture)) {
        fx$fixture$result_stats_home_1h[[1]] <- scraped$stats_1h$home
        fx$fixture$result_stats_away_1h[[1]] <- scraped$stats_1h$away
      }
      if ("result_stats_home_2h" %in% names(fx$fixture)) {
        fx$fixture$result_stats_home_2h[[1]] <- scraped$stats_2h$home
        fx$fixture$result_stats_away_2h[[1]] <- scraped$stats_2h$away
      }
    }
  } else if (site$kind == "form") {
    m <- fx[[site$form_key]][[site$pos]]
    is_concerned_home <- isTRUE(m$concerned_was_home)
    m$stats    <- if (is_concerned_home) scraped$stats$home    else scraped$stats$away
    m$stats_1h <- if (is_concerned_home) scraped$stats_1h$home else scraped$stats_1h$away
    m$stats_2h <- if (is_concerned_home) scraped$stats_2h$home else scraped$stats_2h$away
    fx[[site$form_key]][[site$pos]] <- m
  } else if (site$kind == "h2h") {
    m <- fx$h2h[[site$pos]]
    is_concerned_home <- isTRUE(m$concerned_was_home)
    m$stats    <- if (is_concerned_home) scraped$stats$home    else scraped$stats$away
    m$stats_1h <- if (is_concerned_home) scraped$stats_1h$home else scraped$stats_1h$away
    m$stats_2h <- if (is_concerned_home) scraped$stats_2h$home else scraped$stats_2h$away
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
        --danger:#A32D2D;
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
          div(class = "logo", "Backfill Stats", tags$span(class = "dot", ".")),
          div(class = "sub", "Match stats recovery")),
      div(style = "font-family:var(--fm);font-size:11px;color:rgba(250,247,242,0.8);",
          textOutput("hdr_status", inline = TRUE))),
  div(class = "pg",
      div(class = "section-lab", "1. Load backfilled files"),
      div(class = "section-sub",
          "Upload the goal-times-backfilled files from ./backfilled/. These are the sources whose stats you want patched."),
      div(class = "card",
          div(class = "eyebrow", "Uploads"),
          div(style = "display:grid;grid-template-columns:repeat(2,1fr);gap:14px;",
              fileInput("upload_r1", "R1.rds (backfilled)", accept = ".rds", width = "100%"),
              fileInput("upload_r2", "R2.rds (backfilled)", accept = ".rds", width = "100%"),
              fileInput("upload_r3", "R3.rds (backfilled)", accept = ".rds", width = "100%"),
              fileInput("upload_enriched", "enriched.rds (backfilled)", accept = ".rds", width = "100%")),
          checkboxInput("also_cache", "Also patch cache.rds (from backfilled/ folder)", value = TRUE),
          checkboxInput("sb_filter", "Only backfill SportyBet-verified leagues", value = TRUE),
          checkboxInput("skip_no_stats", "Skip leagues that never have stats (auto-learn + persist)", value = TRUE),
          div(style = "margin-top:8px;font-family:var(--fm);font-size:11px;color:var(--text-3);",
              "Cache: ", tags$code(file.path(BASE_PATH, "backfilled", "cache.rds")),
              tags$br(),
              "Mapper: ", tags$code(file.path(BASE_PATH, "sporty_mapper.rds")),
              tags$br(),
              "Persisted no-stats leagues: ", tags$code(NO_STATS_PATH))
      ),
      
      div(class = "section-lab", style = "margin-top:24px;", "2. Scan for missing stats"),
      div(class = "card",
          div(class = "eyebrow", "Actions"),
          div(style = "display:flex;gap:14px;align-items:center;margin-top:12px;",
              actionButton("scan_btn", "Scan sources", class = "btn-primary-b"),
              div(style = "flex:1;"),
              tags$span(style = "font-family:var(--fm);font-size:11px;color:var(--text-3);",
                        textOutput("scan_status", inline = TRUE)))
      ),
      uiOutput("scan_summary"),
      
      div(class = "section-lab", style = "margin-top:24px;", "3. Run stats backfill"),
      div(class = "section-sub",
          "6 Chrome tabs. Each match: 3 tabs of stats (overall, 1H, 2H). Outputs to ./backfilled_stats/."),
      div(class = "card",
          div(class = "eyebrow", "Controls"),
          div(style = "display:flex;gap:14px;align-items:center;margin-top:12px;",
              actionButton("run_btn", "Start stats backfill", class = "btn-primary-b"),
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
    log = "Ready. Upload the backfilled files and click Scan.\n",
    sources = list(),
    needs = list(),
    queue = list(),
    scan_summary = NULL,
    running = FALSE,
    total_queue = 0,
    done_count = 0,
    fail_count = 0,
    started_at = NULL,
    worker_state = NULL,
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
  
  load_uploaded_rds <- function(file_input, name) {
    req(file_input)
    dest <- file.path(UPLOADS_DIR, file_input$name)
    file.copy(file_input$datapath, dest, overwrite = TRUE)
    data <- tryCatch(readRDS(dest), error = function(e) {
      log_msg(paste0("FAILED to read ", name, ": ", e$message)); NULL
    })
    if (is.null(data)) return()
    rv$sources[[name]] <- data
    log_msg(paste0("Loaded ", name, ": ", length(data), " top-level entries."))
  }
  
  observeEvent(input$upload_r1, { load_uploaded_rds(input$upload_r1, "R1") })
  observeEvent(input$upload_r2, { load_uploaded_rds(input$upload_r2, "R2") })
  observeEvent(input$upload_r3, { load_uploaded_rds(input$upload_r3, "R3") })
  observeEvent(input$upload_enriched, { load_uploaded_rds(input$upload_enriched, "enriched") })
  
  # Cache stats-empty scan (only for match_ids referenced in enriched sources)
  scan_cache_stats <- function(cache) {
    needs <- list()
    for (mid in names(cache)) {
      rec <- cache[[mid]]
      if (is.null(rec)) next
      if (stats_empty(rec$stats_home) && stats_empty(rec$stats_away)) {
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
      
      for (nm in c("R1", "R2", "R3", "enriched")) {
        if (is.null(rv$sources[[nm]])) next
        incProgress(0.12, detail = paste0("Scanning ", nm, "..."))
        src <- rv$sources[[nm]]
        n_needs <- scan_source_stats(src, nm)
        rv$needs[[nm]] <- n_needs
        total_entries <- 0
        for (ef in src) {
          if (!is.null(ef$fixture) && nrow(ef$fixture) > 0) total_entries <- total_entries + 1
          total_entries <- total_entries + length(ef$home_form %||% list())
          total_entries <- total_entries + length(ef$away_form %||% list())
          total_entries <- total_entries + length(ef$h2h %||% list())
        }
        per_source_counts[[nm]] <- list(total_entries = total_entries, needs = length(n_needs))
        all_needs <- c(all_needs, n_needs)
      }
      
      if (isTRUE(input$also_cache)) {
        cache_read_path  <- file.path(BASE_PATH, "backfilled", "cache.rds")
        cache_write_path <- file.path(BACKFILL_DIR, "cache.rds")
        cache_path <- if (file.exists(cache_write_path)) cache_write_path else cache_read_path
        if (file.exists(cache_path)) {
          incProgress(0.15, detail = "Loading cache...")
          cache <- tryCatch(readRDS(cache_path), error = function(e) NULL)
          if (!is.null(cache)) {
            rv$sources[["cache"]] <- cache
            c_needs <- scan_cache_stats(cache)
            rv$needs[["cache"]] <- c_needs
            referenced_mids <- unique(vapply(all_needs, function(n) n$match_id, character(1)))
            c_needs_filtered <- Filter(function(n) n$match_id %in% referenced_mids, c_needs)
            per_source_counts[["cache"]] <- list(
              total_entries = length(cache), needs = length(c_needs),
              filtered = length(c_needs_filtered))
            all_needs <- c(all_needs, c_needs_filtered)
            log_msg(paste0("Cache: ", length(c_needs), " missing stats; ",
                           length(c_needs_filtered), " overlap with sources."))
          }
        } else {
          log_msg("Cache file not found.")
        }
      }
      
      # ─── No-stats league learning ─────
      no_stats_leagues <- load_no_stats_leagues()
      if (isTRUE(input$skip_no_stats)) {
        incProgress(0.85, detail = "Learning no-stats leagues...")
        for (nm in c("R1", "R2", "R3", "enriched")) {
          if (is.null(rv$sources[[nm]])) next
          learned <- detect_leagues_never_had_stats(rv$sources[[nm]])
          no_stats_leagues <- unique(c(no_stats_leagues, learned))
        }
        save_no_stats_leagues(no_stats_leagues)
        log_msg(paste0("No-stats leagues known: ", length(no_stats_leagues)))
      }
      
      # ─── SportyBet mapper ─────
      sb_verified <- NULL
      if (isTRUE(input$sb_filter)) {
        mapper_path <- file.path(BASE_PATH, "sporty_mapper.rds")
        if (file.exists(mapper_path)) {
          mapper <- tryCatch(readRDS(mapper_path), error = function(e) NULL)
          if (!is.null(mapper)) {
            sb_verified <- mapper[!is.na(mapper$status) & mapper$status == "Verified" &
                                    !is.na(mapper$fs_country) & !is.na(mapper$fs_league), , drop = FALSE]
            log_msg(paste0("SportyBet mapper: ", nrow(sb_verified), " Verified leagues."))
          }
        } else {
          log_msg("sporty_mapper.rds not found — filter disabled.")
        }
      }
      
      # ─── Annotate needs with league_key ─────
      annotate_needs <- function(needs, src_name) {
        src <- rv$sources[[src_name]]
        lapply(needs, function(n) {
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
      }
      all_needs_annotated <- list()
      for (nm in c("R1", "R2", "R3", "enriched")) {
        if (is.null(rv$sources[[nm]])) next
        annotated <- annotate_needs(rv$needs[[nm]], nm)
        all_needs_annotated <- c(all_needs_annotated, annotated)
      }
      # Cache needs: inherit league from a sibling
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
      
      # ─── Apply filters ─────
      before_filter <- length(all_needs_annotated)
      if (isTRUE(input$skip_no_stats) && length(no_stats_leagues) > 0) {
        all_needs_annotated <- Filter(function(n) {
          !isTRUE(n$league_key %in% no_stats_leagues)
        }, all_needs_annotated)
        log_msg(paste0("After no-stats filter: ", length(all_needs_annotated),
                       " (dropped ", before_filter - length(all_needs_annotated), ")"))
      }
      if (isTRUE(input$sb_filter) && !is.null(sb_verified)) {
        before_sb <- length(all_needs_annotated)
        allowed_keys <- paste0(sb_verified$fs_country, " :: ", sb_verified$fs_league)
        all_needs_annotated <- Filter(function(n) {
          !is.na(n$league_key) && n$league_key %in% allowed_keys
        }, all_needs_annotated)
        log_msg(paste0("After SportyBet filter: ", length(all_needs_annotated),
                       " (dropped ", before_sb - length(all_needs_annotated), ")"))
      }
      
      incProgress(0.95, detail = "Deduplicating queue...")
      rv$queue <- build_queue(all_needs_annotated)
      rv$total_queue <- length(rv$queue)
      
      rows <- list()
      for (nm in names(per_source_counts)) {
        row <- per_source_counts[[nm]]
        rows[[length(rows) + 1]] <- data.frame(
          Source = nm,
          `Total entries` = row$total_entries,
          `Missing stats` = row$needs,
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
              div(class = "kpi-lab", "Entries needing stats")),
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
    log_msg(paste0("Stats backfill starting. ", rv$total_queue, " matches queued."))
    
    withProgress(message = "Backfilling stats", value = 0, {
      queue <- rv$queue
      sources <- rv$sources
      total <- length(queue)
      no_stats_leagues <- load_no_stats_leagues()
      
      for (i in seq_along(queue)) {
        if (!rv$running) { log_msg("Stopped by user."); break }
        w_idx <- ((i - 1) %% length(SEL_POOL)) + 1
        w <- SEL_POOL[[w_idx]]
        item <- queue[[i]]
        # Determine league_key from first site with country info
        league_key <- NA
        for (site in item$sites) {
          if (!is.null(site$fx_idx) && !is.null(site$src)) {
            ef <- sources[[site$src]][[site$fx_idx]]
            if (!is.null(ef$fixture) && nrow(ef$fixture) > 0) {
              league_key <- paste0(ef$fixture$country[1] %||% "", " :: ",
                                   ef$fixture$league[1] %||% "")
              break
            }
          }
        }
        
        # Skip if league already known to have no stats
        if (!is.na(league_key) && league_key %in% no_stats_leagues) {
          rv$done_count <- i
          rv$worker_state[[w_idx]] <- list(status = "skipped: no-stats league",
                                           url = league_key)
          next
        }
        
        rv$worker_state[[w_idx]] <- list(status = "scraping",
                                         url = basename(item$match_url))
        
        scraped <- tryCatch(scrape_match_stats(w, item$match_url),
                            error = function(e) { log_msg(paste0("SCRAPE ERR: ", e$message)); NULL })
        
        got_stats <- !is.null(scraped) &&
          length(scraped$stats$home) > 0
        
        if (!got_stats) {
          rv$fail_count <- rv$fail_count + 1
          rv$worker_state[[w_idx]]$status <- "no stats found"
        } else {
          for (site in item$sites) {
            if (site$kind == "cache") {
              rec <- sources$cache[[site$mid]]
              if (!is.null(rec)) {
                rec$stats_home    <- scraped$stats$home
                rec$stats_away    <- scraped$stats$away
                rec$stats_home_1h <- scraped$stats_1h$home
                rec$stats_away_1h <- scraped$stats_1h$away
                rec$stats_home_2h <- scraped$stats_2h$home
                rec$stats_away_2h <- scraped$stats_2h$away
                sources$cache[[site$mid]] <- rec
              }
            } else {
              sources <- patch_stats_site(sources, site, scraped)
            }
          }
          rv$worker_state[[w_idx]]$status <- paste0("done: ",
                                                    length(scraped$stats$home), " stats")
        }
        rv$done_count <- i
        setProgress(value = i / total, detail = paste0(i, "/", total))
        if (i %% 25 == 0) {
          for (nm in names(sources)) {
            saveRDS(sources[[nm]], file.path(BACKFILL_DIR, paste0(nm, ".rds")))
          }
        }
      }
      for (nm in names(sources)) {
        saveRDS(sources[[nm]], file.path(BACKFILL_DIR, paste0(nm, ".rds")))
      }
      log_msg(paste0("Written all sources to ", BACKFILL_DIR))
    })
    
    log_msg("Stopping Chrome pool...")
    tryCatch(stop_pool(), error = function(e) NULL)
    rv$running <- FALSE
    rv$run_status <- paste0("Done. ", rv$done_count, " processed, ", rv$fail_count, " failed.")
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
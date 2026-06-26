# ============================================================
# M.O.A.B — Mother of All Boards
# Premium football intelligence platform powered by flashscore
# Ivory · Cobalt · Soft Gold theme
# ============================================================

library(shiny)
library(DT)
library(dplyr)
library(rvest)
library(httr)
library(jsonlite)
library(future)
library(parallelly)
library(openxlsx)
library(jsonlite)

# ════════════════════════════════════════════════════════════
# CONFIG
# ════════════════════════════════════════════════════════════

BASE_PATH       <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
if (!dir.exists(BASE_PATH)) dir.create(BASE_PATH, recursive = TRUE)
CACHE_PATH      <- file.path(BASE_PATH, "match_cache.rds")
DIRECTORY_PATH  <- file.path(BASE_PATH, "league_directory.rds")
ENRICHED_PATH   <- file.path(BASE_PATH, "enriched_fixtures.rds")
STANDINGS_PATH  <- file.path(BASE_PATH, "standings.rds")
FIXTURES_PATH   <- file.path(BASE_PATH, "fixtures.rds")
SETTINGS_PATH   <- file.path(BASE_PATH, "moab_settings.json")
ANALYSIS_PATH   <- file.path(BASE_PATH, "moab_analysis.R")
NO_STATS_PATH   <- file.path(BASE_PATH, "leagues_without_stats.rds")
NO_STATS_THRESHOLD <- 2L   # consecutive main-league matches with empty stats -> league flagged
CACHE_VERSION   <- 2L   # v2: stores match_url + half-stats (1h/2h) + h2h deep-scraped
BASE_URL        <- "https://www.flashscore.com"

# Load list of league names known to have NO stats (saves 3 selenium loads each).
# Returns a character vector. Empty if the file doesn't exist yet.
load_no_stats_leagues <- function() {
  if (!file.exists(NO_STATS_PATH)) return(character())
  tryCatch(as.character(readRDS(NO_STATS_PATH)), error = function(e) character())
}
save_no_stats_leagues <- function(leagues) {
  tryCatch(saveRDS(unique(as.character(leagues)), NO_STATS_PATH),
           error = function(e) NULL)
}

# Source analysis module if present (must live next to moab.R)
if (file.exists(ANALYSIS_PATH)) {
  tryCatch(source(ANALYSIS_PATH, local = FALSE),
           error = function(e) message("Analysis module load failed: ", e$message))
}

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (is.data.frame(a) || is.list(a)) return(a)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# CACHE
# ════════════════════════════════════════════════════════════

load_cache <- function() {
  if (!file.exists(CACHE_PATH)) return(list())
  tryCatch(readRDS(CACHE_PATH), error = function(e) {
    file.copy(CACHE_PATH, paste0(CACHE_PATH, ".corrupt"), overwrite = TRUE)
    list()
  })
}

save_cache <- function(cache) {
  tryCatch({
    tmp <- paste0(CACHE_PATH, ".tmp")
    saveRDS(cache, tmp)
    file.rename(tmp, CACHE_PATH)
  }, error = function(e) invisible())
}

is_valid_cache_entry <- function(entry) {
  if (is.null(entry)) return(FALSE)
  ev <- entry$cache_version %||% 0L
  if (ev < CACHE_VERSION) return(FALSE)
  status <- entry$status %||% "UNKNOWN"
  if (status == "FINAL") {
    if (is.null(entry$ft_home) || is.null(entry$ft_away)) return(FALSE)
    if (is.na(entry$ft_home) || is.na(entry$ft_away)) return(FALSE)
    return(TRUE)
  }
  FALSE
}

cache_hit <- function(cache, match_id, force_refresh = FALSE) {
  if (isTRUE(force_refresh)) return(FALSE)
  is_valid_cache_entry(cache[[match_id]])
}

prepare_for_cache <- function(data) {
  if (is.null(data)) return(NULL)
  data$cache_version <- CACHE_VERSION
  data$cached_at     <- Sys.time()
  if (!is_valid_cache_entry(data)) attr(data, "skip_cache") <- TRUE
  data
}

# ════════════════════════════════════════════════════════════
# SELENIUM
# ════════════════════════════════════════════════════════════

# Worker = one chromedriver process on its own port, one Chrome window, with
# multiple tabs (handles) we round-robin through for true parallel page loads.

N_WORKERS  <- 4   # number of Chrome windows (used by sequential pool for fixtures/standings)
N_TABS     <- 2   # tabs per window
N_PARALLEL_WORKERS <- 6  # true parallel R processes for form/H2H enrichment
CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
WORKER_PROGRESS_DIR <- file.path(BASE_PATH, "worker_progress")
if (!dir.exists(WORKER_PROGRESS_DIR)) dir.create(WORKER_PROGRESS_DIR, recursive = TRUE)

# Pool of workers — populated by start_selenium_pool()
SEL_POOL <- list()

kill_all_chromedrivers <- function() {
  for (i in 1:3) {
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
           ignore.stderr = TRUE, wait = TRUE); Sys.sleep(0.5)
  }
  # Also kill orphaned Chrome processes spawned by the drivers
  system("taskkill /F /IM chrome.exe", ignore.stdout = TRUE,
         ignore.stderr = TRUE, wait = TRUE)
  Sys.sleep(1)
  cleanup_temp_chrome_dirs()
}

# Remove orphaned Chrome user-data folders (scoped_dir*) left in %TEMP% when
# chromedrivers are force-killed. Each can be several GB; on a near-full SSD
# these will fill the disk and crash a long scrape. Safe to delete — they only
# exist for dead Chrome sessions.
cleanup_temp_chrome_dirs <- function() {
  tryCatch({
    temp_root <- Sys.getenv("TEMP")
    if (nchar(temp_root) == 0) temp_root <- Sys.getenv("TMP")
    if (nchar(temp_root) == 0) return(invisible())
    dirs <- list.dirs(temp_root, recursive = FALSE, full.names = TRUE)
    scoped <- dirs[grepl("scoped_dir", basename(dirs), ignore.case = TRUE)]
    if (length(scoped) == 0) return(invisible())
    removed <- 0
    for (d in scoped) {
      ok <- tryCatch({ unlink(d, recursive = TRUE, force = TRUE); TRUE },
                     error = function(e) FALSE)
      if (ok) removed <- removed + 1
    }
    message("[ CLEANUP ] Removed ", removed, " orphaned scoped_dir folders from TEMP")
  }, error = function(e) invisible())
}

# Find N free ports for N workers
find_free_ports <- function(n) {
  ports <- c()
  set.seed(as.integer(Sys.time()))
  candidates <- c(8888, 8889, 7777, 7778, 6666, 5555, 9876, 9877, 9878, 9879)
  for (p in candidates) {
    if (length(ports) >= n) break
    ports <- c(ports, p)
  }
  while (length(ports) < n) {
    p <- sample(10000:60000, 1)
    if (!p %in% ports) ports <- c(ports, p)
  }
  ports
}

# Start ONE worker: chromedriver on a specific port + create N_TABS tabs.
# Returns list with port, session_id, handles (list of window handles).
start_worker <- function(port) {
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
  if (is.null(response) || status_code(response) != 200) stop("Worker start failed on port ", port)
  sd <- fromJSON(content(response, as = "text"))
  session_id <- sd$sessionId %||% sd$value$sessionId
  
  # Get the initial window handle
  r <- GET(paste0("http://localhost:", port, "/session/", session_id, "/window/handles"))
  handles <- fromJSON(content(r, as = "text"))$value
  
  # Open additional tabs to reach N_TABS total
  worker <- list(port = port, session_id = session_id, handles = handles,
                 current_tab = 1)
  while (length(worker$handles) < N_TABS) {
    body_json <- '{"type": "tab"}'
    POST(paste0("http://localhost:", port, "/session/", session_id, "/window/new"),
         body = body_json, encode = "raw",
         add_headers(`Content-Type` = "application/json"), timeout(15))
    Sys.sleep(0.5)
    r <- GET(paste0("http://localhost:", port, "/session/", session_id, "/window/handles"))
    worker$handles <- fromJSON(content(r, as = "text"))$value
  }
  worker
}

# Start the whole pool: N_WORKERS workers, each with N_TABS tabs
start_selenium_pool <- function() {
  kill_all_chromedrivers()
  ports <- find_free_ports(N_WORKERS)
  SEL_POOL <<- list()
  for (i in seq_along(ports)) {
    message("[ POOL ] Starting worker ", i, "/", N_WORKERS, " on port ", ports[i])
    w <- tryCatch(start_worker(ports[i]), error = function(e) {
      message("Worker ", i, " failed: ", e$message); NULL
    })
    if (!is.null(w)) SEL_POOL[[length(SEL_POOL) + 1]] <<- w
    Sys.sleep(1)
  }
  if (length(SEL_POOL) == 0) stop("All workers failed to start")
  # Warm up each worker
  for (w in SEL_POOL) {
    tab_navigate(w, w$handles[1], paste0(BASE_URL, "/"))
    Sys.sleep(3)
    tab_dismiss_cookie(w, w$handles[1])
  }
  message("[ POOL ] ", length(SEL_POOL), " workers ready (",
          length(SEL_POOL) * N_TABS, " concurrent tabs)")
}

stop_selenium_pool <- function() {
  for (w in SEL_POOL) {
    tryCatch(DELETE(paste0("http://localhost:", w$port, "/session/", w$session_id),
                    timeout(10)), error = function(e) invisible(NULL))
  }
  kill_all_chromedrivers()
  SEL_POOL <<- list()
}

# Tab-level operations (operate on a specific window handle within a worker)
switch_to_tab <- function(worker, handle) {
  body_json <- sprintf('{"handle": "%s"}', handle)
  POST(paste0("http://localhost:", worker$port, "/session/", worker$session_id, "/window"),
       body = body_json, encode = "raw",
       add_headers(`Content-Type` = "application/json"), timeout(10))
}

tab_navigate <- function(worker, handle, url) {
  switch_to_tab(worker, handle)
  POST(paste0("http://localhost:", worker$port, "/session/", worker$session_id, "/url"),
       body = list(url = url), encode = "json", timeout(30))
}

tab_get_source <- function(worker, handle) {
  switch_to_tab(worker, handle)
  res <- GET(paste0("http://localhost:", worker$port, "/session/", worker$session_id,
                    "/source"), timeout(30))
  html_raw <- fromJSON(content(res, as = "text"))$value
  tryCatch(read_html(html_raw), error = function(e) NULL)
}

tab_js <- function(worker, handle, script_text) {
  switch_to_tab(worker, handle)
  body_json <- sprintf('{"script": %s, "args": []}',
                       toJSON(script_text, auto_unbox = TRUE))
  tryCatch({
    r <- POST(paste0("http://localhost:", worker$port, "/session/", worker$session_id,
                     "/execute/sync"),
              body = body_json, encode = "raw",
              add_headers(`Content-Type` = "application/json"),
              timeout(15))
    fromJSON(content(r, as = "text"))$value
  }, error = function(e) NULL)
}

tab_dismiss_cookie <- function(worker, handle) {
  tab_js(worker, handle,
         "var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
  Sys.sleep(0.5)
}

tab_wait_for_page <- function(worker, handle, max_wait = 20) {
  for (i in seq_len(max_wait)) {
    Sys.sleep(1)
    title <- tab_js(worker, handle, "return document.title;")
    title <- if (is.null(title)) "" else as.character(title)[1]
    if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
        nchar(title) > 0) return(TRUE)
  }
  FALSE
}

tab_get_html <- function(worker, handle, url) {
  Sys.sleep(runif(1, 0.5, 1.5))
  tab_navigate(worker, handle, url)
  Sys.sleep(runif(1, 2, 3))
  tab_wait_for_page(worker, handle, max_wait = 15)
  tab_dismiss_cookie(worker, handle)
  Sys.sleep(0.5)
  tab_get_source(worker, handle)
}

tab_click_show_more <- function(worker, handle) {
  for (attempt in 1:2) {
    n <- tab_js(worker, handle, paste0(
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

# Backward-compat helpers that pick a tab from the pool round-robin
# (used by sequential code paths like fixtures/standings scrape)
pool_tab_idx <- 1L
get_next_tab <- function() {
  if (length(SEL_POOL) == 0) stop("Pool not started")
  total_tabs <- length(SEL_POOL) * N_TABS
  idx <- (pool_tab_idx - 1) %% total_tabs + 1
  pool_tab_idx <<- pool_tab_idx + 1L
  worker_idx <- ((idx - 1) %/% N_TABS) + 1
  tab_within  <- ((idx - 1) %%  N_TABS) + 1
  w <- SEL_POOL[[worker_idx]]
  list(worker = w, handle = w$handles[tab_within])
}

selenium_get_html <- function(url) {
  t <- get_next_tab()
  tab_get_html(t$worker, t$handle, url)
}

selenium_navigate <- function(url) {
  t <- get_next_tab()
  tab_navigate(t$worker, t$handle, url)
}

selenium_get_source <- function() {
  if (length(SEL_POOL) == 0) return(NULL)
  w <- SEL_POOL[[1]]; tab_get_source(w, w$handles[1])
}

dismiss_cookie_popup <- function() {
  if (length(SEL_POOL) == 0) return(invisible())
  w <- SEL_POOL[[1]]; tab_dismiss_cookie(w, w$handles[1])
}

wait_for_page <- function(max_wait = 20) {
  if (length(SEL_POOL) == 0) return(FALSE)
  w <- SEL_POOL[[1]]; tab_wait_for_page(w, w$handles[1], max_wait)
}

js_eval <- function(script_text) {
  if (length(SEL_POOL) == 0) return(NULL)
  w <- SEL_POOL[[1]]; tab_js(w, w$handles[1], script_text)
}

click_show_more_h2h <- function() {
  if (length(SEL_POOL) == 0) return(invisible())
  w <- SEL_POOL[[1]]; tab_click_show_more(w, w$handles[1])
}

# Compatibility wrappers for old API
start_selenium <- function() start_selenium_pool()
stop_selenium  <- function() stop_selenium_pool()

# ════════════════════════════════════════════════════════════
# PARSERS
# ════════════════════════════════════════════════════════════

build_url <- function(base, page) paste0(base, page, "/")

YEAR <- format(Sys.Date(), "%Y")
parse_date <- function(s) {
  m <- regmatches(s, regexpr("\\d{1,2}\\.\\d{1,2}\\.", s))
  if (length(m) == 0 || nchar(m) == 0) return(list(date = NA, time = NA))
  d <- tryCatch(as.Date(paste0(m, YEAR), "%d.%m.%Y"), error = function(e) NA)
  t <- regmatches(s, regexpr("\\d{1,2}:\\d{2}", s))
  list(date = d, time = if (length(t) > 0) t else NA)
}

scrape_fixtures <- function(league_base, league_name, country = NA) {
  url <- build_url(league_base, "fixtures")
  page <- selenium_get_html(url)
  if (is.null(page)) return(data.frame())
  match_nodes <- page %>% html_nodes("div.event__match")
  if (length(match_nodes) == 0) return(data.frame())
  out <- data.frame()
  for (m in match_nodes) {
    time_node <- m %>% html_node("div.event__time")
    if (is.null(time_node)) next
    dt <- parse_date(html_text(time_node, trim = TRUE))
    home_img <- m %>% html_node("div.event__homeParticipant img")
    away_img <- m %>% html_node("div.event__awayParticipant img")
    home <- if (!is.null(home_img)) html_attr(home_img, "alt") else NA
    away <- if (!is.null(away_img)) html_attr(away_img, "alt") else NA
    if (is.na(home) || is.na(away)) next
    link_node <- m %>% html_node("a.eventRowLink")
    match_url <- if (!is.null(link_node)) html_attr(link_node, "href") else NA
    match_id <- NA
    div_id <- html_attr(m, "id") %||% ""
    if (grepl("g_1_", div_id)) match_id <- gsub("^g_1_", "", div_id)
    home_team_id <- NA; away_team_id <- NA
    if (!is.na(match_url)) {
      matches <- regmatches(match_url, gregexpr("-([A-Za-z0-9]{8})/", match_url))[[1]]
      if (length(matches) >= 2) {
        ids <- gsub("[-/]", "", matches)
        home_team_id <- ids[1]; away_team_id <- ids[2]
      }
    }
    out <- rbind(out, data.frame(
      league = league_name, league_url = league_base, country = country,
      home = home, away = away,
      home_team_id = home_team_id, away_team_id = away_team_id,
      fixture_date = dt$date, match_time = dt$time,
      match_id = match_id, match_url = match_url,
      stringsAsFactors = FALSE
    ))
  }
  out
}

scrape_standings <- function(league_base, league_name) {
  url <- build_url(league_base, "standings")
  page <- selenium_get_html(url)
  if (is.null(page)) return(data.frame())
  rows <- page %>% html_nodes("div.ui-table__row")
  if (length(rows) == 0) return(data.frame())
  out <- data.frame()
  for (r in rows) {
    rank_node <- r %>% html_node("div.tableCellRank")
    if (is.null(rank_node)) next
    rank <- suppressWarnings(as.integer(gsub("\\.", "", html_text(rank_node, trim = TRUE))))
    if (is.na(rank)) next
    promo_title <- html_attr(rank_node, "title") %||% ""
    team_node <- r %>% html_node("a.tableCellParticipant__name")
    team_name <- if (!is.null(team_node)) html_text(team_node, trim = TRUE) else NA
    team_url  <- if (!is.null(team_node)) html_attr(team_node, "href") else NA
    if (is.na(team_name)) next
    vals <- r %>% html_nodes("span.table__cell--value") %>% html_text(trim = TRUE)
    if (length(vals) < 7) next
    g <- vals[5]
    gfor <- NA; gag <- NA
    if (!is.na(g) && grepl(":", g)) {
      gp <- strsplit(g, ":")[[1]]
      gfor <- suppressWarnings(as.integer(gp[1])); gag <- suppressWarnings(as.integer(gp[2]))
    }
    form_nodes <- r %>% html_nodes("div.tableCellFormIcon div.wcl-badgeform_AKaAR")
    form_chars <- character()
    for (fn in form_nodes) {
      tt <- html_attr(fn, "data-testid") %||% ""
      form_chars <- c(form_chars,
                      if (grepl("win", tt)) "W"
                      else if (grepl("lose", tt)) "L"
                      else if (grepl("draw", tt)) "D"
                      else "?")
    }
    if (length(form_chars) > 0 && form_chars[1] == "?") form_chars <- form_chars[-1]
    team_id <- NA
    if (!is.na(team_url)) {
      mid <- regmatches(team_url, regexpr("/[A-Za-z0-9]{8}/?$", team_url))
      if (length(mid) > 0) team_id <- gsub("/", "", mid)
    }
    out <- rbind(out, data.frame(
      league = league_name, rank = rank, team = team_name, team_id = team_id,
      team_url = ifelse(!is.na(team_url), paste0(BASE_URL, team_url), NA),
      mp = suppressWarnings(as.integer(vals[1])),
      w  = suppressWarnings(as.integer(vals[2])),
      d  = suppressWarnings(as.integer(vals[3])),
      l  = suppressWarnings(as.integer(vals[4])),
      goals_for = gfor, goals_against = gag,
      gd = suppressWarnings(as.integer(vals[6])),
      pts = suppressWarnings(as.integer(vals[7])),
      form = paste(form_chars, collapse = ""),
      promo_title = promo_title, stringsAsFactors = FALSE
    ))
  }
  out
}

parse_h2h_row <- function(row_node) {
  date_node  <- row_node %>% html_node("span.h2h__date")
  event_node <- row_node %>% html_node("span.h2h__event")
  home_node  <- row_node %>% html_node("span.h2h__homeParticipant span.h2h__participantInner")
  away_node  <- row_node %>% html_node("span.h2h__awayParticipant span.h2h__participantInner")
  result_nodes <- row_node %>% html_nodes("span.h2h__result > span")
  match_url <- html_attr(row_node, "href")
  competition_full <- if (!is.null(event_node)) html_attr(event_node, "title") else NA
  competition_tag  <- if (!is.null(event_node)) html_text(event_node, trim = TRUE) else NA
  home_score <- NA; away_score <- NA
  if (length(result_nodes) >= 2) {
    home_score <- suppressWarnings(as.integer(html_text(result_nodes[1], trim = TRUE)))
    away_score <- suppressWarnings(as.integer(html_text(result_nodes[2], trim = TRUE)))
  }
  match_id <- NA
  if (!is.na(match_url)) {
    mid <- regmatches(match_url, regexpr("mid=([A-Za-z0-9]+)", match_url))
    if (length(mid) > 0) match_id <- gsub("mid=", "", mid)
  }
  list(match_id = match_id, match_url = match_url,
       date = if (!is.null(date_node)) html_text(date_node, trim = TRUE) else NA,
       home = if (!is.null(home_node)) html_text(home_node, trim = TRUE) else NA,
       away = if (!is.null(away_node)) html_text(away_node, trim = TRUE) else NA,
       home_score = home_score, away_score = away_score,
       competition_full = competition_full, competition_tag = competition_tag)
}

scrape_h2h_page <- function(fixture_match_url, league_name_for_filter) {
  h2h_url <- sub("(\\?mid=)", "h2h/overall/\\1", fixture_match_url)
  Sys.sleep(runif(1, 2, 4)); selenium_navigate(h2h_url); Sys.sleep(runif(1, 4, 6))
  wait_for_page(max_wait = 20); dismiss_cookie_popup(); Sys.sleep(2)
  click_show_more_h2h()
  page <- selenium_get_source()
  if (is.null(page)) return(list(home_form = list(), away_form = list(), h2h = list()))
  sections <- page %>% html_nodes("div.h2h__section")
  out <- list(home_form = list(), away_form = list(), h2h = list())
  for (i in seq_along(sections)) {
    sec <- sections[[i]]
    title_node <- sec %>% html_node("span.wcl-scores-overline-02_bpqU7")
    title_txt  <- if (!is.null(title_node)) html_text(title_node, trim = TRUE) else ""
    rows <- sec %>% html_nodes("a.h2h__row")
    parsed <- lapply(rows, parse_h2h_row)
    for (k in seq_along(parsed)) {
      cf <- parsed[[k]]$competition_full %||% ""
      parsed[[k]]$is_main_league <- grepl(league_name_for_filter, cf, ignore.case = TRUE)
    }
    if (i == 1)      out$home_form <- head(parsed, 7)
    else if (i == 2) out$away_form <- head(parsed, 7)
    else if (i == 3) out$h2h       <- head(parsed, 5)
  }
  out
}

WANTED_STATS <- c("Expected goals (xG)", "Ball possession", "Total shots",
                  "Shots on target", "Shots off target", "Blocked shots",
                  "Shots inside the box", "Shots outside the box",
                  "Big chances", "Corner kicks", "Touches in opposition box",
                  "Fouls", "Offsides", "Free kicks", "Throw ins",
                  "Yellow cards", "Red cards", "Goalkeeper saves")

slug <- function(x) {
  x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_|_$", "", x)
}

scrape_match_summary <- function(match_url) {
  page <- tryCatch(selenium_get_html(match_url), error = function(e) NULL)
  if (is.null(page)) return(list(goal_times_home = list(), goal_times_away = list(),
                                 ht_home = NA_integer_, ht_away = NA_integer_))
  result <- list(goal_times_home = list(), goal_times_away = list(),
                 ht_home = NA_integer_, ht_away = NA_integer_)
  home_rows <- page %>% html_nodes("div.smv__participantRow.smv__homeParticipant")
  away_rows <- page %>% html_nodes("div.smv__participantRow.smv__awayParticipant")
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
  result$goal_times_home <- as.list(extract(home_rows))
  result$goal_times_away <- as.list(extract(away_rows))
  # Pull HT score from the "1st Half" partial-score block.
  # HTML shape (canonical):
  #   <div data-testid="wcl-headerSection-text">
  #     <span>1st Half</span><span><div>2 - 0</div></span>
  #   </div>
  blocks <- page %>% html_nodes("[data-testid='wcl-headerSection-text']")
  for (b in blocks) {
    spans <- b %>% html_nodes("span")
    if (length(spans) < 2) next
    label <- html_text(spans[[1]], trim = TRUE)
    if (identical(label, "1st Half")) {
      score_txt <- html_text(spans[[2]], trim = TRUE)
      m <- regmatches(score_txt, regexec("([0-9]+)\\s*-\\s*([0-9]+)", score_txt))[[1]]
      if (length(m) >= 3) {
        result$ht_home <- as.integer(m[2])
        result$ht_away <- as.integer(m[3])
      }
      break
    }
  }
  result
}

build_stats_url <- function(match_url, half = "overall") {
  seg <- paste0("summary/stats/", half, "/")
  if (grepl("summary/stats/", match_url))
    return(sub("summary/stats/[^/]+/", seg, match_url))
  sub("(\\?mid=)", paste0(seg, "\\1"), match_url)
}

# Parse one stats tab page into home/away named lists
parse_stats_page <- function(page) {
  home <- list(); away <- list()
  if (is.null(page)) return(list(home = home, away = away))
  rows <- page %>% html_nodes("div.wcl-row_2oCpS")
  for (r in rows) {
    cat_node <- r %>% html_node("div.wcl-category_6sT1J span")
    if (is.null(cat_node)) next
    cat_name <- html_text(cat_node, trim = TRUE)
    if (!(cat_name %in% WANTED_STATS)) next
    vals <- r %>% html_nodes("div.wcl-value_XJG99 span.wcl-bold_NZXv6")
    if (length(vals) < 2) next
    key <- slug(cat_name)
    home[[key]] <- html_text(vals[1], trim = TRUE)
    away[[key]] <- html_text(vals[2], trim = TRUE)
  }
  list(home = home, away = away)
}

scrape_match_stats <- function(match_url) {
  # Overall tab (also carries the score header)
  page <- tryCatch(selenium_get_html(build_stats_url(match_url, "overall")), error = function(e) NULL)
  if (is.null(page)) return(NULL)
  result <- list(stats_home = list(), stats_away = list(),
                 stats_home_1h = list(), stats_away_1h = list(),
                 stats_home_2h = list(), stats_away_2h = list(),
                 ht_home = NA, ht_away = NA, ft_home = NA, ft_away = NA,
                 goal_times_home = list(), goal_times_away = list(),
                 match_url = match_url, status = "UNKNOWN")
  ov <- parse_stats_page(page)
  result$stats_home <- ov$home; result$stats_away <- ov$away
  header_score <- page %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
  if (length(header_score) > 0) {
    txt <- html_text(header_score[1], trim = TRUE)
    nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
    if (length(nums) >= 2) {
      result$ft_home <- as.integer(nums[1])
      result$ft_away <- as.integer(nums[2])
      result$status <- "FINAL"
    }
  }
  # First-half + second-half tabs (not all matches have them; empty list is fine)
  p1 <- tryCatch(selenium_get_html(build_stats_url(match_url, "1st-half")), error = function(e) NULL)
  h1 <- parse_stats_page(p1)
  result$stats_home_1h <- h1$home; result$stats_away_1h <- h1$away
  p2 <- tryCatch(selenium_get_html(build_stats_url(match_url, "2nd-half")), error = function(e) NULL)
  h2 <- parse_stats_page(p2)
  result$stats_home_2h <- h2$home; result$stats_away_2h <- h2$away
  # Goal times stored for future "no goal in X minutes" market only.
  summary_data <- tryCatch(scrape_match_summary(match_url),
                           error = function(e) list(goal_times_home = list(),
                                                    goal_times_away = list(),
                                                    ht_home = NA_integer_,
                                                    ht_away = NA_integer_))
  result$goal_times_home <- summary_data$goal_times_home
  result$goal_times_away <- summary_data$goal_times_away
  # HT score: ONLY from the "1st Half" partial-score block. No fallback.
  # If FlashScore didn't render that block, HT stays NA.
  result$ht_home <- summary_data$ht_home
  result$ht_away <- summary_data$ht_away
  result
}

enrich_form_entry <- function(entry, concerned_team_name, cache, force_refresh = FALSE) {
  if (is.null(entry$match_id) || is.na(entry$match_id))
    return(list(entry = entry, cache_updated = FALSE, new_data = NULL))
  cache_key <- entry$match_id
  use_cache <- cache_hit(cache, cache_key, force_refresh)
  if (use_cache) cached <- cache[[cache_key]]
  else {
    cached <- scrape_match_stats(entry$match_url)
    if (is.null(cached)) return(list(entry = entry, cache_updated = FALSE, new_data = NULL))
    cached <- prepare_for_cache(cached)
  }
  is_concerned_home <- !is.na(entry$home) &&
    grepl(concerned_team_name, entry$home, ignore.case = TRUE)
  entry$concerned_was_home <- is_concerned_home
  entry$ht_for     <- if (is_concerned_home) cached$ht_home else cached$ht_away
  entry$ht_against <- if (is_concerned_home) cached$ht_away else cached$ht_home
  entry$ft_for     <- if (is_concerned_home) cached$ft_home else cached$ft_away
  entry$ft_against <- if (is_concerned_home) cached$ft_away else cached$ft_home
  entry$goal_times <- if (is_concerned_home) cached$goal_times_home else cached$goal_times_away
  entry$stats      <- if (is_concerned_home) cached$stats_home else cached$stats_away
  entry$stats_1h   <- if (is_concerned_home) cached$stats_home_1h else cached$stats_away_1h
  entry$stats_2h   <- if (is_concerned_home) cached$stats_home_2h else cached$stats_away_2h
  entry$status     <- cached$status
  list(entry = entry, cache_updated = !use_cache, new_data = cached)
}


# ════════════════════════════════════════════════════════════
# PARALLEL WORKER FUNCTION
# ════════════════════════════════════════════════════════════
# Each worker runs in an isolated R process via future::multisession.
# It starts its own chromedriver, scrapes its assigned chunk of match URLs,
# checkpoints to disk every N matches, and returns the result list.
#
# Workers NEVER write to the main cache file. Main process merges results.

# League worker — scrapes fixtures + standings for a chunk of leagues.
# Returns list(fixtures=df, standings=df)
run_league_worker <- function(worker_id, leagues_df, chrome_path, chromedriver_path,
                              base_url, progress_dir) {
  library(rvest); library(httr); library(jsonlite); library(dplyr)
  
  port <- 30000 + worker_id * 271 + sample(1:999, 1)
  session_id <- NULL
  
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.data.frame(a) || is.list(a)) return(a)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }
  YEAR <- format(Sys.Date(), "%Y")
  parse_date <- function(s) {
    m <- regmatches(s, regexpr("\\d{1,2}\\.\\d{1,2}\\.", s))
    if (length(m) == 0 || nchar(m) == 0) return(list(date = NA, time = NA))
    d <- tryCatch(as.Date(paste0(m, YEAR), "%d.%m.%Y"), error = function(e) NA)
    t <- regmatches(s, regexpr("\\d{1,2}:\\d{2}", s))
    list(date = d, time = if (length(t) > 0) t else NA)
  }
  build_url <- function(base, page) paste0(base, page, "/")
  
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
  wait_for_page <- function(max_wait = 15) {
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      title <- js_eval("return document.title;")
      title <- if (is.null(title)) "" else as.character(title)[1]
      if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
          nchar(title) > 0) return(TRUE)
    }
    FALSE
  }
  dismiss_cookie <- function() {
    js_eval("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
    Sys.sleep(0.5)
  }
  get_html <- function(url) {
    Sys.sleep(runif(1, 1, 2))
    navigate(url)
    Sys.sleep(runif(1, 2, 3))
    wait_for_page(15)
    dismiss_cookie()
    Sys.sleep(0.5)
    get_source()
  }
  
  scrape_fixtures <- function(league_base, league_name, country = NA) {
    url <- build_url(league_base, "fixtures")
    page <- get_html(url)
    if (is.null(page)) return(data.frame())
    nodes <- page %>% html_nodes("div.event__match")
    if (length(nodes) == 0) return(data.frame())
    out <- data.frame()
    for (m in nodes) {
      tn <- m %>% html_node("div.event__time")
      if (is.null(tn)) next
      dt <- parse_date(html_text(tn, trim = TRUE))
      hi <- m %>% html_node("div.event__homeParticipant img")
      ai <- m %>% html_node("div.event__awayParticipant img")
      home <- if (!is.null(hi)) html_attr(hi, "alt") else NA
      away <- if (!is.null(ai)) html_attr(ai, "alt") else NA
      if (is.na(home) || is.na(away)) next
      lk <- m %>% html_node("a.eventRowLink")
      mu <- if (!is.null(lk)) html_attr(lk, "href") else NA
      mid <- NA
      did <- html_attr(m, "id") %||% ""
      if (grepl("g_1_", did)) mid <- gsub("^g_1_", "", did)
      hid <- NA; aid <- NA
      if (!is.na(mu)) {
        mtch <- regmatches(mu, gregexpr("-([A-Za-z0-9]{8})/", mu))[[1]]
        if (length(mtch) >= 2) { ids <- gsub("[-/]", "", mtch); hid <- ids[1]; aid <- ids[2] }
      }
      out <- rbind(out, data.frame(
        league = league_name, league_url = league_base, country = country,
        home = home, away = away,
        home_team_id = hid, away_team_id = aid,
        fixture_date = dt$date, match_time = dt$time,
        match_id = mid, match_url = mu, stringsAsFactors = FALSE))
    }
    out
  }
  
  scrape_standings <- function(league_base, league_name) {
    url <- build_url(league_base, "standings")
    page <- get_html(url)
    if (is.null(page)) return(data.frame())
    rows <- page %>% html_nodes("div.ui-table__row")
    if (length(rows) == 0) return(data.frame())
    out <- data.frame()
    for (r in rows) {
      rn <- r %>% html_node("div.tableCellRank")
      if (is.null(rn)) next
      rank <- suppressWarnings(as.integer(gsub("\\.", "", html_text(rn, trim = TRUE))))
      if (is.na(rank)) next
      pt <- html_attr(rn, "title") %||% ""
      tn <- r %>% html_node("a.tableCellParticipant__name")
      tname <- if (!is.null(tn)) html_text(tn, trim = TRUE) else NA
      turl  <- if (!is.null(tn)) html_attr(tn, "href") else NA
      if (is.na(tname)) next
      vals <- r %>% html_nodes("span.table__cell--value") %>% html_text(trim = TRUE)
      if (length(vals) < 7) next
      g <- vals[5]; gfor <- NA; gag <- NA
      if (!is.na(g) && grepl(":", g)) {
        gp <- strsplit(g, ":")[[1]]
        gfor <- suppressWarnings(as.integer(gp[1]))
        gag <- suppressWarnings(as.integer(gp[2]))
      }
      fn <- r %>% html_nodes("div.tableCellFormIcon div.wcl-badgeform_AKaAR")
      fc <- character()
      for (f in fn) {
        tt <- html_attr(f, "data-testid") %||% ""
        fc <- c(fc, if (grepl("win", tt)) "W"
                else if (grepl("lose", tt)) "L"
                else if (grepl("draw", tt)) "D" else "?")
      }
      if (length(fc) > 0 && fc[1] == "?") fc <- fc[-1]
      tid <- NA
      if (!is.na(turl)) {
        mid <- regmatches(turl, regexpr("/[A-Za-z0-9]{8}/?$", turl))
        if (length(mid) > 0) tid <- gsub("/", "", mid)
      }
      out <- rbind(out, data.frame(
        league = league_name, rank = rank, team = tname, team_id = tid,
        team_url = ifelse(!is.na(turl), paste0(base_url, turl), NA),
        mp = suppressWarnings(as.integer(vals[1])),
        w  = suppressWarnings(as.integer(vals[2])),
        d  = suppressWarnings(as.integer(vals[3])),
        l  = suppressWarnings(as.integer(vals[4])),
        goals_for = gfor, goals_against = gag,
        gd = suppressWarnings(as.integer(vals[6])),
        pts = suppressWarnings(as.integer(vals[7])),
        form = paste(fc, collapse = ""),
        promo_title = pt, stringsAsFactors = FALSE))
    }
    out
  }
  
  # Start chromedriver
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
    return(list(fixtures = data.frame(), standings = data.frame()))
  }
  navigate(paste0(base_url, "/"))
  Sys.sleep(3); wait_for_page(30); dismiss_cookie(); Sys.sleep(2)
  
  progress_path <- file.path(progress_dir, paste0("worker_a_", worker_id, "_progress.txt"))
  all_fix <- data.frame(); all_std <- data.frame()
  total <- nrow(leagues_df)
  for (i in seq_len(total)) {
    lg <- leagues_df[i, ]
    ct <- if ("country" %in% names(lg)) as.character(lg$country) else NA
    fix <- tryCatch(scrape_fixtures(lg$league_url, lg$league_name, ct),
                    error = function(e) data.frame())
    if (nrow(fix) > 0) all_fix <- rbind(all_fix, fix)
    std <- tryCatch(scrape_standings(lg$league_url, lg$league_name),
                    error = function(e) data.frame())
    if (nrow(std) > 0) all_std <- rbind(all_std, std)
    writeLines(paste0(i, "/", total), progress_path)
  }
  writeLines(paste0(total, "/", total, " DONE"), progress_path)
  
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  
  list(fixtures = all_fix, standings = all_std)
}

# H2H worker — scrapes H2H pages for a chunk of fixtures.
# Returns list of h2h data keyed by position in chunk.
run_h2h_worker <- function(worker_id, fixtures_df, chrome_path, chromedriver_path,
                           base_url, progress_dir) {
  library(rvest); library(httr); library(jsonlite); library(dplyr)
  
  port <- 40000 + worker_id * 419 + sample(1:999, 1)
  session_id <- NULL
  
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
  wait_for_page <- function(max_wait = 15) {
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      title <- js_eval("return document.title;")
      title <- if (is.null(title)) "" else as.character(title)[1]
      if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
          nchar(title) > 0) return(TRUE)
    }
    FALSE
  }
  dismiss_cookie <- function() {
    js_eval("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
    Sys.sleep(0.5)
  }
  click_show_more <- function() {
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
  
  parse_h2h_row <- function(row_node) {
    date_node  <- row_node %>% html_node("span.h2h__date")
    event_node <- row_node %>% html_node("span.h2h__event")
    home_node  <- row_node %>% html_node("span.h2h__homeParticipant span.h2h__participantInner")
    away_node  <- row_node %>% html_node("span.h2h__awayParticipant span.h2h__participantInner")
    result_nodes <- row_node %>% html_nodes("span.h2h__result > span")
    mu <- html_attr(row_node, "href")
    cf <- if (!is.null(event_node)) html_attr(event_node, "title") else NA
    ct <- if (!is.null(event_node)) html_text(event_node, trim = TRUE) else NA
    hs <- NA; as_ <- NA
    if (length(result_nodes) >= 2) {
      hs  <- suppressWarnings(as.integer(html_text(result_nodes[1], trim = TRUE)))
      as_ <- suppressWarnings(as.integer(html_text(result_nodes[2], trim = TRUE)))
    }
    mid <- NA
    if (!is.na(mu)) {
      m <- regmatches(mu, regexpr("mid=([A-Za-z0-9]+)", mu))
      if (length(m) > 0) mid <- gsub("mid=", "", m)
    }
    list(match_id = mid, match_url = mu,
         date = if (!is.null(date_node)) html_text(date_node, trim = TRUE) else NA,
         home = if (!is.null(home_node)) html_text(home_node, trim = TRUE) else NA,
         away = if (!is.null(away_node)) html_text(away_node, trim = TRUE) else NA,
         home_score = hs, away_score = as_,
         competition_full = cf, competition_tag = ct)
  }
  
  scrape_h2h <- function(fixture_match_url, league_name_for_filter) {
    h2h_url <- sub("(\\?mid=)", "h2h/overall/\\1", fixture_match_url)
    Sys.sleep(runif(1, 1, 2))
    navigate(h2h_url)
    Sys.sleep(runif(1, 2, 3))
    wait_for_page(15)
    dismiss_cookie()
    Sys.sleep(1)
    click_show_more()
    page <- get_source()
    if (is.null(page)) return(list(home_form = list(), away_form = list(), h2h = list()))
    sections <- page %>% html_nodes("div.h2h__section")
    out <- list(home_form = list(), away_form = list(), h2h = list())
    for (i in seq_along(sections)) {
      sec <- sections[[i]]
      rows <- sec %>% html_nodes("a.h2h__row")
      parsed <- lapply(rows, parse_h2h_row)
      for (k in seq_along(parsed)) {
        cf <- parsed[[k]]$competition_full %||% ""
        parsed[[k]]$is_main_league <- grepl(league_name_for_filter, cf, ignore.case = TRUE)
      }
      if (i == 1)      out$home_form <- head(parsed, 7)
      else if (i == 2) out$away_form <- head(parsed, 7)
      else if (i == 3) out$h2h       <- head(parsed, 5)
    }
    out
  }
  
  # Start chromedriver
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
    return(replicate(nrow(fixtures_df), NULL, simplify = FALSE))
  }
  navigate(paste0(base_url, "/"))
  Sys.sleep(3); wait_for_page(30); dismiss_cookie(); Sys.sleep(2)
  
  progress_path <- file.path(progress_dir, paste0("worker_h_", worker_id, "_progress.txt"))
  results <- list()
  total <- nrow(fixtures_df)
  for (i in seq_len(total)) {
    fx <- fixtures_df[i, ]
    results[[i]] <- tryCatch(scrape_h2h(fx$match_url, fx$league),
                             error = function(e) NULL)
    writeLines(paste0(i, "/", total), progress_path)
  }
  writeLines(paste0(total, "/", total, " DONE"), progress_path)
  
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  results
}

run_parallel_worker <- function(worker_id, work_chunk, chrome_path, chromedriver_path,
                                base_url, progress_dir, cache_version,
                                wanted_stats,
                                no_stats_leagues = character(),
                                no_stats_threshold = 2L) {
  # Re-import libs in worker process
  library(rvest); library(httr); library(jsonlite); library(dplyr)
  
  port <- 20000 + worker_id * 137 + sample(1:999, 1)
  session_id <- NULL
  
  # Local helpers (self-contained)
  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (is.data.frame(a) || is.list(a)) return(a)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }
  slug <- function(x) { x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_|_$", "", x) }
  
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
  wait_for_page <- function(max_wait = 20) {
    for (i in seq_len(max_wait)) {
      Sys.sleep(1)
      title <- js_eval("return document.title;")
      title <- if (is.null(title)) "" else as.character(title)[1]
      if (length(title) == 1 && !grepl("Just a moment|Checking", title, ignore.case = TRUE) &&
          nchar(title) > 0) return(TRUE)
    }
    FALSE
  }
  dismiss_cookie <- function() {
    js_eval("var b=document.querySelectorAll('button,a');var k=['Reject','Decline','Accept','Agree'];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}")
    Sys.sleep(0.5)
  }
  get_html <- function(url) {
    Sys.sleep(runif(1, 0.4, 0.9))
    navigate(url)
    Sys.sleep(runif(1, 1, 1.6))
    wait_for_page(max_wait = 12)
    dismiss_cookie()
    Sys.sleep(0.3)
    get_source()
  }
  
  # Scrape summary page (goal times)
  scrape_summary <- function(match_url) {
    page <- tryCatch(get_html(match_url), error = function(e) NULL)
    if (is.null(page)) return(list(goal_times_home = list(), goal_times_away = list(),
                                   ht_home = NA_integer_, ht_away = NA_integer_,
                                   ft_home = NA_integer_, ft_away = NA_integer_))
    extract <- function(rows) {
      t <- character()
      for (n in rows) {
        gid <- n %>% html_node("div.smv__incidentIcon")
        if (is.null(gid) || length(gid) == 0) next
        gsvg <- gid %>% html_node("svg[data-testid='wcl-icon-incidents-goal-soccer']")
        if (is.null(gsvg) || length(gsvg) == 0) next
        tm <- n %>% html_node("div.smv__timeBox") %>% html_text(trim = TRUE)
        if (!is.na(tm) && nchar(tm) > 0) t <- c(t, tm)
      }
      t
    }
    home_rows <- page %>% html_nodes("div.smv__participantRow.smv__homeParticipant")
    away_rows <- page %>% html_nodes("div.smv__participantRow.smv__awayParticipant")
    ht_home <- NA_integer_; ht_away <- NA_integer_
    ft_home <- NA_integer_; ft_away <- NA_integer_
    blocks <- page %>% html_nodes("[data-testid='wcl-headerSection-text']")
    for (b in blocks) {
      spans <- b %>% html_nodes("span")
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
    # FT from the main detailScore header (always present on a finished match summary).
    hdr <- page %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
    if (length(hdr) > 0) {
      txt <- html_text(hdr[1], trim = TRUE)
      nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
      if (length(nums) >= 2) {
        ft_home <- as.integer(nums[1]); ft_away <- as.integer(nums[2])
      }
    }
    list(goal_times_home = as.list(extract(home_rows)),
         goal_times_away = as.list(extract(away_rows)),
         ht_home = ht_home, ht_away = ht_away,
         ft_home = ft_home, ft_away = ft_away)
  }
  
  # Scrape stats page (overall + 1st-half + 2nd-half)
  scrape_stats <- function(match_url, skip_stats = FALSE) {
    build_half <- function(mu, half) {
      seg <- paste0("summary/stats/", half, "/")
      if (grepl("summary/stats/", mu)) sub("summary/stats/[^/]+/", seg, mu)
      else sub("(\\?mid=)", paste0(seg, "\\1"), mu)
    }
    parse_tab <- function(page) {
      home <- list(); away <- list()
      if (is.null(page)) return(list(home = home, away = away))
      rows <- page %>% html_nodes("div.wcl-row_2oCpS")
      for (r in rows) {
        cn <- r %>% html_node("div.wcl-category_6sT1J span")
        if (is.null(cn)) next
        cat_name <- html_text(cn, trim = TRUE)
        if (!(cat_name %in% wanted_stats)) next
        vals <- r %>% html_nodes("div.wcl-value_XJG99 span.wcl-bold_NZXv6")
        if (length(vals) < 2) next
        key <- slug(cat_name)
        home[[key]] <- html_text(vals[1], trim = TRUE)
        away[[key]] <- html_text(vals[2], trim = TRUE)
      }
      list(home = home, away = away)
    }
    # Overall stats — only when not skipping. The score header is on this page,
    # but we ALSO get FT from the summary scrape below, so we can skip overall entirely.
    result <- list(stats_home = list(), stats_away = list(),
                   stats_home_1h = list(), stats_away_1h = list(),
                   stats_home_2h = list(), stats_away_2h = list(),
                   ht_home = NA, ht_away = NA, ft_home = NA, ft_away = NA,
                   goal_times_home = list(), goal_times_away = list(),
                   match_url = match_url, status = "UNKNOWN")
    if (!skip_stats) {
      page <- tryCatch(get_html(build_half(match_url, "overall")), error = function(e) NULL)
      if (is.null(page)) return(NULL)
      ov <- parse_tab(page)
      result$stats_home <- ov$home; result$stats_away <- ov$away
      hdr <- page %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
      if (length(hdr) > 0) {
        txt <- html_text(hdr[1], trim = TRUE)
        nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
        if (length(nums) >= 2) {
          result$ft_home <- as.integer(nums[1])
          result$ft_away <- as.integer(nums[2])
          result$status <- "FINAL"
        }
      }
      p1 <- tryCatch(get_html(build_half(match_url, "1st-half")), error = function(e) NULL)
      h1 <- parse_tab(p1); result$stats_home_1h <- h1$home; result$stats_away_1h <- h1$away
      p2 <- tryCatch(get_html(build_half(match_url, "2nd-half")), error = function(e) NULL)
      h2 <- parse_tab(p2); result$stats_home_2h <- h2$home; result$stats_away_2h <- h2$away
    }
    # Goal times + HT + FT always from summary page.
    s <- tryCatch(scrape_summary(match_url), error = function(e)
      list(goal_times_home = list(), goal_times_away = list(),
           ht_home = NA_integer_, ht_away = NA_integer_,
           ft_home = NA_integer_, ft_away = NA_integer_))
    result$goal_times_home <- s$goal_times_home
    result$goal_times_away <- s$goal_times_away
    result$ht_home <- s$ht_home
    result$ht_away <- s$ht_away
    # If we skipped stats, FT also comes from summary scrape
    if (skip_stats) {
      result$ft_home <- s$ft_home %||% NA_integer_
      result$ft_away <- s$ft_away %||% NA_integer_
      if (!is.na(result$ft_home) && !is.na(result$ft_away)) result$status <- "FINAL"
    }
    result
  }
  
  # Start chromedriver
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
    return(list(worker_id = worker_id, success = FALSE, error = "Failed to start chromedriver",
                results = list()))
  }
  
  # Warm up
  navigate(paste0(base_url, "/"))
  Sys.sleep(4)
  wait_for_page(30)
  dismiss_cookie()
  Sys.sleep(2)
  
  # Process chunk, checkpoint every 25
  results <- list()
  checkpoint_path <- file.path(progress_dir, paste0("worker_", worker_id, ".rds"))
  progress_path   <- file.path(progress_dir, paste0("worker_", worker_id, "_progress.txt"))
  
  # Resume from existing checkpoint if present
  if (file.exists(checkpoint_path)) {
    results <- tryCatch(readRDS(checkpoint_path), error = function(e) list())
  }
  
  total <- length(work_chunk)
  # In-memory tracker: league_name -> consecutive empty-stats main-league count.
  empty_counts <- list()
  # Local skip set seeded from main process + grown during this worker's run.
  local_skip <- new.env(parent = emptyenv())
  for (lg in no_stats_leagues) assign(lg, TRUE, envir = local_skip)
  newly_flagged_path <- file.path(progress_dir, paste0("nostats_w_", worker_id, ".txt"))
  flag_league <- function(league_name) {
    assign(league_name, TRUE, envir = local_skip)
    # Append to worker's newly-flagged file; main process aggregates.
    cat(league_name, "\n", file = newly_flagged_path, append = TRUE)
  }
  
  for (i in seq_along(work_chunk)) {
    w <- work_chunk[[i]]
    if (!is.null(results[[w$match_id]])) {
      writeLines(paste0(i, "/", total, " (cached in checkpoint)"), progress_path)
      next
    }
    comp <- as.character(w$competition_full %||% "")
    is_main <- isTRUE(w$is_main_league)
    skip_stats <- nzchar(comp) && exists(comp, envir = local_skip, inherits = FALSE)
    data <- tryCatch(scrape_stats(w$match_url, skip_stats = skip_stats),
                     error = function(e) NULL)
    if (!is.null(data)) {
      data$cache_version <- cache_version
      data$cached_at <- Sys.time()
      results[[w$match_id]] <- data
      # If this was a main-league match and we DID try to scrape stats,
      # check if all three stats blocks came back empty. If so, count toward
      # the threshold. When threshold reached, flag the league.
      if (is_main && !skip_stats && nzchar(comp) &&
          length(data$stats_home) == 0 && length(data$stats_away) == 0 &&
          length(data$stats_home_1h) == 0 && length(data$stats_away_1h) == 0 &&
          length(data$stats_home_2h) == 0 && length(data$stats_away_2h) == 0) {
        prev <- empty_counts[[comp]] %||% 0L
        empty_counts[[comp]] <- prev + 1L
        if (empty_counts[[comp]] >= no_stats_threshold &&
            !exists(comp, envir = local_skip, inherits = FALSE)) {
          flag_league(comp)
        }
      }
    }
    writeLines(paste0(i, "/", total), progress_path)
    if (i %% 25 == 0) saveRDS(results, checkpoint_path)
  }
  saveRDS(results, checkpoint_path)
  writeLines(paste0(total, "/", total, " DONE"), progress_path)
  
  # Cleanup
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  
  list(worker_id = worker_id, success = TRUE, error = NULL, results = results,
       checkpoint_path = checkpoint_path)
}

# ════════════════════════════════════════════════════════════
# UI
# ════════════════════════════════════════════════════════════

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,700;9..144,900&family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML("
      *, *::before, *::after { margin:0; padding:0; box-sizing:border-box; }
      :root {
        --ivory:#FAF7F2;
        --ivory-2:#F5EEDF;
        --cobalt:#1E3A8A;
        --cobalt-deep:#0F1F4D;
        --gold:#C9A14A;
        --gold-deep:#8C6A1E;
        --text:#0F1F4D;
        --text-2:#5c5448;
        --text-3:#8e8678;
        --border:#e8dfc9;
        --white:#ffffff;
        --success:#0F6E56;
        --success-bg:#E1F5EE;
        --danger:#A32D2D;
        --danger-bg:#FCEBEB;
        --warning:#854F0B;
        --warning-bg:#FAEEDA;
        --fh:'Fraunces',Georgia,serif;
        --fb:'Inter',system-ui,sans-serif;
        --fm:'JetBrains Mono',monospace;
      }
      body { background:var(--ivory); color:var(--text); font-family:var(--fb); }

      /* HEADER */
      .moab-hd { background:var(--cobalt); color:var(--ivory);
                  padding:24px 36px; display:flex; align-items:center;
                  justify-content:space-between; position:relative; overflow:hidden;
                  border-bottom:3px solid var(--gold); }
      .moab-hd::before { content:''; position:absolute; right:-40px; top:-40px;
                          width:200px; height:200px; opacity:0.12;
                          background:radial-gradient(circle, var(--gold) 0%, transparent 70%); }
      .moab-brand { display:flex; align-items:baseline; gap:18px; position:relative; z-index:1; }
      .moab-logo { font-family:var(--fh); font-size:38px; font-weight:900;
                    letter-spacing:-0.5px; line-height:1; color:var(--ivory); }
      .moab-logo .dot { color:var(--gold); }
      .moab-sub { font-family:var(--fb); font-size:10px; font-weight:600;
                    letter-spacing:4px; text-transform:uppercase;
                    color:rgba(250,247,242,0.75); }
      .moab-meta { font-family:var(--fm); font-size:11px; color:rgba(250,247,242,0.8);
                    position:relative; z-index:1; text-align:right; }
      .moab-meta-l { color:var(--gold); font-weight:500; letter-spacing:1px;
                      text-transform:uppercase; font-size:9px; margin-bottom:4px; }

      /* PAGE */
      .pg { max-width:1500px; margin:0 auto; padding:32px 36px; }

      /* TABS */
      .nav-tabs { border:none !important;
                  border-bottom:1px solid var(--border) !important; margin-bottom:32px !important; }
      .nav-tabs > li > a { font-family:var(--fb) !important; font-size:11px !important;
                            font-weight:600 !important; letter-spacing:2.5px !important;
                            text-transform:uppercase !important; color:var(--text-3) !important;
                            border:none !important; padding:14px 28px !important;
                            border-bottom:2px solid transparent !important;
                            background:transparent !important; }
      .nav-tabs > li.active > a { color:var(--cobalt) !important;
                                   border-bottom-color:var(--gold) !important; }

      /* CARDS */
      .card-elite { background:var(--white); border:1px solid var(--border);
                      border-radius:6px; padding:28px 30px; margin-bottom:18px;
                      position:relative; box-shadow:0 1px 3px rgba(15,31,77,0.04); }
      .card-elite::before { content:''; position:absolute; top:0; left:0; width:48px; height:2px;
                              background:var(--gold); }
      .ct { font-family:var(--fb); font-size:10px; font-weight:700; letter-spacing:3px;
            text-transform:uppercase; color:var(--text-3); margin-bottom:16px; }
      .card-eyebrow { font-family:var(--fb); font-size:9px; letter-spacing:3px;
                       text-transform:uppercase; color:var(--gold-deep); font-weight:700; }

      /* BUTTONS */
      .btn-primary-moab { font-family:var(--fb) !important; font-size:11px !important;
                          font-weight:600 !important; letter-spacing:2px !important;
                          text-transform:uppercase !important;
                          background:var(--cobalt) !important; color:var(--ivory) !important;
                          border:none !important; border-radius:3px !important;
                          padding:12px 28px !important; cursor:pointer !important;
                          box-shadow:0 2px 8px rgba(30,58,138,0.18) !important;
                          transition:all 0.2s !important; }
      .btn-primary-moab:hover { background:var(--cobalt-deep) !important;
                                 box-shadow:0 4px 16px rgba(30,58,138,0.3) !important; }
      .btn-outline-moab { font-family:var(--fb) !important; font-size:10px !important;
                          font-weight:600 !important; letter-spacing:2px !important;
                          text-transform:uppercase !important; background:transparent !important;
                          color:var(--cobalt) !important;
                          border:1px solid var(--cobalt) !important;
                          border-radius:3px !important; padding:9px 22px !important;
                          cursor:pointer !important; }
      .btn-outline-moab:hover { background:var(--cobalt) !important; color:var(--ivory) !important; }
      .btn-gold { font-family:var(--fb) !important; font-size:10px !important;
                  font-weight:600 !important; letter-spacing:2px !important;
                  text-transform:uppercase !important;
                  background:var(--gold) !important; color:var(--cobalt-deep) !important;
                  border:none !important; border-radius:3px !important;
                  padding:9px 22px !important; cursor:pointer !important; }
      .btn-gold:hover { background:var(--gold-deep) !important; color:var(--ivory) !important; }

      /* KPI */
      .kpi-grid { display:grid; grid-template-columns:repeat(4,1fr); gap:14px; margin-bottom:24px; }
      .kpi-card { background:var(--white); border:1px solid var(--border); border-radius:6px;
                  padding:22px 24px; position:relative; }
      .kpi-card::before { content:''; position:absolute; top:0; left:0; width:32px; height:2px;
                            background:var(--cobalt); }
      .kpi-num { font-family:var(--fh); font-size:40px; font-weight:900; line-height:1;
                  color:var(--cobalt); letter-spacing:-1px; }
      .kpi-lab { font-family:var(--fm); font-size:9px; font-weight:500; letter-spacing:2.5px;
                  text-transform:uppercase; color:var(--text-3); margin-top:10px; }

      /* TABLES */
      table.dataTable thead th { background:var(--ivory-2) !important; font-size:10px !important;
                                  font-weight:700 !important; letter-spacing:2px !important;
                                  text-transform:uppercase !important; color:var(--text-2) !important;
                                  font-family:var(--fb) !important; padding:14px 16px !important;
                                  border-bottom:1px solid var(--border) !important; }
      table.dataTable tbody td { font-size:13px !important; padding:11px 16px !important;
                                  font-family:var(--fb) !important;
                                  border-bottom:1px solid var(--border) !important; }
      table.dataTable tbody tr:hover td { background:var(--ivory-2) !important; cursor:pointer; }

      /* INPUTS */
      .form-control, .selectize-input { font-family:var(--fb) !important; font-size:13px !important;
                                          border:1px solid var(--border) !important;
                                          border-radius:3px !important;
                                          padding:8px 12px !important; }
      .shiny-input-container label { font-family:var(--fb) !important; font-size:10px !important;
                                       font-weight:600 !important; letter-spacing:2px !important;
                                       text-transform:uppercase !important;
                                       color:var(--text-2) !important; margin-bottom:6px !important; }

      /* LOG */
      .log-pane { background:var(--cobalt-deep); border-radius:4px; padding:16px 18px;
                   font-family:var(--fm); font-size:11px; color:var(--gold);
                   height:280px; overflow-y:auto; white-space:pre-wrap; line-height:1.8; }

      /* BADGES */
      .pill { display:inline-block; font-family:var(--fm); font-size:10px; font-weight:500;
              letter-spacing:1px; padding:4px 12px; border-radius:12px; border:1px solid; }
      .pill-gold { background:#FDF7E7; color:var(--gold-deep); border-color:#E5D4A0; }
      .pill-cobalt { background:#E8EEF8; color:var(--cobalt-deep); border-color:#BCC8E0; }
      .pill-success { background:var(--success-bg); color:var(--success); border-color:#9FE1CB; }
      .pill-danger { background:var(--danger-bg); color:var(--danger); border-color:#F7C1C1; }
      .pill-warning { background:var(--warning-bg); color:var(--warning); border-color:#FAC775; }

      /* PROGRESS */
      .progress-track { width:100%; height:8px; background:var(--ivory-2);
                         border-radius:4px; overflow:hidden; margin:12px 0; }
      .progress-fill { height:100%; background:linear-gradient(90deg, var(--cobalt) 0%, var(--gold) 100%);
                        transition:width 0.3s; }

      /* SECTION LABEL */
      .section-lab { font-family:var(--fh); font-size:24px; font-weight:700;
                      color:var(--cobalt-deep); letter-spacing:-0.5px; margin-bottom:6px; }
      .section-sub { font-family:var(--fb); font-size:13px; color:var(--text-3);
                      margin-bottom:24px; }

      /* SHINY NOTIFICATIONS */
      .shiny-notification { font-family:var(--fb) !important; border-radius:4px !important;
                             border:1px solid var(--border) !important; }
    "))
  ),
  div(class = "moab-hd",
      div(class = "moab-brand",
          div(class = "moab-logo", "M.O", tags$span(class = "dot", "."), "A.B"),
          div(class = "moab-sub", "Mother of all Boards")),
      div(class = "moab-meta",
          div(class = "moab-meta-l", "Football intelligence · v2"),
          textOutput("hdr_status", inline = TRUE))),
  div(class = "pg",
      tabsetPanel(id = "main_tabs",
                  # ─────────────── PIPELINE ───────────────
                  tabPanel("Pipeline",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Scrape pipeline"),
                               div(class = "section-sub",
                                   "Fetch fixtures, standings, form and head-to-head across all verified leagues."),
                               uiOutput("pipeline_kpis"),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Controls"),
                                   div(style = "display:flex; gap:14px; align-items:center; margin-top:14px;",
                                       actionButton("fetch_btn", "Fetch data", class = "btn-primary-moab"),
                                       actionButton("reload_btn", "Reload from disk", class = "btn-outline-moab"),
                                       div(style = "flex:1;"),
                                       checkboxInput("force_refresh", "Force refresh cache", value = FALSE))
                               ),
                               uiOutput("live_progress_card"),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Activity log"),
                                   div(class = "log-pane", textOutput("log_text"))
                               )
                           )
                  ),
                  # ─────────────── DATA ───────────────
                  tabPanel("Data",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Enriched fixtures"),
                               div(class = "section-sub",
                                   "Browse fixtures with form, head-to-head, and per-match statistics."),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Downloads"),
                                   div(style = "display:flex; gap:10px; flex-wrap:wrap; margin-top:14px;",
                                       downloadButton("dl_fixtures_xlsx", "Fixtures (XLSX)", class = "btn-outline-moab"),
                                       downloadButton("dl_fixtures_rds", "Fixtures (RDS)", class = "btn-outline-moab"),
                                       downloadButton("dl_standings_xlsx", "Standings (XLSX)", class = "btn-outline-moab"),
                                       downloadButton("dl_standings_rds", "Standings (RDS)", class = "btn-outline-moab"),
                                       downloadButton("dl_enriched_rds", "Enriched (RDS)", class = "btn-outline-moab"))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Fixtures in window"),
                                   div(style = "margin-top:14px;", DTOutput("fixtures_table"))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Standings preview"),
                                   div(style = "margin-top:14px;", DTOutput("standings_table"))
                               )
                           )
                  ),
                  # ─────────────── PREDICTIONS ───────────────
                  tabPanel("Predictions",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Market predictions"),
                               div(class = "section-sub",
                                   "Multi-market analysis: Phase 1 goal markets, Phase 2 stats markets, Phase 3 derivatives."),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Data source"),
                                   div(style = "display:flex; gap:14px; align-items:end; margin-top:14px; flex-wrap:wrap;",
                                       div(style = "flex:1; min-width:280px;",
                                           fileInput("upload_enriched_pred", "Upload an enriched_fixtures.rds (optional)",
                                                     accept = ".rds", width = "100%")),
                                       actionButton("reset_enriched_pred", "Reset to live cache", class = "btn-outline-moab")),
                                   div(style = "margin-top:8px; font-family:var(--fm); font-size:11px; color:var(--text-3);",
                                       textOutput("enriched_source_status", inline = TRUE))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Controls"),
                                   div(style = "display:flex; gap:14px; align-items:center; margin-top:14px; flex-wrap:wrap;",
                                       actionButton("run_pred_btn", "Run analysis", class = "btn-primary-moab"),
                                       div(style = "flex:1;"),
                                       downloadButton("dl_predictions_xlsx", "Download (XLSX)", class = "btn-outline-moab"),
                                       downloadButton("dl_predictions_rds", "Download (RDS)", class = "btn-outline-moab")),
                                   div(style = "margin-top:12px; font-family:var(--fm); font-size:11px; color:var(--text-3);",
                                       textOutput("pred_status", inline = TRUE))
                               ),
                               uiOutput("predictions_summary"),
                               uiOutput("predictions_tables")
                           )
                  ),
                  # ─────────────── SETTINGS ───────────────
                  tabPanel("Settings",
                           div(style = "padding-top:8px;",
                               div(class = "section-lab", "Settings"),
                               div(class = "section-sub", "Paths, cache, and analysis thresholds."),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Storage paths"),
                                   tags$table(style = "width:100%; margin-top:12px; font-family:var(--fm); font-size:12px;",
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3); width:200px;", "Base path"),
                                                      tags$td(style = "padding:8px 0;", BASE_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Cache"),
                                                      tags$td(style = "padding:8px 0;", CACHE_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Directory"),
                                                      tags$td(style = "padding:8px 0;", DIRECTORY_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Enriched"),
                                                      tags$td(style = "padding:8px 0;", ENRICHED_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Standings"),
                                                      tags$td(style = "padding:8px 0;", STANDINGS_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Fixtures"),
                                                      tags$td(style = "padding:8px 0;", FIXTURES_PATH)),
                                              tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Analysis"),
                                                      tags$td(style = "padding:8px 0;", ANALYSIS_PATH)))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Cache statistics"),
                                   div(style = "margin-top:14px;", uiOutput("cache_stats"))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Analysis thresholds"),
                                   div(style = "margin-top:14px;", uiOutput("threshold_inputs")),
                                   div(style = "margin-top:14px; display:flex; gap:12px;",
                                       actionButton("save_thresh_btn", "Save thresholds", class = "btn-primary-moab"),
                                       actionButton("reset_thresh_btn", "Reset to defaults", class = "btn-outline-moab"))
                               ),
                               div(class = "card-elite",
                                   div(class = "card-eyebrow", "Maintenance"),
                                   div(style = "margin-top:14px; display:flex; gap:12px;",
                                       actionButton("clear_cache_btn", "Clear cache", class = "btn-outline-moab"),
                                       actionButton("clear_enriched_btn", "Clear enriched fixtures", class = "btn-outline-moab"))
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
    log = "Ready. Click Fetch data to begin.\n",
    directory = NULL,
    fixtures  = NULL,
    standings = NULL,
    enriched  = NULL,
    cache     = NULL,
    last_run  = NULL,
    predictions = NULL,
    pred_ran_at = NULL,
    thresholds = NULL,
    # Live-progress tracking
    scrape_active = FALSE,
    scrape_phase  = NULL,
    scrape_start  = NULL,
    scrape_total_work = 0,
    scrape_progress_pattern = "worker_",
    # Rolling rate samples: list of list(ts=POSIXct, done=int)
    scrape_samples = list(),
    # Predictions override: optional uploaded enriched fixtures
    enriched_override = NULL,
    enriched_override_name = NULL
  )
  
  log_msg <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log <- paste0(rv$log, "[", ts, "] ", msg, "\n")
  }
  
  # Load existing files ONCE on startup using isolate so it doesn't re-fire
  isolate({
    if (file.exists(DIRECTORY_PATH))
      rv$directory <- tryCatch(readRDS(DIRECTORY_PATH), error = function(e) NULL)
    if (file.exists(ENRICHED_PATH))
      rv$enriched <- tryCatch(readRDS(ENRICHED_PATH), error = function(e) NULL)
    if (file.exists(STANDINGS_PATH))
      rv$standings <- tryCatch(readRDS(STANDINGS_PATH), error = function(e) NULL)
    if (file.exists(FIXTURES_PATH))
      rv$fixtures <- tryCatch(readRDS(FIXTURES_PATH), error = function(e) NULL)
    rv$cache <- load_cache()
    dir_n <- if (is.null(rv$directory)) 0 else nrow(rv$directory)
    enr_n <- if (is.null(rv$enriched)) 0 else length(rv$enriched)
    log_msg(paste0("Loaded directory (", dir_n, " entries), ",
                   "cache (", length(rv$cache), " matches), ",
                   "enriched (", enr_n, " fixtures)."))
    # Thresholds: start from defaults in analysis module, override from disk
    rv$thresholds <- if (exists("DEFAULT_MOAB_THRESHOLDS"))
      DEFAULT_MOAB_THRESHOLDS else list()
    if (file.exists(SETTINGS_PATH)) {
      saved <- tryCatch(fromJSON(SETTINGS_PATH, simplifyVector = FALSE),
                        error = function(e) NULL)
      if (is.list(saved)) {
        for (k in names(saved)) rv$thresholds[[k]] <- saved[[k]]
        log_msg(paste0("Loaded ", length(saved), " threshold overrides from disk."))
      }
    }
  })
  
  output$log_text   <- renderText({ rv$log })
  output$hdr_status <- renderText({
    if (!is.null(rv$enriched)) paste0(length(rv$enriched), " fixtures · ",
                                      length(rv$cache), " cached matches")
    else paste0(length(rv$cache), " cached matches")
  })
  
  output$pipeline_kpis <- renderUI({
    verified <- if (!is.null(rv$directory))
      sum(rv$directory$verified == TRUE, na.rm = TRUE) else 0
    div(class = "kpi-grid",
        div(class = "kpi-card", div(class = "kpi-num", verified),
            div(class = "kpi-lab", "Verified leagues")),
        div(class = "kpi-card", div(class = "kpi-num", length(rv$cache)),
            div(class = "kpi-lab", "Cached matches")),
        div(class = "kpi-card",
            div(class = "kpi-num", length(rv$enriched %||% list())),
            div(class = "kpi-lab", "Enriched fixtures")),
        div(class = "kpi-card",
            div(class = "kpi-num", style = "font-size:16px; padding-top:14px;",
                if (is.null(rv$last_run)) "Never"
                else format(rv$last_run, "%d %b %H:%M")),
            div(class = "kpi-lab", "Last run")))
  })
  
  # ─── LIVE PROGRESS: re-reads worker files on a 3s timer so the UI updates
  # independently of the blocking polling loop. Hidden when no scrape active. ───
  live_tick <- reactiveTimer(3000)
  
  format_duration <- function(secs) {
    if (is.na(secs) || secs < 0 || !is.finite(secs)) return("--")
    h <- floor(secs / 3600); m <- floor((secs %% 3600) / 60); s <- floor(secs %% 60)
    if (h > 0) sprintf("%dh %02dm", h, m)
    else if (m > 0) sprintf("%dm %02ds", m, s)
    else sprintf("%ds", s)
  }
  
  output$live_progress_card <- renderUI({
    live_tick()  # refresh trigger
    if (!isTRUE(rv$scrape_active)) return(NULL)
    
    # Read every worker_*.txt file in WORKER_PROGRESS_DIR to compute live state
    # Build the per-phase glob pattern (Phase A uses worker_a_, H2H worker_h_, C worker_)
    pat <- rv$scrape_progress_pattern %||% "worker_"
    # Match e.g. ^worker_\d+_progress\.txt$  or ^worker_a_\d+_progress\.txt$
    glob <- paste0("^", pat, "\\d+_progress\\.txt$")
    files <- list.files(WORKER_PROGRESS_DIR, pattern = glob, full.names = TRUE)
    if (length(files) == 0) {
      return(div(class = "card-elite",
                 div(class = "card-eyebrow", "Live progress"),
                 div(style = "font-family:var(--fm); font-size:12px; color:var(--text-3); margin-top:10px;",
                     "Waiting for workers to start...")))
    }
    
    n_workers <- length(files)
    total_done <- 0; total_target <- 0
    done_workers <- 0
    per_worker_rows <- list()
    for (idx in seq_along(files)) {
      pf <- files[idx]
      wnum <- as.integer(sub(".*worker_(\\d+).*", "\\1", basename(pf)))
      txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
      m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
      a <- 0; b <- 0
      if (length(m) > 0) {
        parts <- as.integer(strsplit(m, "/")[[1]])
        if (length(parts) == 2) { a <- parts[1]; b <- parts[2] }
      }
      is_done <- grepl("DONE", txt)
      if (is_done) done_workers <- done_workers + 1
      total_done <- total_done + a
      total_target <- total_target + b
      pct <- if (b > 0) round(a / b * 100) else 0
      status_color <- if (is_done) "var(--success)" else if (a > 0) "var(--cobalt)" else "var(--text-3)"
      per_worker_rows[[idx]] <- tags$tr(
        tags$td(style = "padding:6px 10px; font-family:var(--fm); font-size:11px; color:var(--text-3);",
                paste0("W", wnum)),
        tags$td(style = "padding:6px 10px; font-family:var(--fm); font-size:11px;",
                paste0(a, " / ", b)),
        tags$td(style = paste0("padding:6px 10px; font-family:var(--fm); font-size:11px; color:", status_color, "; font-weight:600;"),
                if (is_done) "DONE" else paste0(pct, "%")))
    }
    
    pct_overall <- if (total_target > 0) total_done / total_target else 0
    elapsed_secs <- as.numeric(difftime(Sys.time(), rv$scrape_start, units = "secs"))
    
    # Rolling rate from recent samples (last ~5 min worth)
    samples <- rv$scrape_samples
    rate_per_sec <- NA_real_
    if (length(samples) >= 2) {
      # Use last min(20, length) samples for smoothing
      tail_n <- min(20, length(samples))
      tail_samples <- samples[(length(samples) - tail_n + 1):length(samples)]
      first <- tail_samples[[1]]; last <- tail_samples[[length(tail_samples)]]
      dt <- as.numeric(difftime(last$ts, first$ts, units = "secs"))
      dd <- last$done - first$done
      if (dt > 0 && dd > 0) rate_per_sec <- dd / dt
    }
    
    remaining <- max(0, total_target - total_done)
    eta_secs <- if (!is.na(rate_per_sec) && rate_per_sec > 0) remaining / rate_per_sec else NA_real_
    eta_clock <- if (!is.na(eta_secs)) format(Sys.time() + eta_secs, "%a %H:%M") else "--"
    rate_per_min <- if (!is.na(rate_per_sec)) round(rate_per_sec * 60, 1) else NA
    
    div(class = "card-elite",
        div(class = "card-eyebrow", paste0("Live progress \u00b7 ", rv$scrape_phase %||% "")),
        # Top row: progress bar
        div(style = "margin-top:14px;",
            div(style = paste0("height:8px; background:var(--ivory-2); border-radius:4px; overflow:hidden;"),
                div(style = paste0("height:100%; width:", round(pct_overall * 100, 1), "%; background:var(--cobalt); transition:width 0.5s;"))),
            div(style = "display:flex; justify-content:space-between; font-family:var(--fm); font-size:11px; margin-top:6px; color:var(--text-3);",
                tags$span(paste0(total_done, " / ", total_target, " matches \u00b7 ", round(pct_overall * 100, 1), "%")),
                tags$span(paste0(done_workers, "/", n_workers, " workers done")))),
        # Stats grid
        div(style = "display:grid; grid-template-columns:repeat(4,1fr); gap:14px; margin-top:18px;",
            div(div(style = "font-family:var(--fm); font-size:9px; letter-spacing:2px; text-transform:uppercase; color:var(--text-3);", "Elapsed"),
                div(style = "font-family:var(--fh); font-size:22px; font-weight:800; color:var(--cobalt); margin-top:4px;",
                    format_duration(elapsed_secs))),
            div(div(style = "font-family:var(--fm); font-size:9px; letter-spacing:2px; text-transform:uppercase; color:var(--text-3);", "Rate"),
                div(style = "font-family:var(--fh); font-size:22px; font-weight:800; color:var(--cobalt); margin-top:4px;",
                    if (is.na(rate_per_min)) "--" else paste0(rate_per_min, "/min"))),
            div(div(style = "font-family:var(--fm); font-size:9px; letter-spacing:2px; text-transform:uppercase; color:var(--text-3);", "ETA duration"),
                div(style = "font-family:var(--fh); font-size:22px; font-weight:800; color:var(--cobalt); margin-top:4px;",
                    format_duration(eta_secs))),
            div(div(style = "font-family:var(--fm); font-size:9px; letter-spacing:2px; text-transform:uppercase; color:var(--text-3);", "ETA finish"),
                div(style = "font-family:var(--fh); font-size:22px; font-weight:800; color:var(--cobalt); margin-top:4px;",
                    eta_clock))),
        # Per-worker breakdown
        div(style = "margin-top:18px; border-top:1px solid var(--border); padding-top:12px;",
            div(style = "font-family:var(--fb); font-size:10px; font-weight:700; letter-spacing:2px; text-transform:uppercase; color:var(--text-3); margin-bottom:6px;",
                "Per-worker"),
            tags$table(style = "width:100%; border-collapse:collapse;",
                       tags$tbody(do.call(tagList, per_worker_rows)))))
  })
  
  output$cache_stats <- renderUI({
    n <- length(rv$cache)
    sz <- if (file.exists(CACHE_PATH))
      paste0(round(file.info(CACHE_PATH)$size / 1024, 1), " KB")
    else "0 KB"
    tags$table(style = "width:100%; font-family:var(--fm); font-size:12px;",
               tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3); width:200px;", "Entries"),
                       tags$td(style = "padding:8px 0;", n)),
               tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "File size"),
                       tags$td(style = "padding:8px 0;", sz)),
               tags$tr(tags$td(style = "padding:8px 0; color:var(--text-3);", "Version"),
                       tags$td(style = "padding:8px 0;", CACHE_VERSION)))
  })
  
  observeEvent(input$reload_btn, {
    if (file.exists(ENRICHED_PATH))
      rv$enriched <- tryCatch(readRDS(ENRICHED_PATH), error = function(e) NULL)
    rv$cache <- load_cache()
    log_msg(paste0("Reloaded from disk: ", length(rv$enriched %||% list()),
                   " fixtures, ", length(rv$cache), " cache entries."))
    showNotification("Reloaded from disk", type = "message", duration = 2)
  })
  
  observeEvent(input$clear_cache_btn, {
    showModal(modalDialog(title = "Clear cache?",
                          "This will delete all cached match data. Subsequent scrapes will re-fetch everything.",
                          footer = tagList(modalButton("Cancel"),
                                           actionButton("clear_cache_confirm", "Clear", class = "btn-primary-moab")),
                          easyClose = TRUE))
  })
  observeEvent(input$clear_cache_confirm, {
    rv$cache <- list()
    if (file.exists(CACHE_PATH)) file.remove(CACHE_PATH)
    log_msg("Cache cleared.")
    removeModal()
    showNotification("Cache cleared", type = "warning", duration = 3)
  })
  
  observeEvent(input$clear_enriched_btn, {
    rv$enriched <- NULL
    if (file.exists(ENRICHED_PATH)) file.remove(ENRICHED_PATH)
    log_msg("Enriched fixtures cleared.")
    showNotification("Enriched cleared", type = "warning", duration = 3)
  })
  
  # ── FETCH ──
  observeEvent(input$fetch_btn, {
    log_msg("Fetch button clicked.")
    showNotification("Pipeline starting...", type = "message", duration = 3)
    if (is.null(rv$directory)) {
      log_msg("ERROR: No league_directory.rds found at " %||% DIRECTORY_PATH)
      log_msg(paste0("ERROR: No league_directory.rds at ", DIRECTORY_PATH))
      showNotification("Directory missing - run directory + verify scripts first",
                       type = "error", duration = 6)
      return()
    }
    log_msg(paste0("Directory loaded: ", nrow(rv$directory), " entries."))
    verified <- rv$directory %>% filter(verified == TRUE, !is.na(verified))
    if (nrow(verified) == 0) {
      log_msg("ERROR: No verified leagues found in directory.")
      showNotification("No verified leagues", type = "error", duration = 4)
      return()
    }
    log_msg(paste0("Verified leagues: ", nrow(verified)))
    today <- Sys.Date(); horizon <- today + 7
    log_msg(paste0("Pipeline started. Targeting ", nrow(verified), " leagues."))
    
    tryCatch({
      # Phase A: parallel fixtures + standings scrape using future workers
      log_msg(paste0("Spawning ", N_PARALLEL_WORKERS,
                     " parallel workers for fixtures + standings..."))
      n_workers_a <- min(N_PARALLEL_WORKERS, nrow(verified))
      league_chunks <- if (n_workers_a <= 1 || nrow(verified) <= 1) {
        list(seq_len(nrow(verified)))
      } else {
        split(seq_len(nrow(verified)),
              cut(seq_len(nrow(verified)), n_workers_a, labels = FALSE))
      }
      
      chrome_p <- CHROME_PATH; cd_p <- CHROMEDRIVER_PATH
      base_u <- BASE_URL; prog_dir <- WORKER_PROGRESS_DIR
      
      plan(multisession, workers = n_workers_a)
      
      for (i in seq_len(n_workers_a)) {
        pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_a_", i, "_progress.txt"))
        if (file.exists(pf)) file.remove(pf)
      }
      
      futures_a <- list()
      for (i in seq_len(n_workers_a)) {
        local_leagues <- verified[league_chunks[[i]], ]
        startup_delay <- (i - 1) * runif(1, 5, 10)
        futures_a[[i]] <- future({
          Sys.sleep(startup_delay)
          run_league_worker(i, local_leagues, chrome_p, cd_p, base_u, prog_dir)
        }, seed = TRUE,
        globals = list(i = i, local_leagues = local_leagues,
                       startup_delay = startup_delay,
                       chrome_p = chrome_p, cd_p = cd_p, base_u = base_u,
                       prog_dir = prog_dir,
                       run_league_worker = run_league_worker))
      }
      
      withProgress(message = "Fixtures + standings (parallel)", value = 0, {
        # ── LIVE-PROGRESS state setup ─────────────────────
        rv$scrape_active <- TRUE
        rv$scrape_phase  <- "Phase A: fixtures + standings"
        rv$scrape_start  <- Sys.time()
        rv$scrape_total_work <- nrow(verified)
        rv$scrape_samples <- list(list(ts = Sys.time(), done = 0))
        rv$scrape_progress_pattern <- "worker_a_"
        done <- rep(FALSE, n_workers_a)
        while (!all(done)) {
          Sys.sleep(5)
          total_progress <- 0
          total_done_units <- 0
          for (i in seq_len(n_workers_a)) {
            pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_a_", i, "_progress.txt"))
            if (file.exists(pf)) {
              txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
              if (grepl("DONE", txt)) done[i] <- TRUE
              m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
              if (length(m) > 0) {
                parts <- as.integer(strsplit(m, "/")[[1]])
                if (length(parts) == 2 && parts[2] > 0) {
                  total_progress <- total_progress + (parts[1] / parts[2])
                  total_done_units <- total_done_units + parts[1]
                }
              }
            }
            if (resolved(futures_a[[i]])) done[i] <- TRUE
          }
          avg <- total_progress / n_workers_a
          new_sample <- list(ts = Sys.time(), done = total_done_units)
          rv$scrape_samples <- c(rv$scrape_samples, list(new_sample))
          if (length(rv$scrape_samples) > 60)
            rv$scrape_samples <- rv$scrape_samples[(length(rv$scrape_samples) - 59):length(rv$scrape_samples)]
          setProgress(value = avg,
                      detail = paste0(round(avg * 100), "% across ", n_workers_a, " workers"))
          tryCatch(shiny:::flushReact(), error = function(e) invisible())
          if (!is.null(session)) tryCatch(session$flushOutput(), error = function(e) invisible())
          if (all(done)) break
        }
      })
      
      log_msg("Workers finished. Merging fixtures and standings...")
      all_fixtures <- data.frame(); all_standings <- data.frame()
      for (i in seq_len(n_workers_a)) {
        res <- tryCatch(value(futures_a[[i]]), error = function(e) {
          log_msg(paste0("Worker ", i, " error: ", e$message))
          list(fixtures = data.frame(), standings = data.frame())
        })
        if (nrow(res$fixtures) > 0) all_fixtures <- rbind(all_fixtures, res$fixtures)
        if (nrow(res$standings) > 0) all_standings <- rbind(all_standings, res$standings)
      }
      plan(sequential)
      log_msg(paste0("Collected fixtures: ", nrow(all_fixtures),
                     " | standings: ", nrow(all_standings)))
      
      rv$fixtures <- all_fixtures
      rv$standings <- all_standings
      # Persist to disk so other tabs / future sessions can use them
      tryCatch(saveRDS(all_fixtures, FIXTURES_PATH), error = function(e) NULL)
      tryCatch(saveRDS(all_standings, STANDINGS_PATH), error = function(e) NULL)
      log_msg(paste0("Collected ", nrow(all_fixtures), " fixtures and ",
                     nrow(all_standings), " standings rows."))
      
      in_window <- all_fixtures %>%
        filter(!is.na(fixture_date), fixture_date >= today, fixture_date <= horizon) %>%
        arrange(fixture_date, match_time)
      log_msg(paste0(nrow(in_window), " fixtures in 7-day window."))
      
      enriched <- list(); cache <- rv$cache
      # Build a flat work queue of (fixture_idx, h2h_data) by first scraping
      # all h2h pages in parallel across tabs, then enriching each form game in parallel.
      if (nrow(in_window) > 0) {
        total_tabs <- length(SEL_POOL) * N_TABS
        log_msg(paste0("Parallel enrichment across ", total_tabs, " tabs (",
                       length(SEL_POOL), " windows x ", N_TABS, " tabs)."))
        
        # Phase H2H: scrape H2H pages in parallel
        log_msg("Phase H2H: scraping H2H pages in parallel...")
        n_workers_h <- min(N_PARALLEL_WORKERS, nrow(in_window))
        fix_chunks <- if (n_workers_h <= 1 || nrow(in_window) <= 1) {
          list(seq_len(nrow(in_window)))
        } else {
          split(seq_len(nrow(in_window)),
                cut(seq_len(nrow(in_window)), n_workers_h, labels = FALSE))
        }
        
        chrome_p <- CHROME_PATH; cd_p <- CHROMEDRIVER_PATH
        base_u <- BASE_URL; prog_dir <- WORKER_PROGRESS_DIR
        
        for (i in seq_len(n_workers_h)) {
          pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_h_", i, "_progress.txt"))
          if (file.exists(pf)) file.remove(pf)
        }
        
        plan(multisession, workers = n_workers_h)
        futures_h <- list()
        for (i in seq_len(n_workers_h)) {
          local_fix <- in_window[fix_chunks[[i]], ]
          startup_delay <- (i - 1) * runif(1, 5, 10)
          futures_h[[i]] <- future({
            Sys.sleep(startup_delay)
            run_h2h_worker(i, local_fix, chrome_p, cd_p, base_u, prog_dir)
          }, seed = TRUE,
          globals = list(i = i, local_fix = local_fix,
                         startup_delay = startup_delay,
                         chrome_p = chrome_p, cd_p = cd_p, base_u = base_u,
                         prog_dir = prog_dir,
                         run_h2h_worker = run_h2h_worker))
        }
        
        withProgress(message = "H2H pages (parallel)", value = 0, {
          rv$scrape_active <- TRUE
          rv$scrape_phase  <- "Phase H2H: head-to-head pages"
          rv$scrape_start  <- Sys.time()
          rv$scrape_total_work <- nrow(in_window)
          rv$scrape_samples <- list(list(ts = Sys.time(), done = 0))
          rv$scrape_progress_pattern <- "worker_h_"
          done <- rep(FALSE, n_workers_h)
          while (!all(done)) {
            Sys.sleep(5)
            total_prog <- 0
            total_done_units <- 0
            for (i in seq_len(n_workers_h)) {
              pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_h_", i, "_progress.txt"))
              if (file.exists(pf)) {
                txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
                if (grepl("DONE", txt)) done[i] <- TRUE
                m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
                if (length(m) > 0) {
                  parts <- as.integer(strsplit(m, "/")[[1]])
                  if (length(parts) == 2 && parts[2] > 0) {
                    total_prog <- total_prog + (parts[1] / parts[2])
                    total_done_units <- total_done_units + parts[1]
                  }
                }
              }
              if (resolved(futures_h[[i]])) done[i] <- TRUE
            }
            avg <- total_prog / n_workers_h
            new_sample <- list(ts = Sys.time(), done = total_done_units)
            rv$scrape_samples <- c(rv$scrape_samples, list(new_sample))
            if (length(rv$scrape_samples) > 60)
              rv$scrape_samples <- rv$scrape_samples[(length(rv$scrape_samples) - 59):length(rv$scrape_samples)]
            setProgress(value = avg,
                        detail = paste0(round(avg * 100), "% across ", n_workers_h, " workers"))
            tryCatch(shiny:::flushReact(), error = function(e) invisible())
            if (!is.null(session)) tryCatch(session$flushOutput(), error = function(e) invisible())
            if (all(done)) break
          }
        })
        
        # Collect H2H results: each worker returns list of (fixture_idx -> h2h data)
        h2h_results <- vector("list", nrow(in_window))
        for (i in seq_len(n_workers_h)) {
          res <- tryCatch(value(futures_h[[i]]), error = function(e) {
            log_msg(paste0("H2H worker ", i, " error: ", e$message))
            list()
          })
          chunk_indices <- fix_chunks[[i]]
          for (j in seq_along(chunk_indices)) {
            h2h_results[[chunk_indices[j]]] <- res[[j]]
          }
        }
        plan(sequential)
        log_msg(paste0("H2H scraping complete: ",
                       sum(sapply(h2h_results, function(x) !is.null(x))),
                       "/", nrow(in_window), " successful."))
        
        # Phase B: collect ALL unique form/h2h match URLs to scrape
        log_msg("Phase B: building work queue of form/h2h match details...")
        work_queue <- list()  # each: list(match_id, match_url, fixture_idx, side, entry_idx, competition_full, is_main_league)
        for (k in seq_len(nrow(in_window))) {
          h <- h2h_results[[k]]
          if (is.null(h)) next
          fx <- in_window[k, ]
          add_to_queue <- function(form_list, side, fx_idx) {
            for (j in seq_along(form_list)) {
              entry <- form_list[[j]]
              if (is.null(entry$match_id) || is.na(entry$match_id)) next
              if (cache_hit(cache, entry$match_id, input$force_refresh)) next
              work_queue[[length(work_queue) + 1]] <<- list(
                match_id = entry$match_id, match_url = entry$match_url,
                fixture_idx = fx_idx, side = side, entry_idx = j,
                competition_full = entry$competition_full %||% NA,
                is_main_league = isTRUE(entry$is_main_league))
            }
          }
          add_to_queue(h$home_form, "home", k)
          add_to_queue(h$away_form, "away", k)
          add_to_queue(h$h2h, "h2h", k)
        }
        log_msg(paste0("Need to scrape ", length(work_queue), " match detail pages (cache hits skipped)."))
        
        # Phase C: parallel scrape via future::multisession workers
        if (length(work_queue) > 0) {
          
          # Split work_queue into N chunks
          n_workers <- min(N_PARALLEL_WORKERS, length(work_queue))
          chunks <- if (n_workers <= 1 || length(work_queue) <= 1) {
            list(work_queue)
          } else {
            split(work_queue,
                  cut(seq_along(work_queue), n_workers, labels = FALSE))
          }
          log_msg(paste0("Spawning ", n_workers, " parallel workers (", 
                         round(length(work_queue) / n_workers),
                         " matches each)..."))
          
          # ── LIVE-PROGRESS state setup ─────────────────────
          rv$scrape_active <- TRUE
          rv$scrape_phase  <- "Phase C: deep match scrape"
          rv$scrape_start  <- Sys.time()
          rv$scrape_total_work <- length(work_queue)
          rv$scrape_samples <- list(list(ts = Sys.time(), done = 0))
          rv$scrape_progress_pattern <- "worker_"
          
          # Set up future plan
          plan(multisession, workers = n_workers)
          
          # Capture vars for workers
          chrome_p <- CHROME_PATH; cd_p <- CHROMEDRIVER_PATH
          base_u <- BASE_URL; prog_dir <- WORKER_PROGRESS_DIR
          cv <- CACHE_VERSION; ws <- WANTED_STATS
          no_stats_lg <- load_no_stats_leagues()
          no_stats_thr <- NO_STATS_THRESHOLD
          log_msg(paste0("Loaded ", length(no_stats_lg),
                         " leagues known to have no stats (their stat pages will be skipped)."))
          
          # Submit all worker jobs
          futures <- list()
          for (i in seq_len(n_workers)) {
            local_chunk <- chunks[[i]]
            futures[[i]] <- future({
              run_parallel_worker(i, local_chunk, chrome_p, cd_p,
                                  base_u, prog_dir, cv, ws,
                                  no_stats_lg, no_stats_thr)
            }, seed = TRUE,
            globals = list(i = i, local_chunk = local_chunk,
                           chrome_p = chrome_p, cd_p = cd_p, base_u = base_u,
                           prog_dir = prog_dir, cv = cv, ws = ws,
                           no_stats_lg = no_stats_lg, no_stats_thr = no_stats_thr,
                           run_parallel_worker = run_parallel_worker))
          }
          
          # Poll progress files while workers run
          withProgress(message = "Parallel workers running", value = 0, {
            done <- rep(FALSE, n_workers)
            while (!all(done)) {
              Sys.sleep(5)
              total_progress <- 0
              total_done_matches <- 0
              for (i in seq_len(n_workers)) {
                pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_", i, "_progress.txt"))
                if (file.exists(pf)) {
                  txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
                  if (grepl("DONE", txt)) done[i] <- TRUE
                  m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
                  if (length(m) > 0) {
                    parts <- as.integer(strsplit(m, "/")[[1]])
                    if (length(parts) == 2 && parts[2] > 0) {
                      total_progress <- total_progress + (parts[1] / parts[2])
                      total_done_matches <- total_done_matches + parts[1]
                    }
                  }
                }
                if (resolved(futures[[i]])) done[i] <- TRUE
              }
              avg <- total_progress / n_workers
              # Append rolling sample (cap at last 60 = ~5 min @ 5s polls)
              new_sample <- list(ts = Sys.time(), done = total_done_matches)
              rv$scrape_samples <- c(rv$scrape_samples, list(new_sample))
              if (length(rv$scrape_samples) > 60)
                rv$scrape_samples <- rv$scrape_samples[(length(rv$scrape_samples) - 59):length(rv$scrape_samples)]
              setProgress(value = avg,
                          detail = paste0(round(avg * 100), "% across ", n_workers, " workers"))
              # Force Shiny to flush reactive state & redraw UI so the live progress
              # card and activity log refresh between polls instead of only at the end.
              tryCatch(shiny:::flushReact(), error = function(e) invisible())
              if (!is.null(session)) tryCatch(session$flushOutput(), error = function(e) invisible())
              if (all(done)) break
            }
          })
          rv$scrape_active <- FALSE
          
          # Collect all worker results
          log_msg("All workers finished. Merging results...")
          all_results <- list()
          for (i in seq_len(n_workers)) {
            res <- tryCatch(value(futures[[i]]), error = function(e) {
              log_msg(paste0("Worker ", i, " error: ", e$message))
              list(success = FALSE, results = list())
            })
            if (!is.null(res$success) && res$success) {
              all_results <- c(all_results, res$results)
              log_msg(paste0("Worker ", i, ": ", length(res$results), " matches scraped"))
            }
          }
          
          # Merge into main cache
          for (mid in names(all_results)) {
            d <- all_results[[mid]]
            if (is_valid_cache_entry(d)) cache[[mid]] <- d
          }
          save_cache(cache)
          
          # Aggregate newly-flagged "no stats" leagues from worker files.
          newly_flagged <- character()
          for (i in seq_len(n_workers)) {
            wf <- file.path(WORKER_PROGRESS_DIR, paste0("nostats_w_", i, ".txt"))
            if (file.exists(wf)) {
              lines <- tryCatch(readLines(wf, warn = FALSE), error = function(e) character())
              newly_flagged <- c(newly_flagged, trimws(lines[nchar(trimws(lines)) > 0]))
              file.remove(wf)
            }
          }
          if (length(newly_flagged) > 0) {
            existing_no_stats <- load_no_stats_leagues()
            combined <- unique(c(existing_no_stats, newly_flagged))
            new_only <- setdiff(combined, existing_no_stats)
            if (length(new_only) > 0) {
              save_no_stats_leagues(combined)
              log_msg(paste0("Flagged ", length(new_only),
                             " new league(s) as having no stats: ",
                             paste(new_only, collapse = ", ")))
            }
          }
          log_msg(paste0("Merged ", length(all_results), " matches into main cache."))
          
          # Cleanup worker progress files but KEEP checkpoint RDS for safety
          for (i in seq_len(n_workers)) {
            pf <- file.path(WORKER_PROGRESS_DIR, paste0("worker_", i, "_progress.txt"))
            if (file.exists(pf)) file.remove(pf)
          }
          
          # Reset future plan
          plan(sequential)
          
          # Workers are done — purge any orphaned Chrome temp dirs they left
          cleanup_temp_chrome_dirs()
          
          # Restart sequential pool for Phase D (stitching uses cache only, no scraping)
          # Actually Phase D doesn't scrape - it just reads cache. No pool needed.
        }
        
        # Phase D: stitch enriched fixtures using cache (which now has everything)
        log_msg("Phase D: stitching enriched fixtures...")
        # Build standings lookup once: (league, team) -> {rank, mp, pts, gd}
        # Plus league_size (count of teams per league). Used to enrich every
        # fixture row with home/away rank, ppg, mp, gd and league_size.
        std_lookup <- list()
        league_sizes <- list()
        if (!is.null(all_standings) && nrow(all_standings) > 0) {
          for (si in seq_len(nrow(all_standings))) {
            lg <- as.character(all_standings$league[si])
            tm <- as.character(all_standings$team[si])
            key <- paste0(lg, "::", tm)
            std_lookup[[key]] <- list(
              rank = all_standings$rank[si],
              mp   = all_standings$mp[si],
              pts  = all_standings$pts[si],
              gd   = all_standings$gd[si]
            )
            league_sizes[[lg]] <- (league_sizes[[lg]] %||% 0L) + 1L
          }
        }
        attach_standings <- function(fixture_row) {
          lg   <- as.character(fixture_row$league %||% NA)
          home <- as.character(fixture_row$home %||% NA)
          away <- as.character(fixture_row$away %||% NA)
          h_rec <- if (!is.na(lg) && !is.na(home)) std_lookup[[paste0(lg, "::", home)]] else NULL
          a_rec <- if (!is.na(lg) && !is.na(away)) std_lookup[[paste0(lg, "::", away)]] else NULL
          fixture_row$home_rank <- if (!is.null(h_rec)) h_rec$rank else NA
          fixture_row$home_mp   <- if (!is.null(h_rec)) h_rec$mp   else NA
          fixture_row$home_pts  <- if (!is.null(h_rec)) h_rec$pts  else NA
          fixture_row$home_gd   <- if (!is.null(h_rec)) h_rec$gd   else NA
          fixture_row$home_ppg  <- if (!is.null(h_rec) && !is.na(h_rec$mp) && h_rec$mp > 0)
            h_rec$pts / h_rec$mp else NA
          fixture_row$away_rank <- if (!is.null(a_rec)) a_rec$rank else NA
          fixture_row$away_mp   <- if (!is.null(a_rec)) a_rec$mp   else NA
          fixture_row$away_pts  <- if (!is.null(a_rec)) a_rec$pts  else NA
          fixture_row$away_gd   <- if (!is.null(a_rec)) a_rec$gd   else NA
          fixture_row$away_ppg  <- if (!is.null(a_rec) && !is.na(a_rec$mp) && a_rec$mp > 0)
            a_rec$pts / a_rec$mp else NA
          fixture_row$league_size <- if (!is.na(lg) && !is.null(league_sizes[[lg]]))
            league_sizes[[lg]] else NA
          fixture_row
        }
        for (k in seq_len(nrow(in_window))) {
          h <- h2h_results[[k]]
          if (is.null(h)) next
          fx <- attach_standings(in_window[k, ])
          enrich_side <- function(form_list, team_name) {
            out <- list()
            for (e in form_list) {
              r <- enrich_form_entry(e, team_name, cache, FALSE)
              out[[length(out) + 1]] <- r$entry
            }
            out
          }
          enriched[[k]] <- list(
            fixture   = fx,
            home_form = enrich_side(h$home_form, fx$home),
            away_form = enrich_side(h$away_form, fx$away),
            h2h       = enrich_side(h$h2h, fx$home)
          )
        }
        saveRDS(enriched, ENRICHED_PATH)
      }
      # Pool already shut down inside Phase C; just save final state
      save_cache(cache); saveRDS(enriched, ENRICHED_PATH)
      rv$cache <- cache; rv$enriched <- enriched; rv$last_run <- Sys.time()
      log_msg(paste0("Pipeline complete. ", length(enriched), " fixtures enriched."))
      showNotification("Pipeline complete", type = "message", duration = 4)
    }, error = function(e) {
      tryCatch(stop_selenium(), error = function(e2) invisible())
      log_msg(paste0("ERROR: ", e$message))
      showNotification(paste0("Error: ", e$message), type = "error", duration = 6)
    })
  })
  
  # ── DATA TABLES ──
  output$fixtures_table <- renderDT({
    req(rv$enriched)
    if (length(rv$enriched) == 0) return(datatable(data.frame(Message = "No data")))
    rows <- lapply(rv$enriched, function(e) {
      if (is.null(e)) return(NULL)
      fx <- e$fixture
      data.frame(
        Date = as.character(fx$fixture_date), Time = fx$match_time,
        League = fx$league, Home = fx$home, Away = fx$away,
        HomeForm = paste0(length(e$home_form), " games"),
        AwayForm = paste0(length(e$away_form), " games"),
        H2H = paste0(length(e$h2h), " games"),
        stringsAsFactors = FALSE)
    })
    d <- do.call(rbind, Filter(Negate(is.null), rows))
    if (is.null(d) || nrow(d) == 0) return(datatable(data.frame(Message = "No data")))
    datatable(d, rownames = FALSE, selection = "none",
              options = list(pageLength = 25, dom = "ftp", scrollX = TRUE))
  }, server = FALSE)
  
  output$standings_table <- renderDT({
    req(rv$standings)
    if (is.null(rv$standings) || nrow(rv$standings) == 0)
      return(datatable(data.frame(Message = "Standings refresh on next fetch")))
    datatable(rv$standings %>%
                select(league, rank, team, mp, w, d, l, goals_for, goals_against, gd, pts, form),
              rownames = FALSE, selection = "none", colnames = c("League","#","Team","MP","W","D","L","GF","GA","GD","Pts","Form"),
              options = list(pageLength = 25, dom = "ftp", scrollX = TRUE))
  }, server = FALSE)
  
  # ════════════════════════════════════════════════════════════
  # DOWNLOAD HANDLERS — fixtures, standings, enriched, predictions
  # ════════════════════════════════════════════════════════════
  
  write_xlsx_simple <- function(file, df, sheet = "data") {
    wb <- createWorkbook()
    addWorksheet(wb, sheet, gridLines = FALSE)
    hs <- createStyle(fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
                      fgFill = "#1E3A8A", halign = "CENTER", textDecoration = "BOLD")
    writeData(wb, sheet, df, startRow = 1, startCol = 1)
    addStyle(wb, sheet, hs, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
    setColWidths(wb, sheet, cols = seq_len(ncol(df)), widths = "auto")
    freezePane(wb, sheet, firstRow = TRUE)
    saveWorkbook(wb, file, overwrite = TRUE)
  }
  
  output$dl_fixtures_xlsx <- downloadHandler(
    filename = function() paste0("moab_fixtures_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      df <- rv$fixtures
      if (is.null(df) || nrow(df) == 0) {
        write_xlsx_simple(file, data.frame(Message = "No fixtures loaded"))
      } else {
        write_xlsx_simple(file, df, "fixtures")
      }
    })
  
  output$dl_fixtures_rds <- downloadHandler(
    filename = function() paste0("moab_fixtures_", format(Sys.Date(), "%Y%m%d"), ".rds"),
    content = function(file) saveRDS(rv$fixtures, file))
  
  output$dl_standings_xlsx <- downloadHandler(
    filename = function() paste0("moab_standings_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      df <- rv$standings
      if (is.null(df) || nrow(df) == 0) {
        write_xlsx_simple(file, data.frame(Message = "No standings loaded"))
      } else {
        write_xlsx_simple(file, df, "standings")
      }
    })
  
  output$dl_standings_rds <- downloadHandler(
    filename = function() paste0("moab_standings_", format(Sys.Date(), "%Y%m%d"), ".rds"),
    content = function(file) saveRDS(rv$standings, file))
  
  output$dl_enriched_rds <- downloadHandler(
    filename = function() paste0("moab_enriched_", format(Sys.Date(), "%Y%m%d"), ".rds"),
    content = function(file) saveRDS(rv$enriched, file))
  
  # ════════════════════════════════════════════════════════════
  # PREDICTIONS — run analysis on enriched fixtures
  # ════════════════════════════════════════════════════════════
  
  # Helper: which enriched dataset will the next run use?
  active_enriched <- reactive({
    if (!is.null(rv$enriched_override)) rv$enriched_override else rv$enriched
  })
  
  output$enriched_source_status <- renderText({
    if (!is.null(rv$enriched_override)) {
      paste0("Using uploaded file: ", rv$enriched_override_name %||% "(file)",
             " \u00b7 ", length(rv$enriched_override), " fixtures")
    } else {
      n <- if (is.null(rv$enriched)) 0 else length(rv$enriched)
      paste0("Using live cache: ", n, " fixtures")
    }
  })
  
  observeEvent(input$upload_enriched_pred, {
    req(input$upload_enriched_pred)
    obj <- tryCatch(readRDS(input$upload_enriched_pred$datapath), error = function(e) NULL)
    if (is.null(obj) || !is.list(obj) || length(obj) == 0) {
      showNotification("Could not read that .rds (expected an enriched_fixtures list)",
                       type = "error", duration = 5)
      return()
    }
    rv$enriched_override <- obj
    rv$enriched_override_name <- input$upload_enriched_pred$name
    showNotification(paste0("Loaded ", length(obj), " fixtures from ",
                            input$upload_enriched_pred$name,
                            ". Click Run analysis to use it."),
                     type = "message", duration = 5)
  })
  
  observeEvent(input$reset_enriched_pred, {
    rv$enriched_override <- NULL
    rv$enriched_override_name <- NULL
    showNotification("Reset to live cache", type = "message", duration = 3)
  })
  
  output$pred_status <- renderText({
    src <- active_enriched()
    if (is.null(rv$predictions)) {
      if (is.null(src) || length(src) == 0)
        "No enriched data yet. Run the pipeline first or upload a file."
      else paste0(length(src), " enriched fixtures available. Click Run analysis.")
    } else {
      total <- sum(sapply(rv$predictions, function(d) if (is.null(d)) 0 else nrow(d)))
      paste0(total, " total picks across ", length(rv$predictions), " markets. Last run: ",
             format(rv$pred_ran_at, "%d %b %H:%M"))
    }
  })
  
  observeEvent(input$run_pred_btn, {
    src <- active_enriched()
    if (is.null(src) || length(src) == 0) {
      showNotification("No enriched fixtures to analyse", type = "error", duration = 4)
      return()
    }
    if (!exists("run_moab_analysis")) {
      showNotification("Analysis module not loaded \u2014 check moab_analysis.R path",
                       type = "error", duration = 6)
      log_msg("ERROR: run_moab_analysis not found.")
      return()
    }
    src_label <- if (!is.null(rv$enriched_override))
      paste0("uploaded file (", rv$enriched_override_name, ")") else "live cache"
    log_msg(paste0("Running analysis on ", length(src), " fixtures from ", src_label, "..."))
    withProgress(message = "Analysing fixtures", value = 0.3, {
      tryCatch({
        preds <- run_moab_analysis(src, rv$thresholds %||% DEFAULT_MOAB_THRESHOLDS,
                                   standings = rv$standings)
        rv$predictions <- preds
        rv$pred_ran_at <- Sys.time()
        total <- sum(sapply(preds, function(d) if (is.null(d)) 0 else nrow(d)))
        log_msg(paste0("Analysis complete: ", total, " picks across ", length(preds), " markets."))
        showNotification(paste0("Analysis complete: ", total, " picks"),
                         type = "message", duration = 4)
      }, error = function(e) {
        log_msg(paste0("Analysis error: ", e$message))
        showNotification(paste0("Error: ", e$message), type = "error", duration = 6)
      })
    })
  })
  
  output$predictions_summary <- renderUI({
    req(rv$predictions)
    p <- rv$predictions
    counts <- sapply(p, function(d) if (is.null(d)) 0 else nrow(d))
    div(class = "kpi-grid",
        div(class = "kpi-card", div(class = "kpi-num", sum(counts)),
            div(class = "kpi-lab", "Total picks")),
        div(class = "kpi-card",
            div(class = "kpi-num", counts[["ht_draw"]] %||% 0),
            div(class = "kpi-lab", "HT Draw")),
        div(class = "kpi-card",
            div(class = "kpi-num", counts[["btts"]] %||% 0),
            div(class = "kpi-lab", "BTTS")),
        div(class = "kpi-card",
            div(class = "kpi-num", counts[["over_15_ft"]] %||% 0),
            div(class = "kpi-lab", "O1.5 FT"))
    )
  })
  
  output$predictions_tables <- renderUI({
    req(rv$predictions)
    p <- rv$predictions
    nonzero <- names(p)[sapply(p, function(d) !is.null(d) && nrow(d) > 0)]
    if (length(nonzero) == 0) {
      return(div(class = "card-elite", style = "text-align:center; padding:40px;",
                 div(style = "font-family:var(--fh); font-size:18px; color:var(--text-3);",
                     "No picks generated. Try adjusting thresholds in Settings.")))
    }
    blocks <- lapply(nonzero, function(mkt) {
      df <- p[[mkt]]
      # Shiny IDs cannot contain spaces, dots, or other special chars
      safe_id <- gsub("[^A-Za-z0-9]", "_", as.character(mkt))
      tbl_id <- paste0("pred_table_", safe_id)
      label <- tryCatch({
        v <- df$market
        if (is.null(v) || length(v) == 0) as.character(mkt) else as.character(v[1])
      }, error = function(e) as.character(mkt))
      n_picks <- tryCatch(as.character(nrow(df)), error = function(e) "?")
      div(class = "card-elite",
          div(style = "display:flex; justify-content:space-between; align-items:baseline; margin-bottom:10px;",
              div(class = "card-eyebrow", paste0(label, " \u2014 ", n_picks, " picks")),
              div()),
          div(style = "margin-top:10px;", DTOutput(tbl_id)))
    })
    # Register per-table outputs lazily
    for (mkt in nonzero) {
      local({
        m <- as.character(mkt)
        safe_id <- gsub("[^A-Za-z0-9]", "_", m)
        df <- p[[m]]
        output[[paste0("pred_table_", safe_id)]] <- renderDT({
          tryCatch({
            cols <- intersect(c("country","league","fixture_date","match_time",
                                "home","away","pick","confidence","expected"),
                              names(df))
            sub <- df[, cols, drop = FALSE]
            datatable(sub, rownames = FALSE, selection = "none",
                      options = list(pageLength = 20, dom = "ftp", scrollX = TRUE))
          }, error = function(e) {
            datatable(data.frame(Error = as.character(e$message)),
                      rownames = FALSE, options = list(dom = "t"))
          })
        }, server = FALSE)
      })
    }
    do.call(tagList, blocks)
  })
  
  output$dl_predictions_xlsx <- downloadHandler(
    filename = function() paste0("moab_predictions_", format(Sys.Date(), "%Y%m%d"), ".xlsx"),
    content = function(file) {
      p <- rv$predictions
      if (is.null(p)) {
        write_xlsx_simple(file, data.frame(Message = "No predictions yet"))
        return()
      }
      wb <- createWorkbook()
      hs <- createStyle(fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
                        fgFill = "#1E3A8A", halign = "CENTER", textDecoration = "BOLD")
      # Combined sheet
      all_rows <- list()
      for (mkt in names(p)) {
        df <- p[[mkt]]
        if (!is.null(df) && nrow(df) > 0) all_rows[[mkt]] <- df
      }
      if (length(all_rows) > 0) {
        combined <- bind_rows(all_rows)
        addWorksheet(wb, "All Picks", gridLines = FALSE)
        writeData(wb, "All Picks", combined, startRow = 1, startCol = 1)
        addStyle(wb, "All Picks", hs, rows = 1, cols = seq_len(ncol(combined)), gridExpand = TRUE)
        setColWidths(wb, "All Picks", cols = seq_len(ncol(combined)), widths = "auto")
        freezePane(wb, "All Picks", firstRow = TRUE)
      }
      # Per-market sheets
      for (mkt in names(p)) {
        df <- p[[mkt]]
        if (is.null(df) || nrow(df) == 0) next
        sheet <- substr(gsub("[^A-Za-z0-9]", "_", mkt), 1, 31)
        addWorksheet(wb, sheet, gridLines = FALSE)
        writeData(wb, sheet, df, startRow = 1, startCol = 1)
        addStyle(wb, sheet, hs, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
        setColWidths(wb, sheet, cols = seq_len(ncol(df)), widths = "auto")
        freezePane(wb, sheet, firstRow = TRUE)
      }
      saveWorkbook(wb, file, overwrite = TRUE)
    })
  
  output$dl_predictions_rds <- downloadHandler(
    filename = function() paste0("moab_predictions_", format(Sys.Date(), "%Y%m%d"), ".rds"),
    content = function(file) saveRDS(rv$predictions, file))
  
  # ════════════════════════════════════════════════════════════
  # THRESHOLDS UI + save/reset
  # ════════════════════════════════════════════════════════════
  
  output$threshold_inputs <- renderUI({
    th <- rv$thresholds %||% DEFAULT_MOAB_THRESHOLDS
    keys <- names(th)
    rows <- lapply(keys, function(k) {
      val <- th[[k]]
      column(4,
             numericInput(paste0("th_", k), label = k, value = val, step = 0.01))
    })
    fluidRow(rows)
  })
  
  observeEvent(input$save_thresh_btn, {
    th <- rv$thresholds %||% DEFAULT_MOAB_THRESHOLDS
    for (k in names(th)) {
      iv <- input[[paste0("th_", k)]]
      if (!is.null(iv) && !is.na(iv)) th[[k]] <- iv
    }
    rv$thresholds <- th
    tryCatch(write(toJSON(th, auto_unbox = TRUE, pretty = TRUE), SETTINGS_PATH),
             error = function(e) NULL)
    log_msg("Thresholds saved.")
    showNotification("Thresholds saved", type = "message", duration = 3)
  })
  
  observeEvent(input$reset_thresh_btn, {
    rv$thresholds <- DEFAULT_MOAB_THRESHOLDS
    if (file.exists(SETTINGS_PATH)) file.remove(SETTINGS_PATH)
    log_msg("Thresholds reset to defaults.")
    showNotification("Reset to defaults", type = "message", duration = 3)
  })
}

shinyApp(ui = ui, server = server)
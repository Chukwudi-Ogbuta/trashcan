# ============================================================
# moab_result_scraper.R — Shared parallel result scraping module
# Used by moab_live_checker.R and moab_simulator.R.
# Sourced by both apps to avoid code duplication.
#
# PARITY UPDATE: scrape_result now mirrors main pipeline's scrape_match_stats:
#   - Overall + 1H + 2H stats tabs (full 18-stat WANTED_STATS list)
#   - data-testid selectors with hashed-class fallbacks (resilient to DOM churn)
#   - Goal times + HT from single summary scrape (one page load, not two)
#   - Derived aggregates (total_corners, total_cards, etc.) preserved for
#     backward compatibility with grade_pick() and downstream consumers
# ============================================================

library(future)
library(parallelly)
library(rvest)
library(httr)
library(jsonlite)

CHROME_PATH       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
BASE_URL          <- "https://www.flashscore.com"
N_PARALLEL_WORKERS <- 6

# Full stat list — matches main pipeline exactly. Aggregates for grading
# (corners, cards, shots, SOT) are computed downstream from these.
WANTED_STATS <- c("Expected goals (xG)", "Ball possession", "Total shots",
                  "Shots on target", "Shots off target", "Blocked shots",
                  "Shots inside the box", "Shots outside the box",
                  "Big chances", "Corner kicks", "Touches in opposition box",
                  "Fouls", "Offsides", "Free kicks", "Throw ins",
                  "Yellow cards", "Red cards", "Goalkeeper saves")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ─── Worker function (runs in isolated R process) ───
run_result_worker <- function(worker_id, work_chunk, chrome_path, chromedriver_path,
                              base_url, progress_dir, wanted_stats) {
  library(rvest); library(httr); library(jsonlite); library(dplyr)
  
  port <- 50000 + worker_id * 211 + sample(1:999, 1)
  session_id <- NULL
  
  `%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b
  slug <- function(x) { x <- tolower(x); x <- gsub("[^a-z0-9]+", "_", x); gsub("^_|_$", "", x) }
  
  parse_min <- function(t) {
    if (is.null(t) || is.na(t)) return(NA_integer_)
    t <- gsub("[^0-9+]", "", t)
    # 45+X -> 45, 90+X -> 90 (so the goal stays in the correct half).
    if (grepl("\\+", t)) {
      p <- suppressWarnings(as.integer(strsplit(t, "\\+")[[1]]))
      if (length(p) >= 1 && !is.na(p[1])) return(p[1])
    }
    suppressWarnings(as.integer(t))
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
    raw <- tryCatch(fromJSON(content(res, as = "text"))$value, error = function(e) NULL)
    if (is.null(raw)) return(NULL)
    tryCatch(read_html(raw), error = function(e) NULL)
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
  
  # Build the stats URL for a given tab (overall / 1st-half / 2nd-half).
  # Matches main pipeline's build_stats_url exactly.
  build_stats_url <- function(match_url, half = "overall") {
    seg <- paste0("summary/stats/", half, "/")
    if (grepl("summary/stats/", match_url))
      return(sub("summary/stats/[^/]+/", seg, match_url))
    sub("(\\?mid=)", paste0(seg, "\\1"), match_url)
  }
  
  # Parse one stats-tab page into home/away named lists.
  # Prefers stable data-testid selectors, falls back to hashed classes.
  # Mirrors parse_stats_page from main pipeline.
  parse_stats_page <- function(page) {
    home <- list(); away <- list()
    if (is.null(page)) return(list(home = home, away = away))
    rows <- page %>% html_nodes("div[data-testid='wcl-statistics']")
    if (length(rows) == 0) rows <- page %>% html_nodes("div.wcl-row_2oCpS")
    for (r in rows) {
      cat_node <- r %>% html_node("div[data-testid='wcl-statistics-category'] span")
      if (is.null(cat_node)) cat_node <- r %>% html_node("div.wcl-category_6sT1J span")
      if (is.null(cat_node)) next
      cat_name <- html_text(cat_node, trim = TRUE)
      if (!(cat_name %in% wanted_stats)) next
      val_divs <- r %>% html_nodes("div[data-testid='wcl-statistics-value']")
      if (length(val_divs) < 2) val_divs <- r %>% html_nodes("div.wcl-value_XJG99")
      if (length(val_divs) < 2) next
      home_val <- val_divs[[1]] %>% html_node("span")
      away_val <- val_divs[[2]] %>% html_node("span")
      if (is.null(home_val) || is.null(away_val)) next
      key <- slug(cat_name)
      home[[key]] <- html_text(home_val, trim = TRUE)
      away[[key]] <- html_text(away_val, trim = TRUE)
    }
    list(home = home, away = away)
  }
  
  # Scrape summary page ONCE for goal times + HT score.
  # Mirrors main pipeline's scrape_match_summary. Consolidates what were
  # two separate page loads in the old scrape_result into one.
  scrape_summary <- function(match_url) {
    fail <- list(goal_times_home = list(), goal_times_away = list(),
                 ht_home = NA_integer_, ht_away = NA_integer_)
    page <- tryCatch(get_html(match_url), error = function(e) NULL)
    if (is.null(page)) return(fail)
    
    # Goal times: only smv incident rows tagged with the goal-soccer icon.
    home_rows <- page %>% html_nodes("div.smv__participantRow.smv__homeParticipant")
    away_rows <- page %>% html_nodes("div.smv__participantRow.smv__awayParticipant")
    extract <- function(rows) {
      out <- character()
      for (n in rows) {
        gid <- n %>% html_node("div.smv__incidentIcon")
        if (is.null(gid) || length(gid) == 0) next
        gsvg <- gid %>% html_node("svg[data-testid='wcl-icon-incidents-goal-soccer']")
        if (is.null(gsvg) || length(gsvg) == 0) next
        tm <- n %>% html_node("div.smv__timeBox") %>% html_text(trim = TRUE)
        if (!is.na(tm) && nchar(tm) > 0) out <- c(out, tm)
      }
      out
    }
    goal_times_home <- as.list(extract(home_rows))
    goal_times_away <- as.list(extract(away_rows))
    
    # HT from the "1st Half" partial-score block. Single source of truth.
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
    list(goal_times_home = goal_times_home,
         goal_times_away = goal_times_away,
         ht_home = ht_home, ht_away = ht_away)
  }
  
  # Full-parity result scrape. Returns everything the main pipeline stores
  # per match, PLUS derived aggregates for backward compatibility.
  scrape_result <- function(match_url) {
    if (is.null(match_url) || is.na(match_url) || nchar(match_url) == 0)
      return(list(status = "ERROR"))
    
    # Result skeleton — mirrors main pipeline structure, plus derived fields.
    out <- list(
      status = "PENDING",
      match_url = match_url,
      ft_home = NA, ft_away = NA, ht_home = NA, ht_away = NA,
      stats_home = list(), stats_away = list(),
      stats_home_1h = list(), stats_away_1h = list(),
      stats_home_2h = list(), stats_away_2h = list(),
      goal_times_home = list(), goal_times_away = list(),
      # Derived aggregates (kept for grade_pick backward compatibility)
      total_corners = NA, total_cards = NA, total_shots = NA, total_sot = NA,
      home_corners = NA, away_corners = NA, home_cards = NA, away_cards = NA
    )
    
    # Overall stats tab — also carries the FT header
    page_ov <- tryCatch(get_html(build_stats_url(match_url, "overall")),
                       error = function(e) NULL)
    if (is.null(page_ov)) return(out)  # keep status = PENDING
    ov <- parse_stats_page(page_ov)
    out$stats_home <- ov$home; out$stats_away <- ov$away
    
    # FT from the detailScore header on the overall-stats page
    hdr <- page_ov %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
    if (length(hdr) > 0) {
      txt <- html_text(hdr[1], trim = TRUE)
      nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
      if (length(nums) >= 2) {
        out$ft_home <- as.integer(nums[1])
        out$ft_away <- as.integer(nums[2])
        out$status <- "FINAL"
      }
    }
    
    # First-half stats tab
    page_1h <- tryCatch(get_html(build_stats_url(match_url, "1st-half")),
                       error = function(e) NULL)
    h1 <- parse_stats_page(page_1h)
    out$stats_home_1h <- h1$home; out$stats_away_1h <- h1$away
    
    # Second-half stats tab
    page_2h <- tryCatch(get_html(build_stats_url(match_url, "2nd-half")),
                       error = function(e) NULL)
    h2 <- parse_stats_page(page_2h)
    out$stats_home_2h <- h2$home; out$stats_away_2h <- h2$away
    
    # Summary page — goal times + HT in a single load
    summary_data <- tryCatch(scrape_summary(match_url),
                             error = function(e) list(
                               goal_times_home = list(), goal_times_away = list(),
                               ht_home = NA_integer_, ht_away = NA_integer_))
    out$goal_times_home <- summary_data$goal_times_home
    out$goal_times_away <- summary_data$goal_times_away
    out$ht_home <- summary_data$ht_home
    out$ht_away <- summary_data$ht_away
    
    # ── Derived aggregates for grade_pick ──
    # These read from the overall stats block. If a league has no stats
    # (main pipeline flagged it), stats_home/stats_away will be empty and
    # these end up NA, which grade_pick handles as PENDING for stats markets.
    num <- function(v) {
      if (is.null(v) || length(v) == 0) return(NA_real_)
      v <- gsub("%", "", as.character(v))
      suppressWarnings(as.numeric(v))
    }
    out$home_corners <- num(out$stats_home$corner_kicks)
    out$away_corners <- num(out$stats_away$corner_kicks)
    out$total_corners <- sum(c(out$home_corners, out$away_corners), na.rm = TRUE)
    # sum(..., na.rm=TRUE) on two NAs returns 0, not NA — restore NA when both
    # sides missing so grading stays PENDING rather than falsely LOSS.
    if (is.na(out$home_corners) && is.na(out$away_corners)) out$total_corners <- NA
    
    out$home_cards <- num(out$stats_home$yellow_cards %||% 0) +
                      num(out$stats_home$red_cards    %||% 0)
    out$away_cards <- num(out$stats_away$yellow_cards %||% 0) +
                      num(out$stats_away$red_cards    %||% 0)
    out$total_cards <- out$home_cards + out$away_cards
    
    out$total_shots <- num(out$stats_home$total_shots %||% 0) +
                       num(out$stats_away$total_shots %||% 0)
    if (is.na(num(out$stats_home$total_shots)) && is.na(num(out$stats_away$total_shots)))
      out$total_shots <- NA
    
    out$total_sot <- num(out$stats_home$shots_on_target %||% 0) +
                     num(out$stats_away$shots_on_target %||% 0)
    if (is.na(num(out$stats_home$shots_on_target)) && is.na(num(out$stats_away$shots_on_target)))
      out$total_sot <- NA
    
    out
  }
  
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
  if (is.null(session_id))
    return(list(worker_id = worker_id, success = FALSE, results = list()))
  
  navigate(paste0(base_url, "/")); Sys.sleep(3)
  wait_for_page(30); dismiss_cookie(); Sys.sleep(2)
  
  progress_path <- file.path(progress_dir, paste0("checker_w_", worker_id, "_progress.txt"))
  results <- list()
  total <- length(work_chunk)
  for (i in seq_along(work_chunk)) {
    w <- work_chunk[[i]]
    res <- tryCatch(scrape_result(w$url), error = function(e) NULL)
    if (!is.null(res)) results[[as.character(w$match_id)]] <- res
    writeLines(paste0(i, "/", total), progress_path)
  }
  writeLines(paste0(total, "/", total, " DONE"), progress_path)
  
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                  timeout(10)), error = function(e) invisible())
  
  list(worker_id = worker_id, success = TRUE, results = results)
}

# ─── Public API ───
# work_queue: list of list(match_id, url) — one entry per UNIQUE fixture to scrape
# Returns: named list keyed by match_id -> scrape_result
parallel_scrape_results <- function(work_queue, progress_dir,
                                    n_workers = N_PARALLEL_WORKERS, log_fn = NULL) {
  if (length(work_queue) == 0) return(list())
  n_workers <- min(n_workers, length(work_queue))
  # cut() requires at least 2 intervals AND length >= 2; for tiny queues just
  # assign everything to one chunk (single-worker execution).
  if (n_workers <= 1 || length(work_queue) <= 1) {
    chunks <- list(work_queue)
    n_workers <- 1
  } else {
    chunks <- split(work_queue, cut(seq_along(work_queue), n_workers, labels = FALSE))
  }
  
  for (i in seq_len(n_workers)) {
    pf <- file.path(progress_dir, paste0("checker_w_", i, "_progress.txt"))
    if (file.exists(pf)) file.remove(pf)
  }
  
  plan(multisession, workers = n_workers)
  cp <- CHROME_PATH; cdp <- CHROMEDRIVER_PATH; bu <- BASE_URL
  pdir <- progress_dir; ws <- WANTED_STATS
  
  futures <- list()
  for (i in seq_len(n_workers)) {
    local_chunk <- chunks[[i]]
    startup_delay <- (i - 1) * runif(1, 4, 8)
    futures[[i]] <- future({
      Sys.sleep(startup_delay)
      run_result_worker(i, local_chunk, cp, cdp, bu, pdir, ws)
    }, seed = TRUE,
    globals = list(i = i, local_chunk = local_chunk, startup_delay = startup_delay,
                   cp = cp, cdp = cdp, bu = bu, pdir = pdir, ws = ws,
                   run_result_worker = run_result_worker))
  }
  
  withProgress(message = paste0(n_workers, " workers scraping"), value = 0, {
    done <- rep(FALSE, n_workers)
    while (!all(done)) {
      Sys.sleep(5)
      total_prog <- 0
      for (i in seq_len(n_workers)) {
        pf <- file.path(progress_dir, paste0("checker_w_", i, "_progress.txt"))
        if (file.exists(pf)) {
          txt <- tryCatch(readLines(pf, warn = FALSE)[1], error = function(e) "")
          if (grepl("DONE", txt)) done[i] <- TRUE
          m <- regmatches(txt, regexpr("(\\d+)/(\\d+)", txt))
          if (length(m) > 0) {
            parts <- as.integer(strsplit(m, "/")[[1]])
            if (length(parts) == 2 && parts[2] > 0)
              total_prog <- total_prog + (parts[1] / parts[2])
          }
        }
        if (resolved(futures[[i]])) done[i] <- TRUE
      }
      avg <- total_prog / n_workers
      setProgress(value = avg,
                  detail = paste0(round(avg * 100), "% across ", n_workers, " workers"))
      if (all(done)) break
    }
  })
  
  all_results <- list()
  n_success <- 0
  for (i in seq_len(n_workers)) {
    res <- tryCatch(value(futures[[i]]), error = function(e) {
      if (!is.null(log_fn)) log_fn(paste0("Worker ", i, " error: ", e$message))
      list(success = FALSE, results = list())
    })
    if (!is.null(res$success) && res$success) {
      n_success <- n_success + 1
      all_results <- c(all_results, res$results)
    }
  }
  if (n_success == 0 && !is.null(log_fn)) {
    log_fn("ALL WORKERS FAILED — likely chromedriver/Chrome version mismatch. Check versions.")
  }
  plan(sequential)
  
  for (i in seq_len(n_workers)) {
    pf <- file.path(progress_dir, paste0("checker_w_", i, "_progress.txt"))
    if (file.exists(pf)) file.remove(pf)
  }
  
  all_results
}

# ─── Grade a pick using a scraped result ───
# ─── Grade a pick using a scraped result ───
# Markets align with M.O.A.B's 12 target markets (16 market strings).
grade_pick <- function(market, result) {
  if (is.null(result)) return("PENDING")
  st <- result$status
  if (is.null(st) || length(st) == 0 || is.na(st) || st != "FINAL") return("PENDING")
  
  ft_h <- result$ft_home; ft_a <- result$ft_away
  ht_h <- result$ht_home; ht_a <- result$ht_away
  if (is.null(ft_h) || length(ft_h) == 0) ft_h <- NA
  if (is.null(ft_a) || length(ft_a) == 0) ft_a <- NA
  if (is.null(ht_h) || length(ht_h) == 0) ht_h <- NA
  if (is.null(ht_a) || length(ht_a) == 0) ht_a <- NA
  ft <- if (!is.na(ft_h) && !is.na(ft_a)) ft_h + ft_a else NA
  ht <- if (!is.na(ht_h) && !is.na(ht_a)) ht_h + ht_a else NA
  two_h <- if (!is.na(ft) && !is.na(ht)) ft - ht else NA
  
  # Parse goal times (used for 1UP + timed markets).
  # Result stores goal_times_home / goal_times_away as lists of strings ("45+2", "12", etc).
  parse_min <- function(v) {
    if (is.null(v) || length(v) == 0) return(integer(0))
    out <- integer(0)
    for (t in v) {
      if (is.null(t) || is.na(t)) next
      s <- gsub("[^0-9+]", "", as.character(t))
      if (grepl("\\+", s)) {
        p <- suppressWarnings(as.integer(strsplit(s, "\\+")[[1]]))
        if (length(p) >= 1 && !is.na(p[1])) out <- c(out, p[1])
      } else {
        n <- suppressWarnings(as.integer(s))
        if (!is.na(n)) out <- c(out, n)
      }
    }
    out
  }
  home_mins <- parse_min(result$goal_times_home)
  away_mins <- parse_min(result$goal_times_away)
  has_goal_times <- length(home_mins) + length(away_mins) > 0 ||
    (!is.na(ft) && ft == 0)  # 0-0 counts as valid absence of goals
  
  # Helper: was team X ever ahead by 1 at any point (used for 1UP)
  ever_ahead <- function(my_mins, opp_mins) {
    if (length(my_mins) == 0) return(FALSE)
    events <- sort(c(my_mins, opp_mins))
    if (length(events) == 0) return(FALSE)
    my_score <- 0; opp_score <- 0
    all_mins <- sort(unique(c(my_mins, opp_mins)))
    for (m in all_mins) {
      my_score  <- my_score  + sum(my_mins  == m)
      opp_score <- opp_score + sum(opp_mins == m)
      if (my_score - opp_score >= 1) return(TRUE)
    }
    FALSE
  }
  
  m <- market
  
  # 1X2 (Home / Away — Draw handled by FT Draw)
  if (m == "1X2 Home")             return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_h > ft_a) "WIN" else "LOSS")
  if (m == "1X2 Away")             return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_a > ft_h) "WIN" else "LOSS")
  
  # 1UP — needs goal times
  if (m == "1UP Home") {
    if (!has_goal_times) return("PENDING")
    return(if (ever_ahead(home_mins, away_mins)) "WIN" else "LOSS")
  }
  if (m == "1UP Away") {
    if (!has_goal_times) return("PENDING")
    return(if (ever_ahead(away_mins, home_mins)) "WIN" else "LOSS")
  }
  
  # Double Chance
  if (m == "Double Chance 1X")     return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_h >= ft_a) "WIN" else "LOSS")
  if (m == "Double Chance 12")     return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_h != ft_a) "WIN" else "LOSS")
  if (m == "Double Chance X2")     return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_a >= ft_h) "WIN" else "LOSS")
  
  # Over/Under totals
  if (m == "Over 1.5")             return(if (is.na(ft)) "PENDING" else if (ft >= 2) "WIN" else "LOSS")
  if (m == "Over 2.5")             return(if (is.na(ft)) "PENDING" else if (ft >= 3) "WIN" else "LOSS")
  
  # BTTS
  if (m == "BTTS")                 return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_h >= 1 && ft_a >= 1) "WIN" else "LOSS")
  
  # Halves
  if (m == "1H Over 0.5")          return(if (is.na(ht)) "PENDING" else if (ht >= 1) "WIN" else "LOSS")
  if (m == "2H Over 0.5")          return(if (is.na(two_h)) "PENDING" else if (two_h >= 1) "WIN" else "LOSS")
  
  # Timed markets — need goal times
  if (m == "Draw up to min 10") {
    if (!has_goal_times) return("PENDING")
    goals_in_window <- sum(home_mins <= 10) + sum(away_mins <= 10)
    return(if (goals_in_window == 0) "WIN" else "LOSS")
  }
  if (m == "Draw up to min 15") {
    if (!has_goal_times) return("PENDING")
    goals_in_window <- sum(home_mins <= 15) + sum(away_mins <= 15)
    return(if (goals_in_window == 0) "WIN" else "LOSS")
  }
  if (m == "Under 0.5 up to min 15") {
    if (!has_goal_times) return("PENDING")
    goals_in_window <- sum(home_mins <= 15) + sum(away_mins <= 15)
    return(if (goals_in_window == 0) "WIN" else "LOSS")
  }
  
  # FT Draw
  if (m == "FT Draw")              return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_h == ft_a) "WIN" else "LOSS")
  
  "PENDING"
}
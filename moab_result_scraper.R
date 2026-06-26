# ============================================================
# moab_result_scraper.R — Shared parallel result scraping module
# Used by moab_live_checker.R and moab_simulator.R.
# Sourced by both apps to avoid code duplication.
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

WANTED_STATS <- c("Corner kicks", "Yellow cards", "Red cards", "Total shots",
                  "Shots on target")

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
  
  # Scrape HT/2H partial scores from the FlashScore match summary page.
  # Real HTML (from the live page):
  #   <div data-testid="wcl-headerSection-text">
  #     <span ...>1st Half</span>
  #     <span ...><div>2 - 0</div></span>
  #   </div>
  # Strategy: find every wcl-headerSection-text block, read the two spans,
  # if span1 == "1st Half" take span2's number-number text.
  scrape_partial_scores <- function(match_url) {
    fail <- list(ht_home = NA_integer_, ht_away = NA_integer_)
    if (is.null(match_url) || is.na(match_url) || nchar(match_url) == 0) return(fail)
    # The bare match URL already loads the summary page which contains the
    # 1st Half partial-score block. No URL transformation needed.
    page <- tryCatch(get_html(match_url), error = function(e) NULL)
    if (is.null(page)) return(fail)
    blocks <- page %>% html_nodes("[data-testid='wcl-headerSection-text']")
    for (b in blocks) {
      spans <- b %>% html_nodes("span")
      if (length(spans) < 2) next
      label <- html_text(spans[[1]], trim = TRUE)
      score_txt <- html_text(spans[[2]], trim = TRUE)
      if (identical(label, "1st Half")) {
        m <- regmatches(score_txt, regexec("([0-9]+)\\s*-\\s*([0-9]+)", score_txt))[[1]]
        if (length(m) >= 3) {
          return(list(ht_home = as.integer(m[2]), ht_away = as.integer(m[3])))
        }
      }
    }
    fail
  }
  
  scrape_summary_goals <- function(match_url) {
    page <- tryCatch(get_html(match_url), error = function(e) NULL)
    if (is.null(page)) return(list(home = list(), away = list()))
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
    list(home = as.list(extract(home_rows)), away = as.list(extract(away_rows)))
  }
  
  scrape_result <- function(match_url) {
    if (is.null(match_url) || is.na(match_url) || nchar(match_url) == 0)
      return(list(status = "ERROR"))
    stats_url <- if (grepl("summary/stats/overall", match_url)) match_url
    else sub("(\\?mid=)", "summary/stats/overall/\\1", match_url)
    page <- tryCatch(get_html(stats_url), error = function(e) NULL)
    if (is.null(page)) return(list(status = "PENDING"))
    out <- list(status = "PENDING",
                ft_home = NA, ft_away = NA, ht_home = NA, ht_away = NA,
                stats_home = list(), stats_away = list(),
                goal_times_home = list(), goal_times_away = list(),
                total_corners = NA, total_cards = NA, total_shots = NA, total_sot = NA,
                home_corners = NA, away_corners = NA, home_cards = NA, away_cards = NA)
    rows <- page %>% html_nodes("div.wcl-row_2oCpS")
    for (r in rows) {
      cn <- r %>% html_node("div.wcl-category_6sT1J span")
      if (is.null(cn)) next
      name <- html_text(cn, trim = TRUE)
      if (!(name %in% wanted_stats)) next
      vals <- r %>% html_nodes("div.wcl-value_XJG99 span.wcl-bold_NZXv6")
      if (length(vals) < 2) next
      key <- slug(name)
      out$stats_home[[key]] <- html_text(vals[1], trim = TRUE)
      out$stats_away[[key]] <- html_text(vals[2], trim = TRUE)
    }
    hdr <- page %>% html_nodes("div.detailScore__wrapper, span.detailScore__matchResult")
    if (length(hdr) > 0) {
      txt <- html_text(hdr[1], trim = TRUE)
      nums <- regmatches(txt, gregexpr("\\d+", txt))[[1]]
      if (length(nums) >= 2) {
        out$ft_home <- as.integer(nums[1]); out$ft_away <- as.integer(nums[2])
        out$status <- "FINAL"
      }
    }
    # HT score: read DIRECTLY from FlashScore's "1st Half" partial-score block.
    # This is the single source of truth. No URL transform — match_url already
    # loads the summary page containing this block. No fallback to goal-times.
    page_main <- tryCatch(get_html(match_url), error = function(e) NULL)
    out$ht_home <- NA_integer_; out$ht_away <- NA_integer_
    if (!is.null(page_main)) {
      blocks <- page_main %>% html_nodes("[data-testid='wcl-headerSection-text']")
      for (b in blocks) {
        spans <- b %>% html_nodes("span")
        if (length(spans) < 2) next
        label <- html_text(spans[[1]], trim = TRUE)
        if (identical(label, "1st Half")) {
          score_txt <- html_text(spans[[2]], trim = TRUE)
          m <- regmatches(score_txt, regexec("([0-9]+)\\s*-\\s*([0-9]+)", score_txt))[[1]]
          if (length(m) >= 3) {
            out$ht_home <- as.integer(m[2])
            out$ht_away <- as.integer(m[3])
          }
          break
        }
      }
    }
    num <- function(v) {
      if (is.null(v) || length(v) == 0) return(NA_real_)
      v <- gsub("%", "", as.character(v))
      suppressWarnings(as.numeric(v))
    }
    out$home_corners <- num(out$stats_home$corner_kicks)
    out$away_corners <- num(out$stats_away$corner_kicks)
    out$total_corners <- sum(c(out$home_corners, out$away_corners), na.rm = TRUE)
    out$home_cards <- num(out$stats_home$yellow_cards %||% 0) + num(out$stats_home$red_cards %||% 0)
    out$away_cards <- num(out$stats_away$yellow_cards %||% 0) + num(out$stats_away$red_cards %||% 0)
    out$total_cards <- out$home_cards + out$away_cards
    out$total_shots <- num(out$stats_home$total_shots %||% 0) + num(out$stats_away$total_shots %||% 0)
    out$total_sot   <- num(out$stats_home$shots_on_target %||% 0) +
      num(out$stats_away$shots_on_target %||% 0)
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
  for (i in seq_len(n_workers)) {
    res <- tryCatch(value(futures[[i]]), error = function(e) {
      if (!is.null(log_fn)) log_fn(paste0("Worker ", i, " error: ", e$message))
      list(success = FALSE, results = list())
    })
    if (!is.null(res$success) && res$success) all_results <- c(all_results, res$results)
  }
  plan(sequential)
  
  for (i in seq_len(n_workers)) {
    pf <- file.path(progress_dir, paste0("checker_w_", i, "_progress.txt"))
    if (file.exists(pf)) file.remove(pf)
  }
  
  all_results
}

# ─── Grade a pick using a scraped result ───
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
  m <- market
  
  if (m == "HT Draw")              return(if (is.na(ht_h) || is.na(ht_a)) "PENDING" else if (ht_h == ht_a) "WIN" else "LOSS")
  if (m == "FH Over 0.5")          return(if (is.na(ht)) "PENDING" else if (ht >= 1) "WIN" else "LOSS")
  if (m == "Over 1.5 FT")          return(if (is.na(ft)) "PENDING" else if (ft >= 2) "WIN" else "LOSS")
  if (m == "BTTS")                 return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_h >= 1 && ft_a >= 1) "WIN" else "LOSS")
  if (m == "Under 4.5 FT")         return(if (is.na(ft)) "PENDING" else if (ft <= 4) "WIN" else "LOSS")
  if (m == "Corners Over 9.5")     return(if (is.null(result$total_corners) || is.na(result$total_corners)) "PENDING" else if (result$total_corners > 9.5) "WIN" else "LOSS")
  if (m == "Home Corners Over 4.5")return(if (is.null(result$home_corners)  || is.na(result$home_corners))  "PENDING" else if (result$home_corners > 4.5) "WIN" else "LOSS")
  if (m == "Away Corners Over 3.5")return(if (is.null(result$away_corners)  || is.na(result$away_corners))  "PENDING" else if (result$away_corners > 3.5) "WIN" else "LOSS")
  if (m == "Match Cards Over 3.5") return(if (is.null(result$total_cards)   || is.na(result$total_cards))   "PENDING" else if (result$total_cards > 3.5) "WIN" else "LOSS")
  if (m == "Home Bookings Over 1.5") return(if (is.null(result$home_cards)  || is.na(result$home_cards))    "PENDING" else if (result$home_cards > 1.5) "WIN" else "LOSS")
  if (m == "Away Bookings Over 1.5") return(if (is.null(result$away_cards)  || is.na(result$away_cards))    "PENDING" else if (result$away_cards > 1.5) "WIN" else "LOSS")
  if (m == "Total Shots Over 21.5")return(if (is.null(result$total_shots)   || is.na(result$total_shots))   "PENDING" else if (result$total_shots > 21.5) "WIN" else "LOSS")
  if (m == "SOT Over 7.5")         return(if (is.null(result$total_sot)     || is.na(result$total_sot))     "PENDING" else if (result$total_sot > 7.5) "WIN" else "LOSS")
  if (m == "2H Over 1.5")          return(if (is.na(two_h)) "PENDING" else if (two_h >= 2) "WIN" else "LOSS")
  if (m == "2H BTTS") {
    if (is.na(ht_h) || is.na(ht_a) || is.na(ft_h) || is.na(ft_a)) return("PENDING")
    return(if ((ft_h - ht_h) >= 1 && (ft_a - ht_a) >= 1) "WIN" else "LOSS")
  }
  if (m == "Home Clean Sheet")     return(if (is.na(ft_a)) "PENDING" else if (ft_a == 0) "WIN" else "LOSS")
  if (m == "Away Clean Sheet")     return(if (is.na(ft_h)) "PENDING" else if (ft_h == 0) "WIN" else "LOSS")
  if (m == "Home Win to Nil")      return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_h > ft_a && ft_a == 0) "WIN" else "LOSS")
  if (m == "Away Win to Nil")      return(if (is.na(ft_h) || is.na(ft_a)) "PENDING" else if (ft_a > ft_h && ft_h == 0) "WIN" else "LOSS")
  if (m == "Both Halves Under 1.5") {
    if (is.na(ht) || is.na(two_h)) return("PENDING")
    return(if (ht <= 1 && two_h <= 1) "WIN" else "LOSS")
  }
  "PENDING"
}
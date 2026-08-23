# ============================================================
# FLASHSCORE LEAGUE VERIFICATION — PARALLEL (6 WORKERS)
# Re-verifies only leagues currently marked FALSE or NA in
# league_directory.rds (skips already-verified TRUE entries).
# ============================================================

library(rvest)
library(dplyr)
library(httr)
library(jsonlite)
library(parallel)

BASE_PATH      <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
DIRECTORY_PATH <- file.path(BASE_PATH, "league_directory.rds")
BASE_URL       <- "https://www.flashscore.com"
N_WORKERS      <- 6
CHROMEDRIVER   <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"
CHROME_PATH    <- "C:/Program Files/Google/Chrome/Application/chrome.exe"

# Where each worker writes results as it goes
WORKER_DIR <- file.path(BASE_PATH, "verify_workers")
if (!dir.exists(WORKER_DIR)) dir.create(WORKER_DIR, recursive = TRUE)

`%||%` <- function(a, b) {
  if (is.null(a)) return(b)
  if (length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ════════════════════════════════════════════════════════════
# WORKER FUNCTION — runs in a child R process
# Each worker gets its own port, chromedriver instance, and
# a slice of URLs to verify.
# ════════════════════════════════════════════════════════════
worker_verify <- function(worker_id, task_slice, chromedriver, chrome_path,
                           base_url, output_dir) {
  library(rvest)
  library(httr)
  library(jsonlite)

  `%||%` <- function(a, b) {
    if (is.null(a)) return(b)
    if (length(a) == 0) return(b)
    if (length(a) == 1 && is.na(a)) return(b)
    a
  }

  # Pick a port unique to this worker
  port <- 8880 + worker_id * 3
  session_id <- NULL

  log_path <- file.path(output_dir, paste0("worker_", worker_id, "_log.txt"))
  writeLines(paste0("[W", worker_id, "] starting on port ", port,
                     " with ", nrow(task_slice), " tasks"),
             log_path)

  # Start chromedriver on our port
  system2(chromedriver, args = paste0("--port=", port), wait = FALSE)
  Sys.sleep(3 + worker_id * 0.5)  # stagger startups

  # Open a Selenium session
  response <- tryCatch(POST(
    paste0("http://localhost:", port, "/session"),
    body = list(capabilities = list(alwaysMatch = list(
      browserName = "chrome",
      `goog:chromeOptions` = list(
        binary = chrome_path,
        args = list("--no-sandbox", "--disable-dev-shm-usage",
                    "--disable-blink-features=AutomationControlled",
                    "--disable-extensions", "--headless=new",
                    paste0("--window-size=1200,800")),
        excludeSwitches = list("enable-automation"),
        useAutomationExtension = FALSE)))),
    encode = "json", timeout(30)), error = function(e) NULL)

  if (is.null(response) || status_code(response) != 200) {
    cat(paste0("[W", worker_id, "] FAILED to start Selenium\n"),
        file = log_path, append = TRUE)
    return(NULL)
  }
  sd <- fromJSON(content(response, as = "text"))
  session_id <- sd$sessionId %||% sd$value$sessionId

  # Warm up
  POST(paste0("http://localhost:", port, "/session/", session_id, "/url"),
       body = list(url = paste0(base_url, "/")),
       encode = "json", timeout(30))
  Sys.sleep(5)
  # Dismiss cookies
  tryCatch({
    body_json <- '{"script": "var b=document.querySelectorAll(\\"button,a\\");var k=[\\"Reject\\",\\"Decline\\",\\"Accept\\",\\"Agree\\"];for(var x of b){for(var y of k){if(x.innerText&&x.innerText.trim().toLowerCase().includes(y.toLowerCase())){x.click();return;}}}", "args": []}'
    POST(paste0("http://localhost:", port, "/session/", session_id, "/execute/sync"),
         body = body_json, encode = "raw",
         add_headers(`Content-Type` = "application/json"), timeout(10))
  }, error = function(e) NULL)
  Sys.sleep(2)

  # Verify each URL
  results <- data.frame(
    row_idx = integer(0),
    verified = logical(0),
    stringsAsFactors = FALSE
  )

  for (j in seq_len(nrow(task_slice))) {
    url <- task_slice$league_url[j]
    row_idx <- task_slice$row_idx[j]
    standings_url <- paste0(url, "standings/")

    verified <- tryCatch({
      # Navigate
      POST(paste0("http://localhost:", port, "/session/", session_id, "/url"),
           body = list(url = standings_url),
           encode = "json", timeout(30))
      Sys.sleep(runif(1, 3, 5))

      # Wait for page (up to 15 sec)
      wait_ok <- FALSE
      body_wait <- '{"script": "return document.title;", "args": []}'
      for (w in 1:15) {
        Sys.sleep(1)
        r <- tryCatch(POST(paste0("http://localhost:", port, "/session/",
                                    session_id, "/execute/sync"),
                            body = body_wait, encode = "raw",
                            add_headers(`Content-Type` = "application/json"),
                            timeout(5)), error = function(e) NULL)
        if (!is.null(r)) {
          title <- tryCatch({ v <- fromJSON(content(r, as = "text"))$value
                              if (is.null(v)) "" else as.character(v)[1] },
                            error = function(e) "")
          if (length(title) == 1 && nchar(title) > 0 &&
              !grepl("Just a moment|Checking", title, ignore.case = TRUE)) {
            wait_ok <- TRUE; break
          }
        }
      }
      if (!wait_ok) return(NA)

      # Get page source
      src_res <- GET(paste0("http://localhost:", port, "/session/",
                             session_id, "/source"), timeout(30))
      html_raw <- fromJSON(content(src_res, as = "text"))$value
      page <- tryCatch(read_html(html_raw), error = function(e) NULL)
      if (is.null(page)) return(NA)

      # Verify: at least 1 row with rank number + team name
      rows <- page %>% html_nodes("div.ui-table__row")
      if (length(rows) == 0) return(FALSE)

      valid_count <- 0
      for (r in rows) {
        rank_node <- r %>% html_node("div.tableCellRank")
        team_node <- r %>% html_node("a.tableCellParticipant__name")
        if (is.null(rank_node) || is.null(team_node)) next
        rank_str <- html_text(rank_node, trim = TRUE)
        team_name <- html_text(team_node, trim = TRUE)
        rank <- suppressWarnings(as.integer(gsub("\\.", "", rank_str)))
        if (!is.na(rank) && rank >= 1 && !is.na(team_name) &&
            nchar(team_name) > 0) {
          valid_count <- valid_count + 1
          if (valid_count >= 1) return(TRUE)
        }
      }
      return(FALSE)
    }, error = function(e) NA)

    results <- rbind(results, data.frame(
      row_idx = row_idx, verified = verified,
      stringsAsFactors = FALSE))

    cat(paste0("[W", worker_id, "] ", j, "/", nrow(task_slice),
                " row=", row_idx, " -> ",
                if (isTRUE(verified)) "OK"
                else if (isFALSE(verified)) "FAIL" else "NA", "\n"),
        file = log_path, append = TRUE)

    # Save partial every 10 entries
    if (j %% 10 == 0) {
      saveRDS(results,
              file.path(output_dir, paste0("worker_", worker_id, "_results.rds")))
    }
  }

  # Final save
  saveRDS(results,
          file.path(output_dir, paste0("worker_", worker_id, "_results.rds")))

  # Close Selenium session
  tryCatch(DELETE(paste0("http://localhost:", port, "/session/", session_id),
                    timeout(10)), error = function(e) NULL)

  return(nrow(results))
}

# ════════════════════════════════════════════════════════════
# MAIN
# ════════════════════════════════════════════════════════════
cat("\n=== PARALLEL FLASHSCORE LEAGUE VERIFICATION ===\n")

# Kill any leftover chromedrivers
system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
       ignore.stderr = TRUE, wait = TRUE)
Sys.sleep(2)

d <- readRDS(DIRECTORY_PATH)
cat("Total directory entries:", nrow(d), "\n")

if (!"verified" %in% names(d)) d$verified <- NA

# Only re-check FALSE and NA entries where is_league is TRUE
to_verify_idx <- which(d$is_league == TRUE & !is.na(d$is_league) &
                        (is.na(d$verified) | d$verified == FALSE))
cat("Entries needing re-verification (FALSE + NA):", length(to_verify_idx), "\n\n")

if (length(to_verify_idx) == 0) {
  cat("Nothing to verify. Exiting.\n")
  quit()
}

# Build task table
tasks <- data.frame(
  row_idx    = to_verify_idx,
  league_url = d$league_url[to_verify_idx],
  stringsAsFactors = FALSE
)

# Split evenly across workers
worker_slices <- split(tasks, cut(seq_len(nrow(tasks)),
                                    N_WORKERS, labels = FALSE))
cat("Workers:", N_WORKERS, "  Approx tasks per worker:",
    round(nrow(tasks) / N_WORKERS), "\n\n")

# Wipe old worker results
old <- list.files(WORKER_DIR, pattern = "worker_.*\\.(rds|txt)$",
                   full.names = TRUE)
if (length(old) > 0) file.remove(old)

# Launch workers using mclapply-style parallel — on Windows we use parLapply
cl <- makeCluster(N_WORKERS)
on.exit({
  stopCluster(cl)
  system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
         ignore.stderr = TRUE, wait = TRUE)
}, add = TRUE)

# Export required objects to cluster
clusterExport(cl, c("worker_verify", "CHROMEDRIVER", "CHROME_PATH",
                     "BASE_URL", "WORKER_DIR"))

t0 <- Sys.time()

# Dispatch: each worker gets its id + slice
worker_args <- lapply(seq_len(N_WORKERS), function(i) {
  list(worker_id = i, task_slice = worker_slices[[i]])
})

results_counts <- parLapply(cl, worker_args, function(a) {
  worker_verify(a$worker_id, a$task_slice, CHROMEDRIVER, CHROME_PATH,
                 BASE_URL, WORKER_DIR)
})

t1 <- Sys.time()
cat("\nAll workers finished in ", round(as.numeric(t1 - t0, units = "mins"), 1),
    " minutes\n", sep="")

# ════════════════════════════════════════════════════════════
# AGGREGATE AND SAVE
# ════════════════════════════════════════════════════════════
all_results <- do.call(rbind, lapply(seq_len(N_WORKERS), function(i) {
  path <- file.path(WORKER_DIR, paste0("worker_", i, "_results.rds"))
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}))

cat("\nTotal results aggregated:", nrow(all_results), "\n")

# Apply to directory
for (k in seq_len(nrow(all_results))) {
  d$verified[all_results$row_idx[k]] <- all_results$verified[k]
}

saveRDS(d, DIRECTORY_PATH)
cat("Saved updated directory to:", DIRECTORY_PATH, "\n\n")

# ════════════════════════════════════════════════════════════
# SUMMARY
# ════════════════════════════════════════════════════════════
cat("=== FINAL SUMMARY ===\n")
cat("Total entries in directory:", nrow(d), "\n")
cat("is_league = TRUE           :", sum(d$is_league == TRUE, na.rm=T), "\n")
cat("Verified TRUE              :", sum(d$verified == TRUE, na.rm=T), "\n")
cat("Verified FALSE             :", sum(d$verified == FALSE, na.rm=T), "\n")
cat("Verified NA (still errored):", sum(is.na(d$verified) & d$is_league == TRUE), "\n")

# Show what got flipped from FALSE/NA to TRUE
flipped <- all_results[isTRUE(all_results$verified), ]
if (nrow(flipped) > 0) {
  cat("\nEntries flipped to VERIFIED = TRUE:\n")
  print(d[flipped$row_idx, c("country","league_name")])
}

# ============================================================
# M.O.A.B — Simulator
# Test new analysis code against historical enriched fixtures.
# Two result modes:
#   (A) FRESH: enriched fixtures with no results yet -> scrape with 6 workers,
#       save results back to an .rds so next time you just upload that.
#   (B) CACHED: upload an enriched-with-results .rds -> no scraping needed.
# Then run analysis code, grade picks, compare runs, push to production.
# Theme: Cobalt + Soft Gold on Ivory. Accent rail: GOLD (distinct from checker).
# ============================================================

library(shiny)
library(DT)
library(dplyr)
library(openxlsx)
library(jsonlite)
use_ace <- requireNamespace("shinyAce", quietly = TRUE)
if (use_ace) library(shinyAce)

BASE_PATH        <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
ANALYSIS_PATH    <- file.path(BASE_PATH, "moab_analysis.R")
SCRAPER_PATH     <- file.path(BASE_PATH, "moab_result_scraper.R")
SIM_HISTORY_PATH <- file.path(BASE_PATH, "moab_sim_history.json")
RESULTS_DIR      <- file.path(BASE_PATH, "results")
SIM_WORKER_DIR   <- file.path(BASE_PATH, "sim_worker_progress")
if (!dir.exists(RESULTS_DIR)) dir.create(RESULTS_DIR, recursive = TRUE)
if (!dir.exists(SIM_WORKER_DIR)) dir.create(SIM_WORKER_DIR, recursive = TRUE)

if (file.exists(SCRAPER_PATH)) source(SCRAPER_PATH, local = FALSE)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

clean_code_text <- function(s) {
  if (is.null(s) || nchar(s) == 0) return(s)
  s <- gsub("\ufeff", "", s); s <- gsub("\u00A0", " ", s)
  s <- gsub("\u200B|\u200C|\u200D", "", s)
  s <- gsub("\r\n", "\n", s); s <- gsub("\r", "\n", s); s
}

read_production_code <- function() {
  if (!file.exists(ANALYSIS_PATH)) return("")
  tryCatch(paste(readLines(ANALYSIS_PATH, warn = FALSE), collapse = "\n"), error = function(e) "")
}

load_sim_history <- function() {
  if (!file.exists(SIM_HISTORY_PATH)) return(list())
  tryCatch(fromJSON(SIM_HISTORY_PATH, simplifyDataFrame = FALSE), error = function(e) list())
}
save_sim_history <- function(h) {
  tryCatch({
    write(toJSON(h, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 6), SIM_HISTORY_PATH)
    TRUE
  }, error = function(e) { message("save_sim_history failed: ", e$message); FALSE })
}

to_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (!is.list(x) || length(x) == 0) return(NULL)
  if (is.list(x[[1]]) && !is.null(names(x[[1]]))) {
    return(tryCatch({
      cols <- unique(unlist(lapply(x, names)))
      rows <- lapply(x, function(r) {
        v <- setNames(vector("list", length(cols)), cols)
        for (c in cols) { val <- r[[c]]; v[[c]] <- if (is.null(val) || length(val) == 0) NA else val[[1]] }
        as.data.frame(v, stringsAsFactors = FALSE)
      })
      do.call(rbind, rows)
    }, error = function(e) NULL))
  }
  return(tryCatch({
    lens <- sapply(x, length); n <- max(lens)
    cols <- lapply(x, function(v) { v <- if (is.list(v)) unlist(v) else v; if (length(v) < n) rep(v, length.out = n) else v })
    as.data.frame(cols, stringsAsFactors = FALSE)
  }, error = function(e) NULL))
}

THEME_CSS <- "
*,*::before,*::after{margin:0;padding:0;box-sizing:border-box}
:root{
  --ivory:#FAF7F2;--ivory-2:#F5EEDF;--cobalt:#1E3A8A;--cobalt-deep:#0F1F4D;
  --gold:#C9A14A;--gold-deep:#8C6A1E;
  --text:#0F1F4D;--text-2:#5c5448;--text-3:#8e8678;
  --border:#e8dfc9;--white:#fff;
  --success:#0F6E56;--success-bg:#E1F5EE;--danger:#A32D2D;--danger-bg:#FCEBEB;
  --emerald:#0d8a6c;--emerald-bg:#d4f5e8;--amber:#b8860b;--amber-bg:#fef3c7;
  --fh:'Fraunces',Georgia,serif;--fb:'Inter',system-ui,sans-serif;--fm:'JetBrains Mono',monospace;
}
body{background:var(--ivory);color:var(--text);font-family:var(--fb)}
.hd{background:var(--cobalt-deep);color:var(--ivory);padding:24px 36px;display:flex;
    align-items:center;justify-content:space-between;border-bottom:3px solid var(--gold);
    position:relative;overflow:hidden}
.hd::after{content:'LABORATORY';position:absolute;right:30px;bottom:-12px;font-family:var(--fh);
    font-size:64px;font-weight:900;color:rgba(201,161,74,0.10);letter-spacing:6px;line-height:1}
.logo{font-family:var(--fh);font-size:32px;font-weight:900;letter-spacing:-0.5px;line-height:1;position:relative;z-index:1}
.logo .dot{color:var(--gold)}
.sub{font-family:var(--fb);font-size:10px;font-weight:600;letter-spacing:4px;
      text-transform:uppercase;color:rgba(250,247,242,0.75);margin-top:6px;position:relative;z-index:1}
.pg{max-width:1500px;margin:0 auto;padding:32px 36px}
.cd{background:var(--white);border:1px solid var(--border);border-radius:6px;
    padding:24px 26px;margin-bottom:18px;position:relative;box-shadow:0 1px 3px rgba(15,31,77,0.04)}
.cd::before{content:'';position:absolute;top:0;left:0;width:48px;height:2px;background:var(--gold)}
.ct{font-family:var(--fb);font-size:10px;font-weight:700;letter-spacing:3px;
    text-transform:uppercase;color:var(--text-3);margin-bottom:16px}
.bd{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
    letter-spacing:2px!important;text-transform:uppercase!important;
    background:var(--cobalt)!important;color:var(--ivory)!important;border:none!important;
    border-radius:3px!important;padding:12px 26px!important;cursor:pointer!important;
    box-shadow:0 2px 8px rgba(30,58,138,0.18)!important}
.bd:hover{background:var(--cobalt-deep)!important}
.bo{font-family:var(--fb)!important;font-size:10px!important;font-weight:600!important;
    letter-spacing:2px!important;text-transform:uppercase!important;background:transparent!important;
    color:var(--cobalt)!important;border:1px solid var(--cobalt)!important;border-radius:3px!important;
    padding:9px 22px!important;cursor:pointer!important}
.bo:hover{background:var(--cobalt)!important;color:var(--ivory)!important}
.bgold{font-family:var(--fb)!important;font-size:10px!important;font-weight:600!important;
        letter-spacing:2px!important;text-transform:uppercase!important;background:var(--gold)!important;
        color:var(--cobalt-deep)!important;border:none!important;border-radius:3px!important;
        padding:9px 22px!important;cursor:pointer!important}
.bgold:hover{background:var(--gold-deep)!important;color:var(--ivory)!important}
.bdanger{background:var(--danger)!important;color:#fff!important;border:none!important}
.ks{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:24px}
.kc{background:var(--white);border:1px solid var(--border);border-radius:6px;padding:22px 24px;text-align:center;position:relative}
.kc::before{content:'';position:absolute;top:0;left:0;width:32px;height:2px;background:var(--gold)}
.kn{font-family:var(--fh);font-size:38px;font-weight:900;line-height:1;color:var(--cobalt)}
.kl{font-family:var(--fm);font-size:9px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);margin-top:10px}
.pl{display:inline-block;font-family:var(--fm);font-size:10px;font-weight:600;letter-spacing:1px;padding:4px 10px;border-radius:12px;border:1px solid}
.pl-ok{background:var(--success-bg);color:var(--success);border-color:#9FE1CB}
.pl-err{background:var(--danger-bg);color:var(--danger);border-color:#F7C1C1}
.pl-pend{background:var(--ivory-2);color:var(--text-3);border-color:var(--border)}
.nav-tabs{border:none!important;border-bottom:1px solid var(--border)!important;margin-bottom:32px!important}
.nav-tabs>li>a{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
                letter-spacing:2.5px!important;text-transform:uppercase!important;color:var(--text-3)!important;
                border:none!important;padding:14px 28px!important;border-bottom:2px solid transparent!important;background:transparent!important}
.nav-tabs>li.active>a{color:var(--cobalt)!important;border-bottom-color:var(--gold)!important}
table.dataTable thead th{background:var(--ivory-2)!important;font-size:10px!important;font-weight:700!important;
                          letter-spacing:2px!important;text-transform:uppercase!important;color:var(--text-2)!important;
                          font-family:var(--fb)!important;padding:14px 16px!important}
table.dataTable tbody td{font-size:13px!important;padding:11px 16px!important;font-family:var(--fb)!important}
.lab{font-family:var(--fh);font-size:24px;font-weight:700;color:var(--cobalt-deep);margin-bottom:6px}
.lab-sub{font-family:var(--fb);font-size:13px;color:var(--text-3);margin-bottom:24px}
.grid-cell{padding:10px 8px;text-align:center;font-family:var(--fm);font-size:11px;border:1px solid var(--border)}
.grid-100{background:var(--success-bg);color:var(--success);font-weight:700}
.grid-90{background:var(--emerald-bg);color:var(--emerald);font-weight:600}
.grid-75{background:var(--amber-bg);color:var(--amber);font-weight:600}
.grid-low{background:var(--danger-bg);color:var(--danger);font-weight:600}
.grid-empty{background:var(--ivory-2);color:var(--text-3)}
.grid-overall{background:#e8f5f0;border:2px solid var(--emerald)}
.log-pane{background:var(--cobalt-deep);border-radius:4px;padding:16px 18px;font-family:var(--fm);
          font-size:11px;color:var(--gold);height:200px;overflow-y:auto;white-space:pre-wrap;line-height:1.8}
.mode-pill{display:inline-block;padding:3px 10px;border-radius:10px;font-family:var(--fm);font-size:10px;
            background:var(--ivory-2);border:1px solid var(--border);color:var(--text-2);margin-left:8px}
"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,700;9..144,900&family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML(THEME_CSS))
  ),
  div(class = "hd",
      div(div(class = "logo", "M.O", tags$span(class = "dot", "."), "A.B"),
          div(class = "sub", "Simulator \u00b7 Strategy Laboratory")),
      div(style = "font-family:var(--fm); font-size:11px; color:rgba(250,247,242,0.8); position:relative; z-index:1;",
          textOutput("hdr_status", inline = TRUE))),
  div(class = "pg",
      tabsetPanel(id = "tabs",
                  tabPanel("Lab", value = "lab",
                           div(style = "padding-top:8px;",
                               div(class = "lab", "Strategy laboratory"),
                               div(class = "lab-sub", "Load analysis code, supply test data, run, grade, then push winners to production."),
                               div(class = "cd",
                                   div(class = "ct", "Analysis code"),
                                   div(style = "display:flex; gap:10px; margin-bottom:14px; flex-wrap:wrap;",
                                       actionButton("load_prod_btn", "Load production code", class = "bo"),
                                       fileInput("code_upload", NULL, accept = ".R", buttonLabel = "Upload .R", placeholder = "")),
                                   if (use_ace)
                                     aceEditor("code_editor", value = "", mode = "r", theme = "tomorrow", fontSize = 12, height = "460px")
                                   else
                                     textAreaInput("code_editor", NULL, value = "", height = "460px", width = "100%",
                                                   placeholder = "Paste analysis code...")),
                               div(class = "cd",
                                   div(class = "ct", "Test data \u00b7 enriched fixtures"),
                                   fileInput("enriched_upload", NULL, accept = ".rds", width = "100%",
                                              buttonLabel = "Upload enriched_fixtures.rds"),
                                   uiOutput("data_status"),
                                   div(style = "margin-top:14px; padding-top:14px; border-top:1px dashed var(--border);"),
                                   div(class = "ct", "Results for grading"),
                                   div(style = "font-family:var(--fb); font-size:12px; color:var(--text-2); margin-bottom:12px;",
                                       "If this is a fresh round with no results yet, scrape them once (6 workers). They save back to disk so you never re-scrape."),
                                   div(style = "display:flex; gap:10px; align-items:center; flex-wrap:wrap;",
                                       actionButton("scrape_btn", "Scrape results (6 workers)", class = "bgold"),
                                       downloadButton("dl_results_rds", "Save results file (RDS)", class = "bo"),
                                       fileInput("results_upload", NULL, accept = ".rds",
                                                  buttonLabel = "Upload saved results", placeholder = ""),
                                       uiOutput("results_status"))),
                               div(class = "cd",
                                   div(class = "ct", "Run"),
                                   div(style = "display:flex; gap:10px; flex-wrap:wrap;",
                                       actionButton("validate_btn", "Validate", class = "bo"),
                                       actionButton("run_btn", "Run simulation", class = "bd"),
                                       actionButton("commit_btn", "Commit run", class = "bgold"),
                                       actionButton("push_prod_btn", "Push to production", class = "bd bdanger"))),
                               div(class = "cd",
                                   div(class = "ct", "Activity log"),
                                   div(class = "log-pane", textOutput("log_text"))))),
                  tabPanel("Results", value = "results",
                           div(style = "padding-top:8px;",
                               div(class = "lab", "Simulation result"),
                               div(class = "lab-sub", "Per-market pick counts and hit rates (graded against scraped results)."),
                               uiOutput("sim_summary"),
                               uiOutput("sim_detail_tables"))),
                  tabPanel("Comparison", value = "comparison",
                           div(style = "padding-top:8px;",
                               div(class = "lab", "Run comparison"),
                               div(class = "lab-sub", "Rows = committed runs, columns = markets."),
                               uiOutput("comparison_grid"))))))

server <- function(input, output, session) {
  rv <- reactiveValues(
    log = "Ready. Load code, upload enriched fixtures, then scrape or upload results.\n",
    enriched = NULL,
    results_cache = list(),   # match_id -> scrape_result
    merged_simready = NULL,   # enriched fixtures + result_* columns, one file
    sim_results = NULL,
    graded = NULL,
    sim_ran_at = NULL,
    sim_history = load_sim_history()
  )

  log_msg <- function(m) { ts <- format(Sys.time(), "%H:%M:%S"); rv$log <- paste0(rv$log, "[", ts, "] ", m, "\n") }
  output$log_text <- renderText({ rv$log })
  output$hdr_status <- renderText({ paste0(length(rv$sim_history), " run", if (length(rv$sim_history)==1) "" else "s", " committed") })

  observeEvent(input$load_prod_btn, {
    code <- read_production_code()
    if (nchar(code) == 0) { showNotification("Production code not found", type = "error"); return() }
    if (use_ace) updateAceEditor(session, "code_editor", value = code) else updateTextAreaInput(session, "code_editor", value = code)
    log_msg(paste0("Loaded production code (", nchar(code), " chars)"))
  })

  observeEvent(input$code_upload, {
    req(input$code_upload)
    code <- clean_code_text(tryCatch(paste(readLines(input$code_upload$datapath, warn = FALSE), collapse = "\n"), error = function(e) ""))
    if (use_ace) updateAceEditor(session, "code_editor", value = code) else updateTextAreaInput(session, "code_editor", value = code)
    log_msg(paste0("Uploaded ", input$code_upload$name))
  })

  observeEvent(input$enriched_upload, {
    req(input$enriched_upload)
    df <- tryCatch(readRDS(input$enriched_upload$datapath), error = function(e) NULL)
    if (is.null(df)) { showNotification("Could not read enriched fixtures", type = "error"); return() }
    rv$enriched <- df
    log_msg(paste0("Loaded ", length(df), " enriched fixtures"))
  })

  output$data_status <- renderUI({
    if (is.null(rv$enriched)) div(style = "font-family:var(--fm); font-size:11px; color:var(--text-3); margin-top:8px;", "No data loaded")
    else div(style = "font-family:var(--fm); font-size:11px; color:var(--success); margin-top:8px;", paste0(length(rv$enriched), " fixtures ready"))
  })

  output$results_status <- renderUI({
    n <- length(rv$results_cache)
    if (n == 0) div(class = "mode-pill", "no results yet")
    else div(class = "mode-pill", paste0(n, " fixtures scraped"))
  })

  # Upload an already-scraped results file
  observeEvent(input$results_upload, {
    req(input$results_upload)
    obj <- tryCatch(readRDS(input$results_upload$datapath), error = function(e) NULL)
    if (is.null(obj) || !is.list(obj)) { showNotification("Bad results file", type = "error"); return() }
    rv$results_cache <- obj
    log_msg(paste0("Loaded ", length(obj), " cached results from file"))
    showNotification("Results loaded", type = "message", duration = 3)
  })

  # Scrape results for all fixtures in enriched data (6 workers)
  # UTC kickoff parser (same logic as live checker)
  pick_kickoff <- function(date_str, time_str) {
    if (is.null(date_str) || is.na(date_str) || nchar(as.character(date_str)) == 0)
      return(as.POSIXct(NA, tz = "UTC"))
    date_str <- as.character(date_str)
    time_str <- as.character(time_str %||% "23:59")
    if (is.na(time_str) || nchar(time_str) == 0) time_str <- "23:59"
    ts <- paste(date_str, time_str)
    fmts <- c("%Y-%m-%d %H:%M", "%d.%m.%Y %H:%M", "%d/%m/%Y %H:%M",
              "%d-%m-%Y %H:%M", "%d.%m.%y %H:%M", "%Y-%m-%dT%H:%M",
              "%Y/%m/%d %H:%M")
    for (f in fmts) {
      p <- suppressWarnings(as.POSIXct(ts, format = f, tz = "UTC"))
      if (!is.na(p)) return(p)
    }
    for (f in c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%y")) {
      p <- suppressWarnings(as.POSIXct(paste(date_str, "23:59"),
                                        format = paste(f, "%H:%M"), tz = "UTC"))
      if (!is.na(p)) return(p)
    }
    as.POSIXct(NA, tz = "UTC")
  }

  observeEvent(input$scrape_btn, {
    req(rv$enriched)
    # Build unique fixture queue WITH date/time from enriched fixtures
    fx_rows <- lapply(rv$enriched, function(e) {
      if (is.null(e) || is.null(e$fixture)) return(NULL)
      f <- e$fixture
      mid <- as.character(f$match_id %||% NA); mu <- as.character(f$match_url %||% NA)
      if (is.na(mid) || is.na(mu) || nchar(mu) == 0) return(NULL)
      data.frame(match_id = mid, match_url = mu,
                  fixture_date = as.character(f$fixture_date %||% NA),
                  match_time   = as.character(f$match_time %||% NA),
                  stringsAsFactors = FALSE)
    })
    fx <- bind_rows(Filter(Negate(is.null), fx_rows))
    if (is.null(fx) || nrow(fx) == 0) { showNotification("No fixture URLs found", type = "error"); return() }
    fx <- distinct(fx, match_id, match_url, fixture_date, match_time)
    cache <- rv$results_cache %||% list()

    now_ts <- Sys.time(); grace_secs <- 2 * 3600
    is_played <- logical(nrow(fx)); is_upcoming <- logical(nrow(fx))
    first_debug <- NULL
    for (i in seq_len(nrow(fx))) {
      mid <- as.character(fx$match_id[i])
      r <- cache[[mid]]
      if (!is.null(r) && (r$status %||% "") == "FINAL") next   # skip already final
      ko <- pick_kickoff(fx$fixture_date[i], fx$match_time[i])
      if (is.null(first_debug)) {
        first_debug <- list(date = fx$fixture_date[i], time = fx$match_time[i],
                             parsed = ko, now = now_ts,
                             delta_hours = round(as.numeric(difftime(now_ts, ko, units = "hours")), 2))
      }
      if (is.na(ko)) { is_played[i] <- TRUE; next }
      delta <- as.numeric(difftime(now_ts, ko, units = "secs"))
      if (delta >= grace_secs) is_played[i] <- TRUE else is_upcoming[i] <- TRUE
    }

    if (!is.null(first_debug)) {
      log_msg(paste0("DEBUG first fixture: date=", first_debug$date,
                     " time=", first_debug$time,
                     " | parsed UTC=", format(first_debug$parsed, "%Y-%m-%d %H:%M", tz = "UTC"),
                     " | now=", format(first_debug$now, "%Y-%m-%d %H:%M %Z"),
                     " | now-kickoff=", first_debug$delta_hours, "h"))
    }

    n_upcoming <- sum(is_upcoming)
    n_already_final <- nrow(fx) - sum(is_played) - n_upcoming
    fx <- fx[is_played, , drop = FALSE]
    if (nrow(fx) == 0) {
      msg <- if (n_upcoming > 0) "Nothing to scrape yet \u2014 all remaining fixtures are upcoming."
             else "All fixtures already scraped."
      showNotification(msg, type = "message"); return()
    }
    if (n_upcoming > 0)
      showNotification(paste0(n_upcoming, " unplayed fixture",
                              if (n_upcoming == 1) "" else "s",
                              " skipped (kept as PENDING)."),
                       type = "message", duration = 5)

    work_queue <- lapply(seq_len(nrow(fx)), function(i) {
      mu <- as.character(fx$match_url[i])
      list(match_id = as.character(fx$match_id[i]), url = if (startsWith(mu, "http")) mu else paste0(BASE_URL, mu))
    })
    log_msg(paste0("Scraping ", length(work_queue), " played fixtures with ",
                   N_PARALLEL_WORKERS, " workers (", n_upcoming,
                   " upcoming, ", n_already_final, " already final)..."))
    tryCatch({
      results <- parallel_scrape_results(work_queue, SIM_WORKER_DIR, N_PARALLEL_WORKERS, log_fn = log_msg)
      for (mid in names(results)) cache[[mid]] <- results[[mid]]
      rv$results_cache <- cache
      
      # Merge results onto the enriched fixtures so we save ONE file
      # containing both features + results — ready for grid search.
      merged <- rv$enriched
      result_fields <- c("status", "ft_home", "ft_away", "ht_home", "ht_away",
                         "total_corners", "home_corners", "away_corners",
                         "total_cards", "home_cards", "away_cards",
                         "total_shots", "total_sot")
      for (f in result_fields) {
        col <- paste0("result_", f)
        merged[[col]] <- sapply(merged$match_id, function(mid) {
          r <- cache[[as.character(mid)]]
          v <- if (!is.null(r)) r[[f]] else NULL
          if (is.null(v) || length(v) == 0) NA else v[1]
        })
      }
      rv$merged_simready <- merged
      
      out_path <- file.path(RESULTS_DIR, paste0("MoabSim_Ready_", format(Sys.Date(), "%Y%m%d"), "_",
                                                  format(Sys.time(), "%H%M"), ".rds"))
      saveRDS(merged, out_path)
      n_final <- sum(sapply(cache, function(r) (r$status %||% "") == "FINAL"))
      log_msg(paste0("Scraped. ", n_final, " final results. Sim-ready file saved: ", basename(out_path)))
      showNotification(paste0("Scraped ", length(results), " fixtures \u2014 merged file saved (",
                              nrow(merged), " rows, ", round(file.size(out_path)/1024), " KB)"),
                       type = "message", duration = 5)
    }, error = function(e) { log_msg(paste0("Scrape error: ", e$message)); showNotification(paste0("Error: ", e$message), type = "error", duration = 6) })
  })

  output$dl_results_rds <- downloadHandler(
    filename = function() paste0("MoabSim_Ready_", format(Sys.Date(), "%Y%m%d"), "_",
                                 format(Sys.time(), "%H%M"), ".rds"),
    content = function(file) {
      d <- rv$merged_simready %||% rv$results_cache
      saveRDS(d, file)
    })

  observeEvent(input$validate_btn, {
    code <- clean_code_text(input$code_editor %||% "")
    if (nchar(code) == 0) { showNotification("No code", type = "warning"); return() }
    parsed <- tryCatch(parse(text = code), error = function(e) e)
    if (inherits(parsed, "error")) { log_msg(paste0("VALIDATION FAILED: ", parsed$message)); showNotification(paste0("Syntax error: ", parsed$message), type = "error", duration = 6); return() }
    log_msg("Validation passed."); showNotification("Code is valid", type = "message", duration = 3)
  })

  # Grade sim picks using results_cache
  grade_with_cache <- function(preds) {
    cache <- rv$results_cache %||% list()
    if (length(cache) == 0) return(NULL)
    lapply(preds, function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)
      df$outcome <- sapply(seq_len(nrow(df)), function(i) {
        res <- cache[[as.character(df$match_id[i])]]
        if (is.null(res)) "PENDING" else grade_pick(df$market[i], res)
      })
      df
    })
  }

  observeEvent(input$run_btn, {
    code <- clean_code_text(input$code_editor %||% "")
    if (nchar(code) == 0) { showNotification("No code", type = "warning"); return() }
    if (is.null(rv$enriched)) { showNotification("Upload enriched data first", type = "warning"); return() }
    env <- new.env()
    ok <- tryCatch({ eval(parse(text = code), envir = env); TRUE },
                   error = function(e) { log_msg(paste0("SOURCE ERROR: ", e$message)); showNotification(paste0("Error: ", e$message), type = "error", duration = 6); FALSE })
    if (!ok) return()
    if (!exists("run_moab_analysis", envir = env)) {
      log_msg("Code lacks run_moab_analysis()"); showNotification("Must define run_moab_analysis()", type = "error", duration = 6); return()
    }
    thresholds <- if (exists("DEFAULT_MOAB_THRESHOLDS", envir = env)) get("DEFAULT_MOAB_THRESHOLDS", envir = env) else list()
    log_msg(paste0("Running analysis on ", length(rv$enriched), " fixtures..."))
    withProgress(message = "Simulating", value = 0.3, {
      tryCatch({
        preds <- env$run_moab_analysis(rv$enriched, thresholds)
        rv$sim_results <- preds
        rv$graded <- grade_with_cache(preds)
        rv$sim_ran_at <- Sys.time()
        total <- sum(sapply(preds, function(d) if (is.null(d)) 0 else nrow(d)))
        log_msg(paste0("Done: ", total, " picks across ", length(preds), " markets",
                       if (is.null(rv$graded)) " (no results to grade)" else " (graded)"))
        updateTabsetPanel(session, "tabs", selected = "results")
      }, error = function(e) { log_msg(paste0("RUN ERROR: ", e$message)); showNotification(paste0("Error: ", e$message), type = "error", duration = 6) })
    })
  })

  observeEvent(input$commit_btn, {
    req(rv$sim_results)
    label <- paste0("Sim_", format(Sys.time(), "%Y%m%d_%H%M"))
    id <- format(Sys.time(), "%Y%m%d%H%M%S")
    rv$sim_history[[id]] <- list(id = id, label = label,
                                 committed = format(Sys.time(), "%d %b %Y \u00b7 %H:%M"),
                                 results = rv$graded %||% rv$sim_results,
                                 ran_at = as.character(rv$sim_ran_at))
    save_sim_history(rv$sim_history)
    log_msg(paste0("Committed run: ", label)); showNotification(paste0("Committed: ", label), type = "message", duration = 3)
  })

  observeEvent(input$push_prod_btn, {
    code <- clean_code_text(input$code_editor %||% "")
    if (nchar(code) == 0) { showNotification("No code", type = "warning"); return() }
    parsed <- tryCatch(parse(text = code), error = function(e) e)
    if (inherits(parsed, "error")) { showNotification("Fix syntax first", type = "error", duration = 5); return() }
    showModal(modalDialog(title = "Push to production?",
      HTML("<p style='font-family:Inter,sans-serif; font-size:14px;'>Overwrite <code>moab_analysis.R</code>. The main app uses this on next launch. A timestamped backup is kept.</p>"),
      footer = tagList(modalButton("Cancel"), actionButton("push_confirm", "Push", class = "bd bdanger")), easyClose = TRUE))
  })

  observeEvent(input$push_confirm, {
    code <- clean_code_text(input$code_editor %||% "")
    backup <- paste0(ANALYSIS_PATH, ".bak.", format(Sys.time(), "%Y%m%d_%H%M%S"))
    tryCatch({
      if (file.exists(ANALYSIS_PATH)) file.copy(ANALYSIS_PATH, backup, overwrite = TRUE)
      writeLines(code, ANALYSIS_PATH)
      log_msg(paste0("Pushed to production (backup ", basename(backup), ")"))
      showNotification("Production updated", type = "message", duration = 4); removeModal()
    }, error = function(e) { log_msg(paste0("Push error: ", e$message)); showNotification(paste0("Error: ", e$message), type = "error", duration = 6) })
  })

  output$sim_summary <- renderUI({
    req(rv$sim_results)
    p <- rv$graded %||% rv$sim_results
    counts <- sapply(p, function(d) if (is.null(d)) 0 else nrow(d)); total <- sum(counts)
    if (!is.null(rv$graded)) {
      all <- bind_rows(Filter(function(d) !is.null(d) && nrow(d) > 0, p))
      w <- sum(all$outcome == "WIN", na.rm = TRUE); l <- sum(all$outcome == "LOSS", na.rm = TRUE)
      hr <- if ((w + l) > 0) paste0(round(w / (w + l) * 100, 1), "%") else "\u2014"
    } else { w <- l <- 0; hr <- "\u2014" }
    div(class = "ks",
        div(class = "kc", div(class = "kn", total), div(class = "kl", "Total picks")),
        div(class = "kc", div(class = "kn", w), div(class = "kl", "Wins")),
        div(class = "kc", div(class = "kn", l), div(class = "kl", "Losses")),
        div(class = "kc", div(class = "kn", hr), div(class = "kl", "Hit rate")))
  })

  output$sim_detail_tables <- renderUI({
    req(rv$sim_results)
    p <- rv$graded %||% rv$sim_results
    nonzero <- names(p)[sapply(p, function(d) !is.null(d) && nrow(d) > 0)]
    if (length(nonzero) == 0) return(div(class = "cd", style = "text-align:center;", div(style = "font-family:var(--fh); color:var(--text-3);", "No picks")))
    blocks <- lapply(nonzero, function(mkt) {
      df <- p[[mkt]]; tbl_id <- paste0("sim_tbl_", mkt); label <- df$market[1]
      hdr <- if (!is.null(df$outcome)) {
        w <- sum(df$outcome == "WIN", na.rm = TRUE); l <- sum(df$outcome == "LOSS", na.rm = TRUE)
        hr <- if ((w + l) > 0) paste0(round(w / (w + l) * 100, 1), "%") else "\u2014"
        paste0(label, " \u2014 ", nrow(df), " picks (", w, "W ", l, "L = ", hr, ")")
      } else paste0(label, " \u2014 ", nrow(df), " picks")
      div(class = "cd", div(class = "ct", hdr), DTOutput(tbl_id))
    })
    for (mkt in nonzero) local({
      m <- mkt; df <- p[[m]]
      if (!is.null(df$outcome)) df$outcome_badge <- sapply(df$outcome, function(o) {
        cls <- switch(o, WIN = "pl-ok", LOSS = "pl-err", "pl-pend"); paste0('<span class="pl ', cls, '">', o, '</span>') })
      output[[paste0("sim_tbl_", m)]] <- renderDT({
        show <- intersect(c("league","fixture_date","home","away","pick","confidence","expected","outcome_badge"), names(df))
        d <- df[, show, drop = FALSE]
        if ("outcome_badge" %in% names(d)) names(d)[names(d) == "outcome_badge"] <- "outcome"
        datatable(d, rownames = FALSE, selection = "none", escape = FALSE,
                  options = list(pageLength = 15, dom = "ftp", scrollX = TRUE))
      }, server = FALSE)
    })
    do.call(tagList, blocks)
  })

  output$comparison_grid <- renderUI({
    h <- rv$sim_history
    if (length(h) == 0) return(div(class = "cd", style = "text-align:center; padding:60px;",
                                    div(style = "font-family:var(--fh); font-size:18px; color:var(--text-3);", "No committed runs")))
    all_markets <- unique(unlist(lapply(h, function(s) { r <- s$results; if (is.null(r)) character() else names(r) })))
    if (length(all_markets) == 0) return(div(class = "cd", "No data"))
    color_cell <- function(w, l, n) {
      if (n == 0) return('<td class="grid-cell grid-empty">\u2014</td>')
      if ((w + l) == 0) return(sprintf('<td class="grid-cell grid-empty">%d picks</td>', n))
      hr <- w / (w + l) * 100
      cls <- if (hr == 100) "grid-100" else if (hr >= 90) "grid-90" else if (hr >= 75) "grid-75" else "grid-low"
      sprintf('<td class="grid-cell %s">%s%%<br>%d/%d</td>', cls, round(hr, 0), w, w + l)
    }
    ord <- names(h)[order(sapply(h, function(x) x$id))]
    body_rows <- lapply(ord, function(id) {
      e <- h[[id]]; cells <- ""; tw <- 0; tl <- 0; tp <- 0
      for (mkt in all_markets) {
        df <- to_df(e$results[[mkt]])
        if (is.null(df) || nrow(df) == 0) { cells <- paste0(cells, color_cell(0,0,0)); next }
        n <- nrow(df)
        w <- if (!is.null(df$outcome)) sum(df$outcome == "WIN", na.rm = TRUE) else 0
        l <- if (!is.null(df$outcome)) sum(df$outcome == "LOSS", na.rm = TRUE) else 0
        cells <- paste0(cells, color_cell(w, l, n)); tw <- tw + w; tl <- tl + l; tp <- tp + n
      }
      ov <- if (tp == 0) '<td class="grid-cell grid-empty grid-overall">\u2014</td>'
            else if ((tw + tl) == 0) sprintf('<td class="grid-cell grid-empty grid-overall">%d picks</td>', tp)
            else { hr <- tw / (tw + tl) * 100
                   cls <- if (hr == 100) "grid-100" else if (hr >= 90) "grid-90" else if (hr >= 75) "grid-75" else "grid-low"
                   sprintf('<td class="grid-cell %s grid-overall">%s%%<br>%d/%d</td>', cls, round(hr, 0), tw, tw + tl) }
      paste0('<tr><td style="padding:10px 12px; font-weight:600;">', e$label, '</td>', cells, ov, '</tr>')
    })
    header <- paste0('<th style="padding:10px 12px; background:var(--ivory-2);">Run</th>',
                     paste(sprintf('<th style="padding:10px 12px; background:var(--ivory-2); font-size:9px;">%s</th>', all_markets), collapse = ""),
                     '<th style="padding:10px 12px; background:#e8f5f0; font-size:9px;">OVERALL</th>')
    div(class = "cd", div(class = "ct", "Comparison grid"),
        HTML(paste0('<table style="width:100%; border-collapse:collapse; font-size:11px;">',
                    '<thead><tr>', header, '</tr></thead><tbody>',
                    paste(unlist(body_rows), collapse = ""), '</tbody></table>')))
  })
}

shinyApp(ui = ui, server = server)

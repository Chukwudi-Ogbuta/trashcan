# ============================================================
# M.O.A.B — Live Result Checker
# Upload predictions, scrape UNIQUE fixtures (deduped by match_id)
# with 6 parallel workers, grade each pick, save graded slip.
# Theme: Cobalt + Soft Gold on Ivory. Accent rail: COBALT (distinct from simulator).
# ============================================================

library(shiny)
library(DT)
library(dplyr)
library(openxlsx)
library(jsonlite)

BASE_PATH    <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
if (!dir.exists(BASE_PATH)) dir.create(BASE_PATH, recursive = TRUE)
HISTORY_PATH <- file.path(BASE_PATH, "moab_slip_history.json")
SCRAPER_PATH <- file.path(BASE_PATH, "moab_result_scraper.R")
WORKER_PROGRESS_DIR <- file.path(BASE_PATH, "checker_worker_progress")
if (!dir.exists(WORKER_PROGRESS_DIR)) dir.create(WORKER_PROGRESS_DIR, recursive = TRUE)

if (file.exists(SCRAPER_PATH)) source(SCRAPER_PATH, local = FALSE)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

load_history <- function() {
  if (!file.exists(HISTORY_PATH)) return(list())
  tryCatch(fromJSON(HISTORY_PATH, simplifyDataFrame = FALSE), error = function(e) list())
}
save_history <- function(h) {
  tryCatch({
    j <- toJSON(h, auto_unbox = TRUE, pretty = TRUE, na = "null", digits = 6)
    write(j, HISTORY_PATH)
    TRUE
  }, error = function(e) {
    message("save_history failed: ", e$message)
    FALSE
  })
}

# Coerce a picks data.frame to safe, JSON-round-trippable column types.
# read.xlsx can produce Date/factor/logical columns that break round-tripping.
normalize_picks <- function(df) {
  if (is.null(df) || nrow(df) == 0) return(df)
  for (col in names(df)) {
    v <- df[[col]]
    if (inherits(v, "Date") || inherits(v, "POSIXct")) df[[col]] <- as.character(v)
    else if (is.factor(v)) df[[col]] <- as.character(v)
  }
  # Force text columns to character, numeric stays numeric
  char_cols <- intersect(c("market","country","league","home","away","pick","match_id",
                           "match_url","fixture_date","match_time","outcome"), names(df))
  for (col in char_cols) df[[col]] <- as.character(df[[col]])
  df
}

to_df <- function(x) {
  if (is.null(x)) return(NULL)
  if (is.data.frame(x)) return(x)
  if (!is.list(x) || length(x) == 0) return(NULL)
  # Case A: list of row-lists (multi-row slip) -> x[[1]] is itself a list
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
  # Case B: named list of columns (jsonlite collapsed a single-row df, or columns-as-vectors)
  # e.g. list(market="HT Draw", home="Arsenal", ...) or list(market=c(...), home=c(...))
  return(tryCatch({
    lens <- sapply(x, length)
    n <- max(lens)
    cols <- lapply(x, function(v) {
      v <- if (is.list(v)) unlist(v) else v
      if (length(v) < n) rep(v, length.out = n) else v
    })
    as.data.frame(cols, stringsAsFactors = FALSE)
  }, error = function(e) NULL))
}

read_predictions <- function(path) {
  ext <- tolower(tools::file_ext(path))
  if (ext == "rds") {
    obj <- tryCatch(readRDS(path), error = function(e) NULL)
    if (is.list(obj) && !is.data.frame(obj))
      return(bind_rows(Filter(function(d) !is.null(d) && nrow(d) > 0, obj)))
    return(obj)
  }
  if (ext %in% c("xlsx", "xls")) {
    sheets <- tryCatch(getSheetNames(path), error = function(e) character())
    if ("All Picks" %in% sheets) return(read.xlsx(path, sheet = "All Picks"))
    return(read.xlsx(path, sheet = 1))
  }
  NULL
}

THEME_CSS <- "
*,*::before,*::after{margin:0;padding:0;box-sizing:border-box}
:root{
  --ivory:#FAF7F2;--ivory-2:#F5EEDF;--cobalt:#1E3A8A;--cobalt-deep:#0F1F4D;
  --gold:#C9A14A;--gold-deep:#8C6A1E;
  --text:#0F1F4D;--text-2:#5c5448;--text-3:#8e8678;
  --border:#e8dfc9;--white:#fff;
  --success:#0F6E56;--success-bg:#E1F5EE;
  --danger:#A32D2D;--danger-bg:#FCEBEB;
  --emerald:#0d8a6c;--emerald-bg:#d4f5e8;
  --amber:#b8860b;--amber-bg:#fef3c7;
  --fh:'Fraunces',Georgia,serif;--fb:'Inter',system-ui,sans-serif;--fm:'JetBrains Mono',monospace;
}
body{background:var(--ivory);color:var(--text);font-family:var(--fb)}
.hd{background:var(--cobalt);color:var(--ivory);padding:24px 36px;display:flex;
    align-items:center;justify-content:space-between;border-bottom:3px solid var(--gold);
    position:relative;overflow:hidden}
.hd::after{content:'RESULTS';position:absolute;right:30px;bottom:-12px;font-family:var(--fh);
    font-size:80px;font-weight:900;color:rgba(201,161,74,0.10);letter-spacing:4px;line-height:1}
.logo{font-family:var(--fh);font-size:32px;font-weight:900;letter-spacing:-0.5px;line-height:1;position:relative;z-index:1}
.logo .dot{color:var(--gold)}
.sub{font-family:var(--fb);font-size:10px;font-weight:600;letter-spacing:4px;
      text-transform:uppercase;color:rgba(250,247,242,0.75);margin-top:6px;position:relative;z-index:1}
.pg{max-width:1500px;margin:0 auto;padding:32px 36px}
.cd{background:var(--white);border:1px solid var(--border);border-radius:6px;
    padding:24px 26px;margin-bottom:18px;position:relative;box-shadow:0 1px 3px rgba(15,31,77,0.04)}
.cd::before{content:'';position:absolute;top:0;left:0;width:48px;height:2px;background:var(--cobalt)}
.ct{font-family:var(--fb);font-size:10px;font-weight:700;letter-spacing:3px;
    text-transform:uppercase;color:var(--text-3);margin-bottom:16px}
.step-n{display:inline-flex;align-items:center;justify-content:center;width:22px;height:22px;
    border-radius:50%;background:var(--cobalt);color:var(--ivory);font-family:var(--fm);
    font-size:11px;font-weight:600;margin-right:10px}
.bd{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
    letter-spacing:2px!important;text-transform:uppercase!important;
    background:var(--cobalt)!important;color:var(--ivory)!important;border:none!important;
    border-radius:3px!important;padding:12px 26px!important;cursor:pointer!important;
    box-shadow:0 2px 8px rgba(30,58,138,0.18)!important}
.bd:hover{background:var(--cobalt-deep)!important}
.bo{font-family:var(--fb)!important;font-size:10px!important;font-weight:600!important;
    letter-spacing:2px!important;text-transform:uppercase!important;
    background:transparent!important;color:var(--cobalt)!important;
    border:1px solid var(--cobalt)!important;border-radius:3px!important;
    padding:9px 22px!important;cursor:pointer!important}
.bo:hover{background:var(--cobalt)!important;color:var(--ivory)!important}
.ks{display:grid;grid-template-columns:repeat(4,1fr);gap:14px;margin-bottom:24px}
.kc{background:var(--white);border:1px solid var(--border);border-radius:6px;
    padding:22px 24px;text-align:center;position:relative}
.kc::before{content:'';position:absolute;top:0;left:0;width:32px;height:2px;background:var(--cobalt)}
.kn{font-family:var(--fh);font-size:38px;font-weight:900;line-height:1;color:var(--cobalt)}
.kl{font-family:var(--fm);font-size:9px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);margin-top:10px}
.pl{display:inline-block;font-family:var(--fm);font-size:10px;font-weight:600;
    letter-spacing:1px;padding:4px 10px;border-radius:12px;border:1px solid}
.pl-ok{background:var(--success-bg);color:var(--success);border-color:#9FE1CB}
.pl-err{background:var(--danger-bg);color:var(--danger);border-color:#F7C1C1}
.pl-pend{background:var(--ivory-2);color:var(--text-3);border-color:var(--border)}
.nav-tabs{border:none!important;border-bottom:1px solid var(--border)!important;margin-bottom:32px!important}
.nav-tabs>li>a{font-family:var(--fb)!important;font-size:11px!important;font-weight:600!important;
                letter-spacing:2.5px!important;text-transform:uppercase!important;
                color:var(--text-3)!important;border:none!important;padding:14px 28px!important;
                border-bottom:2px solid transparent!important;background:transparent!important}
.nav-tabs>li.active>a{color:var(--cobalt)!important;border-bottom-color:var(--gold)!important}
table.dataTable thead th{background:var(--ivory-2)!important;font-size:10px!important;
                          font-weight:700!important;letter-spacing:2px!important;
                          text-transform:uppercase!important;color:var(--text-2)!important;
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
"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Fraunces:opsz,wght@9..144,500;9..144,700;9..144,900&family=Inter:wght@400;500;600;700&family=JetBrains+Mono:wght@400;500&display=swap"),
    tags$style(HTML(THEME_CSS))
  ),
  div(class = "hd",
      div(div(class = "logo", "M.O", tags$span(class = "dot", "."), "A.B"),
          div(class = "sub", "Live Result Checker")),
      div(style = "font-family:var(--fm); font-size:11px; color:rgba(250,247,242,0.8); position:relative; z-index:1;",
          textOutput("hdr_status", inline = TRUE))),
  div(class = "pg",
      tabsetPanel(id = "tabs",
                  tabPanel("Active", value = "active",
                           div(style = "padding-top:8px;",
                               div(class = "lab", "Upload & grade"),
                               div(class = "lab-sub", "Upload predictions \u2192 commit \u2192 scrape unique fixtures with 6 workers \u2192 grade every pick."),
                               div(class = "cd",
                                   div(class = "ct", span(class="step-n","1"), "Upload predictions"),
                                   fileInput("pred_upload", NULL, accept = c(".xlsx", ".rds"), width = "100%"),
                                   uiOutput("upload_status"),
                                   div(style = "margin-top:14px;",
                                       div(style = "font-family:var(--fb); font-size:10px; font-weight:700; letter-spacing:2px; text-transform:uppercase; color:var(--text-3); margin-bottom:6px;",
                                           "Filter markets (optional)"),
                                       uiOutput("market_filter_ui"))),
                               div(class = "cd",
                                   div(class = "ct", span(class="step-n","2"), "Label & commit"),
                                   div(style = "display:flex; gap:14px; align-items:end;",
                                       div(style = "flex:1; max-width:320px;",
                                           textInput("slip_label", "Slip label",
                                                     value = paste0("Round_", format(Sys.Date(), "%Y%m%d")), width = "100%")),
                                       actionButton("commit_btn", "Commit slip", class = "bd"))),
                               div(class = "cd",
                                   div(class = "ct", span(class="step-n","3"), "Fetch results \u00b7 6 parallel workers"),
                                   div(style = "display:flex; gap:14px; align-items:center;",
                                       actionButton("fetch_btn", "Fetch results", class = "bd"),
                                       div(style = "font-family:var(--fm); font-size:11px; color:var(--text-3);",
                                           textOutput("fetch_info", inline = TRUE)))),
                               uiOutput("active_summary"),
                               uiOutput("active_table_card"),
                               uiOutput("export_card"))),
                  tabPanel("History", value = "history",
                           div(style = "padding-top:8px;",
                               div(class = "lab", "Slip history"),
                               div(class = "lab-sub", "Every committed slip with W/L per market."),
                               uiOutput("history_content"))),
                  tabPanel("Analytics Grid", value = "analytics",
                           div(style = "padding-top:8px;",
                               div(class = "lab", "Per-market analytics"),
                               div(class = "lab-sub", "Rows = slips, columns = markets, cumulative footer."),
                               uiOutput("analytics_grid"))))))

server <- function(input, output, session) {
  rv <- reactiveValues(uploaded = NULL, active_id = NULL, history = load_history())
  
  output$hdr_status <- renderText({
    n <- length(rv$history)
    if (n == 0) "No slips yet" else paste0(n, " slip", if (n == 1) "" else "s")
  })
  
  observeEvent(input$pred_upload, {
    req(input$pred_upload)
    df <- tryCatch(read_predictions(input$pred_upload$datapath), error = function(e) NULL)
    if (is.null(df) || nrow(df) == 0) { showNotification("Could not read predictions", type = "error"); return() }
    rv$uploaded <- df
    showNotification(paste0("Loaded ", nrow(df), " picks"), type = "message", duration = 3)
  })
  
  output$upload_status <- renderUI({
    req(rv$uploaded)
    div(style = "margin-top:8px; font-family:var(--fm); font-size:11px; color:var(--text-3);",
        paste0(nrow(rv$uploaded), " picks \u00b7 ",
               length(unique(rv$uploaded$match_id)), " unique fixtures \u00b7 ",
               length(unique(rv$uploaded$market)), " markets"))
  })
  
  output$market_filter_ui <- renderUI({
    if (is.null(rv$uploaded)) {
      return(div(style = "font-family:var(--fm); font-size:11px; color:var(--text-3);",
                 "Upload a file to see available markets."))
    }
    mkts <- sort(unique(as.character(rv$uploaded$market)))
    selectizeInput("market_filter", NULL, choices = mkts, selected = mkts,
                   multiple = TRUE, width = "100%",
                   options = list(placeholder = "Leave empty = all markets",
                                  plugins = list("remove_button")))
  })
  
  observeEvent(input$commit_btn, {
    req(rv$uploaded)
    label <- trimws(input$slip_label %||% "")
    if (nchar(label) == 0) { showNotification("Label required", type = "warning"); return() }
    id <- format(Sys.time(), "%Y%m%d%H%M%S")
    df <- normalize_picks(rv$uploaded)
    if (is.null(df) || nrow(df) == 0) {
      showNotification("Nothing to commit", type = "error"); return()
    }
    # Apply market filter if user narrowed the dropdown
    sel <- input$market_filter
    if (!is.null(sel) && length(sel) > 0) {
      total_before <- nrow(df)
      df <- df[as.character(df$market) %in% sel, , drop = FALSE]
      if (nrow(df) == 0) {
        showNotification("Selected markets produced 0 picks \u2014 nothing to commit",
                         type = "error", duration = 5); return()
      }
      if (nrow(df) < total_before) {
        showNotification(paste0("Filtered to ", nrow(df), " of ", total_before,
                                " picks (", length(sel), " market",
                                if (length(sel) == 1) "" else "s", " selected)."),
                         type = "message", duration = 4)
      }
    }
    if (is.null(df$outcome)) df$outcome <- "PENDING"
    for (col in c("ft_home","ft_away","ht_home","ht_away")) if (is.null(df[[col]])) df[[col]] <- NA
    rv$history[[id]] <- list(id = id, label = label,
                             committed = format(Sys.time(), "%d %b %Y \u00b7 %H:%M"),
                             picks = df, results_cache = list())
    ok <- save_history(rv$history)
    if (!ok) {
      rv$history[[id]] <- NULL  # roll back in-memory so UI matches disk
      showNotification("Commit FAILED \u2014 could not write history file. Check the R console.",
                       type = "error", duration = 8)
      return()
    }
    rv$active_id <- id
    showNotification(paste0("Committed: ", label, " (", nrow(df), " picks saved)"),
                     type = "message", duration = 4)
  })
  
  output$fetch_info <- renderText({
    if (is.null(rv$active_id)) return("No active slip")
    slip <- rv$history[[rv$active_id]]
    picks <- to_df(slip$picks); if (is.null(picks)) return("")
    cache <- slip$results_cache %||% list()
    all_fx <- unique(picks$match_id)
    final_fx <- names(cache)[sapply(cache, function(r) (r$status %||% "") == "FINAL")]
    paste0(length(setdiff(all_fx, final_fx)), " fixtures to scrape (",
           length(final_fx), " already final)")
  })
  
  # Parse a pick's fixture_date + match_time into UTC POSIXct.
  # NB: tz="UTC" is explicit so we avoid system-tz arithmetic surprises.
  # Comparison is against Sys.time() which is also a unix instant, so UTC-vs-UTC math works.
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
    # Fallback: just the date (assume end-of-day so we don't miss late games)
    for (f in c("%Y-%m-%d", "%d.%m.%Y", "%d/%m/%Y", "%d-%m-%Y", "%d.%m.%y")) {
      p <- suppressWarnings(as.POSIXct(paste(date_str, "23:59"),
                                       format = paste(f, "%H:%M"), tz = "UTC"))
      if (!is.na(p)) return(p)
    }
    as.POSIXct(NA, tz = "UTC")
  }
  
  observeEvent(input$fetch_btn, {
    req(rv$active_id)
    slip <- rv$history[[rv$active_id]]
    picks <- to_df(slip$picks)
    if (is.null(picks) || nrow(picks) == 0) { showNotification("No picks", type = "error"); return() }
    cache <- slip$results_cache %||% list()
    
    now_ts <- Sys.time()
    grace_secs <- 2 * 3600   # match must be >= 2h past kickoff to be "played"
    if (is.null(picks$status)) picks$status <- "PENDING"
    if (is.null(picks$outcome)) picks$outcome <- "PENDING"
    
    fixtures_all <- picks %>%
      filter(!is.na(match_id), !is.na(match_url), nchar(as.character(match_url)) > 0) %>%
      distinct(match_id, match_url, fixture_date, match_time)
    
    if (nrow(fixtures_all) == 0) {
      showNotification("No usable fixtures (missing IDs/URLs)", type = "error"); return()
    }
    
    is_played   <- logical(nrow(fixtures_all))
    is_upcoming <- logical(nrow(fixtures_all))
    # Debug capture: first parsed kickoff for visibility
    first_debug <- NULL
    for (i in seq_len(nrow(fixtures_all))) {
      mid <- as.character(fixtures_all$match_id[i])
      r <- cache[[mid]]
      if (!is.null(r) && (r$status %||% "") == "FINAL") next     # already final, skip
      ko <- pick_kickoff(fixtures_all$fixture_date[i], fixtures_all$match_time[i])
      if (is.null(first_debug)) {
        first_debug <- list(
          date = fixtures_all$fixture_date[i],
          time = fixtures_all$match_time[i],
          parsed = ko,
          now = now_ts,
          delta_hours = round(as.numeric(difftime(now_ts, ko, units = "hours")), 2)
        )
      }
      if (is.na(ko)) { is_played[i] <- TRUE; next }              # unknown date -> try
      delta <- tryCatch(as.numeric(difftime(now_ts, ko, units = "secs")),
                        error = function(e) NA_real_)
      if (is.na(delta)) { is_played[i] <- TRUE; next }            # bad delta -> try
      if (delta >= grace_secs) is_played[i] <- TRUE
      else is_upcoming[i] <- TRUE
    }
    
    # Show debug for the first unprocessed fixture
    if (!is.null(first_debug)) {
      showNotification(
        paste0("DEBUG first fixture: date=", first_debug$date,
               " time=", first_debug$time,
               " | parsed UTC=", format(first_debug$parsed, "%Y-%m-%d %H:%M", tz = "UTC"),
               " | now=", format(first_debug$now, "%Y-%m-%d %H:%M %Z"),
               " | now-kickoff=", first_debug$delta_hours, "h"),
        type = "default", duration = 12)
    }
    
    n_upcoming <- sum(is_upcoming)
    n_already_final <- nrow(fixtures_all) - sum(is_played) - n_upcoming
    fixtures <- fixtures_all[is_played, , drop = FALSE]
    
    if (n_upcoming > 0) {
      showNotification(paste0(n_upcoming, " unplayed fixture",
                              if (n_upcoming == 1) "" else "s",
                              " skipped (kept as PENDING)."),
                       type = "message", duration = 5)
    }
    if (nrow(fixtures) == 0) {
      msg <- if (n_upcoming > 0) "Nothing to scrape yet \u2014 all remaining fixtures are upcoming."
      else "All fixtures already final."
      showNotification(msg, type = "message"); return()
    }
    
    work_queue <- lapply(seq_len(nrow(fixtures)), function(i) {
      mu <- as.character(fixtures$match_url[i])
      list(match_id = as.character(fixtures$match_id[i]),
           url = if (startsWith(mu, "http")) mu else paste0(BASE_URL, mu))
    })
    
    showNotification(paste0("Spawning ", min(N_PARALLEL_WORKERS, length(work_queue)),
                            " workers for ", length(work_queue), " played fixtures (",
                            n_upcoming, " upcoming, ", n_already_final, " already final)..."),
                     type = "message", duration = 4)
    tryCatch({
      results <- parallel_scrape_results(work_queue, WORKER_PROGRESS_DIR, N_PARALLEL_WORKERS)
      for (mid in names(results)) cache[[mid]] <- results[[mid]]
      # Ensure result columns exist before per-row assignment
      if (!"ft_home" %in% names(picks)) picks$ft_home <- NA_integer_
      if (!"ft_away" %in% names(picks)) picks$ft_away <- NA_integer_
      if (!"ht_home" %in% names(picks)) picks$ht_home <- NA_integer_
      if (!"ht_away" %in% names(picks)) picks$ht_away <- NA_integer_
      if (!"outcome" %in% names(picks)) picks$outcome <- NA_character_
      safe_int <- function(x) if (is.null(x) || length(x) == 0) NA_integer_ else as.integer(x[1])
      safe_chr <- function(x) if (is.null(x) || length(x) == 0) NA_character_ else as.character(x[1])
      for (i in seq_len(nrow(picks))) {
        mid <- as.character(picks$match_id[i]); res <- cache[[mid]]
        if (is.null(res) || (res$status %||% "") != "FINAL") next
        picks$ft_home[i] <- safe_int(res$ft_home); picks$ft_away[i] <- safe_int(res$ft_away)
        picks$ht_home[i] <- safe_int(res$ht_home); picks$ht_away[i] <- safe_int(res$ht_away)
        picks$outcome[i] <- safe_chr(grade_pick(picks$market[i], res))
      }
      rv$history[[rv$active_id]]$picks <- picks
      rv$history[[rv$active_id]]$results_cache <- cache
      save_history(rv$history)
      n_final <- sum(sapply(cache, function(r) (r$status %||% "") == "FINAL"))
      showNotification(paste0("Done. ", n_final, " of ", length(unique(picks$match_id)),
                              " fixtures final."), type = "message", duration = 4)
    }, error = function(e) showNotification(paste0("Error: ", e$message), type = "error", duration = 6))
  })
  
  output$active_summary <- renderUI({
    req(rv$active_id)
    picks <- to_df(rv$history[[rv$active_id]]$picks); req(picks)
    w <- sum(picks$outcome == "WIN", na.rm = TRUE); l <- sum(picks$outcome == "LOSS", na.rm = TRUE)
    p <- sum(picks$outcome == "PENDING", na.rm = TRUE)
    hr <- if ((w + l) > 0) paste0(round(w / (w + l) * 100, 1), "%") else "\u2014"
    div(class = "ks",
        div(class = "kc", div(class = "kn", w), div(class = "kl", "Wins")),
        div(class = "kc", div(class = "kn", l), div(class = "kl", "Losses")),
        div(class = "kc", div(class = "kn", p), div(class = "kl", "Pending")),
        div(class = "kc", div(class = "kn", hr), div(class = "kl", "Hit Rate")))
  })
  
  output$active_table_card <- renderUI({
    req(rv$active_id)
    div(class = "cd", div(class = "ct", "Picks & results"), DTOutput("active_table"))
  })
  
  output$active_table <- renderDT({
    req(rv$active_id)
    picks <- to_df(rv$history[[rv$active_id]]$picks); req(picks)
    ft <- ifelse(is.na(picks$ft_home) | is.na(picks$ft_away), "\u2014", paste0(picks$ft_home, ":", picks$ft_away))
    ht <- ifelse(is.na(picks$ht_home) | is.na(picks$ht_away), "\u2014", paste0(picks$ht_home, ":", picks$ht_away))
    badge <- function(o) {
      if (is.null(o) || length(o) == 0 || is.na(o) || nchar(o) == 0) o <- "PENDING"
      cls <- switch(o, WIN = "pl-ok", LOSS = "pl-err", "pl-pend")
      paste0('<span class="pl ', cls, '">', o, '</span>')
    }
    out_b <- sapply(picks$outcome %||% rep("PENDING", nrow(picks)), badge)
    # Columns that may not exist on older slips — fall back to em-dash
    col_or_dash <- function(name) {
      if (name %in% names(picks)) as.character(picks[[name]]) else rep("\u2014", nrow(picks))
    }
    d <- data.frame(
      Market     = picks$market,
      Country    = col_or_dash("country"),
      League     = col_or_dash("league"),
      Date       = col_or_dash("fixture_date"),
      Time       = col_or_dash("match_time"),
      Home       = picks$home,
      Away       = picks$away,
      Pick       = picks$pick,
      Confidence = paste0(picks$confidence, "%"),
      HT         = ht,
      FT         = ft,
      Outcome    = out_b,
      stringsAsFactors = FALSE
    )
    datatable(d, rownames = FALSE, selection = "none", escape = FALSE,
              options = list(pageLength = 30, dom = "ftp", scrollX = TRUE))
  }, server = FALSE)
  
  output$export_card <- renderUI({
    req(rv$active_id)
    div(class = "cd", div(class = "ct", "Export"),
        downloadButton("dl_xlsx", "Download graded results (XLSX)", class = "bo"))
  })
  
  output$dl_xlsx <- downloadHandler(
    filename = function() paste0("moab_results_", gsub("[^A-Za-z0-9]", "_", rv$history[[rv$active_id]]$label), ".xlsx"),
    content = function(file) {
      df <- to_df(rv$history[[rv$active_id]]$picks)
      wb <- createWorkbook(); addWorksheet(wb, "Results", gridLines = FALSE)
      hs <- createStyle(fontName = "Calibri", fontSize = 11, fontColour = "#FFFFFF",
                        fgFill = "#1E3A8A", halign = "CENTER", textDecoration = "BOLD")
      writeData(wb, "Results", df, startRow = 1, startCol = 1)
      addStyle(wb, "Results", hs, rows = 1, cols = seq_len(ncol(df)), gridExpand = TRUE)
      setColWidths(wb, "Results", cols = seq_len(ncol(df)), widths = "auto")
      freezePane(wb, "Results", firstRow = TRUE)
      saveWorkbook(wb, file, overwrite = TRUE)
    })
  
  output$history_content <- renderUI({
    h <- rv$history
    if (length(h) == 0) return(div(class = "cd", style = "text-align:center; padding:60px 20px;",
                                   div(style = "font-family:var(--fh); font-size:22px; color:var(--text-3);", "No slips yet")))
    ord <- names(h)[order(sapply(h, function(x) x$id), decreasing = TRUE)]
    rows <- lapply(ord, function(id) {
      e <- h[[id]]; picks <- to_df(e$picks)
      legs <- if (!is.null(picks)) nrow(picks) else 0
      w <- if (!is.null(picks)) sum(picks$outcome == "WIN", na.rm = TRUE) else 0
      l <- if (!is.null(picks)) sum(picks$outcome == "LOSS", na.rm = TRUE) else 0
      p <- if (!is.null(picks)) sum(picks$outcome == "PENDING", na.rm = TRUE) else 0
      hr <- if ((w + l) > 0) paste0(round(w / (w + l) * 100, 1), "%") else "\u2014"
      tags$tr(
        tags$td(style = "padding:12px 14px; font-family:var(--fh); font-weight:700;", e$label),
        tags$td(style = "padding:12px 14px; font-family:var(--fm); font-size:10px; color:var(--text-3);", e$committed),
        tags$td(style = "padding:12px 14px; text-align:center;", legs),
        tags$td(style = "padding:12px 14px; text-align:center; color:var(--success); font-weight:700;", w),
        tags$td(style = "padding:12px 14px; text-align:center; color:var(--danger); font-weight:700;", l),
        tags$td(style = "padding:12px 14px; text-align:center;", p),
        tags$td(style = "padding:12px 14px; text-align:center; font-family:var(--fh); font-weight:800;", hr),
        tags$td(style = "padding:12px 14px; white-space:nowrap;",
                actionButton(paste0("activate_", id), "Activate", class = "bo",
                             style = "padding:5px 12px!important; font-size:10px!important; margin-right:4px;"),
                actionButton(paste0("rename_", id), "Rename", class = "bo",
                             style = "padding:5px 12px!important; font-size:10px!important; margin-right:4px;"),
                actionButton(paste0("delete_", id), "Delete", class = "bo",
                             style = "padding:5px 12px!important; font-size:10px!important; color:var(--danger)!important;")))
    })
    div(class = "cd", div(class = "ct", "All slips"),
        tags$table(style = "width:100%; border-collapse:separate;",
                   tags$thead(tags$tr(
                     tags$th(style="text-align:left;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);","Label"),
                     tags$th(style="text-align:left;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);","Committed"),
                     tags$th(style="text-align:center;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);","Legs"),
                     tags$th(style="text-align:center;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);","W"),
                     tags$th(style="text-align:center;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);","L"),
                     tags$th(style="text-align:center;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);","P"),
                     tags$th(style="text-align:center;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);","Rate"),
                     tags$th(style="text-align:left;padding:10px 14px;font-size:10px;letter-spacing:2px;text-transform:uppercase;color:var(--text-3);",""))),
                   tags$tbody(do.call(tagList, rows))))
  })
  
  observe({
    h <- rv$history
    lapply(names(h), function(id) {
      observeEvent(input[[paste0("activate_", id)]], {
        rv$active_id <- id; updateTabsetPanel(session, "tabs", selected = "active")
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("rename_", id)]], {
        showModal(modalDialog(title = "Rename",
                              textInput(paste0("new_label_", id), "New label", value = h[[id]]$label, width = "100%"),
                              footer = tagList(modalButton("Cancel"),
                                               actionButton(paste0("rename_confirm_", id), "Save", class = "bd")), easyClose = TRUE))
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("rename_confirm_", id)]], {
        nl <- trimws(input[[paste0("new_label_", id)]] %||% "")
        if (nchar(nl) > 0) { rv$history[[id]]$label <- nl; save_history(rv$history) }
        removeModal()
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("delete_", id)]], {
        showModal(modalDialog(title = "Delete?",
                              HTML(paste0("<p>Permanently delete <strong>", h[[id]]$label, "</strong>?</p>")),
                              footer = tagList(modalButton("Cancel"),
                                               actionButton(paste0("delete_confirm_", id), "Delete", class = "bd")), easyClose = TRUE))
      }, ignoreInit = TRUE)
      observeEvent(input[[paste0("delete_confirm_", id)]], {
        rv$history[[id]] <- NULL; save_history(rv$history)
        if (!is.null(rv$active_id) && rv$active_id == id) rv$active_id <- NULL
        removeModal()
      }, ignoreInit = TRUE)
    })
  })
  
  output$analytics_grid <- renderUI({
    h <- rv$history
    if (length(h) == 0) return(div(class = "cd", style = "text-align:center; padding:60px;",
                                   div(style = "font-family:var(--fh); font-size:18px; color:var(--text-3);", "No slips to analyse")))
    all_markets <- unique(unlist(lapply(h, function(s) { p <- to_df(s$picks); if (is.null(p)) character() else unique(p$market) })))
    if (length(all_markets) == 0) return(div(class = "cd", "No graded picks yet"))
    color_cell <- function(w, l) {
      if ((w + l) == 0) return('<td class="grid-cell grid-empty">\u2014</td>')
      hr <- w / (w + l) * 100
      cls <- if (hr == 100) "grid-100" else if (hr >= 90) "grid-90" else if (hr >= 75) "grid-75" else "grid-low"
      sprintf('<td class="grid-cell %s">%s%%<br>%d/%d</td>', cls, round(hr, 0), w, w + l)
    }
    ord <- names(h)[order(sapply(h, function(x) x$id))]
    body_rows <- lapply(ord, function(id) {
      e <- h[[id]]; picks <- to_df(e$picks); if (is.null(picks)) return(NULL)
      cells <- ""; tw <- 0; tl <- 0
      for (mkt in all_markets) {
        sub <- picks[picks$market == mkt, , drop = FALSE]
        w <- sum(sub$outcome == "WIN", na.rm = TRUE); l <- sum(sub$outcome == "LOSS", na.rm = TRUE)
        cells <- paste0(cells, color_cell(w, l)); tw <- tw + w; tl <- tl + l
      }
      ov <- if ((tw + tl) == 0) '<td class="grid-cell grid-empty grid-overall">\u2014</td>' else {
        hr <- tw / (tw + tl) * 100
        cls <- if (hr == 100) "grid-100" else if (hr >= 90) "grid-90" else if (hr >= 75) "grid-75" else "grid-low"
        sprintf('<td class="grid-cell %s grid-overall">%s%%<br>%d/%d</td>', cls, round(hr, 0), tw, tw + tl)
      }
      paste0('<tr><td style="padding:10px 12px; font-weight:600;">', e$label, '</td>', cells, ov, '</tr>')
    })
    cum_cells <- ""; cw <- 0; cl <- 0
    for (mkt in all_markets) {
      w <- 0; l <- 0
      for (id in names(h)) { p <- to_df(h[[id]]$picks); if (is.null(p)) next
      sub <- p[p$market == mkt, , drop = FALSE]
      w <- w + sum(sub$outcome == "WIN", na.rm = TRUE); l <- l + sum(sub$outcome == "LOSS", na.rm = TRUE) }
      cum_cells <- paste0(cum_cells, color_cell(w, l)); cw <- cw + w; cl <- cl + l
    }
    cum_ov <- if ((cw + cl) == 0) '<td class="grid-cell grid-empty grid-overall">\u2014</td>' else {
      hr <- cw / (cw + cl) * 100
      cls <- if (hr == 100) "grid-100" else if (hr >= 90) "grid-90" else if (hr >= 75) "grid-75" else "grid-low"
      sprintf('<td class="grid-cell %s grid-overall">%s%%<br>%d/%d</td>', cls, round(hr, 0), cw, cw + cl)
    }
    header <- paste0('<th style="padding:10px 12px; background:var(--ivory-2);">Slip</th>',
                     paste(sprintf('<th style="padding:10px 12px; background:var(--ivory-2); font-size:9px;">%s</th>', all_markets), collapse = ""),
                     '<th style="padding:10px 12px; background:#e8f5f0; font-size:9px;">OVERALL</th>')
    div(class = "cd", div(class = "ct", "Analytics grid"),
        HTML(paste0('<table style="width:100%; border-collapse:collapse; font-size:11px;">',
                    '<thead><tr>', header, '</tr></thead><tbody>',
                    paste(unlist(body_rows), collapse = ""),
                    '<tr style="border-top:2px solid var(--border); background:var(--ivory-2);">',
                    '<td style="padding:10px 12px; font-weight:700; font-family:var(--fh);">CUMULATIVE</td>',
                    cum_cells, cum_ov, '</tr></tbody></table>')))
  })
}

shinyApp(ui = ui, server = server)
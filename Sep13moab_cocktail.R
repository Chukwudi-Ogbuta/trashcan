# ============================================================
# moab_cocktail.R
# Reads MOAB predictions Excel, combines selected market sheets,
# dedupes by fixture keeping strongest market.
# Also reads sim history to recommend safe flex level.
# ============================================================

library(shiny)
library(DT)
library(openxlsx)
library(readxl)
library(jsonlite)

BASE_PATH        <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
SIM_HISTORY_PATH <- file.path(BASE_PATH, "moab_sim_history.json")

# Sheet name -> strength (higher = kept when dupes)
DEFAULT_MARKETS <- list(
  "drawupto10" = list(strength = 4, selected = TRUE),
  "dc1x"       = list(strength = 3, selected = TRUE),
  "over_15_ft" = list(strength = 2, selected = TRUE),
  "1hover05"   = list(strength = 1, selected = TRUE),
  "dcx2"       = list(strength = 0, selected = FALSE),
  "dc12"       = list(strength = 0, selected = FALSE),
  "drawupto15" = list(strength = 0, selected = FALSE)
)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0) b else a

# Load sim history and compute per-market stats
load_sim_stats <- function(selected_markets) {
  if (!file.exists(SIM_HISTORY_PATH)) return(NULL)
  h <- tryCatch(fromJSON(SIM_HISTORY_PATH, simplifyDataFrame = FALSE),
                error = function(e) NULL)
  if (is.null(h) || length(h) == 0) return(NULL)
  h <- h[order(sapply(h, function(x) x$id))]
  
  stats <- list()
  for (mkt in selected_markets) {
    rows <- list()
    for (run in h) {
      picks <- run$results[[mkt]]
      if (is.null(picks) || length(picks) == 0) next
      outcomes <- vapply(picks, function(p) p$outcome %||% "PENDING", character(1))
      w <- sum(outcomes == "WIN", na.rm = TRUE)
      l <- sum(outcomes == "LOSS", na.rm = TRUE)
      p_total <- length(outcomes)
      rows[[length(rows) + 1]] <- data.frame(
        label = run$label, picks = p_total, wins = w, losses = l,
        hit_rate = if ((w + l) > 0) w / (w + l) else NA,
        stringsAsFactors = FALSE)
    }
    if (length(rows) > 0) stats[[mkt]] <- do.call(rbind, rows)
  }
  stats
}

# Build recommendation text
build_recommendation <- function(stats, current_cocktail = NULL) {
  # Normalize market names to raw keys for consistent lookup
  normalize_market <- function(m) {
    switch(as.character(m),
           "Over 1.5"          = "over_15_ft",
           "1H Over 0.5"       = "1hover05",
           "Double Chance 1X"  = "dc1x",
           "Double Chance 12"  = "dc12",
           "Double Chance X2"  = "dcx2",
           "Draw up to min 10" = "drawupto10",
           "Draw up to min 15" = "drawupto15",
           as.character(m))
  }
  if (!is.null(current_cocktail) && "market" %in% names(current_cocktail)) {
    current_cocktail$market <- vapply(current_cocktail$market, normalize_market, character(1))
  }
  if (is.null(stats) || length(stats) == 0)
    return("No historical data yet. Commit sim runs to build recommendations.")
  
  per_market_lines <- c()
  total_expected_losses <- 0
  trend_alerts <- c()
  
  curr_by_mkt <- if (!is.null(current_cocktail)) table(current_cocktail$market) else NULL
  
  for (mkt in names(stats)) {
    s <- stats[[mkt]]
    n_rounds <- nrow(s)
    hist_loss_rate <- mean(s$losses / s$picks, na.rm = TRUE)  # loss rate per pick
    hist_avg_picks <- mean(s$picks, na.rm = TRUE)
    hist_avg_losses <- mean(s$losses, na.rm = TRUE)
    
    curr_val <- if (!is.null(curr_by_mkt)) curr_by_mkt[mkt] else NA
    curr_n <- if (is.na(curr_val)) 0L else as.integer(curr_val)
    
    expected_curr_losses <- curr_n * hist_loss_rate
    total_expected_losses <- total_expected_losses + expected_curr_losses
    
    # Per-market safety needed = ceiling of expected losses
    per_mkt_flex <- ceiling(expected_curr_losses)
    
    # Trend
    trend <- ""
    if (n_rounds >= 2) {
      last_hr <- s$hit_rate[n_rounds]; prev_hr <- s$hit_rate[n_rounds - 1]
      if (!is.na(last_hr) && !is.na(prev_hr) && last_hr < prev_hr - 0.02) {
        trend_alerts <- c(trend_alerts, mkt)
      }
    }
    
    volume_note <- ""
    if (curr_n > hist_avg_picks * 1.4) volume_note <- " ⚠ higher volume than usual"
    if (curr_n < hist_avg_picks * 0.6 && curr_n > 0) volume_note <- " ⚠ lower volume than usual"
    if (curr_n == 0) volume_note <- " (no picks today)"
    
    # Build loss history string like "R1: 0L, R2: 1L"
    loss_history <- paste0(sprintf("R%d: %dL", seq_len(nrow(s)), s$losses), collapse = ", ")
    
    per_market_lines <- c(per_market_lines, sprintf(
      "  %s: %d picks today (avg %d)\n    History: %s\n    Loss rate: %.1f%% per pick → expected %.1f losses today → needs %d flex%s\n",
      mkt, curr_n, round(hist_avg_picks),
      loss_history, hist_loss_rate * 100,
      expected_curr_losses, per_mkt_flex, volume_note))
  }
  
  # Combined flex = ceiling of expected losses × 1.3 safety multiplier
  combined_flex <- ceiling(total_expected_losses * 1.3)
  if (combined_flex < 1) combined_flex <- 1
  
  out <- paste0("═══════════════════════════════════════\n",
                "COMBINED FLEX RECOMMENDATION: ", combined_flex, "\n",
                "═══════════════════════════════════════\n\n",
                "Based on ", nrow(stats[[1]]), " committed round(s), expected losses today: ",
                round(total_expected_losses, 1), "\n\n",
                "PER-MARKET BREAKDOWN:\n",
                paste(per_market_lines, collapse = "\n"))
  
  if (length(trend_alerts) > 0) {
    out <- paste0(out, "\n\nTREND ALERTS (losing more in recent rounds):\n  ",
                  paste(trend_alerts, collapse = ", "))
  }
  
  out
}

ui <- fluidPage(
  titlePanel("MOAB Cocktail"),
  sidebarLayout(
    sidebarPanel(
      fileInput("xlsx_upload", "Upload MOAB predictions Excel (.xlsx)",
                accept = c(".xlsx"), width = "100%"),
      hr(),
      h4("Markets to combine"),
      p(em("Tick markets. Higher strength = kept when a fixture appears in multiple.")),
      uiOutput("market_checkboxes"),
      hr(),
      actionButton("run_btn", "Run cocktail", class = "btn-primary", style = "width:100%"),
      br(), br(),
      downloadButton("dl_excel", "Download Excel", style = "width:100%"),
      hr(),
      verbatimTextOutput("status")
    ),
    mainPanel(
      h4("Recommendation"),
      verbatimTextOutput("recommendation"),
      hr(),
      h4("Cocktail picks"),
      verbatimTextOutput("summary"),
      DTOutput("picks_table")
    )
  )
)

server <- function(input, output, session) {
  rv <- reactiveValues(
    sheets   = NULL,
    cocktail = NULL,
    status   = "Upload predictions Excel to begin.",
    recommendation = "Load Excel and click Run to see recommendation."
  )
  
  output$market_checkboxes <- renderUI({
    lapply(names(DEFAULT_MARKETS), function(nm) {
      m <- DEFAULT_MARKETS[[nm]]
      checkboxInput(paste0("mkt_", gsub("[^A-Za-z0-9]", "_", nm)),
                    paste0(nm, " (rank ", m$strength, ")"),
                    value = m$selected)
    })
  })
  
  # Show recommendation on load based on currently ticked markets
  observe({
    selected <- names(DEFAULT_MARKETS)[
      vapply(names(DEFAULT_MARKETS),
             function(nm) isTRUE(input[[paste0("mkt_", gsub("[^A-Za-z0-9]", "_", nm))]]),
             logical(1))
    ]
    if (length(selected) > 0) {
      stats <- load_sim_stats(selected)
      rv$recommendation <- build_recommendation(stats, rv$cocktail)
    }
  })
  
  observeEvent(input$xlsx_upload, {
    req(input$xlsx_upload)
    sheet_names <- tryCatch(excel_sheets(input$xlsx_upload$datapath), error = function(e) NULL)
    if (is.null(sheet_names) || length(sheet_names) == 0) {
      rv$status <- "Could not read Excel"
      showNotification("Failed to load", type = "error"); return()
    }
    sheets <- list()
    for (sn in sheet_names) {
      df <- tryCatch(read_excel(input$xlsx_upload$datapath, sheet = sn),
                     error = function(e) NULL)
      if (!is.null(df) && nrow(df) > 0) sheets[[sn]] <- as.data.frame(df)
    }
    rv$sheets <- sheets
    rv$status <- paste0("Loaded ", length(sheets), " sheets: ",
                        paste(names(sheets), collapse = ", "))
  })
  
  observeEvent(input$run_btn, {
    if (is.null(rv$sheets)) {
      showNotification("Upload Excel first", type = "warning"); return()
    }
    selected_names <- names(DEFAULT_MARKETS)[
      vapply(names(DEFAULT_MARKETS),
             function(nm) isTRUE(input[[paste0("mkt_", gsub("[^A-Za-z0-9]", "_", nm))]]),
             logical(1))
    ]
    if (length(selected_names) == 0) {
      showNotification("Select at least one market", type = "warning"); return()
    }
    
    all_picks <- do.call(rbind, lapply(selected_names, function(nm) {
      df <- rv$sheets[[nm]]
      if (is.null(df) || nrow(df) == 0) return(NULL)
      df$strength <- DEFAULT_MARKETS[[nm]]$strength
      df
    }))
    
    if (is.null(all_picks) || nrow(all_picks) == 0) {
      rv$cocktail <- NULL
      rv$status <- "No picks in selected markets found in the Excel"
      return()
    }
    
    key_cols <- intersect(c("home", "away", "fixture_date"), names(all_picks))
    if (length(key_cols) < 2) {
      rv$status <- "Predictions Excel missing home/away/date columns — cannot dedup"
      showNotification("Missing key columns", type = "error"); return()
    }
    all_picks$fixture_key <- do.call(paste, c(all_picks[, key_cols], sep = "|"))
    
    all_picks <- all_picks[order(-all_picks$strength), ]
    cocktail <- all_picks[!duplicated(all_picks$fixture_key), ]
    if ("fixture_date" %in% names(cocktail))
      cocktail <- cocktail[order(cocktail$fixture_date), ]
    
    cocktail$strength <- NULL
    cocktail$fixture_key <- NULL
    
    rv$cocktail <- cocktail
    rv$status <- paste0("Cocktail: ", nrow(cocktail), " picks from ",
                        length(selected_names), " markets")
    
    # Refresh recommendation with volume comparison
    stats <- load_sim_stats(selected_names)
    rv$recommendation <- build_recommendation(stats, cocktail)
  })
  
  output$status <- renderText({ rv$status })
  output$recommendation <- renderText({ rv$recommendation })
  
  output$summary <- renderText({
    if (is.null(rv$cocktail)) return("Run cocktail to see picks.")
    tbl <- if ("market" %in% names(rv$cocktail)) table(rv$cocktail$market) else "(no market col)"
    paste0("Total picks: ", nrow(rv$cocktail), "\n\n",
           "By market:\n",
           if (is.table(tbl)) paste0("  ", names(tbl), ": ", tbl, collapse = "\n") else tbl)
  })
  
  output$picks_table <- renderDT({
    if (is.null(rv$cocktail)) return(NULL)
    datatable(rv$cocktail, rownames = FALSE, filter = "top",
              options = list(pageLength = 25, scrollX = TRUE))
  })
  
  output$dl_excel <- downloadHandler(
    filename = function() paste0("MOAB_Cocktail_", format(Sys.Date(), "%Y%m%d"),
                                 "_", format(Sys.time(), "%H%M"), ".xlsx"),
    content = function(file) {
      if (is.null(rv$cocktail)) return()
      wb <- createWorkbook()
      addWorksheet(wb, "Cocktail")
      writeData(wb, "Cocktail", rv$cocktail)
      freezePane(wb, "Cocktail", firstActiveRow = 2)
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
}

shinyApp(ui = ui, server = server)
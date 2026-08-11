# ============================================================
# app.R
# MOAB Grid Searcher — Shiny application
# Session 1 scope: file loading, Stage 1 grid search, BTTS market,
# dual leaderboard, in-app market editor.
# ============================================================

library(shiny)
library(DT)

# ─── Paths (adjust for local install) ────────────────────────
APP_DIR <- normalizePath(".", mustWork = FALSE)
ENGINE_DIR    <- file.path(APP_DIR, "engine")
MARKETS_DIR   <- file.path(APP_DIR, "markets")
BACKUPS_DIR   <- file.path(MARKETS_DIR, "backups")
SESSIONS_DIR  <- file.path(APP_DIR, "sessions")

# Ensure directories exist
for (d in c(MARKETS_DIR, BACKUPS_DIR, SESSIONS_DIR)) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ─── Source engine files ─────────────────────────────────────
source(file.path(ENGINE_DIR, "loading.R"))
source(file.path(ENGINE_DIR, "features.R"))
source(file.path(ENGINE_DIR, "grid_search.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Load all market files (with tryCatch safety) ────────────
load_all_markets <- function() {
  files <- list.files(MARKETS_DIR, pattern = "\\.R$", full.names = TRUE)
  markets <- list()
  errors <- list()
  for (f in files) {
    result <- tryCatch({
      env <- new.env()
      sys.source(f, envir = env)
      # Find the market_* variable in that env
      obj_names <- ls(env)
      market_var <- obj_names[grepl("^market_", obj_names)]
      if (length(market_var) == 0) return(NULL)
      m <- get(market_var[1], envir = env)
      m$source_file <- basename(f)
      m
    }, error = function(e) {
      errors[[basename(f)]] <<- e$message
      NULL
    })
    if (!is.null(result)) markets[[result$name]] <- result
  }
  list(markets = markets, errors = errors)
}

# ─── UI ──────────────────────────────────────────────────────
ui <- fluidPage(
  titlePanel("MOAB Grid Searcher"),
  tabsetPanel(
    # ══════════════════════════════════════════════════════════
    # TAB 1 — DATA (file loading)
    # ══════════════════════════════════════════════════════════
    tabPanel("Data",
      br(),
      fluidRow(
        column(4,
          h4("Load mode"),
          radioButtons("load_mode", NULL,
            choices = c("Single file" = "single", "Multi-file (with roles)" = "multi"),
            selected = "single"
          ),
          hr(),
          h4("Add file(s)"),
          fileInput("upload_files", "Upload sim-ready RDS file(s)",
                    accept = c(".rds"), multiple = TRUE),
          conditionalPanel(
            condition = "input.load_mode == 'multi'",
            actionButton("auto_tag_btn", "Auto-tag roles (newest→Test)"),
            br(), br(),
            textInput("session_name_save", "Save session as:", value = ""),
            actionButton("save_session_btn", "Save session"),
            br(), br(),
            selectInput("session_load_choice", "Load session:", choices = c(""), selected = ""),
            actionButton("load_session_btn", "Load session")
          )
        ),
        column(8,
          h4("Loaded files"),
          DTOutput("files_table"),
          br(),
          verbatimTextOutput("load_summary")
        )
      )
    ),
    # ══════════════════════════════════════════════════════════
    # TAB 2 — GRID SEARCH
    # ══════════════════════════════════════════════════════════
    tabPanel("Grid Search",
      br(),
      fluidRow(
        column(3,
          h4("Market"),
          selectInput("market_pick", NULL, choices = c(""), selected = ""),
          verbatimTextOutput("market_desc"),
          hr(),
          h4("Options"),
          checkboxInput("use_main_only", "Filter to main-league only", value = FALSE),
          numericInput("min_picks", "Min picks (leaderboard floor):", value = 30, min = 1),
          radioButtons("sort_by", "Sort by:",
            choices = c("Hit rate" = "hit_rate",
                        "Volume"   = "volume",
                        "Baseline lift" = "baseline_lift"),
            selected = "hit_rate"
          ),
          checkboxInput("dedup_lb", "Dedup identical rules", value = TRUE),
          hr(),
          actionButton("run_search_btn", "Run Grid Search",
                       class = "btn-primary", style = "width: 100%")
        ),
        column(9,
          h4("Results"),
          verbatimTextOutput("search_summary"),
          tabsetPanel(
            tabPanel("Picked (main leaderboard)",
              br(),
              DTOutput("picked_leaderboard")
            ),
            tabPanel("Rejected (missed opportunities)",
              br(),
              p(em("Rules whose rejected pile has high win rate = rules leaving winners on the table. Candidates for rescue rules.")),
              DTOutput("rejected_leaderboard")
            ),
            tabPanel("Baseline",
              br(),
              verbatimTextOutput("baseline_info")
            )
          )
        )
      )
    ),
    # ══════════════════════════════════════════════════════════
    # TAB 3 — MARKETS (in-app editor)
    # ══════════════════════════════════════════════════════════
    tabPanel("Markets",
      br(),
      fluidRow(
        column(3,
          h4("Market files"),
          selectInput("market_edit_pick", NULL, choices = c(""), selected = ""),
          actionButton("new_market_btn", "New"),
          actionButton("duplicate_market_btn", "Duplicate"),
          actionButton("reload_markets_btn", "Reload all"),
          hr(),
          h5("Available features"),
          verbatimTextOutput("feature_list")
        ),
        column(9,
          h4("Editor"),
          p(em("Edit the R code below. Click Validate & Save to write to disk. A timestamped backup is created before overwrite.")),
          textInput("edit_filename", "Filename (.R):", value = ""),
          tags$textarea(id = "market_code", rows = 30, style = "width:100%; font-family: monospace; font-size: 12px;", ""),
          br(), br(),
          actionButton("validate_market_btn", "Validate syntax"),
          actionButton("save_market_btn", "Validate & Save", class = "btn-success"),
          actionButton("delete_market_btn", "Delete file", class = "btn-danger"),
          br(), br(),
          verbatimTextOutput("market_edit_status")
        )
      )
    )
  )
)

# ─── Server ──────────────────────────────────────────────────
server <- function(input, output, session) {
  # ── Reactive state ─────────────────────────────────────────
  rv <- reactiveValues(
    files_df = NULL,          # data.frame of loaded files (multi-mode)
    fixtures_pool = NULL,     # loaded fixtures (single-mode: all; multi-mode: role-split lists)
    load_mode = "single",
    markets = list(),
    market_errors = list(),
    last_search = NULL,       # result of run_grid_search
    market_edit_status = ""
  )
  
  # ── Refresh markets on startup ─────────────────────────────
  refresh_markets <- function() {
    res <- load_all_markets()
    rv$markets <- res$markets
    rv$market_errors <- res$errors
    updateSelectInput(session, "market_pick",
      choices = c("", names(rv$markets)), selected = "")
    updateSelectInput(session, "market_edit_pick",
      choices = c("", basename(list.files(MARKETS_DIR, pattern = "\\.R$"))), selected = "")
    # Session load choices
    updateSelectInput(session, "session_load_choice",
      choices = c("", list_sessions(SESSIONS_DIR)), selected = "")
  }
  
  observe({ refresh_markets() }, priority = 1000)
  
  observeEvent(input$reload_markets_btn, {
    refresh_markets()
    showNotification(paste0("Reloaded. Markets: ", length(rv$markets),
                            "; Errors: ", length(rv$market_errors)),
                     type = "message")
  })
  
  # ── Load mode ──────────────────────────────────────────────
  observeEvent(input$load_mode, { rv$load_mode <- input$load_mode })
  
  # ── File upload handler ────────────────────────────────────
  observeEvent(input$upload_files, {
    req(input$upload_files)
    metas <- list()
    for (i in seq_len(nrow(input$upload_files))) {
      path <- input$upload_files$datapath[i]
      fname <- input$upload_files$name[i]
      # Copy to a stable location (Shiny uploads use temp paths)
      stable_path <- file.path(APP_DIR, "uploads", fname)
      if (!dir.exists(dirname(stable_path))) dir.create(dirname(stable_path), recursive = TRUE)
      file.copy(path, stable_path, overwrite = TRUE)
      res <- load_sim_ready_file(stable_path)
      if (res$ok) {
        m <- res$meta
        m$filename <- fname
        m$fullpath <- stable_path
        m$role <- "Tune"  # default
        metas[[length(metas) + 1]] <- m
      }
    }
    if (length(metas) == 0) {
      showNotification("No valid RDS files loaded", type = "error")
      return()
    }
    new_df <- do.call(rbind, metas)
    if (is.null(rv$files_df)) {
      rv$files_df <- new_df
    } else {
      # Merge; drop duplicates by filename
      combined <- rbind(rv$files_df, new_df)
      combined <- combined[!duplicated(combined$filename), , drop = FALSE]
      rv$files_df <- combined
    }
    showNotification(paste0("Loaded ", length(metas), " file(s)"), type = "message")
  })
  
  # ── Auto-tag roles ─────────────────────────────────────────
  observeEvent(input$auto_tag_btn, {
    if (is.null(rv$files_df) || nrow(rv$files_df) == 0) return()
    rv$files_df <- auto_tag_roles(rv$files_df)
    showNotification("Roles auto-tagged", type = "message")
  })
  
  # ── Files table (editable roles in multi mode) ─────────────
  output$files_table <- renderDT({
    if (is.null(rv$files_df) || nrow(rv$files_df) == 0) return(NULL)
    show_cols <- c("filename", "round_label", "n_fixtures", "n_final",
                   "date_min", "date_max", "role")
    df <- rv$files_df[, show_cols, drop = FALSE]
    datatable(df,
              options = list(pageLength = 10, dom = "t"),
              editable = if (rv$load_mode == "multi") list(target = "column", columns = 7) else FALSE,
              rownames = FALSE)
  })
  
  # Handle role edits in DT
  observeEvent(input$files_table_cell_edit, {
    info <- input$files_table_cell_edit
    if (rv$load_mode != "multi") return()
    # info$col is 0-indexed; role is at position 6 in show_cols → col index 6
    if (info$col == 6) {
      new_val <- as.character(info$value)
      if (!(new_val %in% c("Tune","Validate","Test","Exclude"))) {
        showNotification("Role must be one of Tune / Validate / Test / Exclude", type = "error")
        return()
      }
      rv$files_df$role[info$row] <- new_val
    }
  })
  
  # ── Session save/load ──────────────────────────────────────
  observeEvent(input$save_session_btn, {
    nm <- trimws(input$session_name_save)
    if (nm == "" || is.null(rv$files_df)) {
      showNotification("Enter a session name and load files first", type = "error")
      return()
    }
    save_session(rv$files_df, nm, SESSIONS_DIR)
    updateSelectInput(session, "session_load_choice",
      choices = c("", list_sessions(SESSIONS_DIR)), selected = nm)
    showNotification(paste0("Saved session: ", nm), type = "message")
  })
  
  observeEvent(input$load_session_btn, {
    nm <- input$session_load_choice
    if (nm == "") return()
    loaded <- load_session(nm, SESSIONS_DIR)
    if (is.null(loaded)) {
      showNotification("Session not found", type = "error")
      return()
    }
    rv$files_df <- loaded
    showNotification(paste0("Loaded session: ", nm), type = "message")
  })
  
  # ── Load summary text ──────────────────────────────────────
  output$load_summary <- renderText({
    if (is.null(rv$files_df) || nrow(rv$files_df) == 0) {
      return("No files loaded.")
    }
    lines <- c(
      paste0("Files loaded: ", nrow(rv$files_df)),
      paste0("Total fixtures: ", sum(rv$files_df$n_fixtures)),
      paste0("Total FINAL: ", sum(rv$files_df$n_final))
    )
    if (rv$load_mode == "multi") {
      role_counts <- table(rv$files_df$role)
      lines <- c(lines, "", "By role:")
      for (r in names(role_counts)) lines <- c(lines, paste0("  ", r, ": ", role_counts[r]))
    }
    paste(lines, collapse = "\n")
  })
  
  # ── Build fixtures pool for grid search ────────────────────
  build_fixtures_pool <- reactive({
    if (is.null(rv$files_df) || nrow(rv$files_df) == 0) return(NULL)
    if (rv$load_mode == "single") {
      # Use the first (and only expected) file
      res <- load_sim_ready_file(rv$files_df$fullpath[1])
      if (!res$ok) return(NULL)
      list(tune = filter_final_only(res$fixtures),
           validate = list(), test = list())
    } else {
      out <- load_multi_files(rv$files_df)
      list(tune = filter_final_only(out$tune),
           validate = filter_final_only(out$validate),
           test = filter_final_only(out$test))
    }
  })
  
  # ── Market picker description ──────────────────────────────
  output$market_desc <- renderText({
    m <- rv$markets[[input$market_pick]]
    if (is.null(m)) return("Pick a market to see details.")
    paste(
      paste0("Name: ", m$name),
      paste0("Tier: ", m$tier),
      paste0("Features: ", paste(names(m$threshold_grid), collapse = ", ")),
      paste0("Combos: ", prod(sapply(m$threshold_grid, length))),
      paste0("Notes: ", m$notes %||% ""),
      sep = "\n"
    )
  })
  
  # ── Run grid search ────────────────────────────────────────
  observeEvent(input$run_search_btn, {
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) {
      showNotification("Load at least one file with FINAL fixtures first", type = "error")
      return()
    }
    market <- rv$markets[[input$market_pick]]
    if (is.null(market)) {
      showNotification("Pick a market first", type = "error")
      return()
    }
    withProgress(message = "Running grid search", value = 0.1, {
      # Build feature matrix on the Tune pool
      feature_names <- names(market$threshold_grid)
      incProgress(0.2, detail = "Building features...")
      feat_df <- build_feature_matrix(
        pool$tune,
        feature_names = feature_names,
        window = market$window %||% 5,
        main_only = input$use_main_only,
        min_required = market$min_required
      )
      incProgress(0.4, detail = "Sweeping thresholds...")
      res <- run_grid_search(
        feat_df,
        grade_fn = market$grade_fn,
        threshold_grid = market$threshold_grid,
        min_picks = input$min_picks
      )
      incProgress(0.9, detail = "Done")
      rv$last_search <- res
      rv$last_search$market_name <- market$name
    })
    showNotification("Grid search complete", type = "message")
  })
  
  # ── Search summary ─────────────────────────────────────────
  output$search_summary <- renderText({
    r <- rv$last_search
    if (is.null(r)) return("No search yet. Configure market and click Run.")
    paste(
      paste0("Market: ", r$market_name),
      paste0("Rules tested: ", r$n_rules_tested,
             " (", r$n_unique_rules, " unique after dedup)"),
      paste0("Baseline hit rate: ", round(r$baseline$hit_rate * 100, 2), "% ",
             "on ", r$baseline$n_gradable, " gradable fixtures"),
      sep = "\n"
    )
  })
  
  # ── Baseline detail ────────────────────────────────────────
  output$baseline_info <- renderText({
    r <- rv$last_search
    if (is.null(r)) return("No search yet.")
    b <- r$baseline
    paste(
      "Baseline = 'pick this market on every gradable fixture in the pool'.",
      "Any rule below baseline is worse than random.",
      "",
      paste0("Gradable fixtures: ", b$n_gradable),
      paste0("Wins: ", b$n_wins),
      paste0("Hit rate: ", round(b$hit_rate * 100, 2), "%"),
      sep = "\n"
    )
  })
  
  # ── Picked leaderboard ─────────────────────────────────────
  output$picked_leaderboard <- renderDT({
    r <- rv$last_search
    if (is.null(r)) return(NULL)
    lb <- sort_leaderboard(r$leaderboard,
                            sort_by = input$sort_by,
                            min_picks = input$min_picks,
                            dedup = input$dedup_lb)
    if (nrow(lb) == 0) {
      return(datatable(data.frame(Message = "No rules meet the minimum picks floor"),
                       options = list(dom = "t"), rownames = FALSE))
    }
    show_cols <- c(names(r$leaderboard)[!(names(r$leaderboard) %in%
                     c("rule_id","fingerprint","rejected_wins","dedup_group"))])
    lb_display <- lb[, show_cols, drop = FALSE]
    # Round numeric cols
    num_cols <- sapply(lb_display, is.numeric)
    lb_display[num_cols] <- lapply(lb_display[num_cols], function(x) round(x, 3))
    datatable(lb_display, options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ── Rejected leaderboard (missed opportunities) ────────────
  output$rejected_leaderboard <- renderDT({
    r <- rv$last_search
    if (is.null(r)) return(NULL)
    lb <- sort_by_missed_opportunity(r$leaderboard,
                                       min_rejected = input$min_picks,
                                       dedup = input$dedup_lb)
    if (nrow(lb) == 0) {
      return(datatable(data.frame(Message = "No rules with enough rejected fixtures"),
                       options = list(dom = "t"), rownames = FALSE))
    }
    show_cols <- c(names(r$leaderboard)[!(names(r$leaderboard) %in%
                     c("rule_id","fingerprint","dedup_group"))])
    lb_display <- lb[, show_cols, drop = FALSE]
    num_cols <- sapply(lb_display, is.numeric)
    lb_display[num_cols] <- lapply(lb_display[num_cols], function(x) round(x, 3))
    datatable(lb_display, options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE)
  })
  
  # ══════════════════════════════════════════════════════════
  # MARKETS EDITOR
  # ══════════════════════════════════════════════════════════
  
  # Feature list sidebar
  output$feature_list <- renderText({
    if (length(FEATURE_CATALOG) == 0) return("(none)")
    lines <- character()
    cats <- unique(sapply(FEATURE_CATALOG, function(x) x$category))
    for (c in cats) {
      lines <- c(lines, paste0("[", c, "]"))
      for (fn in names(FEATURE_CATALOG)) {
        if (identical(FEATURE_CATALOG[[fn]]$category, c)) {
          lines <- c(lines, paste0("  ", fn))
        }
      }
      lines <- c(lines, "")
    }
    paste(lines, collapse = "\n")
  })
  
  # Load selected market file into editor
  observeEvent(input$market_edit_pick, {
    if (input$market_edit_pick == "") {
      updateTextAreaInput_safe(session, "market_code", "")
      updateTextInput(session, "edit_filename", value = "")
      return()
    }
    path <- file.path(MARKETS_DIR, input$market_edit_pick)
    if (!file.exists(path)) return()
    code <- paste(readLines(path, warn = FALSE), collapse = "\n")
    updateTextAreaInput_safe(session, "market_code", code)
    updateTextInput(session, "edit_filename", value = input$market_edit_pick)
  })
  
  # New market template
  observeEvent(input$new_market_btn, {
    template <- paste(
      "# markets/new_market.R",
      "# Rename filename above and this comment.",
      "",
      "market_new_market <- list(",
      "  name = \"NewMarket\",",
      "  full_name = \"New Market Full Name\",",
      "  tier = 1,",
      "  description = \"Describe what wins this market\",",
      "  grade_fn = function(row) {",
      "    # Return TRUE (win), FALSE (loss), or NA (ungradable)",
      "    h <- row$result_ft_home",
      "    a <- row$result_ft_away",
      "    if (is.na(h) || is.na(a)) return(NA)",
      "    (h + a) >= 3  # example: Over 2.5 FT",
      "  },",
      "  threshold_grid = list(",
      "    h_scored_n5 = 2:5,",
      "    a_scored_n5 = 2:5",
      "  ),",
      "  min_picks_default = 30,",
      "  primary_sort = \"hit_rate\",",
      "  window = 5,",
      "  main_only = FALSE,",
      "  min_required = NULL,",
      "  notes = \"\"",
      ")",
      sep = "\n"
    )
    updateTextAreaInput_safe(session, "market_code", template)
    updateTextInput(session, "edit_filename", value = "new_market.R")
  })
  
  # Duplicate current
  observeEvent(input$duplicate_market_btn, {
    fname <- input$edit_filename
    if (fname == "") return()
    base <- tools::file_path_sans_ext(fname)
    updateTextInput(session, "edit_filename", value = paste0(base, "_copy.R"))
  })
  
  # Validate syntax
  validate_market_code <- function(code) {
    result <- tryCatch({
      env <- new.env()
      eval(parse(text = code), envir = env)
      obj_names <- ls(env)
      market_var <- obj_names[grepl("^market_", obj_names)]
      if (length(market_var) == 0) {
        return(list(ok = FALSE, msg = "No `market_*` variable found in code."))
      }
      m <- get(market_var[1], envir = env)
      required <- c("name", "grade_fn", "threshold_grid")
      missing_fields <- required[!(required %in% names(m))]
      if (length(missing_fields) > 0) {
        return(list(ok = FALSE, msg = paste0("Missing required fields: ",
                                                paste(missing_fields, collapse = ", "))))
      }
      # Feature name check
      unknown <- setdiff(names(m$threshold_grid), names(FEATURE_CATALOG))
      if (length(unknown) > 0) {
        return(list(ok = FALSE, msg = paste0("Unknown features referenced (not in FEATURE_CATALOG): ",
                                                paste(unknown, collapse = ", "))))
      }
      list(ok = TRUE, msg = paste0("OK. Market '", m$name, "' with ",
                                    length(m$threshold_grid), " features, ",
                                    prod(sapply(m$threshold_grid, length)), " threshold combos."))
    }, error = function(e) {
      list(ok = FALSE, msg = paste0("Parse error: ", e$message))
    })
    result
  }
  
  observeEvent(input$validate_market_btn, {
    res <- validate_market_code(input$market_code)
    rv$market_edit_status <- res$msg
  })
  
  # Save (with validation + backup)
  observeEvent(input$save_market_btn, {
    fname <- trimws(input$edit_filename)
    if (fname == "" || !grepl("\\.R$", fname)) {
      rv$market_edit_status <- "Filename must end with .R"
      return()
    }
    res <- validate_market_code(input$market_code)
    if (!res$ok) {
      rv$market_edit_status <- paste0("Not saved. ", res$msg)
      return()
    }
    # Backup existing file if present
    target <- file.path(MARKETS_DIR, fname)
    if (file.exists(target)) {
      backup_name <- paste0(tools::file_path_sans_ext(fname),
                             "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      file.copy(target, file.path(BACKUPS_DIR, backup_name))
    }
    writeLines(input$market_code, target)
    rv$market_edit_status <- paste0("Saved: ", fname, " | ", res$msg)
    refresh_markets()
  })
  
  # Delete
  observeEvent(input$delete_market_btn, {
    fname <- trimws(input$edit_filename)
    if (fname == "") return()
    target <- file.path(MARKETS_DIR, fname)
    if (file.exists(target)) {
      # Move to backups instead of hard delete
      backup_name <- paste0("deleted_", tools::file_path_sans_ext(fname),
                             "_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
      file.rename(target, file.path(BACKUPS_DIR, backup_name))
      rv$market_edit_status <- paste0("Deleted (moved to backups): ", fname)
      refresh_markets()
      updateTextAreaInput_safe(session, "market_code", "")
      updateTextInput(session, "edit_filename", value = "")
    }
  })
  
  output$market_edit_status <- renderText({
    if (length(rv$market_errors) > 0) {
      errs <- paste0(names(rv$market_errors), ": ", unlist(rv$market_errors), collapse = "\n")
      return(paste0(rv$market_edit_status, "\n\nStartup errors:\n", errs))
    }
    rv$market_edit_status
  })
}

# ─── Helper: safe update for textareaInput (shiny uses updateTextAreaInput) ─
updateTextAreaInput_safe <- function(session, id, value) {
  session$sendInputMessage(id, list(value = value))
}

# ─── Launch ──────────────────────────────────────────────────
shinyApp(ui, server)

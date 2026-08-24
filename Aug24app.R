# ============================================================
# app.R
# MOAB Grid Searcher — Shiny application (Session 2)
#
# What's new vs Session 1:
#   - Full feature library (stats, H2H, meeting-point, Poisson, goal-timing)
#   - Rule families: ge / le / range / diff / ratio / sum / quality
#   - Window sweeps (windows 3/5/7 available per feature)
#   - Coverage panel (feature-level %pop with tier classification)
#   - Orthogonality warnings (|r| >= 0.85 by default)
#   - Round-driven main_league policy (rounds 1-4 off, 5-8 optional, 9+ on for stats)
#   - Markets: BTTS, Over 2.5, Over 1.5, HT Draw, Home/Away to Score,
#     2H Over 1.5, Home Clean Sheet
# ============================================================

library(shiny)
library(DT)

# ─── Paths ───────────────────────────────────────────────────
APP_DIR       <- normalizePath(".", mustWork = FALSE)
ENGINE_DIR    <- file.path(APP_DIR, "engine")
MARKETS_DIR   <- file.path(APP_DIR, "markets")
BACKUPS_DIR   <- file.path(MARKETS_DIR, "backups")
SESSIONS_DIR  <- file.path(APP_DIR, "sessions")
INTUITIONS_DIR<- file.path(APP_DIR, "intuitions")
DRIFT_DIR     <- file.path(APP_DIR, "drift")
DEPLOY_DIR    <- file.path(APP_DIR, "deploy")
for (d in c(MARKETS_DIR, BACKUPS_DIR, SESSIONS_DIR, INTUITIONS_DIR,
            DRIFT_DIR, DEPLOY_DIR,
            file.path(APP_DIR, "uploads"))) {
  if (!dir.exists(d)) dir.create(d, recursive = TRUE)
}

# ─── Source engine files ─────────────────────────────────────
source(file.path(ENGINE_DIR, "loading.R"))
source(file.path(ENGINE_DIR, "features.R"))
source(file.path(ENGINE_DIR, "grid_search.R"))
source(file.path(ENGINE_DIR, "coverage.R"))
source(file.path(ENGINE_DIR, "orthogonality.R"))
source(file.path(ENGINE_DIR, "round_policy.R"))
source(file.path(ENGINE_DIR, "intuition_loader.R"))
source(file.path(ENGINE_DIR, "stage2.R"))
source(file.path(ENGINE_DIR, "validation.R"))
source(file.path(ENGINE_DIR, "drift.R"))
source(file.path(ENGINE_DIR, "code_emitter.R"))
source(file.path(ENGINE_DIR, "safety.R"))
source(file.path(ENGINE_DIR, "replay.R"))
source(file.path(ENGINE_DIR, "launcher_card.R"))
source(file.path(ENGINE_DIR, "rule_inspector.R"))

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Market loader (with tryCatch safety, unchanged from Session 1) ─
load_all_markets <- function() {
  files <- list.files(MARKETS_DIR, pattern = "\\.R$", full.names = TRUE)
  markets <- list(); errors <- list()
  for (f in files) {
    result <- tryCatch({
      env <- new.env()
      sys.source(f, envir = env)
      obj_names <- ls(env)
      market_var <- obj_names[grepl("^market_", obj_names)]
      if (length(market_var) == 0) return(NULL)
      m <- get(market_var[1], envir = env)
      m$source_file <- basename(f)
      # Backward compat: convert threshold_grid to grid_spec if the market
      # is still Session-1 style.
      if (is.null(m$grid_spec) && !is.null(m$threshold_grid)) {
        m$grid_spec <- threshold_grid_to_spec(m$threshold_grid)
      }
      m
    }, error = function(e) {
      errors[[basename(f)]] <<- e$message; NULL
    })
    if (!is.null(result)) markets[[result$name]] <- result
  }
  list(markets = markets, errors = errors)
}

# Helper: total combos for a grid_spec
count_combos <- function(grid_spec) {
  if (is.null(grid_spec) || length(grid_spec) == 0) return(0)
  total <- 1
  for (tpl in grid_spec) {
    fam <- tpl$family %||% "ge"
    if (fam == "range") {
      # rough upper bound; enforce_lo_le_hi actually shrinks it
      total <- total * (length(tpl$grid_low) * length(tpl$grid_high))
    } else {
      total <- total * length(tpl$grid)
    }
  }
  total
}

# ─── UI ──────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet", type = "text/css", href = "moab.css"),
    tags$title("MOAB Grid Searcher")
  ),
  # ─── Persistent top status bar ─────────────────────────────
  div(class = "moab-titlebar",
    span(class = "brand", "MOAB Grid Searcher",
          tags$small("Tune. Validate. Deploy.")),
    uiOutput("top_status_market",   inline = TRUE),
    uiOutput("top_status_round",    inline = TRUE),
    uiOutput("top_status_pipeline", inline = TRUE),
    uiOutput("top_status_alerts",   inline = TRUE)
  ),
  # ─── Startup safety banner (only appears if there are findings) ─
  uiOutput("startup_safety_banner"),

  tabsetPanel(id = "main_tabs",

    # ═══════════════════════════════════════════════════════════
    # TAB 1 — DATA
    # ═══════════════════════════════════════════════════════════
    tabPanel("Data",
      br(),
      fluidRow(
        column(4,
          div(class = "moab-card",
            h4("Load mode"),
            radioButtons("load_mode", NULL,
              choices = c("Single file" = "single", "Multi-file (with roles)" = "multi"),
              selected = "single")
          ),
          div(class = "moab-card",
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
          div(class = "moab-card",
            h4("Current round"),
            numericInput("current_round", NULL, value = 1, min = 1, max = 40, step = 1),
            verbatimTextOutput("round_policy_hint")
          )
        ),
        column(8,
          div(class = "moab-card",
            h4("Loaded files"),
            DTOutput("files_table")
          ),
          div(class = "moab-card",
            h4("Summary"),
            verbatimTextOutput("load_summary")
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 2 — GRID SEARCH
    # ═══════════════════════════════════════════════════════════
    tabPanel("Grid Search",
      br(),
      fluidRow(
        column(3,
          div(class = "moab-card",
            h4("Market"),
            selectInput("market_pick", NULL, choices = c(""), selected = ""),
            verbatimTextOutput("market_desc")
          ),
          div(class = "moab-card",
            h4("Options"),
            radioButtons("main_only_choice", "Main-league filter:",
              choices = c("Default for round" = "default",
                          "Force ON"          = "on",
                          "Force OFF"         = "off"),
              selected = "default"),
            numericInput("default_window", "Default window (if not in feature name):",
                          value = 5, min = 3, max = 10),
            numericInput("min_picks", "Min picks (leaderboard floor):", value = 30, min = 1),
            radioButtons("sort_by", "Sort by:",
              choices = c("Hit rate" = "hit_rate",
                          "Volume"   = "volume",
                          "Baseline lift" = "baseline_lift"),
              selected = "hit_rate"),
            checkboxInput("dedup_lb", "Dedup identical rules", value = TRUE)
          ),
          div(class = "moab-card",
            actionButton("run_search_btn", "Run Grid Search",
                         class = "btn-primary", style = "width: 100%"),
            br(), br(),
            conditionalPanel(
              condition = "output.show_side_by_side == true",
              actionButton("run_side_by_side_btn",
                           "Also run w/ main_only=ON (side-by-side)",
                           class = "btn-default", style = "width: 100%")
            )
          )
        ),
        column(9,
          div(class = "moab-card",
            h4("Results"),
            verbatimTextOutput("search_summary"),
            tabsetPanel(
              tabPanel("Picked (main leaderboard)",
                br(),
                DTOutput("picked_leaderboard")
              ),
              tabPanel("Rejected (missed opportunities)",
                br(),
                p(em("Rules whose rejected pile has high win rate = rules leaving winners on the table. Diagnostic view — no separate action pipeline.")),
                DTOutput("rejected_leaderboard")
              ),
              tabPanel("Side-by-side (main-league on/off)",
                br(),
                p(em("Available in rounds 5-8. Compares tuned rules with and without the is_main_league filter.")),
                DTOutput("sidebyside_lb_off"),
                br(),
                DTOutput("sidebyside_lb_on")
              ),
              tabPanel("Baseline",
                br(),
                verbatimTextOutput("baseline_info")
              ),
              tabPanel("Rule Inspector",
                br(),
                p(em("Flat view of every fixture a rule picked, with full form/H2H unfurled into columns. Use column filters to isolate losses and eyeball patterns.")),
                fluidRow(
                  column(6,
                    selectInput("inspector_rule_pick", "Which rule to inspect:",
                      choices = c("Top 1" = "1", "Top 2" = "2", "Top 3" = "3"),
                      selected = "1")
                  ),
                  column(6,
                    br(),
                    downloadButton("inspector_download_btn", "Download as .xlsx",
                                    class = "btn-primary")
                  )
                ),
                verbatimTextOutput("inspector_summary"),
                DTOutput("inspector_table")
              )
            )
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 3 — COVERAGE
    # ═══════════════════════════════════════════════════════════
    tabPanel("Coverage",
      br(),
      fluidRow(
        column(4,
          h4("Pool coverage"),
          p(em("Fraction of loaded fixtures whose data populates each feature. Stats-based features drop out early-season and grow with match volume.")),
          actionButton("run_coverage_btn", "Compute coverage", class = "btn-primary"),
          br(), br(),
          selectInput("coverage_category", "Category filter:",
            choices = c("(all)", "goals", "stats", "goal_timing", "h2h",
                        "poisson", "standings", "meeting_point"),
            selected = "(all)"),
          verbatimTextOutput("coverage_summary_out")
        ),
        column(8,
          h4("Feature coverage detail"),
          DTOutput("coverage_table")
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 4 — ORTHOGONALITY
    # ═══════════════════════════════════════════════════════════
    tabPanel("Orthogonality",
      br(),
      fluidRow(
        column(4,
          h4("Correlation check"),
          p(em("For the currently loaded pool, computes pairwise correlations across all features and flags pairs above the threshold. Highly correlated features waste search compute and inflate apparent rule strength.")),
          numericInput("orth_threshold", "|r| threshold:",
                        value = 0.85, min = 0.5, max = 1.0, step = 0.05),
          numericInput("orth_sample_size", "Sample fixtures (for speed):",
                        value = 300, min = 50, max = 3000, step = 50),
          actionButton("run_orth_btn", "Compute correlations", class = "btn-primary"),
          br(), br(),
          verbatimTextOutput("orth_summary_out")
        ),
        column(8,
          h4("Correlated pairs"),
          DTOutput("orth_pairs_table"),
          hr(),
          h4("Suggested orthogonal-only feature set"),
          p(em("Greedy pruning: drops the feature that appears in the most correlated pairs first, repeats until none remain.")),
          verbatimTextOutput("orth_keep_list")
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 5 — FEATURES (catalog browser)
    # ═══════════════════════════════════════════════════════════
    # ═══════════════════════════════════════════════════════════
    # TAB — STAGE 2
    # ═══════════════════════════════════════════════════════════
    tabPanel("Stage 2",
      br(),
      fluidRow(
        column(3,
          h4("Stage 1 winner"),
          p(em("Pick a Stage 1 rule from the last grid search to carry forward.")),
          selectInput("s2_rule_pick", "Rule (top of leaderboard):",
                      choices = c(""), selected = ""),
          verbatimTextOutput("s2_rule_summary"),
          hr(),
          h4("Intuition scope"),
          uiOutput("s2_category_selector"),
          checkboxInput("s2_depth_singles", "Test singles (depth = 1)", TRUE),
          checkboxInput("s2_depth_pairs",   "Test pairs (depth = 2)", TRUE),
          checkboxInput("s2_depth_triples", "Test triples (depth = 3)", FALSE),
          numericInput("s2_min_picks", "Min picks (leaderboard floor):",
                        value = 10, min = 1),
          radioButtons("s2_sort_by", "Sort by:",
            choices = c("Hit rate" = "hit_rate",
                        "Lift"     = "lift",
                        "Volume"   = "volume"),
            selected = "hit_rate"),
          hr(),
          actionButton("s2_reload_intuitions", "Reload intuitions"),
          br(), br(),
          actionButton("s2_run_btn", "Run Stage 2",
                       class = "btn-primary", style = "width: 100%")
        ),
        column(9,
          h4("Stage 2 results"),
          verbatimTextOutput("s2_summary"),
          tabsetPanel(
            tabPanel("Refine (filter Stage 1 picks)",
                     br(),
                     p(em("Applied to fixtures the Stage 1 rule PICKED. Improvement = same wins, fewer losses.")),
                     DTOutput("s2_refine_lb")
            ),
            tabPanel("Intuition library",
              br(),
              verbatimTextOutput("s2_lib_errors"),
              DTOutput("s2_lib_table")
            )
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB — VALIDATION
    # ═══════════════════════════════════════════════════════════
    tabPanel("Validation",
      br(),
      fluidRow(
        column(4,
          h4("Rule under test"),
          verbatimTextOutput("val_pipeline_summary"),
          hr(),
          h4("1. Validate"),
          p(em("Re-grade the current rule on files tagged 'Validate'.")),
          numericInput("val_tolerance", "Collapse tolerance (pp drop):",
                        value = 0.10, min = 0.01, max = 0.30, step = 0.01),
          actionButton("val_run_validate_btn", "Run Validate", class = "btn-primary"),
          hr(),
          h4("2. Commit"),
          p(em("Only VALIDATED rules can be committed. Committing UNLOCKS the Test tab.")),
          actionButton("val_commit_btn", "Commit rule"),
          checkboxInput("val_force_commit", "Force-commit collapsed rule (not recommended)", FALSE),
          hr(),
          h4("3. Test"),
          p(em("Hard-blocked until commit. One-shot terminal measurement.")),
          uiOutput("val_test_gate_ui")
        ),
        column(8,
          h4("Metrics timeline"),
          DTOutput("val_metrics_table"),
          hr(),
          h4("Per-round breakdown"),
          p(em("Rule performance by loaded file (round proxy). High variance across rounds = unstable rule.")),
          verbatimTextOutput("val_stability_summary"),
          DTOutput("val_per_round_table")
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB — DRIFT MONITOR
    # ═══════════════════════════════════════════════════════════
    tabPanel("Drift Monitor",
      br(),
      fluidRow(
        column(3,
          h4("Registered rules"),
          verbatimTextOutput("drift_summary_out"),
          hr(),
          h4("Register current rule"),
          p(em("Adds the currently-tested rule (from Validation tab) to the drift registry.")),
          numericInput("drift_hit_drop", "Alert: hit-rate drop (pp)",
                        value = 0.10, min = 0.02, max = 0.30, step = 0.01),
          numericInput("drift_vol_drop", "Alert: volume drop (fraction)",
                        value = 0.30, min = 0.05, max = 0.90, step = 0.05),
          numericInput("drift_loss_ceiling", "Alert: loss ceiling per round",
                        value = 4, min = 1, max = 20, step = 1),
          numericInput("drift_window", "Rolling window (rounds)",
                        value = 3, min = 1, max = 10, step = 1),
          actionButton("drift_register_btn", "Register rule", class = "btn-primary"),
          hr(),
          h4("Log latest round"),
          p(em("Uses the current Grid Search fixtures pool (Tune) as the 'latest round' source. Rebuilds the feature matrix and grades every registered rule.")),
          textInput("drift_round_label", "Round label:",
                     value = format(Sys.Date(), "R%Y%m%d")),
          actionButton("drift_log_btn", "Log & re-evaluate alerts")
        ),
        column(9,
          h4("Dashboard"),
          DTOutput("drift_dashboard"),
          hr(),
          h4("Per-rule history"),
          selectInput("drift_selected_rule", "Rule:", choices = c(""), selected = ""),
          DTOutput("drift_history_table"),
          br(),
          actionButton("drift_retire_btn", "Retire rule", class = "btn-warning"),
          actionButton("drift_reactivate_btn", "Reactivate rule"),
          actionButton("drift_delete_btn", "Delete rule + history", class = "btn-danger"),
          verbatimTextOutput("drift_action_status")
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB — DEPLOY (code emission + push)
    # ═══════════════════════════════════════════════════════════
    tabPanel("Deploy",
      br(),
      fluidRow(
        column(4,
          h4("Source rule"),
          p(em("Emit code from the current pipeline (Validation tab) or a registered rule.")),
          radioButtons("deploy_source", NULL,
            choices = c("Current pipeline" = "pipeline",
                        "Registered rule"  = "registry"),
            selected = "pipeline"),
          conditionalPanel(
            condition = "input.deploy_source == 'registry'",
            selectInput("deploy_registry_pick", "Rule:", choices = c(""), selected = "")
          ),
          hr(),
          h4("Target file"),
          textInput("deploy_target_file", "Target moab_analysis.R path:",
                     value = file.path(DEPLOY_DIR, "moab_analysis.R")),
          p(em("Backup will be written to the same directory as the target: moab_analysis_backup_YYYYMMDD_HHMM.R")),
          hr(),
          actionButton("deploy_preview_btn", "Generate & preview diff",
                       class = "btn-primary", style = "width: 100%"),
          br(), br(),
          actionButton("deploy_apply_btn", "Apply push (writes backup + new file)",
                       class = "btn-warning", style = "width: 100%"),
          br(), br(),
          downloadButton("deploy_download_manual", "Download emitted block only",
                          style = "width: 100%"),
          hr(),
          verbatimTextOutput("deploy_action_status")
        ),
        column(8,
          h4("Emitted block (manual-paste fallback)"),
          verbatimTextOutput("deploy_emitted_block"),
          hr(),
          h4("Diff preview"),
          verbatimTextOutput("deploy_diff_stats"),
          fluidRow(
            column(6,
              h5("OLD (current file)"),
              verbatimTextOutput("deploy_old_content")
            ),
            column(6,
              h5("NEW (after apply)"),
              verbatimTextOutput("deploy_new_content")
            )
          )
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB — SESSION (snapshots)
    # ═══════════════════════════════════════════════════════════
    tabPanel("Session",
      br(),
      fluidRow(
        column(4,
          div(class = "moab-card",
            h4("Save workspace"),
            p(class = "moab-card-note",
              em("Captures files, current round, Stage 1 result, Stage 2 result, pipeline state, and coverage/orthogonality caches. Drift registry lives separately and persists regardless.")),
            textInput("snapshot_name", "Snapshot name:", value = ""),
            actionButton("snapshot_save_btn", "Save snapshot", class = "btn-primary")
          ),
          div(class = "moab-card",
            h4("Load workspace"),
            selectInput("snapshot_load_choice", "Available snapshots:",
                        choices = c(""), selected = ""),
            actionButton("snapshot_load_btn", "Load snapshot"),
            actionButton("snapshot_delete_btn", "Delete snapshot", class = "btn-danger"),
            hr(),
            verbatimTextOutput("snapshot_load_preview")
          )
        ),
        column(8,
          div(class = "moab-card",
            h4("Launcher card (dormant)"),
            p(class = "moab-card-note",
              em("Preview and generate the launcher-card snippet for MOAB. Integration is switched off until you enable MOAB_LAUNCHER_INTEGRATION_ENABLED in engine/launcher_card.R.")),
            verbatimTextOutput("launcher_preview"),
            hr(),
            actionButton("launcher_generate_btn", "Generate snippet"),
            downloadButton("launcher_download_btn", "Download snippet"),
            hr(),
            verbatimTextOutput("launcher_snippet_out")
          )
        )
      )
    ),

    tabPanel("Features",
      br(),
      fluidRow(
        column(3,
          h4("Filters"),
          selectInput("feat_cat", "Category:",
            choices = c("(all)", "goals", "stats", "goal_timing", "h2h",
                        "poisson", "standings", "meeting_point"),
            selected = "(all)"),
          selectInput("feat_side", "Side:",
            choices = c("(all)", "home", "away", "h2h", "match"),
            selected = "(all)"),
          checkboxInput("feat_stats_only", "Only stats-based", FALSE),
          checkboxInput("feat_gt_only",    "Only goal-timing", FALSE)
        ),
        column(9,
          h4("Feature catalog"),
          DTOutput("features_table")
        )
      )
    ),

    # ═══════════════════════════════════════════════════════════
    # TAB 6 — MARKETS (editor)
    # ═══════════════════════════════════════════════════════════
    tabPanel("Markets",
      br(),
      fluidRow(
        column(3,
          h4("Market files"),
          selectInput("market_edit_pick", NULL, choices = c(""), selected = ""),
          actionButton("new_market_btn", "New"),
          actionButton("duplicate_market_btn", "Duplicate"),
          actionButton("reload_markets_btn", "Reload all")
        ),
        column(9,
          h4("Editor"),
          p(em("Edit the R code. Click Validate & Save to write to disk. A timestamped backup is created before overwrite. Markets must supply grid_spec (or legacy threshold_grid).")),
          textInput("edit_filename", "Filename (.R):", value = ""),
          tags$textarea(id = "market_code", rows = 30,
                        style = "width:100%; font-family: monospace; font-size: 12px;", ""),
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

  rv <- reactiveValues(
    files_df = NULL,
    load_mode = "single",
    markets = list(),
    market_errors = list(),
    last_search = NULL,            # main run
    side_by_side_run = NULL,       # optional main_only=ON companion run
    coverage = NULL,               # data.frame from compute_pool_coverage
    orth_pairs = NULL,
    orth_kept  = NULL,
    market_edit_status = "",
    # Session 3 additions
    intuitions = list(),
    intuition_errors = list(),
    stage2_result = NULL,
    pipeline = NULL,               # the rule currently walking Tune->Validate->Test
    validate_feat_df = NULL,       # cached feature matrix for Validate set
    test_feat_df = NULL,           # cached feature matrix for Test set
    # Session 4 additions
    drift_action_status = "",
    deploy_preview = NULL,         # list(old_lines, new_lines, action, summary)
    deploy_action_status = "",
    # Session 5 additions
    startup_findings = list(),
    snapshot_status = "",
    launcher_snippet = ""
  )

  # ── Startup: load markets ─────────────────────────────────
  refresh_markets <- function() {
    res <- load_all_markets()
    rv$markets <- res$markets
    rv$market_errors <- res$errors
    updateSelectInput(session, "market_pick",
      choices = c("", names(rv$markets)), selected = "")
    updateSelectInput(session, "market_edit_pick",
      choices = c("", basename(list.files(MARKETS_DIR, pattern = "\\.R$"))), selected = "")
    updateSelectInput(session, "session_load_choice",
      choices = c("", list_sessions(SESSIONS_DIR)), selected = "")
  }

  refresh_intuitions <- function() {
    res <- load_all_intuitions(INTUITIONS_DIR)
    rv$intuitions <- res$intuitions
    rv$intuition_errors <- res$errors
  }
  observe({ refresh_intuitions() }, priority = 999)
  observeEvent(input$s2_reload_intuitions, {
    refresh_intuitions()
    showNotification(paste0("Reloaded intuitions: ", length(rv$intuitions),
                            "; Errors: ", length(rv$intuition_errors)), type = "message")
  })

  # ══════════════════════════════════════════════════════════
  # SESSION 5 — Startup safety
  # ══════════════════════════════════════════════════════════
  observe({
    # Run once at startup after markets have loaded
    isolate({
      rv$startup_findings <- run_startup_checks(
        paths = list(
          engine = ENGINE_DIR, markets = MARKETS_DIR,
          intuitions = INTUITIONS_DIR, uploads = file.path(APP_DIR, "uploads"),
          drift = DRIFT_DIR
        ),
        markets_loaded = rv$markets
      )
    })
  }, priority = 100)

  output$startup_safety_banner <- renderUI({
    findings <- rv$startup_findings
    if (length(findings) == 0) return(NULL)
    summ <- summarize_findings(findings)
    if (summ$errors + summ$warns == 0) return(NULL)
    severity_class <- if (summ$errors > 0) "moab-card"  # errors take precedence
                      else "moab-card"
    border_color <- if (summ$errors > 0) "#dc2626" else "#d97706"
    div(class = severity_class,
      style = paste0("border-left: 4px solid ", border_color, ";"),
      h4(
        if (summ$errors > 0)
          paste0("Startup errors: ", summ$errors,
                  if (summ$warns > 0) paste0(" (plus ", summ$warns, " warnings)") else "")
        else
          paste0("Startup warnings: ", summ$warns)
      ),
      tags$pre(summ$text)
    )
  })

  # ══════════════════════════════════════════════════════════
  # SESSION 5 — Top status bar chips
  # ══════════════════════════════════════════════════════════
  output$top_status_market <- renderUI({
    m <- input$market_pick
    label <- if (is.null(m) || m == "") "(none)" else m
    span(class = "moab-status-chip",
      span(class = "label", "MARKET"),
      label)
  })
  output$top_status_round <- renderUI({
    r <- input$current_round
    span(class = "moab-status-chip",
      span(class = "label", "ROUND"),
      as.character(r %||% "?"))
  })
  output$top_status_pipeline <- renderUI({
    p <- rv$pipeline
    if (is.null(p)) {
      return(span(class = "moab-status-chip",
        span(class = "label", "PIPELINE"), "none"))
    }
    class_suffix <- switch(p$state,
      TUNED = "state-tuned",
      VALIDATED = "state-validated",
      VALIDATED_COLLAPSED = "state-collapsed",
      COMMITTED = "state-committed",
      TESTED = "state-tested",
      "state-tuned")
    span(class = paste("moab-status-chip", class_suffix),
      span(class = "label", "PIPELINE"),
      p$state)
  })
  output$top_status_alerts <- renderUI({
    rv$drift_action_status  # tie to reactive trigger
    dash <- tryCatch(build_drift_dashboard(DRIFT_DIR), error = function(e) NULL)
    if (is.null(dash) || nrow(dash) == 0) {
      return(span(class = "moab-status-chip",
        span(class = "label", "DRIFT"), "0 rules"))
    }
    n_alert <- sum(dash$in_alert, na.rm = TRUE)
    n_total <- nrow(dash)
    class_suffix <- if (n_alert > 0) "state-collapsed" else "state-committed"
    span(class = paste("moab-status-chip", class_suffix),
      span(class = "label", "DRIFT"),
      paste0(n_alert, " of ", n_total, " in alert"))
  })

  # ══════════════════════════════════════════════════════════
  # SESSION 5 — Snapshot save / load / delete
  # ══════════════════════════════════════════════════════════
  refresh_snapshots <- function() {
    updateSelectInput(session, "snapshot_load_choice",
      choices = c("", list_snapshots(SESSIONS_DIR)),
      selected = input$snapshot_load_choice %||% "")
  }
  observe({ refresh_snapshots() }, priority = 50)

  observeEvent(input$snapshot_save_btn, {
    nm <- trimws(input$snapshot_name)
    if (nm == "") {
      showNotification("Enter a snapshot name", type = "error"); return()
    }
    workspace <- list(
      files_df       = rv$files_df,
      current_round  = input$current_round,
      main_only_choice = input$main_only_choice,
      default_window = input$default_window,
      min_picks      = input$min_picks,
      market_pick    = input$market_pick,
      last_search    = rv$last_search,
      side_by_side_run = rv$side_by_side_run,
      stage2_result  = rv$stage2_result,
      pipeline       = rv$pipeline,
      coverage       = rv$coverage,
      orth_pairs     = rv$orth_pairs,
      orth_kept      = rv$orth_kept
    )
    res <- save_snapshot(nm, SESSIONS_DIR, workspace)
    if (res$ok) {
      rv$snapshot_status <- paste0("Saved snapshot: ", res$path)
      showNotification(rv$snapshot_status, type = "message")
      refresh_snapshots()
    } else {
      showNotification("Save failed", type = "error")
    }
  })

  observeEvent(input$snapshot_load_btn, {
    nm <- input$snapshot_load_choice
    if (is.null(nm) || nm == "") return()
    res <- load_snapshot(nm, SESSIONS_DIR)
    if (!res$ok) { showNotification(res$msg, type = "error"); return() }
    ws <- res$workspace
    # Restore reactiveValues
    rv$files_df         <- ws$files_df
    rv$last_search      <- ws$last_search
    rv$side_by_side_run <- ws$side_by_side_run
    rv$stage2_result    <- ws$stage2_result
    rv$pipeline         <- ws$pipeline
    rv$coverage         <- ws$coverage
    rv$orth_pairs       <- ws$orth_pairs
    rv$orth_kept        <- ws$orth_kept
    # Restore control values
    updateNumericInput(session, "current_round", value = ws$current_round %||% 1)
    updateRadioButtons(session, "main_only_choice", selected = ws$main_only_choice %||% "default")
    updateNumericInput(session, "default_window",  value = ws$default_window %||% 5)
    updateNumericInput(session, "min_picks",       value = ws$min_picks %||% 30)
    updateSelectInput(session, "market_pick", selected = ws$market_pick %||% "")
    rv$snapshot_status <- paste0("Loaded snapshot: ", nm)
    showNotification(rv$snapshot_status, type = "message")
  })

  observeEvent(input$snapshot_delete_btn, {
    nm <- input$snapshot_load_choice
    if (is.null(nm) || nm == "") return()
    path <- snapshot_filename(SESSIONS_DIR, nm)
    if (file.exists(path)) {
      file.remove(path)
      rv$snapshot_status <- paste0("Deleted: ", nm)
      refresh_snapshots()
    }
  })

  output$snapshot_load_preview <- renderText({
    nm <- input$snapshot_load_choice
    if (is.null(nm) || nm == "") return("Select a snapshot to preview it.")
    snapshot_summary(nm, SESSIONS_DIR)
  })

  # ══════════════════════════════════════════════════════════
  # SESSION 5 — Launcher card (dormant)
  # ══════════════════════════════════════════════════════════
  output$launcher_preview <- renderText({
    preview_launcher_card(APP_DIR)
  })
  observeEvent(input$launcher_generate_btn, {
    res <- generate_launcher_card(APP_DIR)
    if (!res$ok) {
      rv$launcher_snippet <- res$msg
      showNotification(res$msg, type = "warning", duration = 10)
    } else {
      rv$launcher_snippet <- paste(res$snippet, collapse = "\n")
      showNotification(res$msg, type = "message")
    }
  })
  output$launcher_snippet_out <- renderText({
    if (rv$launcher_snippet == "")
      return("Click 'Generate snippet' to build the launcher card code.")
    rv$launcher_snippet
  })
  output$launcher_download_btn <- downloadHandler(
    filename = function() paste0("grid_searcher_launcher_card_",
                                   format(Sys.time(), "%Y%m%d_%H%M%S"), ".R"),
    content = function(file) {
      res <- generate_launcher_card(APP_DIR)
      if (res$ok) writeLines(res$snippet, file)
      else        writeLines(paste0("# ", res$msg), file)
    }
  )
  observe({ refresh_markets() }, priority = 1000)
  observeEvent(input$reload_markets_btn, {
    refresh_markets()
    showNotification(paste0("Reloaded. Markets: ", length(rv$markets),
                            "; Errors: ", length(rv$market_errors)), type = "message")
  })

  observeEvent(input$load_mode, { rv$load_mode <- input$load_mode })

  # ── File upload ────────────────────────────────────────────
  observeEvent(input$upload_files, {
    req(input$upload_files)
    metas <- list()
    for (i in seq_len(nrow(input$upload_files))) {
      path <- input$upload_files$datapath[i]
      fname <- input$upload_files$name[i]
      stable_path <- file.path(APP_DIR, "uploads", fname)
      if (!dir.exists(dirname(stable_path))) dir.create(dirname(stable_path), recursive = TRUE)
      file.copy(path, stable_path, overwrite = TRUE)
      res <- load_sim_ready_file(stable_path)
      if (res$ok) {
        m <- res$meta
        m$filename <- fname; m$fullpath <- stable_path; m$role <- "Tune"
        metas[[length(metas)+1]] <- m
      }
    }
    if (length(metas) == 0) {
      showNotification("No valid RDS files loaded", type = "error"); return()
    }
    new_df <- do.call(rbind, metas)
    if (is.null(rv$files_df)) rv$files_df <- new_df
    else {
      combined <- rbind(rv$files_df, new_df)
      rv$files_df <- combined[!duplicated(combined$filename), , drop = FALSE]
    }
    showNotification(paste0("Loaded ", length(metas), " file(s)"), type = "message")
  })

  observeEvent(input$auto_tag_btn, {
    if (is.null(rv$files_df) || nrow(rv$files_df) == 0) return()
    rv$files_df <- auto_tag_roles(rv$files_df)
    showNotification("Roles auto-tagged", type = "message")
  })

  # ── Files table ────────────────────────────────────────────
  output$files_table <- renderDT({
    if (is.null(rv$files_df) || nrow(rv$files_df) == 0) return(NULL)
    show_cols <- c("filename","round_label","n_fixtures","n_final",
                   "date_min","date_max","role")
    df <- rv$files_df[, show_cols, drop = FALSE]
    datatable(df, options = list(pageLength = 10, dom = "t"),
              editable = if (rv$load_mode == "multi") list(target = "column", columns = 7) else FALSE,
              rownames = FALSE)
  })
  observeEvent(input$files_table_cell_edit, {
    info <- input$files_table_cell_edit
    if (rv$load_mode != "multi") return()
    if (info$col == 6) {
      nv <- as.character(info$value)
      if (!(nv %in% c("Tune","Validate","Test","Exclude"))) {
        showNotification("Role must be Tune/Validate/Test/Exclude", type = "error"); return()
      }
      rv$files_df$role[info$row] <- nv
    }
  })

  # ── Session save/load ──────────────────────────────────────
  observeEvent(input$save_session_btn, {
    nm <- trimws(input$session_name_save)
    if (nm == "" || is.null(rv$files_df)) {
      showNotification("Enter a session name and load files first", type = "error"); return()
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
    if (is.null(loaded)) { showNotification("Session not found", type = "error"); return() }
    rv$files_df <- loaded
    showNotification(paste0("Loaded session: ", nm), type = "message")
  })

  # ── Round policy hint ──────────────────────────────────────
  output$round_policy_hint <- renderText({
    pol <- main_league_default_for_round(input$current_round)
    hint <- switch(pol,
      off = "Rounds 1-4: main_league filter DISABLED by default (friendlies dominate).",
      optional = "Rounds 5-8: main_league filter OPTIONAL — use the side-by-side comparison.",
      on_for_stats = "Rounds 9+: main_league filter ON by default for stats markets (tier 2/3).",
      "")
    hint
  })

  # ── Load summary ───────────────────────────────────────────
  output$load_summary <- renderText({
    if (is.null(rv$files_df) || nrow(rv$files_df) == 0) return("No files loaded.")
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

  # ── Market info ────────────────────────────────────────────
  output$market_desc <- renderText({
    m <- rv$markets[[input$market_pick]]
    if (is.null(m)) return("Pick a market to see details.")
    combos <- count_combos(m$grid_spec)
    families <- paste(unique(vapply(m$grid_spec %||% list(), function(cl) cl$family %||% "ge", character(1))), collapse = ",")
    paste(
      paste0("Name: ", m$name),
      paste0("Tier: ", m$tier),
      paste0("Rule families used: ", families),
      paste0("Combos: ", combos),
      paste0("Notes: ", m$notes %||% ""),
      sep = "\n"
    )
  })

  # ── Compute effective main_only from round policy + user choice ─
  effective_main_only <- reactive({
    m <- rv$markets[[input$market_pick]]
    if (is.null(m)) return(FALSE)
    ov <- switch(input$main_only_choice, on = "on", off = "off", "default" = NULL, NULL)
    resolve_main_only(input$current_round, market_tier = m$tier %||% 1, user_override = ov)
  })

  # Side-by-side button visible only in rounds 5-8
  output$show_side_by_side <- reactive({
    side_by_side_needed(input$current_round)
  })
  outputOptions(output, "show_side_by_side", suspendWhenHidden = FALSE)

  # ── Run grid search ────────────────────────────────────────
  run_search_core <- function(fixtures, market, main_only, default_window,
                              min_picks) {
    feature_names <- unique(unlist(lapply(market$grid_spec, function(cl) {
      c(cl$feature, cl$feature2 %||% NA)
    })))
    feature_names <- feature_names[!is.na(feature_names) & feature_names != ""]
    withProgress(message = "Grid search", value = 0.1, {
      incProgress(0.15, detail = "Building features...")
      feat_df <- build_feature_matrix(
        fixtures, feature_names = feature_names,
        default_window = default_window,
        main_only = main_only,
        min_required = market$min_required)
      incProgress(0.35, detail = "Sweeping thresholds...")
      res <- run_grid_search(
        feat_df, grade_fn = market$grade_fn,
        grid_spec = market$grid_spec,
        min_picks = min_picks)
      incProgress(0.9, detail = "Done")
      res$market_name <- market$name
      res$main_only <- main_only
      res
    })
  }

  observeEvent(input$run_search_btn, {
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) {
      showNotification("Load at least one file with FINAL fixtures first", type = "error"); return()
    }
    market <- rv$markets[[input$market_pick]]
    if (is.null(market)) {
      showNotification("Pick a market first", type = "error"); return()
    }
    if (is.null(market$grid_spec) || length(market$grid_spec) == 0) {
      showNotification("Market has no grid_spec (or legacy threshold_grid)", type = "error"); return()
    }
    mo <- effective_main_only()
    rv$last_search <- run_search_core(pool$tune, market,
      main_only = mo,
      default_window = input$default_window,
      min_picks = input$min_picks)
    rv$side_by_side_run <- NULL
    showNotification(paste0("Grid search complete (main_only=", mo, ")"), type = "message")
  })

  observeEvent(input$run_side_by_side_btn, {
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) return()
    market <- rv$markets[[input$market_pick]]
    if (is.null(market)) return()
    rv$side_by_side_run <- run_search_core(pool$tune, market,
      main_only = TRUE,
      default_window = input$default_window,
      min_picks = input$min_picks)
    showNotification("Side-by-side run (main_only=ON) complete", type = "message")
  })

  # ── Search summary ─────────────────────────────────────────
  output$search_summary <- renderText({
    r <- rv$last_search
    if (is.null(r)) return("No search yet. Configure market and click Run.")
    paste(
      paste0("Market: ", r$market_name, " | main_only=", r$main_only),
      paste0("Rules tested: ", r$n_rules_tested,
             " (", r$n_unique_rules, " unique after dedup)"),
      paste0("Baseline hit rate: ", round(r$baseline$hit_rate * 100, 2), "% ",
             "on ", r$baseline$n_gradable, " gradable fixtures"),
      sep = "\n"
    )
  })

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

  # ── Leaderboards ───────────────────────────────────────────
  render_leaderboard <- function(res, sort_by, min_picks, dedup, mode = c("picked","rejected")) {
    mode <- match.arg(mode)
    if (is.null(res)) return(NULL)
    lb <- if (mode == "picked")
            sort_leaderboard(res$leaderboard, sort_by = sort_by,
                              min_picks = min_picks, dedup = dedup)
          else
            sort_by_missed_opportunity(res$leaderboard,
                              min_rejected = min_picks, dedup = dedup)
    if (nrow(lb) == 0) {
      return(datatable(data.frame(Message = "No rules meet the floor"),
                       options = list(dom = "t"), rownames = FALSE))
    }
    show_cols <- setdiff(names(lb), c("rule_id","dedup_group"))
    disp <- lb[, show_cols, drop = FALSE]
    num_cols <- sapply(disp, is.numeric)
    disp[num_cols] <- lapply(disp[num_cols], function(x) round(x, 3))
    datatable(disp, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  }

  output$picked_leaderboard <- renderDT({
    render_leaderboard(rv$last_search, input$sort_by, input$min_picks, input$dedup_lb, "picked")
  })
  output$rejected_leaderboard <- renderDT({
    render_leaderboard(rv$last_search, input$sort_by, input$min_picks, input$dedup_lb, "rejected")
  })

  output$sidebyside_lb_off <- renderDT({
    if (is.null(rv$last_search)) return(NULL)
    if (!isTRUE(side_by_side_needed(input$current_round))) return(NULL)
    lb <- sort_leaderboard(rv$last_search$leaderboard, sort_by = input$sort_by,
                            min_picks = input$min_picks, dedup = input$dedup_lb)
    if (nrow(lb) == 0) return(NULL)
    show_cols <- setdiff(names(lb), c("rule_id","dedup_group"))
    disp <- lb[, show_cols, drop = FALSE]
    num_cols <- sapply(disp, is.numeric); disp[num_cols] <- lapply(disp[num_cols], function(x) round(x,3))
    datatable(disp, caption = paste0("main_only=OFF (n_unique=", rv$last_search$n_unique_rules, ")"),
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })
  output$sidebyside_lb_on <- renderDT({
    r <- rv$side_by_side_run
    if (is.null(r)) return(NULL)
    lb <- sort_leaderboard(r$leaderboard, sort_by = input$sort_by,
                            min_picks = input$min_picks, dedup = input$dedup_lb)
    if (nrow(lb) == 0) return(NULL)
    show_cols <- setdiff(names(lb), c("rule_id","dedup_group"))
    disp <- lb[, show_cols, drop = FALSE]
    num_cols <- sapply(disp, is.numeric); disp[num_cols] <- lapply(disp[num_cols], function(x) round(x,3))
    datatable(disp, caption = paste0("main_only=ON  (n_unique=", r$n_unique_rules, ")"),
              options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # ══════════════════════════════════════════════════════════
  # COVERAGE TAB
  # ══════════════════════════════════════════════════════════
  observeEvent(input$run_coverage_btn, {
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) {
      showNotification("Load files first", type = "error"); return()
    }
    withProgress(message = "Computing coverage", value = 0.3, {
      cov <- compute_pool_coverage(pool$tune, feature_names = names(FEATURE_CATALOG),
                                    default_window = input$default_window)
      rv$coverage <- cov
    })
    showNotification("Coverage computed", type = "message")
  })

  output$coverage_summary_out <- renderText({
    if (is.null(rv$coverage)) return("Not computed yet.")
    coverage_summary_text(rv$coverage)
  })
  output$coverage_table <- renderDT({
    if (is.null(rv$coverage)) return(NULL)
    df <- rv$coverage
    if (input$coverage_category != "(all)") df <- df[df$category == input$coverage_category, , drop = FALSE]
    if (nrow(df) == 0) return(NULL)
    df$coverage_pct <- round(df$coverage * 100, 1)
    df <- df[, c("feature","category","coverage_pct","tier_action")]
    df <- df[order(df$category, -df$coverage_pct), ]
    datatable(df, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE) |>
      formatStyle("tier_action",
        backgroundColor = styleEqual(c("free","warn","disabled"),
                                     c("#d4edda","#fff3cd","#f8d7da")))
  })

  # ══════════════════════════════════════════════════════════
  # ORTHOGONALITY TAB
  # ══════════════════════════════════════════════════════════
  observeEvent(input$run_orth_btn, {
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) {
      showNotification("Load files first", type = "error"); return()
    }
    # Sample fixtures for speed
    fx <- pool$tune
    if (length(fx) > input$orth_sample_size)
      fx <- fx[sample(length(fx), input$orth_sample_size)]

    withProgress(message = "Computing correlations", value = 0.2, {
      # Restrict to features that are likely to be populated (skip stats/gt if
      # the coverage panel says they're disabled — safest is to compute on ALL,
      # cor() handles pairwise-complete gracefully)
      feature_names <- names(FEATURE_CATALOG)
      incProgress(0.2, detail = "Building feature matrix...")
      feat_df <- build_feature_matrix(fx, feature_names = feature_names,
                                       default_window = input$default_window,
                                       main_only = FALSE)
      incProgress(0.5, detail = "Correlation matrix...")
      cor_mat <- feature_correlation(feat_df, feature_names)
      pairs   <- correlated_pairs(cor_mat, threshold = input$orth_threshold)
      keep    <- suggest_orthogonal_set(feature_names, pairs)
      rv$orth_pairs <- pairs
      rv$orth_kept  <- keep
    })
    showNotification("Correlation check done", type = "message")
  })

  output$orth_summary_out <- renderText({
    if (is.null(rv$orth_pairs)) return("Not computed yet.")
    orthogonality_summary_text(rv$orth_pairs, cor_threshold = input$orth_threshold)
  })
  output$orth_pairs_table <- renderDT({
    if (is.null(rv$orth_pairs) || nrow(rv$orth_pairs) == 0) return(NULL)
    datatable(rv$orth_pairs, options = list(pageLength = 20, scrollX = TRUE), rownames = FALSE)
  })
  output$orth_keep_list <- renderText({
    if (is.null(rv$orth_kept)) return("(not computed)")
    paste0("Keep ", length(rv$orth_kept), " of ", length(FEATURE_CATALOG), " features:\n\n",
           paste(rv$orth_kept, collapse = ", "))
  })

  # ══════════════════════════════════════════════════════════
  # FEATURES TAB
  # ══════════════════════════════════════════════════════════
  output$features_table <- renderDT({
    df <- feature_catalog_df()
    if (input$feat_cat != "(all)")  df <- df[df$category == input$feat_cat, , drop = FALSE]
    if (input$feat_side != "(all)") df <- df[df$side == input$feat_side, , drop = FALSE]
    if (input$feat_stats_only)      df <- df[df$needs_stats, , drop = FALSE]
    if (input$feat_gt_only)         df <- df[df$needs_goaltime, , drop = FALSE]
    datatable(df, options = list(pageLength = 25, scrollX = TRUE), rownames = FALSE)
  })

  # ══════════════════════════════════════════════════════════
  # MARKETS EDITOR (Session 1 flow preserved; validation now accepts grid_spec too)
  # ══════════════════════════════════════════════════════════
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

  observeEvent(input$new_market_btn, {
    template <- paste(
      "# markets/new_market.R",
      "",
      "market_new_market <- list(",
      "  name = \"NewMarket\",",
      "  full_name = \"New Market Full Name\",",
      "  tier = 1,",
      "  description = \"Describe what wins this market\",",
      "  grade_fn = function(row) {",
      "    h <- row$result_ft_home; a <- row$result_ft_away",
      "    if (is.na(h) || is.na(a)) return(NA)",
      "    (h + a) >= 3  # example: Over 2.5 FT",
      "  },",
      "  # grid_spec is a list of clause templates; sweep = expand.grid across clauses",
      "  grid_spec = list(",
      "    list(family = \"ge\", feature = \"h_scored_n5\", grid = 2:5),",
      "    list(family = \"ge\", feature = \"a_scored_n5\", grid = 2:5)",
      "    # Other family examples:",
      "    #   list(family = \"le\",    feature = \"a_scored_n5\", grid = 1:3),",
      "    #   list(family = \"range\", feature = \"h_scored_n5\", grid_low = 2:3, grid_high = 4:5),",
      "    #   list(family = \"diff\",  feature = \"h_ppg\", feature2 = \"a_ppg\", grid = seq(0.5,1.5,0.25)),",
      "    #   list(family = \"sum\",   feature = \"h_scored_n5\", feature2 = \"a_scored_n5\", grid = 5:9),",
      "    #   list(family = \"ratio\", feature = \"lambda_h\",    feature2 = \"lambda_a\",    grid = seq(1.2,2.0,0.2))",
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

  observeEvent(input$duplicate_market_btn, {
    fname <- input$edit_filename
    if (fname == "") return()
    base <- tools::file_path_sans_ext(fname)
    updateTextInput(session, "edit_filename", value = paste0(base, "_copy.R"))
  })

  validate_market_code <- function(code) {
    result <- tryCatch({
      env <- new.env()
      eval(parse(text = code), envir = env)
      obj_names <- ls(env)
      market_var <- obj_names[grepl("^market_", obj_names)]
      if (length(market_var) == 0)
        return(list(ok = FALSE, msg = "No `market_*` variable found in code."))
      m <- get(market_var[1], envir = env)
      required <- c("name","grade_fn")
      missing_fields <- required[!(required %in% names(m))]
      if (length(missing_fields) > 0)
        return(list(ok = FALSE, msg = paste0("Missing required fields: ",
                                              paste(missing_fields, collapse = ", "))))
      # Prefer grid_spec, fall back to threshold_grid
      spec <- m$grid_spec
      if (is.null(spec) && !is.null(m$threshold_grid))
        spec <- threshold_grid_to_spec(m$threshold_grid)
      if (is.null(spec) || length(spec) == 0)
        return(list(ok = FALSE, msg = "Market must supply grid_spec (or threshold_grid)."))
      # Check every referenced feature is in catalog
      refs <- unique(unlist(lapply(spec, function(cl) c(cl$feature, cl$feature2))))
      refs <- refs[!is.null(refs) & !is.na(refs) & refs != ""]
      unknown <- setdiff(refs, names(FEATURE_CATALOG))
      if (length(unknown) > 0)
        return(list(ok = FALSE, msg = paste0("Unknown features (not in FEATURE_CATALOG): ",
                                              paste(unknown, collapse = ", "))))
      combos <- count_combos(spec)
      list(ok = TRUE, msg = paste0("OK. Market '", m$name, "' with ",
                                    length(spec), " clauses, ", combos, " threshold combos."))
    }, error = function(e) list(ok = FALSE, msg = paste0("Parse error: ", e$message)))
    result
  }

  observeEvent(input$validate_market_btn, {
    res <- validate_market_code(input$market_code)
    rv$market_edit_status <- res$msg
  })
  observeEvent(input$save_market_btn, {
    fname <- trimws(input$edit_filename)
    if (fname == "" || !grepl("\\.R$", fname)) {
      rv$market_edit_status <- "Filename must end with .R"; return()
    }
    res <- validate_market_code(input$market_code)
    if (!res$ok) { rv$market_edit_status <- paste0("Not saved. ", res$msg); return() }
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
  observeEvent(input$delete_market_btn, {
    fname <- trimws(input$edit_filename)
    if (fname == "") return()
    target <- file.path(MARKETS_DIR, fname)
    if (file.exists(target)) {
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

  # ══════════════════════════════════════════════════════════
  # RULE INSPECTOR (sub-tab under Grid Search)
  # ══════════════════════════════════════════════════════════

  # Resolve which rule is currently selected in the inspector
  inspector_current_rule <- reactive({
    r <- rv$last_search
    if (is.null(r)) return(NULL)
    lb <- sort_leaderboard(r$leaderboard, sort_by = input$sort_by,
                            min_picks = input$min_picks, dedup = input$dedup_lb)
    rank <- as.integer(input$inspector_rule_pick %||% "1")
    if (nrow(lb) < rank) return(NULL)
    rule_id <- lb$rule_id[rank]
    grid <- expand_grid_spec(r$grid_spec)
    row_to_rule(grid[rule_id, , drop = FALSE], r$clause_meta)
  })

  # Rebuild feature matrix + inspector df for the current rule
  inspector_data <- reactive({
    rule <- inspector_current_rule()
    if (is.null(rule)) return(NULL)
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) return(NULL)
    market <- rv$markets[[input$market_pick]]
    if (is.null(market)) return(NULL)
    # We need the feature matrix used at grid-search time. Rebuild it on the
    # same pool with the same features so clause values line up.
    feats_needed <- unique(unlist(lapply(market$grid_spec, function(cl)
      c(cl$feature, cl$feature2 %||% NA))))
    feats_needed <- feats_needed[!is.na(feats_needed) & feats_needed != ""]
    feat_df <- build_feature_matrix(pool$tune, feats_needed,
                                     default_window = input$default_window,
                                     main_only = effective_main_only())
    df <- build_inspector_df(rule, pool$tune, feat_df, market$grade_fn,
                              n_form = 5, n_h2h = 5)
    list(df = df, rule = rule, rule_label = rule_label(rule))
  })

  output$inspector_summary <- renderText({
    d <- inspector_data()
    if (is.null(d) || nrow(d$df) == 0) return("No rule selected or no picks to show.")
    n_win <- sum(d$df$outcome == "WIN", na.rm = TRUE)
    n_los <- sum(d$df$outcome == "LOSS", na.rm = TRUE)
    n_unk <- sum(d$df$outcome == "UNKNOWN", na.rm = TRUE)
    paste0(
      "Rule: ", d$rule_label, "\n",
      "Picks: ", nrow(d$df),
      "   Wins: ", n_win,
      "   Losses: ", n_los,
      "   Unknown: ", n_unk,
      "   Hit rate: ", if ((n_win+n_los) > 0)
                        paste0(round(100*n_win/(n_win+n_los),2),"%") else "n/a"
    )
  })

  output$inspector_table <- renderDT({
    d <- inspector_data()
    if (is.null(d) || nrow(d$df) == 0) return(NULL)
    datatable(d$df,
      options = list(pageLength = 25, scrollX = TRUE,
                     scrollY = "600px", scrollCollapse = TRUE),
      filter = "top", rownames = FALSE) |>
      formatStyle("outcome",
        backgroundColor = styleEqual(c("WIN","LOSS","UNKNOWN"),
                                      c("#dcfce7","#fee2e2","#f1f2f4")),
        color = styleEqual(c("WIN","LOSS","UNKNOWN"),
                            c("#166534","#991b1b","#6b7280")),
        fontWeight = "bold")
  })

  output$inspector_download_btn <- downloadHandler(
    filename = function() {
      market_name <- input$market_pick %||% "market"
      rank <- input$inspector_rule_pick %||% "1"
      paste0("inspector_", market_name, "_top", rank, "_",
              format(Sys.time(), "%Y%m%d_%H%M%S"), ".xlsx")
    },
    content = function(file) {
      d <- inspector_data()
      if (is.null(d) || nrow(d$df) == 0) {
        writeLines("No picks to export", sub("\\.xlsx$", ".txt", file)); return()
      }
      write_inspector_xlsx(d$df, file, rule_label = d$rule_label)
    }
  )



  # ══════════════════════════════════════════════════════════
  # STAGE 2 TAB
  # ══════════════════════════════════════════════════════════

  # Populate rule picker from last Stage 1 leaderboard
  observe({
    r <- rv$last_search
    if (is.null(r)) {
      updateSelectInput(session, "s2_rule_pick", choices = c(""), selected = "")
      return()
    }
    lb <- sort_leaderboard(r$leaderboard, sort_by = input$sort_by,
                            min_picks = input$min_picks, dedup = input$dedup_lb)
    if (nrow(lb) == 0) {
      updateSelectInput(session, "s2_rule_pick", choices = c(""), selected = "")
      return()
    }
    # take top 20 rules and give them a compact label with metrics
    top <- head(lb, 20)
    labels <- paste0(top$rule_label,
                      "  |  hit=", round(top$hit_rate * 100, 1),
                      "% picks=", top$picks,
                      " lift=", round(top$baseline_lift * 100, 1), "pp")
    updateSelectInput(session, "s2_rule_pick",
      choices = c("", setNames(as.character(top$rule_id), labels)),
      selected = "")
  })

  output$s2_rule_summary <- renderText({
    if (is.null(rv$last_search) || is.null(input$s2_rule_pick) || input$s2_rule_pick == "")
      return("Pick a rule from the dropdown.")
    lb <- rv$last_search$leaderboard
    row <- lb[lb$rule_id == as.integer(input$s2_rule_pick), ]
    if (nrow(row) == 0) return("(rule not found)")
    paste0(
      "Rule: ", row$rule_label, "\n",
      "Family: ", row$family_tag, "\n",
      "Stage 1 picks: ", row$picks, " (wins ", row$wins, " / losses ", row$losses, ")\n",
      "Stage 1 hit rate: ", round(row$hit_rate * 100, 2), "%\n",
      "Missed opportunity rate on rejected: ", round(row$missed_opportunity_rate * 100, 1), "%"
    )
  })

  # Category selector — dynamic, based on loaded intuitions
  output$s2_category_selector <- renderUI({
    cats <- if (length(rv$intuitions) > 0)
              unique(vapply(rv$intuitions, function(x) x$category %||% "uncategorized", character(1)))
            else character(0)
    if (length(cats) == 0) {
      return(helpText("No intuitions loaded. Add files to intuitions/ and click Reload."))
    }
    checkboxGroupInput("s2_categories", "Categories to include:",
      choices = cats, selected = cats)
  })

  # Intuition library table
  output$s2_lib_table <- renderDT({
    df <- intuition_catalog_df(rv$intuitions)
    if (nrow(df) == 0) return(NULL)
    datatable(df, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  })
  output$s2_lib_errors <- renderText({
    if (length(rv$intuition_errors) == 0) return("Intuition library: OK")
    paste0("Intuition load errors:\n",
           paste0("  ", names(rv$intuition_errors), ": ", unlist(rv$intuition_errors), collapse = "\n"))
  })

  # Reconstruct concrete rule from its rule_id in last_search
  reconstruct_rule <- function(rule_id) {
    r <- rv$last_search
    if (is.null(r)) return(NULL)
    grid <- expand_grid_spec(r$grid_spec)
    if (rule_id < 1 || rule_id > nrow(grid)) return(NULL)
    row_to_rule(grid[rule_id, , drop = FALSE], r$clause_meta)
  }

  # Run Stage 2
  observeEvent(input$s2_run_btn, {
    if (is.null(rv$last_search)) {
      showNotification("Run Stage 1 first", type = "error"); return()
    }
    if (is.null(input$s2_rule_pick) || input$s2_rule_pick == "") {
      showNotification("Pick a rule", type = "error"); return()
    }
    if (length(rv$intuitions) == 0) {
      showNotification("No intuitions loaded. Add files to intuitions/ folder.", type = "error"); return()
    }
    depths <- integer(0)
    if (isTRUE(input$s2_depth_singles)) depths <- c(depths, 1L)
    if (isTRUE(input$s2_depth_pairs))   depths <- c(depths, 2L)
    if (isTRUE(input$s2_depth_triples)) depths <- c(depths, 3L)
    if (length(depths) == 0) {
      showNotification("Pick at least one combination depth", type = "error"); return()
    }

    market <- rv$markets[[input$market_pick]]
    if (is.null(market)) { showNotification("Market not set", type = "error"); return() }
    rule <- reconstruct_rule(as.integer(input$s2_rule_pick))
    if (is.null(rule)) { showNotification("Could not reconstruct rule", type = "error"); return() }

    # Filter intuitions by category selection AND market status
    intu_active <- active_intuitions_for_market(rv$intuitions, market$name)
    cats <- input$s2_categories %||% character(0)
    if (length(cats) > 0) {
      intu_active <- Filter(function(x) (x$category %||% "uncategorized") %in% cats, intu_active)
    }
    if (length(intu_active) == 0) {
      showNotification("No intuitions match the category / status filter", type = "error"); return()
    }

    # Rebuild the feature matrix — need Stage 1 features + every feature
    # any intuition might touch. Cheapest safe path: build over the union of
    # (Stage 1 features) + (all features present in FEATURE_CATALOG that the
    # intuitions might reference). Without static analysis of filter_fn bodies,
    # we build over ALL catalog features on the Tune pool. This is the same
    # matrix Stage 1 sat on, so build once per Stage 2 run.
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) {
      showNotification("Load files first", type = "error"); return()
    }
    all_feats <- names(FEATURE_CATALOG)
    withProgress(message = "Stage 2", value = 0.05, {
      incProgress(0.2, detail = "Building feature matrix (all features)...")
      feat_df <- build_feature_matrix(pool$tune, all_feats,
                                       default_window = input$default_window,
                                       main_only = effective_main_only())
      incProgress(0.4, detail = "Sweeping intuition combos...")
      res <- run_stage2(
        stage1_rule = rule, feat_df = feat_df,
        grade_fn = market$grade_fn, intuitions = intu_active,
        depths = depths, min_picks = input$s2_min_picks,
        current_round = input$current_round
      )
      incProgress(0.4, detail = "Done")
      rv$stage2_result <- res
      # Also seed the pipeline for the Validation tab
      # Tune metrics come from Stage 1's own leaderboard row (we already know them)
      tune_m <- list(
        picks = rv$last_search$leaderboard$picks[rv$last_search$leaderboard$rule_id == as.integer(input$s2_rule_pick)],
        wins  = rv$last_search$leaderboard$wins[rv$last_search$leaderboard$rule_id == as.integer(input$s2_rule_pick)],
        losses = rv$last_search$leaderboard$losses[rv$last_search$leaderboard$rule_id == as.integer(input$s2_rule_pick)],
        hit_rate = rv$last_search$leaderboard$hit_rate[rv$last_search$leaderboard$rule_id == as.integer(input$s2_rule_pick)]
      )
      p <- new_pipeline(market_name = market$name, stage1_rule = rule)
      p$tune_metrics <- tune_m
      rv$pipeline <- p
    })
    showNotification("Stage 2 done. Pipeline seeded for Validation.", type = "message")
  })

  output$s2_summary <- renderText({
    r <- rv$stage2_result
    if (is.null(r)) return("Run Stage 2 to see results.")
    paste(
      paste0("Stage 1 picks: ", r$n_picked,
             "  baseline hit rate on picks: ",
             round((r$stage1_picks_baseline %||% NA) * 100, 2), "%"),
      paste0("Candidates expanded: ", r$n_candidates_expanded,
             " (across depths ", paste(r$depths_swept, collapse = ","), ")"),
      sep = "\n"
    )
  })

  render_stage2_lb <- function(lb) {
    if (is.null(lb) || nrow(lb) == 0) {
      return(datatable(data.frame(Message = "No combos meet the floor"),
                       options = list(dom = "t"), rownames = FALSE))
    }
    lb2 <- sort_stage2(lb, sort_by = input$s2_sort_by,
                        min_picks = input$s2_min_picks, dedup = TRUE)
    if (nrow(lb2) == 0) {
      return(datatable(data.frame(Message = "No combos meet the floor after dedup"),
                       options = list(dom = "t"), rownames = FALSE))
    }
    show_cols <- setdiff(names(lb2), c("fingerprint","dedup_group"))
    disp <- lb2[, show_cols, drop = FALSE]
    num_cols <- sapply(disp, is.numeric)
    disp[num_cols] <- lapply(disp[num_cols], function(x) round(x, 3))
    datatable(disp, options = list(pageLength = 15, scrollX = TRUE), rownames = FALSE)
  }

  output$s2_refine_lb <- renderDT({ render_stage2_lb(rv$stage2_result$refine) })

  # ══════════════════════════════════════════════════════════
  # VALIDATION TAB
  # ══════════════════════════════════════════════════════════

  output$val_pipeline_summary <- renderText({
    p <- rv$pipeline
    if (is.null(p)) return("No pipeline yet. Run Stage 2 to seed one.")
    lines <- c(
      paste0("Market: ", p$market_name),
      paste0("State: ", p$state),
      paste0("Rule: ", rule_label(p$stage1_rule))
    )
    if (!is.null(p$tune_metrics))
      lines <- c(lines, paste0("Tune hit rate: ",
                                round((p$tune_metrics$hit_rate %||% NA) * 100, 2), "%"))
    if (!is.null(p$validate_metrics))
      lines <- c(lines, paste0("Validate hit rate: ",
                                round((p$validate_metrics$hit_rate %||% NA) * 100, 2), "%"))
    if (!is.null(p$test_metrics))
      lines <- c(lines, paste0("Test hit rate: ",
                                round((p$test_metrics$hit_rate %||% NA) * 100, 2), "%"))
    paste(lines, collapse = "\n")
  })

  # Run Validate
  observeEvent(input$val_run_validate_btn, {
    p <- rv$pipeline
    if (is.null(p)) {
      showNotification("No pipeline. Run Stage 2 first.", type = "error"); return()
    }
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$validate) == 0) {
      showNotification("No fixtures tagged 'Validate'. In multi-file mode, tag at least one file as Validate.",
                       type = "error"); return()
    }
    market <- rv$markets[[p$market_name]]
    if (is.null(market)) { showNotification("Market not loaded", type = "error"); return() }

    withProgress(message = "Validating", value = 0.2, {
      feat_df <- build_feature_matrix(pool$validate, names(FEATURE_CATALOG),
                                       default_window = input$default_window,
                                       main_only = effective_main_only())
      rv$validate_feat_df <- feat_df
      res <- validate_pipeline(p, feat_df, market$grade_fn,
                                tolerance = input$val_tolerance)
    })
    if (!res$ok) {
      showNotification(res$msg, type = "error"); return()
    }
    rv$pipeline <- res$pipeline
    if (isTRUE(res$collapsed)) {
      showNotification(res$msg, type = "warning", duration = 10)
    } else {
      showNotification(paste0(res$msg,
                              " Tune=", round((res$tune_hit_rate %||% NA) * 100, 2), "%",
                              " Validate=", round((res$validate_hit_rate %||% NA) * 100, 2), "%"),
                       type = "message")
    }
  })

  # Commit
  observeEvent(input$val_commit_btn, {
    p <- rv$pipeline
    if (is.null(p)) { showNotification("No pipeline", type = "error"); return() }
    res <- commit_pipeline(p, force = isTRUE(input$val_force_commit))
    if (!res$ok) { showNotification(res$msg, type = "error"); return() }
    rv$pipeline <- res$pipeline
    showNotification(res$msg, type = "message")
  })

  # Test gate UI — dynamic based on pipeline state
  output$val_test_gate_ui <- renderUI({
    p <- rv$pipeline
    if (is.null(p)) {
      return(helpText("No pipeline."))
    }
    if (p$state == "COMMITTED") {
      tagList(
        p(strong("Rule committed. Test unlocked."), style = "color: #155724;"),
        actionButton("val_run_test_btn", "Run Test (one-shot, terminal)",
                     class = "btn-warning", style = "width: 100%")
      )
    } else if (p$state == "TESTED") {
      m <- p$test_metrics
      tagList(
        p(strong("Rule already tested (terminal)."), style = "color: #721c24;"),
        verbatimTextOutput("val_test_result_static")
      )
    } else {
      tagList(
        p(strong(paste0("Test blocked. State = ", p$state)), style = "color: #721c24;"),
        helpText("Run Validate, then Commit, to unlock Test.")
      )
    }
  })

  output$val_test_result_static <- renderText({
    p <- rv$pipeline
    if (is.null(p) || is.null(p$test_metrics)) return("")
    m <- p$test_metrics
    paste0("picks=", m$picks, " wins=", m$wins, " losses=", m$losses,
           " hit_rate=", round(m$hit_rate * 100, 2), "%")
  })

  # Run Test — commit gate enforced inside test_pipeline()
  observeEvent(input$val_run_test_btn, {
    p <- rv$pipeline
    if (is.null(p)) { showNotification("No pipeline", type = "error"); return() }
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$test) == 0) {
      showNotification("No fixtures tagged 'Test'. Tag at least one file as Test.",
                       type = "error"); return()
    }
    market <- rv$markets[[p$market_name]]
    if (is.null(market)) { showNotification("Market not loaded", type = "error"); return() }

    withProgress(message = "Testing (terminal)", value = 0.2, {
      feat_df <- build_feature_matrix(pool$test, names(FEATURE_CATALOG),
                                       default_window = input$default_window,
                                       main_only = effective_main_only())
      rv$test_feat_df <- feat_df
      res <- test_pipeline(p, feat_df, market$grade_fn)
    })
    if (!res$ok) { showNotification(res$msg, type = "error"); return() }
    rv$pipeline <- res$pipeline
    m <- res$test_metrics
    showNotification(paste0("Test complete. hit_rate=",
                             round((m$hit_rate %||% NA) * 100, 2),
                             "% picks=", m$picks,
                             " losses=", m$losses),
                     type = "message", duration = 15)
  })

  # Metrics timeline
  output$val_metrics_table <- renderDT({
    p <- rv$pipeline
    if (is.null(p)) return(NULL)
    rows <- list()
    fmt <- function(m) {
      if (is.null(m)) return(data.frame(picks = NA, wins = NA, losses = NA, hit_rate = NA))
      data.frame(picks = m$picks, wins = m$wins, losses = m$losses,
                  hit_rate = round((m$hit_rate %||% NA) * 100, 2))
    }
    df <- rbind(
      cbind(stage = "Tune",     fmt(p$tune_metrics)),
      cbind(stage = "Validate", fmt(p$validate_metrics)),
      cbind(stage = "Test",     fmt(p$test_metrics))
    )
    datatable(df, options = list(dom = "t"), rownames = FALSE)
  })

  # Per-round breakdown (uses the Tune matrix from last Stage 1 run,
  # falling back to Validate/Test if available)
  output$val_per_round_table <- renderDT({
    p <- rv$pipeline
    if (is.null(p)) return(NULL)
    # Prefer Validate feat_df (has source_file), else Tune-rebuilt
    feat_df <- rv$validate_feat_df
    if (is.null(feat_df)) {
      pool <- build_fixtures_pool()
      if (is.null(pool) || length(pool$tune) == 0) return(NULL)
      feat_df <- build_feature_matrix(pool$tune, names(FEATURE_CATALOG),
                                       default_window = input$default_window,
                                       main_only = effective_main_only())
    }
    market <- rv$markets[[p$market_name]]
    if (is.null(market)) return(NULL)
    df <- per_round_breakdown(p, feat_df, market$grade_fn)
    if (nrow(df) == 0) return(NULL)
    df$hit_rate <- round(df$hit_rate * 100, 2)
    datatable(df, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  output$val_stability_summary <- renderText({
    p <- rv$pipeline
    if (is.null(p)) return("(no pipeline)")
    feat_df <- rv$validate_feat_df
    if (is.null(feat_df)) {
      pool <- build_fixtures_pool()
      if (is.null(pool) || length(pool$tune) == 0) return("(no data)")
      feat_df <- build_feature_matrix(pool$tune, names(FEATURE_CATALOG),
                                       default_window = input$default_window,
                                       main_only = effective_main_only())
    }
    market <- rv$markets[[p$market_name]]
    if (is.null(market)) return("")
    pr <- per_round_breakdown(p, feat_df, market$grade_fn)
    stab <- stability_summary(pr)
    stab$msg
  })

  # ══════════════════════════════════════════════════════════
  # DRIFT MONITOR TAB
  # ══════════════════════════════════════════════════════════

  refresh_drift <- reactive({
    invalidateLater(0)
    rv$drift_action_status  # tie to trigger reactivity
    build_drift_dashboard(DRIFT_DIR)
  })

  output$drift_summary_out <- renderText({
    df <- build_drift_dashboard(DRIFT_DIR)
    if (nrow(df) == 0) return("No rules registered yet.")
    n_alert <- sum(df$in_alert, na.rm = TRUE)
    n_active <- sum(df$status == "active", na.rm = TRUE)
    n_retired <- sum(df$status == "retired", na.rm = TRUE)
    paste0("Rules: ", nrow(df),
           "  |  active: ", n_active,
           "  |  in alert: ", n_alert,
           "  |  retired: ", n_retired)
  })

  output$drift_dashboard <- renderDT({
    rv$drift_action_status
    df <- build_drift_dashboard(DRIFT_DIR)
    if (nrow(df) == 0) return(NULL)
    datatable(df, options = list(pageLength = 15, scrollX = TRUE),
              rownames = FALSE,
              selection = list(mode = "single", target = "row")) |>
      formatStyle("status",
        backgroundColor = styleEqual(
          c("active","alert","retired"),
          c("#d4edda","#f8d7da","#e2e3e5"))) |>
      formatStyle("in_alert",
        backgroundColor = styleEqual(c(TRUE, FALSE), c("#f8d7da", "")))
  })

  observe({
    rv$drift_action_status
    df <- build_drift_dashboard(DRIFT_DIR)
    updateSelectInput(session, "drift_selected_rule",
      choices = c("", df$rule_id), selected = input$drift_selected_rule)
    updateSelectInput(session, "deploy_registry_pick",
      choices = c("", df$rule_id), selected = input$deploy_registry_pick)
  })

  output$drift_history_table <- renderDT({
    rv$drift_action_status
    if (is.null(input$drift_selected_rule) || input$drift_selected_rule == "") return(NULL)
    hist <- load_history(input$drift_selected_rule, DRIFT_DIR)
    if (nrow(hist) == 0)
      return(datatable(data.frame(Message = "No history logged yet"),
                        options = list(dom = "t"), rownames = FALSE))
    disp <- hist
    disp$hit_rate <- round(disp$hit_rate * 100, 2)
    datatable(disp, options = list(pageLength = 10, scrollX = TRUE), rownames = FALSE)
  })

  # Register the current pipeline
  observeEvent(input$drift_register_btn, {
    p <- rv$pipeline
    if (is.null(p)) {
      showNotification("No pipeline. Run Stage 2 first.", type = "error"); return()
    }
    res <- register_rule(p, DRIFT_DIR,
      alert_hit_rate_drop_pp = input$drift_hit_drop,
      alert_volume_drop_pct  = input$drift_vol_drop,
      alert_loss_ceiling     = input$drift_loss_ceiling,
      rolling_window         = input$drift_window)
    if (!res$ok) {
      showNotification(res$msg, type = "error"); return()
    }
    rv$drift_action_status <- paste0("Registered: ", res$rule_id,
                                      " at ", format(Sys.time()))
    showNotification(paste0("Registered rule: ", res$rule_id), type = "message")
  })

  # Log latest round: grades every registered ACTIVE rule against the
  # current Tune pool (interpreting it as "the fresh round of data").
  observeEvent(input$drift_log_btn, {
    pool <- build_fixtures_pool()
    if (is.null(pool) || length(pool$tune) == 0) {
      showNotification("Load files first (the Tune pool is used as the fresh round).",
                       type = "error"); return()
    }
    registry <- load_registry(DRIFT_DIR)
    active_ids <- names(registry)[vapply(registry, function(e) e$status != "retired", logical(1))]
    if (length(active_ids) == 0) {
      showNotification("No active rules registered.", type = "warning"); return()
    }
    withProgress(message = "Logging round", value = 0.2, {
      feat_df <- build_feature_matrix(pool$tune, names(FEATURE_CATALOG),
                                       default_window = input$default_window,
                                       main_only = FALSE)
      round_lbl <- input$drift_round_label
      if (is.null(round_lbl) || round_lbl == "") round_lbl <- format(Sys.Date(), "R%Y%m%d")
      results <- lapply(active_ids, function(rid) {
        log_round_from_pipeline(rid, round_lbl, feat_df, rv$markets, DRIFT_DIR)
      })
    })
    n_alert <- sum(vapply(results, function(r)
      isTRUE(r$ok) && isTRUE(r$alert$in_alert), logical(1)))
    rv$drift_action_status <- paste0("Logged '", input$drift_round_label,
                                      "' for ", length(active_ids), " rule(s). ",
                                      "In alert: ", n_alert, ".")
    showNotification(rv$drift_action_status, type = "message", duration = 10)
  })

  observeEvent(input$drift_retire_btn, {
    if (is.null(input$drift_selected_rule) || input$drift_selected_rule == "") return()
    retire_rule(input$drift_selected_rule, DRIFT_DIR)
    rv$drift_action_status <- paste0("Retired: ", input$drift_selected_rule)
  })
  observeEvent(input$drift_reactivate_btn, {
    if (is.null(input$drift_selected_rule) || input$drift_selected_rule == "") return()
    reactivate_rule(input$drift_selected_rule, DRIFT_DIR)
    rv$drift_action_status <- paste0("Reactivated: ", input$drift_selected_rule)
  })
  observeEvent(input$drift_delete_btn, {
    if (is.null(input$drift_selected_rule) || input$drift_selected_rule == "") return()
    delete_rule(input$drift_selected_rule, DRIFT_DIR)
    rv$drift_action_status <- paste0("Deleted: ", input$drift_selected_rule)
  })
  output$drift_action_status <- renderText({ rv$drift_action_status })

  # ══════════════════════════════════════════════════════════
  # DEPLOY TAB
  # ══════════════════════════════════════════════════════════

  # Build a pipeline-like object from whichever source is selected
  deploy_source_pipeline <- reactive({
    if (input$deploy_source == "pipeline") return(rv$pipeline)
    # registry
    rid <- input$deploy_registry_pick
    if (is.null(rid) || rid == "") return(NULL)
    registry <- load_registry(DRIFT_DIR)
    entry <- registry[[rid]]
    if (is.null(entry)) return(NULL)
    # Rehydrate a pipeline-shaped object for the emitter
    list(
      market_name  = entry$market_name,
      stage1_rule  = entry$stage1_rule,
      stage2_combo = entry$stage2_combo,
      tune_metrics     = list(picks = entry$baseline_picks, wins = NA, losses = NA,
                              hit_rate = entry$baseline_hit_rate),
      validate_metrics = list(picks = NA, wins = NA, losses = NA,
                              hit_rate = entry$validate_hit_rate),
      test_metrics     = list(picks = NA, wins = NA, losses = NA,
                              hit_rate = entry$test_hit_rate)
    )
  })

  output$deploy_emitted_block <- renderText({
    p <- deploy_source_pipeline()
    if (is.null(p)) return("Select a source (pipeline or registered rule).")
    paste(emit_full_block(p), collapse = "\n")
  })

  observeEvent(input$deploy_preview_btn, {
    p <- deploy_source_pipeline()
    if (is.null(p)) {
      showNotification("No rule source selected", type = "error"); return()
    }
    tgt <- input$deploy_target_file
    if (is.null(tgt) || tgt == "") {
      showNotification("Target file path required", type = "error"); return()
    }
    if (!dir.exists(dirname(tgt))) dir.create(dirname(tgt), recursive = TRUE)
    rv$deploy_preview <- build_proposed_content(p, tgt)
    rv$deploy_action_status <- paste0("Preview ready. ", rv$deploy_preview$summary)
  })

  output$deploy_diff_stats <- renderText({
    if (is.null(rv$deploy_preview)) return("Click 'Generate & preview diff' to build a proposal.")
    stats <- diff_stats(rv$deploy_preview$old_lines, rv$deploy_preview$new_lines)
    paste0("Action: ", rv$deploy_preview$action, "\n",
           "Old lines: ", stats$old_line_count, "\n",
           "New lines: ", stats$new_line_count, "\n",
           "Lines added:   ", stats$lines_added, "\n",
           "Lines removed: ", stats$lines_removed, "\n",
           "\n", rv$deploy_preview$summary)
  })

  output$deploy_old_content <- renderText({
    if (is.null(rv$deploy_preview)) return("")
    paste(rv$deploy_preview$old_lines, collapse = "\n")
  })
  output$deploy_new_content <- renderText({
    if (is.null(rv$deploy_preview)) return("")
    paste(rv$deploy_preview$new_lines, collapse = "\n")
  })

  observeEvent(input$deploy_apply_btn, {
    if (is.null(rv$deploy_preview)) {
      showNotification("Generate a preview first", type = "error"); return()
    }
    tgt <- input$deploy_target_file
    res <- apply_push(tgt, rv$deploy_preview$new_lines, backup_dir = dirname(tgt))
    rv$deploy_action_status <- paste0(
      "Applied. Wrote ", res$n_lines_written, " lines to ", res$target_file,
      if (!is.na(res$backup_path)) paste0("\nBackup: ", res$backup_path) else ""
    )
    showNotification(rv$deploy_action_status, type = "message", duration = 15)
  })

  output$deploy_action_status <- renderText({ rv$deploy_action_status })

  output$deploy_download_manual <- downloadHandler(
    filename = function() {
      p <- deploy_source_pipeline()
      market <- if (!is.null(p)) tolower(p$market_name) else "rule"
      paste0("moab_", market, "_emitted_", format(Sys.time(), "%Y%m%d_%H%M%S"), ".R")
    },
    content = function(file) {
      p <- deploy_source_pipeline()
      if (is.null(p)) {
        writeLines("# No pipeline available", file); return()
      }
      writeLines(emit_full_block(p), file)
    }
  )

}

updateTextAreaInput_safe <- function(session, id, value) {
  session$sendInputMessage(id, list(value = value))
}

shinyApp(ui, server)

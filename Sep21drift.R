# ============================================================
# engine/drift.R
# Session 4 — drift detection and monitoring.
#
# What this file provides:
#   - A persistent registry of DEPLOYED rules (survives app restarts)
#   - Per-round performance logging when new data arrives
#   - Rolling-window aggregation across last N rounds
#   - Alert evaluation against configurable thresholds
#   - Retirement / re-tuning bookkeeping
#
# Storage layout (under drift_dir/):
#   registry.rds          — list of registered rules with baselines + alert config
#   history/<rule_id>.rds — per-round performance rows for that rule
#
# Each rule_id is a stable slug (market_name + "_" + timestamp) so
# multiple rules can coexist per market.
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Registry paths ──────────────────────────────────────────
drift_paths <- function(drift_dir) {
  list(
    registry_file = file.path(drift_dir, "registry.rds"),
    history_dir   = file.path(drift_dir, "history")
  )
}

ensure_drift_dirs <- function(drift_dir) {
  p <- drift_paths(drift_dir)
  if (!dir.exists(drift_dir)) dir.create(drift_dir, recursive = TRUE)
  if (!dir.exists(p$history_dir)) dir.create(p$history_dir, recursive = TRUE)
  p
}

# ─── Registry read/write ─────────────────────────────────────
load_registry <- function(drift_dir) {
  p <- ensure_drift_dirs(drift_dir)
  if (!file.exists(p$registry_file)) return(list())
  tryCatch(readRDS(p$registry_file), error = function(e) list())
}

save_registry <- function(registry, drift_dir) {
  p <- ensure_drift_dirs(drift_dir)
  saveRDS(registry, p$registry_file)
  invisible(TRUE)
}

# ─── Register a new rule from a committed pipeline ───────────
# Called after test_pipeline() succeeds — takes the Committed pipeline
# and adds it to the registry. Baseline = the TUNE metrics (that's what
# drift is measured against, per Section 13 of the plan).
register_rule <- function(pipeline, drift_dir,
                          alert_hit_rate_drop_pp = 0.10,
                          alert_volume_drop_pct  = 0.30,
                          alert_loss_ceiling     = 4,
                          rolling_window         = 3) {
  if (is.null(pipeline$tune_metrics)) {
    return(list(ok = FALSE, msg = "Pipeline has no tune_metrics"))
  }
  rule_id <- paste0(pipeline$market_name, "_",
                     format(Sys.time(), "%Y%m%d_%H%M%S"))

  entry <- list(
    rule_id       = rule_id,
    market_name   = pipeline$market_name,
    stage1_rule   = pipeline$stage1_rule,
    stage2_combo  = pipeline$stage2_combo,
    tuned_at      = pipeline$committed_at %||% Sys.time(),
    baseline_hit_rate = pipeline$tune_metrics$hit_rate,
    baseline_picks    = pipeline$tune_metrics$picks,
    validate_hit_rate = pipeline$validate_metrics$hit_rate %||% NA_real_,
    test_hit_rate     = pipeline$test_metrics$hit_rate %||% NA_real_,
    status = "active",   # active | alert | retired
    retired_at = NA,
    alert_config = list(
      hit_rate_drop_pp = alert_hit_rate_drop_pp,
      volume_drop_pct  = alert_volume_drop_pct,
      loss_ceiling     = alert_loss_ceiling,
      rolling_window   = rolling_window
    )
  )
  registry <- load_registry(drift_dir)
  registry[[rule_id]] <- entry
  save_registry(registry, drift_dir)
  list(ok = TRUE, rule_id = rule_id, entry = entry)
}

# ─── History I/O ─────────────────────────────────────────────
load_history <- function(rule_id, drift_dir) {
  p <- ensure_drift_dirs(drift_dir)
  f <- file.path(p$history_dir, paste0(rule_id, ".rds"))
  if (!file.exists(f)) return(data.frame(
    round_label = character(0),
    graded_at   = as.POSIXct(character(0)),
    picks       = integer(0),
    wins        = integer(0),
    losses      = integer(0),
    hit_rate    = numeric(0),
    source_file = character(0),
    stringsAsFactors = FALSE
  ))
  tryCatch(readRDS(f), error = function(e) data.frame())
}

save_history <- function(rule_id, hist_df, drift_dir) {
  p <- ensure_drift_dirs(drift_dir)
  f <- file.path(p$history_dir, paste0(rule_id, ".rds"))
  saveRDS(hist_df, f)
}

# ─── Log a rule's performance on a round ─────────────────────
# Given a rule_id, a feat_df for a specific round (feat_df must carry
# a source_file column tagging which file it came from) and a grade_fn,
# grade the rule on that round and append a history row.
log_round_performance <- function(rule_id, round_label, feat_df, drift_dir) {
  registry <- load_registry(drift_dir)
  entry <- registry[[rule_id]]
  if (is.null(entry)) return(list(ok = FALSE, msg = "Rule not in registry"))

  # Need the market's grade_fn — the caller supplies it via pipeline machinery
  # (see log_round_from_pipeline below).
  list(ok = FALSE, msg = "Use log_round_from_pipeline() instead")
}

# ─── Log performance for a rule using its market's grade_fn ──
# markets_lookup: named list of market objects (as loaded by load_all_markets)
log_round_from_pipeline <- function(rule_id, round_label, feat_df,
                                     markets_lookup, drift_dir) {
  registry <- load_registry(drift_dir)
  entry <- registry[[rule_id]]
  if (is.null(entry)) return(list(ok = FALSE, msg = "Rule not in registry"))
  market <- markets_lookup[[entry$market_name]]
  if (is.null(market)) return(list(ok = FALSE, msg = paste0(
    "Market '", entry$market_name, "' not loaded")))

  # Rebuild a lightweight pipeline just for grading
  fake_pipeline <- list(
    stage1_rule  = entry$stage1_rule,
    stage2_combo = entry$stage2_combo,
    state        = "COMMITTED"
  )
  m <- grade_full_pipeline(fake_pipeline, feat_df, market$grade_fn)

  hist_df <- load_history(rule_id, drift_dir)
  # Idempotent: if this round_label already exists, replace it (support
  # re-grading after scraping fixes)
  hist_df <- hist_df[hist_df$round_label != round_label, , drop = FALSE]
  new_row <- data.frame(
    round_label = round_label,
    graded_at   = Sys.time(),
    picks       = m$picks,
    wins        = m$wins,
    losses      = m$losses,
    hit_rate    = m$hit_rate %||% NA_real_,
    source_file = if (!is.null(feat_df$source_file)) feat_df$source_file[1] else NA_character_,
    stringsAsFactors = FALSE
  )
  hist_df <- rbind(hist_df, new_row)
  hist_df <- hist_df[order(hist_df$round_label), , drop = FALSE]
  save_history(rule_id, hist_df, drift_dir)

  # Re-evaluate alert state
  alert <- evaluate_alerts(entry, hist_df)
  entry$status <- if (alert$in_alert) "alert" else entry$status
  registry[[rule_id]] <- entry
  save_registry(registry, drift_dir)
  list(ok = TRUE, metrics = m, alert = alert, hist_df = hist_df)
}

# ─── Alert evaluation ────────────────────────────────────────
# Returns list(in_alert, reasons)
evaluate_alerts <- function(entry, hist_df) {
  if (is.null(entry) || nrow(hist_df) == 0) {
    return(list(in_alert = FALSE, reasons = character(0)))
  }
  cfg <- entry$alert_config %||% list()
  window <- cfg$rolling_window %||% 3L
  reasons <- character(0)

  # Rolling window
  recent <- tail(hist_df, window)

  # 1. Hit-rate drop
  if (nrow(recent) > 0) {
    total_picks <- sum(recent$picks, na.rm = TRUE)
    total_wins  <- sum(recent$wins,  na.rm = TRUE)
    if (total_picks > 0) {
      recent_hr <- total_wins / total_picks
      drop <- entry$baseline_hit_rate - recent_hr
      if (!is.na(drop) && drop > (cfg$hit_rate_drop_pp %||% 0.10)) {
        reasons <- c(reasons, paste0(
          "Hit-rate dropped ", round(drop * 100, 1), "pp over last ",
          nrow(recent), " rounds (", round(recent_hr * 100, 1), "% vs baseline ",
          round(entry$baseline_hit_rate * 100, 1), "%)"))
      }
    }
  }

  # 2. Volume drop
  # baseline_picks is the total from the tune set, which may span many rounds.
  # Estimate per-round expectation as baseline_picks / (assumed rounds in tune).
  # Without knowing that count, use median historical volume as proxy.
  if (nrow(hist_df) >= 2) {
    median_vol <- median(hist_df$picks, na.rm = TRUE)
    latest_vol <- tail(recent$picks, 1)
    if (!is.na(median_vol) && median_vol > 0 && !is.na(latest_vol)) {
      pct_drop <- 1 - (latest_vol / median_vol)
      if (pct_drop > (cfg$volume_drop_pct %||% 0.30)) {
        reasons <- c(reasons, paste0(
          "Volume dropped ", round(pct_drop * 100, 0), "% in latest round (",
          latest_vol, " vs median ", median_vol, ")"))
      }
    }
  }

  # 3. Loss ceiling in most recent round
  latest <- tail(hist_df, 1)
  if (nrow(latest) > 0 && !is.na(latest$losses) &&
      latest$losses > (cfg$loss_ceiling %||% 4)) {
    reasons <- c(reasons, paste0(
      "Loss ceiling breached: ", latest$losses, " losses in round '",
      latest$round_label, "' (ceiling ", cfg$loss_ceiling %||% 4, ")"))
  }

  list(in_alert = length(reasons) > 0, reasons = reasons)
}

# ─── Retire a rule ───────────────────────────────────────────
retire_rule <- function(rule_id, drift_dir) {
  registry <- load_registry(drift_dir)
  if (is.null(registry[[rule_id]])) return(list(ok = FALSE, msg = "Not found"))
  registry[[rule_id]]$status <- "retired"
  registry[[rule_id]]$retired_at <- Sys.time()
  save_registry(registry, drift_dir)
  list(ok = TRUE)
}

# ─── Reactivate a rule (e.g. after re-tuning outside the emitter) ─
reactivate_rule <- function(rule_id, drift_dir) {
  registry <- load_registry(drift_dir)
  if (is.null(registry[[rule_id]])) return(list(ok = FALSE, msg = "Not found"))
  registry[[rule_id]]$status <- "active"
  registry[[rule_id]]$retired_at <- NA
  save_registry(registry, drift_dir)
  list(ok = TRUE)
}

# ─── Delete a rule + its history ─────────────────────────────
delete_rule <- function(rule_id, drift_dir) {
  registry <- load_registry(drift_dir)
  registry[[rule_id]] <- NULL
  save_registry(registry, drift_dir)
  p <- ensure_drift_dirs(drift_dir)
  hist_file <- file.path(p$history_dir, paste0(rule_id, ".rds"))
  if (file.exists(hist_file)) file.remove(hist_file)
  list(ok = TRUE)
}

# ─── Dashboard view: one row per registered rule ────────────
# Returns a data.frame ready for DT rendering.
build_drift_dashboard <- function(drift_dir) {
  registry <- load_registry(drift_dir)
  if (length(registry) == 0) {
    return(data.frame(
      rule_id = character(0), market_name = character(0),
      status = character(0),
      baseline_hit_rate = numeric(0), rolling_hit_rate = numeric(0),
      drift_delta_pp = numeric(0),
      total_picks = integer(0), total_rounds = integer(0),
      in_alert = logical(0), alert_reasons = character(0),
      stringsAsFactors = FALSE
    ))
  }
  rows <- lapply(registry, function(entry) {
    hist <- load_history(entry$rule_id, drift_dir)
    window <- entry$alert_config$rolling_window %||% 3L
    recent <- tail(hist, window)
    rolling_hr <- if (nrow(recent) > 0 && sum(recent$picks, na.rm = TRUE) > 0)
                    sum(recent$wins, na.rm = TRUE) / sum(recent$picks, na.rm = TRUE)
                  else NA_real_
    delta_pp <- if (!is.na(rolling_hr))
                  (rolling_hr - entry$baseline_hit_rate) * 100
                else NA_real_
    alert <- evaluate_alerts(entry, hist)
    data.frame(
      rule_id           = entry$rule_id,
      market_name       = entry$market_name,
      status            = if (alert$in_alert) "alert" else entry$status,
      baseline_hit_rate = round(entry$baseline_hit_rate * 100, 2),
      rolling_hit_rate  = if (is.na(rolling_hr)) NA else round(rolling_hr * 100, 2),
      drift_delta_pp    = if (is.na(delta_pp))   NA else round(delta_pp, 2),
      total_picks       = sum(hist$picks, na.rm = TRUE),
      total_rounds      = nrow(hist),
      in_alert          = alert$in_alert,
      alert_reasons     = paste(alert$reasons, collapse = " | "),
      stringsAsFactors  = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df[order(-df$in_alert, df$market_name), , drop = FALSE]
}

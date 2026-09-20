# ============================================================
# engine/validation.R
# Session 3 — 3-way split enforcement + per-round breakdown.
#
# Workflow gates a rule through three states:
#
#   TUNED        Stage 1 + Stage 2 done on Tune set. Rule exists but
#                has not been checked on any hold-out.
#   VALIDATED    Rule was re-graded on Validate set and did not
#                collapse (hit-rate drop below configured tolerance).
#                Only VALIDATED rules can proceed to Test.
#   COMMITTED    User explicitly locked the rule in. Only COMMITTED
#                rules unlock the Test tab.
#   TESTED       Rule was graded on Test. Terminal — any further
#                tweak requires the rule be re-tuned from scratch
#                (with a warning that continuing constitutes leakage).
#
# The Shiny layer reads these flags to enable/disable UI controls.
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Build the pipeline object that carries a rule through gates ─
# Every rule under test lives in this shape.
new_pipeline <- function(market_name, stage1_rule, stage2_combo = NULL) {
  list(
    market_name  = market_name,
    stage1_rule  = stage1_rule,        # list of clauses
    stage2_combo = stage2_combo,       # list of candidate filters, or NULL
    state        = "TUNED",
    tune_metrics     = NULL,
    validate_metrics = NULL,
    test_metrics     = NULL,
    committed_at     = NA,
    tested_at        = NA,
    notes            = ""
  )
}

# ─── Grade a rule (Stage 1 clauses + optional Stage 2 combo) on a feat_df ─
# Returns list(picks, wins, losses, hit_rate, undecidable)
grade_full_pipeline <- function(pipeline, feat_df, grade_fn) {
  passes <- apply_rule(feat_df, pipeline$stage1_rule)
  used_cols <- rule_features(pipeline$stage1_rule)
  sample_mask <- if (length(used_cols) == 0) rep(TRUE, nrow(feat_df))
                 else Reduce(`&`, lapply(used_cols, function(c) !is.na(feat_df[[c]])))

  # If Stage 2 combo present, AND the Stage 2 KEEP mask onto Stage 1 picks.
  if (!is.null(pipeline$stage2_combo) && length(pipeline$stage2_combo) > 0) {
    n <- nrow(feat_df)
    keep_matrix <- vapply(pipeline$stage2_combo, function(cand) {
      apply_candidate(cand, feat_df)
    }, logical(n))
    if (!is.matrix(keep_matrix)) keep_matrix <- matrix(keep_matrix, ncol = 1)
    any_false <- apply(keep_matrix, 1, function(r) isTRUE(any(r == FALSE, na.rm = TRUE)))
    all_true  <- apply(keep_matrix, 1, function(r) all(r == TRUE, na.rm = FALSE) & !any(is.na(r)))
    stage2_keep <- all_true
    stage2_undecidable <- !any_false & !all_true
  } else {
    stage2_keep <- rep(TRUE, nrow(feat_df))
    stage2_undecidable <- rep(FALSE, nrow(feat_df))
  }

  grades <- vapply(seq_len(nrow(feat_df)), function(i) {
    g <- grade_fn(feat_df[i, , drop = FALSE])
    if (is.null(g) || length(g) == 0) return(NA)
    if (is.na(g)) return(NA)
    isTRUE(g)
  }, logical(1))

  picked <- passes & sample_mask & stage2_keep & !is.na(grades)
  picks  <- sum(picked)
  wins   <- sum(grades[picked])
  list(
    picks       = as.integer(picks),
    wins        = as.integer(wins),
    losses      = as.integer(picks - wins),
    hit_rate    = if (picks > 0) wins / picks else NA_real_,
    undecidable = sum(passes & sample_mask & stage2_undecidable),
    picked_row_ids = which(picked)
  )
}

# ─── Validate step: re-grade on Validate set ─────────────────
# tolerance: max acceptable hit-rate drop from Tune to Validate.
#            Default 0.10 (10 percentage points). Above that the rule
#            is considered "collapsed" — user is warned but can override.
validate_pipeline <- function(pipeline, validate_feat_df, grade_fn,
                              tolerance = 0.10) {
  if (pipeline$state == "TESTED") {
    return(list(ok = FALSE,
                msg = "Rule already tested. Cannot re-validate without re-tuning."))
  }
  if (nrow(validate_feat_df) == 0) {
    return(list(ok = FALSE, msg = "Validate set is empty. Load a file tagged 'Validate'."))
  }
  m <- grade_full_pipeline(pipeline, validate_feat_df, grade_fn)
  tune_hr <- pipeline$tune_metrics$hit_rate %||% NA_real_
  collapsed <- FALSE
  if (!is.na(tune_hr) && !is.na(m$hit_rate)) {
    collapsed <- (tune_hr - m$hit_rate) > tolerance
  }
  pipeline$validate_metrics <- m
  pipeline$state <- if (collapsed) "VALIDATED_COLLAPSED" else "VALIDATED"
  list(
    ok = TRUE,
    pipeline = pipeline,
    collapsed = collapsed,
    tune_hit_rate = tune_hr,
    validate_hit_rate = m$hit_rate,
    drop = if (!is.na(tune_hr) && !is.na(m$hit_rate)) tune_hr - m$hit_rate else NA_real_,
    msg = if (collapsed)
            paste0("Hit-rate dropped by ", round((tune_hr - m$hit_rate) * 100, 1),
                   "pp on Validate. Rule marked collapsed.")
          else "Validate passed."
  )
}

# ─── Commit gate ─────────────────────────────────────────────
# Only VALIDATED pipelines can be committed. VALIDATED_COLLAPSED is
# also blocked (user must re-tune or explicitly force-commit).
commit_pipeline <- function(pipeline, force = FALSE) {
  if (pipeline$state == "COMMITTED") {
    return(list(ok = TRUE, pipeline = pipeline, msg = "Already committed."))
  }
  if (pipeline$state == "TESTED") {
    return(list(ok = FALSE, msg = "Already tested. Cannot re-commit."))
  }
  if (pipeline$state == "TUNED") {
    return(list(ok = FALSE, msg = "Must run Validate before committing."))
  }
  if (pipeline$state == "VALIDATED_COLLAPSED" && !force) {
    return(list(ok = FALSE,
                msg = "Rule collapsed on Validate. Use force=TRUE to override (not recommended)."))
  }
  pipeline$state <- "COMMITTED"
  pipeline$committed_at <- Sys.time()
  list(ok = TRUE, pipeline = pipeline, msg = "Committed. Test tab unlocked.")
}

# ─── Test — final honest measurement ─────────────────────────
# HARD-BLOCK: only COMMITTED pipelines can be tested.
test_pipeline <- function(pipeline, test_feat_df, grade_fn) {
  if (pipeline$state != "COMMITTED") {
    return(list(ok = FALSE,
                msg = paste0("Test blocked. Rule state is '", pipeline$state,
                             "'. Must be COMMITTED first.")))
  }
  if (nrow(test_feat_df) == 0) {
    return(list(ok = FALSE, msg = "Test set is empty. Load a file tagged 'Test'."))
  }
  m <- grade_full_pipeline(pipeline, test_feat_df, grade_fn)
  pipeline$test_metrics <- m
  pipeline$state <- "TESTED"
  pipeline$tested_at <- Sys.time()
  list(ok = TRUE, pipeline = pipeline, msg = "Tested (terminal state).",
       test_metrics = m)
}

# ─── Per-round breakdown ─────────────────────────────────────
# Given a feat_df with a per-row `source_file` column (from loading.R) and
# a pipeline, break performance out by source_file (round proxy).
per_round_breakdown <- function(pipeline, feat_df, grade_fn) {
  if (!"source_file" %in% names(feat_df)) {
    return(data.frame(source_file = character(0), picks = integer(0),
                      wins = integer(0), losses = integer(0),
                      hit_rate = numeric(0), stringsAsFactors = FALSE))
  }
  files <- unique(feat_df$source_file)
  rows <- lapply(files, function(f) {
    sub <- feat_df[feat_df$source_file == f, , drop = FALSE]
    m <- grade_full_pipeline(pipeline, sub, grade_fn)
    data.frame(
      source_file = f,
      n_fixtures  = nrow(sub),
      picks       = m$picks,
      wins        = m$wins,
      losses      = m$losses,
      hit_rate    = m$hit_rate,
      stringsAsFactors = FALSE
    )
  })
  df <- do.call(rbind, rows)
  df[order(df$source_file), , drop = FALSE]
}

# ─── Stability summary (per-round variance) ──────────────────
# Flags a rule as "unstable" if per-round hit rate varies wildly.
stability_summary <- function(per_round_df, sd_flag_threshold = 0.15) {
  hr <- per_round_df$hit_rate
  hr <- hr[!is.na(hr)]
  if (length(hr) < 2) {
    return(list(sd = NA_real_, mean = if (length(hr) > 0) mean(hr) else NA_real_,
                unstable = FALSE, msg = "Not enough rounds for stability check."))
  }
  s <- sd(hr)
  list(
    sd = s,
    mean = mean(hr),
    unstable = s > sd_flag_threshold,
    msg = if (s > sd_flag_threshold)
            paste0("Unstable: sd=", round(s, 3), " across ", length(hr), " rounds.")
          else paste0("Stable: sd=", round(s, 3), " across ", length(hr), " rounds.")
  )
}

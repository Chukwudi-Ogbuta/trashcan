# ============================================================
# engine/grid_search.R
# Session 2 — Stage 1 grid search with rule families and window sweeps.
#
# A "rule" is now a list of clauses. Each clause specifies:
#   family       : "ge" | "le" | "range" | "diff" | "ratio" | "sum" | "quality"
#   feature      : primary feature name (or paired features for diff/sum)
#   feature2     : secondary feature (diff/sum/ratio)
#   threshold    : numeric (or c(low, high) for range)
#   quality_gate : threshold applied to the underlying counted event (for
#                  "quality" family — e.g. "scored 2+ goals 3 of last 5")
# A rule is a list of clauses. All clauses must fire for the rule to pick.
#
# For Stage 1, the market file supplies:
#   feature_sets : named list of feature -> grid_values      (family = ge, backward compat)
#   OR
#   rules_spec   : list of clause templates with grids (see grid_spec module below)
#
# The engine sweeps every clause combination, evaluates picks/wins/losses,
# tracks per-rule sample size and coverage, dedups on picked-fingerprint,
# and returns a leaderboard.
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Clause application ──────────────────────────────────────
# Apply one clause to a feature matrix. Returns a logical vector.
# NA in any relevant feature ⇒ FALSE for that fixture (rule can't fire).
apply_clause <- function(feat_df, clause) {
  fam <- clause$family
  fn  <- clause$feature
  fn2 <- clause$feature2 %||% NA_character_
  th  <- clause$threshold
  gate <- clause$quality_gate

  v1 <- if (!is.null(feat_df[[fn]])) as.numeric(feat_df[[fn]]) else return(rep(FALSE, nrow(feat_df)))
  v2 <- if (!is.na(fn2) && !is.null(feat_df[[fn2]])) as.numeric(feat_df[[fn2]]) else NULL

  switch(fam,
    ge      = !is.na(v1) & v1 >= th,
    le      = !is.na(v1) & v1 <= th,
    range   = {
      lo <- th[1]; hi <- th[2]
      !is.na(v1) & v1 >= lo & v1 <= hi
    },
    diff    = {
      if (is.null(v2)) return(rep(FALSE, nrow(feat_df)))
      d <- v1 - v2
      !is.na(d) & d >= th
    },
    sum     = {
      if (is.null(v2)) return(rep(FALSE, nrow(feat_df)))
      s <- v1 + v2
      !is.na(s) & s >= th
    },
    ratio   = {
      if (is.null(v2)) return(rep(FALSE, nrow(feat_df)))
      # zero-safe: ratio undefined ⇒ NA ⇒ excluded
      r <- ifelse(is.na(v1) | is.na(v2) | v2 == 0, NA_real_, v1 / v2)
      !is.na(r) & r >= th
    },
    quality = {
      # v1 must be a "count" feature; the quality_gate has already been baked
      # into the underlying feature choice (e.g. h_scored_2plus_n5 vs h_scored_n5),
      # so this behaves like ge here. Preserved as its own family so leaderboards
      # can display "quality-adjusted" tag.
      !is.na(v1) & v1 >= th
    },
    # unknown family ⇒ never fires
    rep(FALSE, nrow(feat_df))
  )
}

# ─── Rule (list of clauses) application ──────────────────────
apply_rule <- function(feat_df, rule) {
  if (length(rule) == 0) return(rep(TRUE, nrow(feat_df)))
  passes <- rep(TRUE, nrow(feat_df))
  for (cl in rule) passes <- passes & apply_clause(feat_df, cl)
  passes
}

# ─── Features referenced by a rule (for sample-size calculation) ─
rule_features <- function(rule) {
  fns <- character(0)
  for (cl in rule) {
    fns <- c(fns, cl$feature)
    if (!is.null(cl$feature2) && !is.na(cl$feature2)) fns <- c(fns, cl$feature2)
  }
  unique(fns)
}

# ─── Baseline ────────────────────────────────────────────────
compute_baseline <- function(feat_df, grade_fn) {
  grades <- sapply(seq_len(nrow(feat_df)), function(i) grade_fn(feat_df[i, ]))
  gradable <- !is.na(grades)
  list(
    n_gradable = sum(gradable),
    n_wins     = sum(grades[gradable]),
    hit_rate   = if (sum(gradable) > 0) sum(grades[gradable]) / sum(gradable) else NA_real_
  )
}

# ─── Grid spec expansion ─────────────────────────────────────
# grid_spec: list of clause TEMPLATES. Each template is a list with:
#   family, feature (or feature+feature2), grid (numeric vector — thresholds
#   to try) OR grid_low/grid_high (for range)
# Optional: family default = "ge"
# expand_grid_spec returns a data.frame of concrete clause combos.
# Each row of the returned data.frame is one full rule (multiple clauses).
expand_grid_spec <- function(grid_spec) {
  # Build a list per clause-template of the values to sweep, then expand.grid
  per_clause <- list()
  clause_meta <- list()
  for (i in seq_along(grid_spec)) {
    tpl <- grid_spec[[i]]
    fam <- tpl$family %||% "ge"
    if (fam == "range") {
      # sweep low+high pairs; require grid_low, grid_high (equal length or Cartesian?)
      lo <- tpl$grid_low
      hi <- tpl$grid_high
      pairs <- expand.grid(lo = lo, hi = hi, stringsAsFactors = FALSE)
      pairs <- pairs[pairs$lo <= pairs$hi, , drop = FALSE]  # enforce lo<=hi
      # encode as strings for expand.grid downstream
      per_clause[[paste0("c", i)]] <- paste0(pairs$lo, ":", pairs$hi)
    } else {
      per_clause[[paste0("c", i)]] <- as.character(tpl$grid)
    }
    clause_meta[[i]] <- tpl
  }
  grid <- do.call(expand.grid, c(per_clause, list(stringsAsFactors = FALSE)))
  attr(grid, "clause_meta") <- clause_meta
  grid
}

# Convert one row of the expanded grid back into a concrete rule (list of clauses).
row_to_rule <- function(grid_row, clause_meta) {
  rule <- list()
  for (i in seq_along(clause_meta)) {
    tpl <- clause_meta[[i]]
    fam <- tpl$family %||% "ge"
    cell <- as.character(grid_row[[paste0("c", i)]])
    cl <- list(
      family = fam,
      feature = tpl$feature,
      feature2 = tpl$feature2 %||% NA_character_
    )
    if (fam == "range") {
      parts <- strsplit(cell, ":")[[1]]
      cl$threshold <- as.numeric(parts)
    } else {
      cl$threshold <- as.numeric(cell)
    }
    if (!is.null(tpl$quality_gate)) cl$quality_gate <- tpl$quality_gate
    rule[[i]] <- cl
  }
  rule
}

# Short human label for a clause (for the leaderboard)
clause_label <- function(cl) {
  fam <- cl$family
  switch(fam,
    ge      = paste0(cl$feature, " >= ", cl$threshold),
    le      = paste0(cl$feature, " <= ", cl$threshold),
    range   = paste0(cl$feature, " in [", cl$threshold[1], ",", cl$threshold[2], "]"),
    diff    = paste0(cl$feature, " - ", cl$feature2, " >= ", cl$threshold),
    sum     = paste0(cl$feature, " + ", cl$feature2, " >= ", cl$threshold),
    ratio   = paste0(cl$feature, " / ", cl$feature2, " >= ", cl$threshold),
    quality = paste0(cl$feature, " >= ", cl$threshold, " (quality)"),
    paste0("?", fam)
  )
}

rule_label <- function(rule) {
  paste(vapply(rule, clause_label, character(1)), collapse = " & ")
}

# ─── Main grid search ────────────────────────────────────────
# feat_df    : feature matrix
# grade_fn   : row -> TRUE/FALSE/NA
# grid_spec  : list of clause templates with grids
# min_picks  : leaderboard floor
# progress_cb: optional function(i, n) called as rules are processed
run_grid_search <- function(feat_df, grade_fn, grid_spec, min_picks = 20,
                             progress_cb = NULL) {
  grades <- sapply(seq_len(nrow(feat_df)), function(i) grade_fn(feat_df[i, ]))
  gradable <- !is.na(grades)
  baseline <- compute_baseline(feat_df, grade_fn)

  grid <- expand_grid_spec(grid_spec)
  clause_meta <- attr(grid, "clause_meta")
  n_rules <- nrow(grid)

  # Preallocate
  picks_v <- integer(n_rules)
  wins_v  <- integer(n_rules)
  losses_v<- integer(n_rules)
  hit_v   <- rep(NA_real_, n_rules)
  lift_v  <- rep(NA_real_, n_rules)
  rej_v   <- integer(n_rules)
  rej_win_v <- integer(n_rules)
  rej_hit_v <- rep(NA_real_, n_rules)
  sample_v  <- integer(n_rules)
  fp_v      <- character(n_rules)
  rule_lbl  <- character(n_rules)

  for (i in seq_len(n_rules)) {
    rule <- row_to_rule(grid[i, , drop = FALSE], clause_meta)
    passes <- apply_rule(feat_df, rule)

    # sample size = fixtures where all rule features are non-NA
    used_cols <- rule_features(rule)
    if (length(used_cols) == 0) {
      sample_mask <- rep(TRUE, nrow(feat_df))
    } else {
      sample_mask <- Reduce(`&`, lapply(used_cols, function(c) !is.na(feat_df[[c]])))
    }

    picked_gradable   <- passes & gradable
    picks <- sum(picked_gradable)
    wins  <- sum(grades[picked_gradable])
    rejected_gradable <- sample_mask & !passes & gradable
    rej_n <- sum(rejected_gradable)
    rej_w <- sum(grades[rejected_gradable])
    hit <- if (picks > 0) wins / picks else NA_real_

    picks_v[i]  <- picks
    wins_v[i]   <- wins
    losses_v[i] <- picks - wins
    hit_v[i]    <- hit
    lift_v[i]   <- if (!is.na(hit)) hit - baseline$hit_rate else NA_real_
    rej_v[i]    <- rej_n
    rej_win_v[i]<- rej_w
    rej_hit_v[i]<- if (rej_n > 0) rej_w / rej_n else NA_real_
    sample_v[i] <- sum(sample_mask)
    fp_v[i]     <- paste(which(picked_gradable), collapse = ",")
    rule_lbl[i] <- rule_label(rule)

    if (!is.null(progress_cb) && (i %% max(1, floor(n_rules/20)) == 0)) {
      progress_cb(i, n_rules)
    }
  }

  # Uses only rule families & feature names for a "family_tag" per rule
  families <- vapply(seq_len(n_rules), function(i) {
    r <- row_to_rule(grid[i, , drop = FALSE], clause_meta)
    paste(unique(vapply(r, function(cl) cl$family, character(1))), collapse = "+")
  }, character(1))

  leaderboard <- data.frame(
    rule_id       = seq_len(n_rules),
    rule_label    = rule_lbl,
    family_tag    = families,
    picks         = picks_v,
    wins          = wins_v,
    losses        = losses_v,
    hit_rate      = hit_v,
    baseline_lift = lift_v,
    rejected      = rej_v,
    rejected_wins = rej_win_v,
    rejected_hit_rate = rej_hit_v,
    missed_opportunity_rate = rej_hit_v,
    sample_size   = sample_v,
    stringsAsFactors = FALSE
  )
  leaderboard$dedup_group <- match(fp_v, unique(fp_v))

  list(
    baseline = baseline,
    leaderboard = leaderboard,
    n_rules_tested = n_rules,
    n_unique_rules = length(unique(fp_v)),
    grid_spec = grid_spec,
    clause_meta = clause_meta
  )
}

# ─── Sorting helpers (unchanged interface from Session 1) ────
dedup_leaderboard <- function(leaderboard) {
  leaderboard[!duplicated(leaderboard$dedup_group), ]
}

sort_leaderboard <- function(leaderboard, sort_by = "hit_rate",
                              min_picks = 20, dedup = TRUE) {
  lb <- if (dedup) dedup_leaderboard(leaderboard) else leaderboard
  lb <- lb[lb$picks >= min_picks, , drop = FALSE]
  if (nrow(lb) == 0) return(lb)
  o <- switch(sort_by,
    hit_rate      = order(-lb$hit_rate, -lb$picks),
    volume        = order(-lb$picks, -lb$hit_rate),
    baseline_lift = order(-lb$baseline_lift, -lb$picks),
    order(-lb$hit_rate, -lb$picks)
  )
  lb[o, , drop = FALSE]
}

sort_by_missed_opportunity <- function(leaderboard, min_rejected = 20, dedup = TRUE) {
  lb <- if (dedup) dedup_leaderboard(leaderboard) else leaderboard
  lb <- lb[lb$rejected >= min_rejected, , drop = FALSE]
  if (nrow(lb) == 0) return(lb)
  lb[order(-lb$missed_opportunity_rate, -lb$rejected), , drop = FALSE]
}

# ─── Backward-compat helper: convert a Session-1 style threshold_grid
# (named list of numeric vectors) into a Session-2 grid_spec (all ge clauses).
threshold_grid_to_spec <- function(threshold_grid) {
  lapply(names(threshold_grid), function(fn) {
    list(family = "ge", feature = fn, grid = threshold_grid[[fn]])
  })
}

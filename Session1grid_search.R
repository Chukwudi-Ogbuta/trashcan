# ============================================================
# engine/grid_search.R
# Stage 1 grid search engine.
# Sweeps threshold combinations for a market's rule and produces
# a ranked dual leaderboard (picked + rejected).
#
# Features:
#   - Configurable threshold ranges per feature
#   - Automatic dedup of rules producing identical outcomes
#   - Per-rule sample size tracking
#   - Dual leaderboard: picked-fixtures + rejected-fixtures
#   - Multiple sort options (hit rate, volume, baseline lift)
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Compute baseline hit rate for a market on a fixture set ─
# feat_df: feature matrix from build_feature_matrix
# grade_fn: function(row) -> TRUE (win) / FALSE (loss) / NA (ungradable)
compute_baseline <- function(feat_df, grade_fn) {
  grades <- sapply(seq_len(nrow(feat_df)), function(i) grade_fn(feat_df[i, ]))
  gradable <- !is.na(grades)
  n_gradable <- sum(gradable)
  n_wins <- sum(grades[gradable])
  list(
    n_gradable = n_gradable,
    n_wins = n_wins,
    hit_rate = if (n_gradable > 0) n_wins / n_gradable else NA_real_
  )
}

# ─── Apply a rule (list of feature >= threshold conditions) ──
# rule: named list where names are feature names, values are thresholds
# Returns a logical vector: TRUE = fixture passes rule
apply_threshold_rule <- function(feat_df, rule) {
  if (length(rule) == 0) return(rep(TRUE, nrow(feat_df)))
  passes <- rep(TRUE, nrow(feat_df))
  for (fn in names(rule)) {
    threshold <- rule[[fn]]
    col_vals <- as.numeric(feat_df[[fn]])
    # Fixtures with NA in a used feature are excluded (can't evaluate)
    passes <- passes & !is.na(col_vals) & (col_vals >= threshold)
  }
  passes
}

# ─── Run Stage 1 grid search ─────────────────────────────────
# feat_df:         feature matrix (with result columns)
# grade_fn:        function(row) -> TRUE/FALSE/NA
# threshold_grid:  named list, each element is a numeric vector of values to sweep
#                  e.g. list(h_scored_n5 = 2:5, a_scored_n5 = 2:5)
# min_picks:       minimum picks for a rule to appear on hit-rate leaderboard
# Returns: leaderboard data.frame with picks/wins/losses/hit_rate + rejected stats + dedup group
run_grid_search <- function(feat_df, grade_fn, threshold_grid, min_picks = 20) {
  # Grade every fixture once (avoid recomputing per rule)
  grades <- sapply(seq_len(nrow(feat_df)), function(i) grade_fn(feat_df[i, ]))
  gradable_idx <- which(!is.na(grades))
  
  # Baseline
  baseline <- compute_baseline(feat_df, grade_fn)
  
  # Generate all threshold combinations
  grid <- do.call(expand.grid, c(threshold_grid, list(stringsAsFactors = FALSE)))
  n_rules <- nrow(grid)
  
  # Preallocate result columns
  result <- data.frame(
    rule_id = seq_len(n_rules),
    picks = 0L,
    wins = 0L,
    losses = 0L,
    hit_rate = NA_real_,
    baseline_lift = NA_real_,
    rejected = 0L,
    rejected_wins = 0L,
    rejected_hit_rate = NA_real_,
    missed_opportunity_rate = NA_real_,
    sample_size = 0L,
    fingerprint = "",
    stringsAsFactors = FALSE
  )
  # Bind threshold columns to the front for readability
  result <- cbind(grid, result)
  
  # Loop through each rule
  # Fingerprint = which gradable fixtures the rule picks (allows dedup)
  for (i in seq_len(n_rules)) {
    rule <- as.list(grid[i, , drop = FALSE])
    passes <- apply_threshold_rule(feat_df, rule)
    
    # Sample size = fixtures where the rule can be evaluated (no NA in used features)
    # Since apply_threshold_rule already excludes NA rows, sample_size = fixtures where
    # ALL rule features have non-NA values.
    used_cols <- names(rule)
    sample_mask <- Reduce(`&`, lapply(used_cols, function(c) !is.na(feat_df[[c]])))
    sample_size <- sum(sample_mask)
    
    # Picked-fixtures stats
    picked_and_gradable <- passes & !is.na(grades)
    picks <- sum(picked_and_gradable)
    wins <- sum(grades[picked_and_gradable])
    losses <- picks - wins
    hit_rate <- if (picks > 0) wins / picks else NA_real_
    
    # Rejected-fixtures stats (within the sample where rule could be evaluated)
    rejected_and_gradable <- sample_mask & !passes & !is.na(grades)
    rejected_n <- sum(rejected_and_gradable)
    rejected_wins <- sum(grades[rejected_and_gradable])
    rejected_hit_rate <- if (rejected_n > 0) rejected_wins / rejected_n else NA_real_
    missed_opp_rate <- rejected_hit_rate  # same value, named for clarity in leaderboard
    
    # Fingerprint: string encoding of which gradable fixture indices got picked
    fp <- paste(which(picked_and_gradable), collapse = ",")
    
    result$picks[i] <- picks
    result$wins[i] <- wins
    result$losses[i] <- losses
    result$hit_rate[i] <- hit_rate
    result$baseline_lift[i] <- if (!is.na(hit_rate)) hit_rate - baseline$hit_rate else NA_real_
    result$rejected[i] <- rejected_n
    result$rejected_wins[i] <- rejected_wins
    result$rejected_hit_rate[i] <- rejected_hit_rate
    result$missed_opportunity_rate[i] <- missed_opp_rate
    result$sample_size[i] <- sample_size
    result$fingerprint[i] <- fp
  }
  
  # ─── Dedup: collapse rules with identical fingerprints ─────
  # Keep the "tightest" representative (highest threshold combo) as the primary
  # for each group.
  result$dedup_group <- match(result$fingerprint, unique(result$fingerprint))
  # Drop fingerprint from user-facing output (large strings, only needed for dedup)
  result$fingerprint <- NULL
  
  list(
    baseline = baseline,
    leaderboard = result,
    n_rules_tested = n_rules,
    n_unique_rules = length(unique(result$dedup_group))
  )
}

# ─── Dedup helper: return only one representative row per dedup group ─
dedup_leaderboard <- function(leaderboard) {
  # Take first row of each group (grid ordering means this is a stable pick)
  leaderboard[!duplicated(leaderboard$dedup_group), ]
}

# ─── Sort leaderboard by chosen metric ───────────────────────
# sort_by: "hit_rate" | "volume" | "baseline_lift"
# min_picks: filter rows below this floor before sorting
sort_leaderboard <- function(leaderboard, sort_by = "hit_rate",
                              min_picks = 20, dedup = TRUE) {
  lb <- if (dedup) dedup_leaderboard(leaderboard) else leaderboard
  lb <- lb[lb$picks >= min_picks, , drop = FALSE]
  if (nrow(lb) == 0) return(lb)
  o <- switch(
    sort_by,
    hit_rate      = order(-lb$hit_rate, -lb$picks),
    volume        = order(-lb$picks, -lb$hit_rate),
    baseline_lift = order(-lb$baseline_lift, -lb$picks),
    order(-lb$hit_rate, -lb$picks)
  )
  lb[o, , drop = FALSE]
}

# ─── Sort by "missed opportunity" for rescue analysis ────────
# Shows rules whose REJECTED pile has the highest hit rate — those are
# the rules leaving the most winners on the table.
sort_by_missed_opportunity <- function(leaderboard, min_rejected = 20, dedup = TRUE) {
  lb <- if (dedup) dedup_leaderboard(leaderboard) else leaderboard
  lb <- lb[lb$rejected >= min_rejected, , drop = FALSE]
  if (nrow(lb) == 0) return(lb)
  lb[order(-lb$missed_opportunity_rate, -lb$rejected), , drop = FALSE]
}

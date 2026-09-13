# ============================================================
# engine/stage2.R
# Stage 2 engine — REFINE ONLY.
#
# Given:
#   - A Stage 1 winning rule (list of clauses, from grid_search.R)
#   - The feature matrix used for Stage 1
#   - A grade_fn (from the market)
#   - A collection of intuitions (from intuition_loader.R)
#
# Stage 2 applies intuition filters to Stage 1's PICKED set only.
# Goal: reduce loss count while preserving useful volume.
# An intuition improves refine if it keeps most of Stage 1's wins
# and drops mostly its losses.
#
# The rescue track (mining Stage 1's rejected pile) was removed
# by design: Stage 1's grid is far more thorough than intuitions
# can reasonably override. If Stage 1 rejects a fixture, we trust
# that decision.
#
# Combination depth:
#   • First pass:  each intuition ALONE
#   • Second pass: all pairs
#   • Third pass:  all triples (optional; combinatorial blow-up,
#                  off by default)
#
# Each intuition can carry a `tunable` param grid — Stage 2 sweeps
# that too, so a single intuition with a 3-value threshold becomes
# 3 candidate filters in the leaderboard.
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Expand one intuition's tunable grid to concrete filters ─
# Returns a list of (intuition_name, param_label, param_list) tuples.
# For intuitions with no tunable params, returns a single entry with empty params.
expand_intuition_params <- function(intu) {
  tun <- intu$tunable %||% list()
  if (length(tun) == 0) {
    return(list(list(
      intuition = intu,
      param_label = intu$name,
      params = list()
    )))
  }
  grid <- do.call(expand.grid, c(tun, list(stringsAsFactors = FALSE)))
  lapply(seq_len(nrow(grid)), function(i) {
    p <- as.list(grid[i, , drop = FALSE])
    # Build a short label e.g. "home_form_momentum[threshold_scored=2]"
    label <- paste0(intu$name, "[",
                    paste(names(p), unlist(p), sep = "=", collapse = ","),
                    "]")
    list(intuition = intu, param_label = label, params = p)
  })
}

# ─── Apply one candidate filter to a subset of the feature matrix ─
# Returns a logical vector (length = nrow(subset)) of KEEP/DROP/NA.
# NA rows are dropped from consideration (can't evaluate).
apply_candidate <- function(candidate, subset_df) {
  intu <- candidate$intuition
  params <- candidate$params
  n <- nrow(subset_df)
  if (n == 0) return(logical(0))
  keep <- vapply(seq_len(n), function(i) {
    v <- tryCatch(intu$filter_fn(subset_df[i, , drop = FALSE], params),
                  error = function(e) NA)
    if (is.null(v) || length(v) == 0) return(NA)
    if (is.na(v)) return(NA)
    isTRUE(v)
  }, logical(1))
  keep
}

# ─── Grade the outcome of applying a filter combo ────────────
# subset_df   : feature matrix restricted to the fixtures in play
# combo       : list of candidate filters (length 1, 2, or 3)
# grade_fn    : the market's grade function
# track       : "refine" | "rescue" (labels only; math is identical
#               — both compute picks/wins/losses on the resulting subset)
# baseline_hit_rate : Stage 1's hit rate on this subset (for lift calc)
grade_combo <- function(subset_df, combo, grade_fn,
                        track = "refine", baseline_hit_rate = NA_real_) {
  n <- nrow(subset_df)
  if (n == 0) {
    return(list(picks = 0L, wins = 0L, losses = 0L,
                hit_rate = NA_real_, lift = NA_real_,
                undecidable = 0L, picked_ids = integer(0)))
  }
  keep_matrix <- vapply(combo, function(cand) apply_candidate(cand, subset_df),
                        logical(n))
  # If combo has length 1, vapply gave us a length-n vector, not a matrix.
  if (!is.matrix(keep_matrix)) keep_matrix <- matrix(keep_matrix, ncol = 1)

  # Row-wise AND with NA propagation:
  # - Any FALSE = drop
  # - All TRUE = keep
  # - Otherwise (some NA, no FALSE) = undecidable
  n_filters <- ncol(keep_matrix)
  any_false <- apply(keep_matrix, 1, function(r) isTRUE(any(r == FALSE, na.rm = TRUE)))
  all_true  <- apply(keep_matrix, 1, function(r) all(r == TRUE, na.rm = FALSE) & !any(is.na(r)))
  undecidable <- !any_false & !all_true

  grades <- vapply(seq_len(n), function(i) {
    g <- grade_fn(subset_df[i, , drop = FALSE])
    if (is.null(g) || length(g) == 0) return(NA)
    if (is.na(g)) return(NA)
    isTRUE(g)
  }, logical(1))

  # Track semantics both reduce to: what's the hit rate on rows we KEEP?
  kept_and_gradable <- all_true & !is.na(grades)
  picks <- sum(kept_and_gradable)
  wins  <- sum(grades[kept_and_gradable])
  hit_rate <- if (picks > 0) wins / picks else NA_real_
  lift <- if (!is.na(hit_rate) && !is.na(baseline_hit_rate))
            hit_rate - baseline_hit_rate else NA_real_

  list(
    picks       = as.integer(picks),
    wins        = as.integer(wins),
    losses      = as.integer(picks - wins),
    hit_rate    = hit_rate,
    lift        = lift,
    undecidable = sum(undecidable),
    picked_ids  = which(kept_and_gradable)
  )
}

# ─── Sweep combos of a given depth ────────────────────────────
# candidates : the full list of expanded candidate filters (post param sweep)
# depth      : 1, 2, or 3
# Returns a data.frame with one row per combo:
#   combo_label, depth, picks, wins, losses, hit_rate, lift, undecidable, fingerprint
sweep_stage2_combos <- function(candidates, subset_df, grade_fn, depth,
                                 baseline_hit_rate, track,
                                 min_picks = 10, progress_cb = NULL) {
  n_cand <- length(candidates)
  if (n_cand == 0 || depth < 1 || depth > 3) {
    return(data.frame())
  }
  idx <- if (depth == 1) matrix(seq_len(n_cand), ncol = 1)
         else t(utils::combn(n_cand, depth))
  n_combos <- nrow(idx)

  rows <- vector("list", n_combos)
  for (i in seq_len(n_combos)) {
    combo <- candidates[idx[i, ]]
    res <- grade_combo(subset_df, combo, grade_fn,
                       track = track,
                       baseline_hit_rate = baseline_hit_rate)
    rows[[i]] <- data.frame(
      combo_label   = paste(vapply(combo, function(c) c$param_label, character(1)),
                             collapse = " & "),
      depth         = depth,
      picks         = res$picks,
      wins          = res$wins,
      losses        = res$losses,
      hit_rate      = res$hit_rate,
      lift          = res$lift,
      undecidable   = res$undecidable,
      fingerprint   = paste(res$picked_ids, collapse = ","),
      stringsAsFactors = FALSE
    )
    if (!is.null(progress_cb) && (i %% max(1, floor(n_combos/20)) == 0)) {
      progress_cb(i, n_combos, depth)
    }
  }
  do.call(rbind, rows)
}

# ─── Main Stage 2 entry point ────────────────────────────────
# stage1_result : object returned by run_grid_search(), plus the chosen rule
# rule          : the concrete rule (list of clauses) selected from Stage 1
# feat_df       : the feature matrix used in Stage 1
# grade_fn      : market grade function
# intuitions    : list of intuition objects (filtered by round + market status
#                 by the caller)
# depths        : which combination depths to sweep (default c(1, 2))
# min_picks     : leaderboard floor
# current_round : integer, used to auto-exclude intuitions whose min_round is higher
#
# Returns list with:
#   refine    : data.frame (leaderboard on Stage 1 picks)
#   rescue    : data.frame (leaderboard on Stage 1 rejected)
#   stage1_picks_baseline : hit rate of the Stage 1 rule alone on picks
#   stage1_rejected_baseline : hit rate on rejected (missed_opportunity_rate)
run_stage2 <- function(stage1_rule, feat_df, grade_fn, intuitions,
                       depths = c(1, 2), min_picks = 10,
                       current_round = NA_integer_,
                       progress_cb = NULL) {
  # Filter intuitions by min_round if we know the round
  if (!is.na(current_round)) {
    intuitions <- Filter(function(intu) {
      (intu$min_round %||% 1L) <= current_round
    }, intuitions)
  }

  # Expand param grids
  candidates <- unlist(lapply(intuitions, expand_intuition_params),
                       recursive = FALSE, use.names = FALSE)
  if (length(candidates) == 0) {
    return(list(
      refine = data.frame(),
      note = "No intuitions available for this round/market."
    ))
  }

  # Apply Stage 1 rule to isolate picks (Stage 2 refines picks only —
  # the rescue track was removed by design: Stage 1's grid is more
  # thorough than intuitions can reasonably override).
  passes <- apply_rule(feat_df, stage1_rule)
  # sample_mask = rows where every Stage 1 feature is non-NA
  used_cols <- rule_features(stage1_rule)
  sample_mask <- if (length(used_cols) == 0) rep(TRUE, nrow(feat_df))
                 else Reduce(`&`, lapply(used_cols, function(c) !is.na(feat_df[[c]])))

  grades_all <- vapply(seq_len(nrow(feat_df)), function(i) {
    g <- grade_fn(feat_df[i, , drop = FALSE])
    if (is.null(g) || length(g) == 0) return(NA)
    if (is.na(g)) return(NA)
    isTRUE(g)
  }, logical(1))

  picked_idx <- which(passes & sample_mask & !is.na(grades_all))

  # Baseline for the refine track
  picks_baseline <- if (length(picked_idx) > 0)
                      mean(grades_all[picked_idx]) else NA_real_

  # REFINE track — sweep on Stage 1's picked subset
  refine_dfs <- list()
  picked_df <- feat_df[picked_idx, , drop = FALSE]
  for (d in depths) {
    df <- sweep_stage2_combos(candidates, picked_df, grade_fn,
                              depth = d,
                              baseline_hit_rate = picks_baseline,
                              track = "refine",
                              min_picks = min_picks,
                              progress_cb = progress_cb)
    if (nrow(df) > 0) {
      df$track <- "refine"
      refine_dfs[[length(refine_dfs) + 1]] <- df
    }
  }
  refine_lb <- if (length(refine_dfs) > 0) do.call(rbind, refine_dfs) else data.frame()

  # Dedup on fingerprint
  if (nrow(refine_lb) > 0) refine_lb$dedup_group <- match(refine_lb$fingerprint, unique(refine_lb$fingerprint))

  list(
    refine = refine_lb,
    stage1_picks_baseline = picks_baseline,
    n_picked   = length(picked_idx),
    n_candidates_expanded = length(candidates),
    depths_swept = depths
  )
}

# ─── Sort a Stage 2 leaderboard ──────────────────────────────
sort_stage2 <- function(lb, sort_by = "hit_rate", min_picks = 10, dedup = TRUE) {
  if (is.null(lb) || nrow(lb) == 0) return(lb)
  if (dedup && "dedup_group" %in% names(lb)) {
    lb <- lb[!duplicated(lb$dedup_group), , drop = FALSE]
  }
  lb <- lb[lb$picks >= min_picks, , drop = FALSE]
  if (nrow(lb) == 0) return(lb)
  o <- switch(sort_by,
    hit_rate = order(-lb$hit_rate, -lb$picks),
    lift     = order(-lb$lift, -lb$picks),
    volume   = order(-lb$picks, -lb$hit_rate),
    order(-lb$hit_rate, -lb$picks)
  )
  lb[o, , drop = FALSE]
}

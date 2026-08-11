# ============================================================
# engine/features.R
# Feature library shared across all markets.
# Every feature is a function that takes a fixture (ef) and returns
# a named list of computed values. Missing data returns NA, not 0.
#
# Session 1 scope: goals-based features sufficient for BTTS.
# Session 2 expands: stats, H2H, meeting-point, Poisson, opponent-strength,
# identical-team, goal-timing features.
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Utility: safe missing check for nested list fields ──────
# Form/H2H entries sometimes have list() where scalars are expected.
is_missing_scalar <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0) return(TRUE)
  if (is.list(x)) return(TRUE)
  isTRUE(is.na(x[1]))
}

# ─── Utility: pull last N form entries, optionally filtered ──
# form_list: list of form matches
# n: window size
# main_only: if TRUE, only main-league matches count toward the window
# min_required: if fewer than this many qualify, return NULL (insufficient data)
select_form_window <- function(form_list, n = 5, main_only = FALSE,
                                min_required = NULL, max_history = 10) {
  if (is.null(form_list) || length(form_list) == 0) return(NULL)
  # Walk history taking qualifying matches
  qualified <- list()
  depth <- min(length(form_list), max_history)
  for (i in seq_len(depth)) {
    m <- form_list[[i]]
    if (main_only && !isTRUE(m$is_main_league)) next
    qualified[[length(qualified) + 1]] <- m
    if (length(qualified) >= n) break
  }
  # Check minimum
  if (!is.null(min_required) && length(qualified) < min_required) return(NULL)
  qualified
}

# ─── Utility: count matches matching a predicate ─────────────
count_where <- function(form_window, predicate) {
  if (is.null(form_window) || length(form_window) == 0) return(NA_integer_)
  sum(vapply(form_window, function(m) isTRUE(predicate(m)), logical(1)))
}

# ─── Utility: mean of a scalar field across window ───────────
mean_field <- function(form_window, field) {
  if (is.null(form_window) || length(form_window) == 0) return(NA_real_)
  vals <- vapply(form_window, function(m) {
    v <- m[[field]]
    if (is_missing_scalar(v)) NA_real_ else as.numeric(v[1])
  }, numeric(1))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals)
}

# ─── Predicates on individual form matches ───────────────────
pred_scored     <- function(m) !is_missing_scalar(m$ft_for)     && as.numeric(m$ft_for[1])     >= 1
pred_conceded   <- function(m) !is_missing_scalar(m$ft_against) && as.numeric(m$ft_against[1]) >= 1
pred_btts       <- function(m) pred_scored(m) && pred_conceded(m)
pred_over_15    <- function(m) {
  if (is_missing_scalar(m$ft_for) || is_missing_scalar(m$ft_against)) return(FALSE)
  (as.numeric(m$ft_for[1]) + as.numeric(m$ft_against[1])) >= 2
}
pred_over_25    <- function(m) {
  if (is_missing_scalar(m$ft_for) || is_missing_scalar(m$ft_against)) return(FALSE)
  (as.numeric(m$ft_for[1]) + as.numeric(m$ft_against[1])) >= 3
}
pred_clean_sheet <- function(m) !is_missing_scalar(m$ft_against) && as.numeric(m$ft_against[1]) == 0
pred_scored_2plus <- function(m) !is_missing_scalar(m$ft_for) && as.numeric(m$ft_for[1]) >= 2

# ─── Feature computation for one fixture ─────────────────────
# Returns a named numeric vector of features for the fixture.
# Only computes features listed in `feature_names` (efficiency).
# Every unknown/missing feature returns NA.
compute_features <- function(ef, feature_names, window = 5,
                              main_only = FALSE, min_required = NULL) {
  out <- setNames(rep(NA_real_, length(feature_names)), feature_names)
  hf <- select_form_window(ef$home_form, n = window,
                           main_only = main_only, min_required = min_required)
  af <- select_form_window(ef$away_form, n = window,
                           main_only = main_only, min_required = min_required)
  h2 <- ef$h2h %||% list()  # H2H typically small, no window filter
  
  for (fn in feature_names) {
    v <- switch(
      fn,
      # ── HOME form (count features) ─────────────────────────
      h_scored_n5      = count_where(hf, pred_scored),
      h_conceded_n5    = count_where(hf, pred_conceded),
      h_btts_n5        = count_where(hf, pred_btts),
      h_over15_n5      = count_where(hf, pred_over_15),
      h_over25_n5      = count_where(hf, pred_over_25),
      h_clean_sheets_n5 = count_where(hf, pred_clean_sheet),
      h_scored_2plus_n5 = count_where(hf, pred_scored_2plus),
      # ── HOME form (mean features) ──────────────────────────
      h_avg_goals_scored_n5   = mean_field(hf, "ft_for"),
      h_avg_goals_conceded_n5 = mean_field(hf, "ft_against"),
      h_avg_ht_scored_n5      = mean_field(hf, "ht_for"),
      # ── AWAY form (count features) ─────────────────────────
      a_scored_n5      = count_where(af, pred_scored),
      a_conceded_n5    = count_where(af, pred_conceded),
      a_btts_n5        = count_where(af, pred_btts),
      a_over15_n5      = count_where(af, pred_over_15),
      a_over25_n5      = count_where(af, pred_over_25),
      a_clean_sheets_n5 = count_where(af, pred_clean_sheet),
      a_scored_2plus_n5 = count_where(af, pred_scored_2plus),
      # ── AWAY form (mean features) ──────────────────────────
      a_avg_goals_scored_n5   = mean_field(af, "ft_for"),
      a_avg_goals_conceded_n5 = mean_field(af, "ft_against"),
      a_avg_ht_scored_n5      = mean_field(af, "ht_for"),
      # ── H2H features ───────────────────────────────────────
      h2h_btts_n5      = count_where(h2, pred_btts),
      h2h_over25_n5    = count_where(h2, pred_over_25),
      h2h_avg_goals    = mean_field(h2, "ft_for"),  # NOTE: h2h uses concerned_was_home
      # ── STANDINGS features (top-level) ─────────────────────
      home_rank        = as.numeric(ef$fixture$home_rank %||% NA),
      away_rank        = as.numeric(ef$fixture$away_rank %||% NA),
      home_ppg         = as.numeric(ef$fixture$home_ppg %||% NA),
      away_ppg         = as.numeric(ef$fixture$away_ppg %||% NA),
      rank_gap         = {
        hr <- as.numeric(ef$fixture$home_rank %||% NA)
        ar <- as.numeric(ef$fixture$away_rank %||% NA)
        if (is.na(hr) || is.na(ar)) NA_real_ else abs(hr - ar)
      },
      NA_real_
    )
    if (!is.null(v) && length(v) > 0) out[fn] <- v[1]
  }
  out
}

# ─── Feature catalog (registry) ──────────────────────────────
# What each feature IS, its category, and whether it needs stats
# (for coverage-aware handling in Session 2).
FEATURE_CATALOG <- list(
  h_scored_n5      = list(category = "goals",     needs_stats = FALSE, desc = "Home scored in N of last 5"),
  h_conceded_n5    = list(category = "goals",     needs_stats = FALSE, desc = "Home conceded in N of last 5"),
  h_btts_n5        = list(category = "goals",     needs_stats = FALSE, desc = "BTTS hit in home's last 5"),
  h_over15_n5      = list(category = "goals",     needs_stats = FALSE, desc = "Over 1.5 hit in home's last 5"),
  h_over25_n5      = list(category = "goals",     needs_stats = FALSE, desc = "Over 2.5 hit in home's last 5"),
  h_clean_sheets_n5 = list(category = "goals",    needs_stats = FALSE, desc = "Home clean sheets in last 5"),
  h_scored_2plus_n5 = list(category = "goals",    needs_stats = FALSE, desc = "Home scored 2+ in N of last 5"),
  h_avg_goals_scored_n5   = list(category = "goals", needs_stats = FALSE, desc = "Home mean goals scored last 5"),
  h_avg_goals_conceded_n5 = list(category = "goals", needs_stats = FALSE, desc = "Home mean goals conceded last 5"),
  h_avg_ht_scored_n5      = list(category = "goals", needs_stats = FALSE, desc = "Home mean HT goals scored last 5"),
  a_scored_n5      = list(category = "goals",     needs_stats = FALSE, desc = "Away scored in N of last 5"),
  a_conceded_n5    = list(category = "goals",     needs_stats = FALSE, desc = "Away conceded in N of last 5"),
  a_btts_n5        = list(category = "goals",     needs_stats = FALSE, desc = "BTTS hit in away's last 5"),
  a_over15_n5      = list(category = "goals",     needs_stats = FALSE, desc = "Over 1.5 hit in away's last 5"),
  a_over25_n5      = list(category = "goals",     needs_stats = FALSE, desc = "Over 2.5 hit in away's last 5"),
  a_clean_sheets_n5 = list(category = "goals",    needs_stats = FALSE, desc = "Away clean sheets in last 5"),
  a_scored_2plus_n5 = list(category = "goals",    needs_stats = FALSE, desc = "Away scored 2+ in N of last 5"),
  a_avg_goals_scored_n5   = list(category = "goals", needs_stats = FALSE, desc = "Away mean goals scored last 5"),
  a_avg_goals_conceded_n5 = list(category = "goals", needs_stats = FALSE, desc = "Away mean goals conceded last 5"),
  a_avg_ht_scored_n5      = list(category = "goals", needs_stats = FALSE, desc = "Away mean HT goals scored last 5"),
  h2h_btts_n5      = list(category = "h2h",       needs_stats = FALSE, desc = "BTTS hit in H2H last 5"),
  h2h_over25_n5    = list(category = "h2h",       needs_stats = FALSE, desc = "Over 2.5 hit in H2H last 5"),
  h2h_avg_goals    = list(category = "h2h",       needs_stats = FALSE, desc = "Mean H2H goals scored"),
  home_rank        = list(category = "standings", needs_stats = FALSE, desc = "Home league position"),
  away_rank        = list(category = "standings", needs_stats = FALSE, desc = "Away league position"),
  home_ppg         = list(category = "standings", needs_stats = FALSE, desc = "Home points per game"),
  away_ppg         = list(category = "standings", needs_stats = FALSE, desc = "Away points per game"),
  rank_gap         = list(category = "standings", needs_stats = FALSE, desc = "Absolute rank difference")
)

# ─── Build feature matrix for a set of fixtures ──────────────
# Returns a data.frame: one row per fixture, one column per feature.
# Also includes result columns for grading.
build_feature_matrix <- function(fixtures, feature_names, window = 5,
                                  main_only = FALSE, min_required = NULL) {
  rows <- lapply(fixtures, function(ef) {
    feats <- compute_features(ef, feature_names, window = window,
                              main_only = main_only, min_required = min_required)
    fx <- ef$fixture
    c(
      list(
        match_id = as.character(fx$match_id %||% NA),
        league   = as.character(fx$league   %||% NA),
        country  = as.character(fx$country  %||% NA),
        home     = as.character(fx$home     %||% NA),
        away     = as.character(fx$away     %||% NA),
        result_status   = as.character(fx$result_status  %||% NA),
        result_ft_home  = as.numeric(fx$result_ft_home  %||% NA),
        result_ft_away  = as.numeric(fx$result_ft_away  %||% NA),
        result_ht_home  = as.numeric(fx$result_ht_home  %||% NA),
        result_ht_away  = as.numeric(fx$result_ht_away  %||% NA),
        result_total_corners = as.numeric(fx$result_total_corners %||% NA),
        result_total_cards   = as.numeric(fx$result_total_cards   %||% NA),
        result_total_shots   = as.numeric(fx$result_total_shots   %||% NA),
        result_total_sot     = as.numeric(fx$result_total_sot     %||% NA),
        source_file = as.character(ef$source_file %||% "single"),
        source_role = as.character(ef$source_role %||% "Tune")
      ),
      as.list(feats)
    )
  })
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

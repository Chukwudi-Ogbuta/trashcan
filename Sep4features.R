# ============================================================
# engine/features.R
# Session 2 — full feature library.
#
# Architecture:
#   - Every feature is computed by name via compute_features().
#   - Feature names encode window: "..._n5", "..._n3", "..._n7".
#     Rule families (range/diff/ratio/sum) are applied downstream
#     in grid_search.R; here we just supply the raw features they need.
#   - FEATURE_CATALOG tags each feature with:
#       category      — goals | stats | h2h | meeting_point | poisson |
#                       goal_timing | opponent_strength | identical_team |
#                       standings
#       needs_stats   — TRUE if depends on stats/stats_1h/stats_2h
#       needs_goaltime— TRUE if depends on goal_times
#       min_round     — rules using this feature should not fire before
#                       this round (identical-team = 5+, opponent-strength = 4+)
#       side          — "home" | "away" | "match" | "h2h"
#       desc          — human-readable
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Missing-scalar guard for nested list fields ─────────────
is_missing_scalar <- function(x) {
  if (is.null(x)) return(TRUE)
  if (length(x) == 0) return(TRUE)
  if (is.list(x)) return(TRUE)
  isTRUE(is.na(x[1]))
}

# ─── Stat string parser ──────────────────────────────────────
# stats fields arrive as strings: "58%", "10", "7". Convert to numeric.
# Percentages become 0-100. Missing/empty returns NA.
parse_stat_value <- function(x) {
  if (is_missing_scalar(x)) return(NA_real_)
  s <- as.character(x[1])
  s <- trimws(s)
  if (nchar(s) == 0) return(NA_real_)
  # strip trailing % if present
  pct <- endsWith(s, "%")
  s <- gsub("%$", "", s)
  v <- suppressWarnings(as.numeric(s))
  if (is.na(v)) return(NA_real_)
  v
}

# ─── Goal-time parser ────────────────────────────────────────
# goal_times entries look like "88'" or "90+3'". Return integer minute.
parse_goal_minute <- function(gt_entry) {
  if (is_missing_scalar(gt_entry)) return(NA_integer_)
  s <- as.character(gt_entry[1])
  s <- gsub("'", "", s, fixed = TRUE)
  # handle "90+3" style — sum the parts
  if (grepl("\\+", s)) {
    parts <- suppressWarnings(as.integer(strsplit(s, "\\+")[[1]]))
    if (any(is.na(parts))) return(NA_integer_)
    return(sum(parts))
  }
  v <- suppressWarnings(as.integer(s))
  if (is.na(v)) return(NA_integer_)
  v
}

parse_goal_times <- function(gt_list) {
  if (is.null(gt_list) || length(gt_list) == 0) return(integer(0))
  mins <- vapply(gt_list, parse_goal_minute, integer(1))
  mins[!is.na(mins)]
}

# ─── Window selection with adaptive fill (main-league policy) ─
# Walks up to max_history matches, collecting those that qualify.
# Stops early once n qualify. Returns NULL if fewer than min_required.
select_form_window <- function(form_list, n = 5, main_only = FALSE,
                                min_required = NULL, max_history = 10) {
  if (is.null(form_list) || length(form_list) == 0) return(NULL)
  qualified <- list()
  depth <- min(length(form_list), max_history)
  for (i in seq_len(depth)) {
    m <- form_list[[i]]
    if (main_only && !isTRUE(m$is_main_league)) next
    qualified[[length(qualified) + 1]] <- m
    if (length(qualified) >= n) break
  }
  if (!is.null(min_required) && length(qualified) < min_required) return(NULL)
  qualified
}

# ─── Generic aggregators over a window ───────────────────────
count_where <- function(form_window, predicate) {
  if (is.null(form_window) || length(form_window) == 0) return(NA_integer_)
  sum(vapply(form_window, function(m) isTRUE(predicate(m)), logical(1)))
}

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

var_field <- function(form_window, field) {
  if (is.null(form_window) || length(form_window) < 2) return(NA_real_)
  vals <- vapply(form_window, function(m) {
    v <- m[[field]]
    if (is_missing_scalar(v)) NA_real_ else as.numeric(v[1])
  }, numeric(1))
  vals <- vals[!is.na(vals)]
  if (length(vals) < 2) return(NA_real_)
  var(vals)
}

# ─── Stat aggregators (reach into stats / stats_1h / stats_2h) ─
mean_stat <- function(form_window, stat_name, half = c("full", "1h", "2h")) {
  half <- match.arg(half)
  container <- switch(half, full = "stats", `1h` = "stats_1h", `2h` = "stats_2h")
  if (is.null(form_window) || length(form_window) == 0) return(NA_real_)
  vals <- vapply(form_window, function(m) {
    s <- m[[container]]
    if (is.null(s) || length(s) == 0) return(NA_real_)
    parse_stat_value(s[[stat_name]])
  }, numeric(1))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals)
}

# ─── Predicates on individual form matches (goals-based) ────
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
pred_over_35    <- function(m) {
  if (is_missing_scalar(m$ft_for) || is_missing_scalar(m$ft_against)) return(FALSE)
  (as.numeric(m$ft_for[1]) + as.numeric(m$ft_against[1])) >= 4
}
pred_clean_sheet  <- function(m) !is_missing_scalar(m$ft_against) && as.numeric(m$ft_against[1]) == 0
pred_scored_2plus <- function(m) !is_missing_scalar(m$ft_for) && as.numeric(m$ft_for[1]) >= 2
pred_scored_3plus <- function(m) !is_missing_scalar(m$ft_for) && as.numeric(m$ft_for[1]) >= 3
pred_ht_scored    <- function(m) !is_missing_scalar(m$ht_for) && as.numeric(m$ht_for[1]) >= 1
pred_ht_conceded  <- function(m) !is_missing_scalar(m$ht_against) && as.numeric(m$ht_against[1]) >= 1
pred_ht_draw      <- function(m) {
  if (is_missing_scalar(m$ht_for) || is_missing_scalar(m$ht_against)) return(FALSE)
  as.numeric(m$ht_for[1]) == as.numeric(m$ht_against[1])
}
pred_2h_over15    <- function(m) {
  if (is_missing_scalar(m$ft_for) || is_missing_scalar(m$ft_against) ||
      is_missing_scalar(m$ht_for) || is_missing_scalar(m$ht_against)) return(FALSE)
  ((as.numeric(m$ft_for[1]) - as.numeric(m$ht_for[1])) +
   (as.numeric(m$ft_against[1]) - as.numeric(m$ht_against[1]))) >= 2
}
pred_2h_btts      <- function(m) {
  if (is_missing_scalar(m$ft_for) || is_missing_scalar(m$ft_against) ||
      is_missing_scalar(m$ht_for) || is_missing_scalar(m$ht_against)) return(FALSE)
  (as.numeric(m$ft_for[1]) - as.numeric(m$ht_for[1])) >= 1 &&
    (as.numeric(m$ft_against[1]) - as.numeric(m$ht_against[1])) >= 1
}

# ─── Goal-timing aggregators over a window ───────────────────
# Return the *rate* (0-1) of concerned-team goals falling in a minute bucket.
# Denominator is all recorded goals across the window (not matches).
goal_timing_rate <- function(form_window, from_min, to_min) {
  if (is.null(form_window) || length(form_window) == 0) return(NA_real_)
  all_mins <- integer(0)
  for (m in form_window) {
    all_mins <- c(all_mins, parse_goal_times(m$goal_times_concerned))
  }
  if (length(all_mins) == 0) return(NA_real_)
  mean(all_mins >= from_min & all_mins <= to_min)
}

# Same shape but reads OPPONENT goal times — measures conceded-goal timing.
goal_timing_conceded_rate <- function(form_window, from_min, to_min) {
  if (is.null(form_window) || length(form_window) == 0) return(NA_real_)
  all_mins <- integer(0)
  for (m in form_window) {
    all_mins <- c(all_mins, parse_goal_times(m$goal_times_opponent))
  }
  if (length(all_mins) == 0) return(NA_real_)
  mean(all_mins >= from_min & all_mins <= to_min)
}

# ─── HT/FT-based half rates (no goal_times dependency) ──────
# Uses ht_for / ft_for etc, which are populated on every form entry.
# Wider coverage than the goal-time-based half rates.
half_rate_scored <- function(form_window, half = c("first","second")) {
  half <- match.arg(half)
  if (is.null(form_window) || length(form_window) == 0) return(NA_real_)
  total_ft  <- 0; total_half <- 0; any_valid <- FALSE
  for (m in form_window) {
    ft <- suppressWarnings(as.numeric(m$ft_for))
    ht <- suppressWarnings(as.numeric(m$ht_for))
    if (length(ft) == 0 || length(ht) == 0) next
    if (is.na(ft) || is.na(ht)) next
    any_valid  <- TRUE
    total_ft   <- total_ft   + ft
    half_goals <- if (half == "first") ht else (ft - ht)
    total_half <- total_half + half_goals
  }
  if (!any_valid || total_ft == 0) return(NA_real_)
  total_half / total_ft
}

half_rate_conceded <- function(form_window, half = c("first","second")) {
  half <- match.arg(half)
  if (is.null(form_window) || length(form_window) == 0) return(NA_real_)
  total_ft  <- 0; total_half <- 0; any_valid <- FALSE
  for (m in form_window) {
    ft <- suppressWarnings(as.numeric(m$ft_against))
    ht <- suppressWarnings(as.numeric(m$ht_against))
    if (length(ft) == 0 || length(ht) == 0) next
    if (is.na(ft) || is.na(ht)) next
    any_valid  <- TRUE
    total_ft   <- total_ft   + ft
    half_goals <- if (half == "first") ht else (ft - ht)
    total_half <- total_half + half_goals
  }
  if (!any_valid || total_ft == 0) return(NA_real_)
  total_half / total_ft
}

# Late goal tendency = fraction of goals after minute 65
# Early goal tendency = fraction in first 20
# 2H rate            = fraction between minute 46 and 90+extra

# ─── Poisson helpers ─────────────────────────────────────────
# Recency-weighted lambda per team.
# Uses last-window matches with linearly decreasing weights (most recent = window, oldest = 1).
weighted_mean <- function(vals, weights) {
  if (length(vals) == 0) return(NA_real_)
  ok <- !is.na(vals) & !is.na(weights)
  if (!any(ok)) return(NA_real_)
  sum(vals[ok] * weights[ok]) / sum(weights[ok])
}

lambda_for_side <- function(form_window, league_avg = NA_real_) {
  if (is.null(form_window) || length(form_window) == 0) return(NA_real_)
  n <- length(form_window)
  w <- seq(from = n, to = 1)  # newest weighted highest
  scored <- vapply(form_window, function(m)
    if (is_missing_scalar(m$ft_for)) NA_real_ else as.numeric(m$ft_for[1]), numeric(1))
  conceded_opp <- vapply(form_window, function(m)
    if (is_missing_scalar(m$ft_against)) NA_real_ else as.numeric(m$ft_against[1]), numeric(1))
  team_rate <- weighted_mean(scored, w)
  # Opponent conceding rate proxy = mean of `ft_against` across the window.
  opp_rate  <- weighted_mean(conceded_opp, w)
  if (is.na(team_rate) || is.na(opp_rate)) return(NA_real_)
  la <- if (is.na(league_avg)) 1.35 else league_avg  # default global mean goals/side/match
  # Multiplicative baseline: (team_scoring / la) * (opp_conceding / la) * la
  # simplifies to team_scoring * opp_conceding / la
  (team_rate * opp_rate) / la
}

poisson_p_over <- function(lambda_h, lambda_a, threshold) {
  if (is.na(lambda_h) || is.na(lambda_a)) return(NA_real_)
  # score grid 0..8 each side (0.5% mass beyond that is negligible for these lambdas)
  grid_max <- 8
  ph <- dpois(0:grid_max, lambda_h)
  pa <- dpois(0:grid_max, lambda_a)
  # outer product = joint probs; sum over cells where home+away > threshold
  totals <- outer(0:grid_max, 0:grid_max, "+")
  mask <- totals > threshold
  sum(outer(ph, pa) * mask)
}

poisson_p_btts <- function(lambda_h, lambda_a) {
  if (is.na(lambda_h) || is.na(lambda_a)) return(NA_real_)
  # P(BTTS) = 1 - P(home=0) - P(away=0) + P(both=0)
  p_h0 <- dpois(0, lambda_h)
  p_a0 <- dpois(0, lambda_a)
  1 - p_h0 - p_a0 + p_h0 * p_a0
}

poisson_p_1x2 <- function(lambda_h, lambda_a, which = c("home", "draw", "away")) {
  which <- match.arg(which)
  if (is.na(lambda_h) || is.na(lambda_a)) return(NA_real_)
  grid_max <- 8
  ph <- dpois(0:grid_max, lambda_h)
  pa <- dpois(0:grid_max, lambda_a)
  # matrix M[i,j] = P(home=i-1) * P(away=j-1)
  M <- outer(ph, pa)
  hs <- row(M) - 1L
  as_ <- col(M) - 1L
  switch(which,
    home = sum(M[hs > as_]),
    draw = sum(M[hs == as_]),
    away = sum(M[hs < as_])
  )
}

# ─── Opponent-strength: reach into concerned-team's recent opponents ─
# Uses the "concerned_was_home" flag to identify which side the recorded team was.
# The OPPONENT in each form entry is not represented by rank fields (fixture-level
# only), so opponent strength is a proxy derived from that historical match's
# own goal record. Level-1 features fall out of average goals scored/conceded
# by that opponent, which we already have. Level-2 identical-team pattern-match
# requires a same-league opponent lookup that we don't have without cross-fixture
# joins — deferred to Phase 3+ per plan.
#
# What IS computable now from the current schema:
#   - avg goals conceded by opponents in that recent match (their ft_for is the
#     concerned team's ft_against, so this collapses to h_avg_goals_conceded).
# Real opponent-strength arrives once we build cross-fixture team indexing —
# flagged as needs_cross_index in the catalog and returned as NA for now.

opponent_strength_placeholder <- function(...) NA_real_

# ─── Feature computer ────────────────────────────────────────
# compute_features(ef, feature_names, window_map = NULL, main_only = FALSE, min_required = NULL)
# window_map: optional named list mapping window-suffix (3/5/7) OR feature-name to
#             window size. Since feature names carry _nN suffix, we parse the suffix.
#             If a feature is passed without _nN, `default_window` is used.
parse_feature_window <- function(name, default = 5) {
  m <- regmatches(name, regexpr("_n[0-9]+$", name))
  if (length(m) == 0) return(default)
  as.integer(sub("_n", "", m))
}

parse_feature_side <- function(name) {
  if (startsWith(name, "h_"))      return("home")
  if (startsWith(name, "a_"))      return("away")
  if (startsWith(name, "h2h_"))    return("h2h")
  return("match")
}

# Split feature name into (side, root, window). e.g. "h_scored_n5" -> ("h", "scored", 5)
parse_feature_parts <- function(name) {
  side <- if (startsWith(name, "h_")) "h" else
          if (startsWith(name, "a_")) "a" else
          if (startsWith(name, "h2h_")) "h2h" else ""
  rest <- if (side == "h2h") sub("^h2h_", "", name) else
          if (side %in% c("h","a")) sub("^[ha]_", "", name) else name
  win_match <- regmatches(rest, regexpr("_n[0-9]+$", rest))
  root <- if (length(win_match) > 0) sub("_n[0-9]+$", "", rest) else rest
  win  <- if (length(win_match) > 0) as.integer(sub("_n", "", win_match)) else NA_integer_
  list(side = side, root = root, window = win)
}

# Cached windowed selects to avoid recomputing per feature.
# Returns a function(window, main_only) -> form list for the given side.
make_window_cache <- function(form_list, main_only, min_required, max_history = 10) {
  memo <- new.env(parent = emptyenv())
  function(win) {
    key <- as.character(win)
    if (!is.null(memo[[key]])) return(memo[[key]])
    v <- select_form_window(form_list, n = win, main_only = main_only,
                            min_required = min_required, max_history = max_history)
    memo[[key]] <- if (is.null(v)) list(EMPTY = TRUE) else v
    memo[[key]]
  }
}

# Returns list() if the cached slot is a sentinel EMPTY marker.
resolve_window <- function(cached) {
  if (is.list(cached) && isTRUE(cached$EMPTY)) return(NULL)
  cached
}

# ─── Core feature dispatch ───────────────────────────────────
compute_features <- function(ef, feature_names, default_window = 5,
                              main_only = FALSE, min_required = NULL,
                              league_avg_goals = 1.35) {
  out <- setNames(rep(NA_real_, length(feature_names)), feature_names)
  home_cache <- make_window_cache(ef$home_form, main_only, min_required)
  away_cache <- make_window_cache(ef$away_form, main_only, min_required)
  h2h_list   <- ef$h2h %||% list()

  # Precompute Poisson lambdas (any Poisson-dependent feature reuses them).
  # Lambda uses the concerned team's own form only. Home lambda comes from home_form
  # (goals for the home team in past matches), away lambda from away_form.
  hf5 <- resolve_window(home_cache(5))
  af5 <- resolve_window(away_cache(5))
  lambda_h <- lambda_for_side(hf5, league_avg = league_avg_goals)
  lambda_a <- lambda_for_side(af5, league_avg = league_avg_goals)

  for (fn in feature_names) {
    parts <- parse_feature_parts(fn)
    win <- if (is.na(parts$window)) default_window else parts$window
    side <- parts$side; root <- parts$root

    # pick which form list to use
    fw <- switch(side,
                 h   = resolve_window(home_cache(win)),
                 a   = resolve_window(away_cache(win)),
                 h2h = h2h_list,
                 NULL)

    v <- NA_real_

    # ─── SIDE = home / away (form-based) ───────────────────────
    if (side %in% c("h", "a") && !is.null(fw)) {
      v <- switch(root,
        # ── COUNT features ──
        scored          = count_where(fw, pred_scored),
        conceded        = count_where(fw, pred_conceded),
        btts            = count_where(fw, pred_btts),
        over15          = count_where(fw, pred_over_15),
        over25          = count_where(fw, pred_over_25),
        over35          = count_where(fw, pred_over_35),
        clean_sheets    = count_where(fw, pred_clean_sheet),
        scored_2plus    = count_where(fw, pred_scored_2plus),
        scored_3plus    = count_where(fw, pred_scored_3plus),
        ht_scored       = count_where(fw, pred_ht_scored),
        ht_conceded     = count_where(fw, pred_ht_conceded),
        ht_draw         = count_where(fw, pred_ht_draw),
        second_h_over15 = count_where(fw, pred_2h_over15),
        second_h_btts   = count_where(fw, pred_2h_btts),
        # ── MEAN features ──
        avg_goals_scored     = mean_field(fw, "ft_for"),
        avg_goals_conceded   = mean_field(fw, "ft_against"),
        avg_ht_scored        = mean_field(fw, "ht_for"),
        avg_ht_conceded      = mean_field(fw, "ht_against"),
        avg_goals_when_scored = {
          # mean goals per scoring game (excludes blanks)
          scoring <- Filter(function(m) pred_scored(m), fw)
          if (length(scoring) == 0) NA_real_ else mean_field(scoring, "ft_for")
        },
        # ── VOLATILITY ──
        score_variance = var_field(fw, "ft_for"),
        concede_variance = var_field(fw, "ft_against"),
        # ── STATS features (mean_stat: full match) ──
        avg_possession   = mean_stat(fw, "ball_possession", "full"),
        avg_shots        = mean_stat(fw, "total_shots", "full"),
        avg_sot          = mean_stat(fw, "shots_on_target", "full"),
        avg_shots_off    = mean_stat(fw, "shots_off_target", "full"),
        avg_corners      = mean_stat(fw, "corner_kicks", "full"),
        avg_offsides     = mean_stat(fw, "offsides", "full"),
        avg_free_kicks   = mean_stat(fw, "free_kicks", "full"),
        avg_throw_ins    = mean_stat(fw, "throw_ins", "full"),
        avg_fouls        = mean_stat(fw, "fouls", "full"),
        # ── STATS 1H / 2H splits ──
        avg_shots_1h     = mean_stat(fw, "total_shots", "1h"),
        avg_shots_2h     = mean_stat(fw, "total_shots", "2h"),
        avg_sot_1h       = mean_stat(fw, "shots_on_target", "1h"),
        avg_sot_2h       = mean_stat(fw, "shots_on_target", "2h"),
        avg_corners_1h   = mean_stat(fw, "corner_kicks", "1h"),
        avg_corners_2h   = mean_stat(fw, "corner_kicks", "2h"),
        avg_fouls_1h     = mean_stat(fw, "fouls", "1h"),
        avg_fouls_2h     = mean_stat(fw, "fouls", "2h"),
        # ── GOAL TIMING (scored) — early/late need goal_times ──
        early_goal_rate  = goal_timing_rate(fw, 1, 20),
        late_goal_rate   = goal_timing_rate(fw, 65, 130),  # 130 covers ET
        # ── HALF RATES (scored) — use HT/FT, wider coverage ──
        second_half_goal_rate = half_rate_scored(fw, "second"),
        first_half_goal_rate  = half_rate_scored(fw, "first"),
        # ── GOAL TIMING (conceded) — early/late need goal_times ──
        early_conceded_rate = goal_timing_conceded_rate(fw, 1, 20),
        late_conceded_rate  = goal_timing_conceded_rate(fw, 65, 130),
        # ── HALF RATES (conceded) — use HT/FT, wider coverage ──
        second_half_conceded_rate = half_rate_conceded(fw, "second"),
        first_half_conceded_rate  = half_rate_conceded(fw, "first"),
        NA_real_
      )
    }

    # ─── SIDE = h2h ─────────────────────────────────────────────
    if (side == "h2h" && length(h2h_list) > 0) {
      v <- switch(root,
        btts            = count_where(h2h_list, pred_btts),
        over15          = count_where(h2h_list, pred_over_15),
        over25          = count_where(h2h_list, pred_over_25),
        over35          = count_where(h2h_list, pred_over_35),
        avg_goals       = {
          vals <- vapply(h2h_list, function(m) {
            if (is_missing_scalar(m$ft_for) || is_missing_scalar(m$ft_against)) NA_real_
            else as.numeric(m$ft_for[1]) + as.numeric(m$ft_against[1])
          }, numeric(1))
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) NA_real_ else mean(vals)
        },
        blank_rate      = {
          # Both scored (BTTS) = 1; else blank happened
          n <- length(h2h_list)
          if (n == 0) NA_real_ else 1 - (count_where(h2h_list, pred_btts) / n)
        },
        home_clean_rate = {
          n <- length(h2h_list)
          if (n == 0) NA_real_ else
            sum(vapply(h2h_list, function(m) {
              # in h2h, "ft_against" is concerned home team's ft_against — meaningful only
              # when concerned_was_home == TRUE. When FALSE, ft_for was that team scoring
              # away. We treat clean sheet from home's perspective: ft_against == 0 when
              # concerned_was_home, else ft_for == 0 when concerned_was_away.
              if (is_missing_scalar(m$concerned_was_home)) return(FALSE)
              if (isTRUE(as.logical(m$concerned_was_home[1]))) {
                !is_missing_scalar(m$ft_against) && as.numeric(m$ft_against[1]) == 0
              } else {
                !is_missing_scalar(m$ft_for) && as.numeric(m$ft_for[1]) == 0
              }
            }, logical(1))) / n
        },
        NA_real_
      )
    }

    # ─── SIDE = match (meeting-point / Poisson / standings) ────
    if (side == "" || is.null(side)) side <- "match"
    if (side == "match") {
      v <- switch(fn,
        # ── Poisson ──
        lambda_h   = lambda_h,
        lambda_a   = lambda_a,
        total_lambda = if (is.na(lambda_h) || is.na(lambda_a)) NA_real_ else lambda_h + lambda_a,
        p_over_15    = poisson_p_over(lambda_h, lambda_a, 1),
        p_over_25    = poisson_p_over(lambda_h, lambda_a, 2),
        p_over_35    = poisson_p_over(lambda_h, lambda_a, 3),
        p_btts       = poisson_p_btts(lambda_h, lambda_a),
        p_home_win   = poisson_p_1x2(lambda_h, lambda_a, "home"),
        p_draw       = poisson_p_1x2(lambda_h, lambda_a, "draw"),
        p_away_win   = poisson_p_1x2(lambda_h, lambda_a, "away"),
        # ── Standings (fixture-level) ──
        home_rank    = as.numeric(ef$fixture$home_rank %||% NA),
        away_rank    = as.numeric(ef$fixture$away_rank %||% NA),
        home_ppg     = as.numeric(ef$fixture$home_ppg %||% NA),
        away_ppg     = as.numeric(ef$fixture$away_ppg %||% NA),
        home_gd      = as.numeric(ef$fixture$home_gd %||% NA),
        away_gd      = as.numeric(ef$fixture$away_gd %||% NA),
        rank_gap     = {
          hr <- as.numeric(ef$fixture$home_rank %||% NA)
          ar <- as.numeric(ef$fixture$away_rank %||% NA)
          if (is.na(hr) || is.na(ar)) NA_real_ else abs(hr - ar)
        },
        rank_diff    = {
          # signed home_rank - away_rank (negative = home ranked higher/better)
          hr <- as.numeric(ef$fixture$home_rank %||% NA)
          ar <- as.numeric(ef$fixture$away_rank %||% NA)
          if (is.na(hr) || is.na(ar)) NA_real_ else hr - ar
        },
        ppg_diff     = {
          hp <- as.numeric(ef$fixture$home_ppg %||% NA)
          ap <- as.numeric(ef$fixture$away_ppg %||% NA)
          if (is.na(hp) || is.na(ap)) NA_real_ else hp - ap
        },
        gd_diff      = {
          hg <- as.numeric(ef$fixture$home_gd %||% NA)
          ag <- as.numeric(ef$fixture$away_gd %||% NA)
          if (is.na(hg) || is.na(ag)) NA_real_ else hg - ag
        },
        NA_real_
      )
    }

    if (!is.null(v) && length(v) > 0) out[fn] <- as.numeric(v[1])
  }
  out
}

# ─── Feature catalog ─────────────────────────────────────────
# Programmatic construction to keep it maintainable.
# Each entry: category, needs_stats, needs_goaltime, min_round, side, desc
.catalog_entry <- function(category, desc, needs_stats = FALSE, needs_goaltime = FALSE,
                           min_round = 1, side = "match") {
  list(category = category, desc = desc,
       needs_stats = needs_stats, needs_goaltime = needs_goaltime,
       min_round = min_round, side = side)
}

.build_windowed <- function(root, sides, windows, category, desc,
                            needs_stats = FALSE, needs_goaltime = FALSE, min_round = 1) {
  out <- list()
  for (s in sides) {
    prefix <- if (s == "home") "h_" else if (s == "away") "a_" else "h2h_"
    side_tag <- if (s == "h2h") "h2h" else s
    for (w in windows) {
      nm <- paste0(prefix, root, "_n", w)
      out[[nm]] <- .catalog_entry(category, paste0(desc, " (window ", w, ")"),
                                  needs_stats = needs_stats,
                                  needs_goaltime = needs_goaltime,
                                  min_round = min_round, side = side_tag)
    }
  }
  out
}

FEATURE_CATALOG <- local({
  fc <- list()

  # Goals count (home + away) × windows 3,5,7
  for (root_desc in list(
        list("scored",         "Times scored (>=1) in last N"),
        list("conceded",       "Times conceded (>=1) in last N"),
        list("btts",           "BTTS hit in last N"),
        list("over15",         "Over 1.5 hit in last N"),
        list("over25",         "Over 2.5 hit in last N"),
        list("over35",         "Over 3.5 hit in last N"),
        list("clean_sheets",   "Clean sheets in last N"),
        list("scored_2plus",   "Scored 2+ in last N (quality-adjusted)"),
        list("scored_3plus",   "Scored 3+ in last N (quality-adjusted)"),
        list("ht_scored",      "Scored by HT in last N"),
        list("ht_conceded",    "Conceded by HT in last N"),
        list("ht_draw",        "HT draw in last N"),
        list("second_h_over15","2H Over 1.5 in last N"),
        list("second_h_btts",  "2H BTTS in last N"))) {
    fc <- c(fc, .build_windowed(root_desc[[1]], c("home","away"),
                                c(3,5,7), "goals", root_desc[[2]]))
  }

  # Goals mean
  for (root_desc in list(
        list("avg_goals_scored",     "Mean goals scored last N"),
        list("avg_goals_conceded",   "Mean goals conceded last N"),
        list("avg_ht_scored",        "Mean HT goals scored last N"),
        list("avg_ht_conceded",      "Mean HT goals conceded last N"),
        list("avg_goals_when_scored","Mean goals per scoring game last N"))) {
    fc <- c(fc, .build_windowed(root_desc[[1]], c("home","away"),
                                c(3,5,7), "goals", root_desc[[2]]))
  }

  # Volatility
  fc <- c(fc, .build_windowed("score_variance",   c("home","away"), c(5,7), "goals", "Variance of goals scored"))
  fc <- c(fc, .build_windowed("concede_variance", c("home","away"), c(5,7), "goals", "Variance of goals conceded"))

  # Stats (needs_stats = TRUE, window 5,7 only — stats coverage thin at 3)
  for (root_desc in list(
        list("avg_possession", "Mean ball possession % last N"),
        list("avg_shots",      "Mean total shots last N"),
        list("avg_sot",        "Mean shots on target last N"),
        list("avg_shots_off",  "Mean shots off target last N"),
        list("avg_corners",    "Mean corners last N"),
        list("avg_offsides",   "Mean offsides last N"),
        list("avg_free_kicks", "Mean free kicks last N"),
        list("avg_throw_ins",  "Mean throw-ins last N"),
        list("avg_fouls",      "Mean fouls last N"),
        list("avg_shots_1h",   "Mean 1H shots last N"),
        list("avg_shots_2h",   "Mean 2H shots last N"),
        list("avg_sot_1h",     "Mean 1H SOT last N"),
        list("avg_sot_2h",     "Mean 2H SOT last N"),
        list("avg_corners_1h", "Mean 1H corners last N"),
        list("avg_corners_2h", "Mean 2H corners last N"),
        list("avg_fouls_1h",   "Mean 1H fouls last N"),
        list("avg_fouls_2h",   "Mean 2H fouls last N"))) {
    fc <- c(fc, .build_windowed(root_desc[[1]], c("home","away"),
                                c(5,7), "stats", root_desc[[2]], needs_stats = TRUE))
  }

  # Goal timing (needs_goaltime = TRUE) — early/late buckets need minute-level data
  for (root_desc in list(
        list("early_goal_rate",      "Rate of goals in min 1-20"),
        list("late_goal_rate",       "Rate of goals in min 65+"),
        list("early_conceded_rate",  "Rate of conceded goals in min 1-20"),
        list("late_conceded_rate",   "Rate of conceded goals in min 65+"))) {
    fc <- c(fc, .build_windowed(root_desc[[1]], c("home","away"),
                                c(5,7), "goal_timing", root_desc[[2]], needs_goaltime = TRUE))
  }

  # Half rates (HT/FT-based, wider coverage — no goal_times dependency)
  for (root_desc in list(
        list("first_half_goal_rate",       "Fraction of scored goals in first half (HT/FT-based)"),
        list("second_half_goal_rate",      "Fraction of scored goals in second half (HT/FT-based)"),
        list("first_half_conceded_rate",   "Fraction of conceded goals in first half (HT/FT-based)"),
        list("second_half_conceded_rate",  "Fraction of conceded goals in second half (HT/FT-based)"))) {
    fc <- c(fc, .build_windowed(root_desc[[1]], c("home","away"),
                                c(5,7), "half_rate", root_desc[[2]]))
  }

  # H2H features (single-window, use whole H2H list)
  fc[["h2h_btts"]]       <- .catalog_entry("h2h", "Count H2H matches where BTTS hit", side = "h2h")
  fc[["h2h_over15"]]     <- .catalog_entry("h2h", "Count H2H matches over 1.5", side = "h2h")
  fc[["h2h_over25"]]     <- .catalog_entry("h2h", "Count H2H matches over 2.5", side = "h2h")
  fc[["h2h_over35"]]     <- .catalog_entry("h2h", "Count H2H matches over 3.5", side = "h2h")
  fc[["h2h_avg_goals"]]  <- .catalog_entry("h2h", "Mean total goals in H2H", side = "h2h")
  fc[["h2h_blank_rate"]] <- .catalog_entry("h2h", "H2H rate where at least one team blanked", side = "h2h")
  fc[["h2h_home_clean_rate"]] <- .catalog_entry("h2h", "H2H clean-sheet rate for home", side = "h2h")

  # Poisson (meeting-point)
  fc[["lambda_h"]]     <- .catalog_entry("poisson", "Recency-weighted home lambda", side = "match")
  fc[["lambda_a"]]     <- .catalog_entry("poisson", "Recency-weighted away lambda", side = "match")
  fc[["total_lambda"]] <- .catalog_entry("poisson", "lambda_h + lambda_a",         side = "match")
  fc[["p_over_15"]]    <- .catalog_entry("poisson", "Poisson P(Over 1.5)",         side = "match")
  fc[["p_over_25"]]    <- .catalog_entry("poisson", "Poisson P(Over 2.5)",         side = "match")
  fc[["p_over_35"]]    <- .catalog_entry("poisson", "Poisson P(Over 3.5)",         side = "match")
  fc[["p_btts"]]       <- .catalog_entry("poisson", "Poisson P(BTTS)",             side = "match")
  fc[["p_home_win"]]   <- .catalog_entry("poisson", "Poisson P(Home win)",         side = "match")
  fc[["p_draw"]]       <- .catalog_entry("poisson", "Poisson P(Draw)",             side = "match")
  fc[["p_away_win"]]   <- .catalog_entry("poisson", "Poisson P(Away win)",         side = "match")

  # Standings & meeting-point
  fc[["home_rank"]] <- .catalog_entry("standings", "Home league position", side = "match")
  fc[["away_rank"]] <- .catalog_entry("standings", "Away league position", side = "match")
  fc[["home_ppg"]]  <- .catalog_entry("standings", "Home PPG",             side = "match")
  fc[["away_ppg"]]  <- .catalog_entry("standings", "Away PPG",             side = "match")
  fc[["home_gd"]]   <- .catalog_entry("standings", "Home goal difference", side = "match")
  fc[["away_gd"]]   <- .catalog_entry("standings", "Away goal difference", side = "match")
  fc[["rank_gap"]]  <- .catalog_entry("meeting_point", "Absolute rank gap",  side = "match")
  fc[["rank_diff"]] <- .catalog_entry("meeting_point", "Signed home - away rank", side = "match")
  fc[["ppg_diff"]]  <- .catalog_entry("meeting_point", "Home PPG minus away PPG", side = "match")
  fc[["gd_diff"]]   <- .catalog_entry("meeting_point", "Home GD minus away GD", side = "match")

  fc
})

# ─── Handy view — flat data.frame of the catalog ─────────────
feature_catalog_df <- function() {
  data.frame(
    feature = names(FEATURE_CATALOG),
    category = vapply(FEATURE_CATALOG, function(x) x$category, character(1)),
    side = vapply(FEATURE_CATALOG, function(x) x$side, character(1)),
    needs_stats = vapply(FEATURE_CATALOG, function(x) x$needs_stats, logical(1)),
    needs_goaltime = vapply(FEATURE_CATALOG, function(x) x$needs_goaltime, logical(1)),
    min_round = vapply(FEATURE_CATALOG, function(x) as.integer(x$min_round), integer(1)),
    desc = vapply(FEATURE_CATALOG, function(x) x$desc, character(1)),
    stringsAsFactors = FALSE, row.names = NULL
  )
}

# ─── Feature matrix builder (unchanged interface, per-feature windows now) ─
build_feature_matrix <- function(fixtures, feature_names, default_window = 5,
                                  main_only = FALSE, min_required = NULL,
                                  league_avg_goals = 1.35) {
  rows <- lapply(fixtures, function(ef) {
    feats <- compute_features(ef, feature_names, default_window = default_window,
                              main_only = main_only, min_required = min_required,
                              league_avg_goals = league_avg_goals)
    fx <- ef$fixture
    c(
      list(
        match_id       = as.character(fx$match_id %||% NA),
        league         = as.character(fx$league   %||% NA),
        country        = as.character(fx$country  %||% NA),
        home           = as.character(fx$home     %||% NA),
        away           = as.character(fx$away     %||% NA),
        result_status  = as.character(fx$result_status  %||% NA),
        result_ft_home = as.numeric(fx$result_ft_home  %||% NA),
        result_ft_away = as.numeric(fx$result_ft_away  %||% NA),
        result_ht_home = as.numeric(fx$result_ht_home  %||% NA),
        result_ht_away = as.numeric(fx$result_ht_away  %||% NA),
        result_total_corners = as.numeric(fx$result_total_corners %||% NA),
        result_home_corners  = as.numeric(fx$result_home_corners  %||% NA),
        result_away_corners  = as.numeric(fx$result_away_corners  %||% NA),
        result_total_cards   = as.numeric(fx$result_total_cards   %||% NA),
        result_home_cards    = as.numeric(fx$result_home_cards    %||% NA),
        result_away_cards    = as.numeric(fx$result_away_cards    %||% NA),
        result_total_shots   = as.numeric(fx$result_total_shots   %||% NA),
        result_total_sot     = as.numeric(fx$result_total_sot     %||% NA),
        source_file    = as.character(ef$source_file %||% "single"),
        source_role    = as.character(ef$source_role %||% "Tune")
      ),
      as.list(feats)
    )
  })
  do.call(rbind, lapply(rows, as.data.frame, stringsAsFactors = FALSE))
}

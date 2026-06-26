# ============================================================
# M.O.A.B Analysis — All markets, evidence-based filters
# Sourced by moab.R server. Operates on enriched_fixtures + standings.
# ============================================================
# Form: last 7 matches per team (enriched: ft/ht/goal_times/stats overall+1h+2h)
# H2H : last 5 matches between the two fixture teams (same enrichment)
# Standings: optional but used for opponent-strength soft feature
# ============================================================
# Every market output row has:
#   data_quality = "full"        -> stats filters passed (xG/SOT/etc)
#   data_quality = "structural"  -> structural-only pass (stats absent),
#                                    requires stricter structural gates
# ============================================================

library(dplyr)

# ════════════════════════════════════════════════════════════
# THRESHOLDS — tunable per market
# Two structural gates per market:
#   *_req_full       : N of last 7 required when stats are available
#   *_req_structural : N of last 7 required when stats are NOT available
#                      (stricter, since stats can't back the pattern)
# ════════════════════════════════════════════════════════════

DEFAULT_MOAB_THRESHOLDS <- list(
  # ── Phase 1 (GoalEdge proven 5, tightened with stats layer) ─────────
  ht_draw                  = 0.78,
  ht_draw_form_req         = 3,    # 3/5 last form matches with HT total <= 1 (both teams)
  ht_draw_h2h_req          = 3,    # 3/5 H2H with HT total <= 1
  
  fh_over_05               = 0.85,   # Poisson FH-goal probability floor
  fh_over_05_concede_req   = 3,      # both teams conceded FH in 3+/5 (both leaky)
  fh_over_05_score_req     = 3,      # at least one team scored FH in 3+/5
  fh_over_05_purpose_req   = 3,      # at least one team won 3+ of last 7 (recency-weighted)
  fh_over_05_in_form_req   = 2,      # AND that team won 2+ of last 5
  fh_over_05_max_draws_7   = 2,      # FT draws: each team < 3 in last 7
  fh_over_05_max_draws_5   = 1,      # FT draws: each team < 2 in last 5
  fh_over_05_max_ht_draws_7 = 2,     # HT draws: each team < 3 in last 7
  fh_over_05_max_ht_draws_5 = 1,     # HT draws: each team < 2 in last 5
  fh_over_05_monotony_max  = 3,      # FH monotony: neither has >3/7 with ht_for==0 OR ht_against==0
  fh_over_05_h2h_min_ht    = 1,      # most recent H2H had HT total >= 1 (and must exist)
  
  over_15_ft               = 0.92,
  over_15_min_xg           = 2.20,
  over_15_min_sot          = 7.0,
  over_15_min_big_chances  = 2.0,
  over_15_req_full         = 6,
  over_15_req_structural   = 8,
  # ── New weed-out rules (Round 1 trimming) ─────────────────
  over_15_recent_win_req   = 3,    # at least 1 team won 3+ of last 7 (recency-weighted purpose)
  over_15_in_form_req      = 2,    # AND that team also won 2+ of last 5 (still in form)
  over_15_max_draws_7      = 2,    # both teams must have <3 draws in last 7
  over_15_max_draws_5      = 1,    # both teams must have <2 draws in last 5
  over_15_monotony_max     = 4,    # neither team has 5+/7 with ft_for<=1 OR ft_against<=1
  over_15_h2h_min_total    = 2,    # most recent H2H must be over 1.5 (ft total >= 2)
  over_15_min_league_size  = 6,    # league must have >=6 teams (skip if standings absent)
  over_15_bottom_skip      = TRUE, # exclude bottom-feeders vs bottom-feeders (standings-aware)
  over_15_adjacent_skip    = TRUE, # exclude when teams are adjacent in standings (+-1)
  
  btts                     = 0.73,
  btts_min_team_xg         = 0.90,
  btts_min_team_sot        = 3.0,
  btts_min_opp_xg_conceded = 0.80,
  btts_req_full            = 6,
  btts_req_structural      = 7,
  
  under_45_ft              = 0.97,
  under_45_max_xg          = 2.80,
  under_45_max_sot         = 9.0,
  under_45_req_full        = 8,
  under_45_req_structural  = 9,
  under_45_h2h_req         = 4,    # 4/5 H2H had FT <= 4
  
  # ── Phase 2 (stats-strict markets; no stats = no prediction) ────────
  corners_over_95          = 0.78,
  corners_over_95_min_avg  = 10.0,
  corners_min_possession   = 95.0, # combined possession floor (out of 200)
  corners_min_touches_opp  = 30.0, # combined touches-in-opp-box avg
  
  home_corners_over_45     = 0.78,
  home_corners_min_avg     = 5.0,
  home_corners_min_touches = 18.0,
  
  away_corners_over_35     = 0.78,
  away_corners_min_avg     = 4.0,
  away_corners_min_touches = 14.0,
  
  match_cards_over_35      = 0.78,
  match_cards_min_avg      = 4.5,
  match_cards_min_fouls    = 22.0, # combined avg fouls per match
  
  home_bookings_over_15    = 0.78,
  home_bookings_min_avg    = 2.0,
  home_bookings_min_fouls  = 11.0,
  
  away_bookings_over_15    = 0.78,
  away_bookings_min_avg    = 2.0,
  away_bookings_min_fouls  = 11.0,
  
  shots_over_215           = 0.78,
  shots_min_avg            = 23.0,
  shots_min_sot_ratio      = 0.30, # combined SOT/Shots >= 30%
  shots_min_possession_bal = TRUE, # neither side under 30% possession
  
  sot_over_75              = 0.78,
  sot_min_avg              = 8.5,
  sot_min_big_chances      = 3.0,
  
  # ── Phase 3 (derivatives) ──────────────────────────────────────────
  two_h_over_15            = 0.78,
  two_h_min_xg             = 1.30,
  two_h_min_sot            = 5.0,
  two_h_req_full           = 6,
  two_h_req_structural     = 8,
  
  two_h_btts               = 0.65,
  two_h_btts_min_team_xg   = 0.55,
  
  home_clean_sheet         = 0.65,
  home_cs_max_opp_xg       = 0.80,
  home_cs_max_opp_sot      = 3.0,
  home_cs_req_full         = 5,
  home_cs_req_structural   = 7,
  
  away_clean_sheet         = 0.65,
  away_cs_max_opp_xg       = 0.80,
  away_cs_max_opp_sot      = 3.0,
  away_cs_req_full         = 5,
  away_cs_req_structural   = 7,
  
  home_win_to_nil          = 0.55,
  away_win_to_nil          = 0.55,
  
  both_halves_under_15     = 0.65,
  both_halves_max_combined_xg = 1.80,
  both_halves_max_combined_sot = 6.0,
  both_halves_req_full     = 6,
  both_halves_req_structural = 8
)

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0 && !is.na(a[1])) a else b

# ════════════════════════════════════════════════════════════
# PARSING HELPERS
# ════════════════════════════════════════════════════════════

parse_stat <- function(stats, key) {
  if (is.null(stats)) return(NA_real_)
  v <- stats[[key]]
  if (is.null(v) || length(v) == 0 || is.na(v)) return(NA_real_)
  v <- as.character(v)
  v <- gsub("%", "", v)
  suppressWarnings(as.numeric(v))
}

parse_minute <- function(t) {
  if (is.null(t) || is.na(t) || nchar(t) == 0) return(NA_integer_)
  t <- gsub("[^0-9+]", "", t)
  # For stoppage time like "45+5" or "90+3", return the BASE minute (45 or 90)
  # so the goal stays in the correct half (45+X = end of FH; 90+X = end of FT).
  if (grepl("\\+", t)) {
    p <- suppressWarnings(as.integer(strsplit(t, "\\+")[[1]]))
    if (length(p) >= 1 && !is.na(p[1])) return(p[1])
  }
  suppressWarnings(as.integer(t))
}

goals_in_window <- function(goal_times, min_minute, max_minute) {
  if (is.null(goal_times) || length(goal_times) == 0) return(0L)
  mins <- sapply(goal_times, parse_minute)
  mins <- mins[!is.na(mins)]
  sum(mins >= min_minute & mins <= max_minute)
}

safe_mean <- function(x) {
  x <- x[!is.na(x)]
  if (length(x) == 0) return(NA_real_)
  mean(x)
}

# True if the stats list has the key with usable numeric content
has_stat <- function(stats_list, key) {
  if (is.null(stats_list)) return(FALSE)
  v <- stats_list[[key]]
  if (is.null(v) || length(v) == 0) return(FALSE)
  v <- suppressWarnings(as.numeric(gsub("%", "", as.character(v))))
  !is.na(v)
}

# Returns TRUE if the form list has stats available across reasonable share of matches
form_has_stats <- function(form_list, key = "expected_goals_xg", min_share = 0.5) {
  if (length(form_list) == 0) return(FALSE)
  hits <- sum(sapply(form_list, function(m) has_stat(m$stats, key)))
  (hits / length(form_list)) >= min_share
}

# ════════════════════════════════════════════════════════════
# PER-TEAM AGGREGATE FEATURES (operates on full 10-match form list)
# ════════════════════════════════════════════════════════════

team_features <- function(form_list, team_name) {
  n <- length(form_list)
  if (n == 0) return(NULL)
  
  ft_for_vec     <- sapply(form_list, function(m) m$ft_for %||% NA)
  ft_against_vec <- sapply(form_list, function(m) m$ft_against %||% NA)
  ht_for_vec     <- sapply(form_list, function(m) m$ht_for %||% NA)
  ht_against_vec <- sapply(form_list, function(m) m$ht_against %||% NA)
  
  # 2H goal counts: derived as FT - HT. goal_times is NEVER used for halves.
  two_h_for_vec     <- ft_for_vec - ht_for_vec
  two_h_against_vec <- ft_against_vec - ht_against_vec
  two_h_for_vec[is.na(two_h_for_vec) | two_h_for_vec < 0] <- 0
  two_h_against_vec[is.na(two_h_against_vec) | two_h_against_vec < 0] <- 0
  
  # Overall stats
  xg_vec       <- sapply(form_list, function(m) parse_stat(m$stats, "expected_goals_xg"))
  shots_vec    <- sapply(form_list, function(m) parse_stat(m$stats, "total_shots"))
  sot_vec      <- sapply(form_list, function(m) parse_stat(m$stats, "shots_on_target"))
  poss_vec     <- sapply(form_list, function(m) parse_stat(m$stats, "ball_possession"))
  corners_vec  <- sapply(form_list, function(m) parse_stat(m$stats, "corner_kicks"))
  touches_vec  <- sapply(form_list, function(m) parse_stat(m$stats, "touches_in_opposition_box"))
  yellows_vec  <- sapply(form_list, function(m) parse_stat(m$stats, "yellow_cards"))
  reds_vec     <- sapply(form_list, function(m) parse_stat(m$stats, "red_cards"))
  big_ch_vec   <- sapply(form_list, function(m) parse_stat(m$stats, "big_chances"))
  fouls_vec    <- sapply(form_list, function(m) parse_stat(m$stats, "fouls"))
  
  # 1H stats (real, from /1st-half/ tab)
  xg_1h_vec    <- sapply(form_list, function(m) parse_stat(m$stats_1h, "expected_goals_xg"))
  shots_1h_vec <- sapply(form_list, function(m) parse_stat(m$stats_1h, "total_shots"))
  sot_1h_vec   <- sapply(form_list, function(m) parse_stat(m$stats_1h, "shots_on_target"))
  corners_1h_vec <- sapply(form_list, function(m) parse_stat(m$stats_1h, "corner_kicks"))
  big_ch_1h_vec  <- sapply(form_list, function(m) parse_stat(m$stats_1h, "big_chances"))
  
  # 2H stats (real)
  xg_2h_vec    <- sapply(form_list, function(m) parse_stat(m$stats_2h, "expected_goals_xg"))
  shots_2h_vec <- sapply(form_list, function(m) parse_stat(m$stats_2h, "total_shots"))
  sot_2h_vec   <- sapply(form_list, function(m) parse_stat(m$stats_2h, "shots_on_target"))
  big_ch_2h_vec <- sapply(form_list, function(m) parse_stat(m$stats_2h, "big_chances"))
  
  # Conversion ratios (quality vs quantity)
  sot_per_shot <- sot_vec / shots_vec
  sot_per_shot[!is.finite(sot_per_shot)] <- NA
  
  # FH goal counts: derived from ht_for + ht_against (single source of truth).
  fh_goal_counts <- ht_for_vec + ht_against_vec
  fh_goal_counts[is.na(fh_goal_counts)] <- 0
  two_h_goal_counts <- two_h_for_vec + two_h_against_vec
  
  stats_available <- form_has_stats(form_list, "expected_goals_xg", 0.5)
  
  list(
    n = n,
    stats_available = stats_available,
    # Goal counts (always available)
    avg_ft_for     = safe_mean(ft_for_vec),
    avg_ft_against = safe_mean(ft_against_vec),
    avg_ht_for     = safe_mean(ht_for_vec),
    avg_ht_against = safe_mean(ht_against_vec),
    avg_2h_for     = safe_mean(two_h_for_vec),
    avg_2h_against = safe_mean(two_h_against_vec),
    # Overall stats
    avg_xg          = safe_mean(xg_vec),
    avg_shots       = safe_mean(shots_vec),
    avg_sot         = safe_mean(sot_vec),
    avg_possession  = safe_mean(poss_vec),
    avg_corners     = safe_mean(corners_vec),
    avg_touches_opp = safe_mean(touches_vec),
    avg_yellows     = safe_mean(yellows_vec),
    avg_reds        = safe_mean(reds_vec),
    avg_big_chances = safe_mean(big_ch_vec),
    avg_fouls       = safe_mean(fouls_vec),
    avg_sot_per_shot = safe_mean(sot_per_shot),
    # 1H
    avg_fh_xg       = safe_mean(xg_1h_vec),
    avg_fh_shots    = safe_mean(shots_1h_vec),
    avg_fh_sot      = safe_mean(sot_1h_vec),
    avg_fh_corners  = safe_mean(corners_1h_vec),
    avg_fh_big_chances = safe_mean(big_ch_1h_vec),
    # 2H
    avg_2h_xg       = safe_mean(xg_2h_vec),
    avg_2h_shots    = safe_mean(shots_2h_vec),
    avg_2h_sot      = safe_mean(sot_2h_vec),
    avg_2h_big_chances = safe_mean(big_ch_2h_vec),
    # Structural counts — out of last 7
    n_ht_total_le_1 = sum((ht_for_vec + ht_against_vec) <= 1, na.rm = TRUE),
    n_ft_total_ge_2 = sum((ft_for_vec + ft_against_vec) >= 2, na.rm = TRUE),
    n_ft_total_le_4 = sum((ft_for_vec + ft_against_vec) <= 4, na.rm = TRUE),
    n_fh_goal       = sum(fh_goal_counts >= 1, na.rm = TRUE),
    n_2h_goal_ge_2  = sum(two_h_goal_counts >= 2, na.rm = TRUE),
    n_2h_btts       = sum(sapply(seq_along(form_list), function(i) {
      h2 <- two_h_for_vec[i]
      a2 <- two_h_against_vec[i]
      !is.na(h2) && !is.na(a2) && h2 >= 1 && a2 >= 1
    }), na.rm = TRUE),
    n_btts          = sum(ft_for_vec > 0 & ft_against_vec > 0, na.rm = TRUE),
    n_cs            = sum(ft_against_vec == 0, na.rm = TRUE),
    n_wins          = sum(ft_for_vec > ft_against_vec, na.rm = TRUE),
    n_both_halves_under_15 = sum(((ht_for_vec + ht_against_vec) <= 1) &
                                   ((two_h_for_vec + two_h_against_vec) <= 1), na.rm = TRUE),
    # Opponent identifiers for strength-of-schedule lookup
    opponent_names = sapply(form_list, function(m) {
      if (isTRUE(m$concerned_was_home)) m$away %||% NA else m$home %||% NA
    }),
    opponent_competitions = sapply(form_list, function(m) m$competition_full %||% NA)
  )
}

h2h_features <- function(h2h_list) {
  n <- length(h2h_list)
  if (n == 0) return(NULL)
  ft_totals <- sapply(h2h_list, function(m) (m$home_score %||% NA) + (m$away_score %||% NA))
  ht_totals <- sapply(h2h_list, function(m) {
    # Direct ht_for/ht_against (now always present after enrichment).
    if (!is.null(m$ht_for) && !is.na(m$ht_for) &&
        !is.null(m$ht_against) && !is.na(m$ht_against))
      return(m$ht_for + m$ht_against)
    NA_real_
  })
  fh_goal_each <- sapply(h2h_list, function(m) {
    if (!is.null(m$ht_for) && !is.na(m$ht_for) &&
        !is.null(m$ht_against) && !is.na(m$ht_against))
      return(m$ht_for >= 1 || m$ht_against >= 1)
    NA
  })
  list(
    n = n,
    avg_ft = safe_mean(ft_totals),
    avg_ht = safe_mean(ht_totals),
    n_ht_le_1 = sum(ht_totals <= 1, na.rm = TRUE),
    n_ft_le_4 = sum(ft_totals <= 4, na.rm = TRUE),
    n_ft_ge_2 = sum(ft_totals >= 2, na.rm = TRUE),
    n_fh_goal = sum(fh_goal_each, na.rm = TRUE),
    n_btts    = sum(sapply(h2h_list, function(m) (m$home_score %||% 0) > 0 && (m$away_score %||% 0) > 0))
  )
}

# ════════════════════════════════════════════════════════════
# OPPONENT STRENGTH (soft feature using standings as proxy)
# ════════════════════════════════════════════════════════════
# standings_df has columns: league, team, rank, mp, w, d, l, pts, gf, ga, gd
# For each form opponent, try (a) match its competition in standings, then (b)
# any league with that team. Missing -> NA (skipped, not penalized).

build_standings_lookup <- function(standings_df) {
  if (is.null(standings_df) || nrow(standings_df) == 0)
    return(list(by_lg_team = list(), by_team = list()))
  by_lg_team <- list()
  by_team <- list()
  for (i in seq_len(nrow(standings_df))) {
    lg <- as.character(standings_df$league[i])
    tm <- as.character(standings_df$team[i])
    key_lg <- paste0(lg, "::", tm)
    rec <- list(rank = standings_df$rank[i],
                ppg = standings_df$pts[i] / pmax(1, standings_df$mp[i]),
                gd  = standings_df$gd[i])
    by_lg_team[[key_lg]] <- rec
    # last write wins for cross-league (fine for soft feature)
    by_team[[tm]] <- rec
  }
  list(by_lg_team = by_lg_team, by_team = by_team)
}

opponent_strength <- function(opp_names, opp_competitions, lookup) {
  if (length(opp_names) == 0) return(list(avg_rank = NA_real_, avg_ppg = NA_real_, n_matched = 0))
  ranks <- numeric(); ppgs <- numeric()
  for (i in seq_along(opp_names)) {
    nm <- opp_names[i]; cp <- opp_competitions[i]
    if (is.na(nm) || nchar(nm) == 0) next
    rec <- NULL
    if (!is.na(cp)) rec <- lookup$by_lg_team[[paste0(cp, "::", nm)]]
    if (is.null(rec)) rec <- lookup$by_team[[nm]]
    if (is.null(rec)) next
    if (!is.null(rec$rank) && !is.na(rec$rank)) ranks <- c(ranks, rec$rank)
    if (!is.null(rec$ppg) && !is.na(rec$ppg)) ppgs <- c(ppgs, rec$ppg)
  }
  list(avg_rank = if (length(ranks) > 0) mean(ranks) else NA_real_,
       avg_ppg  = if (length(ppgs) > 0) mean(ppgs) else NA_real_,
       n_matched = length(ranks))
}

# ════════════════════════════════════════════════════════════
# POISSON HELPERS
# ════════════════════════════════════════════════════════════

prob_total_over <- function(lambda, line) {
  if (is.na(lambda) || lambda <= 0) return(NA_real_)
  1 - ppois(line, lambda)
}
prob_total_under <- function(lambda, line) {
  if (is.na(lambda) || lambda <= 0) return(NA_real_)
  ppois(line, lambda)
}
prob_btts <- function(lh, la) {
  if (is.na(lh) || is.na(la)) return(NA_real_)
  (1 - dpois(0, lh)) * (1 - dpois(0, la))
}
prob_ht_draw <- function(lh, la, max_goals = 5) {
  if (is.na(lh) || is.na(la)) return(NA_real_)
  pm <- outer(dpois(0:max_goals, lh), dpois(0:max_goals, la))
  sum(diag(pm))
}
prob_clean_sheet <- function(opp_lambda) {
  if (is.na(opp_lambda)) return(NA_real_)
  dpois(0, opp_lambda)
}
prob_win_to_nil <- function(team_l, opp_l) {
  if (is.na(team_l) || is.na(opp_l)) return(NA_real_)
  dpois(0, opp_l) * (1 - dpois(0, team_l))
}

# ════════════════════════════════════════════════════════════
# COMPUTE PER-FIXTURE FEATURES
# ════════════════════════════════════════════════════════════

compute_fixture <- function(enriched_fixture, league_avg_ft = 2.6, league_avg_ht = 1.1,
                            standings_lookup = list(by_lg_team = list(), by_team = list())) {
  fx <- enriched_fixture$fixture
  hf <- team_features(enriched_fixture$home_form, fx$home)
  af <- team_features(enriched_fixture$away_form, fx$away)
  h2hf <- h2h_features(enriched_fixture$h2h)
  if (is.null(hf) || is.null(af)) return(NULL)
  
  home_att <- hf$avg_ft_for     / (league_avg_ft / 2)
  home_def <- hf$avg_ft_against / (league_avg_ft / 2)
  away_att <- af$avg_ft_for     / (league_avg_ft / 2)
  away_def <- af$avg_ft_against / (league_avg_ft / 2)
  lambda_h_ft <- pmax(0.10, home_att * away_def * (league_avg_ft / 2))
  lambda_a_ft <- pmax(0.10, away_att * home_def * (league_avg_ft / 2))
  
  if (!is.null(h2hf) && !is.na(h2hf$avg_ft) && h2hf$avg_ft > 0) {
    h2h_ratio <- pmax(0.7, pmin(1.3, h2hf$avg_ft / (lambda_h_ft + lambda_a_ft)))
    lambda_h_ft <- lambda_h_ft * (0.8 + 0.2 * h2h_ratio)
    lambda_a_ft <- lambda_a_ft * (0.8 + 0.2 * h2h_ratio)
  }
  
  # ── HT LAMBDAS — exact mirror of GoalEdge cocktail.R math ──
  # GoalEdge computes HT lambdas DIRECTLY from last-5 HT scored/conceded,
  # NOT by scaling FT lambdas. This is the proven path; do not change.
  # Build last-5 HT averages right here (mirroring home_l5_scored_ht etc.)
  l5_ht_avg <- function(form_list, side) {
    if (is.null(form_list) || length(form_list) == 0) return(0)
    L <- form_list[seq_len(min(5, length(form_list)))]
    v <- sapply(L, function(m) {
      if (side == "for") m$ht_for %||% NA else m$ht_against %||% NA
    })
    v <- v[!is.na(v)]
    if (length(v) == 0) 0 else mean(v)
  }
  home_l5_scored_ht   <- l5_ht_avg(enriched_fixture$home_form, "for")
  home_l5_conceded_ht <- l5_ht_avg(enriched_fixture$home_form, "against")
  away_l5_scored_ht   <- l5_ht_avg(enriched_fixture$away_form, "for")
  away_l5_conceded_ht <- l5_ht_avg(enriched_fixture$away_form, "against")
  
  league_avg_ht_safe <- if (is.na(league_avg_ht) || league_avg_ht <= 0) 0.5 else league_avg_ht
  home_att_ht <- home_l5_scored_ht   / league_avg_ht_safe
  home_def_ht <- home_l5_conceded_ht / league_avg_ht_safe
  away_att_ht <- away_l5_scored_ht   / league_avg_ht_safe
  away_def_ht <- away_l5_conceded_ht / league_avg_ht_safe
  lambda_h_ht <- pmax(0.05, home_att_ht * away_def_ht * league_avg_ht_safe)
  lambda_a_ht <- pmax(0.05, away_att_ht * home_def_ht * league_avg_ht_safe)
  # H2H bias for HT (within 0.7-1.3 band, like GoalEdge)
  if (!is.null(h2hf) && !is.na(h2hf$avg_ht) && h2hf$avg_ht > 0) {
    poisson_avg_ht <- lambda_h_ht + lambda_a_ht
    if (poisson_avg_ht > 0) {
      h2h_ratio_ht <- pmax(0.7, pmin(1.3, h2hf$avg_ht / poisson_avg_ht))
      lambda_h_ht <- lambda_h_ht * (0.8 + 0.2 * h2h_ratio_ht)
      lambda_a_ht <- lambda_a_ht * (0.8 + 0.2 * h2h_ratio_ht)
    }
  }
  # 2H lambdas = FT - HT (still derived, but only used by 2H markets we'll tackle later)
  lambda_h_2h <- pmax(0.05, lambda_h_ft - lambda_h_ht)
  lambda_a_2h <- pmax(0.05, lambda_a_ft - lambda_a_ht)
  
  combined_fh_xg <- (hf$avg_fh_xg %||% 0) + (af$avg_fh_xg %||% 0)
  combined_2h_xg <- (hf$avg_2h_xg %||% 0) + (af$avg_2h_xg %||% 0)
  combined_xg    <- (hf$avg_xg %||% 0) + (af$avg_xg %||% 0)
  combined_fh_sot <- (hf$avg_fh_sot %||% 0) + (af$avg_fh_sot %||% 0)
  combined_2h_sot <- (hf$avg_2h_sot %||% 0) + (af$avg_2h_sot %||% 0)
  combined_sot   <- (hf$avg_sot %||% 0) + (af$avg_sot %||% 0)
  combined_shots <- (hf$avg_shots %||% 0) + (af$avg_shots %||% 0)
  combined_corners <- (hf$avg_corners %||% 0) + (af$avg_corners %||% 0)
  combined_touches_opp <- (hf$avg_touches_opp %||% 0) + (af$avg_touches_opp %||% 0)
  combined_cards <- (hf$avg_yellows %||% 0) + (af$avg_yellows %||% 0) +
    (hf$avg_reds %||% 0) + (af$avg_reds %||% 0)
  combined_fouls <- (hf$avg_fouls %||% 0) + (af$avg_fouls %||% 0)
  combined_big_chances <- (hf$avg_big_chances %||% 0) + (af$avg_big_chances %||% 0)
  combined_possession <- (hf$avg_possession %||% 0) + (af$avg_possession %||% 0)
  combined_sot_ratio <- if (combined_shots > 0) combined_sot / combined_shots else NA
  
  # Opponent strength (soft feature)
  hos <- opponent_strength(hf$opponent_names, hf$opponent_competitions, standings_lookup)
  aos <- opponent_strength(af$opponent_names, af$opponent_competitions, standings_lookup)
  
  stats_available <- hf$stats_available && af$stats_available
  
  list(
    fixture = fx,
    hf = hf, af = af, h2hf = h2hf,
    home_form_list = enriched_fixture$home_form,
    away_form_list = enriched_fixture$away_form,
    h2h_list       = enriched_fixture$h2h,
    stats_available = stats_available,
    lambda_h_ft = lambda_h_ft, lambda_a_ft = lambda_a_ft,
    lambda_h_ht = lambda_h_ht, lambda_a_ht = lambda_a_ht,
    lambda_h_2h = lambda_h_2h, lambda_a_2h = lambda_a_2h,
    total_lambda_ft = lambda_h_ft + lambda_a_ft,
    total_lambda_ht = lambda_h_ht + lambda_a_ht,
    total_lambda_2h = lambda_h_2h + lambda_a_2h,
    combined_fh_xg = combined_fh_xg, combined_2h_xg = combined_2h_xg, combined_xg = combined_xg,
    combined_fh_sot = combined_fh_sot, combined_2h_sot = combined_2h_sot, combined_sot = combined_sot,
    combined_shots = combined_shots, combined_corners = combined_corners,
    combined_touches_opp = combined_touches_opp,
    combined_cards = combined_cards, combined_fouls = combined_fouls,
    combined_big_chances = combined_big_chances,
    combined_possession = combined_possession,
    combined_sot_ratio = combined_sot_ratio,
    home_opp_strength = hos, away_opp_strength = aos
  )
}

# ════════════════════════════════════════════════════════════
# MARKET FUNCTIONS
# Each returns either NULL or list(market, confidence, pick, data_quality,
#                                  optional: expected, opp_strength_*)
# ════════════════════════════════════════════════════════════

# Helper: build the common output row with opp-strength context
make_pick <- function(market, conf, pick, data_quality, features, extra = list()) {
  base <- list(
    market = market, confidence = conf, pick = pick,
    data_quality = data_quality,
    opp_strength_home_avg_rank = features$home_opp_strength$avg_rank,
    opp_strength_home_avg_ppg  = features$home_opp_strength$avg_ppg,
    opp_strength_away_avg_rank = features$away_opp_strength$avg_rank,
    opp_strength_away_avg_ppg  = features$away_opp_strength$avg_ppg
  )
  c(base, extra)
}

# ── Phase 1 ─────────────────────────────────────────────────

# Count of last-N form matches where HT total (for+against) is <= line.
# Matches GoalEdge's `sum(c(H1_htt,...,H5_htt) <= 1)`. NA HT entries are excluded.
count_form_ht_total_le <- function(form_list, n = 5, line = 1) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    hf <- m$ht_for %||% NA
    ha <- m$ht_against %||% NA
    if (is.na(hf) || is.na(ha)) next
    if ((hf + ha) <= line) cnt <- cnt + 1L
  }
  cnt
}

# Count of last-N H2H matches where HT total <= line. Uses ht_for/ht_against
# now that H2H is enriched with HT data.
count_h2h_ht_total_le <- function(h2h_list, n = 5, line = 1) {
  if (is.null(h2h_list) || length(h2h_list) == 0) return(0L)
  L <- h2h_list[seq_len(min(n, length(h2h_list)))]
  cnt <- 0L
  for (m in L) {
    hf <- m$ht_for %||% NA
    ha <- m$ht_against %||% NA
    if (is.na(hf) || is.na(ha)) next
    if ((hf + ha) <= line) cnt <- cnt + 1L
  }
  cnt
}

# HT Draw — exact mirror of GoalEdge cocktail.R logic.
# Gates:
#   1. Poisson HT-draw probability >= 0.78
#   2. Last 5 home_form: 3+ had HT total <= 1
#   3. Last 5 away_form: 3+ had HT total <= 1
#   4. Last 5 h2h: 3+ had HT total <= 1
# No xG cap, no SOT cap, no stats requirement. Same standard for every fixture.
get_ht_draw <- function(f, T) {
  p <- prob_ht_draw(f$lambda_h_ht, f$lambda_a_ht)
  if (is.na(p) || p < T$ht_draw) return(NULL)
  if (count_form_ht_total_le(f$home_form_list, 5, 1) < T$ht_draw_form_req) return(NULL)
  if (count_form_ht_total_le(f$away_form_list, 5, 1) < T$ht_draw_form_req) return(NULL)
  if (count_h2h_ht_total_le(f$h2h_list, 5, 1) < T$ht_draw_h2h_req) return(NULL)
  dq <- if (f$stats_available) "full" else "structural"
  make_pick("HT Draw", p, "HT Draw", dq, f)
}

# Count of last-N form matches where HT total (for+against) is >= line.
# Symmetric counterpart to count_form_ht_total_le, used by FH Over markets.
count_form_ht_total_ge <- function(form_list, n = 5, line = 1) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    hf <- m$ht_for %||% NA
    ha <- m$ht_against %||% NA
    if (is.na(hf) || is.na(ha)) next
    if ((hf + ha) >= line) cnt <- cnt + 1L
  }
  cnt
}

# Count of last-N H2H matches where HT total >= line.
count_h2h_ht_total_ge <- function(h2h_list, n = 5, line = 1) {
  if (is.null(h2h_list) || length(h2h_list) == 0) return(0L)
  L <- h2h_list[seq_len(min(n, length(h2h_list)))]
  cnt <- 0L
  for (m in L) {
    hf <- m$ht_for %||% NA
    ha <- m$ht_against %||% NA
    if (is.na(hf) || is.na(ha)) next
    if ((hf + ha) >= line) cnt <- cnt + 1L
  }
  cnt
}

# Average of a given FH stat key across last-N form matches (NA-safe).
# Used for FH xG, FH SOT, FH big chances etc.
form_avg_fh_stat <- function(form_list, n = 5, key) {
  if (is.null(form_list) || length(form_list) == 0) return(NA_real_)
  L <- form_list[seq_len(min(n, length(form_list)))]
  vals <- sapply(L, function(m) parse_stat(m$stats_1h, key))
  vals <- vals[!is.na(vals)]
  if (length(vals) == 0) return(NA_real_)
  mean(vals)
}

# Count of last-N form matches where THE TEAM ITSELF scored in FH (ht_for > 0).
# Count of last-N form matches where THE TEAM ITSELF scored in FH (ht_for > 0).
count_form_team_fh_goal <- function(form_list, n = 5) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    hf <- m$ht_for %||% NA
    if (is.na(hf)) next
    if (hf > 0) cnt <- cnt + 1L
  }
  cnt
}

# Count of last-N form matches where THE TEAM CONCEDED in FH (ht_against > 0).
count_form_team_fh_conceded <- function(form_list, n = 5) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    ha <- m$ht_against %||% NA
    if (is.na(ha)) next
    if (ha > 0) cnt <- cnt + 1L
  }
  cnt
}

# Count of last-N form matches that the team won (ft_for > ft_against).
# "Purpose gate" — teams that win are playing for something.
count_form_team_wins <- function(form_list, n = 5) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    f  <- m$ft_for     %||% NA
    a  <- m$ft_against %||% NA
    if (is.na(f) || is.na(a)) next
    if (f > a) cnt <- cnt + 1L
  }
  cnt
}

# FH Over 0.5 — structural-only logic (no stats required).
# Decided after diagnostics showed 97% of off-season fixtures lack FH stats.
# Approach: filter dead-match fixtures, require both defenses leaky in FH,
# require at least one proven FH scorer.
# Gates (all must pass):
#   1. Poisson FH-goal probability >= threshold (math floor)
#   2. Purpose gate: at least one team won 3+ of last 5 (not two dead teams)
#   3. Home conceded FH in 3+/5 (home defense leaks in FH)
#   4. Away conceded FH in 3+/5 (away defense leaks in FH)
#   5. At least one team scored FH in 3+/5 (proven FH scorer present)
# Count of last-N form matches that were HT draws (ht_for == ht_against).
count_form_team_ht_draws <- function(form_list, n) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    hf <- m$ht_for %||% NA; ha <- m$ht_against %||% NA
    if (is.na(hf) || is.na(ha)) next
    if (hf == ha) cnt <- cnt + 1L
  }
  cnt
}

# FH monotony helpers — same shape as FT monotony but on HT axes.
# A team is FH-monotonous on the SCORING side if ht_for == 0 in many recent matches.
# A team is FH-monotonous on the CONCEDING side if ht_against == 0 in many recent matches.
count_form_team_ht_for_zero <- function(form_list, n) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    hf <- m$ht_for %||% NA
    if (is.na(hf)) next
    if (hf == 0) cnt <- cnt + 1L
  }
  cnt
}

count_form_team_ht_against_zero <- function(form_list, n) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    ha <- m$ht_against %||% NA
    if (is.na(ha)) next
    if (ha == 0) cnt <- cnt + 1L
  }
  cnt
}

team_is_fh_monotonous <- function(form_list, n, max_count) {
  count_form_team_ht_for_zero(form_list, n) > max_count ||
    count_form_team_ht_against_zero(form_list, n) > max_count
}

# Stronger team's last match in HT context: was it an HT draw? did they score in FH?
team_last_match_was_ht_draw <- function(form_list) {
  if (is.null(form_list) || length(form_list) == 0) return(NA)
  m <- form_list[[1]]
  hf <- m$ht_for %||% NA; ha <- m$ht_against %||% NA
  if (is.na(hf) || is.na(ha)) return(NA)
  hf == ha
}
team_last_match_fh_scored <- function(form_list) {
  if (is.null(form_list) || length(form_list) == 0) return(NA)
  m <- form_list[[1]]
  hf <- m$ht_for %||% NA
  if (is.na(hf)) return(NA)
  hf >= 1
}

# FH Over 0.5 — full rule set mirroring Over 1.5's structure with HT-context adaptations.
# Demands every gate pass. No H2H = no pick (same standard as O1.5).
get_fh_over_05 <- function(f, T) {
  p <- 1 - (dpois(0, f$lambda_h_ht) * dpois(0, f$lambda_a_ht))
  if (is.na(p) || p < T$fh_over_05) return(NULL)
  
  # ── FH-specific gates (kept from previous version) ───────────────
  home_conceded <- count_form_team_fh_conceded(f$home_form_list, 5)
  away_conceded <- count_form_team_fh_conceded(f$away_form_list, 5)
  if (home_conceded < T$fh_over_05_concede_req) return(NULL)
  if (away_conceded < T$fh_over_05_concede_req) return(NULL)
  home_scored <- count_form_team_fh_goal(f$home_form_list, 5)
  away_scored <- count_form_team_fh_goal(f$away_form_list, 5)
  if (home_scored < T$fh_over_05_score_req &&
      away_scored < T$fh_over_05_score_req) return(NULL)
  
  # ── Rule 2: recency-weighted purpose (FT wins, unchanged from O1.5) ──
  home_w7 <- count_form_team_wins(f$home_form_list, 7)
  away_w7 <- count_form_team_wins(f$away_form_list, 7)
  home_w5 <- count_form_team_wins(f$home_form_list, 5)
  away_w5 <- count_form_team_wins(f$away_form_list, 5)
  home_in_form <- home_w7 >= T$fh_over_05_purpose_req && home_w5 >= T$fh_over_05_in_form_req
  away_in_form <- away_w7 >= T$fh_over_05_purpose_req && away_w5 >= T$fh_over_05_in_form_req
  if (!home_in_form && !away_in_form) return(NULL)
  
  # ── Rule 3: BOTH FT draw cap AND HT draw cap ────────────────────
  if (count_form_team_draws(f$home_form_list, 7) > T$fh_over_05_max_draws_7) return(NULL)
  if (count_form_team_draws(f$away_form_list, 7) > T$fh_over_05_max_draws_7) return(NULL)
  if (count_form_team_draws(f$home_form_list, 5) > T$fh_over_05_max_draws_5) return(NULL)
  if (count_form_team_draws(f$away_form_list, 5) > T$fh_over_05_max_draws_5) return(NULL)
  if (count_form_team_ht_draws(f$home_form_list, 7) > T$fh_over_05_max_ht_draws_7) return(NULL)
  if (count_form_team_ht_draws(f$away_form_list, 7) > T$fh_over_05_max_ht_draws_7) return(NULL)
  if (count_form_team_ht_draws(f$home_form_list, 5) > T$fh_over_05_max_ht_draws_5) return(NULL)
  if (count_form_team_ht_draws(f$away_form_list, 5) > T$fh_over_05_max_ht_draws_5) return(NULL)
  
  # ── Rule 4: FH monotony — neither team has > N/7 with ht_for==0 OR ht_against==0 ──
  if (team_is_fh_monotonous(f$home_form_list, 7, T$fh_over_05_monotony_max)) return(NULL)
  if (team_is_fh_monotonous(f$away_form_list, 7, T$fh_over_05_monotony_max)) return(NULL)
  
  # ── Rule 5: H2H must exist AND most recent had HT total >= 1 ────
  if (is.null(f$h2h_list) || length(f$h2h_list) == 0) return(NULL)
  last_h2h <- f$h2h_list[[1]]
  ht_h <- last_h2h$ht_for %||% NA
  ht_a <- last_h2h$ht_against %||% NA
  # HT must be present from the partial-score scrape; if it's missing, exclude.
  if (is.na(ht_h) || is.na(ht_a)) return(NULL)
  ht_total <- ht_h + ht_a
  if (is.na(ht_total) || ht_total < T$fh_over_05_h2h_min_ht) return(NULL)
  
  # ── Rule 6: stronger team's last match (HT context) — not HT draw AND scored FH ──
  ss <- stronger_team_side(f)
  if (!is.null(ss)) {
    strong_form <- if (ss == "home") f$home_form_list else f$away_form_list
    if (isTRUE(team_last_match_was_ht_draw(strong_form))) return(NULL)
    if (isTRUE(!team_last_match_fh_scored(strong_form))) return(NULL)
  }
  
  # ── Rule 7: standings (silent skip when absent on Round 1) ──────
  fx <- f$fixture
  league_size <- get_fixture_field(fx, "league_size")
  home_rank   <- get_fixture_field(fx, "home_rank")
  away_rank   <- get_fixture_field(fx, "away_rank")
  if (!is.na(league_size) && league_size < T$over_15_min_league_size) return(NULL)
  if (isTRUE(T$over_15_bottom_skip) &&
      !is.na(home_rank) && !is.na(away_rank) && !is.na(league_size)) {
    if (is_bottomfeeder(home_rank, league_size) &&
        is_bottomfeeder(away_rank, league_size)) return(NULL)
  }
  if (isTRUE(T$over_15_adjacent_skip) &&
      !is.na(home_rank) && !is.na(away_rank)) {
    if (abs(home_rank - away_rank) <= 1) return(NULL)
  }
  
  dq <- if (f$stats_available) "full" else "structural"
  make_pick("FH Over 0.5", p, "FH O0.5", dq, f)
}

# Count of last-N form matches where THE TEAM scored at least once FT (ft_for >= 1).
count_form_team_scored_ft <- function(form_list, n = 10) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    f <- m$ft_for %||% NA
    if (is.na(f)) next
    if (f >= 1) cnt <- cnt + 1L
  }
  cnt
}

# Count of last-N form matches that ended in a draw (ft_for == ft_against).
count_form_team_draws <- function(form_list, n) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    f <- m$ft_for %||% NA; a <- m$ft_against %||% NA
    if (is.na(f) || is.na(a)) next
    if (f == a) cnt <- cnt + 1L
  }
  cnt
}

# Score-monotony — checks EACH AXIS of the scoreline independently.
# A team is monotonous on the SCORING axis if their ft_for is <=1 in many recent
# matches (they never score more than 1, regardless of how much they concede).
# A team is monotonous on the CONCEDING axis if their ft_against is <=1 in many
# recent matches (defensive wall, regardless of how much they score).
# Example: scores 7-0, 6-0, 0-1, 4-1, 8-0 -> conceding axis is monotonous (always <=1)
# even though scoring swings wildly. The old combined check missed this.
count_form_team_for_le1 <- function(form_list, n) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    f <- m$ft_for %||% NA
    if (is.na(f)) next
    if (f <= 1) cnt <- cnt + 1L
  }
  cnt
}

count_form_team_against_le1 <- function(form_list, n) {
  if (is.null(form_list) || length(form_list) == 0) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    a <- m$ft_against %||% NA
    if (is.na(a)) next
    if (a <= 1) cnt <- cnt + 1L
  }
  cnt
}

# A team is monotonous if EITHER axis hits the threshold count.
team_is_monotonous <- function(form_list, n, max_count) {
  count_form_team_for_le1(form_list, n) > max_count ||
    count_form_team_against_le1(form_list, n) > max_count
}

# Helpers reading from the standings snapshot baked into the fixture row.
# All return NA-safe values; the calling code skips rules when data is absent.
get_fixture_field <- function(fx, key) {
  v <- fx[[key]]
  if (is.null(v)) return(NA)
  if (length(v) == 0) return(NA)
  v[1]
}

# Stronger team: more wins in last 5; tie -> last 7; still tie -> NULL.
# Returns "home", "away" or NULL.
stronger_team_side <- function(f) {
  h5 <- count_form_team_wins(f$home_form_list, 5)
  a5 <- count_form_team_wins(f$away_form_list, 5)
  if (h5 > a5) return("home")
  if (a5 > h5) return("away")
  h10 <- count_form_team_wins(f$home_form_list, 10)
  a10 <- count_form_team_wins(f$away_form_list, 10)
  if (h10 > a10) return("home")
  if (a10 > h10) return("away")
  NULL
}

# Did this team's most recent match (form[1]) end in a draw?
team_last_match_was_draw <- function(form_list) {
  if (is.null(form_list) || length(form_list) == 0) return(NA)
  m <- form_list[[1]]
  f <- m$ft_for %||% NA; a <- m$ft_against %||% NA
  if (is.na(f) || is.na(a)) return(NA)
  f == a
}

# Did this team score in their most recent match?
team_last_match_scored <- function(form_list) {
  if (is.null(form_list) || length(form_list) == 0) return(NA)
  m <- form_list[[1]]
  f <- m$ft_for %||% NA
  if (is.na(f)) return(NA)
  f >= 1
}

# Bottomfeeder definition (proportional to league size):
#   league_size <= 10  -> bottom 3
#   league_size <= 16  -> bottom 4
#   league_size  > 16  -> bottom 5
# Returns TRUE if rank is in that bottom slice. NA inputs -> FALSE (rule skipped).
is_bottomfeeder <- function(rank, league_size) {
  if (is.na(rank) || is.na(league_size)) return(FALSE)
  bottom_n <- if (league_size <= 10) 3 else if (league_size <= 16) 4 else 5
  rank > (league_size - bottom_n)
}

get_over_15_ft <- function(f, T) {
  p <- prob_total_over(f$total_lambda_ft, 1)
  if (is.na(p) || p < T$over_15_ft) return(NULL)
  
  # ════ FORM-BASED WEED-OUT GATES (always active) ═══════════════════
  # 1. Recency-weighted purpose: at least one team won 3+/7 AND that same
  #    team also won 2+/5 (in form, not just historically strong).
  home_w7 <- count_form_team_wins(f$home_form_list, 7)
  away_w7 <- count_form_team_wins(f$away_form_list, 7)
  home_w5 <- count_form_team_wins(f$home_form_list, 5)
  away_w5 <- count_form_team_wins(f$away_form_list, 5)
  home_in_form <- home_w7 >= T$over_15_recent_win_req && home_w5 >= T$over_15_in_form_req
  away_in_form <- away_w7 >= T$over_15_recent_win_req && away_w5 >= T$over_15_in_form_req
  if (!home_in_form && !away_in_form) return(NULL)
  
  # 2. Draw-stubbornness: each team must have < threshold draws in last 7 AND last 5
  if (count_form_team_draws(f$home_form_list, 7) > T$over_15_max_draws_7) return(NULL)
  if (count_form_team_draws(f$away_form_list, 7) > T$over_15_max_draws_7) return(NULL)
  if (count_form_team_draws(f$home_form_list, 5) > T$over_15_max_draws_5) return(NULL)
  if (count_form_team_draws(f$away_form_list, 5) > T$over_15_max_draws_5) return(NULL)
  
  # 3. Score monotony: neither team is monotonous on EITHER axis (scoring or conceding).
  #    Catches both attacking-rigid (always <=1 scored) and defensive-wall (always <=1 conceded) teams.
  if (team_is_monotonous(f$home_form_list, 7, T$over_15_monotony_max)) return(NULL)
  if (team_is_monotonous(f$away_form_list, 7, T$over_15_monotony_max)) return(NULL)
  
  # 4. Most recent H2H must be over 1.5 (ft total >= 2). No H2H = no pick.
  if (is.null(f$h2h_list) || length(f$h2h_list) == 0) return(NULL)
  last_h2h <- f$h2h_list[[1]]
  h_score <- last_h2h$home_score %||% NA
  a_score <- last_h2h$away_score %||% NA
  if (is.na(h_score) || is.na(a_score) ||
      (h_score + a_score) < T$over_15_h2h_min_total) return(NULL)
  
  # 5. Stronger team's last match: not a draw AND they scored
  ss <- stronger_team_side(f)
  if (!is.null(ss)) {
    strong_form <- if (ss == "home") f$home_form_list else f$away_form_list
    if (isTRUE(team_last_match_was_draw(strong_form))) return(NULL)
    if (isTRUE(!team_last_match_scored(strong_form))) return(NULL)
  }
  
  # ════ STANDINGS-BASED GATES (skip silently if data absent) ════════
  fx <- f$fixture
  league_size <- get_fixture_field(fx, "league_size")
  home_rank   <- get_fixture_field(fx, "home_rank")
  away_rank   <- get_fixture_field(fx, "away_rank")
  
  # 6. Minimum league size (skip if absent)
  if (!is.na(league_size) && league_size < T$over_15_min_league_size) return(NULL)
  
  # 7. Bottom-feeders battle: both teams in bottom slice -> skip
  if (isTRUE(T$over_15_bottom_skip) &&
      !is.na(home_rank) && !is.na(away_rank) && !is.na(league_size)) {
    if (is_bottomfeeder(home_rank, league_size) &&
        is_bottomfeeder(away_rank, league_size)) return(NULL)
  }
  
  # 8. Adjacent rankings (within +/-1): too unpredictable -> skip
  if (isTRUE(T$over_15_adjacent_skip) &&
      !is.na(home_rank) && !is.na(away_rank)) {
    if (abs(home_rank - away_rank) <= 1) return(NULL)
  }
  
  # ════ ORIGINAL LOGIC (the 89%/282 baseline) ═══════════════════════
  if (f$stats_available) {
    if (f$hf$n_ft_total_ge_2 < T$over_15_req_full && f$af$n_ft_total_ge_2 < T$over_15_req_full) return(NULL)
    if (!is.na(f$combined_xg) && f$combined_xg < T$over_15_min_xg) return(NULL)
    if (!is.na(f$combined_sot) && f$combined_sot < T$over_15_min_sot) return(NULL)
    if (!is.na(f$combined_big_chances) && f$combined_big_chances < T$over_15_min_big_chances) return(NULL)
    dq <- "full"
  } else {
    if (f$hf$n_ft_total_ge_2 < T$over_15_req_structural &&
        f$af$n_ft_total_ge_2 < T$over_15_req_structural) return(NULL)
    dq <- "structural"
  }
  make_pick("Over 1.5 FT", p, "O1.5 FT", dq, f)
}

get_btts <- function(f, T) {
  p <- prob_btts(f$lambda_h_ft, f$lambda_a_ft)
  if (is.na(p) || p < T$btts) return(NULL)
  if (f$stats_available) {
    if ((f$hf$avg_xg %||% 0) < T$btts_min_team_xg) return(NULL)
    if ((f$af$avg_xg %||% 0) < T$btts_min_team_xg) return(NULL)
    if ((f$hf$avg_sot %||% 0) < T$btts_min_team_sot) return(NULL)
    if ((f$af$avg_sot %||% 0) < T$btts_min_team_sot) return(NULL)
    # opponent leakiness — defense conceded must be high enough each side
    if ((f$af$avg_xg %||% 0) < T$btts_min_opp_xg_conceded) return(NULL)
    if ((f$hf$avg_xg %||% 0) < T$btts_min_opp_xg_conceded) return(NULL)
    if (f$hf$n_btts < T$btts_req_full && f$af$n_btts < T$btts_req_full) return(NULL)
    dq <- "full"
  } else {
    if (f$hf$n_btts < T$btts_req_structural || f$af$n_btts < T$btts_req_structural) return(NULL)
    dq <- "structural"
  }
  make_pick("BTTS", p, "BTTS Yes", dq, f)
}

get_under_45_ft <- function(f, T) {
  p <- prob_total_under(f$total_lambda_ft, 4)
  if (is.na(p) || p < T$under_45_ft) return(NULL)
  if (f$stats_available) {
    if (f$hf$n_ft_total_le_4 < T$under_45_req_full) return(NULL)
    if (f$af$n_ft_total_le_4 < T$under_45_req_full) return(NULL)
    if (!is.na(f$combined_xg) && f$combined_xg > T$under_45_max_xg) return(NULL)
    if (!is.na(f$combined_sot) && f$combined_sot > T$under_45_max_sot) return(NULL)
    dq <- "full"
  } else {
    if (f$hf$n_ft_total_le_4 < T$under_45_req_structural) return(NULL)
    if (f$af$n_ft_total_le_4 < T$under_45_req_structural) return(NULL)
    dq <- "structural"
  }
  if (!is.null(f$h2hf) && f$h2hf$n_ft_le_4 < T$under_45_h2h_req) return(NULL)
  make_pick("Under 4.5 FT", p, "U4.5 FT", dq, f)
}

# ── Phase 2 (STATS-STRICT: skip if no stats) ────────────────

get_corners_over_95 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- f$combined_corners
  if (is.na(lambda) || lambda < T$corners_over_95_min_avg) return(NULL)
  if (is.na(f$combined_touches_opp) || f$combined_touches_opp < T$corners_min_touches_opp) return(NULL)
  if (is.na(f$combined_possession) || f$combined_possession < T$corners_min_possession) return(NULL)
  p <- prob_total_over(lambda, 9)
  if (is.na(p) || p < T$corners_over_95) return(NULL)
  make_pick("Corners Over 9.5", p, "Corners O9.5", "full", f,
            list(expected = round(lambda, 2)))
}

get_home_corners_over_45 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- f$hf$avg_corners
  if (is.na(lambda) || lambda < T$home_corners_min_avg) return(NULL)
  if (is.na(f$hf$avg_touches_opp) || f$hf$avg_touches_opp < T$home_corners_min_touches) return(NULL)
  p <- prob_total_over(lambda, 4)
  if (is.na(p) || p < T$home_corners_over_45) return(NULL)
  make_pick("Home Corners Over 4.5", p, "H Corners O4.5", "full", f,
            list(expected = round(lambda, 2)))
}

get_away_corners_over_35 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- f$af$avg_corners
  if (is.na(lambda) || lambda < T$away_corners_min_avg) return(NULL)
  if (is.na(f$af$avg_touches_opp) || f$af$avg_touches_opp < T$away_corners_min_touches) return(NULL)
  p <- prob_total_over(lambda, 3)
  if (is.na(p) || p < T$away_corners_over_35) return(NULL)
  make_pick("Away Corners Over 3.5", p, "A Corners O3.5", "full", f,
            list(expected = round(lambda, 2)))
}

get_match_cards_over_35 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- f$combined_cards
  if (is.na(lambda) || lambda < T$match_cards_min_avg) return(NULL)
  if (is.na(f$combined_fouls) || f$combined_fouls < T$match_cards_min_fouls) return(NULL)
  p <- prob_total_over(lambda, 3)
  if (is.na(p) || p < T$match_cards_over_35) return(NULL)
  make_pick("Match Cards Over 3.5", p, "Cards O3.5", "full", f,
            list(expected = round(lambda, 2)))
}

get_home_bookings_over_15 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- (f$hf$avg_yellows %||% 0) + (f$hf$avg_reds %||% 0)
  if (is.na(lambda) || lambda < T$home_bookings_min_avg) return(NULL)
  if (is.na(f$hf$avg_fouls) || f$hf$avg_fouls < T$home_bookings_min_fouls) return(NULL)
  p <- prob_total_over(lambda, 1)
  if (is.na(p) || p < T$home_bookings_over_15) return(NULL)
  make_pick("Home Bookings Over 1.5", p, "H Cards O1.5", "full", f,
            list(expected = round(lambda, 2)))
}

get_away_bookings_over_15 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- (f$af$avg_yellows %||% 0) + (f$af$avg_reds %||% 0)
  if (is.na(lambda) || lambda < T$away_bookings_min_avg) return(NULL)
  if (is.na(f$af$avg_fouls) || f$af$avg_fouls < T$away_bookings_min_fouls) return(NULL)
  p <- prob_total_over(lambda, 1)
  if (is.na(p) || p < T$away_bookings_over_15) return(NULL)
  make_pick("Away Bookings Over 1.5", p, "A Cards O1.5", "full", f,
            list(expected = round(lambda, 2)))
}

get_shots_over_215 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- f$combined_shots
  if (is.na(lambda) || lambda < T$shots_min_avg) return(NULL)
  # Quality gate: SOT/Shots ratio
  if (is.na(f$combined_sot_ratio) || f$combined_sot_ratio < T$shots_min_sot_ratio) return(NULL)
  # Possession balance: neither side hopelessly outclassed (rough threshold)
  if (T$shots_min_possession_bal) {
    if ((f$hf$avg_possession %||% 50) < 30 || (f$af$avg_possession %||% 50) < 30) return(NULL)
  }
  p <- prob_total_over(lambda, 21)
  if (is.na(p) || p < T$shots_over_215) return(NULL)
  make_pick("Total Shots Over 21.5", p, "Shots O21.5", "full", f,
            list(expected = round(lambda, 2)))
}

get_sot_over_75 <- function(f, T) {
  if (!f$stats_available) return(NULL)
  lambda <- f$combined_sot
  if (is.na(lambda) || lambda < T$sot_min_avg) return(NULL)
  if (is.na(f$combined_big_chances) || f$combined_big_chances < T$sot_min_big_chances) return(NULL)
  p <- prob_total_over(lambda, 7)
  if (is.na(p) || p < T$sot_over_75) return(NULL)
  make_pick("SOT Over 7.5", p, "SOT O7.5", "full", f,
            list(expected = round(lambda, 2)))
}

# ── Phase 3 ─────────────────────────────────────────────────

get_2h_over_15 <- function(f, T) {
  p <- prob_total_over(f$total_lambda_2h, 1)
  if (is.na(p) || p < T$two_h_over_15) return(NULL)
  if (f$stats_available) {
    if (f$hf$n_2h_goal_ge_2 < T$two_h_req_full && f$af$n_2h_goal_ge_2 < T$two_h_req_full) return(NULL)
    if (!is.na(f$combined_2h_xg) && f$combined_2h_xg < T$two_h_min_xg) return(NULL)
    if (!is.na(f$combined_2h_sot) && f$combined_2h_sot < T$two_h_min_sot) return(NULL)
    dq <- "full"
  } else {
    if (f$hf$n_2h_goal_ge_2 < T$two_h_req_structural &&
        f$af$n_2h_goal_ge_2 < T$two_h_req_structural) return(NULL)
    dq <- "structural"
  }
  make_pick("2H Over 1.5", p, "2H O1.5", dq, f)
}

get_2h_btts <- function(f, T) {
  p <- prob_btts(f$lambda_h_2h, f$lambda_a_2h)
  if (is.na(p) || p < T$two_h_btts) return(NULL)
  dq <- if (f$stats_available) "full" else "structural"
  if (f$stats_available) {
    if ((f$hf$avg_2h_xg %||% 0) < T$two_h_btts_min_team_xg) return(NULL)
    if ((f$af$avg_2h_xg %||% 0) < T$two_h_btts_min_team_xg) return(NULL)
  } else {
    if (f$hf$n_2h_btts < 6 || f$af$n_2h_btts < 6) return(NULL)
  }
  make_pick("2H BTTS", p, "2H BTTS", dq, f)
}

get_home_clean_sheet <- function(f, T) {
  p <- prob_clean_sheet(f$lambda_a_ft)
  if (is.na(p) || p < T$home_clean_sheet) return(NULL)
  if (f$stats_available) {
    if ((f$af$avg_xg %||% 99) > T$home_cs_max_opp_xg) return(NULL)
    if ((f$af$avg_sot %||% 99) > T$home_cs_max_opp_sot) return(NULL)
    if (f$hf$n_cs < T$home_cs_req_full) return(NULL)
    dq <- "full"
  } else {
    if (f$hf$n_cs < T$home_cs_req_structural) return(NULL)
    dq <- "structural"
  }
  make_pick("Home Clean Sheet", p, "H CS", dq, f)
}

get_away_clean_sheet <- function(f, T) {
  p <- prob_clean_sheet(f$lambda_h_ft)
  if (is.na(p) || p < T$away_clean_sheet) return(NULL)
  if (f$stats_available) {
    if ((f$hf$avg_xg %||% 99) > T$away_cs_max_opp_xg) return(NULL)
    if ((f$hf$avg_sot %||% 99) > T$away_cs_max_opp_sot) return(NULL)
    if (f$af$n_cs < T$away_cs_req_full) return(NULL)
    dq <- "full"
  } else {
    if (f$af$n_cs < T$away_cs_req_structural) return(NULL)
    dq <- "structural"
  }
  make_pick("Away Clean Sheet", p, "A CS", dq, f)
}

get_home_win_to_nil <- function(f, T) {
  p <- prob_win_to_nil(f$lambda_h_ft, f$lambda_a_ft)
  if (is.na(p) || p < T$home_win_to_nil) return(NULL)
  dq <- if (f$stats_available) "full" else "structural"
  make_pick("Home Win to Nil", p, "H WTN", dq, f)
}

get_away_win_to_nil <- function(f, T) {
  p <- prob_win_to_nil(f$lambda_a_ft, f$lambda_h_ft)
  if (is.na(p) || p < T$away_win_to_nil) return(NULL)
  dq <- if (f$stats_available) "full" else "structural"
  make_pick("Away Win to Nil", p, "A WTN", dq, f)
}

get_both_halves_under_15 <- function(f, T) {
  p_fh <- prob_total_under(f$total_lambda_ht, 1)
  p_2h <- prob_total_under(f$total_lambda_2h, 1)
  if (is.na(p_fh) || is.na(p_2h)) return(NULL)
  p <- p_fh * p_2h
  if (p < T$both_halves_under_15) return(NULL)
  if (f$stats_available) {
    combined_total_xg <- (f$combined_fh_xg %||% 0) + (f$combined_2h_xg %||% 0)
    combined_total_sot <- (f$combined_fh_sot %||% 0) + (f$combined_2h_sot %||% 0)
    if (combined_total_xg > T$both_halves_max_combined_xg) return(NULL)
    if (combined_total_sot > T$both_halves_max_combined_sot) return(NULL)
    if (f$hf$n_both_halves_under_15 < T$both_halves_req_full) return(NULL)
    if (f$af$n_both_halves_under_15 < T$both_halves_req_full) return(NULL)
    dq <- "full"
  } else {
    if (f$hf$n_both_halves_under_15 < T$both_halves_req_structural) return(NULL)
    if (f$af$n_both_halves_under_15 < T$both_halves_req_structural) return(NULL)
    dq <- "structural"
  }
  make_pick("Both Halves Under 1.5", p, "BH U1.5", dq, f)
}

# ════════════════════════════════════════════════════════════
# REGISTRY
# ════════════════════════════════════════════════════════════

ALL_MARKETS <- list(
  ht_draw         = get_ht_draw,
  fh_over_05      = get_fh_over_05,
  over_15_ft      = get_over_15_ft,
  btts            = get_btts,
  under_45_ft     = get_under_45_ft,
  corners_o95     = get_corners_over_95,
  home_corners    = get_home_corners_over_45,
  away_corners    = get_away_corners_over_35,
  match_cards     = get_match_cards_over_35,
  home_bookings   = get_home_bookings_over_15,
  away_bookings   = get_away_bookings_over_15,
  shots_o215      = get_shots_over_215,
  sot_o75         = get_sot_over_75,
  two_h_o15       = get_2h_over_15,
  two_h_btts      = get_2h_btts,
  home_cs         = get_home_clean_sheet,
  away_cs         = get_away_clean_sheet,
  home_wtn        = get_home_win_to_nil,
  away_wtn        = get_away_win_to_nil,
  both_halves_u15 = get_both_halves_under_15
)

# ════════════════════════════════════════════════════════════
# MAIN DRIVER
# ════════════════════════════════════════════════════════════

run_moab_analysis <- function(enriched_list, thresholds = DEFAULT_MOAB_THRESHOLDS,
                              league_avg_ft = 2.6, league_avg_ht = 1.1,
                              standings = NULL) {
  if (length(enriched_list) == 0) return(list())
  lookup <- build_standings_lookup(standings)
  
  picks_by_market <- list()
  for (mkt in names(ALL_MARKETS)) picks_by_market[[mkt]] <- list()
  
  for (k in seq_along(enriched_list)) {
    ef <- enriched_list[[k]]
    if (is.null(ef) || is.null(ef$fixture)) next
    f <- tryCatch(compute_fixture(ef, league_avg_ft, league_avg_ht, lookup),
                  error = function(e) NULL)
    if (is.null(f)) next
    for (mkt in names(ALL_MARKETS)) {
      pick <- tryCatch(ALL_MARKETS[[mkt]](f, thresholds), error = function(e) NULL)
      if (is.null(pick)) next
      fx <- f$fixture
      row <- data.frame(
        market       = pick$market,
        country      = as.character(fx$country %||% NA),
        league       = as.character(fx$league %||% NA),
        home         = as.character(fx$home),
        away         = as.character(fx$away),
        fixture_date = as.character(fx$fixture_date),
        match_time   = as.character(fx$match_time %||% NA),
        pick         = pick$pick,
        confidence   = round(pick$confidence * 100, 1),
        data_quality = pick$data_quality,
        opp_strength_home_rank = round(pick$opp_strength_home_avg_rank %||% NA, 1),
        opp_strength_home_ppg  = round(pick$opp_strength_home_avg_ppg  %||% NA, 2),
        opp_strength_away_rank = round(pick$opp_strength_away_avg_rank %||% NA, 1),
        opp_strength_away_ppg  = round(pick$opp_strength_away_avg_ppg  %||% NA, 2),
        match_id     = as.character(fx$match_id %||% NA),
        match_url    = as.character(fx$match_url %||% NA),
        stringsAsFactors = FALSE
      )
      if (!is.null(pick$expected)) row$expected <- pick$expected
      picks_by_market[[mkt]][[length(picks_by_market[[mkt]]) + 1]] <- row
    }
  }
  out <- list()
  for (mkt in names(picks_by_market)) {
    rows <- picks_by_market[[mkt]]
    if (length(rows) == 0) out[[mkt]] <- data.frame()
    else out[[mkt]] <- do.call(rbind, rows) %>% arrange(desc(confidence))
  }
  out
}
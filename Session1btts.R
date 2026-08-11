# ============================================================
# markets/btts.R
# BTTS (Both Teams to Score) market configuration.
# ============================================================

market_btts <- list(
  # ── Identity ────────────────────────────────────────────────
  name = "BTTS",
  full_name = "Both Teams to Score",
  tier = 1,  # 1 = goals-only, 2 = mixed, 3 = stats-only
  description = "Both teams score at least one goal in the match",
  
  # ── Grading function ────────────────────────────────────────
  # Takes a row of the feature matrix, returns TRUE (win) / FALSE (loss) / NA (ungradable)
  grade_fn = function(row) {
    h <- row$result_ft_home
    a <- row$result_ft_away
    if (is.na(h) || is.na(a)) return(NA)
    (h >= 1) && (a >= 1)
  },
  
  # ── Features and threshold ranges to sweep ─────────────────
  # Grid search will try every combination of these threshold values.
  # Total combos = product of lengths of each range.
  threshold_grid = list(
    h_scored_n5      = 2:5,
    h_conceded_n5    = 2:5,
    a_scored_n5      = 2:5,
    a_conceded_n5    = 2:5
  ),
  
  # ── Ranking preferences ────────────────────────────────────
  min_picks_default = 30,   # floor for leaderboard visibility
  primary_sort = "hit_rate", # default sort
  
  # ── Window / filter defaults ───────────────────────────────
  window = 5,
  main_only = FALSE,        # rounds 1-4: leave off; round 9+: user can toggle on
  min_required = NULL,      # for identical-team analysis (Session 2)
  
  # ── Notes (free-text, shown in UI) ─────────────────────────
  notes = "Starter BTTS config. Rules require home AND away to have scored + conceded frequently. Expand with quality-adjusted variants (h_scored_2plus_n5, h_avg_goals_scored_n5) in Session 2."
)

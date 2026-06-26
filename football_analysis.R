# ============================================================
# Football Predictions — Complete Analysis Pipeline
# ============================================================
# Requires: data_frame from pipeline (after clean_data_frame)
# Output:   results for all markets
# ============================================================
# Markets:
#   1. First half over 0.5
#   2. Over 1.5 full time
#   3. Over 2.5 full time
#   4. Both teams to score (BTTS)
#   5. Double chance (1X or X2)
# ============================================================

library(dplyr)
library(purrr)
library(tidyr)


# ============================================================
# SCORE PARSER
# Extracts FT and HT goals from "2:1 (1:0)" format
# ============================================================

parse_score <- function(score_str) {
  tryCatch({
    score_str <- trimws(as.character(score_str))
    ft_part   <- trimws(gsub("\\(.*\\)", "", score_str))
    ft        <- as.integer(strsplit(ft_part, ":")[[1]])
    ht_part   <- trimws(gsub(".*\\((.*)\\).*", "\\1", score_str))
    ht        <- as.integer(strsplit(ht_part, ":")[[1]])
    list(
      ft_scored    = ft[1],
      ft_conceded  = ft[2],
      ht_scored    = ht[1],
      ht_conceded  = ht[2],
      ft_total     = ft[1] + ft[2],
      ht_total     = ht[1] + ht[2]
    )
  }, error = function(e) {
    list(ft_scored=0, ft_conceded=0, ht_scored=0, ht_conceded=0,
         ft_total=0, ht_total=0)
  })
}


# ============================================================
# STEP 1: PREPROCESSING
# Extract per-game stats from score strings
# ============================================================

preprocess <- function(data_frame) {
  
  message("=== STEP 1: PREPROCESSING ===")
  
  result <- data_frame
  
  # Parse H1-H5, A1-A5, H2H1-H2H5
  for (i in seq_len(nrow(result))) {
    
    for (j in 1:5) {
      hs <- parse_score(result[[paste0("H", j)]][i])
      result[[paste0("H", j, "_fts")]][i] <- hs$ft_scored
      result[[paste0("H", j, "_ftc")]][i] <- hs$ft_conceded
      result[[paste0("H", j, "_hts")]][i] <- hs$ht_scored
      result[[paste0("H", j, "_htc")]][i] <- hs$ht_conceded
      result[[paste0("H", j, "_ftt")]][i] <- hs$ft_total
      result[[paste0("H", j, "_htt")]][i] <- hs$ht_total
      
      as <- parse_score(result[[paste0("A", j)]][i])
      result[[paste0("A", j, "_fts")]][i] <- as$ft_scored
      result[[paste0("A", j, "_ftc")]][i] <- as$ft_conceded
      result[[paste0("A", j, "_hts")]][i] <- as$ht_scored
      result[[paste0("A", j, "_htc")]][i] <- as$ht_conceded
      result[[paste0("A", j, "_ftt")]][i] <- as$ft_total
      result[[paste0("A", j, "_htt")]][i] <- as$ht_total
      
      hs2 <- parse_score(result[[paste0("H2H", j)]][i])
      result[[paste0("H2H", j, "_ftt")]][i] <- hs2$ft_total
      result[[paste0("H2H", j, "_htt")]][i] <- hs2$ht_total
    }
  }
  
  message("  Score parsing done")
  
  # ── Last 5 averages ──
  result <- result %>%
    mutate(
      # Home team last 5 FT
      home_l5_scored_ft   = (H1_fts + H2_fts + H3_fts + H4_fts + H5_fts) / 5,
      home_l5_conceded_ft = (H1_ftc + H2_ftc + H3_ftc + H4_ftc + H5_ftc) / 5,
      # Home team last 5 HT
      home_l5_scored_ht   = (H1_hts + H2_hts + H3_hts + H4_hts + H5_hts) / 5,
      home_l5_conceded_ht = (H1_htc + H2_htc + H3_htc + H4_htc + H5_htc) / 5,
      
      # Away team last 5 FT
      away_l5_scored_ft   = (A1_fts + A2_fts + A3_fts + A4_fts + A5_fts) / 5,
      away_l5_conceded_ft = (A1_ftc + A2_ftc + A3_ftc + A4_ftc + A5_ftc) / 5,
      # Away team last 5 HT
      away_l5_scored_ht   = (A1_hts + A2_hts + A3_hts + A4_hts + A5_hts) / 5,
      away_l5_conceded_ht = (A1_htc + A2_htc + A3_htc + A4_htc + A5_htc) / 5,
      
      # H2H averages
      h2h_avg_ft = (H2H1_ftt + H2H2_ftt + H2H3_ftt + H2H4_ftt + H2H5_ftt) / 5,
      h2h_avg_ht = (H2H1_htt + H2H2_htt + H2H3_htt + H2H4_htt + H2H5_htt) / 5,
      
      # Season averages from standings
      home_season_scored_ft   = round(home_goal_scored   / home_match_played, 3),
      home_season_conceded_ft = round(home_goal_conceded / home_match_played, 3),
      away_season_scored_ft   = round(away_goal_scored   / away_match_played, 3),
      away_season_conceded_ft = round(away_goal_conceded / away_match_played, 3),
      
      # 60/40 weighted combined lambdas FT
      home_attack_ft  = 0.6 * home_l5_scored_ft   + 0.4 * home_season_scored_ft,
      home_defense_ft = 0.6 * home_l5_conceded_ft + 0.4 * home_season_conceded_ft,
      away_attack_ft  = 0.6 * away_l5_scored_ft   + 0.4 * away_season_scored_ft,
      away_defense_ft = 0.6 * away_l5_conceded_ft + 0.4 * away_season_conceded_ft,
      
      # HT lambdas from last 5 only (no season HT data available)
      home_attack_ht  = home_l5_scored_ht,
      home_defense_ht = home_l5_conceded_ht,
      away_attack_ht  = away_l5_scored_ht,
      away_defense_ht = away_l5_conceded_ht
    )
  
  # ── League averages for normalization ──
  league_avgs <- result %>%
    group_by(league) %>%
    summarise(
      league_avg_attack_ft = mean(c(home_season_scored_ft, away_season_scored_ft), na.rm = TRUE),
      league_avg_attack_ht = mean(c(home_l5_scored_ht, away_l5_scored_ht), na.rm = TRUE),
      .groups = "drop"
    )
  
  result <- result %>% left_join(league_avgs, by = "league")
  
  message("  Averages and league norms done")
  message("=== STEP 1 DONE ===\n")
  return(result)
}


# ============================================================
# STEP 2: POISSON MODEL
# Calculate match probabilities for all markets
# ============================================================

poisson_probs <- function(lambda_home, lambda_away, max_goals = 8) {
  # P(home scores i, away scores j)
  prob_matrix <- outer(
    dpois(0:max_goals, lambda_home),
    dpois(0:max_goals, lambda_away)
  )
  
  # Market probabilities
  # Over 1.5 = 1 - P(total <= 1)
  p_over15 <- 1 - sum(prob_matrix[1:2, 1]) - sum(prob_matrix[1, 2:2])
  p_over15 <- 1 - (prob_matrix[1,1] + prob_matrix[1,2] + prob_matrix[2,1])
  
  # Over 2.5 = 1 - P(total <= 2)
  p_over25 <- 1 - (prob_matrix[1,1] + prob_matrix[1,2] + prob_matrix[2,1] +
                     prob_matrix[1,3] + prob_matrix[2,2] + prob_matrix[3,1])
  
  # BTTS = P(home >= 1) * P(away >= 1)
  p_btts <- (1 - dpois(0, lambda_home)) * (1 - dpois(0, lambda_away))
  
  # Home win
  p_home_win <- sum(prob_matrix[lower.tri(prob_matrix)])
  
  # Draw
  p_draw <- sum(diag(prob_matrix))
  
  # Away win
  p_away_win <- sum(prob_matrix[upper.tri(prob_matrix)])
  
  # Double chance
  p_1x <- p_home_win + p_draw
  p_x2 <- p_away_win + p_draw
  
  list(
    p_over15   = round(p_over15, 4),
    p_over25   = round(p_over25, 4),
    p_btts     = round(p_btts, 4),
    p_home_win = round(p_home_win, 4),
    p_draw     = round(p_draw, 4),
    p_away_win = round(p_away_win, 4),
    p_1x       = round(p_1x, 4),
    p_x2       = round(p_x2, 4)
  )
}


poisson_ht_prob <- function(lambda_home_ht, lambda_away_ht) {
  # P(first half over 0.5) = 1 - P(0 goals in first half)
  p_fh_over05 <- 1 - dpois(0, lambda_home_ht) * dpois(0, lambda_away_ht)
  round(p_fh_over05, 4)
}


calculate_probabilities <- function(analysis_df) {
  
  message("=== STEP 2: POISSON PROBABILITIES ===")
  
  result <- analysis_df
  
  # Initialise probability columns
  result$p_over15   <- NA_real_
  result$p_over25   <- NA_real_
  result$p_btts     <- NA_real_
  result$p_fh_over05 <- NA_real_
  result$p_home_win <- NA_real_
  result$p_draw     <- NA_real_
  result$p_away_win <- NA_real_
  result$p_1x       <- NA_real_
  result$p_x2       <- NA_real_
  
  for (i in seq_len(nrow(result))) {
    
    # Normalize attack/defense by league average
    league_avg_ft <- result$league_avg_attack_ft[i]
    league_avg_ht <- result$league_avg_attack_ht[i]
    
    if (is.na(league_avg_ft) || league_avg_ft == 0) league_avg_ft <- 1.3
    if (is.na(league_avg_ht) || league_avg_ht == 0) league_avg_ht <- 0.5
    
    # Attack and defense strengths normalized by league average
    home_att_str <- result$home_attack_ft[i] / league_avg_ft
    home_def_str <- result$home_defense_ft[i] / league_avg_ft
    away_att_str <- result$away_attack_ft[i] / league_avg_ft
    away_def_str <- result$away_defense_ft[i] / league_avg_ft
    
    # Expected goals FT
    lambda_home_ft <- home_att_str * away_def_str * league_avg_ft
    lambda_away_ft <- away_att_str * home_def_str * league_avg_ft
    
    # H2H modifier — nudge lambda based on H2H goal history
    h2h_avg     <- suppressWarnings(as.numeric(result$h2h_avg_ft[i]))
    poisson_avg <- suppressWarnings(as.numeric(lambda_home_ft + lambda_away_ft))
    if (!is.na(h2h_avg) && !is.na(poisson_avg) && h2h_avg > 0 && poisson_avg > 0) {
      h2h_ratio      <- max(0.7, min(1.3, h2h_avg / poisson_avg))
      lambda_home_ft <- lambda_home_ft * (0.8 + 0.2 * h2h_ratio)
      lambda_away_ft <- lambda_away_ft * (0.8 + 0.2 * h2h_ratio)
    }
    
    # Ensure lambdas are positive
    lambda_home_ft <- max(0.1, lambda_home_ft, na.rm = TRUE)
    lambda_away_ft <- max(0.1, lambda_away_ft, na.rm = TRUE)
    
    # FT probabilities
    probs <- poisson_probs(lambda_home_ft, lambda_away_ft)
    result$p_over15[i]   <- probs$p_over15
    result$p_over25[i]   <- probs$p_over25
    result$p_btts[i]     <- probs$p_btts
    result$p_home_win[i] <- probs$p_home_win
    result$p_draw[i]     <- probs$p_draw
    result$p_away_win[i] <- probs$p_away_win
    result$p_1x[i]       <- probs$p_1x
    result$p_x2[i]       <- probs$p_x2
    
    # HT lambda
    home_att_ht <- result$home_attack_ht[i] / league_avg_ht
    home_def_ht <- result$home_defense_ht[i] / league_avg_ht
    away_att_ht <- result$away_attack_ht[i] / league_avg_ht
    away_def_ht <- result$away_defense_ht[i] / league_avg_ht
    
    lambda_home_ht <- max(0.05, home_att_ht * away_def_ht * league_avg_ht, na.rm = TRUE)
    lambda_away_ht <- max(0.05, away_att_ht * home_def_ht * league_avg_ht, na.rm = TRUE)
    
    # H2H HT modifier
    h2h_avg_ht     <- suppressWarnings(as.numeric(result$h2h_avg_ht[i]))
    poisson_avg_ht <- suppressWarnings(as.numeric(lambda_home_ht + lambda_away_ht))
    if (!is.na(h2h_avg_ht) && !is.na(poisson_avg_ht) && h2h_avg_ht > 0 && poisson_avg_ht > 0) {
      h2h_ratio_ht   <- max(0.7, min(1.3, h2h_avg_ht / poisson_avg_ht))
      lambda_home_ht <- lambda_home_ht * (0.8 + 0.2 * h2h_ratio_ht)
      lambda_away_ht <- lambda_away_ht * (0.8 + 0.2 * h2h_ratio_ht)
    }
    
    result$p_fh_over05[i] <- poisson_ht_prob(lambda_home_ht, lambda_away_ht)
  }
  
  message("  Probabilities calculated for ", nrow(result), " fixtures")
  message("=== STEP 2 DONE ===\n")
  return(result)
}


# ============================================================
# STEP 3: MARKET FILTERS
# Apply thresholds and form confirmation per market
# ============================================================

# Threshold settings — adjust these to be more/less selective
THRESHOLDS <- list(
  fh_over05  = 0.85,   # First half over 0.5
  over15     = 0.85,   # Over 1.5 FT
  over25     = 0.70,   # Over 2.5 FT
  btts       = 0.67,   # BTTS
  dbl_chance = 0.86    # Double chance
)

# ── Helper: did team score in first half in at least n of last 5? ──
fh_score_rate <- function(row, prefix, n) {
  cols <- paste0(prefix, 1:5, "_hts")
  vals <- as.numeric(unlist(row[cols]))
  sum(vals > 0, na.rm = TRUE) >= n
}

# ── Helper: did both teams score in at least n of last 5 FT? ──
btts_rate <- function(row, n) {
  h_scored <- as.numeric(unlist(row[paste0("H", 1:5, "_fts")]))
  h_conceded <- as.numeric(unlist(row[paste0("H", 1:5, "_ftc")]))
  a_scored <- as.numeric(unlist(row[paste0("A", 1:5, "_fts")]))
  a_conceded <- as.numeric(unlist(row[paste0("A", 1:5, "_ftc")]))
  home_both <- sum(h_scored > 0 & h_conceded > 0, na.rm = TRUE)
  away_both <- sum(a_scored > 0 & a_conceded > 0, na.rm = TRUE)
  (home_both + away_both) / 2 >= n
}


get_fh_over05 <- function(analysis_df) {
  # Leagues with no H2H first half data — still include but rely more on form
  no_h2h <- c("NIFL Premiership", "Turkish Sueperlig")
  
  analysis_df %>%
    filter(p_fh_over05 >= THRESHOLDS$fh_over05) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    filter(!(league %in% no_h2h) | H2H1 == "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      # Home team scored in first half in at least 3 of last 5
      sum(c(H1_hts, H2_hts, H3_hts, H4_hts, H5_hts) > 0, na.rm = TRUE) >= 3 |
        # Away team scored in first half in at least 3 of last 5
        sum(c(A1_hts, A2_hts, A3_hts, A4_hts, A5_hts) > 0, na.rm = TRUE) >= 3
    ) %>%
    ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id, home_position, away_position,
           p_fh_over05, h2h_avg_ht, home_l5_scored_ht, away_l5_scored_ht) %>%
    arrange(desc(p_fh_over05)) %>%
    mutate(market = "First Half Over 0.5",
           confidence = paste0(round(p_fh_over05 * 100, 1), "%"))
}


get_over15 <- function(analysis_df) {
  analysis_df %>%
    filter(p_over15 >= THRESHOLDS$over15) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      # At least 3 of last 5 home games had 2+ goals
      sum(c(H1_ftt, H2_ftt, H3_ftt, H4_ftt, H5_ftt) >= 2, na.rm = TRUE) >= 3 |
        sum(c(A1_ftt, A2_ftt, A3_ftt, A4_ftt, A5_ftt) >= 2, na.rm = TRUE) >= 3
    ) %>%
    ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id, home_position, away_position,
           p_over15, h2h_avg_ft, home_l5_scored_ft, away_l5_scored_ft) %>%
    arrange(desc(p_over15)) %>%
    mutate(market = "Over 1.5 FT",
           confidence = paste0(round(p_over15 * 100, 1), "%"))
}


get_over25 <- function(analysis_df) {
  analysis_df %>%
    filter(p_over25 >= THRESHOLDS$over25) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      # At least 2 of last 5 H2H had 3+ goals
      sum(c(H2H1_ftt, H2H2_ftt, H2H3_ftt, H2H4_ftt, H2H5_ftt) >= 3, na.rm = TRUE) >= 2 &
        sum(c(H1_ftt, H2_ftt, H3_ftt, H4_ftt, H5_ftt) >= 3, na.rm = TRUE) >= 2 &
        sum(c(A1_ftt, A2_ftt, A3_ftt, A4_ftt, A5_ftt) >= 3, na.rm = TRUE) >= 2
    ) %>%
    ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id, home_position, away_position,
           p_over25, h2h_avg_ft) %>%
    arrange(desc(p_over25)) %>%
    mutate(market = "Over 2.5 FT",
           confidence = paste0(round(p_over25 * 100, 1), "%"))
}


get_btts <- function(analysis_df) {
  analysis_df %>%
    filter(p_btts >= THRESHOLDS$btts) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter(
      # Home team scored AND conceded in at least 3 of last 5
      sum(c(H1_fts, H2_fts, H3_fts, H4_fts, H5_fts) > 0 &
            c(H1_ftc, H2_ftc, H3_ftc, H4_ftc, H5_ftc) > 0, na.rm = TRUE) >= 3 |
        # Away team scored AND conceded in at least 3 of last 5
        sum(c(A1_fts, A2_fts, A3_fts, A4_fts, A5_fts) > 0 &
              c(A1_ftc, A2_ftc, A3_ftc, A4_ftc, A5_ftc) > 0, na.rm = TRUE) >= 3
    ) %>%
    ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id, home_position, away_position, p_btts) %>%
    arrange(desc(p_btts)) %>%
    mutate(market = "BTTS",
           confidence = paste0(round(p_btts * 100, 1), "%"))
}


get_double_chance <- function(analysis_df) {
  analysis_df %>%
    filter(abs(home_position - away_position) >= 3) %>%
    mutate(
      stronger     = if_else(home_point > away_point, "home", "away"),
      p_dbl_chance = if_else(home_point > away_point, p_1x, p_x2)
    ) %>%
    filter(p_dbl_chance >= THRESHOLDS$dbl_chance) %>%
    filter(H1 != "0:0 (0:0)", A1 != "0:0 (0:0)") %>%
    rowwise() %>%
    filter({
      # Stronger team has NOT LOST in at least 3 of last 5
      # Not lost = ft_scored >= ft_conceded (win or draw)
      if (stronger == "home") {
        sum(c(H1_fts >= H1_ftc, H2_fts >= H2_ftc, H3_fts >= H3_ftc,
              H4_fts >= H4_ftc, H5_fts >= H5_ftc), na.rm = TRUE) >= 3
      } else {
        sum(c(A1_fts >= A1_ftc, A2_fts >= A2_ftc, A3_fts >= A3_ftc,
              A4_fts >= A4_ftc, A5_fts >= A5_ftc), na.rm = TRUE) >= 3
      }
    }) %>%
    ungroup() %>%
    select(league, home, away, fixture_date, home_href, away_href, competition_id, home_position, away_position,
           stronger, p_dbl_chance) %>%
    arrange(desc(p_dbl_chance)) %>%
    mutate(market     = if_else(stronger == "home", "Double Chance 1X", "Double Chance X2"),
           confidence = paste0(round(p_dbl_chance * 100, 1), "%"))
}


# ============================================================
# STEP 4: RUN ALL MARKETS
# ============================================================

run_analysis <- function(analysis_df,
                         thresh_fh   = THRESHOLDS$fh_over05,
                         thresh_o15  = THRESHOLDS$over15,
                         thresh_o25  = THRESHOLDS$over25,
                         thresh_btts = THRESHOLDS$btts,
                         thresh_dc   = THRESHOLDS$dbl_chance) {
  
  # Override global THRESHOLDS so all filter functions pick up new values
  THRESHOLDS$fh_over05  <<- thresh_fh
  THRESHOLDS$over15     <<- thresh_o15
  THRESHOLDS$over25     <<- thresh_o25
  THRESHOLDS$btts       <<- thresh_btts
  THRESHOLDS$dbl_chance <<- thresh_dc
  
  message("=== STEP 3: MARKET FILTERS ===")
  message("Thresholds: FH=", thresh_fh, " O1.5=", thresh_o15,
          " O2.5=", thresh_o25, " BTTS=", thresh_btts, " DC=", thresh_dc, "\n")
  
  fh_picks     <- get_fh_over05(analysis_df)
  over15_picks <- get_over15(analysis_df)
  over25_picks <- get_over25(analysis_df)
  btts_picks   <- get_btts(analysis_df)
  dc_picks     <- get_double_chance(analysis_df)
  
  message("Results:")
  message("  First Half Over 0.5 : ", nrow(fh_picks))
  message("  Over 1.5 FT         : ", nrow(over15_picks))
  message("  Over 2.5 FT         : ", nrow(over25_picks))
  message("  BTTS                : ", nrow(btts_picks))
  message("  Double Chance       : ", nrow(dc_picks))
  message("\n=== ANALYSIS DONE ===")
  
  list(
    fh_over05    = fh_picks,
    over15       = over15_picks,
    over25       = over25_picks,
    btts         = btts_picks,
    double_chance = dc_picks
  )
}


# ============================================================
# RUN
# ============================================================
# analysis_df <- preprocess(data_frame)
# analysis_df <- calculate_probabilities(analysis_df)
# results     <- run_analysis(analysis_df)
# 
# # View results per market
# print(results$fh_over05)
# print(results$over15)
# print(results$over25)
# print(results$btts)
# print(results$double_chance)

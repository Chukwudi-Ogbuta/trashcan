# ============================================================
# M.O.A.B Analysis — OVER 2.5 FT (production)
# ============================================================
# Validated on patched + SB-filtered data across R1+R2+R3.
#
# Filters:
#   Baseline:
#     p_over_25 >= 0.70
#     total_lambda >= 2.5
#     both teams scored 3+ total goals in 2+ of last 5 matches
#     both teams scored 3+ total goals in 4+ of last 7 matches
#     home clean sheets in last 5 <= 1
#     away clean sheets in last 5 == 0
#   Second-stage filters (from filter exploration):
#     home team's most recent match HT total > 1
#     sum of draws across both teams' last 7 matches <= 4
# ============================================================

suppressPackageStartupMessages(library(dplyr))

`%||%` <- function(a, b) {
  if (!is.null(a) && length(a) > 0 && !all(is.na(a))) a else b
}

# ════════════════════════════════════════════════════════════
# THRESHOLDS
# ════════════════════════════════════════════════════════════
DEFAULT_MOAB_THRESHOLDS <- list(
  o25_min_p           = 0.70,
  o25_min_total_lam   = 2.5,
  o25_min_both_o25_5  = 2,
  o25_min_both_o25_7  = 4,
  o25_max_h_cs_5      = 1,
  o25_max_a_cs_5      = 0,
  o25_min_h_last_ht   = 2,   # H1_ht total >= 2 (i.e. > 1)
  o25_max_sum_draws_7 = 4    # h_draws_7 + a_draws_7 <= 4
)

# ════════════════════════════════════════════════════════════
# UTILITIES
# ════════════════════════════════════════════════════════════
recency_weighted_mean <- function(values) {
  values <- values[!is.na(values)]
  if (!length(values)) return(NA_real_)
  weights <- ifelse(seq_along(values) <= 3, 2, 1)[seq_along(values)]
  sum(values * weights) / sum(weights)
}

cnt_total_ge <- function(form_list, n, thresh) {
  if (is.null(form_list) || !length(form_list)) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    f <- m$ft_for %||% NA; a <- m$ft_against %||% NA
    if (!is.na(f) && !is.na(a) && (f + a) >= thresh) cnt <- cnt + 1L
  }
  cnt
}

cnt_clean_sheets <- function(form_list, n) {
  if (is.null(form_list) || !length(form_list)) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    a <- m$ft_against %||% NA
    if (!is.na(a) && a == 0) cnt <- cnt + 1L
  }
  cnt
}

cnt_draws <- function(form_list, n) {
  if (is.null(form_list) || !length(form_list)) return(0L)
  L <- form_list[seq_len(min(n, length(form_list)))]
  cnt <- 0L
  for (m in L) {
    f <- m$ft_for %||% NA; a <- m$ft_against %||% NA
    if (!is.na(f) && !is.na(a) && f == a) cnt <- cnt + 1L
  }
  cnt
}

last_match_ht_total <- function(form_list) {
  if (is.null(form_list) || !length(form_list)) return(NA_integer_)
  m1 <- form_list[[1]]
  ht_h <- m1$ht_for %||% NA
  ht_a <- m1$ht_against %||% NA
  if (is.na(ht_h) || is.na(ht_a)) return(NA_integer_)
  ht_h + ht_a
}

# ════════════════════════════════════════════════════════════
# LEAGUE NORMALIZATION
# ════════════════════════════════════════════════════════════
build_league_avg_attack <- function(enriched_list) {
  team_a <- list(); team_lg <- list()
  for (ef in enriched_list) {
    fx <- ef$fixture
    if (is.null(fx) || nrow(fx) == 0) next
    lg <- as.character(fx$league %||% NA); if (is.na(lg)) next
    for (side in c("home","away")) {
      nm <- as.character(fx[[side]])
      form <- ef[[paste0(side, "_form")]]
      if (is.null(form) || !length(form)) next
      ft_for <- sapply(form, function(m) m$ft_for %||% NA)
      ft_for <- ft_for[!is.na(ft_for)]
      if (!length(ft_for)) next
      k <- paste0(lg, "::", nm)
      team_a[[k]] <- mean(ft_for)
      team_lg[[k]] <- lg
    }
  }
  lg_data <- list()
  for (k in names(team_a)) {
    lg <- team_lg[[k]]
    lg_data[[lg]] <- c(lg_data[[lg]] %||% numeric(), team_a[[k]])
  }
  sapply(lg_data, mean)
}

# ════════════════════════════════════════════════════════════
# PER-FIXTURE COMPUTATION
# ════════════════════════════════════════════════════════════
build_standings_lookup <- function(standings_df) {
  list(by_lg_team = list(), by_team = list())
}

compute_fixture <- function(enriched_fixture, lg_avg_attack_table,
                            league_avg_ft = 2.6, league_avg_ht = 1.1,
                            standings_lookup = NULL) {
  fx <- enriched_fixture$fixture
  if (is.null(fx) || nrow(fx) == 0) return(NULL)
  
  lg <- as.character(fx$league[1] %||% NA)
  league_avg <- lg_avg_attack_table[lg] %||% 1.3
  
  h_form <- enriched_fixture$home_form
  a_form <- enriched_fixture$away_form
  if (is.null(h_form) || !length(h_form)) return(NULL)
  if (is.null(a_form) || !length(a_form)) return(NULL)
  
  # Home team uses home matches; away team uses away matches
  h_home_form <- Filter(function(m) isTRUE(m$concerned_was_home), h_form)
  a_away_form <- Filter(function(m) !isTRUE(m$concerned_was_home), a_form)
  if (length(h_home_form) < 3) h_home_form <- h_form
  if (length(a_away_form) < 3) a_away_form <- a_form
  
  h_for_w <- recency_weighted_mean(sapply(h_home_form, function(m) m$ft_for %||% NA))
  h_ag_w  <- recency_weighted_mean(sapply(h_home_form, function(m) m$ft_against %||% NA))
  a_for_w <- recency_weighted_mean(sapply(a_away_form, function(m) m$ft_for %||% NA))
  a_ag_w  <- recency_weighted_mean(sapply(a_away_form, function(m) m$ft_against %||% NA))
  if (any(is.na(c(h_for_w, h_ag_w, a_for_w, a_ag_w)))) return(NULL)
  
  lambda_h <- pmax(0.1, (h_for_w / league_avg) * (a_ag_w / league_avg) * league_avg)
  lambda_a <- pmax(0.1, (a_for_w / league_avg) * (h_ag_w / league_avg) * league_avg)
  total_lambda <- lambda_h + lambda_a
  
  ks <- 0:10
  p_h <- dpois(ks, lambda_h); p_a <- dpois(ks, lambda_a)
  pm <- outer(p_h, p_a)
  grid <- outer(ks, ks, "+")
  p_over_25 <- sum(pm[grid >= 3])
  
  list(
    fixture        = fx,
    home_form_list = h_form,
    away_form_list = a_form,
    home_home_form = h_home_form,
    away_away_form = a_away_form,
    h2h_list       = enriched_fixture$h2h,
    lambda_h       = lambda_h,
    lambda_a       = lambda_a,
    total_lambda   = total_lambda,
    p_over_25      = p_over_25
  )
}

# ════════════════════════════════════════════════════════════
# MARKET LOGIC
# ════════════════════════════════════════════════════════════
get_over_25 <- function(f, T) {
  # ----- Baseline Poisson + form gates -----
  if (is.na(f$p_over_25) || f$p_over_25 < T$o25_min_p) return(NULL)
  if (f$total_lambda < T$o25_min_total_lam) return(NULL)
  
  h_o25_5 <- cnt_total_ge(f$home_form_list, 5, 3)
  a_o25_5 <- cnt_total_ge(f$away_form_list, 5, 3)
  if (pmin(h_o25_5, a_o25_5) < T$o25_min_both_o25_5) return(NULL)
  
  h_o25_7 <- cnt_total_ge(f$home_form_list, 7, 3)
  a_o25_7 <- cnt_total_ge(f$away_form_list, 7, 3)
  if (pmin(h_o25_7, a_o25_7) < T$o25_min_both_o25_7) return(NULL)
  
  h_cs_5 <- cnt_clean_sheets(f$home_form_list, 5)
  if (h_cs_5 > T$o25_max_h_cs_5) return(NULL)
  
  a_cs_5 <- cnt_clean_sheets(f$away_form_list, 5)
  if (a_cs_5 > T$o25_max_a_cs_5) return(NULL)
  
  # ----- Second-stage filters -----
  # Home's most recent match HT total > 1
  h_last_ht <- last_match_ht_total(f$home_form_list)
  if (is.na(h_last_ht) || h_last_ht < T$o25_min_h_last_ht) return(NULL)
  
  # Sum of draws (last 7) across both teams <= 4
  h_d <- cnt_draws(f$home_form_list, 7)
  a_d <- cnt_draws(f$away_form_list, 7)
  if ((h_d + a_d) > T$o25_max_sum_draws_7) return(NULL)
  
  list(market = "Over 2.5 FT", confidence = f$p_over_25,
       pick = "Over 2.5", data_quality = "structural")
}

ALL_MARKETS <- list(
  over_25 = get_over_25
)

# ════════════════════════════════════════════════════════════
# MAIN DRIVER
# ════════════════════════════════════════════════════════════
run_moab_analysis <- function(enriched_list,
                              thresholds = DEFAULT_MOAB_THRESHOLDS,
                              league_avg_ft = 2.6,
                              league_avg_ht = 1.1,
                              standings = NULL) {
  if (length(enriched_list) == 0) return(list())
  
  lg_avg_attack <- build_league_avg_attack(enriched_list)
  
  picks_by_market <- list()
  for (mkt in names(ALL_MARKETS)) picks_by_market[[mkt]] <- list()
  
  for (k in seq_along(enriched_list)) {
    ef <- enriched_list[[k]]
    if (is.null(ef) || is.null(ef$fixture)) next
    f <- tryCatch(compute_fixture(ef, lg_avg_attack, league_avg_ft, league_avg_ht),
                  error = function(e) NULL)
    if (is.null(f)) next
    
    for (mkt in names(ALL_MARKETS)) {
      pick <- tryCatch(ALL_MARKETS[[mkt]](f, thresholds),
                       error = function(e) NULL)
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
        match_id     = as.character(fx$match_id %||% NA),
        match_url    = as.character(fx$match_url %||% NA),
        stringsAsFactors = FALSE
      )
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
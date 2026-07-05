# ============================================================
# SportyBet helpers — live API + fuzzy league matching
# ============================================================
suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(stringdist)
})

SB_DIRECTORY_API <- paste0("https://www.sportybet.com",
                           "/api/ng/factsCenter/popularAndSportList",
                           "?sportId=sr:sport:1&productId=3")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ─────────────────────────────────────────────────────────────
# Country alias map
# ─────────────────────────────────────────────────────────────
COUNTRY_ALIASES <- list(
  "Korea Republic" = c("Korea Rep", "Republic of Korea", "South Korea"),
  "Korea Rep"      = c("Korea Republic", "Republic of Korea", "South Korea"),
  "Czechia"        = c("Czech Republic"),
  "Czech Republic" = c("Czechia"),
  "United States"  = c("USA"),
  "USA"            = c("United States"),
  "United Arab Emirates" = c("UAE"),
  "UAE"            = c("United Arab Emirates"),
  "DR Congo"       = c("Democratic Republic of Congo", "Congo DR"),
  "Bosnia"         = c("Bosnia-Herzegovina", "Bosnia and Herzegovina"),
  "Cape Verde"     = c("Cabo Verde"),
  "Ivory Coast"    = c("Cote d'Ivoire")
)
resolve_country_aliases <- function(country) {
  if (is.null(country) || is.na(country) || !nzchar(country)) return(character())
  aliases <- COUNTRY_ALIASES[[country]] %||% character()
  unique(c(country, aliases))
}
normalize_league_name <- function(s) {
  if (is.null(s) || is.na(s) || !nzchar(s)) return("")
  x <- tolower(s); x <- gsub("[[:punct:]]+", " ", x); x <- gsub("\\s+", " ", x)
  trimws(x)
}

fetch_sportybet_directory <- function() {
  resp <- tryCatch(
    httr::GET(SB_DIRECTORY_API,
              httr::add_headers(
                `User-Agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/120.0.0.0 Safari/537.36",
                `Accept` = "application/json, text/plain, */*",
                `Accept-Language` = "en-US,en;q=0.9",
                `Referer` = "https://www.sportybet.com/ng/sport/football"),
              httr::timeout(30)),
    error = function(e) { warning("SportyBet API unreachable: ", e$message); NULL })
  if (is.null(resp)) return(NULL)
  if (httr::status_code(resp) != 200) {
    warning("SportyBet API returned ", httr::status_code(resp)); return(NULL)
  }
  body <- httr::content(resp, "parsed", "application/json")
  if (is.null(body$data) || is.null(body$data$sportList)) return(NULL)
  football <- NULL
  for (sp in body$data$sportList) {
    if (identical(sp$id, "sr:sport:1")) { football <- sp; break }
  }
  if (is.null(football)) return(NULL)
  rows <- list()
  for (cat in football$categories) {
    for (trn in cat$tournaments) {
      rows[[length(rows) + 1]] <- data.frame(
        country = cat$name, league = trn$name,
        category_id = cat$id, tournament_id = trn$id,
        event_size = trn$eventSize %||% NA,
        league_url = paste0("https://www.sportybet.com/ng/sport/football/",
                            cat$id, "/", trn$id),
        stringsAsFactors = FALSE)
    }
  }
  if (length(rows) == 0) return(NULL)
  do.call(rbind, rows)
}

# Fuzzy-match one (country, league) pair against SB directory
match_one_league <- function(fs_country, fs_league, sb_dir, fuzzy_threshold = 0.75) {
  if (is.null(sb_dir) || nrow(sb_dir) == 0)
    return(list(matched = FALSE, sb_row = NULL, score = 0, reason = "no_sb_dir"))
  if (is.na(fs_country) || is.na(fs_league))
    return(list(matched = FALSE, sb_row = NULL, score = 0, reason = "missing_fs_input"))
  
  fs_country_candidates <- resolve_country_aliases(fs_country)
  fs_country_candidates_lc <- tolower(fs_country_candidates)
  sb_country_lc <- tolower(sb_dir$country)
  matching_country_rows <- sb_dir[sb_country_lc %in% fs_country_candidates_lc, , drop = FALSE]
  
  if (nrow(matching_country_rows) == 0) {
    sb_countries <- unique(sb_dir$country)
    best_c <- NULL; best_cs <- 0
    for (sbc in sb_countries) {
      s <- 1 - stringdist::stringdist(tolower(fs_country), tolower(sbc), method = "jw")
      if (s > best_cs) { best_cs <- s; best_c <- sbc }
    }
    if (best_cs >= 0.85) {
      matching_country_rows <- sb_dir[tolower(sb_dir$country) == tolower(best_c), , drop = FALSE]
    } else {
      return(list(matched = FALSE, sb_row = NULL, score = best_cs,
                  reason = paste0("country_unmatched (closest=", best_c, " score=",
                                  round(best_cs, 2), ")")))
    }
  }
  
  fs_norm <- normalize_league_name(fs_league)
  sb_norm <- sapply(matching_country_rows$league, normalize_league_name)
  exact <- which(sb_norm == fs_norm)
  if (length(exact) > 0) {
    return(list(matched = TRUE, sb_row = matching_country_rows[exact[1], , drop = FALSE],
                score = 1.0, reason = "exact"))
  }
  scores <- 1 - stringdist::stringdist(fs_norm, sb_norm, method = "jw")
  best_idx <- which.max(scores)
  best_score <- scores[best_idx]
  if (best_score >= fuzzy_threshold) {
    return(list(matched = TRUE, sb_row = matching_country_rows[best_idx, , drop = FALSE],
                score = best_score,
                reason = paste0("fuzzy (", round(best_score, 2), ")")))
  }
  list(matched = FALSE, sb_row = NULL, score = best_score,
       reason = paste0("league_below_threshold (closest=",
                       matching_country_rows$league[best_idx],
                       " score=", round(best_score, 2), ")"))
}

# Filter df + emit diagnostic
filter_to_sportybet_df <- function(df, sb_dir = NULL, fuzzy_threshold = 0.75) {
  if (is.null(sb_dir)) sb_dir <- fetch_sportybet_directory()
  if (is.null(sb_dir) || nrow(sb_dir) == 0) {
    warning("Could not load SportyBet directory; returning unfiltered df")
    return(list(df = df, matched = rep(TRUE, nrow(df)),
                diagnostic = NULL, sb_dir = NULL))
  }
  matched_vec <- logical(nrow(df))
  diag_rows <- list()
  for (i in seq_len(nrow(df))) {
    fs_c <- as.character(df$country[i] %||% NA)
    fs_l <- as.character(df$league[i] %||% NA)
    res <- match_one_league(fs_c, fs_l, sb_dir, fuzzy_threshold)
    matched_vec[i] <- res$matched
    sb_c <- if (!is.null(res$sb_row)) as.character(res$sb_row$country) else NA_character_
    sb_l <- if (!is.null(res$sb_row)) as.character(res$sb_row$league)  else NA_character_
    diag_rows[[i]] <- data.frame(
      fs_country = fs_c, fs_league = fs_l,
      matched = res$matched, score = round(res$score, 3),
      sb_country = sb_c, sb_league = sb_l,
      reason = res$reason, stringsAsFactors = FALSE)
  }
  diag <- do.call(rbind, diag_rows)
  list(df = df[matched_vec, , drop = FALSE],
       matched = matched_vec, diagnostic = diag, sb_dir = sb_dir)
}

filter_enriched_to_sportybet <- function(enriched, sb_dir = NULL, fuzzy_threshold = 0.75) {
  if (length(enriched) == 0) {
    return(list(enriched = enriched, kept = 0L, dropped = 0L,
                sb_dir = NULL, diagnostic = NULL))
  }
  if (is.null(sb_dir)) sb_dir <- fetch_sportybet_directory()
  if (is.null(sb_dir) || nrow(sb_dir) == 0) {
    warning("Could not load SportyBet directory; returning unfiltered list")
    return(list(enriched = enriched, kept = length(enriched), dropped = 0L,
                sb_dir = NULL, diagnostic = NULL))
  }
  pairs <- list()
  for (ef in enriched) {
    fx <- ef$fixture
    if (is.null(fx) || nrow(fx) == 0) next
    k <- paste0(as.character(fx$country[1] %||% ""), "||",
                as.character(fx$league[1] %||% ""))
    pairs[[k]] <- list(country = as.character(fx$country[1] %||% NA),
                       league  = as.character(fx$league[1]  %||% NA))
  }
  if (length(pairs) == 0) {
    return(list(enriched = enriched[FALSE], kept = 0L,
                dropped = length(enriched), sb_dir = sb_dir, diagnostic = NULL))
  }
  pair_df <- do.call(rbind, lapply(pairs, as.data.frame))
  rownames(pair_df) <- NULL
  filt <- filter_to_sportybet_df(pair_df, sb_dir, fuzzy_threshold)
  matched_keys <- paste0(filt$diagnostic$fs_country[filt$matched], "||",
                         filt$diagnostic$fs_league[filt$matched])
  keep <- logical(length(enriched))
  for (i in seq_along(enriched)) {
    fx <- enriched[[i]]$fixture
    if (is.null(fx) || nrow(fx) == 0) { keep[i] <- FALSE; next }
    k <- paste0(as.character(fx$country[1] %||% ""), "||",
                as.character(fx$league[1] %||% ""))
    keep[i] <- k %in% matched_keys
  }
  list(enriched = enriched[keep], kept = sum(keep), dropped = sum(!keep),
       sb_dir = sb_dir, diagnostic = filt$diagnostic)
}

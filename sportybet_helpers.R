# ============================================================
# SportyBet helpers — live API + fuzzy league matching (v2)
# ============================================================
# Public API (unchanged):
#   fetch_sportybet_directory()
#   match_one_league(fs_country, fs_league, sb_dir, fuzzy_threshold = 0.75)
#   filter_to_sportybet_df(df, sb_dir = NULL, fuzzy_threshold = 0.75)
#   filter_enriched_to_sportybet(enriched, sb_dir = NULL, fuzzy_threshold = 0.75)
#
# What's new in v2:
#   1. Category guard  — women / U-age / reserve leagues can only match
#      their own kind. "Allsvenskan Women" never maps to "Allsvenskan".
#      Handles ambiguous "Premier League 2" (English U21) via override.
#   2. Tier guard      — same base name + different tier marker forces
#      no match. "Serie C" no longer maps to "Serie A".
#   3. League aliases  — hardcoded map for genuine variations across
#      naming conventions (e.g. Icelandic "karla" = "men's" suffix).
#   4. Amateur bridge  — Flashscore amateur leagues can find SportyBet
#      "<Country> Amateur" bucket via the country alias map.
# ============================================================

suppressPackageStartupMessages({
  library(httr); library(jsonlite); library(stringdist)
})

SB_DIRECTORY_API <- paste0("https://www.sportybet.com",
                           "/api/ng/factsCenter/popularAndSportList",
                           "?sportId=sr:sport:1&productId=3")

`%||%` <- function(a, b) if (!is.null(a) && length(a) > 0) a else b

# ─────────────────────────────────────────────────────────────
# Country alias map (unchanged from v1 + Amateur bridges)
# ─────────────────────────────────────────────────────────────
COUNTRY_ALIASES <- list(
  "Korea Republic"       = c("Korea Rep", "Republic of Korea", "South Korea"),
  "Korea Rep"            = c("Korea Republic", "Republic of Korea", "South Korea"),
  "South Korea" = c("Korea Republic", "Korea Rep", "Republic of Korea"),
  "Czechia"              = c("Czech Republic"),
  "Czech Republic"       = c("Czechia"),
  "United States"        = c("USA"),
  "USA"                  = c("United States"),
  "United Arab Emirates" = c("UAE"),
  "UAE"                  = c("United Arab Emirates"),
  "DR Congo"             = c("Democratic Republic of Congo", "Congo DR"),
  "Bosnia"               = c("Bosnia-Herzegovina", "Bosnia and Herzegovina"),
  "Cape Verde"           = c("Cabo Verde"),
  "Ivory Coast"          = c("Cote d'Ivoire")
)

# ─────────────────────────────────────────────────────────────
# League name aliases — for genuine cross-source name variations
# where fuzzy matching alone would fail after our tier guard.
# Both sides are matched case-insensitively.
# ─────────────────────────────────────────────────────────────
LEAGUE_ALIASES <- list(
  # Icelandic "karla" = "men's" — Flashscore adds it, SportyBet drops it.
  "Besta deild karla" = c("Besta deild"),

  # ── 44 baseline mappings (2026-08-25 review) ───────────────
  # Key = Flashscore league name, value = SportyBet league name(s).
  "A-League"                       = c("A-League"),                    # Australia (season suffix stripped)
  "Jupiler Pro League"             = c("Pro League"),                  # Belgium
  "WWIN Liga BiH"                  = c("Premijer Liga"),               # Bosnia
  "Serie A Betano"                 = c("Brasileiro Serie A"),          # Brazil top flight
  "Serie B"                        = c("Brasileiro Serie B"),          # Brazil second tier
  "efbet League"                   = c("Parva Liga"),                  # Bulgaria
  "Liga de Primera"                = c("Primera Division"),            # Chile
  "Super League"                   = c("Chinese Super League"),        # China
  "Liga Women"                     = c("Liga Femenina"),               # Colombia women
  "Cyprus League"                  = c("1st Division"),                # Cyprus
  "Chance Liga"                    = c("1. Liga"),                     # Czechia top flight
  "ChNL"                           = c("FNL"),                         # Czechia second tier
  "Liga Pro"                       = c("LigaPro Primera A"),           # Ecuador
  "Isthmian League Premier Division" = c("Isthmian League North Division"), # England 2nd bucket
  "NPL Premier Division"           = c("Northern Premier League Premier"),  # England Amateur
  "Southern League Premier South"  = c("Southern League Premier South"),    # England Amateur (season suffix stripped)
  "Meistriliiga"                   = c("Premium Liiga"),               # Estonia
  "Division 1"                     = c("1. deild", "First Division", "1. Liga"),  # Iceland / Ireland – ambiguous name; country guard resolves
  "Ligat ha'Al"                    = c("Premier League"),              # Israel
  "Serie C - Group A"              = c("Serie C, Group A"),            # Italy
  "Serie C - Group B"              = c("Serie C, Group B"),            # Italy
  "Serie C - Group C"              = c("Serie C, Group C"),            # Italy
  "NIFL Premiership"               = c("Premiership"),                 # Northern Ireland
  "OBOS-ligaen"                    = c("1st Division"),                # Norway 2nd tier
  "Division 2"                        = c("II Liga"),                     # Poland 3rd tier
  "K League 1"                     = c("K-League 1"),                  # Korea
  "K League 2"                     = c("K-League 2"),                  # Korea
  "FNL"                            = c("1. Liga"),                     # Russia 2nd tier
  "Mozzart Bet Super Liga"         = c("Superliga"),                   # Serbia
  "Nike liga"                      = c("Superliga"),                   # Slovakia
  "Division 1 - Norra"             = c("Ettan"),                       # Sweden 3rd tier north
  "Allsvenskan Women"              = c("Damallsvenskan"),              # Sweden women top flight
  "UAE League"                     = c("Arabian Gulf League"),         # UAE
  "Liga AUF Uruguaya"              = c("Primera Division"),            # Uruguay
  "Regionalliga Nordost" = c("Regionalliga Northeast"),
  "NWSL Women"                     = c("National Womens Soccer League") # USA
)

# ─────────────────────────────────────────────────────────────
# Different-tier pairs — country-scoped list of Flashscore league
# names that must NEVER match specific SportyBet league names,
# because they name different tiers in a language where regex-based
# tier extraction can't distinguish them.
# Structure: list of list(country, fs, sb) triples.
# ─────────────────────────────────────────────────────────────
DIFFERENT_TIER_PAIRS <- list(
  list(country = "Estonia",     fs = "Esiliiga",          sb = "Premium Liiga"),
  list(country = "Estonia",     fs = "Esiliiga",          sb = "Meistriliiga"),
  list(country = "Estonia",     fs = "Esiliiga B",        sb = "Premium Liiga"),
  list(country = "Estonia",     fs = "Esiliiga B",        sb = "Meistriliiga"),
  list(country = "Finland",     fs = "Ykkonen",           sb = "Kolmonen"),
  list(country = "Finland",     fs = "Ykk\u00f6nen",      sb = "Kolmonen"),
  list(country = "Netherlands", fs = "Tweede Divisie",    sb = "Eerste Divisie"),
  list(country = "Netherlands", fs = "Derde Divisie",     sb = "Eerste Divisie"),
  list(country = "Netherlands", fs = "Derde Divisie",     sb = "Eredivisie"),
  list(country = "Paraguay",    fs = "Division Intermedia", sb = "Division de Honor"),
  list(country = "Paraguay",    fs = "Divisi\u00f3n Intermedia", sb = "Divisi\u00f3n de Honor"),
  list(country = "Belarus", fs = "Pershaya Liga", sb = "Vysshaya Liga"),
  list(country = "Brazil", fs = "Brasiliense", sb = "Brasileiro Serie A"),
  list(country = "Brazil", fs = "Brasiliense", sb = "Brasileiro Serie B"),
  list(country = "Germany", fs = "Regionalliga Bayern",  sb = "Regionalliga Northeast"),
  list(country = "Germany", fs = "Regionalliga Bayern",  sb = "Regionalliga Nord"),
  list(country = "Germany", fs = "Regionalliga West",    sb = "Regionalliga Northeast"),
  list(country = "Germany", fs = "Regionalliga Nord",    sb = "Regionalliga Northeast"),
  list(country = "Germany", fs = "Regionalliga Sudwest", sb = "Regionalliga Northeast"),
  list(country = "Japan", fs = "J2/J3 League", sb = "J2 League"),
  list(country = "Japan", fs = "J2/J3 League", sb = "J1 League"),
  list(country = "Japan", fs = "J2/J3 League", sb = "J3 League"),
  list(country = "South Korea", fs = "K3 League", sb = "K-League 1"),
  list(country = "South Korea", fs = "K3 League", sb = "K-League 2"),
  list(country = "South Korea", fs = "K4 League", sb = "K-League 1"),
  list(country = "South Korea", fs = "K4 League", sb = "K-League 2"),
  list(country = "USA", fs = "MLS Next Pro", sb = "MLS"),
  list(country = "USA", fs = "MLS Next Pro", sb = "Major League Soccer"),
  list(country = "Oman", fs = "Professional League", sb = "Professional League Cup"),
  list(country = "Uzbekistan", fs = "Pro Liga", sb = "Superliga"),
  list(country = "Ukraine",     fs = "Druha Liga",        sb = "Persha Liga")
)

# ─────────────────────────────────────────────────────────────
# Override list for league names whose numeric/roman suffix DOES
# indicate a reserve/youth competition rather than an adult tier.
# Any name here is treated as a "restricted" league and can only
# match another restricted league of the same base.
# ─────────────────────────────────────────────────────────────
RESERVE_YOUTH_OVERRIDES <- c(
  "Premier League 2"      # England U21 development league
)

# ─────────────────────────────────────────────────────────────
# Helpers
# ─────────────────────────────────────────────────────────────
resolve_country_aliases <- function(country) {
  if (is.null(country) || length(country) == 0) return(character())
  country <- as.character(country)[1]
  if (is.na(country) || !nzchar(country)) return(character())
  aliases <- COUNTRY_ALIASES[[country]] %||% character()
  # Also try the "<Country> Amateur" bridge automatically
  aliases <- c(aliases, paste0(country, " Amateur"))
  unique(c(country, aliases))
}

normalize_league_name <- function(s) {
  if (is.null(s) || length(s) == 0) return("")
  s <- as.character(s)[1]
  if (is.na(s) || !nzchar(s)) return("")
  x <- tolower(s); x <- gsub("[[:punct:]]+", " ", x); x <- gsub("\\s+", " ", x)
  trimws(x)
}

# Classify a league name into one of: "men_senior", "women", "reserve_youth".
# Anything with women / ladies / W-League markers → "women".
# Anything with U-age markers (U17, U18, U19, U20, U21, U23), "Reserve",
# "Youth", "Junior", or in the RESERVE_YOUTH_OVERRIDES → "reserve_youth".
# Otherwise → "men_senior".
classify_league <- function(name) {
  if (is.null(name) || length(name) == 0) return("men_senior")
  name <- as.character(name)[1]
  if (is.na(name) || !nzchar(name)) return("men_senior")
  low <- tolower(name)
  if (tolower(name) %in% tolower(RESERVE_YOUTH_OVERRIDES)) return("reserve_youth")
  if (grepl("\\bwomens?\\b|\\bwom\\b|\\bladies\\b|\\bwsl\\b|\\bw[-\\s]league\\b|\\bfeminin(a|e|as|es)?\\b|femenin|\\bdamen\\b|\\bdam\\b|damallsvenskan|\\bfemme\\b|\\bfem\\b",
            low, perl = TRUE)) return("women")
  if (grepl("\\bu[-\\s]?\\d{2}\\b|\\bunder[-\\s]?\\d{2}\\b|\\breserve[s]?\\b|\\byouth\\b|\\bjunior[s]?\\b|\\bacademy\\b",
            low, perl = TRUE)) return("reserve_youth")
  "men_senior"
}

# Extract a (base, tier) pair from a league name.
# Strips trailing tier markers so leagues can be compared at their base level.
#   "Serie C"                 -> list(base = "serie",           tier = "c")
#   "1. Bundesliga"           -> list(base = "bundesliga",      tier = "1")
#   "Bundesliga"              -> list(base = "bundesliga",      tier = "")
#   "NB III - Northeast"      -> list(base = "nb",              tier = "3 northeast")
#   "Division 2 - Norrland"   -> list(base = "division",        tier = "2 norrland")
#   "Serie C - Group A"       -> list(base = "serie",           tier = "c group a")
# Tier tokens recognised: numeric (1..99), roman (I..XX), single letter A..F,
# ordinal prefixes ("1st","2nd","3rd","1.","2.","3."), "group X".
extract_base_and_tier <- function(name) {
  if (is.null(name) || length(name) == 0)
    return(list(base = "", tier = "", has_tier = FALSE))
  name <- as.character(name)[1]
  if (is.na(name) || !nzchar(name))
    return(list(base = "", tier = "", has_tier = FALSE))

  # Strip trailing season suffix like "26/27", "2026-27", "26/2027".
  name <- gsub("\\s+\\d{2,4}\\s*[/-]\\s*\\d{2,4}\\s*$", "", name)

  # Split on dash BEFORE normalization (so " - " boundaries survive).
  # Everything after " - " (or " -" or "- ") is treated as pure tier detail.
  raw_parts <- strsplit(name, "\\s*-\\s*|\\s+-\\s+")[[1]]
  head_raw  <- raw_parts[1]
  extra_tier_raw <- if (length(raw_parts) > 1) paste(raw_parts[-1], collapse = " ") else ""

  # Now normalise the head part (punctuation to space, lowercase, collapse spaces)
  low <- normalize_league_name(head_raw)
  extra_tier <- tolower(gsub("[[:punct:]]+", " ", extra_tier_raw))
  extra_tier <- trimws(gsub("\\s+", " ", extra_tier))

  # Map spelled-out tier words to digits so "League Two" and "League 2" align.
  word_to_num <- c(one = "1", two = "2", three = "3", four = "4", five = "5",
                   six = "6", seven = "7", eight = "8", first = "1", second = "2",
                   third = "3", fourth = "4", fifth = "5")

  # Handle no-space alphanumeric suffixes on the head. Examples:
  #   "LaLiga2" -> base="laliga", tier="2"
  #   "NB1"     -> base="nb",     tier="1"
  suffix_num <- ""
  m <- regmatches(low, regexec("^(.+?)([0-9]{1,2})$", low))[[1]]
  if (length(m) == 3 && nchar(m[2]) >= 3) {
    low <- trimws(m[2])
    suffix_num <- m[3]
  }

  toks <- strsplit(low, "\\s+")[[1]]

  # Ordinal prefix like "1." "2." "1st" "2nd" "3rd" moves to tier
  ordinal_prefix <- ""
  if (length(toks) > 0) {
    if (grepl("^[0-9]+(st|nd|rd|th)?\\.?$", toks[1])) {
      ordinal_prefix <- gsub("[^0-9]", "", toks[1])
      toks <- toks[-1]
    }
  }

  # Alphanumeric leading tier like "J1", "K2" — a single letter + digits at
  # the FRONT of the head. Extract to tier and remove that token.
  alnum_prefix <- ""
  if (length(toks) > 0 && grepl("^[a-z][0-9]{1,2}$", toks[1])) {
    alnum_prefix <- toks[1]
    toks <- toks[-1]
  }

  # Tier token recognisers
  roman_re     <- "^(i{1,3}|iv|v|vi{1,3}|ix|x{1,2}i{0,3}|xiv|xv|xvi{1,3}|xix|xx)$"
  letter_re    <- "^[a-f]$"                     # single-letter A..F suffixes
  num_re       <- "^[0-9]+$"                    # plain integer
  alnum_re     <- "^[a-z][0-9]{1,2}$"           # J1/J2/J3
  cardinal_re  <- "^(north|south|east|west|northeast|northwest|southeast|southwest|centre|central|norra|s\u00f6dra|sodra|v\u00e4stra|vastra|\u00f6stra|ostra|nord|sud|est|oeste|norte)$"
  word_num_re  <- paste0("^(", paste(names(word_to_num), collapse = "|"), ")$")
  is_tier_tok <- function(t) {
    grepl(num_re, t) || grepl(roman_re, t) || grepl(letter_re, t) ||
      grepl(alnum_re, t) || grepl(cardinal_re, t) || grepl(word_num_re, t)
  }

  trailing_tier <- ""
  # Consume trailing tier tokens right-to-left. Also handle "group X" pairs.
  while (length(toks) > 0) {
    last <- toks[length(toks)]
    if (is_tier_tok(last)) {
      # normalize word-numbers to digits
      norm_last <- if (grepl(word_num_re, last)) word_to_num[[last]] else last
      trailing_tier <- paste(norm_last, trailing_tier); toks <- toks[-length(toks)]
      next
    }
    if (length(toks) >= 2 && toks[length(toks)] == "group") {
      trailing_tier <- paste("group", trailing_tier); toks <- toks[-length(toks)]
      next
    }
    break
  }
  trailing_tier <- trimws(trailing_tier)

  # Normalise roman -> arabic so "III" and "3" compare equal
  roman_to_num <- function(x) {
    if (!grepl(roman_re, x)) return(x)
    map <- c(i = 1, v = 5, x = 10)
    out <- 0; prev <- 0
    for (ch in rev(strsplit(x, "")[[1]])) {
      v <- map[[ch]]
      if (v < prev) out <- out - v else out <- out + v
      prev <- v
    }
    as.character(out)
  }
  norm_tier_parts <- vapply(strsplit(trailing_tier, "\\s+")[[1]],
                             roman_to_num, character(1), USE.NAMES = FALSE)
  trailing_tier <- paste(norm_tier_parts, collapse = " ")

  base <- paste(toks, collapse = " ")
  tier <- trimws(paste(ordinal_prefix, alnum_prefix, suffix_num,
                        trailing_tier, extra_tier))
  list(base = base, tier = tier, has_tier = nzchar(tier))
}

# ─────────────────────────────────────────────────────────────
# Fetch SportyBet's live football directory
# ─────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────
# Match one Flashscore (country, league) against SB directory
# ─────────────────────────────────────────────────────────────
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

  # ─── Guard 1: category guard (women / U-age / reserve) ───────
  fs_class <- classify_league(fs_league)
  sb_classes <- vapply(matching_country_rows$league, classify_league, character(1))
  keep_mask <- sb_classes == fs_class
  if (!any(keep_mask)) {
    return(list(matched = FALSE, sb_row = NULL, score = 0,
                reason = paste0("category_unmatched (fs='", fs_class,
                                "' none available in country)")))
  }
  matching_country_rows <- matching_country_rows[keep_mask, , drop = FALSE]

  # ─── Guard 1b: hard block for known-different-tier pairs ─────
  # E.g. Finnish "Ykkönen" (T2) must never match "Kolmonen" (T4).
  blocked_sb_names <- character()
  for (bl in DIFFERENT_TIER_PAIRS) {
    if (tolower(bl$country) %in% tolower(fs_country_candidates) &&
        tolower(bl$fs) == tolower(fs_league)) {
      blocked_sb_names <- c(blocked_sb_names, tolower(bl$sb))
    }
  }
  if (length(blocked_sb_names) > 0) {
    keep_mask2 <- !(tolower(matching_country_rows$league) %in% blocked_sb_names)
    matching_country_rows <- matching_country_rows[keep_mask2, , drop = FALSE]
    if (nrow(matching_country_rows) == 0) {
      return(list(matched = FALSE, sb_row = NULL, score = 0,
                  reason = "blocked_by_different_tier_pair"))
    }
  }

  # ─── Alias fast-path ──────────────────────────────────────────
  fs_norm <- normalize_league_name(fs_league)
  fs_aliases <- LEAGUE_ALIASES[[fs_league]] %||% character()
  alias_hits <- character()
  if (length(fs_aliases) > 0) {
    sb_norm_all <- vapply(matching_country_rows$league,
                           normalize_league_name, character(1), USE.NAMES = FALSE)
    for (al in fs_aliases) {
      hit <- which(sb_norm_all == normalize_league_name(al))
      if (length(hit) > 0) {
        return(list(matched = TRUE, sb_row = matching_country_rows[hit[1], , drop = FALSE],
                    score = 1.0, reason = "alias"))
      }
    }
  }

  # ─── Exact ────────────────────────────────────────────────────
  sb_norm <- sapply(matching_country_rows$league, normalize_league_name)
  exact <- which(sb_norm == fs_norm)
  if (length(exact) > 0) {
    return(list(matched = TRUE, sb_row = matching_country_rows[exact[1], , drop = FALSE],
                score = 1.0, reason = "exact"))
  }

  # ─── Guard 2: tier guard on fuzzy candidates ─────────────────
  # Compute base + tier once for fs, once per sb candidate.
  fs_bt <- extract_base_and_tier(fs_league)
  scores <- 1 - stringdist::stringdist(fs_norm, sb_norm, method = "jw")
  # Order candidates best-first; walk down and skip tier-mismatched ones.
  ord <- order(scores, decreasing = TRUE)
  for (i in ord) {
    if (scores[i] < fuzzy_threshold) break
    sb_bt <- extract_base_and_tier(matching_country_rows$league[i])
    # If bases align (after normalisation) AND both have tiers AND tiers differ → skip
    if (nzchar(fs_bt$base) && nzchar(sb_bt$base) &&
        fs_bt$base == sb_bt$base &&
        fs_bt$has_tier && sb_bt$has_tier &&
        fs_bt$tier != sb_bt$tier) {
      next
    }
    # Extra safety: if one side has a tier and the other doesn't AND the
    # non-tier side's base matches the tier side's base, that also implies
    # different tiers (e.g. "Serie A" vs "Serie" would be caught here — rare
    # in practice, but harmless).
    if (nzchar(fs_bt$base) && nzchar(sb_bt$base) &&
        fs_bt$base == sb_bt$base &&
        (fs_bt$has_tier != sb_bt$has_tier)) {
      next
    }
    return(list(matched = TRUE, sb_row = matching_country_rows[i, , drop = FALSE],
                score = scores[i],
                reason = paste0("fuzzy (", round(scores[i], 2), ")")))
  }

  # Best available candidate for diagnostics, even if we rejected it
  best_i <- ord[1]
  list(matched = FALSE, sb_row = NULL, score = scores[best_i],
       reason = paste0("league_below_threshold_or_tier_mismatch (closest=",
                       matching_country_rows$league[best_i],
                       " score=", round(scores[best_i], 2), ")"))
}

# ─────────────────────────────────────────────────────────────
# Filter a data.frame of (country, league) pairs
# ─────────────────────────────────────────────────────────────
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

# ─────────────────────────────────────────────────────────────
# Filter an enriched fixtures list (unchanged)
# ─────────────────────────────────────────────────────────────
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

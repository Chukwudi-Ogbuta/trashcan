# ============================================================
# engine/loading.R
# Handles loading sim-ready RDS files into the grid searcher.
# Supports two modes:
#   - Single-file mode: one file, no role tagging
#   - Multi-file mode:  N files, each tagged Tune / Validate / Test / Exclude
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── Load a single sim-ready RDS ─────────────────────────────
# Returns a list with: fixtures (list), meta (data.frame summary)
load_sim_ready_file <- function(path) {
  if (!file.exists(path)) {
    return(list(ok = FALSE, error = "File does not exist", fixtures = NULL, meta = NULL))
  }
  fixtures <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(fixtures) || !is.list(fixtures) || length(fixtures) == 0) {
    return(list(ok = FALSE, error = "Not a valid sim-ready RDS", fixtures = NULL, meta = NULL))
  }
  
  # Extract summary metadata
  dates <- sapply(fixtures, function(ef) {
    d <- ef$fixture$fixture_date
    if (is.null(d) || length(d) == 0) return(NA_character_)
    as.character(d[1])
  })
  dates <- dates[!is.na(dates) & dates != ""]
  
  n_final <- sum(sapply(fixtures, function(ef) {
    s <- ef$fixture$result_status
    !is.null(s) && length(s) > 0 && !is.na(s[1]) && s[1] == "FINAL"
  }))
  
  meta <- data.frame(
    filename = basename(path),
    fullpath = path,
    n_fixtures = length(fixtures),
    n_final = n_final,
    date_min = if (length(dates) > 0) min(dates) else NA_character_,
    date_max = if (length(dates) > 0) max(dates) else NA_character_,
    round_label = parse_round_label(basename(path)),
    stringsAsFactors = FALSE
  )
  
  list(ok = TRUE, error = NULL, fixtures = fixtures, meta = meta)
}

# ─── Auto-parse round label from filename ────────────────────
# Filenames like "MoabSim_Ready_20260810_1846.rds" become "20260810"
# Users can override in the UI.
parse_round_label <- function(filename) {
  m <- regmatches(filename, regexpr("[0-9]{8}", filename))
  if (length(m) > 0 && nchar(m) == 8) return(m)
  # fallback: strip extension
  tools::file_path_sans_ext(filename)
}

# ─── Multi-file loader ───────────────────────────────────────
# Takes a data.frame of files-with-roles and combines them into a
# single analysis pool keyed by role.
# Roles: "Tune", "Validate", "Test", "Exclude"
# Returns a list: tune (fixtures), validate (fixtures), test (fixtures), meta_by_file
load_multi_files <- function(files_df) {
  # files_df expected columns: fullpath, role
  out <- list(tune = list(), validate = list(), test = list(), meta_by_file = list())
  for (i in seq_len(nrow(files_df))) {
    role <- files_df$role[i]
    if (identical(role, "Exclude")) next
    res <- load_sim_ready_file(files_df$fullpath[i])
    if (!res$ok) {
      warning("Failed to load ", files_df$fullpath[i], ": ", res$error)
      next
    }
    # Tag each fixture with its source file (useful for per-round breakdown later)
    tagged <- lapply(res$fixtures, function(ef) {
      ef$source_file <- files_df$filename[i]
      ef$source_role <- role
      ef
    })
    if (identical(role, "Tune"))     out$tune     <- c(out$tune, tagged)
    if (identical(role, "Validate")) out$validate <- c(out$validate, tagged)
    if (identical(role, "Test"))     out$test     <- c(out$test, tagged)
    out$meta_by_file[[files_df$filename[i]]] <- res$meta
  }
  out
}

# ─── Auto-tag helper for multi-file loading ──────────────────
# Given files sorted by round_label (or filename), assign:
#   newest → Test, second-newest → Validate, older → Tune
auto_tag_roles <- function(files_df) {
  if (nrow(files_df) == 0) return(files_df)
  # Sort by round_label descending (newest first)
  files_df <- files_df[order(files_df$round_label, decreasing = TRUE), ]
  files_df$role <- "Tune"
  if (nrow(files_df) >= 1) files_df$role[1] <- "Test"
  if (nrow(files_df) >= 2) files_df$role[2] <- "Validate"
  # Restore original ordering if needed by caller (keep sorted for now)
  files_df
}

# ─── Filter fixtures to only FINAL (gradable) ────────────────
filter_final_only <- function(fixtures) {
  Filter(function(ef) {
    s <- ef$fixture$result_status
    !is.null(s) && length(s) > 0 && !is.na(s[1]) && s[1] == "FINAL"
  }, fixtures)
}

# ─── Session save/load ───────────────────────────────────────
save_session <- function(files_df, session_name, sessions_dir) {
  if (!dir.exists(sessions_dir)) dir.create(sessions_dir, recursive = TRUE)
  path <- file.path(sessions_dir, paste0(session_name, ".rds"))
  saveRDS(files_df, path)
  path
}

load_session <- function(session_name, sessions_dir) {
  path <- file.path(sessions_dir, paste0(session_name, ".rds"))
  if (!file.exists(path)) return(NULL)
  readRDS(path)
}

list_sessions <- function(sessions_dir) {
  if (!dir.exists(sessions_dir)) return(character())
  files <- list.files(sessions_dir, pattern = "\\.rds$", full.names = FALSE)
  tools::file_path_sans_ext(files)
}


# ============================================================
# Append these functions to the END of engine/loading.R
# (right after list_sessions() — do not remove any existing code)
# ============================================================

# ─── League directory (MOAB's verification file) ─────────────
# Hardcoded to the MOAB path so both apps share one source of truth.
LEAGUE_DIRECTORY_PATH <- "C:/Users/Ogbuta/OneDrive/New Projects 3/league_directory.rds"

# Load the directory once and build a (country -> ranked league list) index.
# Only verified=TRUE leagues are ranked. Order follows the row order in the
# RDS, which mirrors Flashscore's display order (top-flight first per country).
# Returns NULL if the directory is missing or unreadable.
load_league_ranks <- function(path = LEAGUE_DIRECTORY_PATH) {
  if (!file.exists(path)) {
    warning("League directory not found at: ", path)
    return(NULL)
  }
  d <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(d)) return(NULL)
  # Keep only verified leagues
  d <- d[isTRUE_vec(d$is_league) & isTRUE_vec(d$verified), , drop = FALSE]
  if (nrow(d) == 0) return(NULL)
  # Group by country, preserve original row order → that IS the rank
  ranks <- list()
  for (i in seq_len(nrow(d))) {
    ct <- as.character(d$country[i])
    lg <- as.character(d$league_name[i])
    if (is.na(ct) || is.na(lg)) next
    if (is.null(ranks[[ct]])) ranks[[ct]] <- character()
    if (!(lg %in% ranks[[ct]])) ranks[[ct]] <- c(ranks[[ct]], lg)
  }
  ranks
}

# Vectorized isTRUE helper (base isTRUE only handles scalars)
isTRUE_vec <- function(x) {
  vapply(x, function(v) isTRUE(v), logical(1))
}

# Given a fixture's country + league and the ranks index, return the rank
# (1-based). Returns NA if the country/league isn't in the directory.
lookup_league_rank <- function(country, league, ranks_index) {
  if (is.null(ranks_index) || is.na(country) || is.na(league)) return(NA_integer_)
  ct <- as.character(country); lg <- as.character(league)
  if (is.null(ranks_index[[ct]])) return(NA_integer_)
  idx <- match(lg, ranks_index[[ct]])
  if (is.na(idx)) return(NA_integer_) else as.integer(idx)
}

# Filter a list of fixtures to only those whose league is in the top-N
# verified leagues within its country. Fixtures whose (country, league) pair
# is unknown to the directory are DROPPED — treating unknown as "unverified".
# Pass top_n = NULL to keep everything (no filtering).
filter_fixtures_top_n <- function(fixtures, top_n, ranks_index) {
  if (is.null(top_n) || is.na(top_n)) return(fixtures)
  if (is.null(ranks_index)) {
    warning("No league ranks index available — returning fixtures unfiltered.")
    return(fixtures)
  }
  Filter(function(ef) {
    fx <- ef$fixture
    ct <- as.character(fx$country[1] %||% NA)
    lg <- as.character(fx$league[1]   %||% NA)
    rk <- lookup_league_rank(ct, lg, ranks_index)
    !is.na(rk) && rk <= top_n
  }, fixtures)
}

# ─── Handpicked "decent football country" tier ───────────────
# Curated by Chidi from the 176-country league directory.
HANDPICKED_COUNTRIES <- c(
  "Albania", "Algeria", "Andorra", "Antigua & Barbuda", "Argentina",
  "Armenia", "Australia", "Austria", "Bahrain", "Belarus", "Belgium",
  "Bhutan", "Bolivia", "Bosnia and Herzegovina", "Brazil", "Bulgaria",
  "Canada", "Chile", "China", "Colombia", "Costa Rica", "Croatia",
  "Cyprus", "Czech Republic", "Denmark", "Ecuador", "Egypt", "El Salvador",
  "England", "Estonia", "Faroe Islands", "Fiji", "Finland", "France",
  "Georgia", "Germany", "Greece", "Honduras", "Hong Kong", "Hungary",
  "Iceland", "Iran", "Iraq", "Ireland", "Israel", "Italy", "Japan",
  "Kuwait", "Malta", "Mexico", "Morocco", "Netherlands", "New Zealand",
  "North Macedonia", "Northern Ireland", "Norway", "Oman", "Panama",
  "Paraguay", "Peru", "Poland", "Portugal", "Qatar", "Romania", "Russia",
  "Saudi Arabia", "Scotland", "Senegal", "Serbia", "Seychelles",
  "Sierra Leone", "Singapore", "Slovakia", "Slovenia", "South Africa",
  "South Korea", "Spain", "Sweden", "Switzerland", "Syria", "Tunisia",
  "Turkey", "USA", "Ukraine", "United Arab Emirates", "Uruguay",
  "Venezuela", "Wales"
)

# Regional/confederation buckets that are not real countries.
# Excluded from the Custom country selector by default.
REGIONAL_BUCKETS <- c("Africa", "Asia", "Australia & Oceania", "Europe",
                      "North & Central America", "South America", "World")

# Return the sorted list of real countries in the directory,
# with regional buckets stripped out. Used to populate the
# Custom multi-select dropdown.
list_countries_for_selector <- function(path = LEAGUE_DIRECTORY_PATH) {
  if (!file.exists(path)) return(character())
  d <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(d)) return(character())
  cats <- sort(unique(as.character(d$country[!is.na(d$country)])))
  setdiff(cats, REGIONAL_BUCKETS)
}

# Filter a list of fixtures to only those whose country is in `keep_countries`.
# Pass keep_countries = NULL to skip filtering (equivalent to "All").
filter_fixtures_by_country <- function(fixtures, keep_countries) {
  if (is.null(keep_countries) || length(keep_countries) == 0) return(fixtures)
  Filter(function(ef) {
    ct <- as.character(ef$fixture$country[1] %||% NA)
    !is.na(ct) && ct %in% keep_countries
  }, fixtures)
}

# ─── SB-verified league filter ───────────────────────────────
SPORTY_MAPPER_PATH <- "C:/Users/Ogbuta/OneDrive/New Projects 3/sporty_mapper.rds"

# Load the (fs_country, fs_league) pairs marked Verified in the mapper.
# Returns NULL if the mapper doesn't exist yet.
load_sb_verified_pairs <- function(path = SPORTY_MAPPER_PATH) {
  if (!file.exists(path)) {
    warning("sporty_mapper.rds not found at: ", path)
    return(NULL)
  }
  m <- tryCatch(readRDS(path), error = function(e) NULL)
  if (is.null(m) || nrow(m) == 0) return(NULL)
  v <- m[!is.na(m$status) & m$status == "Verified" &
           !is.na(m$fs_country) & !is.na(m$fs_league),
         c("fs_country", "fs_league"), drop = FALSE]
  if (nrow(v) == 0) return(NULL)
  unique(v)
}

# Filter fixtures to only those whose (country, league) matches a Verified
# pair in the mapper. Pass verified_pairs=NULL to skip filtering.
filter_fixtures_by_sb_verified <- function(fixtures, verified_pairs) {
  if (is.null(verified_pairs) || nrow(verified_pairs) == 0) return(fixtures)
  keys_v <- paste0(tolower(verified_pairs$fs_country), "||",
                   tolower(verified_pairs$fs_league))
  Filter(function(ef) {
    fx <- ef$fixture
    ct <- as.character(fx$country[1] %||% NA)
    lg <- as.character(fx$league[1]  %||% NA)
    if (is.na(ct) || is.na(lg)) return(FALSE)
    paste0(tolower(ct), "||", tolower(lg)) %in% keys_v
  }, fixtures)
}
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

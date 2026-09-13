# ============================================================
# engine/replay.R
# Session 5 — full-workspace snapshot save/load.
#
# Extends the Session 1 session save/load (which only stored files_df)
# to capture the entire working state so you can:
#   - Save mid-analysis, close the app, resume tomorrow exactly where
#     you left off.
#   - Reproduce a Stage 1 result from a captured snapshot when reviewing
#     a decision weeks later ("what did I run to get that rule?").
#
# What is (and isn't) captured:
#   Captured:
#     - files_df (path, role, metadata)
#     - current_round, main_league override, default_window, min_picks
#     - last Stage 1 search (leaderboard + grid_spec + baseline)
#     - last Stage 2 result (refine + rescue leaderboards)
#     - pipeline object (with all metrics + state)
#     - coverage & orthogonality snapshots
#
#   NOT captured (persistent externally):
#     - drift registry (lives under drift/ regardless)
#     - market files (lives under markets/)
#     - intuition files (lives under intuitions/)
# ============================================================

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ─── File paths ──────────────────────────────────────────────
snapshot_paths <- function(sessions_dir) {
  list(dir = sessions_dir)
}

snapshot_filename <- function(sessions_dir, name) {
  file.path(sessions_dir, paste0(name, "_snapshot.rds"))
}

# ─── Save full workspace ─────────────────────────────────────
save_snapshot <- function(name, sessions_dir, workspace) {
  if (!dir.exists(sessions_dir)) dir.create(sessions_dir, recursive = TRUE)
  path <- snapshot_filename(sessions_dir, name)
  # Version tag lets us handle format migrations down the line
  workspace$moab_snapshot_version <- 1L
  workspace$moab_snapshot_saved_at <- Sys.time()
  saveRDS(workspace, path)
  list(ok = TRUE, path = path)
}

# ─── Load full workspace ─────────────────────────────────────
load_snapshot <- function(name, sessions_dir) {
  path <- snapshot_filename(sessions_dir, name)
  if (!file.exists(path))
    return(list(ok = FALSE, msg = paste0("Snapshot not found: ", path)))
  ws <- tryCatch(readRDS(path),
                 error = function(e) NULL)
  if (is.null(ws))
    return(list(ok = FALSE, msg = "Failed to read snapshot (corrupted?)"))
  if (is.null(ws$moab_snapshot_version))
    return(list(ok = FALSE, msg = "Not a MOAB snapshot file."))
  list(ok = TRUE, workspace = ws)
}

# ─── List available snapshots ────────────────────────────────
list_snapshots <- function(sessions_dir) {
  if (!dir.exists(sessions_dir)) return(character(0))
  files <- list.files(sessions_dir, pattern = "_snapshot\\.rds$")
  sub("_snapshot\\.rds$", "", files)
}

# ─── Snapshot summary for the UI ─────────────────────────────
snapshot_summary <- function(name, sessions_dir) {
  res <- load_snapshot(name, sessions_dir)
  if (!res$ok) return(res$msg)
  ws <- res$workspace
  paste(
    paste0("Saved at: ", format(ws$moab_snapshot_saved_at %||% NA)),
    paste0("Files: ", if (!is.null(ws$files_df)) nrow(ws$files_df) else 0),
    paste0("Round: ", ws$current_round %||% "?"),
    paste0("Market: ", ws$market_pick %||% "(none)"),
    paste0("Stage 1 result: ", if (!is.null(ws$last_search)) "present" else "none"),
    paste0("Stage 2 result: ", if (!is.null(ws$stage2_result)) "present" else "none"),
    paste0("Pipeline state: ", ws$pipeline$state %||% "(no pipeline)"),
    sep = "\n"
  )
}

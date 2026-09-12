# ============================================================
# sporty_mapper_app.R  (v2)
# Review-first design. Everything the app proposes must be
# explicitly reviewed. Once reviewed, decisions stick across
# refreshes.
# ============================================================

library(shiny)
library(DT)

BASE_PATH  <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
DIR_PATH   <- file.path(BASE_PATH, "league_directory.rds")
MAP_PATH   <- file.path(BASE_PATH, "sporty_mapper.rds")
HELPERS    <- file.path(BASE_PATH, "sportybet_helpers.R")
XBET_HLP   <- file.path(BASE_PATH, "onexbet_helpers.R")

source(HELPERS)
if (file.exists(XBET_HLP)) source(XBET_HLP)

`%||%` <- function(a, b) if (is.null(a) || length(a) == 0 || all(is.na(a))) b else a

# ────────────────────────────────────────────────────────────
# HELPERS
# ────────────────────────────────────────────────────────────
load_verified_fs <- function() {
  d <- readRDS(DIR_PATH)
  vf <- d[!is.na(d$is_league) & d$is_league == TRUE &
          !is.na(d$verified) & d$verified == TRUE,
          c("country", "league_name")]
  names(vf)[2] <- "league"
  vf
}

# Given a country + the current mapper, return FS leagues in that country
# that are NOT already mapped as fs_league in any Verified row of the mapper.
unmapped_fs_for_country <- function(country, mapper, verified_fs) {
  fs_all <- verified_fs$league[verified_fs$country == country]
  already <- mapper$fs_league[!is.na(mapper$fs_country) &
                               mapper$fs_country == country &
                               !is.na(mapper$status) &
                               mapper$status == "Verified"]
  sort(setdiff(fs_all, already))
}

# ────────────────────────────────────────────────────────────
# UI
# ────────────────────────────────────────────────────────────
ui <- fluidPage(
  tags$head(tags$style(HTML("
    body { font-family: 'Inter', system-ui, sans-serif; background: #FAF7F2;
           color: #1A1A1A; margin: 0; }
    .hd { background: #1E3A8A; color: #FAF7F2; padding: 20px 32px;
          border-bottom: 3px solid #C9A14A; }
    .hd h1 { margin: 0; font-size: 22px; font-weight: 800; letter-spacing: -.02em; }
    .hd .sub { font-size: 11px; letter-spacing: 3px; text-transform: uppercase;
               color: rgba(255,255,255,.7); margin-top: 4px; }
    .pg { max-width: 1500px; margin: 24px auto; padding: 0 24px; }
    .banner { display: flex; gap: 18px; padding: 18px 22px; margin-bottom: 18px;
              background: #fff; border: 1px solid #E5E0D5; border-radius: 6px; }
    .bnum { font-size: 26px; font-weight: 800; }
    .blab { font-size: 10px; letter-spacing: 2px; text-transform: uppercase;
            color: #888; margin-top: 4px; }
    .ok { color: #2E7D5B; } .warn { color: #B8860B; } .bad { color: #C44536; }
    .neu { color: #888; } .info { color: #1E3A8A; }
    .card { background: #fff; border: 1px solid #E5E0D5; border-radius: 6px;
            padding: 20px 24px; margin-bottom: 16px; }
    .btn-primary { background: #1E3A8A !important; border: none !important;
                   color: #fff !important; font-weight: 600; letter-spacing: 1px;
                   text-transform: uppercase; font-size: 11px;
                   padding: 10px 22px !important; }
    .btn-outline { background: transparent !important; color: #1E3A8A !important;
                   border: 1px solid #1E3A8A !important; font-weight: 600;
                   letter-spacing: 1px; text-transform: uppercase; font-size: 11px;
                   padding: 10px 22px !important; }
    .row-panel { border: 1px solid #E5E0D5; border-radius: 4px;
                 padding: 12px 16px; margin-bottom: 10px; background: #FAFAFA; }
    .row-panel.exact  { border-left: 4px solid #2E7D5B; }
    .row-panel.fuzzy  { border-left: 4px solid #B8860B; }
    .row-panel.alias  { border-left: 4px solid #1E3A8A; }
    .row-panel.nomatch{ border-left: 4px solid #888; }
    .rp-head { font-weight: 700; font-size: 13px; margin-bottom: 6px; color: #1A1A1A; }
    .rp-meta { font-size: 11px; color: #666; margin-bottom: 8px; }
    .rp-controls { display: flex; gap: 12px; align-items: center; }
    .rp-controls .form-group { margin-bottom: 0; }
    .search-box { width: 100%; padding: 8px 12px; margin-bottom: 12px;
                  border: 1px solid #E5E0D5; border-radius: 4px; font-size: 13px; }
  "))),
  div(class = "hd", h1("MOAB SB Mapper"),
      div(class = "sub", "Review \u00b7 approve \u00b7 override")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('updateLeagueOpts', function(msg) {
      var el = document.getElementById(msg.id);
      if (el) { el.innerHTML = msg.html; el.value = ''; }
    });
  ")),
  div(class = "pg",
      uiOutput("banner"),
      div(class = "card",
          div(style = "display: flex; gap: 12px; align-items: center;",
              actionButton("refresh_btn", "Refresh from SB API", class = "btn-primary"),
              actionButton("submit_btn", "Submit reviews", class = "btn-primary"),
              actionButton("rebuild_btn", "Rebuild from scratch", class = "btn-outline"),
              div(style = "flex:1;"),
              textOutput("last_saved", inline = TRUE))),
      div(class = "card",
          tabsetPanel(id = "tabs",
                      tabPanel("To review",
                               br(),
                               div(style = "margin-bottom:12px; font-size:12px; color:#666;",
                                   "Pick Action + Specify League (if needed) for each row, then Submit reviews."),
                               DTOutput("review_tbl")),
                      tabPanel("Directory changes",
                               br(),
                               div(style = "margin-bottom:12px; font-size:12px; color:#666;",
                                   "Rows flagged after FS Directory refresh. Confirm mapping or re-map."),
                               uiOutput("dir_changes_ui")),
                      tabPanel("1xBet mapping",
                               br(),
                               div(style = "margin-bottom:12px; display:flex; gap:12px; align-items:center;",
                                   actionButton("xbet_refresh_btn", "Refresh from 1xBet API",
                                                 class = "btn-primary"),
                                   actionButton("xbet_submit_btn", "Submit 1xBet reviews",
                                                 class = "btn-primary"),
                                   div(style = "flex:1;"),
                                   textOutput("xbet_last_action", inline = TRUE)),
                               div(style = "margin-bottom:12px;",
                                   radioButtons("xbet_filter", NULL,
                                     choices = c("Needs review" = "needs",
                                                  "Reviewed" = "reviewed",
                                                  "All" = "all"),
                                     selected = "needs", inline = TRUE)),
                               div(style = "margin-bottom:12px; font-size:12px; color:#666;",
                                   "Only Verified SB rows shown. Approve auto-match, mark as Not Available on 1xBet, or Specify manually."),
                               uiOutput("xbet_review_ui")),
                      tabPanel("Reviewed — Verified", br(), DTOutput("tbl_verified")),
                      tabPanel("Reviewed — Not Needed", br(), uiOutput("notneeded_ui")),
                      tabPanel("Removed from SB", br(), DTOutput("tbl_removed")))))
)

# ────────────────────────────────────────────────────────────
# SERVER
# ────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(
    mapper = NULL, sb = NULL, verified_fs = NULL,
    last_saved = "Not saved this session",
    # in-memory review inputs keyed by SB row key
    pending = list()
  )

  key_of <- function(country, league) paste0(country, "||", league)

  # Load mapper + verified FS on startup
  observe({
    if (!file.exists(MAP_PATH)) {
      showNotification("sporty_mapper.rds not found. Run build_sporty_mapper.R first.",
                       type = "error", duration = NULL)
      return()
    }
    rv$mapper <- readRDS(MAP_PATH)
    rv$verified_fs <- load_verified_fs()
  })

  # ═══ REFRESH ═══════════════════════════════════════════════
  observeEvent(input$refresh_btn, {
    req(rv$mapper)
    withProgress(message = "Fetching SB API + matching...", value = 0.2, {
      sb <- fetch_sportybet_directory()
      if (is.null(sb) || nrow(sb) == 0) {
        showNotification("SB API returned nothing.", type = "error"); return()
      }
      rv$sb <- sb
      incProgress(0.4, detail = "Matching")

      verified <- rv$verified_fs
      diag <- filter_to_sportybet_df(verified, sb, fuzzy_threshold = 0.75)$diagnostic
      matched <- diag[diag$matched, ]

      today <- as.character(Sys.Date())
      old <- rv$mapper

      new_mapper <- data.frame(
        sb_country    = sb$country,
        sb_league     = sb$league,
        category_id   = sb$category_id,
        tournament_id = sb$tournament_id,
        status        = NA_character_,
        fs_country    = NA_character_,
        fs_league     = NA_character_,
        match_reason  = NA_character_,
        score         = NA_real_,
        first_seen    = today,
        last_seen     = today,
        reviewed_on   = NA_character_,
        stringsAsFactors = FALSE
      )
      # Overlay helper-proposed matches
      for (i in seq_len(nrow(matched))) {
        m <- matched[i, ]
        idx <- which(new_mapper$sb_country == m$sb_country &
                     new_mapper$sb_league  == m$sb_league)
        if (length(idx) == 0) next
        new_mapper$fs_country[idx]   <- m$fs_country
        new_mapper$fs_league[idx]    <- m$fs_league
        new_mapper$match_reason[idx] <- m$reason
        new_mapper$score[idx]        <- m$score
      }

      # Preserve prior reviewed decisions.
      # Special case: if the old row was marked "Removed" and this SB name has
      # come back, auto-restore whatever status it had before removal.
      key_old <- key_of(old$sb_country, old$sb_league)
      key_new <- key_of(new_mapper$sb_country, new_mapper$sb_league)
      for (i in seq_along(key_new)) {
        j <- match(key_new[i], key_old)
        if (is.na(j)) next  # brand new — leave as-is (will fall through to "New — needs review" below)

        old_status <- old$status[j]
        was_reviewed <- !is.na(old$reviewed_on[j])

        # Preserve xbet_* fields in all cases where the row already existed.
        # (Ensure columns exist on new_mapper first.)
        xbet_cols <- c("xbet_country_id", "xbet_league_id",
                       "xbet_country_name", "xbet_league_name",
                       "xbet_match_reason", "xbet_reviewed_on")
        for (col in xbet_cols) {
          if (!col %in% names(new_mapper)) {
            new_mapper[[col]] <- if (col %in% c("xbet_country_id",
                                                 "xbet_league_id"))
                                    NA_integer_ else NA_character_
          }
          if (col %in% names(old)) new_mapper[[col]][i] <- old[[col]][j]
        }

        if (identical(old_status, "Removed")) {
          # League came back. Restore the pre-removal FS mapping and status if any.
          prior_status <- old$prior_status[j] %||% "Verified"
          new_mapper$status[i]      <- prior_status
          new_mapper$fs_country[i]  <- old$fs_country[j]
          new_mapper$fs_league[i]   <- old$fs_league[j]
          new_mapper$reviewed_on[i] <- old$reviewed_on[j]
          new_mapper$first_seen[i]  <- old$first_seen[j]
        } else if (was_reviewed) {
          # Ordinary reviewed row — carry decisions forward untouched
          new_mapper$status[i]      <- old_status
          new_mapper$fs_country[i]  <- old$fs_country[j]
          new_mapper$fs_league[i]   <- old$fs_league[j]
          new_mapper$reviewed_on[i] <- old$reviewed_on[j]
          new_mapper$first_seen[i]  <- old$first_seen[j]
        } else {
          # Present before, never reviewed → preserve first_seen only
          new_mapper$first_seen[i]  <- old$first_seen[j]
        }
      }

      # Removed SB entries — in old but not in new
      removed_keys <- setdiff(key_old, key_new)
      if (length(removed_keys) > 0) {
        rm_rows <- old[match(removed_keys, key_old), ]
        # Stash the current status BEFORE overwriting to "Removed" so we can
        # restore it when the SB league comes back.
        rm_rows$prior_status <- ifelse(rm_rows$status %in% c("Verified", "Not Needed"),
                                        rm_rows$status,
                                        rm_rows$prior_status %||% NA_character_)
        rm_rows$status <- "Removed"
        # Align columns: any column in rm_rows missing from new_mapper gets added as NA,
        # and vice versa. Handles xbet columns and any other schema drift.
        for (col in setdiff(names(rm_rows), names(new_mapper))) {
          new_mapper[[col]] <- NA
        }
        for (col in setdiff(names(new_mapper), names(rm_rows))) {
          rm_rows[[col]] <- NA
        }
        rm_rows <- rm_rows[, names(new_mapper), drop = FALSE]
        new_mapper <- rbind(new_mapper, rm_rows)
      }

      # Ensure prior_status column exists on new_mapper regardless
      if (!"prior_status" %in% names(new_mapper)) {
        new_mapper$prior_status <- NA_character_
      }

      rv$mapper <- new_mapper
      rv$pending <- list()
      showNotification(paste0(
        "To review: ", sum(is.na(new_mapper$reviewed_on) &
                            new_mapper$status != "Removed"),
        " \u00b7 Removed: ", sum(new_mapper$status == "Removed", na.rm = TRUE)),
        type = "message", duration = 6)
    })
  })

  # ═══ REBUILD (wipe and rebuild from scratch) ═══════════════
  observeEvent(input$rebuild_btn, {
    showModal(modalDialog(
      title = "Rebuild from scratch?",
      "This wipes all your reviewed decisions and rebuilds the mapper from the current SB API + helpers.",
      easyClose = FALSE,
      footer = tagList(
        modalButton("Cancel"),
        actionButton("rebuild_confirm", "Yes, rebuild", class = "btn btn-danger")
      )
    ))
  })
  observeEvent(input$rebuild_confirm, {
    removeModal()
    req(rv$verified_fs)
    withProgress(message = "Rebuilding...", value = 0.5, {
      sb <- fetch_sportybet_directory()
      if (is.null(sb)) return()
      diag <- filter_to_sportybet_df(rv$verified_fs, sb,
                                      fuzzy_threshold = 0.75)$diagnostic
      matched <- diag[diag$matched, ]
      today <- as.character(Sys.Date())
      new_mapper <- data.frame(
        sb_country    = sb$country, sb_league = sb$league,
        category_id   = sb$category_id, tournament_id = sb$tournament_id,
        status        = NA_character_, fs_country = NA_character_,
        fs_league     = NA_character_, match_reason = NA_character_,
        score         = NA_real_,
        first_seen    = today, last_seen = today, reviewed_on = NA_character_,
        stringsAsFactors = FALSE
      )
      for (i in seq_len(nrow(matched))) {
        m <- matched[i, ]
        idx <- which(new_mapper$sb_country == m$sb_country &
                     new_mapper$sb_league  == m$sb_league)
        if (length(idx) == 0) next
        new_mapper$fs_country[idx]   <- m$fs_country
        new_mapper$fs_league[idx]    <- m$fs_league
        new_mapper$match_reason[idx] <- m$reason
        new_mapper$score[idx]        <- m$score
      }
      rv$mapper <- new_mapper
      rv$pending <- list()
      showNotification("Rebuilt from scratch.", type = "warning", duration = 5)
    })
  })

  # ═══ RENDER REVIEW TABLE ══════════════════════════════════
  output$review_tbl <- renderDT({
    req(rv$mapper, rv$verified_fs)
    m <- rv$mapper
    to_review <- m[is.na(m$reviewed_on) &
                    (is.na(m$status) | m$status != "Removed"), , drop = FALSE]
    if (nrow(to_review) == 0) {
      return(datatable(data.frame(Message = "Nothing to review right now."),
                       options = list(dom = "t"), rownames = FALSE))
    }
    # Build per-row Action and Specify League <select> HTML
    action_html <- vapply(seq_len(nrow(to_review)), function(i) {
      default_action <- if (!is.na(to_review$fs_country[i])) "Approve" else "Not Needed"
      paste0(
        '<select class="action-sel" data-row="', i, '" ',
        'onchange="Shiny.setInputValue(\'action_', i, '\', this.value, {priority:\'event\'});',
        'var ct=document.getElementById(\'ctry_', i, '\');',
        'var lg=document.getElementById(\'specify_', i, '\');',
        'if(ct){ ct.disabled = (this.value !== \'Specify FS\'); }',
        'if(lg){ lg.disabled = (this.value !== \'Specify FS\'); }">',
        '<option value="Approve"', if (default_action == "Approve") ' selected' else '', '>Approve</option>',
        '<option value="Not Needed"', if (default_action == "Not Needed") ' selected' else '', '>Not Needed</option>',
        '<option value="Specify FS">Specify FS</option>',
        '</select>')
    }, character(1))

    fs_countries <- sort(unique(rv$verified_fs$country))
    country_html <- vapply(seq_len(nrow(to_review)), function(i) {
      # Default to SB country if it matches an FS country, else blank
      default_ct <- if (to_review$sb_country[i] %in% fs_countries)
                       to_review$sb_country[i] else ""
      opts <- paste0('<option value="">-- country --</option>',
                     paste0('<option value="', htmltools::htmlEscape(fs_countries),
                             '"', ifelse(fs_countries == default_ct, ' selected', ''),
                             '>', htmltools::htmlEscape(fs_countries), '</option>',
                             collapse = ""))
      paste0(
        '<select id="ctry_', i, '" class="ctry-sel" disabled ',
        'onchange="Shiny.setInputValue(\'specify_country_', i, '\', this.value, {priority:\'event\'});">',
        opts, '</select>')
    }, character(1))

    specify_html <- vapply(seq_len(nrow(to_review)), function(i) {
      # Pre-populate with leagues from SB country if it matches an FS country
      init_ct <- if (to_review$sb_country[i] %in% fs_countries)
                    to_review$sb_country[i] else NA_character_
      fs_choices <- if (!is.na(init_ct))
                      unmapped_fs_for_country(init_ct, rv$mapper, rv$verified_fs)
                    else character()
      opts <- paste0('<option value="">-- league --</option>',
                     paste0('<option value="', htmltools::htmlEscape(fs_choices),
                             '">', htmltools::htmlEscape(fs_choices), '</option>',
                             collapse = ""))
      paste0(
        '<select id="specify_', i, '" class="specify-sel" disabled ',
        'onchange="Shiny.setInputValue(\'specify_league_', i, '\', this.value, {priority:\'event\'});">',
        opts, '</select>')
    }, character(1))

    display <- data.frame(
      SB_Country     = to_review$sb_country,
      SB_League      = to_review$sb_league,
      Proposed_FS    = ifelse(is.na(to_review$fs_country), "—",
                              paste0(to_review$fs_country, " · ", to_review$fs_league)),
      Reason         = ifelse(is.na(to_review$match_reason), "no match", to_review$match_reason),
      Action         = action_html,
      Specify_Country = country_html,
      Specify_League = specify_html,
      stringsAsFactors = FALSE
    )

    datatable(display, rownames = FALSE, escape = FALSE,
              options = list(
                pageLength = 25, scrollX = TRUE, dom = "lftip",
                columnDefs = list(list(orderable = FALSE, targets = c(4, 5, 6))),
                rowCallback = JS(
                  "function(row, data) {",
                  "  var reason = String(data[3] || '').toLowerCase();",
                  "  if (reason.indexOf('exact') === 0) {",
                  "    $(row).css('background-color', '#E1F5EE');",
                  "  } else if (reason.indexOf('alias') === 0) {",
                  "    $(row).css('background-color', '#E1EDFB');",
                  "  } else if (reason.indexOf('fuzzy') === 0) {",
                  "    $(row).css('background-color', '#FFF4D6');",
                  "  } else {",
                  "    $(row).css('background-color', '#E0E0E0');",
                  "  }",
                  "}"
                )
              ),
              selection = "none",
              filter = "top")
  }, server = FALSE)

  # When Specify Country dropdown changes, repopulate its League dropdown
  observe({
    req(rv$mapper, rv$verified_fs)
    m <- rv$mapper
    to_review <- m[is.na(m$reviewed_on) &
                    (is.na(m$status) | m$status != "Removed"), , drop = FALSE]
    for (i in seq_len(nrow(to_review))) {
      local({
        idx <- i
        ct_input <- paste0("specify_country_", idx)
        observeEvent(input[[ct_input]], {
          ct <- input[[ct_input]]
          if (is.null(ct) || !nzchar(ct)) return()
          choices <- unmapped_fs_for_country(ct, rv$mapper, rv$verified_fs)
          opts <- paste0('<option value="">-- league --</option>',
                         paste0('<option value="', htmltools::htmlEscape(choices),
                                 '">', htmltools::htmlEscape(choices), '</option>',
                                 collapse = ""))
          # Push new HTML to the specify_<i> select via JS
          session$sendCustomMessage("updateLeagueOpts",
            list(id = paste0("specify_", idx), html = opts))
        }, ignoreInit = TRUE)
      })
    }
  })

  # ═══ SUBMIT REVIEWS ════════════════════════════════════════
  observeEvent(input$submit_btn, {
    req(rv$mapper)
    m <- rv$mapper
    to_review <- m[is.na(m$reviewed_on) &
                    (is.na(m$status) | m$status != "Removed"), , drop = FALSE]
    if (nrow(to_review) == 0) {
      showNotification("Nothing to submit.", type = "message"); return()
    }
    today <- as.character(Sys.Date())
    n_ok <- 0; n_skip <- 0

    for (i in seq_len(nrow(to_review))) {
      row <- to_review[i, ]
      action <- input[[paste0("action_", i)]]
      # If Shiny never got the action (user didn't touch the dropdown),
      # apply the sensible default: Approve if there's a proposal, else Not Needed
      if (is.null(action) || !nzchar(action)) {
        action <- if (!is.na(row$fs_country)) "Approve" else "Not Needed"
      }

      full_idx <- which(m$sb_country == row$sb_country &
                        m$sb_league  == row$sb_league)
      if (length(full_idx) == 0) { n_skip <- n_skip + 1; next }

      if (action == "Approve") {
        if (is.na(row$fs_country)) { n_skip <- n_skip + 1; next }
        m$status[full_idx]      <- "Verified"
        m$reviewed_on[full_idx] <- today
        n_ok <- n_ok + 1
      } else if (action == "Not Needed") {
        m$status[full_idx]      <- "Not Needed"
        m$fs_country[full_idx]  <- NA_character_
        m$fs_league[full_idx]   <- NA_character_
        m$reviewed_on[full_idx] <- today
        n_ok <- n_ok + 1
      } else if (action == "Specify FS") {
        ct <- input[[paste0("specify_country_", i)]]
        lg <- input[[paste0("specify_league_", i)]]
        # Fallback to SB country if user didn't touch the country dropdown
        if (is.null(ct) || !nzchar(ct)) ct <- row$sb_country
        if (is.null(lg) || !nzchar(lg)) { n_skip <- n_skip + 1; next }
        m$status[full_idx]      <- "Verified"
        m$fs_country[full_idx]  <- ct
        m$fs_league[full_idx]   <- lg
        m$match_reason[full_idx]<- "manual"
        m$score[full_idx]       <- 1.0
        m$reviewed_on[full_idx] <- today
        n_ok <- n_ok + 1
      }
    }

    rv$mapper <- m
    saveRDS(m, MAP_PATH)
    rv$last_saved <- paste("Saved at", format(Sys.time(), "%H:%M:%S"))
    showNotification(paste0("Submitted ", n_ok, " decisions",
                             if (n_skip > 0) paste0(" (", n_skip, " skipped)") else ""),
                     type = "message", duration = 5)
  })

  output$last_saved <- renderText(rv$last_saved)

  # ═══ BANNER ═══════════════════════════════════════════════
  output$banner <- renderUI({
    req(rv$mapper)
    m <- rv$mapper
    to_review <- sum(is.na(m$reviewed_on) &
                      (is.na(m$status) | m$status != "Removed"))
    flagged <- if ("needs_recheck" %in% names(m))
                 sum(!is.na(m$needs_recheck) & m$needs_recheck == TRUE &
                      !is.na(m$status) & m$status == "Verified")
               else 0
    div(class = "banner",
        div(div(class = "bnum warn", to_review),
            div(class = "blab warn", "To review")),
        div(div(class = "bnum info", flagged),
            div(class = "blab info", "Dir. flagged")),
        div(div(class = "bnum ok", sum(m$status == "Verified", na.rm = TRUE)),
            div(class = "blab ok", "Verified")),
        div(div(class = "bnum neu", sum(m$status == "Not Needed", na.rm = TRUE)),
            div(class = "blab neu", "Not Needed")),
        div(div(class = "bnum bad", sum(m$status == "Removed", na.rm = TRUE)),
            div(class = "blab bad", "Removed")))
  })

  # ═══ REVIEWED TABLES (read-only) ═══════════════════════════
  render_tbl <- function(subset_fn) {
    renderDT({
      req(rv$mapper)
      m <- subset_fn(rv$mapper)
      if (nrow(m) == 0) {
        return(datatable(data.frame(Message = "None."),
                         options = list(dom = "t"), rownames = FALSE))
      }
      out <- m[, c("sb_country", "sb_league", "fs_country", "fs_league",
                    "match_reason", "score", "reviewed_on")]
      out$reviewed_on <- suppressWarnings(as.Date(out$reviewed_on))
      datatable(out, rownames = FALSE, filter = "top",
        options = list(pageLength = 25, scrollX = TRUE, dom = "lftip"),
        selection = "none")
    }, server = TRUE)
  }
  output$tbl_verified  <- render_tbl(function(m)
    m[!is.na(m$status) & m$status == "Verified", ])
  output$tbl_removed   <- render_tbl(function(m)
    m[!is.na(m$status) & m$status == "Removed", ])

  # Not Needed — table with per-row Reopen buttons.
  # Reopen clears reviewed_on so the row lands back in "To review" next render.
  # Uses SB country+league as the button key (stable across row reorderings)
  # instead of positional index (which would let one click fire many handlers).
  output$notneeded_ui <- renderUI({
    req(rv$mapper)
    m <- rv$mapper
    nn <- m[!is.na(m$status) & m$status == "Not Needed", , drop = FALSE]
    if (nrow(nn) == 0) {
      return(div(style = "text-align:center; padding:32px; color:#888;",
                 "No Not Needed rows."))
    }
    # Encode SB country+league in the button's onclick — one input handler
    # for all buttons, differentiated by the key it sends.
    reopen_html <- vapply(seq_len(nrow(nn)), function(i) {
      key <- paste0(nn$sb_country[i], "||", nn$sb_league[i])
      key_esc <- gsub("'", "\\\\'", key)
      paste0('<button class="btn btn-outline" style="padding:4px 10px;font-size:10px;" ',
             'onclick="Shiny.setInputValue(\'reopen_nn_key\', {key:\'', key_esc,
             '\', rnd:Math.random()}, {priority:\'event\'});">Reopen</button>')
    }, character(1))
    display <- data.frame(
      SB_Country   = nn$sb_country,
      SB_League    = nn$sb_league,
      Reviewed_on  = nn$reviewed_on,
      Reopen       = reopen_html,
      stringsAsFactors = FALSE
    )
    datatable(display, rownames = FALSE, escape = FALSE,
      options = list(pageLength = 25, scrollX = TRUE, dom = "lftip",
                      columnDefs = list(list(orderable = FALSE, targets = 3))),
      selection = "none", filter = "top")
  })

  # Single observer handling all Reopen clicks.
  observeEvent(input$reopen_nn_key, {
    payload <- input$reopen_nn_key
    if (is.null(payload) || is.null(payload$key)) return()
    parts <- strsplit(payload$key, "||", fixed = TRUE)[[1]]
    if (length(parts) < 2) return()
    sb_c <- parts[1]; sb_l <- parts[2]
    m <- rv$mapper
    full_idx <- which(m$sb_country == sb_c & m$sb_league == sb_l)
    if (length(full_idx) == 0) return()
    m$status[full_idx]      <- NA_character_
    m$reviewed_on[full_idx] <- NA_character_
    rv$mapper <- m
    saveRDS(m, MAP_PATH)
    showNotification(paste0("Reopened: ", sb_c, " \u2022 ", sb_l),
                      type = "message", duration = 3)
  }, ignoreInit = TRUE)

  # ═══ DIRECTORY CHANGES TAB ═════════════════════════════════
  # Shows Verified rows flagged by FS Directory Refresher.
  # Each row has: Confirm (clears flag, keeps mapping) or Re-map (opens
  # league dropdown to reassign fs_league within same country).
  output$dir_changes_ui <- renderUI({
    req(rv$mapper, rv$verified_fs)
    m <- rv$mapper
    if (!"needs_recheck" %in% names(m)) {
      return(div(style = "text-align:center; padding:32px; color:#888;",
                 "No flagged rows. Run FS Refresher first."))
    }
    flagged <- m[!is.na(m$needs_recheck) & m$needs_recheck == TRUE &
                  !is.na(m$status) & m$status == "Verified", , drop = FALSE]
    if (nrow(flagged) == 0) {
      return(div(style = "text-align:center; padding:32px; color:#888;",
                 "No rows need re-check."))
    }
    lapply(seq_len(nrow(flagged)), function(i) {
      row <- flagged[i, ]
      # Country from FS side (mapper stores fs_country per row)
      ct <- row$fs_country
      fs_choices <- unmapped_fs_for_country(ct, m, rv$verified_fs)

      div(style = "border:1px solid #E5E0D5; border-radius:4px; padding:12px 16px;
                    margin-bottom:10px; background:#FAFAFA;",
          div(style = "font-weight:700; font-size:13px; margin-bottom:6px;",
              paste0(row$sb_country, " \u2022 ", row$sb_league)),
          div(style = "font-size:11px; color:#666; margin-bottom:8px;",
              paste0("Current mapping: ", ct, " \u00b7 ", row$fs_league,
                     " (needs re-check after directory change)")),
          div(style = "display:flex; gap:12px; align-items:center;",
              selectInput(paste0("remap_league_", i), NULL,
                          choices = c("(keep current)" = "",
                                       unique(c(row$fs_league, fs_choices))),
                          selected = row$fs_league, width = "300px"),
              actionButton(paste0("confirm_dir_", i), "Confirm",
                           class = "btn-primary",
                           style = "padding:6px 14px; font-size:11px;"),
              actionButton(paste0("remap_dir_", i), "Re-map",
                           class = "btn-outline",
                           style = "padding:6px 14px; font-size:11px;")
          ))
    })
  })

  # Handle Confirm / Re-map clicks in Directory changes tab
  observe({
    req(rv$mapper, rv$verified_fs)
    m <- rv$mapper
    if (!"needs_recheck" %in% names(m)) return()
    flagged <- m[!is.na(m$needs_recheck) & m$needs_recheck == TRUE &
                  !is.na(m$status) & m$status == "Verified", , drop = FALSE]

    for (i in seq_len(nrow(flagged))) {
      local({
        idx <- i
        # CONFIRM: clear the flag, keep existing mapping
        confirm_id <- paste0("confirm_dir_", idx)
        observeEvent(input[[confirm_id]], {
          mm <- rv$mapper
          fl <- mm[!is.na(mm$needs_recheck) & mm$needs_recheck == TRUE &
                    !is.na(mm$status) & mm$status == "Verified", , drop = FALSE]
          if (idx > nrow(fl)) return()
          row <- fl[idx, ]
          full_idx <- which(mm$sb_country == row$sb_country &
                            mm$sb_league  == row$sb_league)
          if (length(full_idx) == 0) return()
          mm$needs_recheck[full_idx] <- FALSE
          rv$mapper <- mm
          saveRDS(mm, MAP_PATH)
          showNotification(paste0("Confirmed: ", row$sb_league),
                            type = "message", duration = 3)
        }, ignoreInit = TRUE)

        # RE-MAP: update fs_league to whatever dropdown shows, clear the flag
        remap_id <- paste0("remap_dir_", idx)
        observeEvent(input[[remap_id]], {
          mm <- rv$mapper
          fl <- mm[!is.na(mm$needs_recheck) & mm$needs_recheck == TRUE &
                    !is.na(mm$status) & mm$status == "Verified", , drop = FALSE]
          if (idx > nrow(fl)) return()
          row <- fl[idx, ]
          new_lg <- input[[paste0("remap_league_", idx)]]
          if (is.null(new_lg) || !nzchar(new_lg)) {
            showNotification("Pick a league from the dropdown first.",
                              type = "warning"); return()
          }
          full_idx <- which(mm$sb_country == row$sb_country &
                            mm$sb_league  == row$sb_league)
          if (length(full_idx) == 0) return()
          mm$fs_league[full_idx]     <- new_lg
          mm$match_reason[full_idx]  <- "manual (post-directory refresh)"
          mm$needs_recheck[full_idx] <- FALSE
          mm$reviewed_on[full_idx]   <- as.character(Sys.Date())
          rv$mapper <- mm
          saveRDS(mm, MAP_PATH)
          showNotification(paste0("Re-mapped ", row$sb_league, " \u2192 ",
                                    new_lg), type = "message", duration = 4)
        }, ignoreInit = TRUE)
      })
    }
  })

  # ═══ 1xBET MAPPING TAB ═════════════════════════════════════
  XBET_CACHE_PATH <- file.path(BASE_PATH, "onexbet_directory_cache.rds")
  isolate({
    rv$xbet_dir <- if (file.exists(XBET_CACHE_PATH))
                      tryCatch(readRDS(XBET_CACHE_PATH),
                                error = function(e) NULL) else NULL
    rv$xbet_diag <- NULL
    rv$xbet_last_action <- if (!is.null(rv$xbet_dir))
                              paste("Loaded cached directory (",
                                    nrow(rv$xbet_dir), "leagues)")
                           else "Not refreshed yet"
  })

  output$xbet_last_action <- renderText(rv$xbet_last_action)

  # Refresh: fetch 1xBet directory + auto-match against Verified SB rows.
  # Preserves prior 1xBet reviews (xbet_reviewed_on stamped) untouched.
  observeEvent(input$xbet_refresh_btn, {
    req(rv$mapper)
    if (!exists("fetch_onexbet_directory")) {
      showNotification("onexbet_helpers.R not loaded.", type = "error"); return()
    }
    withProgress(message = "Fetching 1xBet directory...", value = 0.3, {
      xdir <- fetch_onexbet_directory()
      if (is.null(xdir) || nrow(xdir) == 0) {
        showNotification("1xBet API returned nothing.", type = "error"); return()
      }
      rv$xbet_dir <- xdir
      saveRDS(xdir, XBET_CACHE_PATH)
      incProgress(0.5, detail = "Matching Verified SB rows")
      diag <- match_verified_to_onexbet(rv$mapper, xdir, fuzzy_threshold = 0.75)
      rv$xbet_diag <- diag

      # Ensure xbet columns exist on mapper
      m <- rv$mapper
      for (col in c("xbet_country_id", "xbet_league_id",
                     "xbet_country_name", "xbet_league_name",
                     "xbet_match_reason", "xbet_reviewed_on")) {
        if (!col %in% names(m)) {
          m[[col]] <- if (col == "xbet_country_id" || col == "xbet_league_id")
                        NA_integer_ else NA_character_
        }
      }
      # Overlay diagnostic on unreviewed rows only (preserve previously
      # reviewed 1xBet mappings — same pattern as SB refresh preserves
      # reviewed_on rows).
      for (i in seq_len(nrow(diag))) {
        d <- diag[i, ]
        idx <- which(m$sb_country == d$sb_country & m$sb_league == d$sb_league)
        if (length(idx) == 0) next
        if (!is.na(m$xbet_reviewed_on[idx])) next   # user has reviewed — leave alone
        m$xbet_country_id[idx]   <- d$xbet_country_id
        m$xbet_league_id[idx]    <- d$xbet_league_id
        m$xbet_country_name[idx] <- d$xbet_country_name
        m$xbet_league_name[idx]  <- d$xbet_league_name
        m$xbet_match_reason[idx] <- d$reason
      }
      rv$mapper <- m
      saveRDS(m, MAP_PATH)
      rv$xbet_last_action <- paste("Refreshed at",
                                    format(Sys.time(), "%H:%M:%S"))
      showNotification(paste0(
        "1xBet directory: ", nrow(xdir), " leagues \u00b7 ",
        "SB \u2192 1xBet matches: ", sum(!is.na(diag$xbet_league_id)),
        " / ", nrow(diag)),
        type = "message", duration = 5)
    })
  })

  # Render the review UI for the 1xBet tab. Shows only Verified SB rows,
  # colour-coded by match reason (exact / fuzzy / country_unmatched / no_match).
  output$xbet_review_ui <- renderUI({
    req(rv$mapper)
    m <- rv$mapper
    verified <- m[!is.na(m$status) & m$status == "Verified" &
                   !is.na(m$fs_country) & !is.na(m$fs_league), , drop = FALSE]
    # Ensure column exists for the filter logic
    if (!"xbet_reviewed_on" %in% names(verified))
      verified$xbet_reviewed_on <- NA_character_
    # Apply filter
    filt <- input$xbet_filter %||% "needs"
    verified <- switch(filt,
      "needs"    = verified[is.na(verified$xbet_reviewed_on), , drop = FALSE],
      "reviewed" = verified[!is.na(verified$xbet_reviewed_on), , drop = FALSE],
      verified)
    if (nrow(verified) == 0) {
      return(div(style = "text-align:center; padding:32px; color:#888;",
                 if (filt == "needs") "All Verified SB rows reviewed."
                 else if (filt == "reviewed") "No reviewed rows yet."
                 else "No Verified SB rows yet."))
    }
    # Prepare 1xBet country + league choices for dropdowns
    xdir <- rv$xbet_dir
    xbet_countries <- if (!is.null(xdir)) sort(unique(xdir$country_name))
                       else character()

    # Stable per-row keys — the ONLY safe identifier across render/submit.
    # Positional index breaks when filter changes or ordering shifts.
    row_keys <- paste0(verified$sb_country, "||", verified$sb_league)
    row_ids  <- gsub("[^A-Za-z0-9]", "_", row_keys)

    # Per-row Action + Country + League dropdowns via JS bridge
    action_html <- vapply(seq_len(nrow(verified)), function(i) {
      rid <- row_ids[i]
      default <- if (!is.na(verified$xbet_league_id[i])) "Approve"
                  else "Not Available"
      paste0(
        '<select data-row="', rid, '" ',
        'onchange="Shiny.setInputValue(\'xbet_action_', rid, '\', this.value, {priority:\'event\'});',
        'var ct=document.getElementById(\'xbet_ct_', rid, '\');',
        'var lg=document.getElementById(\'xbet_lg_', rid, '\');',
        'if(ct){ct.disabled=(this.value!==\'Specify 1xBet\');}',
        'if(lg){lg.disabled=(this.value!==\'Specify 1xBet\');}">',
        '<option value="Approve"', if (default == "Approve") ' selected' else '', '>Approve</option>',
        '<option value="Not Available"', if (default == "Not Available") ' selected' else '', '>Not Available on 1xBet</option>',
        '<option value="Specify 1xBet">Specify 1xBet</option>',
        '</select>')
    }, character(1))

    country_html <- vapply(seq_len(nrow(verified)), function(i) {
      rid <- row_ids[i]
      default_ct <- verified$xbet_country_name[i] %||%
                     (if (verified$fs_country[i] %in% xbet_countries)
                        verified$fs_country[i] else "")
      opts <- paste0('<option value="">-- country --</option>',
                     paste0('<option value="', htmltools::htmlEscape(xbet_countries),
                             '"', ifelse(xbet_countries == default_ct,
                                          ' selected', ''),
                             '>', htmltools::htmlEscape(xbet_countries),
                             '</option>', collapse = ""))
      paste0('<select id="xbet_ct_', rid, '" disabled ',
             'onchange="Shiny.setInputValue(\'xbet_specify_ct_', rid,
             '\', this.value, {priority:\'event\'});">',
             opts, '</select>')
    }, character(1))

    league_html <- vapply(seq_len(nrow(verified)), function(i) {
      rid <- row_ids[i]
      init_ct <- verified$xbet_country_name[i] %||%
                  (if (verified$fs_country[i] %in% xbet_countries)
                    verified$fs_country[i] else NA_character_)
      choices <- if (!is.na(init_ct) && !is.null(xdir))
                    sort(unique(xdir$league_name_only[
                      xdir$country_name == init_ct]))
                  else character()
      opts <- paste0('<option value="">-- league --</option>',
                     paste0('<option value="', htmltools::htmlEscape(choices),
                             '">', htmltools::htmlEscape(choices),
                             '</option>', collapse = ""))
      paste0('<select id="xbet_lg_', rid, '" disabled ',
             'onchange="Shiny.setInputValue(\'xbet_specify_lg_', rid,
             '\', this.value, {priority:\'event\'});">',
             opts, '</select>')
    }, character(1))

    display <- data.frame(
      SB_Country      = verified$sb_country,
      SB_League       = verified$sb_league,
      FS_League       = verified$fs_league,
      Proposed_1xBet  = ifelse(is.na(verified$xbet_league_id), "\u2014",
                                paste0(verified$xbet_country_name, " \u00b7 ",
                                       verified$xbet_league_name)),
      Reason          = ifelse(is.na(verified$xbet_match_reason),
                                "not attempted", verified$xbet_match_reason),
      Action          = action_html,
      Specify_Country = country_html,
      Specify_League  = league_html,
      Reviewed_on     = verified$xbet_reviewed_on %||% NA,
      stringsAsFactors = FALSE
    )

    datatable(display, rownames = FALSE, escape = FALSE,
      options = list(
        pageLength = 25, scrollX = TRUE, dom = "lftip",
        columnDefs = list(list(orderable = FALSE, targets = c(5, 6, 7))),
        rowCallback = JS(
          "function(row, data) {",
          "  var reason = String(data[4] || '').toLowerCase();",
          "  if (reason.indexOf('exact') === 0) {",
          "    $(row).css('background-color', '#E1F5EE');",
          "  } else if (reason.indexOf('fuzzy') === 0) {",
          "    $(row).css('background-color', '#FFF4D6');",
          "  } else if (reason.indexOf('country_unmatched') === 0) {",
          "    $(row).css('background-color', '#F5E5E5');",
          "  } else if (reason.indexOf('no_match') === 0) {",
          "    $(row).css('background-color', '#E0E0E0');",
          "  }",
          "}")),
      selection = "none", filter = "top")
  })

  # When 1xBet Specify country dropdown changes → repopulate its league list.
  # Uses stable IDs (from sb_country||sb_league) so filter changes don't
  # break the wiring.
  observe({
    req(rv$mapper, rv$xbet_dir)
    m <- rv$mapper
    verified <- m[!is.na(m$status) & m$status == "Verified" &
                   !is.na(m$fs_country) & !is.na(m$fs_league), , drop = FALSE]
    row_keys <- paste0(verified$sb_country, "||", verified$sb_league)
    row_ids  <- gsub("[^A-Za-z0-9]", "_", row_keys)
    for (i in seq_len(nrow(verified))) {
      local({
        rid <- row_ids[i]
        ct_input <- paste0("xbet_specify_ct_", rid)
        observeEvent(input[[ct_input]], {
          ct <- input[[ct_input]]
          if (is.null(ct) || !nzchar(ct)) return()
          choices <- sort(unique(rv$xbet_dir$league_name_only[
            rv$xbet_dir$country_name == ct]))
          opts <- paste0('<option value="">-- league --</option>',
                         paste0('<option value="',
                                 htmltools::htmlEscape(choices), '">',
                                 htmltools::htmlEscape(choices), '</option>',
                                 collapse = ""))
          session$sendCustomMessage("updateLeagueOpts",
            list(id = paste0("xbet_lg_", rid), html = opts))
        }, ignoreInit = TRUE)
      })
    }
  })

  # Submit 1xBet reviews — writes decisions into mapper, stamps xbet_reviewed_on.
  # Iterates ONLY the currently-rendered/filtered rows via stable keys
  # (sb_country||sb_league). Never touches unrendered rows.
  observeEvent(input$xbet_submit_btn, {
    req(rv$mapper, rv$xbet_dir)
    m <- rv$mapper

    # Rebuild the SAME filtered subset the UI just rendered.
    verified <- m[!is.na(m$status) & m$status == "Verified" &
                   !is.na(m$fs_country) & !is.na(m$fs_league), , drop = FALSE]
    if (!"xbet_reviewed_on" %in% names(verified))
      verified$xbet_reviewed_on <- NA_character_
    filt <- input$xbet_filter %||% "needs"
    verified <- switch(filt,
      "needs"    = verified[is.na(verified$xbet_reviewed_on), , drop = FALSE],
      "reviewed" = verified[!is.na(verified$xbet_reviewed_on), , drop = FALSE],
      verified)
    if (nrow(verified) == 0) {
      showNotification("Nothing to submit.", type = "message"); return()
    }
    row_keys <- paste0(verified$sb_country, "||", verified$sb_league)
    row_ids  <- gsub("[^A-Za-z0-9]", "_", row_keys)

    today <- as.character(Sys.Date())
    n_ok <- 0; n_skip <- 0

    for (i in seq_len(nrow(verified))) {
      row <- verified[i, ]
      rid <- row_ids[i]
      action <- input[[paste0("xbet_action_", rid)]]
      # If user never touched the Action dropdown for this row, default to what
      # the pre-selected value would have been at render time.
      if (is.null(action) || !nzchar(action)) {
        action <- if (!is.na(row$xbet_league_id)) "Approve" else "Not Available"
      }
      full_idx <- which(m$sb_country == row$sb_country &
                        m$sb_league  == row$sb_league)
      if (length(full_idx) == 0) { n_skip <- n_skip + 1; next }

      if (action == "Approve") {
        if (is.na(row$xbet_league_id)) { n_skip <- n_skip + 1; next }
        m$xbet_reviewed_on[full_idx] <- today
        n_ok <- n_ok + 1
      } else if (action == "Not Available") {
        m$xbet_country_id[full_idx]   <- NA_integer_
        m$xbet_league_id[full_idx]    <- NA_integer_
        m$xbet_country_name[full_idx] <- NA_character_
        m$xbet_league_name[full_idx]  <- NA_character_
        m$xbet_match_reason[full_idx] <- "not_available"
        m$xbet_reviewed_on[full_idx]  <- today
        n_ok <- n_ok + 1
      } else if (action == "Specify 1xBet") {
        ct <- input[[paste0("xbet_specify_ct_", rid)]]
        lg <- input[[paste0("xbet_specify_lg_", rid)]]
        # Country fallback chain: user click → row's saved → row's FS country
        if (is.null(ct) || is.na(ct) || !nzchar(ct)) ct <- row$xbet_country_name
        if (is.null(ct) || is.na(ct) || !nzchar(ct)) {
          if (!is.null(rv$xbet_dir) &&
              row$fs_country %in% rv$xbet_dir$country_name) {
            ct <- row$fs_country
          }
        }
        # League fallback: user click → row's saved
        if (is.null(lg) || is.na(lg) || !nzchar(lg)) lg <- row$xbet_league_name
        if (is.null(ct) || is.na(ct) || !nzchar(ct) ||
            is.null(lg) || is.na(lg) || !nzchar(lg)) {
          n_skip <- n_skip + 1; next
        }
        pick <- rv$xbet_dir[rv$xbet_dir$country_name == ct &
                             rv$xbet_dir$league_name_only == lg, , drop = FALSE]
        if (nrow(pick) == 0) { n_skip <- n_skip + 1; next }
        m$xbet_country_id[full_idx]   <- pick$country_id[1]
        m$xbet_league_id[full_idx]    <- pick$league_id[1]
        m$xbet_country_name[full_idx] <- pick$country_name[1]
        m$xbet_league_name[full_idx]  <- pick$league_name_only[1]
        m$xbet_match_reason[full_idx] <- "manual"
        m$xbet_reviewed_on[full_idx]  <- today
        n_ok <- n_ok + 1
      }
    }

    rv$mapper <- m
    saveRDS(m, MAP_PATH)
    rv$xbet_last_action <- paste("Saved at", format(Sys.time(), "%H:%M:%S"))
    showNotification(paste0("Submitted ", n_ok, " 1xBet decisions",
                             if (n_skip > 0) paste0(" (", n_skip, " skipped)") else ""),
                     type = "message", duration = 5)
  })
}

shinyApp(ui, server)

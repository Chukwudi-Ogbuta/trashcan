library(shiny)
library(DT)
library(shinyjs)
library(dplyr)
library(future)
library(promises)
library(openxlsx)
library(jsonlite)
library(stringdist)

# Use multisession so pipeline runs in background
# ============================================================
# SLIP VALIDATOR FUNCTIONS
# ============================================================

PYTHON_PATH    <- "C:/Users/Ogbuta/AppData/Local/Programs/Python/Python314/python.exe"
TESSERACT_PATH <- "C:/Program Files/Tesseract-OCR/tesseract.exe"

ocr_slip <- function(image_path) {
  if (!file.exists(PYTHON_PATH))    stop("Python not found at: ", PYTHON_PATH)
  if (!file.exists(TESSERACT_PATH)) stop("Tesseract not found at: ", TESSERACT_PATH)
  if (!file.exists(image_path))     stop("Image not found at: ", image_path)
  
  # Write Python script to a regular file with predictable path (not tempfile —
  # tempfile paths can have spaces/special chars that break shell on Windows)
  tmp_py    <- file.path(tempdir(), "goaledge_ocr.py")
  out_file  <- file.path(tempdir(), "goaledge_ocr_out.txt")
  err_file  <- file.path(tempdir(), "goaledge_ocr_err.txt")
  
  py_script <- paste0(
    "import sys\n",
    "try:\n",
    "    from PIL import Image\n",
    "    import pytesseract\n",
    "    pytesseract.pytesseract.tesseract_cmd = r\"", TESSERACT_PATH, "\"\n",
    "    img = Image.open(sys.argv[1])\n",
    "    text = pytesseract.image_to_string(img)\n",
    "    with open(sys.argv[2], 'w', encoding='utf-8') as f:\n",
    "        f.write(text)\n",
    "except Exception as e:\n",
    "    with open(sys.argv[3], 'w', encoding='utf-8') as f:\n",
    "        f.write(str(e))\n",
    "    sys.exit(1)\n"
  )
  writeLines(py_script, tmp_py)
  
  # Clear previous output files
  if (file.exists(out_file)) file.remove(out_file)
  if (file.exists(err_file)) file.remove(err_file)
  
  # Run with explicit shell command
  cmd <- paste(shQuote(PYTHON_PATH),
               shQuote(tmp_py),
               shQuote(image_path),
               shQuote(out_file),
               shQuote(err_file))
  
  exit_code <- tryCatch(
    suppressWarnings(system(cmd, intern = FALSE, wait = TRUE,
                            ignore.stdout = TRUE, ignore.stderr = TRUE)),
    error = function(e) -1
  )
  
  if (file.exists(err_file)) {
    err_msg <- paste(readLines(err_file, warn = FALSE), collapse = " | ")
    stop("Python OCR error: ", err_msg)
  }
  
  if (!file.exists(out_file)) {
    stop("Python OCR produced no output (exit code: ", exit_code, ")")
  }
  
  text <- paste(readLines(out_file, warn = FALSE, encoding = "UTF-8"),
                collapse = "\n")
  text
}

parse_slip <- function(text, market_type) {
  lines <- trimws(strsplit(text, "\n")[[1]])
  lines <- lines[nchar(lines) > 0]
  fixtures <- list()
  i <- 1
  while (i <= length(lines)) {
    line       <- lines[i]
    line_clean <- trimws(gsub("^[^a-zA-Z]+", "", line))
    is_sel     <- grepl(
      "^(Home or Draw|Draw or Away|Home or Away|Yes|No|Over\\s+[0-9]+(\\.[0-9]+)?)",
      line_clean, ignore.case = TRUE, perl = TRUE
    )
    if (is_sel && i + 1 <= length(lines)) {
      next_line <- trimws(lines[i + 1])
      if (grepl(" vs ", next_line, fixed = TRUE)) {
        teams     <- strsplit(next_line, " vs ", fixed = TRUE)[[1]]
        home      <- trimws(teams[1])
        away      <- if (length(teams) >= 2) trimws(teams[2]) else ""
        selection <- trimws(gsub("(Over\\s+[0-9]+(?:\\.[0-9]+)?)\\s+[0-9]+\\.[0-9]+$", "\\1", line_clean, perl=TRUE))
        selection <- trimws(gsub("(Home or Draw|Draw or Away|Home or Away|Yes|No)\\s+[0-9]+\\.[0-9]+$", "\\1", selection, perl=TRUE))
        market_line <- if (i + 2 <= length(lines)) trimws(lines[i + 2]) else ""
        fixtures[[length(fixtures) + 1]] <- list(
          home=home, away=away, selection=selection, market=market_line)
        i <- i + 3; next
      }
    }
    i <- i + 1
  }
  if (length(fixtures) == 0) return(data.frame())
  bind_rows(lapply(fixtures, as.data.frame, stringsAsFactors = FALSE))
}

val_smart_sim <- function(a, b) {
  a <- tolower(trimws(a)); b <- tolower(trimws(b))
  clean_fn <- function(x) gsub("\\s*(fc|afc|sc|ac|bk|if|fk|sk|il|ok|cf|cd|ud|sd)\\s*$", "", x, ignore.case=TRUE)
  a <- trimws(clean_fn(a)); b <- trimws(clean_fn(b))
  jw  <- 1 - stringdist(a, b, method="jw")
  ta  <- unlist(strsplit(a, "\\s+")); tb <- unlist(strsplit(b, "\\s+"))
  tok <- length(intersect(ta, tb)) / max(length(ta), length(tb))
  sh  <- if (nchar(a) <= nchar(b)) a else b
  lo  <- if (nchar(a) <= nchar(b)) b else a
  sub <- if (nchar(sh) >= 4 && grepl(sh, lo, fixed=TRUE)) 0.9 else 0
  max(jw, tok, sub)
}

val_find_match <- function(slip_home, slip_away, predictions, threshold=0.65) {
  if (nrow(predictions) == 0) return(NULL)
  best_score <- 0; best_row <- NULL
  for (i in seq_len(nrow(predictions))) {
    s <- (val_smart_sim(slip_home, predictions$HOME[i]) +
            val_smart_sim(slip_away, predictions$AWAY[i])) / 2
    if (s > best_score) { best_score <- s; best_row <- i }
  }
  if (best_score >= threshold && !is.null(best_row))
    list(row=best_row, score=round(best_score,3),
         pred_home=predictions$HOME[best_row], pred_away=predictions$AWAY[best_row],
         confidence=predictions$CONFIDENCE[best_row])
  else NULL
}

check_option <- function(selection, market_type, stronger_val) {
  sel <- tolower(trimws(selection))
  if (market_type == "Double Chance") {
    expected <- if (!is.na(stronger_val) && tolower(stronger_val) == "home") "home or draw" else "draw or away"
    booked   <- if (grepl("home or draw", sel)) "home or draw"
    else if (grepl("draw or away", sel)) "draw or away"
    else if (grepl("home or away", sel)) "home or away"
    else sel
    if (booked == expected) "CORRECT" else paste0("WRONG - expected ", toupper(expected), " but booked ", toupper(booked))
  } else if (market_type == "BTTS") {
    if (grepl("^yes", sel)) "CORRECT" else paste0("WRONG - expected YES but booked ", toupper(sel))
  } else if (market_type == "FH Over 0.5") {
    if (grepl("over.*0\\.5", sel)) "CORRECT" else paste0("WRONG - expected OVER 0.5 but booked ", toupper(sel))
  } else if (market_type == "Over 1.5 FT") {
    if (grepl("over.*1\\.5", sel) || grepl("^over 2$", tolower(trimws(sel)))) "CORRECT"
    else paste0("WRONG - expected OVER 1.5 but booked ", toupper(sel))
  } else if (market_type == "Over 2.5 FT") {
    if (grepl("over.*2\\.5", sel)) "CORRECT" else paste0("WRONG - expected OVER 2.5 but booked ", toupper(sel))
  } else "NOT CHECKED"
}

validate_slip <- function(slip_df, predictions, market_type) {
  if (nrow(slip_df) == 0 || nrow(predictions) == 0) return(data.frame())
  has_stronger <- "STRONGER" %in% toupper(names(predictions))
  results <- lapply(seq_len(nrow(slip_df)), function(i) {
    home <- slip_df$home[i]; away <- slip_df$away[i]; sel <- slip_df$selection[i]
    m    <- val_find_match(home, away, predictions)
    if (is.null(m)) {
      d <- data.frame(slip_home=home, slip_away=away, selection=sel,
                      status="NOT FOUND", option_check=NA,
                      pred_home=NA, pred_away=NA, confidence=NA, sim_score=NA,
                      stringsAsFactors=FALSE)
      if (has_stronger) d$stronger <- NA
      d
    } else {
      stronger_val <- if (has_stronger) {
        col <- names(predictions)[toupper(names(predictions)) == "STRONGER"][1]
        as.character(predictions[[col]][m$row])
      } else NA
      opt   <- check_option(sel, market_type, stronger_val)
      ovall <- if (opt %in% c("CORRECT","NOT CHECKED")) "FOUND OK" else "FOUND - WRONG OPTION"
      d <- data.frame(slip_home=home, slip_away=away, selection=sel,
                      status=ovall, option_check=opt,
                      pred_home=m$pred_home, pred_away=m$pred_away,
                      confidence=m$confidence, sim_score=m$score,
                      stringsAsFactors=FALSE)
      if (has_stronger) d$stronger <- stronger_val
      d
    }
  })
  bind_rows(results)
}


plan(multisession)

# ── Source pipeline scripts ──
PIPELINE_PATH  <- "C:/Users/Ogbuta/OneDrive/New Projects/WorldFootball.R"
ANALYSIS_PATH  <- "C:/Users/Ogbuta/OneDrive/New Projects/football_analysis.R"
SETTINGS_PATH  <- "C:/Users/Ogbuta/OneDrive/New Projects/goaledge_settings.json"

`%||%` <- function(a, b) if (!is.null(a) && !is.na(a)) a else b

load_settings <- function() {
  if (file.exists(SETTINGS_PATH))
    tryCatch(fromJSON(SETTINGS_PATH), error=function(e) NULL)
  else NULL
}

save_settings <- function(settings) {
  tryCatch(write(toJSON(settings, auto_unbox=TRUE), SETTINGS_PATH), error=function(e) invisible())
}

# ── Excel export — predictions ──
export_predictions_excel <- function(results, path) {
  wb <- createWorkbook()
  
  hs     <- createStyle(fontName="Calibri", fontSize=11, fontColour="#FFFFFF",
                        fgFill="#DA291C", halign="CENTER", valign="CENTER",
                        textDecoration="BOLD", border="Bottom", borderColour="#C62828")
  body_s <- createStyle(fontName="Calibri", fontSize=10, fontColour="#111111",
                        valign="CENTER", border="Bottom", borderColour="#EEEEEE")
  alt_s  <- createStyle(fontName="Calibri", fontSize=10, fontColour="#111111",
                        fgFill="#FCE4EC", valign="CENTER",
                        border="Bottom", borderColour="#EEEEEE")
  ch_s   <- createStyle(fontName="Calibri", fontSize=10, fontColour="#15803D",
                        fgFill="#DCFCE7", halign="CENTER", textDecoration="BOLD",
                        border="Bottom", borderColour="#EEEEEE")
  cm_s   <- createStyle(fontName="Calibri", fontSize=10, fontColour="#92400E",
                        fgFill="#FEF3C7", halign="CENTER", textDecoration="BOLD",
                        border="Bottom", borderColour="#EEEEEE")
  cl_s   <- createStyle(fontName="Calibri", fontSize=10, fontColour="#DA291C",
                        fgFill="#FCE4EC", halign="CENTER",
                        border="Bottom", borderColour="#EEEEEE")
  ctr_s  <- createStyle(halign="CENTER")
  
  sheets <- list(
    list(name="FH Over 0.5",   data=results$fh_over05,     cols=c("league","home","away","fixture_date","home_href","away_href","competition_id","home_position","away_position","confidence")),
    list(name="Over 1.5 FT",   data=results$over15,        cols=c("league","home","away","fixture_date","home_href","away_href","competition_id","home_position","away_position","confidence")),
    list(name="Over 2.5 FT",   data=results$over25,        cols=c("league","home","away","fixture_date","home_href","away_href","competition_id","home_position","away_position","confidence")),
    list(name="BTTS",          data=results$btts,          cols=c("league","home","away","fixture_date","home_href","away_href","competition_id","home_position","away_position","confidence")),
    list(name="Double Chance", data=results$double_chance, cols=c("league","home","away","fixture_date","home_href","away_href","competition_id","home_position","away_position","stronger","market","confidence"))
  )
  
  for (sh in sheets) {
    if (is.null(sh$data) || nrow(sh$data) == 0) next
    addWorksheet(wb, sh$name, tabColour="#DA291C", gridLines=FALSE)
    d <- sh$data[, intersect(sh$cols, names(sh$data)), drop=FALSE]
    hdrs <- toupper(gsub("_"," ", names(d)))
    writeData(wb, sh$name, as.data.frame(t(hdrs)), startRow=1, startCol=1, colNames=FALSE)
    addStyle(wb, sh$name, hs, rows=1, cols=1:ncol(d), gridExpand=TRUE)
    setRowHeights(wb, sh$name, rows=1, heights=30)
    writeData(wb, sh$name, d, startRow=2, startCol=1, colNames=FALSE)
    setRowHeights(wb, sh$name, rows=2:(nrow(d)+1), heights=20)
    for (r in 1:nrow(d)) {
      addStyle(wb, sh$name, if(r%%2==0) alt_s else body_s, rows=r+1, cols=1:ncol(d), gridExpand=TRUE)
      ci <- which(names(d)=="confidence")
      if (length(ci)>0) {
        v <- as.numeric(gsub("%","",d$confidence[r]))
        addStyle(wb, sh$name, if(!is.na(v)&&v>=90) ch_s else if(!is.na(v)&&v>=75) cm_s else cl_s,
                 rows=r+1, cols=ci)
      }
    }
    num_cols <- which(names(d) %in% c("home_position","away_position","confidence"))
    if (length(num_cols)>0)
      addStyle(wb, sh$name, ctr_s, rows=1:(nrow(d)+1), cols=num_cols, gridExpand=TRUE, stack=TRUE)
    widths <- sapply(names(d), function(cn) min(max(nchar(cn)+4, max(nchar(as.character(d[[cn]])),na.rm=TRUE)+2, 10), 40))
    setColWidths(wb, sh$name, cols=1:ncol(d), widths=widths)
    freezePane(wb, sh$name, firstRow=TRUE)
    addFilter(wb, sh$name, rows=1, cols=1:ncol(d))
  }
  saveWorkbook(wb, path, overwrite=TRUE)
}

# ── Excel export — slug verification ──
export_slug_excel <- function(slug_verif, path) {
  wb <- createWorkbook()
  addWorksheet(wb, "Slug Verification", tabColour="#DA291C", gridLines=FALSE)
  
  hs     <- createStyle(fontName="Calibri", fontSize=11, fontColour="#FFFFFF",
                        fgFill="#DA291C", halign="CENTER", valign="CENTER",
                        textDecoration="BOLD", border="Bottom", borderColour="#C62828")
  good_s <- createStyle(fontName="Calibri", fontSize=10, fontColour="#15803D",
                        fgFill="#DCFCE7", border="Bottom", borderColour="#EEEEEE")
  med_s  <- createStyle(fontName="Calibri", fontSize=10, fontColour="#92400E",
                        fgFill="#FEF3C7", border="Bottom", borderColour="#EEEEEE")
  bad_s  <- createStyle(fontName="Calibri", fontSize=10, fontColour="#DA291C",
                        fgFill="#FCE4EC", border="Bottom", borderColour="#EEEEEE")
  body_s <- createStyle(fontName="Calibri", fontSize=10, fontColour="#111111",
                        border="Bottom", borderColour="#EEEEEE")
  
  d <- slug_verif %>% select(league, team, tl_name, tl_slug, sim)
  names(d) <- c("League","WF Team","TL Team","TL Slug","Sim Score")
  writeData(wb, 1, as.data.frame(t(names(d))), startRow=1, startCol=1, colNames=FALSE)
  addStyle(wb, 1, hs, rows=1, cols=1:ncol(d), gridExpand=TRUE)
  setRowHeights(wb, 1, rows=1, heights=30)
  writeData(wb, 1, d, startRow=2, startCol=1, colNames=FALSE)
  setRowHeights(wb, 1, rows=2:(nrow(d)+1), heights=20)
  
  for (r in 1:nrow(d)) {
    sim_val <- suppressWarnings(as.numeric(d$`Sim Score`[r]))
    row_sty <- if (!is.na(sim_val) && sim_val >= 0.85) good_s else
      if (!is.na(sim_val) && sim_val >= 0.70) med_s  else bad_s
    addStyle(wb, 1, row_sty, rows=r+1, cols=1:ncol(d), gridExpand=TRUE)
  }
  
  widths <- sapply(names(d), function(cn) min(max(nchar(cn)+4, max(nchar(as.character(d[[cn]])),na.rm=TRUE)+2, 12), 50))
  setColWidths(wb, 1, cols=1:ncol(d), widths=widths)
  freezePane(wb, 1, firstRow=TRUE)
  addFilter(wb, 1, rows=1, cols=1:ncol(d))
  saveWorkbook(wb, path, overwrite=TRUE)
}


# ============================================================
# UI — UNCHANGED from original
# ============================================================

ui <- fluidPage(
  useShinyjs(),
  
  tags$head(
    tags$link(rel = "preconnect", href = "https://fonts.googleapis.com"),
    tags$link(rel = "stylesheet", 
              href = "https://fonts.googleapis.com/css2?family=Barlow+Condensed:wght@300;400;600;700;900&family=JetBrains+Mono:wght@300;400;600&display=swap"),
    tags$style(HTML("
    
      /* ── Reset & Base ── */
      * { margin: 0; padding: 0; box-sizing: border-box; }
      
      :root {
        --bg:        #f7f7f8;
        --bg2:       #ffffff;
        --bg3:       #f2f2f4;
        --border:    #e4e4e8;
        --border2:   #d0d0d8;
        --accent:    #DA291C;
        --accent2:   #ff8a80;
        --accent3:   #b71c1c;
        --pink:      #fce4ec;
        --pink2:     #f48fb1;
        --text:      #111111;
        --text2:     #555555;
        --text3:     #999999;
        --font-head: 'Barlow Condensed', sans-serif;
        --font-mono: 'JetBrains Mono', monospace;
      }
      
      body {
        background: var(--bg);
        background-attachment: fixed;
        color: var(--text);
        font-family: var(--font-mono);
        min-height: 100vh;
        overflow-x: hidden;
      }
      
      /* ── Subtle background gradient ── */
      body::before {
        content: '';
        position: fixed;
        inset: 0;
        background-image:
          radial-gradient(circle at 80% 10%, rgba(218,41,28,0.06) 0%, transparent 50%),
          radial-gradient(circle at 10% 90%, rgba(252,228,236,0.5) 0%, transparent 50%);
        background-attachment: fixed;
        pointer-events: none;
        z-index: 0;
      }
      
      /* ── Header ── */
      .app-header {
        position: relative;
        z-index: 10;
        padding: 24px 32px 0;
        display: flex;
        align-items: flex-end;
        gap: 20px;
        border-bottom: 1px solid var(--border);
        padding-bottom: 0;
        background: rgba(255,255,255,0.96);
        box-shadow: 0 1px 0 var(--border), 0 4px 20px rgba(0,0,0,0.06);
      }
      
      .app-logo {
        font-family: var(--font-head);
        font-weight: 900;
        font-size: 42px;
        letter-spacing: -1px;
        color: #111111;
        line-height: 1;
        text-transform: uppercase;
      }
      
      .app-logo span { color: var(--accent); }
      .app-header { background: rgba(255,255,255,0.96) !important; }
      
      .app-subtitle {
        font-family: var(--font-mono);
        font-size: 10px;
        color: #999999;
        letter-spacing: 3px;
        text-transform: uppercase;
        padding-bottom: 8px;
      }
      
      /* ── Nav Tabs ── */
      .nav-tabs {
        border: none !important;
        gap: 4px;
        display: flex;
        padding: 0 32px;
        margin-top: 0 !important;
        background: transparent;
        border-bottom: 1px solid var(--border) !important;
      }
      
      .nav-tabs > li > a {
        font-family: var(--font-head) !important;
        font-weight: 600 !important;
        font-size: 13px !important;
        letter-spacing: 2px !important;
        text-transform: uppercase !important;
        color: #777777 !important;
        background: transparent !important;
        border: none !important;
        border-bottom: 2px solid transparent !important;
        padding: 12px 20px !important;
        border-radius: 0 !important;
        transition: all 0.2s !important;
      }
      
      .nav-tabs > li > a:hover {
        color: #111111 !important;
        background: transparent !important;
        border-bottom-color: var(--accent) !important;
      }
      
      .nav-tabs > li.active > a,
      .nav-tabs > li.active > a:focus,
      .nav-tabs > li.active > a:hover {
        color: var(--accent) !important;
        background: transparent !important;
        border: none !important;
        border-bottom: 3px solid var(--accent) !important;
        text-shadow: none !important;
        font-weight: 800 !important;
      }
      
      /* ── Tab Content ── */
      .tab-content {
        position: relative;
        z-index: 5;
        padding: 28px 32px;
        background: var(--bg);
      }
      
      /* ── Cards ── */
      .card {
        background: var(--bg2);
        border: 1px solid var(--border);
        border-radius: 12px;
        padding: 20px;
        margin-bottom: 20px;
        position: relative;
        overflow: hidden;
        box-shadow: 0 1px 4px rgba(0,0,0,0.06), 0 4px 16px rgba(0,0,0,0.04);
      }
      
      .card::before {
        content: '';
        position: absolute;
        top: 0; left: 0; right: 0;
        height: 1px;
        background: linear-gradient(90deg, transparent, var(--accent), transparent);
        opacity: 0;
        transition: opacity 0.3s;
      }
      
      .card:hover::before { opacity: 0.6; }
      .card:hover { border-color: var(--accent); box-shadow: 0 4px 24px rgba(218,41,28,0.1); }
      
      .card-title {
        font-family: var(--font-head);
        font-weight: 700;
        font-size: 11px;
        letter-spacing: 3px;
        text-transform: uppercase;
        color: #999999;
        margin-bottom: 16px;
      }
      
      /* ── Stat pills ── */
      .stats-row {
        display: flex;
        gap: 12px;
        flex-wrap: wrap;
        margin-bottom: 24px;
      }
      
      .stat-pill {
        background: var(--bg2);
        border: 1px solid var(--border);
        border-top: 3px solid var(--accent);
        border-radius: 10px;
        padding: 16px 18px;
        flex: 1;
        min-width: 120px;
        text-align: center;
        box-shadow: 0 2px 8px rgba(0,0,0,0.06);
        transition: all 0.2s;
      }
      
      .stat-pill:hover {
        border-color: var(--accent);
        box-shadow: 0 4px 20px rgba(0,0,0,0.3);
        transform: translateY(-2px);
      }
      
      .stat-pill .val {
        font-family: var(--font-head);
        font-weight: 900;
        font-size: 32px;
        line-height: 1;
        color: var(--text);
      }
      
      .stat-pill .lbl {
        font-size: 9px;
        letter-spacing: 2px;
        text-transform: uppercase;
        color: #999999;
        margin-top: 4px;
      }
      
      .stat-pill.blue .val  { color: var(--accent2); }
      .stat-pill.red .val   { color: var(--accent3); }
      .stat-pill.gold .val  { color: var(--gold); }
      .stat-pill.white .val { color: var(--text); }
      
      /* ── Run button ── */
      .run-btn {
        font-family: var(--font-head) !important;
        font-weight: 700 !important;
        font-size: 14px !important;
        letter-spacing: 3px !important;
        text-transform: uppercase !important;
        background: linear-gradient(135deg, #DA291C, #b01f14) !important;
        color: #ffffff !important;
        border: none !important;
        border-radius: 8px !important;
        padding: 14px 36px !important;
        cursor: pointer !important;
        transition: all 0.25s !important;
        box-shadow: 0 0 20px rgba(218, 41, 28, 0.3), 0 4px 12px rgba(0,0,0,0.3) !important;
      }
      
      .run-btn:hover {
        background: linear-gradient(135deg, #f03020, #DA291C) !important;
        box-shadow: 0 0 35px rgba(218, 41, 28, 0.45), 0 6px 20px rgba(0,0,0,0.4) !important;
        transform: translateY(-2px) !important;
      }
      
      .run-btn:disabled {
        background: rgba(17, 29, 53, 0.9) !important;
        color: var(--text3) !important;
        box-shadow: none !important;
        transform: none !important;
        cursor: not-allowed !important;
      }
      
      /* ── Log output ── */
      #pipeline_log {
        background: #111111;
        border: 1px solid #2a2a2a;
        border-radius: 8px;
        padding: 16px;
        font-family: var(--font-mono);
        font-size: 11px;
        color: #4ade80;
        height: 280px;
        overflow-y: auto;
        white-space: pre-wrap;
        line-height: 1.8;
        box-shadow: inset 0 2px 12px rgba(0,0,0,0.4);
      }
      
      /* ── Market tab pills ── */
      .market-tabs .nav-tabs {
        padding: 0 !important;
        border-bottom: 1px solid var(--border) !important;
        margin-bottom: 20px !important;
      }
      
      .market-tabs .nav-tabs > li > a {
        font-size: 11px !important;
        padding: 8px 14px !important;
        letter-spacing: 1px !important;
      }
      
      /* ── DataTables ── */
      .dataTables_wrapper {
        font-family: var(--font-mono) !important;
        font-size: 12px !important;
        color: var(--text) !important;
      }
      
      table.dataTable {
        background: transparent !important;
        border-collapse: separate !important;
        border-spacing: 0 !important;
        width: 100% !important;
      }
      
      table.dataTable thead th {
        background: #f7f7f8 !important;
        color: #888888 !important;
        font-family: var(--font-head) !important;
        font-size: 10px !important;
        letter-spacing: 2px !important;
        text-transform: uppercase !important;
        border: none !important;
        border-bottom: 1px solid var(--border) !important;
        padding: 10px 14px !important;
        font-weight: 600 !important;
      }
      
      table.dataTable tbody tr {
        background: transparent !important;
        transition: background 0.15s !important;
      }
      
      table.dataTable tbody tr:hover td { background: #fce4ec !important; }
      
      table.dataTable tbody td {
        border: none !important;
        border-bottom: 1px solid #eeeeee !important;
        padding: 11px 14px !important;
        color: #111111 !important;
        vertical-align: middle !important;
      }
      
      .dataTables_filter input,
      .dataTables_length select {
        background: #ffffff !important;
        border: 1px solid var(--border) !important;
        color: var(--text) !important;
        border-radius: 4px !important;
        padding: 4px 8px !important;
        font-family: var(--font-mono) !important;
        font-size: 11px !important;
      }
      
      .dataTables_info,
      .dataTables_filter label,
      .dataTables_length label {
        color: #999999 !important;
        font-size: 11px !important;
      }
      
      .paginate_button {
        background: #f7f7f8 !important;
        color: #888888 !important;
        border: 1px solid var(--border) !important;
        border-radius: 4px !important;
        margin: 0 2px !important;
        font-family: var(--font-mono) !important;
        font-size: 11px !important;
      }
      
      .paginate_button.current {
        background: var(--accent) !important;
        color: #ffffff !important;
        border-color: var(--accent) !important;
      }
      
      .paginate_button:hover { background: var(--pink) !important; color: var(--accent) !important; border-color: var(--accent) !important; }
      
      /* ── Confidence badges ── */
      .conf-high  { color: #15803d; font-weight: 700; background: #dcfce7; padding: 2px 8px; border-radius: 4px; }
      .conf-med   { color: #92400e; font-weight: 600; background: #fef3c7; padding: 2px 8px; border-radius: 4px; }
      .conf-low   { color: var(--accent); font-weight: 600; background: var(--pink); padding: 2px 8px; border-radius: 4px; }
      
      /* ── League badge ── */
      .league-badge {
        background: var(--bg3);
        border: 1px solid var(--border);
        border-radius: 4px;
        padding: 2px 8px;
        font-size: 10px;
        letter-spacing: 1px;
        color: #999999;
        text-transform: uppercase;
        white-space: nowrap;
      }
      
      /* ── Settings inputs ── */
      .settings-grid { display: grid; grid-template-columns: 1fr 1fr; gap: 16px; }
      
      .setting-row {
        background: var(--bg3);
        border: 1px solid var(--border);
        border-radius: 8px;
        padding: 14px 16px;
      }
      
      .shiny-input-container label {
        font-family: var(--font-mono) !important;
        font-size: 10px !important;
        letter-spacing: 2px !important;
        text-transform: uppercase !important;
        color: var(--text3) !important;
      }
      
      input[type='number'], input[type='text'], select, .form-control {
        background: var(--bg2) !important;
        border: 1px solid var(--border) !important;
        color: #111111 !important;
        border-radius: 4px !important;
        font-family: var(--font-mono) !important;
        font-size: 12px !important;
      }
      
      input[type='range'] { accent-color: var(--accent) !important; }
      .irs--shiny .irs-bar { background: var(--accent) !important; border-color: var(--accent) !important; }
      .irs--shiny .irs-handle { border-color: var(--accent) !important; }
      
      /* ── Save button ── */
      .save-btn {
        font-family: var(--font-head) !important;
        font-weight: 700 !important;
        font-size: 12px !important;
        letter-spacing: 2px !important;
        text-transform: uppercase !important;
        background: transparent !important;
        color: var(--accent) !important;
        border: 1px solid rgba(218,41,28,0.3) !important;
        border-radius: 8px !important;
        padding: 10px 24px !important;
        cursor: pointer !important;
        transition: all 0.2s !important;
      }
      
      .save-btn:hover {
        background: var(--pink) !important;
        border-color: var(--accent) !important;
        box-shadow: 0 2px 12px rgba(218,41,28,0.15) !important;
      }
      
      /* ── Status indicator ── */
      .status-dot {
        display: inline-block;
        width: 7px; height: 7px;
        border-radius: 50%;
        background: var(--text3);
        margin-right: 6px;
        vertical-align: middle;
      }
      
      .status-dot.running { background: var(--gold); animation: pulse 1s infinite; }
      .status-dot.done    { background: #22c55e; }
      .status-dot.error   { background: var(--accent3); }
      
      @keyframes pulse { 0%, 100% { opacity: 1; } 50% { opacity: 0.3; } }
      
      /* ── Slug verification table ── */
      .sim-high { color: #15803d; font-weight: 600; }
      .sim-med  { color: #92400e; font-weight: 500; }
      .sim-low  { color: var(--accent); font-weight: 700; }
      
      /* ── Step indicator ── */
      .steps-row {
        display: flex; gap: 0; margin-bottom: 20px;
        background: var(--bg2); border: 1px solid var(--border);
        border-radius: 8px; overflow: hidden;
      }
      
      .step-item {
        flex: 1; padding: 14px 16px; text-align: center;
        border-right: 1px solid var(--border); position: relative;
      }
      .step-item:last-child { border-right: none; }
      .step-num { font-family: var(--font-head); font-weight: 900; font-size: 22px; line-height: 1; color: var(--text3); }
      .step-name { font-size: 9px; letter-spacing: 2px; text-transform: uppercase; color: var(--text3); margin-top: 4px; }
      .step-item.active .step-num  { color: var(--gold); }
      .step-item.active .step-name { color: var(--gold); }
      .step-item.active { background: rgba(245, 200, 66, 0.05); }
      .step-item.done .step-num  { color: var(--accent); }
      .step-item.done .step-name { color: var(--accent); }
      .step-item.done { background: rgba(0, 229, 160, 0.05); }
      .step-item.error .step-num  { color: var(--accent3); }
      .step-item.error .step-name { color: var(--accent3); }
      
      /* ── Scrollbar ── */
      .selectize-dropdown { z-index: 9999 !important; }
      .selectize-control { z-index: 9998 !important; }
      .shiny-input-container { overflow: visible !important; }

      ::-webkit-scrollbar { width: 5px; height: 5px; }
      ::-webkit-scrollbar-track { background: var(--bg3); }
      ::-webkit-scrollbar-thumb { background: #cccccc; border-radius: 3px; }
      ::-webkit-scrollbar-thumb:hover { background: var(--accent); }
      
      .last-run { font-size: 10px; color: #999999; letter-spacing: 1px; padding-left: 8px; }
      
      .no-data {
        text-align: center; padding: 60px 20px;
        color: #999999; font-size: 12px;
        letter-spacing: 2px; text-transform: uppercase;
      }
      .no-data .icon { font-size: 36px; margin-bottom: 12px; }
      
    "))
  ),
  
  # ── Header ──
  div(class = "app-header",
      div(
        div(class = "app-logo", "GOAL", tags$span("EDGE")),
        div(class = "app-subtitle", "Football Intelligence System")
      ),
      div(style = "margin-left: auto; padding-bottom: 8px; display: flex; align-items: center; gap: 16px;",
          uiOutput("status_indicator"),
          uiOutput("last_run_ui")
      )
  ),
  
  # ── Main tabs ──
  tabsetPanel(id = "main_tabs",
              
              # ── PIPELINE ──
              tabPanel("⚡ Pipeline",
                       div(class = "tab-content",
                           uiOutput("stats_row"),
                           div(class = "card",
                               div(class = "card-title", "Pipeline Steps — Run Each In Order"),
                               div(style = "display: grid; grid-template-columns: 1fr 1fr 1fr 1fr; gap: 12px; margin-bottom: 8px;",
                                   div(style = "background: var(--bg3); border: 1px solid var(--border); border-top: 3px solid var(--accent); border-radius: 8px; padding: 16px;",
                                       div(style = "font-family: var(--font-head); font-size: 11px; letter-spacing: 2px; color: var(--accent); margin-bottom: 10px;", "STEP 1"),
                                       div(style = "font-size: 13px; font-weight: 600; color: var(--text); margin-bottom: 12px;", "Fixtures & Standings"),
                                       uiOutput("btn_step1_ui")
                                   ),
                                   div(style = "background: var(--bg3); border: 1px solid var(--border); border-top: 3px solid var(--accent); border-radius: 8px; padding: 16px;",
                                       div(style = "font-family: var(--font-head); font-size: 11px; letter-spacing: 2px; color: var(--accent); margin-bottom: 10px;", "STEP 2"),
                                       div(style = "font-size: 13px; font-weight: 600; color: var(--text); margin-bottom: 12px;", "Team Form"),
                                       uiOutput("btn_step2_ui")
                                   ),
                                   div(style = "background: var(--bg3); border: 1px solid var(--border); border-top: 3px solid var(--accent); border-radius: 8px; padding: 16px;",
                                       div(style = "font-family: var(--font-head); font-size: 11px; letter-spacing: 2px; color: var(--accent); margin-bottom: 10px;", "STEP 3"),
                                       div(style = "font-size: 13px; font-weight: 600; color: var(--text); margin-bottom: 12px;", "H2H Results"),
                                       uiOutput("btn_step3_ui")
                                   ),
                                   div(style = "background: var(--bg3); border: 1px solid var(--border); border-top: 3px solid var(--accent); border-radius: 8px; padding: 16px;",
                                       div(style = "font-family: var(--font-head); font-size: 11px; letter-spacing: 2px; color: var(--accent); margin-bottom: 10px;", "STEP 4"),
                                       div(style = "font-size: 13px; font-weight: 600; color: var(--text); margin-bottom: 12px;", "Run Analysis"),
                                       uiOutput("btn_step4_ui")
                                   )
                               ),
                               div(style = "display: flex; align-items: center; gap: 12px; flex-wrap: wrap; margin-top: 8px;",
                                   uiOutput("step1_badge"), uiOutput("step2_badge"),
                                   uiOutput("step3_badge"), uiOutput("step4_badge"),
                                   div(style = "margin-left: auto;",
                                       uiOutput("load_last_session_ui")
                                   )
                               )
                           ),
                           div(class = "card",
                               div(class = "card-title", "Pipeline Log"),
                               verbatimTextOutput("pipeline_log")
                           )
                       )
              ),
              
              # ── PREDICTIONS ──
              tabPanel("🎯 Predictions",
                       div(class = "tab-content",
                           # Export button row
                           div(style = "display: flex; justify-content: flex-end; margin-bottom: 16px;",
                               actionButton("export_predictions", "📥  Export to Excel", class = "save-btn")
                           ),
                           div(class = "market-tabs",
                               tabsetPanel(id = "market_tabs",
                                           tabPanel("⚽ FH Over 0.5",  uiOutput("fh_ui")),
                                           tabPanel("📈 Over 1.5",      uiOutput("o15_ui")),
                                           tabPanel("🔥 Over 2.5",      uiOutput("o25_ui")),
                                           tabPanel("🤝 BTTS",          uiOutput("btts_ui")),
                                           tabPanel("🛡 Double Chance", uiOutput("dc_ui"))
                               )
                           )
                       )
              ),
              
              # ── DATA ──
              tabPanel("📊 Data",
                       div(class = "tab-content",
                           div(style = "display: flex; gap: 12px; margin-bottom: 16px; flex-wrap: wrap;",
                               actionButton("save_data",       "💾  Save RDS Backup",  class = "save-btn"),
                               actionButton("export_data_csv", "📥  Export Data Excel",       class = "save-btn"),
                               uiOutput("data_summary_pills")
                           ),
                           div(class = "card",
                               div(class = "card-title", "Data Frame — Current State"),
                               div(style = "margin-bottom: 8px;", uiOutput("col_selector_ui")),
                               DTOutput("data_table")
                           )
                       )
              ),
              
              # ── SLUG CHECK ──
              tabPanel("🔗 Slug Check",
                       div(class = "tab-content",
                           div(class = "card",
                               div(class = "card-title", "Team Name Mapping — WorldFootball vs TablesLeague"),
                               div(style = "display: flex; align-items: center; justify-content: space-between; margin-bottom: 12px;",
                                   div(style = "font-size: 11px; color: var(--text3);",
                                       "🟢 sim ≥ 0.85  🟡 sim 0.70–0.84  🔴 sim < 0.70 or NA slug — review manually"
                                   ),
                                   actionButton("export_slug", "📥  Export Slug Check", class = "save-btn")
                               ),
                               DTOutput("slug_table")
                           )
                       )
              ),
              
              # ── SETTINGS ──
              tabPanel("⚙ Settings",
                       div(class = "tab-content",
                           div(class = "card",
                               div(class = "card-title", "Confidence Thresholds"),
                               div(class = "settings-grid",
                                   div(class = "setting-row", sliderInput("thresh_fh",  "First Half Over 0.5", 0.5, 0.99, 0.85, 0.01, width="100%")),
                                   div(class = "setting-row", sliderInput("thresh_o15", "Over 1.5 FT",         0.5, 0.99, 0.85, 0.01, width="100%")),
                                   div(class = "setting-row", sliderInput("thresh_o25", "Over 2.5 FT",         0.4, 0.99, 0.65, 0.01, width="100%")),
                                   div(class = "setting-row", sliderInput("thresh_btts","BTTS",                0.4, 0.99, 0.65, 0.01, width="100%")),
                                   div(class = "setting-row", sliderInput("thresh_dc",  "Double Chance",       0.5, 0.99, 0.85, 0.01, width="100%"))
                               ),
                               div(style = "margin-top: 12px;",
                                   actionButton("save_thresholds", "💾  Save Thresholds", class = "save-btn")
                               )
                           ),
                           div(class = "card",
                               div(class = "card-title", "TablesLeague URLs — Update if leagues change"),
                               div(style = "margin-bottom: 12px;",
                                   actionButton("save_urls", "💾  Save URLs", class = "save-btn")
                               ),
                               DTOutput("url_editor")
                           )
                       )
              ),
              
              # ══ SLIP VALIDATOR ══
              tabPanel("✅ Slip Validator",
                       div(class = "tab-content",
                           div(style = "display: grid; grid-template-columns: 1fr 1fr; gap: 16px;",
                               div(class = "card",
                                   div(class = "card-title", "Step 1 — Upload Predictions Excel"),
                                   fileInput("val_excel", "Predictions Excel (.xlsx)", accept=".xlsx", width="100%"),
                                   uiOutput("val_excel_status")
                               ),
                               div(class = "card",
                                   div(class = "card-title", "Step 2 — Upload Booking Slip Image"),
                                   fileInput("val_slip_img", "Sportybet Slip (JPG/PNG)", accept=c(".jpg",".jpeg",".png"), width="100%"),
                                   uiOutput("val_ocr_status")
                               )
                           ),
                           div(class = "card", style = "overflow: visible; z-index: 100; position: relative;",
                               div(class = "card-title", "Step 3 — Select Market and Validate"),
                               div(style = "display: grid; grid-template-columns: 2fr 1fr; gap: 16px; align-items: end;",
                                   selectInput("val_market", "Market Sheet",
                                               choices = c(
                                                 "FH Over 0.5  (Sportybet: Over 0.5)"          = "FH Over 0.5",
                                                 "Over 1.5 FT  (Sportybet: Over 1.5 / Over 2)" = "Over 1.5 FT",
                                                 "Over 2.5 FT  (Sportybet: Over 2.5)"          = "Over 2.5 FT",
                                                 "BTTS         (Sportybet: Yes / GG)"           = "BTTS",
                                                 "Double Chance (Sportybet: Home or Draw etc)"  = "Double Chance"
                                               ), width = "100%"),
                                   actionButton("val_run", "Validate Slip", class = "save-btn")
                               )
                           ),
                           uiOutput("val_ocr_preview"),
                           div(class = "card",
                               div(class = "card-title", "Processing Log"),
                               verbatimTextOutput("val_log")
                           ),
                           uiOutput("val_results")
                       )
              )
  )
)


# ============================================================
# SERVER
# ============================================================

server <- function(input, output, session) {
  
  rv <- reactiveValues(
    data_frame   = NULL,
    analysis_df  = NULL,
    results      = NULL,
    slug_verif   = NULL,
    log          = "[ GOALEDGE ] Ready. Click Run Pipeline to start.\n",
    status       = "idle",
    current_step = 0,
    last_run     = NULL,
    tl_urls      = NULL
  )
  
  add_log <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    rv$log <- paste0(rv$log, paste0("[", ts, "] ", msg, "\n"))
  }
  
  # ── Load scripts + saved settings on startup ──
  isolate({
    tryCatch({
      source(PIPELINE_PATH, local = TRUE)
      source(ANALYSIS_PATH, local = TRUE)
      rv$tl_urls <- tablesleague_config
      # Load persisted settings
      saved <- load_settings()
      if (!is.null(saved)) {
        if (!is.null(saved$thresh_fh))   updateSliderInput(session, "thresh_fh",   value = saved$thresh_fh)
        if (!is.null(saved$thresh_o15))  updateSliderInput(session, "thresh_o15",  value = saved$thresh_o15)
        if (!is.null(saved$thresh_o25))  updateSliderInput(session, "thresh_o25",  value = saved$thresh_o25)
        if (!is.null(saved$thresh_btts)) updateSliderInput(session, "thresh_btts", value = saved$thresh_btts)
        if (!is.null(saved$thresh_dc))   updateSliderInput(session, "thresh_dc",   value = saved$thresh_dc)
        if (!is.null(saved$tl_urls))     rv$tl_urls <- as.data.frame(saved$tl_urls, stringsAsFactors = FALSE)
      }
      add_log("[ OK ] Scripts loaded. Click a step to start.")
      
    }, error = function(e) {
      add_log(paste0("[ ERROR ] Could not load scripts: ", e$message))
    })
  })
  
  # ── Status indicator ──
  output$status_indicator <- renderUI({
    cls <- switch(rv$status, running="status-dot running", done="status-dot done",
                  error="status-dot error", "status-dot")
    lbl <- switch(rv$status, running="RUNNING", done="READY", error="ERROR", "IDLE")
    tags$span(tags$span(class=cls), lbl, style="font-size:10px; letter-spacing:2px; color: var(--text3);")
  })
  
  output$last_run_ui <- renderUI({
    if (!is.null(rv$last_run))
      tags$span(class="last-run", paste0("Last run: ", format(rv$last_run, "%d %b %Y %H:%M")))
  })
  
  # ── Stats row ──
  output$stats_row <- renderUI({
    if (is.null(rv$results)) {
      div(class="stats-row",
          div(class="stat-pill",      div(class="val","—"), div(class="lbl","Fixtures")),
          div(class="stat-pill",      div(class="val","—"), div(class="lbl","FH Over 0.5")),
          div(class="stat-pill blue", div(class="val","—"), div(class="lbl","Over 1.5")),
          div(class="stat-pill red",  div(class="val","—"), div(class="lbl","Over 2.5")),
          div(class="stat-pill gold", div(class="val","—"), div(class="lbl","BTTS")),
          div(class="stat-pill white",div(class="val","—"), div(class="lbl","Dbl Chance"))
      )
    } else {
      div(class="stats-row",
          div(class="stat-pill",      div(class="val",nrow(rv$data_frame)),           div(class="lbl","Fixtures")),
          div(class="stat-pill",      div(class="val",nrow(rv$results$fh_over05)),    div(class="lbl","FH Over 0.5")),
          div(class="stat-pill blue", div(class="val",nrow(rv$results$over15)),       div(class="lbl","Over 1.5")),
          div(class="stat-pill red",  div(class="val",nrow(rv$results$over25)),       div(class="lbl","Over 2.5")),
          div(class="stat-pill gold", div(class="val",nrow(rv$results$btts)),         div(class="lbl","BTTS")),
          div(class="stat-pill white",div(class="val",nrow(rv$results$double_chance)),div(class="lbl","Dbl Chance"))
      )
    }
  })
  
  # ── Log file + polling ──
  log_file <- tempfile(fileext=".txt")
  writeLines("", log_file)
  
  run_step_async <- function(step_num, step_fn, on_done, on_error) {
    rv$status      <- "running"
    rv$current_step <- step_num
    writeLines("", log_file)
    lf <- log_file; pp <- PIPELINE_PATH; ap <- ANALYSIS_PATH
    future(seed=TRUE, {
      source(pp, local=TRUE); source(ap, local=TRUE)
      withCallingHandlers(
        tryCatch(step_fn(), error=function(e) {
          cat(paste0("[ERROR] ", e$message, "\n"), file=lf, append=TRUE)
          list(.__error__=e$message)
        }),
        message=function(m) { cat(conditionMessage(m), file=lf, append=TRUE); invokeRestart("muffleMessage") }
      )
    }) %...>% on_done %...!% on_error
  }
  
  observe({
    invalidateLater(1000, session)
    req(rv$status == "running")
    lines <- tryCatch(readLines(isolate(log_file), warn=FALSE), error=function(e) character(0))
    lines <- lines[nchar(trimws(lines)) > 0]
    if (length(lines) > 0) rv$log <- paste(lines, collapse="\n")
  })
  
  # ── Step button UIs — fully reactive ──
  output$btn_step1_ui <- renderUI({
    if (rv$status=="running" && rv$current_step==1)
      tags$button("⏳ Running...", class="run-btn", disabled=TRUE, style="width:100%;font-size:11px;padding:10px;")
    else
      actionButton("btn_step1","▶ Run Step 1", class="run-btn", style="width:100%;font-size:11px;padding:10px;")
  })
  
  output$btn_step2_ui <- renderUI({
    # Step 2 requires Step 1 done — data_frame exists but H1 not yet populated
    has_fixtures <- !is.null(rv$data_frame) && nrow(rv$data_frame) > 0
    has_form     <- has_fixtures && "H1" %in% names(rv$data_frame) && !all(is.na(rv$data_frame$H1))
    ready        <- has_fixtures && !has_form
    if (rv$status=="running" && rv$current_step==2)
      tags$button("⏳ Running...", class="run-btn", disabled=TRUE, style="width:100%;font-size:11px;padding:10px;")
    else if (!ready && !has_form)
      tags$button("▶ Run Step 2", class="run-btn", disabled=TRUE, style="width:100%;font-size:11px;padding:10px;opacity:0.4;")
    else if (has_form)
      actionButton("btn_step2","▶ Re-run Step 2", class="run-btn", style="width:100%;font-size:11px;padding:10px;")
    else
      actionButton("btn_step2","▶ Run Step 2", class="run-btn", style="width:100%;font-size:11px;padding:10px;")
  })
  
  output$btn_step3_ui <- renderUI({
    # Step 3 requires Step 2 done — form data must exist
    has_form <- !is.null(rv$data_frame) && "H1" %in% names(rv$data_frame) && !all(is.na(rv$data_frame$H1))
    has_h2h  <- has_form && "H2H1" %in% names(rv$data_frame)
    if (rv$status=="running" && rv$current_step==3)
      tags$button("⏳ Running...", class="run-btn", disabled=TRUE, style="width:100%;font-size:11px;padding:10px;")
    else if (!has_form)
      tags$button("▶ Run Step 3", class="run-btn", disabled=TRUE, style="width:100%;font-size:11px;padding:10px;opacity:0.4;")
    else if (has_h2h)
      actionButton("btn_step3","▶ Re-run Step 3", class="run-btn", style="width:100%;font-size:11px;padding:10px;")
    else
      actionButton("btn_step3","▶ Run Step 3", class="run-btn", style="width:100%;font-size:11px;padding:10px;")
  })
  
  output$btn_step4_ui <- renderUI({
    # Step 4 requires Step 3 done — H2H must exist
    has_h2h <- !is.null(rv$data_frame) && "H2H1" %in% names(rv$data_frame)
    has_results <- !is.null(rv$results)
    if (rv$status=="running" && rv$current_step==4)
      tags$button("⏳ Running...", class="run-btn", disabled=TRUE, style="width:100%;font-size:11px;padding:10px;")
    else if (!has_h2h)
      tags$button("▶ Run Step 4", class="run-btn", disabled=TRUE, style="width:100%;font-size:11px;padding:10px;opacity:0.4;")
    else if (has_results)
      actionButton("btn_step4","▶ Re-run Step 4", class="run-btn", style="width:100%;font-size:11px;padding:10px;")
    else
      actionButton("btn_step4","▶ Run Step 4", class="run-btn", style="width:100%;font-size:11px;padding:10px;")
  })
  
  # ── Step badges ──
  output$step1_badge <- renderUI({
    if (!is.null(rv$data_frame) && nrow(rv$data_frame)>0)
      div(style="font-size:10px;letter-spacing:1px;padding:4px 10px;border-radius:4px;background:rgba(0,229,160,0.1);border:1px solid rgba(0,229,160,0.3);color:#00e5a0;",
          paste0("✓ Fixtures — ", nrow(rv$data_frame), " loaded"))
  })
  output$step2_badge <- renderUI({
    if (!is.null(rv$data_frame) && "H1" %in% names(rv$data_frame) && !all(is.na(rv$data_frame$H1)))
      div(style="font-size:10px;letter-spacing:1px;padding:4px 10px;border-radius:4px;background:rgba(0,229,160,0.1);border:1px solid rgba(0,229,160,0.3);color:#00e5a0;", "✓ Form loaded")
  })
  output$step3_badge <- renderUI({
    if (!is.null(rv$data_frame) && "H2H1" %in% names(rv$data_frame))
      div(style="font-size:10px;letter-spacing:1px;padding:4px 10px;border-radius:4px;background:rgba(0,229,160,0.1);border:1px solid rgba(0,229,160,0.3);color:#00e5a0;", "✓ H2H loaded")
  })
  output$step4_badge <- renderUI({
    if (!is.null(rv$results))
      div(style="font-size:10px;letter-spacing:1px;padding:4px 10px;border-radius:4px;background:rgba(0,229,160,0.1);border:1px solid rgba(0,229,160,0.3);color:#00e5a0;", "✓ Analysis done")
  })
  
  output$pipeline_log <- renderText({ rv$log })
  
  # ── Load Last Session button ──
  output$load_last_session_ui <- renderUI({
    backup_path <- "C:/Users/Ogbuta/OneDrive/New Projects/data_frame_backup.rds"
    if (file.exists(backup_path)) {
      info <- tryCatch({
        d <- readRDS(backup_path)
        paste0("📂 Load Last Session (", nrow(d), " fixtures)")
      }, error = function(e) "📂 Load Last Session")
      actionButton("load_last_session", info, class = "save-btn")
    }
  })
  
  observeEvent(input$load_last_session, {
    backup_path <- "C:/Users/Ogbuta/OneDrive/New Projects/data_frame_backup.rds"
    tryCatch({
      d <- readRDS(backup_path)
      rv$data_frame <- d
      steps_done <- c()
      if ("H1"   %in% names(d) && !all(is.na(d$H1)))   steps_done <- c(steps_done, "Form")
      if ("H2H1" %in% names(d))                         steps_done <- c(steps_done, "H2H")
      msg <- paste0("[ RESTORED ] ", nrow(d), " fixtures loaded from last session.")
      if (length(steps_done) > 0) msg <- paste0(msg, " Steps available: ", paste(steps_done, collapse=", "), ".")
      add_log(msg)
      showNotification(paste0(nrow(d), " fixtures restored"), type="message", duration=3)
    }, error = function(e) {
      add_log(paste0("[ RESTORE ERROR ] ", e$message))
      showNotification("Could not load backup", type="error", duration=3)
    })
  })
  
  # ══ STEP 1 ══
  observeEvent(input$btn_step1, {
    add_log("[ STEP 1 ] Starting fixtures & standings...")
    run_step_async(1,
                   step_fn = function() { start_selenium(); r <- get_fixtures(); stop_selenium(); r },
                   on_done = function(result) {
                     rv$current_step <- 0
                     if (!is.null(result$.__error__)) { add_log(paste0("[ STEP 1 FAILED ] ",result$.__error__)); rv$status<-"error" }
                     else {
                       rv$data_frame <- result
                       rv$status     <- "done"
                       tryCatch(saveRDS(result, "C:/Users/Ogbuta/OneDrive/New Projects/data_frame_backup.rds"), error=function(e) invisible())
                       add_log(paste0("[ STEP 1 DONE ] ", nrow(result), " fixtures loaded"))
                     }
                   },
                   on_error = function(e) { add_log(paste0("[ STEP 1 ERROR ] ",e$message)); rv$status<-"error"; rv$current_step<-0 }
    )
  })
  
  # ══ STEP 2 ══
  observeEvent(input$btn_step2, {
    req(!is.null(rv$data_frame), nrow(rv$data_frame)>0)
    add_log("[ STEP 2 ] Starting team form scraping...")
    df_snap <- rv$data_frame
    run_step_async(2,
                   step_fn = function() { start_selenium(); r <- update_form(df_snap); stop_selenium(); r },
                   on_done = function(result) {
                     rv$current_step <- 0
                     if (!is.null(result$.__error__)) { add_log(paste0("[ STEP 2 FAILED ] ",result$.__error__)); rv$status<-"error" }
                     else {
                       rv$data_frame <- result
                       rv$status     <- "done"
                       tryCatch(saveRDS(result, "C:/Users/Ogbuta/OneDrive/New Projects/data_frame_backup.rds"), error=function(e) invisible())
                       add_log("[ STEP 2 DONE ] Team form loaded")
                     }
                   },
                   on_error = function(e) { add_log(paste0("[ STEP 2 ERROR ] ",e$message)); rv$status<-"error"; rv$current_step<-0 }
    )
  })
  
  # ══ STEP 3 ══
  observeEvent(input$btn_step3, {
    req(!is.null(rv$data_frame), nrow(rv$data_frame)>0)
    add_log("[ STEP 3 ] Starting H2H scraping...")
    df_snap <- rv$data_frame; tl_urls <- rv$tl_urls
    run_step_async(3,
                   step_fn = function() {
                     # Source globally so SELENIUM, selenium_get_html etc are all in same global env
                     source(PIPELINE_PATH, local=FALSE)
                     start_selenium()
                     # Override tablesleague_config AFTER sourcing scripts
                     if (!is.null(tl_urls) && nrow(tl_urls) > 0) tablesleague_config <<- tl_urls
                     r <- update_h2h(df_snap)
                     stop_selenium()
                     r
                   },
                   on_done = function(result) {
                     rv$current_step <- 0
                     if (!is.null(result$.__error__)) { add_log(paste0("[ STEP 3 FAILED ] ",result$.__error__)); rv$status<-"error" }
                     else {
                       cleaned       <- clean_data_frame(result$data_frame)
                       rv$data_frame <- cleaned
                       rv$slug_verif <- result$slug_verification
                       rv$status     <- "done"
                       tryCatch({
                         saveRDS(cleaned, "C:/Users/Ogbuta/OneDrive/New Projects/data_frame_backup.rds")
                         add_log("[ STEP 3 DONE ] H2H loaded, cleaned, backup saved.")
                       }, error=function(e) add_log("[ STEP 3 DONE ] H2H loaded and cleaned."))
                     }
                   },
                   on_error = function(e) { add_log(paste0("[ STEP 3 ERROR ] ",e$message)); rv$status<-"error"; rv$current_step<-0 }
    )
  })
  
  # ══ STEP 4 ══
  observeEvent(input$btn_step4, {
    req(!is.null(rv$data_frame), nrow(rv$data_frame)>0)
    add_log("[ STEP 4 ] Running Poisson analysis...")
    df_snap <- rv$data_frame
    tf<-input$thresh_fh; to15<-input$thresh_o15; to25<-input$thresh_o25
    tb<-input$thresh_btts; tdc<-input$thresh_dc
    run_step_async(4,
                   step_fn = function() {
                     adf <- preprocess(df_snap)
                     adf <- calculate_probabilities(adf)
                     # Override THRESHOLDS in the environment where market functions were defined
                     # by passing custom thresholds directly to run_analysis
                     results <- run_analysis(adf,
                                             thresh_fh   = tf,
                                             thresh_o15  = to15,
                                             thresh_o25  = to25,
                                             thresh_btts = tb,
                                             thresh_dc   = tdc
                     )
                     list(adf=adf, results=results)
                   },
                   on_done = function(result) {
                     rv$current_step <- 0
                     if (!is.null(result$.__error__)) { add_log(paste0("[ STEP 4 FAILED ] ",result$.__error__)); rv$status<-"error" }
                     else {
                       rv$analysis_df<-result$adf; rv$results<-result$results
                       rv$status<-"done"; rv$last_run<-Sys.time()
                       r <- result$results
                       add_log(paste0("[ STEP 4 DONE ] FH:",nrow(r$fh_over05)," O1.5:",nrow(r$over15),
                                      " O2.5:",nrow(r$over25)," BTTS:",nrow(r$btts)," DC:",nrow(r$double_chance)))
                     }
                   },
                   on_error = function(e) { add_log(paste0("[ STEP 4 ERROR ] ",e$message)); rv$status<-"error"; rv$current_step<-0 }
    )
  })
  
  # ── Market tables ──
  render_market_table <- function(data_reactive, id) {
    output[[id]] <- renderDT({
      d <- data_reactive()
      if (is.null(d) || nrow(d)==0) return(NULL)
      d$confidence <- ifelse(
        as.numeric(gsub("%","",d$confidence)) >= 90,
        paste0('<span class="conf-high">',d$confidence,'</span>'),
        ifelse(as.numeric(gsub("%","",d$confidence)) >= 75,
               paste0('<span class="conf-med">',d$confidence,'</span>'),
               paste0('<span class="conf-low">',d$confidence,'</span>')))
      d$league <- paste0('<span class="league-badge">',d$league,'</span>')
      datatable(d, escape=FALSE, rownames=FALSE, selection="none",
                options=list(pageLength=20, dom='ftp', ordering=TRUE, scrollX=FALSE,
                             columnDefs=list(list(className='dt-center',
                                                  targets=which(names(d) %in% c("home_position","away_position","confidence"))-1))))
    }, server=FALSE)
  }
  
  render_market_table(reactive(rv$results$fh_over05),    "tbl_fh")
  render_market_table(reactive(rv$results$over15),        "tbl_o15")
  render_market_table(reactive(rv$results$over25),        "tbl_o25")
  render_market_table(reactive(rv$results$btts),          "tbl_btts")
  render_market_table(reactive(rv$results$double_chance), "tbl_dc")
  
  no_data_msg <- function(market) div(class="no-data", div(class="icon","◌"),
                                      div(paste0("No ",market," picks yet")),
                                      div(style="margin-top:6px;font-size:10px;","Run the pipeline to generate predictions"))
  
  output$fh_ui   <- renderUI({ if(is.null(rv$results)) no_data_msg("FH Over 0.5") else DTOutput("tbl_fh") })
  output$o15_ui  <- renderUI({ if(is.null(rv$results)) no_data_msg("Over 1.5")    else DTOutput("tbl_o15") })
  output$o25_ui  <- renderUI({ if(is.null(rv$results)) no_data_msg("Over 2.5")    else DTOutput("tbl_o25") })
  output$btts_ui <- renderUI({ if(is.null(rv$results)) no_data_msg("BTTS")        else DTOutput("tbl_btts") })
  output$dc_ui   <- renderUI({ if(is.null(rv$results)) no_data_msg("Double Chance") else DTOutput("tbl_dc") })
  
  # ── Export predictions to Excel ──
  observeEvent(input$export_predictions, {
    req(!is.null(rv$results))
    tryCatch({
      path <- paste0("C:/Users/Ogbuta/OneDrive/New Projects/GoalEdge_Predictions_",
                     format(Sys.time(),"%Y%m%d_%H%M"),".xlsx")
      export_predictions_excel(rv$results, path)
      add_log(paste0("[ EXCEL ] Predictions saved: ", basename(path)))
      showNotification(paste0("Saved: ", basename(path)), type="message", duration=4)
    }, error=function(e) {
      add_log(paste0("[ EXCEL ERROR ] ",e$message))
      showNotification(paste0("Error: ",e$message), type="error", duration=5)
    })
  })
  
  # ── Data tab ──
  output$data_summary_pills <- renderUI({
    d <- rv$data_frame
    if (is.null(d)||nrow(d)==0) return(NULL)
    div(style="display:flex;gap:10px;flex-wrap:wrap;",
        div(style="font-size:10px;letter-spacing:1px;padding:4px 12px;border-radius:4px;background:rgba(0,229,160,0.1);border:1px solid rgba(0,229,160,0.3);color:#00e5a0;", paste0(nrow(d)," rows")),
        div(style="font-size:10px;letter-spacing:1px;padding:4px 12px;border-radius:4px;background:rgba(0,145,255,0.1);border:1px solid rgba(0,145,255,0.3);color:#0091ff;", paste0(ncol(d)," columns")),
        div(style="font-size:10px;letter-spacing:1px;padding:4px 12px;border-radius:4px;background:rgba(245,200,66,0.1);border:1px solid rgba(245,200,66,0.3);color:#f5c842;", paste0(n_distinct(d$league)," leagues")),
        div(style="font-size:10px;letter-spacing:1px;padding:4px 12px;border-radius:4px;background:rgba(255,77,109,0.1);border:1px solid rgba(255,77,109,0.3);color:#ff4d6d;", paste0(sum(is.na(d))," NAs"))
    )
  })
  
  output$col_selector_ui <- renderUI({
    d <- rv$data_frame
    if (is.null(d)) return(NULL)
    key_cols <- c("league","home","away","home_position","away_position",
                  "H1","H2","H3","H4","H5","A1","A2","A3","A4","A5",
                  "H2H1","H2H2","H2H3","H2H4","H2H5")
    checkboxGroupInput("data_cols","Show columns:", choices=names(d),
                       selected=intersect(key_cols,names(d)), inline=TRUE)
  })
  
  output$data_table <- renderDT({
    d <- rv$data_frame
    if (is.null(d)||nrow(d)==0) return(NULL)
    cols <- input$data_cols
    if (is.null(cols)||length(cols)==0) cols <- names(d)
    datatable(d[,intersect(cols,names(d)),drop=FALSE], rownames=FALSE, selection="none",
              options=list(pageLength=25, dom="ftp", scrollX=TRUE, ordering=TRUE))
  }, server=TRUE)
  
  observeEvent(input$save_data, {
    req(!is.null(rv$data_frame))
    tryCatch({
      p <- "C:/Users/Ogbuta/OneDrive/New Projects/data_frame_backup.rds"
      saveRDS(rv$data_frame, p)
      add_log("[ SAVED ] data_frame_backup.rds")
      showNotification("RDS backup saved", type="message", duration=3)
    }, error=function(e) showNotification(paste0("Save failed: ",e$message), type="error"))
  })
  
  observeEvent(input$export_data_csv, {
    req(!is.null(rv$data_frame))
    tryCatch({
      p <- paste0("C:/Users/Ogbuta/OneDrive/New Projects/GoalEdge_Data_",
                  format(Sys.time(),"%Y%m%d_%H%M"),".xlsx")
      wb  <- createWorkbook()
      addWorksheet(wb, "Data", tabColour="#DA291C", gridLines=FALSE)
      hs  <- createStyle(fontName="Calibri", fontSize=11, fontColour="#FFFFFF",
                         fgFill="#DA291C", halign="CENTER", valign="CENTER",
                         textDecoration="BOLD", border="Bottom", borderColour="#C62828")
      body_s <- createStyle(fontName="Calibri", fontSize=10, fontColour="#111111",
                            valign="CENTER", border="Bottom", borderColour="#EEEEEE")
      alt_s  <- createStyle(fontName="Calibri", fontSize=10, fontColour="#111111",
                            fgFill="#FCE4EC", valign="CENTER",
                            border="Bottom", borderColour="#EEEEEE")
      d <- rv$data_frame
      hdrs <- toupper(gsub("_"," ", names(d)))
      writeData(wb, "Data", as.data.frame(t(hdrs)), startRow=1, startCol=1, colNames=FALSE)
      addStyle(wb, "Data", hs, rows=1, cols=1:ncol(d), gridExpand=TRUE)
      setRowHeights(wb, "Data", rows=1, heights=30)
      writeData(wb, "Data", d, startRow=2, startCol=1, colNames=FALSE)
      setRowHeights(wb, "Data", rows=2:(nrow(d)+1), heights=20)
      for (r in 1:nrow(d))
        addStyle(wb, "Data", if(r%%2==0) alt_s else body_s, rows=r+1, cols=1:ncol(d), gridExpand=TRUE)
      widths <- sapply(names(d), function(cn) min(max(nchar(cn)+3, 10), 35))
      setColWidths(wb, "Data", cols=1:ncol(d), widths=widths)
      freezePane(wb, "Data", firstRow=TRUE)
      addFilter(wb, "Data", rows=1, cols=1:ncol(d))
      saveWorkbook(wb, p, overwrite=TRUE)
      add_log(paste0("[ DATA EXCEL ] Saved: ", basename(p)))
      showNotification(paste0("Data saved: ", basename(p)), type="message", duration=4)
    }, error=function(e) showNotification(paste0("Export error: ",e$message), type="error"))
  })
  
  # ── Slug table ──
  output$slug_table <- renderDT({
    d <- rv$slug_verif
    if (is.null(d) || nrow(d)==0) {
      return(datatable(
        data.frame(Message="No slug data yet — run Step 3 first"),
        rownames=FALSE, colnames=c("Status"),
        options=list(dom="t", ordering=FALSE)
      ))
    }
    d$sim_display <- sapply(d$sim, function(s) {
      if (is.na(s)) return('<span class="sim-low">NA</span>')
      s <- as.numeric(s)
      if (s>=0.85) paste0('<span class="sim-high">',round(s,3),'</span>')
      else if (s>=0.70) paste0('<span class="sim-med">',round(s,3),'</span>')
      else paste0('<span class="sim-low">',round(s,3),'</span>')
    })
    d$slug_display <- ifelse(is.na(d$tl_slug),
                             '<span style="color:var(--accent3)">NA</span>',
                             paste0('<code style="color:var(--accent2);font-size:11px;">',d$tl_slug,'</code>'))
    out <- d %>% select(league, team, tl_name, slug_display, sim_display)
    names(out) <- c("League","WF Team","TL Team","TL Slug","Sim")
    datatable(out, escape=FALSE, rownames=FALSE, selection="none",
              options=list(pageLength=30, dom='ftp', ordering=TRUE))
  }, server=FALSE)
  
  # ── Export slug to Excel ──
  observeEvent(input$export_slug, {
    req(!is.null(rv$slug_verif) && nrow(rv$slug_verif)>0)
    tryCatch({
      path <- paste0("C:/Users/Ogbuta/OneDrive/New Projects/GoalEdge_SlugCheck_",
                     format(Sys.time(),"%Y%m%d_%H%M"),".xlsx")
      export_slug_excel(rv$slug_verif, path)
      add_log(paste0("[ SLUG EXCEL ] Saved: ", basename(path)))
      showNotification(paste0("Slug check saved: ", basename(path)), type="message", duration=4)
    }, error=function(e) {
      add_log(paste0("[ SLUG EXCEL ERROR ] ",e$message))
      showNotification(paste0("Error: ",e$message), type="error", duration=5)
    })
  })
  
  # ── URL editor ──
  output$url_editor <- renderDT({
    d <- rv$tl_urls
    if (is.null(d)) return(NULL)
    datatable(d, editable=list(target="cell", disable=list(columns=0)),
              rownames=FALSE, selection="none",
              options=list(pageLength=35, dom='ft', scrollY="400px"))
  }, server=FALSE)
  
  observeEvent(input$url_editor_cell_edit, {
    info <- input$url_editor_cell_edit
    rv$tl_urls[info$row, info$col+1] <- info$value
    add_log(paste0("[ SETTINGS ] URL updated: ", rv$tl_urls$name[info$row]))
  })
  
  # ── Save thresholds ──
  observeEvent(input$save_thresholds, {
    s <- list(thresh_fh=input$thresh_fh, thresh_o15=input$thresh_o15,
              thresh_o25=input$thresh_o25, thresh_btts=input$thresh_btts,
              thresh_dc=input$thresh_dc, tl_urls=rv$tl_urls)
    save_settings(s)
    add_log("[ SETTINGS ] Thresholds saved to disk")
    showNotification("Thresholds saved", type="message", duration=3)
  })
  
  # ── Save URLs ──
  observeEvent(input$save_urls, {
    s <- list(thresh_fh=input$thresh_fh, thresh_o15=input$thresh_o15,
              thresh_o25=input$thresh_o25, thresh_btts=input$thresh_btts,
              thresh_dc=input$thresh_dc, tl_urls=rv$tl_urls)
    save_settings(s)
    add_log("[ SETTINGS ] URLs saved to disk")
    showNotification("URLs saved", type="message", duration=3)
  })
  
  # ============================================================
  # SLIP VALIDATOR SERVER
  # ============================================================
  
  val_rv <- reactiveValues(
    slip_text = NULL,
    results   = NULL,
    log       = "Ready. Upload predictions Excel and slip image.
"
  )
  
  val_log <- function(msg) {
    ts <- format(Sys.time(), "%H:%M:%S")
    val_rv$log <- paste0(val_rv$log, "[", ts, "] ", msg, "
")
  }
  
  # Load Excel — just read sheet names to confirm
  observeEvent(input$val_excel, {
    req(input$val_excel)
    tryCatch({
      sheets <- getSheetNames(input$val_excel$datapath)
      val_log(paste0("Excel loaded. Sheets: ", paste(sheets, collapse=", ")))
    }, error=function(e) val_log(paste0("Excel error: ", e$message)))
  })
  
  output$val_excel_status <- renderUI({
    req(input$val_excel)
    tryCatch({
      sheets <- getSheetNames(input$val_excel$datapath)
      div(style="font-size:11px; color:#00e5a0; margin-top:6px;",
          paste0("OK ", length(sheets), " sheets: ", paste(sheets, collapse=", ")))
    }, error=function(e) NULL)
  })
  
  # OCR slip image
  observeEvent(input$val_slip_img, {
    req(input$val_slip_img)
    val_log(paste0("Running OCR on: ", basename(input$val_slip_img$name)))
    showNotification("Running OCR — this may take a few seconds...",
                     type = "message", duration = 4, id = "ocr_running")
    
    text <- tryCatch(
      ocr_slip(input$val_slip_img$datapath),
      error = function(e) {
        val_log(paste0("OCR error: ", e$message))
        showNotification(paste0("OCR error: ", e$message),
                         type = "error", duration = 8)
        ""
      }
    )
    
    removeNotification(id = "ocr_running")
    val_rv$slip_text <- text
    
    if (nchar(text) == 0) {
      val_log("OCR produced no output.")
      return()
    }
    
    n <- length(grep(" vs ", strsplit(text, "\n")[[1]]))
    val_log(paste0("OCR complete. ~", n, " fixtures detected."))
    showNotification(paste0("OCR complete: ", n, " fixtures detected"),
                     type = "message", duration = 3)
  })
  
  output$val_ocr_status <- renderUI({
    req(val_rv$slip_text)
    n <- length(grep(" vs ", strsplit(val_rv$slip_text, "
")[[1]]))
    div(style="font-size:11px; color:#00e5a0; margin-top:6px;",
        paste0("OK OCR complete - ", n, " fixtures detected"))
  })
  
  output$val_ocr_preview <- renderUI({
    req(val_rv$slip_text)
    div(class="card",
        div(class="card-title", "OCR Preview — Raw Extracted Text"),
        div(style="background:#0a0a0a; border-radius:6px; padding:12px; font-family:var(--font-mono);
                 font-size:10px; color:#7d90b0; max-height:180px; overflow-y:auto; white-space:pre-wrap;
                 line-height:1.7;",
            val_rv$slip_text)
    )
  })
  
  output$val_log <- renderText({ val_rv$log })
  
  # Validate
  observeEvent(input$val_run, {
    req(input$val_excel, val_rv$slip_text, nchar(val_rv$slip_text) > 0)
    val_log(paste0("Validating against: ", input$val_market))
    
    preds <- tryCatch(
      read.xlsx(input$val_excel$datapath, sheet=input$val_market),
      error=function(e) { val_log(paste0("Sheet error: ", e$message)); NULL }
    )
    if (is.null(preds) || nrow(preds)==0) {
      val_log("No predictions found in selected sheet."); return()
    }
    val_log(paste0("Loaded ", nrow(preds), " predictions."))
    
    slip_df <- parse_slip(val_rv$slip_text, input$val_market)
    if (nrow(slip_df)==0) {
      val_log("Could not parse fixtures from slip. Check OCR preview."); return()
    }
    val_log(paste0("Parsed ", nrow(slip_df), " fixtures from slip."))
    
    results <- validate_slip(slip_df, preds, input$val_market)
    val_rv$results <- results
    
    correct   <- sum(grepl("FOUND OK",      results$status, fixed=TRUE))
    wrong_opt <- sum(grepl("WRONG OPTION",  results$status, fixed=TRUE))
    notfound  <- sum(grepl("NOT FOUND",     results$status, fixed=TRUE))
    val_log(paste0("Done: ", correct, " correct, ", wrong_opt, " wrong option, ", notfound, " not found."))
  })
  
  # Results UI
  output$val_results <- renderUI({
    req(val_rv$results)
    r         <- val_rv$results
    correct   <- sum(grepl("FOUND OK",     r$status, fixed=TRUE))
    wrong_opt <- sum(grepl("WRONG OPTION", r$status, fixed=TRUE))
    notfound  <- sum(grepl("NOT FOUND",    r$status, fixed=TRUE))
    total     <- nrow(r)
    
    tagList(
      div(class="stats-row",
          div(class="stat-pill",       div(class="val", total),     div(class="lbl","Total")),
          div(class="stat-pill",       div(class="val", correct),   div(class="lbl","Correct"),
              style="border-color:rgba(0,229,160,0.4);"),
          div(class="stat-pill",       div(class="val", wrong_opt), div(class="lbl","Wrong Option"),
              style="border-color:rgba(245,200,66,0.4);"),
          div(class="stat-pill",       div(class="val", notfound),  div(class="lbl","Not Found"),
              style="border-color:rgba(255,77,109,0.4);")
      ),
      div(class="card",
          div(class="card-title", "Validation Results"),
          DTOutput("val_table")
      )
    )
  })
  
  output$val_table <- renderDT({
    req(val_rv$results)
    d <- val_rv$results
    
    d$status <- sapply(d$status, function(s) {
      if (grepl("FOUND OK", s, fixed=TRUE))
        '<span style="background:#0a2a0a;color:#00e5a0;padding:2px 8px;border-radius:4px;font-size:11px;font-weight:700;">OK FOUND</span>'
      else if (grepl("WRONG OPTION", s, fixed=TRUE))
        '<span style="background:#2a2000;color:#f5c842;padding:2px 8px;border-radius:4px;font-size:11px;font-weight:700;">WRONG OPTION</span>'
      else
        '<span style="background:#2a0000;color:#ff4d6d;padding:2px 8px;border-radius:4px;font-size:11px;font-weight:700;">NOT FOUND</span>'
    })
    
    d$option_check <- sapply(d$option_check, function(o) {
      if (is.na(o)) return("-")
      if (o == "CORRECT") '<span style="color:#00e5a0;font-weight:600;">Correct</span>'
      else if (o == "NOT CHECKED") '<span style="color:#3d5275;">-</span>'
      else paste0('<span style="color:#ff4d6d;font-size:11px;">', o, '</span>')
    })
    
    d$sim_score <- sapply(d$sim_score, function(s) {
      if (is.na(s)) return("-")
      s <- as.numeric(s)
      col <- if(s>=0.85) "#00e5a0" else if(s>=0.70) "#f5c842" else "#ff4d6d"
      paste0('<span style="color:', col, ';font-weight:600;">', s, '</span>')
    })
    
    # Rename columns
    col_map <- c(slip_home="Slip Home", slip_away="Slip Away", selection="My Selection",
                 stronger="Stronger Team", status="Status", option_check="Option Check",
                 pred_home="Pred Home", pred_away="Pred Away",
                 confidence="Confidence", sim_score="Sim Score")
    present <- names(d)[names(d) %in% names(col_map)]
    names(d)[names(d) %in% names(col_map)] <- col_map[present]
    
    datatable(d, escape=FALSE, rownames=FALSE, selection="none",
              options=list(pageLength=50, dom="ftp", ordering=TRUE, scrollX=TRUE))
  }, server=FALSE)
  
  
}

shinyApp(ui=ui, server=server)
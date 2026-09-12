# ============================================================
# moab_1xbet_auto_bettor.R
# Reads predictions Excel, uses sporty_mapper.rds for FS↔1xBet
# league mapping, matches games via 1xBet games1x2 endpoint,
# books slips via SaveCoupon endpoint. Chunks into slips of 50,
# drops any remainder < 15. Emails codes at the end.
# ============================================================

library(shiny)
library(DT)
library(readxl)
library(httr)
library(jsonlite)
library(stringdist)
library(future)
library(future.apply)

BASE_PATH    <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
MAPPER_PATH  <- file.path(BASE_PATH, "sporty_mapper.rds")
XBET_HLP     <- file.path(BASE_PATH, "onexbet_helpers.R")
if (file.exists(XBET_HLP)) source(XBET_HLP)

# Hardcoded email creds — same as SB bettor
EMAIL_FROM      <- "chukwudiogbuta@gmail.com"
EMAIL_TO        <- "chukwudiogbuta@gmail.com"
EMAIL_APP_PASS  <- "jkhryasdthhxiuts"
EMAIL_SMTP_HOST <- "smtp.gmail.com"
EMAIL_SMTP_PORT <- 587

`%||%` <- function(a, b) {
  if (is.null(a) || length(a) == 0) return(b)
  if (length(a) == 1 && is.na(a)) return(b)
  a
}

# ── 1xBet endpoints ──────────────────────────────────────────
ONEXBET_BASE   <- "https://1xlite-12947.pro"
ONEXBET_GAMES  <- paste0(ONEXBET_BASE, "/service-api/main-line-feed/v3/games1x2")
ONEXBET_GAME_EVENTS <- paste0(ONEXBET_BASE, "/service-api/main-line-feed/v3/gameEvents")
ONEXBET_SLIP   <- paste0(ONEXBET_BASE, "/service-api/LiveBet/Open/SaveCoupon")

onexbet_headers_full <- function(xhd = NULL, league_ref = NULL,
                                   session_cookie = NULL) {
  ref <- if (!is.null(league_ref)) league_ref
         else paste0(ONEXBET_BASE, "/en/line/football")
  hdrs <- list(
    `accept`              = "application/json, text/plain, */*",
    `accept-language`     = "en-US,en;q=0.9",
    `content-type`        = "application/json",
    `x-app-n`             = "__BETTING_APP__",
    `x-svc-source`        = "__BETTING_APP__",
    `x-mobile-project-id` = "0",
    `is-srv`              = "false",
    `x-requested-with`    = "XMLHttpRequest",
    `Referer`             = ref,
    `User-Agent`          = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/152.0.0.0 Safari/537.36"
  )
  if (!is.null(xhd) && nzchar(xhd)) hdrs[["x-hd"]] <- xhd
  if (!is.null(session_cookie) && nzchar(session_cookie)) {
    hdrs[["Cookie"]] <- paste0(
      "auid=uaJaGmqTNGyDa7b5BC9uAg==; lng=en; ",
      "SESSION=", session_cookie, "; ",
      "platform_type=desktop; tzo=3; is-srv=false")
  }
  do.call(add_headers, hdrs)
}

# ── Grab x-hd token via visible Chrome — direct chromedriver, same pattern
# ── as SB bettor (no RSelenium). Enables perf log to read Network events.
CHROME_PATH_XBET       <- "C:/Program Files/Google/Chrome/Application/chrome.exe"
CHROMEDRIVER_PATH_XBET <- "C:/Users/Ogbuta/Downloads/chromedriver-win64/chromedriver.exe"

grab_xhd_token <- function(log_fn = message) {
  log_fn("Launching Chrome to grab x-hd token...")
  if (!file.exists(CHROMEDRIVER_PATH_XBET)) {
    log_fn(paste0("chromedriver not found at ", CHROMEDRIVER_PATH_XBET))
    return(list(token = NULL, session = NULL, chrome_handle = NULL))
  }
  system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
         ignore.stderr = TRUE, wait = TRUE)
  Sys.sleep(0.5)

  port <- 9998L
  system2(CHROMEDRIVER_PATH_XBET, args = paste0("--port=", port),
          wait = FALSE)
  Sys.sleep(2)

  session_body <- list(capabilities = list(alwaysMatch = list(
    browserName = "chrome",
    `goog:chromeOptions` = list(
      binary = CHROME_PATH_XBET,
      args = list("--no-sandbox", "--disable-dev-shm-usage",
                   "--disable-blink-features=AutomationControlled",
                   "--disable-extensions", "--window-size=1200,800"),
      perfLoggingPrefs = list(enableNetwork = TRUE)),
    `goog:loggingPrefs` = list(performance = "ALL", browser = "ALL")
  )))
  resp <- tryCatch(
    POST(paste0("http://localhost:", port, "/session"),
          body = session_body, encode = "json", timeout(60)),
    error = function(e) NULL)
  if (is.null(resp) || status_code(resp) >= 400) {
    log_fn("ChromeDriver failed to start")
    system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
            ignore.stderr = TRUE, wait = TRUE)
    return(list(token = NULL, session = NULL, chrome_handle = NULL))
  }
  sess <- content(resp, "parsed", "application/json")$value$sessionId
  base <- paste0("http://localhost:", port, "/session/", sess)

  token <- NULL
  session_cookie <- NULL
  tryCatch({
    log_fn("Loading 1xBet homepage...")
    POST(paste0(base, "/url"),
          body = list(url = paste0(ONEXBET_BASE, "/en/line/football")),
          encode = "json", timeout(60))
    Sys.sleep(4)
    log_fn("Loading league page to trigger x-hd...")
    POST(paste0(base, "/url"),
          body = list(url = paste0(ONEXBET_BASE, "/en/line/football/127733-spain-la-liga")),
          encode = "json", timeout(60))
    Sys.sleep(6)

    cook_resp <- GET(paste0(base, "/cookie"), timeout(15))
    cookies <- content(cook_resp, "parsed", "application/json")$value
    for (ck in cookies) {
      if (identical(ck$name, "SESSION")) {
        session_cookie <- ck$value; break
      }
    }
    if (!is.null(session_cookie))
      log_fn(paste0("Got SESSION cookie (", nchar(session_cookie), " chars)"))

    log_fn("Reading network log for x-hd...")
    log_resp <- POST(paste0(base, "/se/log"),
                      body = list(type = "performance"),
                      encode = "json", timeout(30))
    if (status_code(log_resp) >= 400) {
      log_resp <- POST(paste0(base, "/log"),
                        body = list(type = "performance"),
                        encode = "json", timeout(30))
    }
    logs <- content(log_resp, "parsed", "application/json")$value
    for (entry in logs) {
      msg <- tryCatch(fromJSON(entry$message, simplifyVector = FALSE)$message,
                       error = function(e) NULL)
      if (is.null(msg)) next
      if (isTRUE(msg$method == "Network.requestWillBeSent")) {
        h <- msg$params$request$headers
        if (!is.null(h[["x-hd"]])) { token <- h[["x-hd"]]; break }
        for (k in names(h)) {
          if (tolower(k) == "x-hd") { token <- h[[k]]; break }
        }
        if (!is.null(token)) break
      }
    }
    if (is.null(token)) log_fn("x-hd not found in network log")
    else log_fn(paste0("Grabbed x-hd (", nchar(token), " chars)"))
  }, error = function(e) log_fn(paste0("Token grab error: ", e$message)))

  # Chrome stays OPEN — return the handle so caller can close later
  list(token = token, session = session_cookie,
        chrome_handle = list(base = base, port = port))
}

# Close Chrome — call at end of run
close_chrome <- function(chrome_handle) {
  if (is.null(chrome_handle)) return(invisible())
  tryCatch(DELETE(chrome_handle$base, timeout(10)), error = function(e) NULL)
  system("taskkill /F /IM chromedriver.exe", ignore.stdout = TRUE,
          ignore.stderr = TRUE, wait = TRUE)
}

# ── Market → 1xBet type + param resolver ─────────────────────
# Locked to 9 market strings for 1xBet.
MARKET_MAP_XBET <- list(
  "1X2 Home"          = list(type = 1,   param = 0),
  "1X2 Away"          = list(type = 3,   param = 0),
  "FT Draw"           = list(type = 2,   param = 0),
  "Double Chance 1X"  = list(type = 4,   param = 0),
  "Double Chance 12"  = list(type = 5,   param = 0),
  "Double Chance X2"  = list(type = 6,   param = 0),
  "BTTS"              = list(type = 180, param = 0),
  "Over 1.5"          = list(type = 9,   param = 1.5),
  "Over 2.5"          = list(type = 9,   param = 2.5)
)

# ── Fetch games for a given 1xBet league ─────────────────────
fetch_league_games <- function(league_id, xhd = NULL,
                                 country_name = NULL, league_name = NULL,
                                 session_cookie = NULL) {
  slug_fn <- function(s) {
    if (is.null(s) || is.na(s)) return("")
    s <- tolower(s)
    s <- gsub("\\.", "", s)
    s <- gsub("[^a-z0-9]+", "-", s)
    gsub("^-+|-+$", "", s)
  }
  league_ref <- paste0(ONEXBET_BASE, "/en/line/football/", league_id,
                        "-", slug_fn(country_name), "-", slug_fn(league_name))
  url <- sprintf("%s?cfView=3&count=40&fcountry=71&gr=413&grMode=4&lng=en&ref=71&selectedMs=2.1.%d",
                 ONEXBET_GAMES, league_id)
  resp <- tryCatch(GET(url,
                        onexbet_headers_full(xhd, league_ref, session_cookie),
                        timeout(30)),
                    error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  payload <- tryCatch(fromJSON(content(resp, as = "text"),
                                simplifyVector = FALSE),
                       error = function(e) NULL)
  if (is.null(payload)) return(NULL)
  # Payload is a list of games (may be wrapped, may not). Normalize:
  if (is.null(names(payload)) || !("id" %in% names(payload))) {
    games <- payload
  } else {
    games <- list(payload)
  }
  # Extract flat game info list
  rows <- list()
  for (g in games) {
    if (is.null(g$id)) next
    rows[[length(rows) + 1]] <- list(
      game_id  = as.integer(g$id),
      home     = g$opponent1$fullName %||% NA_character_,
      away     = g$opponent2$fullName %||% NA_character_,
      start_ts = as.integer(g$startTs %||% NA_integer_),
      kind     = as.integer(g$kind %||% 3),
      raw      = g
    )
  }
  rows
}

# ── Fetch FULL markets for one specific game ─────────────────
# When a game returned by games1x2 lacks the target market (e.g. Over 1.5),
# call gameEvents for that game to get its complete markets list.
fetch_game_full_markets <- function(game_id, xhd = NULL,
                                      referer_url = NULL,
                                      session_cookie = NULL) {
  url <- sprintf("%s?cfView=3&countEvents=250&fcountry=71&gameId=%s&gr=413&grMode=4&lng=en&marketType=1&ref=71",
                 ONEXBET_GAME_EVENTS, game_id)
  ref <- referer_url %||% paste0(ONEXBET_BASE, "/en/line/football")
  resp <- tryCatch(GET(url,
                        onexbet_headers_full(xhd, ref, session_cookie),
                        timeout(30)),
                    error = function(e) NULL)
  if (is.null(resp) || status_code(resp) != 200) return(NULL)
  tryCatch(fromJSON(content(resp, as = "text"), simplifyVector = FALSE),
            error = function(e) NULL)
}

# Find odds for (type, param) inside a gameEvents response
find_odds_in_full <- function(full_payload, type, param) {
  if (is.null(full_payload)) return(NA_real_)
  scan <- function(groups) {
    if (is.null(groups)) return(NA_real_)
    for (grp in groups) {
      if (is.null(grp$events)) next
      for (col in grp$events) {
        for (ev in col) {
          if (is.null(ev$type)) next
          if (ev$type == type) {
            p <- ev$parameter %||% 0
            if (isTRUE(all.equal(p, param))) return(as.numeric(ev$cf))
          }
        }
      }
    }
    NA_real_
  }
  candidates <- list(full_payload$eventGroups,
                      full_payload$centralBlockEventGroups,
                      full_payload$eventsByGroups)
  for (c in candidates) {
    if (!is.null(c)) {
      r <- scan(c)
      if (!is.na(r)) return(r)
    }
  }
  if (is.list(full_payload) && length(full_payload) > 0) {
    for (item in full_payload) {
      if (is.list(item) && !is.null(item$eventGroups)) {
        r <- scan(item$eventGroups)
        if (!is.na(r)) return(r)
      }
    }
  }
  NA_real_
}

# ── Find odds for a specific (type, param) in a game's events ─
find_odds <- function(game_raw, type, param) {
  # Check top-level eventGroups
  candidates <- list()
  scan_events <- function(event_groups) {
    if (is.null(event_groups)) return()
    for (grp in event_groups) {
      if (is.null(grp$events)) next
      for (col in grp$events) {
        for (ev in col) {
          if (is.null(ev$type)) next
          if (ev$type == type) {
            ev_param <- ev$parameter %||% 0
            if (isTRUE(all.equal(ev_param, param))) {
              candidates[[length(candidates) + 1]] <<- ev$cf
            }
          }
        }
      }
    }
  }
  scan_events(game_raw$eventGroups)
  scan_events(game_raw$centralBlockEventGroups)
  if (length(candidates) == 0) return(NA_real_)
  as.numeric(candidates[[1]])
}

# ── Book a single slip via SaveCoupon ────────────────────────
book_slip <- function(events, xhd = NULL, session_cookie = NULL) {
  # events: list of list(game_id, type, coef, param, kind)
  ev_json <- lapply(events, function(e) {
    list(GameId = e$game_id, Type = e$type, Coef = e$coef,
          Param = e$param, PV = NA, PlayerId = 0, Kind = e$kind,
          InstrumentId = 0, Seconds = 0, Price = 0, Expired = 0,
          PlayersDuel = list())
  })
  body <- list(
    notWait         = TRUE,
    CheckCf         = 1,
    partner         = 71,
    AntiExpressCoef = 1,
    Summ            = 100,
    Events          = ev_json,
    Vid             = 1
  )
  body_json <- toJSON(body, auto_unbox = TRUE, na = "null")
  resp <- tryCatch(POST(ONEXBET_SLIP,
                         onexbet_headers_full(xhd, NULL, session_cookie),
                         body = body_json, encode = "raw",
                         content_type("application/json"), timeout(30)),
                    error = function(e) NULL)
  if (is.null(resp)) return(list(ok = FALSE, code = NA_character_,
                                   error = "no response"))
  parsed <- tryCatch(fromJSON(content(resp, as = "text")),
                      error = function(e) NULL)
  if (is.null(parsed)) return(list(ok = FALSE, code = NA_character_,
                                     error = "parse failed"))
  if (isTRUE(parsed$Success)) {
    return(list(ok = TRUE, code = as.character(parsed$Value),
                 error = ""))
  }
  list(ok = FALSE, code = NA_character_,
        error = paste0(parsed$Error %||% "unknown",
                        " (code ", parsed$ErrorCode %||% "?", ")"))
}

# ── Send codes via email (hardcoded creds, namespace-safe) ───
build_email_html <- function(market, codes) {
  code_blocks <- paste0(vapply(codes, function(c) {
    paste0(
      '<div style="margin:10px 0; padding:18px 22px; background:linear-gradient(135deg,#0F1115 0%,#1A1D24 100%); ',
      'border:2px dashed #C9A14A; border-radius:6px; text-align:center;">',
      '<div style="font-family:\'Segoe UI\',Arial,sans-serif; font-size:10px; font-weight:600; ',
      'letter-spacing:3px; text-transform:uppercase; color:#7C8290; margin-bottom:6px;">',
      'Booking Code</div>',
      '<div style="font-family:Consolas,\'Courier New\',monospace; font-size:26px; font-weight:900; ',
      'letter-spacing:5px; color:#C9A14A;">',
      c, '</div></div>')
  }, character(1)), collapse = "")

  paste0(
    '<!doctype html><html><body style="margin:0;padding:0;background:#0A0B0E;font-family:\'Segoe UI\',Arial,sans-serif;">',
    '<table cellpadding="0" cellspacing="0" width="100%" style="background:#0A0B0E;padding:32px 16px;">',
    '<tr><td align="center">',
    '<table cellpadding="0" cellspacing="0" width="640" ',
    'style="background:#15181E;border-radius:10px;overflow:hidden;border:1px solid #2A2E37;">',
    '<tr><td style="background:linear-gradient(135deg,#0F1115 0%,#1E3A8A 50%,#C9A14A 100%);',
    'padding:32px 36px;">',
    '<div style="color:rgba(255,255,255,0.75);font-size:11px;font-weight:700;letter-spacing:4px;',
    'text-transform:uppercase;margin-bottom:6px;">M.O.A.B \u00b7 1xBet Auto-Bettor</div>',
    '<div style="font-family:Georgia,serif;color:#FFFFFF;font-size:32px;font-weight:900;',
    'line-height:1.1;letter-spacing:-0.5px;">Codes <span style="color:#FCD34D;font-style:italic;">Ready</span></div>',
    '<div style="color:rgba(255,255,255,0.7);font-size:13px;margin-top:10px;">',
    format(Sys.time(), "%A, %d %B %Y &middot; %H:%M"), '</div>',
    '<div style="margin-top:14px;background:rgba(201,161,74,0.15);border:1px solid #C9A14A;',
    'padding:6px 14px;border-radius:14px;font-family:Consolas,monospace;font-size:11px;',
    'color:#C9A14A;font-weight:700;display:inline-block;">',
    market, ' \u00b7 ', length(codes), ' CODE', if (length(codes) == 1) '' else 'S', '</div>',
    '</td></tr>',
    '<tr><td style="padding:28px 36px;">', code_blocks, '</td></tr>',
    '<tr><td style="background:#0F1115;padding:18px 36px;border-top:1px solid #2A2E37;',
    'font-size:10px;letter-spacing:3px;text-transform:uppercase;color:#7C8290;text-align:center;">',
    'Load each code on 1xBet to view and stake',
    '</td></tr></table></td></tr></table></body></html>')
}

send_codes_email <- function(market, codes, log_fn) {
  if (length(codes) == 0) {
    log_fn("No codes to email"); return(FALSE)
  }
  if (!requireNamespace("emayili", quietly = TRUE)) {
    log_fn("emayili package not installed"); return(FALSE)
  }
  msg <- emayili::envelope()
  msg <- emayili::from(msg, EMAIL_FROM)
  msg <- emayili::to(msg, EMAIL_TO)
  msg <- emayili::subject(msg, paste0("1xBet codes \u00b7 ", market, " \u00b7 ",
                                        format(Sys.time(), "%d %b %H:%M")))
  msg <- emayili::html(msg, build_email_html(market, codes))
  smtp <- emayili::server(host = EMAIL_SMTP_HOST, port = EMAIL_SMTP_PORT,
                          username = EMAIL_FROM, password = EMAIL_APP_PASS)
  tryCatch({ smtp(msg, verbose = FALSE); TRUE },
            error = function(e) {
              log_fn(paste0("Email error: ", e$message)); FALSE })
}

# ── Chunking with ≥15 remainder rule ─────────────────────────
chunk_events <- function(events, max_per_slip = 50, min_remainder = 15) {
  n <- length(events)
  if (n == 0) return(list())
  if (n <= max_per_slip) return(list(events))
  chunks <- split(events, ceiling(seq_along(events) / max_per_slip))
  # Drop final chunk if < min_remainder
  last <- chunks[[length(chunks)]]
  if (length(last) < min_remainder) chunks[[length(chunks)]] <- NULL
  unname(chunks)
}

# ────────────────────────────────────────────────────────────
# UI — SB-bettor styled
# ────────────────────────────────────────────────────────────
THEME_CSS <- "
  :root {
    --ivory: #FAF7F2;
    --ivory-2: #F1ECE3;
    --cobalt: #1E3A8A;
    --cobalt-dark: #152a66;
    --gold: #C9A14A;
    --text-1: #1A1A1A;
    --text-2: #4A4A4A;
    --text-3: #888;
    --border: #E5E0D5;
    --good: #2E7D5B;
    --bad: #C44536;
  }
  body { background: var(--ivory-2) !important; color: var(--text-1) !important;
         font-family: 'Inter', 'Segoe UI', system-ui, sans-serif; margin: 0; }
  .container-fluid { max-width: 880px; margin: 32px auto; padding: 0 16px; }
  .ab-header { padding: 0 0 24px; border-bottom: 2px solid var(--gold);
               margin-bottom: 28px; }
  .ab-eyebrow { font-size: 10px; font-weight: 700; letter-spacing: 3px;
                text-transform: uppercase; color: var(--gold); }
  .ab-title { font-size: 26px; font-weight: 600; color: var(--cobalt);
              margin-top: 6px; letter-spacing: -0.01em; }
  .ab-card { background: var(--ivory); border: 1px solid var(--border);
             border-radius: 8px; padding: 22px 24px; margin-bottom: 16px; }
  .ab-step { display: flex; align-items: center; gap: 10px; margin-bottom: 14px; }
  .ab-step-num { width: 22px; height: 22px; border-radius: 50%;
                 background: var(--cobalt); color: var(--ivory);
                 font-weight: 700; font-size: 12px;
                 display: flex; align-items: center; justify-content: center; }
  .ab-step-label { font-size: 11px; font-weight: 700; letter-spacing: 2px;
                   text-transform: uppercase; color: var(--cobalt); }
  .form-control, input[type=file], input[type=text] {
    background: var(--ivory-2) !important; border: 1px solid var(--border) !important;
    color: var(--text-1) !important; border-radius: 6px;
  }
  .ab-status { font-size: 12px; color: var(--text-3); margin-top: 10px; }
  .ab-status-ok { color: var(--good); }
  .ab-btn { background: var(--cobalt); color: var(--ivory); border: none;
            border-radius: 6px; padding: 12px 22px; font-weight: 700;
            font-size: 13px; letter-spacing: 1px; text-transform: uppercase;
            cursor: pointer; transition: all .15s; }
  .ab-btn:hover { background: var(--cobalt-dark); }
  .ab-log { background: #FFFFFF; border: 1px solid var(--border);
            border-radius: 6px; padding: 14px 16px;
            font-family: 'JetBrains Mono', Consolas, monospace;
            font-size: 12px; color: var(--text-2); line-height: 1.6;
            max-height: 420px; overflow-y: auto; white-space: pre-wrap;
            margin: 0; }
  .ab-log::-webkit-scrollbar { width: 6px; }
  .ab-log::-webkit-scrollbar-thumb { background: var(--gold); border-radius: 3px; }
"

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=JetBrains+Mono&display=swap"),
    tags$style(HTML(THEME_CSS))
  ),
  div(class = "ab-header",
      div(class = "ab-eyebrow", "M.O.A.B \u00b7 1xBet"),
      div(class = "ab-title", "Auto-Bettor")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "1"),
          div(class = "ab-step-label", "Upload predictions XLSX")),
      fileInput("preds_file", NULL, accept = ".xlsx", width = "100%"),
      uiOutput("pred_status")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "2"),
          div(class = "ab-step-label", "Match against 1xBet")),
      actionButton("fetch_btn", "Fetch & match", class = "ab-btn"),
      uiOutput("match_status")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "3"),
          div(class = "ab-step-label", "Market")),
      selectInput("market_choice", NULL,
                  choices = list(
                    "1X2" = list(
                      "1X2 Home"          = "1X2 Home",
                      "1X2 Away"          = "1X2 Away",
                      "FT Draw"           = "FT Draw"
                    ),
                    "Double Chance" = list(
                      "Double Chance 1X"  = "Double Chance 1X",
                      "Double Chance 12"  = "Double Chance 12",
                      "Double Chance X2"  = "Double Chance X2"
                    ),
                    "Over / Under" = list(
                      "Over 1.5"          = "Over 1.5",
                      "Over 2.5"          = "Over 2.5"
                    ),
                    "BTTS" = list(
                      "BTTS"              = "BTTS"
                    ),
                    "Cocktail" = list(
                      "Cocktail (coming soon)" = "_disabled_Cocktail"
                    )
                  ),
                  selected = "Over 1.5", width = "100%"),
      uiOutput("market_help")),
  div(class = "ab-card",
      div(class = "ab-step",
          div(class = "ab-step-num", "4"),
          div(class = "ab-step-label", "Generate codes")),
      actionButton("run_btn", "Generate codes and email", class = "ab-btn"),
      tags$br(), tags$br(),
      div(class = "ab-step-label", "Activity log"),
      tags$div(style = "height:8px;"),
      tags$pre(class = "ab-log", textOutput("log_out", inline = TRUE)))
)

# ────────────────────────────────────────────────────────────
# SERVER
# ────────────────────────────────────────────────────────────
server <- function(input, output, session) {
  rv <- reactiveValues(preds = NULL, matched = NULL, unmatched = NULL,
                        log = "")
  log_msg <- function(msg) {
    rv$log <- paste0(rv$log, "[", format(Sys.time(), "%H:%M:%S"), "] ", msg, "\n")
  }
  output$log_out <- renderText({ rv$log })

  # ── Step 1: file upload ──
  observeEvent(input$preds_file, {
    req(input$preds_file)
    sheets <- excel_sheets(input$preds_file$datapath)
    sh <- if ("All Picks" %in% sheets) "All Picks" else sheets[1]
    df <- tryCatch(read_excel(input$preds_file$datapath, sheet = sh),
                    error = function(e) NULL)
    if (is.null(df)) {
      showNotification("Could not read file", type = "error"); return()
    }
    names(df) <- tolower(names(df))
    need <- c("country", "league", "home", "away", "market")
    missing <- setdiff(need, names(df))
    if (length(missing) > 0) {
      showNotification(paste0("Missing columns: ",
                                paste(missing, collapse = ", ")),
                        type = "error"); return()
    }
    rv$preds <- as.data.frame(df)
    log_msg(paste0("Loaded ", nrow(df), " picks"))
  })

  output$pred_status <- renderUI({
    if (is.null(rv$preds)) return(div(class = "ab-status", "No file uploaded yet."))
    n_mkt <- table(rv$preds$market)
    n_cty <- length(unique(rv$preds$country))
    div(class = "ab-status ab-status-ok",
        paste0(nrow(rv$preds), " picks \u00b7 ", n_cty, " countries \u00b7 ",
                paste(paste0(names(n_mkt), " ", n_mkt), collapse = " / ")))
  })

  # ── Step 2: fetch mapper + match ──
  observeEvent(input$fetch_btn, {
    req(rv$preds)
    if (!file.exists(MAPPER_PATH)) {
      log_msg("ERROR: sporty_mapper.rds not found.")
      showNotification("sporty_mapper.rds missing — run SB Mapper first",
                        type = "error", duration = 6); return()
    }
    log_msg("Loading mapper...")
    tryCatch({
      m <- readRDS(MAPPER_PATH)
      xbet_ready <- m[!is.na(m$status) & m$status == "Verified" &
                       !is.na(m$xbet_league_id) &
                       !is.na(m$xbet_reviewed_on) &
                       !is.na(m$fs_country) & !is.na(m$fs_league), ,
                       drop = FALSE]
      if (nrow(xbet_ready) == 0) {
        log_msg("No FS<->1xBet pairs verified. Do 1xBet mapping in SB Mapper first.")
        return()
      }
      log_msg(paste0("Mapper: ", nrow(xbet_ready), " FS<->1xBet pairs"))

      # Match predictions by (fs_country, fs_league)
      preds <- rv$preds
      preds$country <- as.character(preds$country)
      preds$league  <- as.character(preds$league)
      key_pred <- paste0(tolower(preds$country), "||", tolower(preds$league))
      key_map  <- paste0(tolower(xbet_ready$fs_country), "||",
                         tolower(xbet_ready$fs_league))
      idx <- match(key_pred, key_map)

      matched_df <- preds[!is.na(idx), , drop = FALSE]
      unmatched_df <- preds[is.na(idx), , drop = FALSE]
      if (nrow(matched_df) > 0) {
        matched_idx <- idx[!is.na(idx)]
        matched_df$xbet_league_id <- xbet_ready$xbet_league_id[matched_idx]
        matched_df$xbet_kind      <- 3L
      }
      rv$matched   <- matched_df
      rv$unmatched <- unmatched_df
      log_msg(paste0("Matched ", nrow(matched_df), " / ", nrow(preds),
                      " predictions to 1xBet via mapper"))
    }, error = function(e) log_msg(paste0("Error: ", e$message)))
  })

  output$match_status <- renderUI({
    if (is.null(rv$matched))
      return(div(class = "ab-status", "Click Fetch to see matchable picks."))
    by_mkt <- table(rv$matched$market)
    div(class = "ab-status ab-status-ok",
        paste0("\u2713 ", nrow(rv$matched), " bookable / ",
                nrow(rv$unmatched), " not on 1xBet \u00b7 ",
                paste(paste0(names(by_mkt), " ", by_mkt), collapse = " / ")))
  })

  # ── Step 3: market help text ──
  output$market_help <- renderUI({
    choice <- input$market_choice %||% ""
    if (startsWith(choice, "_disabled_")) {
      mkt <- sub("^_disabled_", "", choice)
      return(div(class = "ab-status",
                 style = "color:#C44536;",
                 paste0("\u26A0 ", mkt, " is not yet available.")))
    }
    div(class = "ab-status", paste0("Will book ", choice, " picks only."))
  })

  # ── Step 4: generate ──
  observeEvent(input$run_btn, {
    req(rv$matched, input$market_choice)
    choice <- input$market_choice
    if (startsWith(choice, "_disabled_")) {
      showNotification("That market isn't available yet.", type = "error")
      return()
    }
    if (nrow(rv$matched) == 0) {
      showNotification("Nothing matched. Click Fetch first.", type = "error"); return()
    }

    rv$log <- ""
    log_msg(paste0("=== ", choice, " ==="))

    # Filter to selected market only
    sub <- rv$matched[rv$matched$market == choice, , drop = FALSE]
    if (nrow(sub) == 0) {
      log_msg("No picks for that market."); return()
    }
    log_msg(paste0(nrow(sub), " picks for ", choice))

    # Grab x-hd token + SESSION cookie via visible Chrome (Chrome stays OPEN)
    grab_result <- grab_xhd_token(log_msg)
    xhd <- grab_result$token
    session_cookie <- grab_result$session
    chrome_handle <- grab_result$chrome_handle
    if (is.null(xhd) || !nzchar(xhd)) {
      log_msg("FATAL: could not obtain x-hd token. Aborting.")
      close_chrome(chrome_handle)
      return()
    }
    if (is.null(session_cookie) || !nzchar(session_cookie)) {
      log_msg("WARNING: no SESSION cookie captured. Requests may fail.")
    }
    # Ensure Chrome closes even on error
    on.exit(close_chrome(chrome_handle), add = TRUE)

    # Auto-detect Psiphon proxy port so R traffic goes through VPN.
    # Reads Psiphon's log dir for latest port. Falls back silently if
    # not found — user can set HTTPS_PROXY manually before running.
    tryCatch({
      psi_log <- "C:/Users/Ogbuta/AppData/Roaming/Psiphon3/psiphon-tunnel-core.log"
      if (file.exists(psi_log)) {
        lines <- readLines(psi_log, warn = FALSE)
        http_lines <- grep("HTTP proxy is running on localhost port",
                            lines, value = TRUE)
        if (length(http_lines) > 0) {
          port <- sub(".*port (\\d+).*", "\\1",
                       http_lines[length(http_lines)])
          Sys.setenv(HTTPS_PROXY = paste0("http://127.0.0.1:", port))
          Sys.setenv(HTTP_PROXY  = paste0("http://127.0.0.1:", port))
          log_msg(paste0("Using Psiphon proxy port ", port))
        }
      }
    }, error = function(e) NULL)

    # Fetch games per unique league — sequential (parallel breaks in workers)
    unique_leagues <- unique(sub$xbet_league_id)
    log_msg(paste0("Fetching games for ", length(unique_leagues), " league(s)..."))
    m <- readRDS(MAPPER_PATH)
    league_games <- list()
    withProgress(message = "Fetching 1xBet games",
                  value = 0, {
      for (i in seq_along(unique_leagues)) {
        lid <- unique_leagues[i]
        map_row <- m[!is.na(m$xbet_league_id) & m$xbet_league_id == lid, ,
                      drop = FALSE][1, ]
        ct_name <- if (nrow(map_row) > 0) map_row$xbet_country_name else NA
        lg_name <- if (nrow(map_row) > 0) map_row$xbet_league_name else NA
        gm <- fetch_league_games(lid, xhd,
                                   country_name = ct_name,
                                   league_name = lg_name,
                                   session_cookie = session_cookie)
        league_games[[as.character(lid)]] <- gm
        incProgress(1 / length(unique_leagues),
                     detail = paste0("league ", i, "/",
                                      length(unique_leagues)))
      }
    })

    # Resolve each pick → event
    events <- list()
    n_unresolved <- 0
    for (i in seq_len(nrow(sub))) {
      p <- sub[i, ]
      lid <- as.character(p$xbet_league_id)
      games <- league_games[[lid]] %||% list()
      pick_label <- paste0(p$home, " v ", p$away)
      if (length(games) == 0) {
        log_msg(paste0("  SKIP [no games in league ", lid, "] ", pick_label))
        n_unresolved <- n_unresolved + 1; next
      }
      # Fuzzy team match
      home_target <- tolower(as.character(p$home))
      away_target <- tolower(as.character(p$away))
      best_idx <- NA; best_score <- Inf
      # Track best distances for diagnostic when nothing matches
      diag_best_home_d <- Inf; diag_best_away_d <- Inf
      diag_best_home_name <- ""; diag_best_away_name <- ""
      for (gi in seq_along(games)) {
        g <- games[[gi]]
        gh <- tolower(as.character(g$home))
        ga <- tolower(as.character(g$away))
        d_h <- stringdist(home_target, gh, method = "jw", p = 0.1)
        d_a <- stringdist(away_target, ga, method = "jw", p = 0.1)
        if (d_h < diag_best_home_d) {
          diag_best_home_d <- d_h; diag_best_home_name <- g$home
        }
        if (d_a < diag_best_away_d) {
          diag_best_away_d <- d_a; diag_best_away_name <- g$away
        }
        # Both must be reasonably close; at least one must be tight.
        # Relaxed to catch cases like "Wycombe" vs "Wycombe Wanderers".
        if (max(d_h, d_a) > 0.35) next
        if (min(d_h, d_a) > 0.15) next
        tot <- d_h + d_a
        if (tot < best_score) { best_idx <- gi; best_score <- tot }
      }
      if (is.na(best_idx)) {
        log_msg(paste0("  SKIP [team not matched] ", pick_label,
                       "  \u2192 closest home: '", diag_best_home_name,
                       "' (d=", round(diag_best_home_d, 3), ")",
                       ", closest away: '", diag_best_away_name,
                       "' (d=", round(diag_best_away_d, 3), ")"))
        n_unresolved <- n_unresolved + 1; next
      }
      game <- games[[best_idx]]
      mkt <- MARKET_MAP_XBET[[as.character(p$market)]]
      if (is.null(mkt)) {
        log_msg(paste0("  SKIP [unknown market ", p$market, "] ", pick_label))
        n_unresolved <- n_unresolved + 1; next
      }
      coef <- find_odds(game$raw, mkt$type, mkt$param)
      if (is.na(coef)) {
        # Fallback: fetch full markets for this specific game
        # Build proper referer for the fixture
        slug <- function(s) {
          s <- tolower(s %||% ""); s <- gsub("\\.", "", s)
          s <- gsub("[^a-z0-9]+", "-", s); gsub("^-+|-+$", "", s)
        }
        map_row_lookup <- m[!is.na(m$xbet_league_id) &
                              m$xbet_league_id == as.integer(lid), ,
                             drop = FALSE][1, ]
        ct_slug <- slug(map_row_lookup$xbet_country_name)
        lg_slug <- slug(map_row_lookup$xbet_league_name)
        fx_ref <- paste0(ONEXBET_BASE, "/en/line/football/", lid,
                          "-", ct_slug, "-", lg_slug,
                          "/", game$game_id, "-",
                          slug(game$home), "-", slug(game$away))
        full <- fetch_game_full_markets(game$game_id, xhd, fx_ref, session_cookie)
        coef <- find_odds_in_full(full, mkt$type, mkt$param)
      }
      if (is.na(coef)) {
        log_msg(paste0("  SKIP [odds not in response] ", pick_label,
                       " (type=", mkt$type, " param=", mkt$param, ")"))
        n_unresolved <- n_unresolved + 1; next
      }
      events[[length(events) + 1]] <- list(
        game_id = game$game_id, type = mkt$type, coef = coef,
        param = mkt$param, kind = game$kind)
    }
    n_events <- length(events)
    log_msg(paste0("Resolved ", n_events, " picks (", n_unresolved,
                    " could not be resolved)"))
    if (n_events == 0) { log_msg("Nothing bookable."); return() }

    # Chunk: 50 max per slip, drop remainder < 15
    slips <- chunk_events(events, max_per_slip = 50, min_remainder = 15)
    n_slips <- length(slips)
    dropped <- n_events - sum(vapply(slips, length, integer(1)))
    log_msg(paste0("Chunked into ", n_slips, " slip(s); dropped ", dropped,
                    " picks below 15-remainder threshold"))

    # Book each slip
    codes <- character()
    for (si in seq_along(slips)) {
      log_msg(paste0("Booking slip ", si, "/", n_slips, " (",
                      length(slips[[si]]), " picks)..."))
      res <- book_slip(slips[[si]], xhd, session_cookie)
      if (res$ok) {
        log_msg(paste0("  SUCCESS: code = ", res$code))
        codes <- c(codes, res$code)
      } else {
        log_msg(paste0("  FAIL: ", res$error))
      }
    }

    # Email
    log_msg("Sending email...")
    ok <- send_codes_email(choice, codes, log_msg)
    log_msg(if (ok) "Email sent." else "Email NOT sent.")
    log_msg("Done.")
  })
}

shinyApp(ui, server)

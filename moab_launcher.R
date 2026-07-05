# ============================================================
# M.O.A.B Launcher
# FIFA-card style cards on standard MOAB ivory/cobalt/gold theme.
# ============================================================

suppressPackageStartupMessages({
  library(shiny)
  library(base64enc)
})

BASE_PATH  <- "C:/Users/Ogbuta/OneDrive/New Projects 3"
IMG_DIR    <- file.path(BASE_PATH, "appimages")

APP_FILES <- list(
  main         = file.path(BASE_PATH, "moab.R"),
  live_checker = file.path(BASE_PATH, "moab_live_checker.R"),
  simulator    = file.path(BASE_PATH, "moab_simulator.R"),
  sporty       = file.path(BASE_PATH, "moab_auto_bettor.R"),
  cocktail     = NA
)

ICONS <- list(
  main         = "maldini.png",
  live_checker = "pirlo.png",
  simulator    = "kroos.png",
  sporty       = "modric.png",
  cocktail     = "zidane.png"
)

# Card metadata mirroring FIFA card layout: rating, position abbreviation,
# nation flag (we use the country) and player name.
CARDS <- list(
  main         = list(name = "Maldini",  rating = 96, pos = "CB",  app = "Main pipeline",      desc = "Scrape \u00b7 enrich \u00b7 predict"),
  live_checker = list(name = "Pirlo",    rating = 93, pos = "CM",  app = "Live checker",       desc = "Outcome of every pick"),
  simulator    = list(name = "Kroos",    rating = 92, pos = "CM",  app = "Simulator",          desc = "Test slips before staking"),
  sporty       = list(name = "Modric",   rating = 91, pos = "CAM", app = "Auto-bettor",        desc = "Builds slips \u00b7 emails codes"),
  cocktail     = list(name = "Zidane",   rating = 98, pos = "CAM", app = "Cocktail builder",   desc = "Multi-market slips \u00b7 locked")
)

img_b64 <- function(name) {
  p <- file.path(IMG_DIR, name)
  if (!file.exists(p)) return("")
  paste0("data:image/png;base64,", base64enc::base64encode(p))
}

# Locate Rscript.exe in a stable way (x64 first, then fallback).
rscript_path <- function() {
  p1 <- file.path(R.home("bin"), "x64", "Rscript.exe")
  if (file.exists(p1)) return(p1)
  p2 <- file.path(R.home("bin"), "Rscript.exe")
  if (file.exists(p2)) return(p2)
  # Hard fallback to the user's installed R
  hardcoded <- "C:/Program Files/R/R-4.5.2/bin/x64/Rscript.exe"
  if (file.exists(hardcoded)) return(hardcoded)
  "Rscript.exe"
}

# Launch a Shiny app silently in its own R session by writing a .vbs file and
# invoking it via shell.exec. This avoids any cmd window flashing and works
# regardless of how the launcher itself was started.
launch_app <- function(path) {
  if (is.null(path) || is.na(path) || !file.exists(path)) return(FALSE)
  rs   <- rscript_path()
  apath <- gsub("\\\\", "/", path)
  # In VBS, inside a "..." string, "" is an escaped literal ".
  # Target VBS (exactly one line):
  #   CreateObject("WScript.Shell").Run """RSCRIPT"" -e ""shiny::runApp('APP', launch.browser=TRUE)""", 0, False
  # Built in R using single quotes around the whole thing, so " stays unescaped.
  q3 <- '"""'   # three literal " characters in R
  q2 <- '""'    # two literal " characters in R
  line <- paste0(
    'CreateObject("WScript.Shell").Run ',
    q3, rs, q2, ' -e ', q2,
    "shiny::runApp('", apath, "', launch.browser=TRUE)",
    q3, ', 0, False'
  )
  vbs_path <- tempfile(fileext = ".vbs")
  writeLines(line, vbs_path)
  shell.exec(vbs_path)
  TRUE
}

# ════════════════════════════════════════════════════════════
# THEME — standard MOAB (ivory + cobalt + gold), FIFA card style
# ════════════════════════════════════════════════════════════
THEME <- "
:root {
  --ivory: #FAF7F2;
  --ivory-2: #F1ECE3;
  --cobalt: #1E3A8A;
  --cobalt-dark: #152a66;
  --cobalt-mid: #2B4DAA;
  --gold: #C9A14A;
  --gold-light: #E5C474;
  --gold-soft: #F4DFA5;
  --ink: #1A1A1A;
  --ink-2: #4A4A4A;
  --ink-3: #888;
}
body {
  margin: 0; min-height: 100vh;
  background:
    radial-gradient(circle at 15% 15%, rgba(30,58,138,0.06), transparent 40%),
    radial-gradient(circle at 85% 90%, rgba(201,161,74,0.10), transparent 40%),
    var(--ivory-2);
  color: var(--ink);
  font-family: 'Inter', 'Segoe UI', system-ui, sans-serif;
}
.container-fluid {
  max-width: 1180px;
  margin: 0 auto;
  padding: 48px 28px 60px;
}
.eyebrow {
  text-align: center;
  font-size: 11px; font-weight: 700; letter-spacing: 4px;
  text-transform: uppercase; color: var(--gold);
  margin-bottom: 12px;
}
.title {
  text-align: center;
  font-family: 'Playfair Display', 'Georgia', serif;
  font-size: 56px; font-weight: 700;
  color: var(--cobalt);
  margin: 0;
  letter-spacing: -1px;
}
.subtitle {
  text-align: center;
  font-size: 14px; color: var(--ink-3);
  letter-spacing: 1px;
  margin: 8px 0 44px;
  padding-bottom: 22px;
  border-bottom: 2px solid var(--gold);
}
.grid {
  display: grid;
  grid-template-columns: repeat(auto-fit, minmax(220px, 1fr));
  gap: 26px;
  justify-items: center;
}
/* FIFA CARD ============================================== */
.fifa {
  position: relative;
  width: 220px; height: 320px;
  border-radius: 14px;
  cursor: pointer;
  overflow: hidden;
  transition: transform .25s ease, box-shadow .25s ease;
  box-shadow: 0 10px 30px rgba(30,58,138,.18);
  background:
    radial-gradient(ellipse at 50% 0%, rgba(245,222,160,.85) 0%, rgba(228,189,107,0.95) 35%, var(--gold) 70%, var(--cobalt) 100%);
  padding: 2px;
}
.fifa:hover {
  transform: translateY(-6px) scale(1.02);
  box-shadow: 0 18px 40px rgba(30,58,138,.32);
}
.fifa::before, .fifa::after {
  content: '';
  position: absolute;
  background: linear-gradient(120deg, transparent 30%, rgba(255,255,255,.45) 50%, transparent 70%);
  pointer-events: none;
}
.fifa::before {
  top: -50%; left: -30%; width: 60%; height: 200%;
  transform: rotate(20deg);
  opacity: .35;
}
.fifa-inner {
  position: relative;
  width: 100%; height: 100%;
  border-radius: 12px;
  background:
    radial-gradient(circle at 50% 0%, #FFF6DC 0%, #F0D88E 28%, var(--gold) 60%, var(--cobalt-mid) 100%);
  display: flex; flex-direction: column;
  overflow: hidden;
}
/* Top stripe — rating + position */
.fifa-top {
  position: absolute;
  top: 12px; left: 14px;
  z-index: 2;
  display: flex; flex-direction: column;
  align-items: center;
}
.fifa-rating {
  font-family: 'Playfair Display', Georgia, serif;
  font-size: 30px; font-weight: 700; line-height: 1;
  color: var(--cobalt);
  text-shadow: 0 1px 0 rgba(255,255,255,.7);
}
.fifa-pos {
  margin-top: 2px;
  font-size: 11px; font-weight: 700;
  letter-spacing: 1px;
  color: var(--cobalt);
  text-shadow: 0 1px 0 rgba(255,255,255,.7);
}
.fifa-divider {
  width: 18px; height: 1px;
  background: var(--cobalt);
  opacity: .4;
  margin: 3px auto;
}
/* Player image */
.fifa-photo {
  position: absolute;
  top: 22px; left: 0; right: 0;
  display: flex; justify-content: center;
}
.fifa-photo img {
  width: 170px; height: 170px;
  object-fit: cover;
  object-position: top center;
  filter: drop-shadow(0 4px 8px rgba(0,0,0,.25));
}
.fifa-photo .fb {
  width: 150px; height: 150px;
  display: flex; align-items: center; justify-content: center;
  border-radius: 50%;
  background: rgba(255,255,255,.4);
  font-family: 'Playfair Display', Georgia, serif;
  font-size: 64px; font-weight: 700; font-style: italic;
  color: var(--cobalt);
}
/* Name strip */
.fifa-name {
  position: absolute;
  left: 14px; right: 14px;
  top: 195px;
  text-align: center;
  font-family: 'Playfair Display', Georgia, serif;
  font-size: 22px; font-weight: 700;
  color: var(--cobalt);
  text-shadow: 0 1px 0 rgba(255,255,255,.55);
  letter-spacing: 1px;
  padding-bottom: 6px;
  border-bottom: 1px solid rgba(30,58,138,.35);
}
/* Stats — app name + desc */
.fifa-stats {
  position: absolute;
  left: 14px; right: 14px;
  top: 232px;
  text-align: center;
}
.fifa-app {
  font-size: 13px;
  font-weight: 700;
  letter-spacing: 1.5px;
  text-transform: uppercase;
  color: var(--cobalt);
}
.fifa-desc {
  margin-top: 4px;
  font-size: 11px;
  font-style: italic;
  color: var(--ink-2);
  line-height: 1.35;
}
.fifa-badge {
  position: absolute;
  bottom: 12px; right: 14px;
  width: 28px; height: 28px;
  background: var(--cobalt);
  color: var(--gold);
  font-size: 12px;
  font-weight: 700;
  border-radius: 50%;
  display: flex; align-items: center; justify-content: center;
  border: 2px solid var(--gold);
  box-shadow: 0 2px 6px rgba(0,0,0,.2);
}
.fifa.locked .fifa-inner {
  filter: grayscale(.55) brightness(.92);
}
.fifa.locked {
  cursor: not-allowed;
}
.fifa.locked::after {
  content: 'LOCKED';
  position: absolute;
  top: 50%; left: 50%;
  transform: translate(-50%, -50%) rotate(-18deg);
  font-family: 'Playfair Display', Georgia, serif;
  font-size: 30px;
  font-weight: 700;
  letter-spacing: 6px;
  color: rgba(30,58,138,.85);
  background: rgba(250,247,242,.85);
  padding: 6px 18px;
  border: 2px solid var(--cobalt);
  z-index: 5;
  pointer-events: none;
}
.foot {
  text-align: center;
  margin-top: 56px;
  padding-top: 24px;
  border-top: 1px solid var(--gold);
  font-size: 11px;
  letter-spacing: 3px;
  text-transform: uppercase;
  color: var(--ink-3);
}
.modal-content {
  border-radius: 12px !important;
  border: 2px solid var(--gold) !important;
}
"

card_ui <- function(card_id, locked = FALSE) {
  meta <- CARDS[[card_id]]
  src  <- img_b64(ICONS[[card_id]])
  cls  <- if (isTRUE(locked)) "fifa locked" else "fifa"
  div(class = cls, id = card_id,
      onclick = paste0("Shiny.setInputValue('card_click','", card_id,
                       "',{priority:'event'})"),
      div(class = "fifa-inner",
          div(class = "fifa-top",
              div(class = "fifa-rating", meta$rating),
              div(class = "fifa-pos", meta$pos),
              div(class = "fifa-divider")
          ),
          div(class = "fifa-photo",
              if (nzchar(src)) tags$img(src = src)
              else div(class = "fb", substr(meta$name, 1, 1))
          ),
          div(class = "fifa-name", meta$name),
          div(class = "fifa-stats",
              div(class = "fifa-app", meta$app),
              div(class = "fifa-desc", meta$desc)
          ),
          div(class = "fifa-badge", "M")
      )
  )
}

ui <- fluidPage(
  tags$head(
    tags$link(rel = "stylesheet",
              href = "https://fonts.googleapis.com/css2?family=Inter:wght@400;500;600;700&family=Playfair+Display:wght@500;600;700&display=swap"),
    tags$style(HTML(THEME)),
    tags$title("M.O.A.B")
  ),
  div(class = "eyebrow", "The Squad"),
  h1(class = "title", "M.O.A.B"),
  div(class = "subtitle", "Match, Observe, Analyze, Book"),
  div(class = "grid",
      card_ui("main"),
      card_ui("live_checker"),
      card_ui("simulator"),
      card_ui("sporty"),
      card_ui("cocktail", locked = TRUE)
  ),
  div(class = "foot", "Tap a card to launch")
)

server <- function(input, output, session) {
  observeEvent(input$card_click, {
    cid <- input$card_click
    if (cid == "cocktail") {
      showModal(modalDialog(
        title = NULL, easyClose = TRUE, footer = modalButton("Close"),
        div(style = "font-family:Inter,sans-serif; text-align:center; padding:14px 8px;",
            div(style = "font-size:11px; letter-spacing:3px; color:#C9A14A; text-transform:uppercase; font-weight:700;",
                "Cocktail Builder"),
            div(style = "font-family:'Playfair Display',Georgia,serif; font-size:26px; color:#1E3A8A; margin:8px 0 14px;",
                "Not yet ready"),
            div(style = "font-size:14px; color:#4A4A4A; line-height:1.55;",
                "Coming once all individual markets are tuned. Until then, ",
                "use Auto-Bettor for single-market booking codes."))
      ))
      return()
    }
    path <- APP_FILES[[cid]]
    if (is.null(path) || is.na(path) || !file.exists(path)) {
      showNotification(paste0("App file missing: ", path), type = "error", duration = 6)
      return()
    }
    ok <- launch_app(path)
    if (ok) showNotification("Launching...", type = "message", duration = 3)
    else showNotification("Failed to launch.", type = "error", duration = 6)
  })
}

shinyApp(ui, server)
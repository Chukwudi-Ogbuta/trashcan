## ============================================================================
## R/modules/mod_reports.R + reporting engine
## Executive-grade haulage report mirroring Analytics.
## ============================================================================

reports_ui <- function(id) {
  ns <- NS(id)
  tagList(
    htmltools::div(class = "kf-section-header",
      htmltools::div(
        htmltools::div(class = "kf-eyebrow", "Output"),
        htmltools::h2("Reports"),
        htmltools::p(class = "kf-sub",
          "Generate haulage reports, preview, download PDF, and email."))),

    htmltools::div(class = "kf-card-elevated", style = "margin-bottom:18px;",
      htmltools::h4("Build a report", style = "margin-top:0;"),
      htmltools::div(style = "display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-top:8px;",
        selectInput(ns("period"), "Period",
                    choices = c("Daily"="Daily","Weekly"="Weekly","Monthly"="Monthly",
                                "Yearly"="Yearly","All-time"="All-time","Custom"="Custom"),
                    selected = "Weekly", width = "100%"),
        dateInput(ns("anchor"), "Anchor date", value = Sys.Date(),
                  max = Sys.Date(), weekstart = 1, width = "100%"),
        htmltools::div(style = "display:flex;align-items:end;",
          actionButton(ns("gen"),
                       htmltools::tagList(htmltools::tags$i(class="ti ti-file-text"),
                                          " Generate report"),
                       class = "btn-primary"))),
      htmltools::div(id = ns("custom_wrap"), style = "display:none;margin-top:10px;",
        dateRangeInput(ns("custom"), "Custom range",
                       start = Sys.Date()-7, end = Sys.Date(),
                       weekstart = 1, width = "100%"))),

    htmltools::div(id = ns("preview_anchor"))
  )
}

reports_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    ns <- session$ns
    rep <- reactiveValues(report = NULL)

    observeEvent(input$period, {
      code <- if (identical(input$period, "Custom"))
        sprintf("document.getElementById('%s').style.display='block';", ns("custom_wrap"))
      else
        sprintf("document.getElementById('%s').style.display='none';", ns("custom_wrap"))
      session$sendCustomMessage("kf_eval", list(code = code))
    })

    period_range <- reactive({
      a <- input$anchor; p <- input$period
      if (identical(p, "Daily")) list(start = a, end = a)
      else if (identical(p, "Weekly")) {
        wd <- as.integer(format(a, "%u"))
        list(start = a - (wd - 1), end = a - (wd - 1) + 6)
      } else if (identical(p, "Monthly")) {
        first <- as.Date(format(a, "%Y-%m-01"))
        list(start = first,
             end   = seq(first, by = "month", length.out = 2)[2] - 1)
      } else if (identical(p, "Yearly")) {
        list(start = as.Date(format(a, "%Y-01-01")), end = as.Date(format(a, "%Y-12-31")))
      } else if (identical(p, "All-time")) {
        first <- db_get("select min(trip_date) as d from trips;")$d
        list(start = if (length(first) == 0 || is.na(first)) a else as.Date(first), end = a)
      } else list(start = input$custom[1], end = input$custom[2])
    })

    observeEvent(input$gen, {
      rng <- period_range()
      tryCatch({
        rep$report <- generate_haul_report(input$period, rng$start, rng$end)
        removeUI(selector = paste0("#", ns("preview_panel")), immediate = TRUE)
        insertUI(selector = paste0("#", ns("preview_anchor")), where = "beforeEnd",
          ui = htmltools::div(id = ns("preview_panel"), class = "kf-card-elevated",
            htmltools::div(style = "display:flex;justify-content:space-between;align-items:center;margin-bottom:14px;",
              htmltools::div(htmltools::h4("Preview", style = "margin:0;"),
                htmltools::div(paste0(format(rng$start, "%d %b %Y"), " \u2013 ",
                                      format(rng$end,   "%d %b %Y")),
                  style = "font-size:11px;color:#8A8780;letter-spacing:0.06em;margin-top:2px;")),
              htmltools::div(style = "display:flex;gap:8px;",
                downloadButton(ns("dl"),
                  htmltools::tagList(htmltools::tags$i(class="ti ti-download"), " PDF"),
                  class = "btn-outline-primary btn-sm"),
                downloadButton(ns("dl_xlsx"),
                  htmltools::tagList(htmltools::tags$i(class="ti ti-file-spreadsheet"), " Excel"),
                  class = "btn-outline-primary btn-sm"),
                actionButton(ns("email_self"),
                  htmltools::tagList(htmltools::tags$i(class="ti ti-mail"), " Email myself"),
                  class = "btn-outline-primary btn-sm"),
                actionButton(ns("email_share"),
                  htmltools::tagList(htmltools::tags$i(class="ti ti-send"), " Email shareholders"),
                  class = "btn-primary"))),
            htmltools::tags$iframe(srcdoc = rep$report$html_content,
              style = "width:100%;height:900px;border:0.5px solid #E8E6E1;border-radius:8px;background:#FBFAF6;")))
        showNotification("Report generated.", type = "message", duration = 3)
      }, error = function(e) {
        showNotification(paste("Generation failed:", conditionMessage(e)),
                         type = "error", duration = 8)
      })
    })

    output$dl <- downloadHandler(
      filename = function() {
        if (!is.null(rep$report$pdf_path) && file.exists(rep$report$pdf_path))
          basename(rep$report$pdf_path) else basename(rep$report$html_path)
      },
      content = function(file) {
        src <- if (!is.null(rep$report$pdf_path) && file.exists(rep$report$pdf_path))
                 rep$report$pdf_path else rep$report$html_path
        file.copy(src, file, overwrite = TRUE)
      })

    output$dl_xlsx <- downloadHandler(
      filename = function() {
        s <- rep$report$summary
        sprintf("KifayatHaul_%s_%s_to_%s.xlsx",
                gsub("[^A-Za-z0-9]", "", s$period),
                format(s$range_start, "%Y%m%d"),
                format(s$range_end,   "%Y%m%d"))
      },
      content = function(file) {
        if (is.null(rep$report)) stop("Generate a report first.")
        build_haul_xlsx(rep$report$summary$range_start,
                        rep$report$summary$range_end,
                        rep$report$summary$period,
                        file)
      })

    do_send <- function(audience) {
      if (is.null(rep$report)) {
        showNotification("Generate first.", type = "warning"); return()
      }
      emails <- db_get("select email from report_recipients
                         where audience = $1 and active;", params = list(audience))$email
      if (length(emails) == 0) {
        showNotification("No active recipients.", type = "warning"); return()
      }
      tryCatch({
        ## Build xlsx fresh to attach alongside the PDF
        s <- rep$report$summary
        xlsx_name <- sprintf("KifayatHaul_%s_%s_to_%s.xlsx",
                             gsub("[^A-Za-z0-9]", "", s$period),
                             format(s$range_start, "%Y%m%d"),
                             format(s$range_end,   "%Y%m%d"))
        xlsx_path <- file.path(tempdir(), xlsx_name)
        build_haul_xlsx(s$range_start, s$range_end, s$period, xlsx_path)

        send_haul_email(rep$report, recipients = emails,
                        extra_attachments = xlsx_path)
        showNotification(sprintf("Email sent to %d recipient(s).", length(emails)),
                         type = "message", duration = 5)
      }, error = function(e) {
        showNotification(paste("Email failed:", conditionMessage(e)),
                         type = "error", duration = 10)
      })
    }
    observeEvent(input$email_self,  do_send("self"))
    observeEvent(input$email_share, do_send("shareholders"))
  })
}


## ============================================================================
## REPORTING ENGINE (haulage) — mirrors analytics
## ============================================================================

fmt_n <- function(x) paste0("\u20A6", format(round(x), big.mark=","))
fmt_l <- function(x, d = 1) paste0(format(round(x, d), big.mark = ",", nsmall = d), " L")

generate_haul_report <- function(period_label, start_date, end_date,
                                 out_dir = "reports") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  sd <- as.Date(start_date); ed <- as.Date(end_date)
  d <- haul_summary(sd, ed)
  trips <- d$trips; expenses <- d$expenses

  ## --- KPIs ---
  n_trips    <- nrow(trips)
  total_l    <- sum(trips$litres_consumed, na.rm = TRUE)
  total_feed <- sum(trips$feeding_naira,   na.rm = TRUE)
  total_exp  <- sum(expenses$amount_naira, na.rm = TRUE)
  total_cost <- total_feed + total_exp
  avg_l      <- if (n_trips > 0) total_l / n_trips else 0

  ## --- Cost composition ---
  buckets <- if (nrow(expenses) > 0) {
    expenses %>% dplyr::mutate(bucket = classify_expense(category)) %>%
      dplyr::group_by(bucket) %>%
      dplyr::summarise(amt = sum(amount_naira, na.rm = TRUE), .groups = "drop")
  } else data.frame(bucket = character(), amt = numeric())
  composition <- data.frame(
    bucket = c("Driver feeding", "Loading fees", "Repairs & parts",
               "Workmanship & transport", "Other"),
    amt = c(total_feed,
            sum(buckets$amt[buckets$bucket == "Loading fees"]),
            sum(buckets$amt[buckets$bucket == "Repairs & parts"]),
            sum(buckets$amt[buckets$bucket == "Workmanship & transport"]),
            sum(buckets$amt[buckets$bucket == "Other"]))
  )
  total_for_pct <- sum(composition$amt)

  ## --- Truck × Route fuel ---
  truck_route <- if (n_trips > 0) {
    trips %>% dplyr::mutate(route = paste(origin, "\u2192", destination)) %>%
      dplyr::group_by(plate_number, route) %>%
      dplyr::summarise(avg_l = mean(litres_consumed, na.rm = TRUE),
                       trips = dplyr::n(), .groups = "drop")
  } else data.frame()

  ## --- Driver feeding + trips ---
  driver_stats <- if (n_trips > 0) {
    trips %>% dplyr::group_by(driver_name) %>%
      dplyr::summarise(trips = dplyr::n(),
                       feeding = sum(feeding_naira, na.rm = TRUE),
                       avg_feeding = mean(feeding_naira, na.rm = TRUE),
                       routes_covered = dplyr::n_distinct(paste(origin, destination)),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(feeding))
  } else data.frame()

  ## --- Driver × Route freq ---
  driver_route <- if (n_trips > 0) {
    trips %>% dplyr::mutate(route = paste(origin, "\u2192", destination)) %>%
      dplyr::group_by(driver_name, route) %>%
      dplyr::summarise(n = dplyr::n(), .groups = "drop")
  } else data.frame()

  ## --- Truck repair ranking ---
  truck_repairs <- if (nrow(expenses) > 0) {
    expenses %>% dplyr::mutate(bucket = classify_expense(category)) %>%
      dplyr::filter(bucket %in% c("Repairs & parts", "Workmanship & transport")) %>%
      dplyr::filter(!is.na(plate_number)) %>%
      dplyr::group_by(plate_number) %>%
      dplyr::summarise(amt = sum(amount_naira, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(amt))
  } else data.frame()

  ## --- Route economics ---
  route_econ <- if (n_trips > 0) {
    trips %>% dplyr::mutate(route = paste(origin, "\u2192", destination)) %>%
      dplyr::group_by(route) %>%
      dplyr::summarise(trips = dplyr::n(),
                       avg_l = mean(litres_consumed, na.rm = TRUE),
                       avg_feed = mean(feeding_naira, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(trips))
  } else data.frame()

  ## --- Top expense categories ---
  top_cats <- if (nrow(expenses) > 0) {
    expenses %>% dplyr::group_by(category) %>%
      dplyr::summarise(amt = sum(amount_naira, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(amt))
  } else data.frame()

  ## ========================================================================
  ## HTML BUILD
  ## ========================================================================

  css <- '<style>
    body{margin:0;padding:0;background:#FBFAF6;font-family:Inter,system-ui,-apple-system,sans-serif;color:#2A2A28;-webkit-font-smoothing:antialiased;}
    .wrap{max-width:780px;margin:0 auto;padding:42px 36px;}
    .cover{display:flex;justify-content:space-between;align-items:flex-end;padding-bottom:22px;border-bottom:0.5px solid #E8E6E1;}
    .brand{display:flex;align-items:center;gap:12px;}
    .brand-mark{width:36px;height:36px;background:#B8862B;color:#FBF6E8;border-radius:8px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:18px;}
    .brand-name{font-size:16px;font-weight:500;}
    .brand-sub{font-size:10px;color:#8A8780;letter-spacing:0.14em;text-transform:uppercase;margin-top:2px;}
    .cover-right{text-align:right;}
    .cover-right .eyebrow{font-size:10px;color:#B8862B;letter-spacing:0.14em;text-transform:uppercase;font-weight:500;}
    .cover-right .period{font-size:22px;font-weight:500;margin-top:4px;letter-spacing:-0.015em;}
    .cover-right .range{font-size:12px;color:#8A8780;margin-top:2px;}
    h2{font-size:12.5px;font-weight:500;color:#B8862B;letter-spacing:0.13em;text-transform:uppercase;margin:32px 0 12px;}
    .lead{font-size:11.5px;color:#8A8780;margin:-8px 0 14px;line-height:1.5;}
    .summary{font-size:14px;line-height:1.7;color:#2A2A28;margin-top:14px;}
    .kpi-grid{display:grid;grid-template-columns:repeat(5,1fr);gap:9px;margin-top:12px;}
    .kpi{background:#fff;border:0.5px solid #E8E6E1;border-radius:10px;padding:13px 14px;position:relative;overflow:hidden;}
    .kpi::before{content:"";position:absolute;left:0;top:0;bottom:0;width:3px;background:#B8862B;}
    .kpi.warn::before{background:#1F4D2C;}
    .kpi.alt::before{background:#A8323A;}
    .kpi-label{font-size:9px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;}
    .kpi-value{font-size:16px;font-weight:500;letter-spacing:-0.01em;line-height:1.1;font-variant-numeric:tabular-nums;}
    .kpi-foot{font-size:10px;color:#8A8780;margin-top:4px;}
    .comp-row{display:grid;grid-template-columns:170px 1fr 110px 60px;gap:10px;align-items:center;padding:7px 0;border-bottom:0.5px solid #F2F0EB;font-size:12.5px;}
    .comp-bar{background:#F4ECDB;height:7px;border-radius:4px;overflow:hidden;}
    .comp-fill{height:100%;}
    .swatch{display:inline-block;width:9px;height:9px;border-radius:2px;margin-right:7px;vertical-align:middle;}
    table{width:100%;border-collapse:collapse;margin-top:6px;font-size:12.5px;font-variant-numeric:tabular-nums;}
    th{font-size:9.5px;color:#5C5A52;font-weight:500;letter-spacing:0.06em;text-transform:uppercase;text-align:left;padding:7px 9px;border-bottom:0.5px solid #E8E6E1;}
    td{padding:8px 9px;border-bottom:0.5px solid #F2F0EB;}
    td.num,th.num{text-align:right;}
    .heat-table{font-size:11px;}
    .heat-table th{padding:6px 4px;text-align:center;min-width:60px;}
    .heat-table .heat-cell{height:28px;display:flex;align-items:center;justify-content:center;margin:1px;border-radius:4px;font-weight:500;}
    .twocol{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-top:6px;}
    .footer{margin-top:42px;padding-top:18px;border-top:0.5px solid #E8E6E1;font-size:10.5px;color:#8A8780;letter-spacing:0.06em;text-transform:uppercase;display:flex;justify-content:space-between;}
  </style>'

  ## --- Cost composition rows ---
  colors <- c("#B8862B", "#1F4D2C", "#A8323A", "#3D7050", "#5C5A52")
  comp_html <- if (total_for_pct > 0) {
    paste(vapply(seq_len(nrow(composition)), function(i) {
      pct <- 100 * composition$amt[i] / total_for_pct
      sprintf('<div class="comp-row">
        <div><span class="swatch" style="background:%s;"></span>%s</div>
        <div class="comp-bar"><div class="comp-fill" style="background:%s;width:%.1f%%;"></div></div>
        <div class="num">%s</div>
        <div class="num" style="color:#8A8780;">%.0f%%</div></div>',
        colors[i], composition$bucket[i], colors[i], pct,
        fmt_n(composition$amt[i]), pct)
    }, character(1)), collapse = "")
  } else "<div style='padding:10px 0;color:#8A8780;font-size:12px;'>No costs in range.</div>"

  ## --- Truck × Route heatmap ---
  build_heatmap <- function(d, row_col, col_col, val_col, fmt) {
    if (nrow(d) == 0)
      return("<div style='padding:10px 0;color:#8A8780;font-size:12px;'>No data.</div>")
    rows <- sort(unique(d[[row_col]])); cols <- sort(unique(d[[col_col]]))
    if (length(rows) == 0 || length(cols) == 0)
      return("<div style='padding:10px 0;color:#8A8780;font-size:12px;'>No data.</div>")
    max_v <- max(d[[val_col]], na.rm = TRUE)
    header <- paste0(
      sprintf('<th style="text-align:left;">%s</th>', row_col),
      paste(vapply(cols, function(c) sprintf('<th>%s</th>', c), character(1)),
            collapse = ""))
    body <- vapply(rows, function(rname) {
      cells <- vapply(cols, function(cname) {
        m <- d[d[[row_col]] == rname & d[[col_col]] == cname, , drop = FALSE]
        if (nrow(m) == 0)
          return('<td style="padding:0;"><div class="heat-cell" style="background:#F5F3EE;"></div></td>')
        v <- m[[val_col]][1]
        intensity <- min(1, v / max_v)
        alpha <- 0.20 + 0.70 * intensity
        sprintf('<td style="padding:0;"><div class="heat-cell" style="background:rgba(184,134,43,%.2f);color:%s;">%s</div></td>',
                alpha, if (intensity > 0.5) "#fff" else "#2A2A28", fmt(v))
      }, character(1))
      sprintf('<tr><td style="font-weight:500;font-size:11.5px;">%s</td>%s</tr>',
              rname, paste(cells, collapse = ""))
    }, character(1))
    sprintf('<table class="heat-table"><thead><tr>%s</tr></thead><tbody>%s</tbody></table>',
            header, paste(body, collapse = ""))
  }

  tr_heat <- if (nrow(truck_route) > 0) {
    names(truck_route)[1:3] <- c("Truck", "Route", "value")
    build_heatmap(truck_route[, c("Truck", "Route", "value")],
                  "Truck", "Route", "value",
                  function(v) sprintf("%s L", format(round(v, 0), big.mark = ",")))
  } else "<div style='padding:10px 0;color:#8A8780;font-size:12px;'>No trips in range.</div>"

  dr_heat <- if (nrow(driver_route) > 0) {
    names(driver_route)[1:3] <- c("Driver", "Route", "value")
    build_heatmap(driver_route[, c("Driver", "Route", "value")],
                  "Driver", "Route", "value",
                  function(v) as.character(as.integer(v)))
  } else "<div style='padding:10px 0;color:#8A8780;font-size:12px;'>No trips in range.</div>"

  ## --- Driver activity table ---
  driver_tbl <- if (nrow(driver_stats) > 0) {
    paste(vapply(seq_len(nrow(driver_stats)), function(i) sprintf(
      '<tr><td><strong>%s</strong></td><td class="num">%d</td><td class="num">%s</td><td class="num">%s</td><td class="num">%d</td></tr>',
      driver_stats$driver_name[i], as.integer(driver_stats$trips[i]),
      fmt_n(driver_stats$feeding[i]), fmt_n(driver_stats$avg_feeding[i]),
      as.integer(driver_stats$routes_covered[i])), character(1)), collapse = "")
  } else "<tr><td colspan='5' style='color:#8A8780;'>No drivers active.</td></tr>"

  ## --- Truck repair bars ---
  repair_html <- if (nrow(truck_repairs) > 0) {
    max_v <- max(truck_repairs$amt)
    paste(vapply(seq_len(nrow(truck_repairs)), function(i) sprintf(
      '<div class="comp-row" style="grid-template-columns:140px 1fr 130px;">
        <div style="font-family:JetBrains Mono,monospace;font-size:12.5px;">%s</div>
        <div class="comp-bar"><div class="comp-fill" style="background:#A8323A;width:%.1f%%;"></div></div>
        <div class="num">%s</div></div>',
      truck_repairs$plate_number[i], 100 * truck_repairs$amt[i] / max_v,
      fmt_n(truck_repairs$amt[i])), character(1)), collapse = "")
  } else "<div style='padding:10px 0;color:#8A8780;font-size:12px;'>No repair spend in range.</div>"

  ## --- Route economics ---
  route_tbl <- if (nrow(route_econ) > 0) {
    paste(vapply(seq_len(nrow(route_econ)), function(i) sprintf(
      '<tr><td>%s</td><td class="num">%d</td><td class="num">%s</td><td class="num">%s</td></tr>',
      route_econ$route[i], as.integer(route_econ$trips[i]),
      fmt_l(route_econ$avg_l[i]), fmt_n(route_econ$avg_feed[i])), character(1)), collapse = "")
  } else "<tr><td colspan='4' style='color:#8A8780;'>No routes.</td></tr>"

  ## --- Top categories ---
  cat_html <- if (nrow(top_cats) > 0) {
    show <- if (nrow(top_cats) > 10) top_cats[1:10, ] else top_cats
    rows <- paste(vapply(seq_len(nrow(show)), function(i) sprintf(
      '<tr><td>%s</td><td class="num">%s</td></tr>',
      show$category[i], fmt_n(show$amt[i])), character(1)), collapse = "")
    if (nrow(top_cats) > 10) {
      other <- sum(top_cats$amt[11:nrow(top_cats)])
      rows <- paste0(rows, sprintf(
        '<tr><td style="color:#8A8780;font-style:italic;">All other (%d items)</td><td class="num" style="color:#8A8780;">%s</td></tr>',
        nrow(top_cats) - 10, fmt_n(other)))
    }
    rows
  } else "<tr><td colspan='2' style='color:#8A8780;'>No expenses.</td></tr>"

  ## --- Executive summary text ---
  share_feed <- if (total_cost > 0) round(100 * total_feed / total_cost) else 0
  share_rep  <- if (total_cost > 0) round(100 * sum(composition$amt[composition$bucket %in% c("Repairs & parts","Workmanship & transport")]) / total_cost) else 0
  summary_text <- sprintf(
    "The %s ending %s saw <strong>%d trip(s)</strong> dispatched across the fleet. Trucks consumed <strong>%s</strong> of fuel (avg %s/trip). Total spend reached <strong>%s</strong>: <strong>%d%%</strong> to driver feeding, <strong>%d%%</strong> to repairs and workmanship.",
    tolower(period_label), format(ed, "%d %B %Y"),
    n_trips, fmt_l(total_l), fmt_l(avg_l),
    fmt_n(total_cost), share_feed, share_rep)

  html <- paste0('<!DOCTYPE html><html><head><meta charset="utf-8"><title>KifayatHaul Report</title>',
    css, '</head><body><div class="wrap">',
    ## Cover
    '<div class="cover"><div class="brand">',
    '<div class="brand-mark">H</div>',
    '<div><div class="brand-name">KifayatHaul</div>',
    '<div class="brand-sub">Haulage Operations</div></div></div>',
    '<div class="cover-right">',
    '<div class="eyebrow">', period_label, ' report</div>',
    '<div class="period">', format(ed, "%d %B %Y"), '</div>',
    '<div class="range">', format(sd, "%d %b"),
    if (sd != ed) paste0(" \u2013 ", format(ed, "%d %b %Y")) else "",
    '</div></div></div>',

    ## Executive summary
    '<h2>Executive summary</h2><div class="summary">', summary_text, '</div>',

    ## KPI strip
    '<h2>Headline KPIs</h2><div class="kpi-grid">',
    '<div class="kpi"><div class="kpi-label">Trips</div><div class="kpi-value">', n_trips, '</div></div>',
    '<div class="kpi"><div class="kpi-label">Fuel consumed</div><div class="kpi-value">', fmt_l(total_l), '</div>',
    '<div class="kpi-foot">avg ', fmt_l(avg_l), '/trip</div></div>',
    '<div class="kpi warn"><div class="kpi-label">Driver feeding</div><div class="kpi-value">', fmt_n(total_feed), '</div></div>',
    '<div class="kpi"><div class="kpi-label">Truck expenses</div><div class="kpi-value">', fmt_n(total_exp), '</div></div>',
    '<div class="kpi alt"><div class="kpi-label">Total cost</div><div class="kpi-value">', fmt_n(total_cost), '</div></div>',
    '</div>',

    ## Cost composition
    '<h2>Cost composition</h2>',
    '<div class="lead">Where the money goes \u2014 feeding to drivers, fuel & loading, repairs.</div>',
    comp_html,

    ## Truck × Route heat
    '<h2>Fuel consumption \u00B7 truck \u00D7 route</h2>',
    '<div class="lead">Average litres per trip. Heavier cells = higher consumption on that pairing.</div>',
    tr_heat,

    ## Driver heat
    '<h2>Driver \u00D7 route frequency</h2>',
    '<div class="lead">Trip count per driver per route \u2014 specialization vs versatility.</div>',
    dr_heat,

    ## Truck repair bars
    '<h2>Truck repair cost</h2>',
    '<div class="lead">Total repair, parts and workmanship spend per truck. Excludes loading fees.</div>',
    repair_html,

    ## Route economics
    '<h2>Route economics</h2>',
    '<table><thead><tr>',
    '<th>Route</th><th class="num">Trips</th><th class="num">Avg fuel</th><th class="num">Avg feeding</th>',
    '</tr></thead><tbody>', route_tbl, '</tbody></table>',

    ## Driver activity
    '<h2>Driver activity</h2>',
    '<table><thead><tr>',
    '<th>Driver</th><th class="num">Trips</th><th class="num">Feeding</th><th class="num">Avg / trip</th><th class="num">Routes</th>',
    '</tr></thead><tbody>', driver_tbl, '</tbody></table>',

    ## Top expense categories
    '<h2>Top expense categories</h2>',
    '<table><thead><tr><th>Category</th><th class="num">Amount</th></tr></thead>',
    '<tbody>', cat_html, '</tbody></table>',

    '<div class="footer"><div>Generated by KifayatHaul</div>',
    '<div>', format(Sys.time(), "%d %b %Y \u00B7 %H:%M"), '</div></div>',
    '</div></body></html>')

  fname_base <- sprintf("KifayatHaul_%s_%s_to_%s",
                        gsub("[^A-Za-z0-9]", "", period_label),
                        format(sd, "%Y%m%d"), format(ed, "%Y%m%d"))
  html_path <- file.path(out_dir, paste0(fname_base, ".html"))
  writeLines(html, html_path, useBytes = TRUE)

  pdf_path <- file.path(out_dir, paste0(fname_base, ".pdf"))
  pdf_ok <- tryCatch({
    if (requireNamespace("pagedown", quietly = TRUE)) {
      pagedown::chrome_print(input = normalizePath(html_path),
                             output = normalizePath(pdf_path, mustWork = FALSE),
                             format = "pdf", verbose = 0, wait = 1)
      file.exists(pdf_path)
    } else FALSE
  }, error = function(e) FALSE)

  list(html_path = html_path, html_content = html,
       pdf_path = if (isTRUE(pdf_ok)) pdf_path else NULL,
       summary = list(period = period_label, range_start = sd, range_end = ed,
                      trips = n_trips, total_cost = total_cost,
                      total_litres = total_l, total_feeding = total_feed,
                      total_expenses = total_exp))
}


## ---- Email --------------------------------------------------------------

send_haul_email <- function(report, recipients, subject = NULL,
                            extra_attachments = character()) {
  s <- report$summary
  if (is.null(subject)) {
    subject <- sprintf("KifayatHaul %s Report \u2014 %s",
                       s$period,
                       if (s$range_start == s$range_end) format(s$range_end, "%d %b %Y")
                       else paste(format(s$range_start, "%d %b"),
                                  format(s$range_end, "%d %b %Y"), sep = " \u2013 "))
  }

  range_str <- if (s$range_start == s$range_end) format(s$range_end, "%d %B %Y")
               else paste(format(s$range_start, "%d %b"),
                          format(s$range_end, "%d %b %Y"), sep = " \u2013 ")

  body <- sprintf('
  <html><body style="margin:0;padding:0;background:#FBFAF6;font-family:Inter,system-ui,-apple-system,sans-serif;color:#2A2A28;">
    <div style="max-width:560px;margin:0 auto;padding:40px 28px;">
      <div style="display:flex;align-items:center;gap:12px;padding-bottom:20px;border-bottom:0.5px solid #E8E6E1;">
        <div style="width:38px;height:38px;background:#B8862B;color:#FBF6E8;border-radius:8px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:18px;">H</div>
        <div>
          <div style="font-size:16px;font-weight:500;color:#2A2A28;">KifayatHaul</div>
          <div style="font-size:10px;color:#8A8780;letter-spacing:0.14em;text-transform:uppercase;margin-top:2px;">Haulage Operations</div>
        </div>
      </div>
      <div style="margin-top:30px;">
        <div style="font-size:10px;color:#B8862B;letter-spacing:0.14em;text-transform:uppercase;font-weight:500;">%s report</div>
        <div style="font-size:22px;font-weight:500;margin-top:4px;letter-spacing:-0.015em;">%s</div>
      </div>
      <div style="margin-top:24px;font-size:14px;line-height:1.7;">
        <p style="margin:0 0 14px;">Dear Sir,</p>
        <p style="margin:0 0 14px;">Please find attached the <strong>%s haulage report</strong> for the period <strong>%s</strong>.</p>
        <p style="margin:0 0 14px;">Headline figures:</p>
      </div>
      <table cellpadding="0" cellspacing="0" border="0" style="width:100%%;border-collapse:collapse;margin-top:8px;">
        <tr>
          <td style="background:#fff;border:0.5px solid #E8E6E1;border-radius:10px;padding:14px 16px;width:33%%;border-left:3px solid #B8862B;">
            <div style="font-size:9.5px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;">Trips</div>
            <div style="font-size:17px;font-weight:500;font-variant-numeric:tabular-nums;">%d</div>
          </td>
          <td style="width:6px;"></td>
          <td style="background:#fff;border:0.5px solid #E8E6E1;border-radius:10px;padding:14px 16px;width:33%%;border-left:3px solid #B8862B;">
            <div style="font-size:9.5px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;">Litres</div>
            <div style="font-size:17px;font-weight:500;font-variant-numeric:tabular-nums;">%s</div>
          </td>
          <td style="width:6px;"></td>
          <td style="background:#fff;border:0.5px solid #E8E6E1;border-radius:10px;padding:14px 16px;width:33%%;border-left:3px solid #A8323A;">
            <div style="font-size:9.5px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;">Total cost</div>
            <div style="font-size:17px;font-weight:500;font-variant-numeric:tabular-nums;">%s</div>
          </td>
        </tr>
      </table>
      <div style="margin-top:24px;font-size:14px;line-height:1.7;">
        <p style="margin:0 0 14px;">The full report \u2014 with cost composition, truck \u00D7 route fuel heatmap, driver activity, and expense breakdowns \u2014 is attached as a PDF, along with a styled Excel workbook containing the underlying data across multiple sheets.</p>
      </div>
      <div style="margin-top:32px;padding-top:20px;border-top:0.5px solid #E8E6E1;font-size:13px;line-height:1.6;">
        Kind regards,<br>
        <strong>Chukwudi Samuel Ogbuta</strong><br>
        <span style="color:#8A8780;font-size:12px;">Data Analyst \u00B7 Kifayat Global Energy Limited</span>
      </div>
    </div>
  </body></html>',
    s$period, range_str, tolower(s$period), range_str,
    s$trips, paste0(format(round(s$total_litres,1), big.mark=","), " L"),
    paste0("\u20A6", format(round(s$total_cost), big.mark=",")))

  msg <- emayili::envelope() |>
    emayili::from("kifayatdata.reports@gmail.com") |>
    emayili::to(recipients) |>
    emayili::subject(subject) |>
    emayili::html(body)
  if (!is.null(report$pdf_path) && file.exists(report$pdf_path))
    msg <- emayili::attachment(msg, report$pdf_path)
  else if (file.exists(report$html_path))
    msg <- emayili::attachment(msg, report$html_path)

  for (f in extra_attachments) {
    if (!is.null(f) && nzchar(f) && file.exists(f))
      msg <- emayili::attachment(msg, f)
  }

  smtp <- emayili::server(host="smtp.gmail.com", port=465,
    username="kifayatdata.reports@gmail.com",
    password = Sys.getenv("GMAIL_APP_PASSWORD"),
    use_ssl = TRUE, reuse = FALSE)
  smtp(msg, verbose = FALSE)
  TRUE
}


## ============================================================================
## EXCEL EXPORT (styled, multi-sheet)
## ============================================================================

build_haul_xlsx <- function(start_date, end_date, period_label, file) {
  if (!requireNamespace("openxlsx", quietly = TRUE))
    stop("Install openxlsx: install.packages('openxlsx')")

  sd <- as.Date(start_date); ed <- as.Date(end_date)
  d  <- haul_summary(sd, ed)
  trips <- d$trips; expenses <- d$expenses

  wb <- openxlsx::createWorkbook(creator = "KifayatHaul",
                                 title = "KifayatHaul Report")
  openxlsx::modifyBaseFont(wb, fontSize = 11, fontName = "Inter")

  ## ---- Styles ------------------------------------------------------------
  bronze    <- "#B8862B";  emerald  <- "#1F4D2C";  cream <- "#F4ECDB"
  ink       <- "#2A2A28";  stone    <- "#8A8780";  linen <- "#E8E6E1"

  st_title  <- openxlsx::createStyle(fontSize = 22, fontColour = ink,
                                     fontName = "Inter", textDecoration = "bold")
  st_eyebrow <- openxlsx::createStyle(fontSize = 9, fontColour = bronze,
                                      fontName = "Inter", textDecoration = "bold")
  st_sub    <- openxlsx::createStyle(fontSize = 11, fontColour = stone,
                                     fontName = "Inter")
  st_header <- openxlsx::createStyle(
    fontSize = 9, fontColour = "#FFFFFF", fgFill = emerald, halign = "left",
    valign = "center", textDecoration = "bold", border = "TopBottom",
    borderColour = emerald, wrapText = TRUE
  )
  st_header_num <- openxlsx::createStyle(
    fontSize = 9, fontColour = "#FFFFFF", fgFill = emerald, halign = "right",
    valign = "center", textDecoration = "bold", border = "TopBottom",
    borderColour = emerald
  )
  st_band    <- openxlsx::createStyle(fgFill = "#FBFAF6")
  st_num     <- openxlsx::createStyle(halign = "right", numFmt = "#,##0",
                                      fontName = "JetBrains Mono", fontSize = 10)
  st_naira   <- openxlsx::createStyle(halign = "right",
                                      numFmt = "\u20A6#,##0",
                                      fontName = "JetBrains Mono", fontSize = 10)
  st_litres  <- openxlsx::createStyle(halign = "right",
                                      numFmt = "#,##0.0\" L\"",
                                      fontName = "JetBrains Mono", fontSize = 10)
  st_date    <- openxlsx::createStyle(halign = "left", numFmt = "dd mmm yyyy")
  st_text    <- openxlsx::createStyle(halign = "left", fontSize = 10)
  st_kpi_lbl <- openxlsx::createStyle(fontSize = 9, fontColour = stone,
                                      textDecoration = "bold", halign = "left",
                                      valign = "center")
  st_kpi_val <- openxlsx::createStyle(fontSize = 16, fontColour = ink,
                                      textDecoration = "bold", halign = "left",
                                      valign = "center",
                                      fontName = "JetBrains Mono")
  st_section <- openxlsx::createStyle(fontSize = 11, fontColour = bronze,
                                      textDecoration = "bold")

  ## ---- Sheet 1: MASTER (joined view) -------------------------------------
  openxlsx::addWorksheet(wb, "Master", gridLines = FALSE, tabColour = bronze)
  openxlsx::showGridLines(wb, "Master", showGridLines = FALSE)

  ## Cover banner
  openxlsx::writeData(wb, "Master", "KIFAYATHAUL", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "Master", st_eyebrow, rows = 2, cols = 2)
  openxlsx::writeData(wb, "Master",
    sprintf("%s report \u00B7 %s \u2013 %s", period_label,
            format(sd, "%d %b"), format(ed, "%d %b %Y")),
    startCol = 2, startRow = 3)
  openxlsx::addStyle(wb, "Master", st_title, rows = 3, cols = 2)
  openxlsx::writeData(wb, "Master",
    "Master view \u2014 joined trips and expenses for the period",
    startCol = 2, startRow = 4)
  openxlsx::addStyle(wb, "Master", st_sub, rows = 4, cols = 2)

  ## KPI strip
  total_l    <- sum(trips$litres_consumed, na.rm = TRUE)
  total_feed <- sum(trips$feeding_naira,   na.rm = TRUE)
  total_exp  <- sum(expenses$amount_naira, na.rm = TRUE)
  total_cost <- total_feed + total_exp
  kpi_row <- 6
  kpis <- list(
    list("TRIPS",          nrow(trips), st_num),
    list("FUEL CONSUMED",  total_l,     st_litres),
    list("DRIVER FEEDING", total_feed,  st_naira),
    list("TRUCK EXPENSES", total_exp,   st_naira),
    list("TOTAL COST",     total_cost,  st_naira)
  )
  for (i in seq_along(kpis)) {
    col <- 2 + (i - 1) * 2
    openxlsx::writeData(wb, "Master", kpis[[i]][[1]],
                        startCol = col, startRow = kpi_row)
    openxlsx::addStyle(wb, "Master", st_kpi_lbl, rows = kpi_row, cols = col)
    openxlsx::writeData(wb, "Master", kpis[[i]][[2]],
                        startCol = col, startRow = kpi_row + 1)
    openxlsx::addStyle(wb, "Master", kpis[[i]][[3]],
                       rows = kpi_row + 1, cols = col)
    openxlsx::addStyle(wb, "Master", st_kpi_val,
                       rows = kpi_row + 1, cols = col, stack = TRUE)
  }
  openxlsx::setRowHeights(wb, "Master", rows = kpi_row + 1, heights = 28)

  ## Joined master table
  master <- if (nrow(trips) > 0 || nrow(expenses) > 0) {
    trips_v <- if (nrow(trips) > 0) {
      data.frame(
        Date         = as.Date(trips$trip_date),
        Type         = "Trip",
        Truck        = trips$plate_number,
        Driver       = trips$driver_name,
        Origin       = trips$origin,
        Destination  = trips$destination,
        Category     = NA_character_,
        Litres       = trips$litres_consumed,
        `Amount (Naira)` = trips$feeding_naira,
        Note         = trips$purpose_note,
        check.names  = FALSE, stringsAsFactors = FALSE
      )
    } else NULL
    exp_v <- if (nrow(expenses) > 0) {
      data.frame(
        Date         = as.Date(expenses$expense_date),
        Type         = ifelse(expenses$date_estimated,
                              "Expense (date est.)", "Expense"),
        Truck        = expenses$plate_number,
        Driver       = expenses$driver_name,
        Origin       = NA_character_,
        Destination  = NA_character_,
        Category     = expenses$category,
        Litres       = NA_real_,
        `Amount (Naira)` = expenses$amount_naira,
        Note         = expenses$note,
        check.names  = FALSE, stringsAsFactors = FALSE
      )
    } else NULL
    out <- rbind(trips_v, exp_v)
    out <- out[order(out$Date, decreasing = TRUE), , drop = FALSE]
    out
  } else data.frame(Info = character(0))

  table_start <- kpi_row + 3
  openxlsx::writeData(wb, "Master", "Joined records",
                      startCol = 2, startRow = table_start - 1)
  openxlsx::addStyle(wb, "Master", st_section,
                     rows = table_start - 1, cols = 2)

  openxlsx::writeData(wb, "Master", master,
                      startCol = 2, startRow = table_start, headerStyle = st_header)
  if (nrow(master) > 0) {
    n_cols <- ncol(master)
    last_row <- table_start + nrow(master)
    ## Apply column styles
    openxlsx::addStyle(wb, "Master", st_date,
                       rows = (table_start + 1):last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, "Master", st_text,
                       rows = (table_start + 1):last_row, cols = 3:8, gridExpand = TRUE)
    openxlsx::addStyle(wb, "Master", st_litres,
                       rows = (table_start + 1):last_row, cols = 2 + 7,
                       gridExpand = TRUE)
    openxlsx::addStyle(wb, "Master", st_naira,
                       rows = (table_start + 1):last_row, cols = 2 + 8,
                       gridExpand = TRUE)
    openxlsx::addStyle(wb, "Master", st_text,
                       rows = (table_start + 1):last_row, cols = 2 + 9,
                       gridExpand = TRUE)
    ## Banded rows
    band_rows <- seq(table_start + 2, last_row, by = 2)
    if (length(band_rows) > 0)
      openxlsx::addStyle(wb, "Master", st_band,
                         rows = band_rows, cols = 2:(1 + n_cols),
                         gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, "Master", firstActiveRow = table_start + 1)
  }
  openxlsx::setColWidths(wb, "Master",
    cols = 1:12,
    widths = c(2, 13, 17, 12, 14, 14, 14, 26, 11, 14, 30, 2))

  ## ---- Sheet 2: TRIPS ---------------------------------------------------
  openxlsx::addWorksheet(wb, "Trips", gridLines = FALSE, tabColour = emerald)
  openxlsx::showGridLines(wb, "Trips", showGridLines = FALSE)
  openxlsx::writeData(wb, "Trips", "Trips", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "Trips", st_title, rows = 2, cols = 2)
  openxlsx::writeData(wb, "Trips", sprintf("%d trip(s) in period", nrow(trips)),
                      startCol = 2, startRow = 3)
  openxlsx::addStyle(wb, "Trips", st_sub, rows = 3, cols = 2)

  trips_tbl <- if (nrow(trips) > 0) {
    data.frame(
      Date            = as.Date(trips$trip_date),
      Truck           = trips$plate_number,
      Driver          = trips$driver_name,
      Origin          = trips$origin,
      Destination     = trips$destination,
      `Litres`        = trips$litres_consumed,
      `Feeding (Naira)`     = trips$feeding_naira,
      `Cargo`         = trips$cargo_product,
      `Cargo qty`     = trips$cargo_quantity,
      `Loading (Naira)`     = trips$loading_cost_naira,
      `E-payment (Naira)`   = trips$e_payment_naira,
      Note            = trips$purpose_note,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame(Info = "No trips in range.")
  openxlsx::writeData(wb, "Trips", trips_tbl, startCol = 2, startRow = 5,
                      headerStyle = st_header)
  if (nrow(trips_tbl) > 0 && ncol(trips_tbl) > 1) {
    last_row <- 5 + nrow(trips_tbl)
    openxlsx::addStyle(wb, "Trips", st_date,
                       rows = 6:last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, "Trips", st_text,
                       rows = 6:last_row, cols = c(3,4,5,6,9,13), gridExpand = TRUE)
    openxlsx::addStyle(wb, "Trips", st_litres,
                       rows = 6:last_row, cols = 7, gridExpand = TRUE)
    openxlsx::addStyle(wb, "Trips", st_naira,
                       rows = 6:last_row, cols = c(8,11,12), gridExpand = TRUE)
    openxlsx::addStyle(wb, "Trips", st_num,
                       rows = 6:last_row, cols = 10, gridExpand = TRUE)
    band_rows <- seq(7, last_row, by = 2)
    if (length(band_rows) > 0)
      openxlsx::addStyle(wb, "Trips", st_band,
                         rows = band_rows, cols = 2:13,
                         gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, "Trips", firstActiveRow = 6)
  }
  openxlsx::setColWidths(wb, "Trips",
    cols = 1:14,
    widths = c(2, 13, 13, 12, 12, 14, 10, 14, 10, 11, 14, 14, 28, 2))

  ## ---- Sheet 3: EXPENSES ------------------------------------------------
  openxlsx::addWorksheet(wb, "Expenses", gridLines = FALSE, tabColour = emerald)
  openxlsx::showGridLines(wb, "Expenses", showGridLines = FALSE)
  openxlsx::writeData(wb, "Expenses", "Truck expenses", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "Expenses", st_title, rows = 2, cols = 2)
  openxlsx::writeData(wb, "Expenses",
    sprintf("%d expense entries in period", nrow(expenses)),
    startCol = 2, startRow = 3)
  openxlsx::addStyle(wb, "Expenses", st_sub, rows = 3, cols = 2)

  exp_tbl <- if (nrow(expenses) > 0) {
    data.frame(
      Date              = as.Date(expenses$expense_date),
      `Estimated`       = ifelse(expenses$date_estimated, "Yes", "No"),
      Truck             = expenses$plate_number,
      Driver            = expenses$driver_name,
      Category          = expenses$category,
      `Amount (Naira)` = expenses$amount_naira,
      Note              = expenses$note,
      check.names = FALSE, stringsAsFactors = FALSE
    )
  } else data.frame(Info = "No expenses in range.")
  openxlsx::writeData(wb, "Expenses", exp_tbl, startCol = 2, startRow = 5,
                      headerStyle = st_header)
  if (nrow(exp_tbl) > 0 && ncol(exp_tbl) > 1) {
    last_row <- 5 + nrow(exp_tbl)
    openxlsx::addStyle(wb, "Expenses", st_date,
                       rows = 6:last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, "Expenses", st_text,
                       rows = 6:last_row, cols = c(3,4,5,6,8), gridExpand = TRUE)
    openxlsx::addStyle(wb, "Expenses", st_naira,
                       rows = 6:last_row, cols = 7, gridExpand = TRUE)
    band_rows <- seq(7, last_row, by = 2)
    if (length(band_rows) > 0)
      openxlsx::addStyle(wb, "Expenses", st_band,
                         rows = band_rows, cols = 2:8,
                         gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, "Expenses", firstActiveRow = 6)
  }
  openxlsx::setColWidths(wb, "Expenses",
    cols = 1:9,
    widths = c(2, 13, 11, 13, 13, 28, 14, 32, 2))

  ## ---- Sheet 4: BY TRUCK -------------------------------------------------
  openxlsx::addWorksheet(wb, "By Truck", gridLines = FALSE)
  openxlsx::showGridLines(wb, "By Truck", showGridLines = FALSE)
  openxlsx::writeData(wb, "By Truck", "By truck", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "By Truck", st_title, rows = 2, cols = 2)

  by_truck <- if (nrow(trips) > 0) {
    feed_by_t <- trips %>%
      dplyr::group_by(plate_number) %>%
      dplyr::summarise(trips = dplyr::n(),
                       litres = sum(litres_consumed, na.rm = TRUE),
                       feeding = sum(feeding_naira, na.rm = TRUE),
                       .groups = "drop")
    exp_by_t <- if (nrow(expenses) > 0) {
      expenses %>% dplyr::filter(!is.na(plate_number)) %>%
        dplyr::group_by(plate_number) %>%
        dplyr::summarise(expenses = sum(amount_naira, na.rm = TRUE),
                         .groups = "drop")
    } else data.frame(plate_number = character(), expenses = numeric())
    out <- merge(feed_by_t, exp_by_t, by = "plate_number", all = TRUE)
    out$expenses[is.na(out$expenses)] <- 0
    out$total <- out$feeding + out$expenses
    out <- out[order(-out$total), ]
    data.frame(
      Truck = out$plate_number,
      Trips = as.integer(out$trips),
      `Litres` = out$litres,
      `Feeding (Naira)` = out$feeding,
      `Expenses (Naira)` = out$expenses,
      `Total (Naira)` = out$total,
      check.names = FALSE
    )
  } else data.frame(Info = "No trips in range.")
  openxlsx::writeData(wb, "By Truck", by_truck, startCol = 2, startRow = 4,
                      headerStyle = st_header)
  if (nrow(by_truck) > 0 && ncol(by_truck) > 1) {
    last_row <- 4 + nrow(by_truck)
    openxlsx::addStyle(wb, "By Truck", st_text,
                       rows = 5:last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Truck", st_num,
                       rows = 5:last_row, cols = 3, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Truck", st_litres,
                       rows = 5:last_row, cols = 4, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Truck", st_naira,
                       rows = 5:last_row, cols = 5:7, gridExpand = TRUE)
    band_rows <- seq(6, last_row, by = 2)
    if (length(band_rows) > 0)
      openxlsx::addStyle(wb, "By Truck", st_band,
                         rows = band_rows, cols = 2:7,
                         gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, "By Truck", firstActiveRow = 5)
  }
  openxlsx::setColWidths(wb, "By Truck",
    cols = 1:8, widths = c(2, 14, 10, 14, 16, 16, 16, 2))

  ## ---- Sheet 5: BY DRIVER -----------------------------------------------
  openxlsx::addWorksheet(wb, "By Driver", gridLines = FALSE)
  openxlsx::showGridLines(wb, "By Driver", showGridLines = FALSE)
  openxlsx::writeData(wb, "By Driver", "By driver", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "By Driver", st_title, rows = 2, cols = 2)

  by_driver <- if (nrow(trips) > 0) {
    out <- trips %>%
      dplyr::group_by(driver_name) %>%
      dplyr::summarise(trips = dplyr::n(),
                       feeding = sum(feeding_naira, na.rm = TRUE),
                       avg_feed = mean(feeding_naira, na.rm = TRUE),
                       routes = dplyr::n_distinct(paste(origin, destination)),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(feeding))
    data.frame(
      Driver = out$driver_name,
      Trips  = as.integer(out$trips),
      `Feeding (Naira)`     = out$feeding,
      `Avg / trip (Naira)`  = out$avg_feed,
      `Routes covered`      = as.integer(out$routes),
      check.names = FALSE
    )
  } else data.frame(Info = "No trips in range.")
  openxlsx::writeData(wb, "By Driver", by_driver, startCol = 2, startRow = 4,
                      headerStyle = st_header)
  if (nrow(by_driver) > 0 && ncol(by_driver) > 1) {
    last_row <- 4 + nrow(by_driver)
    openxlsx::addStyle(wb, "By Driver", st_text,
                       rows = 5:last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Driver", st_num,
                       rows = 5:last_row, cols = c(3, 6), gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Driver", st_naira,
                       rows = 5:last_row, cols = 4:5, gridExpand = TRUE)
    band_rows <- seq(6, last_row, by = 2)
    if (length(band_rows) > 0)
      openxlsx::addStyle(wb, "By Driver", st_band,
                         rows = band_rows, cols = 2:6,
                         gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, "By Driver", firstActiveRow = 5)
  }
  openxlsx::setColWidths(wb, "By Driver",
    cols = 1:7, widths = c(2, 16, 10, 16, 18, 16, 2))

  ## ---- Sheet 6: BY ROUTE -------------------------------------------------
  openxlsx::addWorksheet(wb, "By Route", gridLines = FALSE)
  openxlsx::showGridLines(wb, "By Route", showGridLines = FALSE)
  openxlsx::writeData(wb, "By Route", "By route", startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "By Route", st_title, rows = 2, cols = 2)

  by_route <- if (nrow(trips) > 0) {
    out <- trips %>%
      dplyr::mutate(route = paste(origin, "\u2192", destination)) %>%
      dplyr::group_by(route) %>%
      dplyr::summarise(trips = dplyr::n(),
                       total_l = sum(litres_consumed, na.rm = TRUE),
                       avg_l   = mean(litres_consumed, na.rm = TRUE),
                       total_feed = sum(feeding_naira, na.rm = TRUE),
                       avg_feed   = mean(feeding_naira, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(trips))
    data.frame(
      Route = out$route,
      Trips = as.integer(out$trips),
      `Total litres` = out$total_l,
      `Avg litres`   = out$avg_l,
      `Total feeding (Naira)` = out$total_feed,
      `Avg feeding (Naira)`   = out$avg_feed,
      check.names = FALSE
    )
  } else data.frame(Info = "No trips in range.")
  openxlsx::writeData(wb, "By Route", by_route, startCol = 2, startRow = 4,
                      headerStyle = st_header)
  if (nrow(by_route) > 0 && ncol(by_route) > 1) {
    last_row <- 4 + nrow(by_route)
    openxlsx::addStyle(wb, "By Route", st_text,
                       rows = 5:last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Route", st_num,
                       rows = 5:last_row, cols = 3, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Route", st_litres,
                       rows = 5:last_row, cols = 4:5, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Route", st_naira,
                       rows = 5:last_row, cols = 6:7, gridExpand = TRUE)
    band_rows <- seq(6, last_row, by = 2)
    if (length(band_rows) > 0)
      openxlsx::addStyle(wb, "By Route", st_band,
                         rows = band_rows, cols = 2:7,
                         gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, "By Route", firstActiveRow = 5)
  }
  openxlsx::setColWidths(wb, "By Route",
    cols = 1:8, widths = c(2, 22, 10, 14, 14, 18, 18, 2))

  ## ---- Sheet 7: BY CATEGORY ----------------------------------------------
  openxlsx::addWorksheet(wb, "By Category", gridLines = FALSE)
  openxlsx::showGridLines(wb, "By Category", showGridLines = FALSE)
  openxlsx::writeData(wb, "By Category", "Expenses by category",
                      startCol = 2, startRow = 2)
  openxlsx::addStyle(wb, "By Category", st_title, rows = 2, cols = 2)

  by_cat <- if (nrow(expenses) > 0) {
    out <- expenses %>%
      dplyr::group_by(category) %>%
      dplyr::summarise(count = dplyr::n(),
                       total = sum(amount_naira, na.rm = TRUE),
                       .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(total))
    data.frame(
      Category = out$category,
      Entries = as.integer(out$count),
      `Amount (Naira)` = out$total,
      check.names = FALSE
    )
  } else data.frame(Info = "No expenses.")
  openxlsx::writeData(wb, "By Category", by_cat, startCol = 2, startRow = 4,
                      headerStyle = st_header)
  if (nrow(by_cat) > 0 && ncol(by_cat) > 1) {
    last_row <- 4 + nrow(by_cat)
    openxlsx::addStyle(wb, "By Category", st_text,
                       rows = 5:last_row, cols = 2, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Category", st_num,
                       rows = 5:last_row, cols = 3, gridExpand = TRUE)
    openxlsx::addStyle(wb, "By Category", st_naira,
                       rows = 5:last_row, cols = 4, gridExpand = TRUE)
    band_rows <- seq(6, last_row, by = 2)
    if (length(band_rows) > 0)
      openxlsx::addStyle(wb, "By Category", st_band,
                         rows = band_rows, cols = 2:4,
                         gridExpand = TRUE, stack = TRUE)
    openxlsx::freezePane(wb, "By Category", firstActiveRow = 5)
  }
  openxlsx::setColWidths(wb, "By Category",
    cols = 1:5, widths = c(2, 36, 10, 18, 2))

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)
}

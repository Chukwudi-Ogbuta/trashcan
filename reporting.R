## ============================================================================
## R/reporting.R — Report generation engine
##
## Single entrypoint: generate_report(period, start_date, end_date)
## Returns a list with:
##   $html_path   - rendered HTML file path
##   $pdf_path    - rendered PDF file path (if pagedown/chromote available)
##   $summary    - structured data used (also returned for emails)
##
## Reports are intentionally inline HTML using our brand stylesheet so they
## render perfectly in Gmail and look identical to the app UI.
## ============================================================================

suppressPackageStartupMessages({
  library(dplyr)
  library(lubridate)
  library(scales)
})

## ---------- DATA AGGREGATION ------------------------------------------------

report_data <- function(start_date, end_date) {
  start_date <- as.Date(start_date)
  end_date   <- as.Date(end_date)
  sd <- as.character(start_date); ed <- as.character(end_date)

  ## All reports in range
  reports <- db_get(sprintf(
    "select dr.id, dr.report_date, dr.block_number, s.name as station,
            s.id as station_id
       from daily_reports dr
       join stations s on s.id = dr.station_id
      where dr.report_date between '%s' and '%s';", sd, ed))

  ## All payments aggregated per report+tank+product
  pay <- db_get(sprintf(
    "select dr.report_date, s.name as station, p.code as product,
            sum(pmt.reported_revenue) as revenue,
            sum(pmt.pos_naira)        as pos,
            sum(pmt.cash_naira)       as cash
       from payments pmt
       join daily_reports dr on dr.id = pmt.report_id
       join stations s on s.id = dr.station_id
       join tanks t on t.id = pmt.tank_id
       join products p on p.id = t.product_id
      where dr.report_date between '%s' and '%s'
      group by dr.report_date, s.name, p.code;", sd, ed))

  ## Pump litres per product per station per day
  pump <- db_get(sprintf(
    "select dr.report_date, s.name as station, p.code as product,
            sum(ps.litres_sold) as litres
       from pump_sales ps
       join pumps pm on pm.id = ps.pump_id
       join tanks t on t.id = pm.tank_id
       join products p on p.id = t.product_id
       join daily_reports dr on dr.id = ps.report_id
       join stations s on s.id = dr.station_id
      where dr.report_date between '%s' and '%s'
      group by dr.report_date, s.name, p.code;", sd, ed))

  ## Deductions per station per day
  ded <- db_get(sprintf(
    "select dr.report_date, s.name as station, p.code as product,
            sum(d.litres) as deduction_litres
       from deductions d
       join daily_reports dr on dr.id = d.report_id
       join stations s on s.id = dr.station_id
       join tanks t on t.id = d.tank_id
       join products p on p.id = t.product_id
      where dr.report_date between '%s' and '%s'
      group by dr.report_date, s.name, p.code;", sd, ed))

  ## Debtor exposure
  debtor_summary <- db_get(sprintf(
    "select coalesce(deb.name, d.label, '(unnamed)') as debtor,
            sum(d.litres) as litres
       from deductions d
       left join debtors deb on deb.id = d.debtor_id
       join daily_reports dr on dr.id = d.report_id
      where dr.report_date between '%s' and '%s'
        and (d.recipient_type = 'debtor' or d.debtor_id is not null)
      group by coalesce(deb.name, d.label, '(unnamed)')
      order by litres desc;", sd, ed))

  ## Expenses by category
  expenses <- db_get(sprintf(
    "select e.category, sum(e.amount_naira) as amount
       from expenses e
       join daily_reports dr on dr.id = e.report_id
      where dr.report_date between '%s' and '%s'
      group by e.category
      order by amount desc;", sd, ed))

  ## Open issues
  issues <- db_get(sprintf(
    "select i.issue_type, i.severity, i.note, s.name as station, dr.report_date
       from issues i
       join daily_reports dr on dr.id = i.report_id
       join stations s on s.id = dr.station_id
      where dr.report_date between '%s' and '%s'
        and i.status = 'open'
      order by case i.severity when 'high' then 1 when 'medium' then 2 else 3 end,
               dr.report_date desc;", sd, ed))

  ## Generator fuel usage per station (litres) — PMS only
  generator <- db_get(sprintf(
    "select s.name as station, sum(d.litres) as litres
       from deductions d
       join daily_reports dr on dr.id = d.report_id
       join stations s on s.id = dr.station_id
       join tanks t on t.id = d.tank_id
       join products p on p.id = t.product_id
      where dr.report_date between '%s' and '%s'
        and d.recipient_type = 'generator'
        and p.code = 'PMS'
      group by s.name
      order by litres desc;", sd, ed))

  ## Prevailing PMS price per station (weighted by days each price is active)
  gen_prices <- db_get(sprintf(
    "with bounds as (
       select s.id as station_id, s.name as station,
              pr.price_per_litre,
              greatest(pr.effective_from, date '%s') as eff_from,
              least(coalesce(pr.effective_to, date '%s'), date '%s') as eff_to
         from prices pr
         join stations s on s.id = pr.station_id
         join products p on p.id = pr.product_id
        where p.code = 'PMS'
          and pr.effective_from <= date '%s'
          and (pr.effective_to is null or pr.effective_to >= date '%s')
     )
     select station,
            sum(price_per_litre * (eff_to - eff_from + 1))::numeric
              / nullif(sum(eff_to - eff_from + 1), 0) as price
       from bounds
       where eff_to >= eff_from
       group by station;",
    sd, ed, ed, ed, sd))

  if (nrow(generator) > 0 && nrow(gen_prices) > 0) {
    generator <- merge(generator, gen_prices, by = "station", all.x = TRUE)
    generator$value_naira <- generator$litres * generator$price
    generator <- generator[order(-generator$value_naira), ]
  } else if (nrow(generator) > 0) {
    generator$price <- 0
    generator$value_naira <- 0
  }

  list(
    reports        = reports,
    payments       = pay,
    pump_litres    = pump,
    deductions     = ded,
    debtor_summary = debtor_summary,
    expenses       = expenses,
    issues         = issues,
    generator      = generator,
    range = list(start = start_date, end = end_date)
  )
}

## ---------- FORMATTING HELPERS ---------------------------------------------

naira <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("\u20A60")
  paste0("\u20A6", format(round(as.numeric(x)), big.mark = ",", scientific = FALSE))
}
litres <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("0 L")
  paste0(format(round(as.numeric(x), 1), big.mark = ",", scientific = FALSE), " L")
}
pct <- function(x) {
  if (is.null(x) || length(x) == 0 || is.na(x)) return("0%")
  paste0(round(100 * x), "%")
}

## ---------- RENDER ---------------------------------------------------------

generate_report <- function(period_label, start_date, end_date,
                            out_dir = "reports") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  start_date <- as.Date(start_date); end_date <- as.Date(end_date)

  d <- report_data(start_date, end_date)

  total_revenue <- sum(d$payments$revenue, na.rm = TRUE)
  total_pos     <- sum(d$payments$pos,     na.rm = TRUE)
  total_cash    <- sum(d$payments$cash,    na.rm = TRUE)
  total_litres  <- sum(d$pump_litres$litres, na.rm = TRUE)
  pms_litres    <- sum(d$pump_litres$litres[d$pump_litres$product == "PMS"], na.rm = TRUE)
  ago_litres    <- sum(d$pump_litres$litres[d$pump_litres$product == "AGO"], na.rm = TRUE)
  pms_share     <- if (total_litres > 0) pms_litres / total_litres else 0
  total_exp     <- sum(d$expenses$amount, na.rm = TRUE)
  n_open        <- nrow(d$issues)
  n_high        <- sum(d$issues$severity == "high")

  ## Station table
  pay_by_station <- d$payments %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(revenue = sum(revenue, na.rm = TRUE),
                     pos     = sum(pos,     na.rm = TRUE),
                     cash    = sum(cash,    na.rm = TRUE), .groups = "drop")
  pump_by_station <- d$pump_litres %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(litres = sum(litres, na.rm = TRUE), .groups = "drop")
  exp_by_station <- d$payments %>% # we don't track expenses by station yet beyond report; skipped
    dplyr::group_by(station) %>%
    dplyr::summarise(n = 0, .groups = "drop") %>% dplyr::select(-n)

  stations_tbl <- pay_by_station %>%
    dplyr::full_join(pump_by_station, by = "station") %>%
    dplyr::mutate(
      pos_pct = ifelse(revenue > 0, pos / revenue, 0),
      revenue = tidyr::replace_na(revenue, 0),
      litres  = tidyr::replace_na(litres, 0)
    ) %>%
    dplyr::arrange(dplyr::desc(revenue))

  ## Daily revenue trend
  trend <- d$payments %>%
    dplyr::group_by(report_date) %>%
    dplyr::summarise(revenue = sum(revenue, na.rm = TRUE), .groups = "drop") %>%
    dplyr::arrange(report_date)

  ## ---------- Build HTML --------------------------------------------------
  css <- '
  <style>
    body{margin:0;padding:0;background:#F5F4ED;font-family:Inter,system-ui,-apple-system,sans-serif;color:#2A2A28;-webkit-font-smoothing:antialiased;}
    .wrap{max-width:760px;margin:0 auto;padding:48px 36px;}
    .cover{display:flex;justify-content:space-between;align-items:flex-end;padding-bottom:24px;border-bottom:0.5px solid #E8E6E1;}
    .brand{display:flex;align-items:center;gap:12px;}
    .brand-mark{width:36px;height:36px;background:#1F4D2C;color:#F4ECDB;border-radius:8px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:18px;}
    .brand-name{font-size:16px;font-weight:500;letter-spacing:0.01em;}
    .brand-sub{font-size:10px;color:#8A8780;letter-spacing:0.14em;text-transform:uppercase;margin-top:2px;}
    .cover-right{text-align:right;}
    .cover-right .eyebrow{font-size:10px;color:#1F4D2C;letter-spacing:0.14em;text-transform:uppercase;font-weight:500;}
    .cover-right .period{font-size:22px;font-weight:500;margin-top:4px;letter-spacing:-0.015em;}
    .cover-right .range{font-size:12px;color:#8A8780;margin-top:2px;}
    h2{font-size:13px;font-weight:500;color:#1F4D2C;letter-spacing:0.12em;text-transform:uppercase;margin:36px 0 14px;}
    .summary{font-size:14px;line-height:1.7;color:#2A2A28;margin-top:14px;}
    .kpi-grid{display:grid;grid-template-columns:repeat(3,1fr);gap:12px;margin-top:12px;}
    .kpi{background:#FFFFFF;border:0.5px solid #E8E6E1;border-radius:10px;padding:16px 18px;position:relative;overflow:hidden;}
    .kpi::before{content:"";position:absolute;left:0;top:0;bottom:0;width:3px;background:#1F4D2C;}
    .kpi-label{font-size:9.5px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:8px;}
    .kpi-value{font-size:20px;font-weight:500;letter-spacing:-0.01em;line-height:1;font-variant-numeric:tabular-nums;}
    .kpi-foot{font-size:11px;color:#8A8780;margin-top:6px;}
    .kpi.issue::before{background:#A8323A;} .kpi.issue .kpi-value{color:#A8323A;}
    .kpi.warn::before{background:#B8862B;}
    table{width:100%;border-collapse:collapse;margin-top:6px;font-size:12.5px;font-variant-numeric:tabular-nums;}
    th{font-size:10px;color:#5C5A52;font-weight:500;letter-spacing:0.06em;text-transform:uppercase;text-align:left;padding:8px 10px;border-bottom:0.5px solid #E8E6E1;}
    td{padding:9px 10px;border-bottom:0.5px solid #F2F0EB;}
    td.num,th.num{text-align:right;font-variant-numeric:tabular-nums;}
    tr:last-child td{border-bottom:none;}
    .bar-wrap{background:#F2F0EB;height:6px;border-radius:3px;overflow:hidden;}
    .bar-fill{height:100%;background:#1F4D2C;}
    .split{display:grid;grid-template-columns:1fr 1fr;gap:14px;margin-top:6px;}
    .pill{display:inline-block;padding:3px 9px;border-radius:5px;font-size:10px;font-weight:500;letter-spacing:0.02em;}
    .pill.ok{background:#EEF2EA;color:#1F4D2C;}
    .pill.issue{background:#F5DDE0;color:#7A1F26;}
    .pill.warn{background:#F4ECDB;color:#7A5410;}
    .pill.pms{background:#F1F6EE;color:#1F4D2C;}
    .pill.ago{background:#FBF2DF;color:#7A5410;}
    .issue-row{padding:10px 0;border-bottom:0.5px solid #F2F0EB;}
    .issue-row:last-child{border-bottom:none;}
    .issue-meta{font-size:10.5px;color:#8A8780;letter-spacing:0.04em;margin-bottom:2px;}
    .issue-note{font-size:12.5px;color:#2A2A28;}
    .actions{background:#FFFFFF;border:0.5px solid #E8E6E1;border-radius:10px;padding:14px 18px;margin-top:8px;}
    .actions ul{margin:0;padding-left:18px;}
    .actions li{font-size:13px;line-height:1.6;color:#2A2A28;margin:4px 0;}
    .footer{margin-top:48px;padding-top:18px;border-top:0.5px solid #E8E6E1;font-size:10.5px;color:#8A8780;letter-spacing:0.06em;text-transform:uppercase;display:flex;justify-content:space-between;}
    .chart-wrap{background:#FFFFFF;border:0.5px solid #E8E6E1;border-radius:10px;padding:16px;margin-top:8px;}
    .trend-svg{width:100%;height:140px;}
    .donut-grid{display:grid;grid-template-columns:140px 1fr;gap:20px;align-items:center;}
    .donut-svg{width:140px;height:140px;}
    .donut-legend{font-size:13px;line-height:1.9;}
    .donut-legend .key{display:inline-block;width:10px;height:10px;border-radius:2px;margin-right:8px;vertical-align:middle;}
  </style>
  '

  ## ---- inline SVG mini-charts -------------------------------------------
  trend_svg <- ""
  if (nrow(trend) > 1) {
    xs <- as.numeric(trend$report_date - min(trend$report_date))
    if (max(xs) == 0) xs <- seq_along(xs) - 1
    ys <- trend$revenue
    W <- 700; H <- 230; P_L <- 70; P_R <- 16; P_T <- 14; P_B <- 36
    max_y <- max(ys, 1)
    x_scale <- function(x) P_L + (W - P_L - P_R) * (x - min(xs)) /
                            max(1, (max(xs) - min(xs)))
    y_scale <- function(y) H - P_B - (H - P_T - P_B) * (y / max_y)

    pts  <- paste(mapply(function(x, y) sprintf("%.1f,%.1f", x_scale(x), y_scale(y)),
                         xs, ys), collapse = " ")
    area <- paste0(sprintf("%.1f,%.1f ", x_scale(min(xs)), H - P_B), pts,
                   sprintf(" %.1f,%.1f", x_scale(max(xs)), H - P_B))

    ## Y-axis: 4 evenly spaced gridlines with naira labels
    y_steps <- pretty(c(0, max_y), n = 4)
    y_lines <- vapply(y_steps, function(v) sprintf(
      '<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#F2F0EB" stroke-width="0.5"/>
       <text x="%d" y="%.1f" font-size="10" fill="#8A8780" font-family="Inter" text-anchor="end">\u20A6%s</text>',
      P_L, y_scale(v), W - P_R, y_scale(v),
      P_L - 8, y_scale(v) + 3,
      if (v >= 1e6) sprintf("%.1fM", v / 1e6)
      else if (v >= 1e3) sprintf("%.0fk", v / 1e3)
      else format(round(v), big.mark = ",")), character(1))

    ## X-axis date labels (sparse if many)
    lbl_idx <- if (length(xs) > 8) round(seq(1, length(xs), length.out = 6))
               else seq_along(xs)
    x_labs <- vapply(lbl_idx, function(i) sprintf(
      '<text x="%.1f" y="%d" text-anchor="middle" font-size="10" fill="#8A8780" font-family="Inter">%s</text>',
      x_scale(xs[i]), H - 12, format(trend$report_date[i], "%d %b")), character(1))

    ## Data points with tooltips
    dots <- paste(mapply(function(x, y, r, d) sprintf(
      '<circle cx="%.1f" cy="%.1f" r="3.5" fill="#1F4D2C" stroke="#FBFAF6" stroke-width="1.5"><title>%s \u00B7 \u20A6%s</title></circle>',
      x_scale(x), y_scale(y), r, format(d, "%d %b %Y"),
      format(round(r), big.mark = ",")),
      xs, ys, ys, trend$report_date), collapse = "")

    trend_svg <- sprintf('
      <div class="chart-wrap">
        <svg viewBox="0 0 %d %d" preserveAspectRatio="none" style="width:100%%;height:230px;">
          %s
          <polygon points="%s" fill="#1F4D2C" fill-opacity="0.10"/>
          <polyline points="%s" fill="none" stroke="#1F4D2C" stroke-width="1.8" stroke-linejoin="round" stroke-linecap="round"/>
          %s %s
        </svg>
      </div>',
      W, H, paste(y_lines, collapse = ""), area, pts, dots,
      paste(x_labs, collapse = ""))
  }

  ## Donut for PMS vs AGO
  donut_svg <- ""
  if (total_litres > 0) {
    r <- 50; cx <- 70; cy <- 70; circ <- 2 * pi * r
    pms_len <- circ * pms_share
    ago_len <- circ - pms_len
    donut_svg <- sprintf('
      <div class="chart-wrap">
        <div class="donut-grid">
          <svg class="donut-svg" viewBox="0 0 140 140">
            <circle cx="%d" cy="%d" r="%d" fill="none" stroke="#B8862B" stroke-width="16" />
            <circle cx="%d" cy="%d" r="%d" fill="none" stroke="#1F4D2C" stroke-width="16"
                    stroke-dasharray="%.2f %.2f" transform="rotate(-90 %d %d)" />
            <text x="%d" y="%d" text-anchor="middle" font-size="18" font-weight="500" fill="#2A2A28" font-family="Inter">%s</text>
            <text x="%d" y="%d" text-anchor="middle" font-size="9" fill="#8A8780" font-family="Inter" letter-spacing="0.1em">PMS</text>
          </svg>
          <div class="donut-legend">
            <div><span class="key" style="background:#1F4D2C;"></span> PMS &nbsp;%s &nbsp;<span style="color:#8A8780;">(%s)</span></div>
            <div><span class="key" style="background:#B8862B;"></span> AGO &nbsp;%s &nbsp;<span style="color:#8A8780;">(%s)</span></div>
          </div>
        </div>
      </div>',
      cx, cy, r, cx, cy, r, pms_len, ago_len, cx, cy,
      cx, cy + 4, pct(pms_share), cx, cy + 22,
      litres(pms_litres), pct(pms_share),
      litres(ago_litres), pct(1 - pms_share))
  }

  ## ---- station table HTML -----------------------------------------------
  station_rows <- if (nrow(stations_tbl) > 0) {
    max_rev <- max(stations_tbl$revenue, 1)
    paste0(vapply(seq_len(nrow(stations_tbl)), function(i) {
      r <- stations_tbl[i, ]
      sprintf('<tr><td><strong>%s</strong></td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td><td><div class="bar-wrap"><div class="bar-fill" style="width:%.1f%%"></div></div></td></tr>',
              r$station, litres(r$litres), naira(r$revenue), pct(r$pos_pct),
              100 * r$revenue / max_rev)
    }, character(1)), collapse = "")
  } else "<tr><td colspan='5' style='color:#8A8780;'>No station activity in this period.</td></tr>"

  ## ---- debtor list ------------------------------------------------------
  debtor_rows <- if (nrow(d$debtor_summary) > 0) {
    top <- d$debtor_summary %>% dplyr::slice(seq_len(min(5, dplyr::n())))
    paste0(vapply(seq_len(nrow(top)), function(i) {
      sprintf('<tr><td><strong>%s</strong></td><td class="num">%s</td></tr>',
              top$debtor[i], litres(top$litres[i]))
    }, character(1)), collapse = "")
  } else "<tr><td colspan='2' style='color:#8A8780;'>No debtor fuel issued in this period.</td></tr>"

  total_debtor_litres <- sum(d$debtor_summary$litres, na.rm = TRUE)

  ## ---- expenses: top 10 + lumped 'All other' ---------------------------
  expense_rows <- if (nrow(d$expenses) > 0) {
    exp <- d$expenses
    if (nrow(exp) > 10) {
      top <- exp[1:10, ]
      other_total <- sum(exp$amount[11:nrow(exp)], na.rm = TRUE)
      n_other <- nrow(exp) - 10
      top_rows <- paste0(vapply(seq_len(nrow(top)), function(i) {
        sprintf('<tr><td>%s</td><td class="num">%s</td></tr>',
                top$category[i], naira(top$amount[i]))
      }, character(1)), collapse = "")
      paste0(top_rows,
        sprintf('<tr><td style="color:#8A8780;font-style:italic;">All other (%d items)</td><td class="num" style="color:#8A8780;">%s</td></tr>',
                n_other, naira(other_total)))
    } else {
      paste0(vapply(seq_len(nrow(exp)), function(i) {
        sprintf('<tr><td>%s</td><td class="num">%s</td></tr>',
                exp$category[i], naira(exp$amount[i]))
      }, character(1)), collapse = "")
    }
  } else "<tr><td colspan='2' style='color:#8A8780;'>No expenses recorded.</td></tr>"

  ## ---- generator fuel (in-kind expense, PMS only) ----------------------
  gen_total_litres <- if (!is.null(d$generator) && nrow(d$generator) > 0)
    sum(d$generator$litres, na.rm = TRUE) else 0
  gen_total_value <- if (!is.null(d$generator) && nrow(d$generator) > 0)
    sum(d$generator$value_naira, na.rm = TRUE) else 0
  cash_expenses_total <- if (nrow(d$expenses) > 0) sum(d$expenses$amount, na.rm = TRUE) else 0
  combined_expenses <- cash_expenses_total + gen_total_value

  generator_html <- if (!is.null(d$generator) && nrow(d$generator) > 0) {
    rows <- paste(vapply(seq_len(nrow(d$generator)), function(i) sprintf(
      '<tr><td><strong>%s</strong></td><td class="num">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
      d$generator$station[i],
      litres(d$generator$litres[i]),
      naira(d$generator$price[i] %||% 0),
      naira(d$generator$value_naira[i])), character(1)), collapse = "")
    paste0(
      '<table><thead><tr><th>Station</th><th class="num">Litres</th><th class="num">Avg PMS price</th><th class="num">Est. value</th></tr></thead>',
      '<tbody>', rows, '</tbody></table>',
      sprintf('<p style="font-size:10.5px;color:#8A8780;margin-top:8px;font-style:italic;line-height:1.5;">Generator fuel is dispatched from station tanks (PMS) for in-house use, not cash sale. Litres valued at the prevailing PMS price. Total in-kind expense: <strong>%s</strong> (%s).</p>',
              naira(gen_total_value), litres(gen_total_litres)))
  } else '<div style="font-size:13px;color:#8A8780;padding:10px 0;">No generator fuel usage recorded in this period.</div>'

  expenses_summary_html <- sprintf(
    '<div style="display:grid;grid-template-columns:repeat(3,1fr);gap:10px;margin-top:10px;margin-bottom:14px;">
      <div style="background:#fff;border:0.5px solid #E8E6E1;border-radius:8px;padding:10px 12px;">
        <div style="font-size:9px;letter-spacing:0.10em;text-transform:uppercase;color:#8A8780;font-weight:500;margin-bottom:4px;">Cash expenses</div>
        <div style="font-size:14px;font-weight:500;font-variant-numeric:tabular-nums;">%s</div></div>
      <div style="background:#fff;border:0.5px solid #E8E6E1;border-radius:8px;padding:10px 12px;border-left:3px solid #B8862B;">
        <div style="font-size:9px;letter-spacing:0.10em;text-transform:uppercase;color:#8A8780;font-weight:500;margin-bottom:4px;">Generator fuel (in-kind)</div>
        <div style="font-size:14px;font-weight:500;font-variant-numeric:tabular-nums;">%s</div>
        <div style="font-size:10px;color:#8A8780;margin-top:2px;">%s</div></div>
      <div style="background:#fff;border:0.5px solid #E8E6E1;border-radius:8px;padding:10px 12px;border-left:3px solid #1F4D2C;">
        <div style="font-size:9px;letter-spacing:0.10em;text-transform:uppercase;color:#8A8780;font-weight:500;margin-bottom:4px;">Combined expenses</div>
        <div style="font-size:14px;font-weight:500;font-variant-numeric:tabular-nums;">%s</div></div>
    </div>',
    naira(cash_expenses_total),
    naira(gen_total_value), litres(gen_total_litres),
    naira(combined_expenses))

  ## ---- issues -----------------------------------------------------------
  issue_html <- if (nrow(d$issues) > 0) {
    top_issues <- d$issues %>% dplyr::slice(seq_len(min(8, dplyr::n())))
    paste0(vapply(seq_len(nrow(top_issues)), function(i) {
      sev <- top_issues$severity[i]
      pill <- if (sev == "high") "issue" else if (sev == "medium") "warn" else "ok"
      sprintf('<div class="issue-row"><div class="issue-meta"><span class="pill %s">%s</span> &nbsp; %s &nbsp;\u00B7&nbsp; %s &nbsp;\u00B7&nbsp; %s</div><div class="issue-note">%s</div></div>',
              pill, toupper(sev), top_issues$station[i],
              format(top_issues$report_date[i], "%d %b"),
              top_issues$issue_type[i], top_issues$note[i])
    }, character(1)), collapse = "")
  } else '<div style="font-size:13px;color:#8A8780;padding:10px 0;">No open issues. All reconciled.</div>'

  ## ---- missed station-days ---------------------------------------------
  ms <- tryCatch(missed_sales(start_date, end_date),
                 error = function(e) list(table = data.frame(), total_revenue = 0))
  missed_html <- if (is.data.frame(ms$table) && nrow(ms$table) > 0) {
    rows <- paste(vapply(seq_len(nrow(ms$table)), function(i) sprintf(
      '<tr><td><strong>%s</strong></td><td class="num">%d</td><td style="font-size:11px;color:#5C5A52;">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
      ms$table$station[i], as.integer(ms$table$missed_days[i]),
      ms$table$dates[i] %||% "",
      litres(ms$table$est_lost_litres[i]),
      naira(ms$table$est_lost_revenue[i])), character(1)), collapse = "")
    paste0(
      '<table><thead><tr><th>Station</th><th class="num">Missed days</th><th>Dates</th><th class="num">Est. lost litres</th><th class="num">Est. lost revenue</th></tr></thead>',
      '<tbody>', rows, '</tbody></table>',
      sprintf('<p style="font-size:10.5px;color:#8A8780;margin-top:8px;font-style:italic;line-height:1.5;">Total estimated revenue missed: <strong>%s</strong>. Methodology: per missing station-day, applied the station\'s average daily litres for the period (separated by product) multiplied by the prevailing per-product price effective during the period.</p>',
              naira(ms$total_revenue)))
  } else '<div style="font-size:13px;color:#8A8780;padding:10px 0;">No missed station-days \u2014 every active station posted data each day in this period.</div>'

  ## ---- action items -----------------------------------------------------
  n_med <- sum(d$issues$severity == "medium")
  n_low <- sum(d$issues$severity == "low")

  actions <- character()
  if (n_high > 0) actions <- c(actions, sprintf("Investigate %d high-severity variance(s) flagged in Issues.", n_high))
  if (n_med > 0)  actions <- c(actions, sprintf("Review and resolve %d medium-severity item(s).", n_med))
  if (n_low > 0)  actions <- c(actions, sprintf("Address %d low-severity item(s) (mostly data hygiene).", n_low))
  if (total_debtor_litres > 0)
    actions <- c(actions, sprintf("Settle outstanding debtor exposure of %s with finance team.", litres(total_debtor_litres)))
  if (is.data.frame(ms$table) && nrow(ms$table) > 0)
    actions <- c(actions, sprintf("Investigate %d station-day(s) with no data (est. %s lost revenue).",
                                  sum(ms$table$missed_days), naira(ms$total_revenue)))
  if (length(actions) == 0) actions <- "No action items \u2014 network is reconciled and clean."

  action_html <- paste0("<ul>", paste0("<li>", actions, "</li>", collapse = ""), "</ul>")

  ## ---- assemble ---------------------------------------------------------
  summary_text <- sprintf(
    "The %s ending %s closed with <strong>%s</strong> in revenue across %s of fuel sold (%s PMS, %s AGO). Net cash to bank stood at %s after expenses. %s",
    period_label, format(end_date, "%d %B %Y"),
    naira(total_revenue), litres(total_litres),
    pct(pms_share), pct(1 - pms_share),
    naira(total_cash - total_exp),
    if (n_open > 0)
      sprintf("There %s <strong>%d open issue%s</strong> requiring review (%d high severity).",
              if (n_open == 1) "is" else "are", n_open,
              if (n_open == 1) "" else "s", n_high)
    else "No open issues \u2014 the network is fully reconciled."
  )

  html <- paste0('<!DOCTYPE html><html><head><meta charset="utf-8">',
    '<title>KifayatOps Report</title>', css, '</head><body><div class="wrap">',

    '<div class="cover"><div class="brand">',
    '<div class="brand-mark">K</div>',
    '<div><div class="brand-name">KifayatOps</div>',
    '<div class="brand-sub">Daily Reconciliation</div></div></div>',
    '<div class="cover-right">',
    '<div class="eyebrow">', period_label, ' report</div>',
    '<div class="period">', format(end_date, "%d %B %Y"), '</div>',
    '<div class="range">', format(start_date, "%d %b"),
    if (start_date != end_date) paste0(" — ", format(end_date, "%d %b %Y")) else "",
    '</div></div></div>',

    '<h2>Executive summary</h2><div class="summary">', summary_text, '</div>',

    '<h2>Headline KPIs</h2>',
    '<div class="kpi-grid">',
    '<div class="kpi"><div class="kpi-label">Total revenue</div><div class="kpi-value">', naira(total_revenue), '</div></div>',
    '<div class="kpi"><div class="kpi-label">Total litres sold</div><div class="kpi-value">', litres(total_litres), '</div></div>',
    '<div class="kpi"><div class="kpi-label">PMS share</div><div class="kpi-value">', pct(pms_share), '</div></div>',
    '<div class="kpi"><div class="kpi-label">Cash to bank</div><div class="kpi-value">', naira(total_cash), '</div></div>',
    '<div class="kpi ', if (total_exp > 0) "warn" else "", '"><div class="kpi-label">Expenses</div><div class="kpi-value">', naira(total_exp), '</div></div>',
    '<div class="kpi ', if (n_open > 0) "issue" else "", '"><div class="kpi-label">Open issues</div><div class="kpi-value">', n_open, '</div></div>',
    '</div>',

    if (nzchar(trend_svg)) paste0('<h2>Revenue trend</h2>', trend_svg) else "",

    '<h2>Station performance</h2><table>',
    '<thead><tr><th>Station</th><th class="num">Litres</th><th class="num">Revenue</th><th class="num">POS %</th><th style="width:120px;">vs. peers</th></tr></thead>',
    '<tbody>', station_rows, '</tbody></table>',

    if (nzchar(donut_svg)) paste0('<h2>Product split</h2>', donut_svg) else "",

    '<div class="split">',
    '<div><h2>Top debtors (litres)</h2><table>',
    '<thead><tr><th>Debtor</th><th class="num">Litres</th></tr></thead>',
    '<tbody>', debtor_rows, '</tbody></table></div>',
    '<div><h2>Expenses by category</h2>',
    expenses_summary_html,
    '<table>',
    '<thead><tr><th>Category</th><th class="num">Amount</th></tr></thead>',
    '<tbody>', expense_rows, '</tbody></table></div></div>',

    '<h2>Generator fuel usage</h2>', generator_html,

    '<h2>Missed station-days</h2>', missed_html,

    '<h2>Open issues</h2>', issue_html,

    '<h2>Action items</h2><div class="actions">', action_html, '</div>',

    '<div class="footer"><div>Generated by KifayatOps \u00B7 DailyRecon</div>',
    '<div>', format(Sys.time(), "%d %b %Y \u00B7 %H:%M"), '</div></div>',

    '</div></body></html>'
  )

  fname_base <- sprintf("KifayatOps_%s_%s_to_%s",
                        gsub("[^A-Za-z0-9]", "", period_label),
                        format(start_date, "%Y%m%d"),
                        format(end_date,   "%Y%m%d"))
  html_path <- file.path(out_dir, paste0(fname_base, ".html"))
  writeLines(html, html_path, useBytes = TRUE)

  ## Render PDF via headless Chrome
  pdf_path <- file.path(out_dir, paste0(fname_base, ".pdf"))
  pdf_ok <- tryCatch({
    if (requireNamespace("pagedown", quietly = TRUE)) {
      pagedown::chrome_print(
        input  = normalizePath(html_path),
        output = normalizePath(pdf_path, mustWork = FALSE),
        format = "pdf",
        verbose = 0,
        wait = 1
      )
      file.exists(pdf_path)
    } else FALSE
  }, error = function(e) FALSE)

  list(
    html_path    = html_path,
    html_content = html,
    pdf_path     = if (isTRUE(pdf_ok)) pdf_path else NULL,
    summary = list(
      revenue = total_revenue, litres = total_litres,
      cash = total_cash, expenses = total_exp,
      open_issues = n_open, period = period_label,
      range_start = start_date, range_end = end_date
    )
  )
}

## ---------- EMAIL BODY (short, branded) ------------------------------------

build_email_body <- function(summary, recipient_name = NULL) {
  s <- summary
  greeting <- if (!is.null(recipient_name) && nzchar(recipient_name))
                paste0("Dear ", recipient_name, ",") else "Dear Sir,"

  range_str <- if (s$range_start == s$range_end)
                 format(s$range_end, "%d %B %Y")
               else
                 paste(format(s$range_start, "%d %b"),
                       format(s$range_end,   "%d %b %Y"), sep = " \u2013 ")

  sprintf('
  <html><body style="margin:0;padding:0;background:#F5F4ED;font-family:Inter,system-ui,-apple-system,sans-serif;color:#2A2A28;-webkit-font-smoothing:antialiased;">
    <div style="max-width:560px;margin:0 auto;padding:40px 28px;">

      <!-- Brand mark -->
      <div style="display:flex;align-items:center;gap:12px;padding-bottom:20px;border-bottom:0.5px solid #E8E6E1;">
        <div style="width:38px;height:38px;background:#1F4D2C;color:#F4ECDB;border-radius:8px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:18px;font-family:Inter;">K</div>
        <div>
          <div style="font-size:16px;font-weight:500;color:#2A2A28;letter-spacing:0.01em;">KifayatOps</div>
          <div style="font-size:10px;color:#8A8780;letter-spacing:0.14em;text-transform:uppercase;margin-top:2px;">Daily Reconciliation</div>
        </div>
      </div>

      <!-- Eyebrow -->
      <div style="margin-top:30px;">
        <div style="font-size:10px;color:#1F4D2C;letter-spacing:0.14em;text-transform:uppercase;font-weight:500;">%s report</div>
        <div style="font-size:22px;font-weight:500;margin-top:4px;letter-spacing:-0.015em;color:#2A2A28;">%s</div>
      </div>

      <!-- Greeting + body -->
      <div style="margin-top:24px;font-size:14px;line-height:1.7;color:#2A2A28;">
        <p style="margin:0 0 14px;">%s</p>
        <p style="margin:0 0 14px;">Please find attached the <strong>%s reconciliation report</strong> for the period <strong>%s</strong>, covering all active Kifayat Global Energy stations.</p>
        <p style="margin:0 0 14px;">A snapshot of the headlines:</p>
      </div>

      <!-- Mini KPI strip -->
      <table cellpadding="0" cellspacing="0" border="0" style="width:100%%;border-collapse:collapse;margin-top:8px;">
        <tr>
          <td style="background:#FFFFFF;border:0.5px solid #E8E6E1;border-radius:10px;padding:14px 16px;width:33%%;border-left:3px solid #1F4D2C;">
            <div style="font-size:9.5px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;">Revenue</div>
            <div style="font-size:17px;font-weight:500;color:#2A2A28;letter-spacing:-0.01em;font-variant-numeric:tabular-nums;">%s</div>
          </td>
          <td style="width:6px;"></td>
          <td style="background:#FFFFFF;border:0.5px solid #E8E6E1;border-radius:10px;padding:14px 16px;width:33%%;border-left:3px solid #1F4D2C;">
            <div style="font-size:9.5px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;">Litres sold</div>
            <div style="font-size:17px;font-weight:500;color:#2A2A28;letter-spacing:-0.01em;font-variant-numeric:tabular-nums;">%s</div>
          </td>
          <td style="width:6px;"></td>
          <td style="background:#FFFFFF;border:0.5px solid #E8E6E1;border-radius:10px;padding:14px 16px;width:33%%;border-left:3px solid %s;">
            <div style="font-size:9.5px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;">Open issues</div>
            <div style="font-size:17px;font-weight:500;color:%s;letter-spacing:-0.01em;font-variant-numeric:tabular-nums;">%d</div>
          </td>
        </tr>
      </table>

      <div style="margin-top:24px;font-size:14px;line-height:1.7;color:#2A2A28;">
        <p style="margin:0 0 14px;">The full report \u2014 with station breakdowns, debtor exposure, expense detail, and open items \u2014 is attached as a PDF for your review.</p>
        <p style="margin:0 0 14px;">Please let me know if any line requires further investigation.</p>
      </div>

      <!-- Signature -->
      <div style="margin-top:32px;padding-top:20px;border-top:0.5px solid #E8E6E1;font-size:13px;color:#2A2A28;line-height:1.6;">
        Kind regards,<br>
        <strong>Chukwudi Samuel Ogbuta</strong><br>
        <span style="color:#8A8780;font-size:12px;">Data Analyst \u00B7 Kifayat Global Energy Limited</span>
      </div>

      <!-- Footer -->
      <div style="margin-top:24px;font-size:10px;color:#8A8780;letter-spacing:0.06em;text-transform:uppercase;">
        Generated by KifayatOps \u00B7 DailyRecon \u00B7 %s
      </div>
    </div>
  </body></html>',
    s$period, range_str,
    greeting,
    tolower(s$period), range_str,
    naira(s$revenue), litres(s$litres),
    if (s$open_issues > 0) "#A8323A" else "#1F4D2C",
    if (s$open_issues > 0) "#A8323A" else "#2A2A28",
    s$open_issues,
    format(Sys.time(), "%d %b %Y \u00B7 %H:%M")
  )
}

## ---------- SEND -----------------------------------------------------------

send_report_email <- function(report, recipients,
                              subject = NULL,
                              cc_recipients = character()) {
  if (!requireNamespace("emayili", quietly = TRUE))
    stop("emayili package required: install.packages('emayili')")

  s <- report$summary
  if (is.null(subject)) {
    subject <- sprintf("KifayatOps %s Report \u2014 %s",
                       s$period,
                       if (s$range_start == s$range_end)
                         format(s$range_end, "%d %b %Y")
                       else
                         paste(format(s$range_start, "%d %b"),
                               format(s$range_end,   "%d %b %Y"), sep = " \u2013 "))
  }

  body_html <- build_email_body(s)

  send_once <- function() {
    msg <- emayili::envelope() |>
      emayili::from("kifayatdata.reports@gmail.com") |>
      emayili::to(recipients) |>
      emayili::subject(subject) |>
      emayili::html(body_html)

    if (!is.null(report$pdf_path) && file.exists(report$pdf_path)) {
      msg <- emayili::attachment(msg, report$pdf_path)
    } else if (!is.null(report$html_path) && file.exists(report$html_path)) {
      msg <- emayili::attachment(msg, report$html_path)
    }
    if (length(cc_recipients) > 0) msg <- emayili::cc(msg, cc_recipients)

    smtp <- emayili::server(
      host     = "smtp.gmail.com",
      port     = 465,
      username = "kifayatdata.reports@gmail.com",
      password = Sys.getenv("GMAIL_APP_PASSWORD"),
      use_ssl  = TRUE,
      reuse    = FALSE
    )
    smtp(msg, verbose = FALSE)
  }

  ## emayili sometimes leaves a stale curl handle after a prior send;
  ## the recommended cure is to GC and retry once on failure.
  result <- tryCatch(send_once(), error = function(e) e)
  if (inherits(result, "error")) {
    gc()
    Sys.sleep(1)
    result <- tryCatch(send_once(), error = function(e) e)
    if (inherits(result, "error")) stop(result)
  }
  TRUE
}


## ============================================================================
## CONSOLIDATED REPORT ENGINE — v2
##   - Reverse-chronological stack of weekly reports
##   - Weekly comparison section (≥2 weeks)
##   - Monthly comparison section (≥2 months)
##   - TOC anchors for navigation
##   - Single-page PDF blocks (no orphan blank pages)
##   - New paper colour: #F5F4ED (KifayatOps now distinct from KifayatHaul)
## ============================================================================

## Fixed editorial palette — never use more than 5 series in one chart
KF_PALETTE <- c("#1F4D2C", "#B8862B", "#A8323A", "#3D7050", "#5C5A52",
                "#7A5410", "#0F2A1A", "#7A1F26")

kf_paper_v2 <- "#F5F4ED"

## ---- helper: list completed weeks in a date range -------------------------
list_weeks <- function(start_date, end_date) {
  sd <- as.Date(start_date); ed <- as.Date(end_date)
  wd <- as.integer(format(sd, "%u"))
  monday <- sd - (wd - 1)
  out <- list()
  while (monday <= ed) {
    wk_start <- monday
    wk_end   <- monday + 6
    out[[length(out) + 1]] <- list(start = wk_start, end = wk_end,
                                   label = format(wk_end, "Week ending %d %b %Y"),
                                   anchor = format(wk_start, "wk%Y%m%d"))
    monday <- monday + 7
  }
  out
}

list_months <- function(start_date, end_date) {
  sd <- as.Date(start_date); ed <- as.Date(end_date)
  first <- as.Date(format(sd, "%Y-%m-01"))
  out <- list()
  while (first <= ed) {
    mstart <- first
    mend   <- seq(first, by = "month", length.out = 2)[2] - 1
    out[[length(out) + 1]] <- list(start = mstart, end = mend,
                                   label = format(mstart, "%B %Y"),
                                   anchor = format(mstart, "mn%Y%m"))
    first <- seq(first, by = "month", length.out = 2)[2]
  }
  out
}

## ---- per-week summary table (used by weekly comparison) -------------------
week_kpis <- function(start_date, end_date) {
  d <- report_data(start_date, end_date)
  total_rev <- sum(d$payments$revenue, na.rm = TRUE)
  total_l   <- sum(d$pump_litres$litres, na.rm = TRUE)
  total_pos <- sum(d$payments$pos, na.rm = TRUE)
  total_cash <- sum(d$payments$cash, na.rm = TRUE)
  total_exp <- sum(d$expenses$amount, na.rm = TRUE)
  gen_litres <- if (!is.null(d$generator) && nrow(d$generator) > 0)
    sum(d$generator$litres, na.rm = TRUE) else 0
  gen_value  <- if (!is.null(d$generator) && nrow(d$generator) > 0)
    sum(d$generator$value_naira, na.rm = TRUE) else 0
  n_high    <- sum(d$issues$severity == "high")
  list(revenue = total_rev, litres = total_l, pos = total_pos,
       cash_to_bank = total_cash, expenses = total_exp,
       gen_litres = gen_litres, gen_value = gen_value,
       combined_expenses = total_exp + gen_value,
       high_issues = n_high, data = d)
}

## ---- missed-sales analysis for a week -------------------------------------
missed_sales <- function(start_date, end_date) {
  message("[ms] start")
  sd <- as.Date(start_date); ed <- as.Date(end_date)

  ## Cap range to the actual data window so we don't count future days
  ## (no one has reported yet) OR pre-data days as missed.
  last_data  <- db_get("select max(report_date) as d from daily_reports;")$d
  first_data <- db_get("select min(report_date) as d from daily_reports;")$d
  if (length(last_data) > 0 && !is.na(last_data)) {
    last_data <- as.Date(last_data)
    if (last_data < ed) ed <- last_data
  }
  if (length(first_data) > 0 && !is.na(first_data)) {
    first_data <- as.Date(first_data)
    if (first_data > sd) sd <- first_data
  }
  if (ed < sd) return(list(table = data.frame(), total_revenue = 0))
  ## Which stations × days have a report (any block)
  rep_days <- db_get(sprintf(
    "select s.name as station, dr.report_date
       from daily_reports dr
       join stations s on s.id = dr.station_id
      where dr.report_date between '%s' and '%s'
      group by s.name, dr.report_date;",
    as.character(sd), as.character(ed)))

  message("[ms] rep_days rows=", nrow(rep_days))
  all_stations <- db_get(
    "select name from stations where active order by name;")$name
  all_days <- seq(sd, ed, by = "day")
  expected <- expand.grid(station = all_stations, report_date = all_days,
                          stringsAsFactors = FALSE)
  expected$report_date <- as.Date(expected$report_date)
  rep_days$report_date <- as.Date(rep_days$report_date)
  missing <- dplyr::anti_join(expected, rep_days,
    by = c("station", "report_date"))
  message("[ms] missing rows=", nrow(missing))
  if (nrow(missing) == 0) return(list(table = data.frame(), total_revenue = 0))

  ## For each station, compute avg daily litres per product in the week
  per_station_l <- db_get(sprintf(
    "select s.name as station, p.code as product,
            count(distinct dr.report_date) as days_with_data,
            sum(ps.litres_sold) as total_l
       from pump_sales ps
       join pumps pm on pm.id = ps.pump_id
       join tanks t on t.id = pm.tank_id
       join products p on p.id = t.product_id
       join daily_reports dr on dr.id = ps.report_id
       join stations s on s.id = dr.station_id
      where dr.report_date between '%s' and '%s'
      group by s.name, p.code;",
    as.character(sd), as.character(ed)))
  per_station_l$avg_daily <- ifelse(per_station_l$days_with_data > 0,
    per_station_l$total_l / per_station_l$days_with_data, 0)

  message("[ms] per_station_l rows=", nrow(per_station_l))
  ## Prevailing price per station × product (most recent before week end)
  prices <- db_get(sprintf(
    "select s.name as station, p.code as product, pr.price_per_litre
       from prices pr
       join stations s on s.id = pr.station_id
       join products p on p.id = pr.product_id
      where pr.effective_from <= '%s'
        and (pr.effective_to is null or pr.effective_to >= '%s');",
    as.character(ed), as.character(sd)))
  prices <- prices %>% dplyr::group_by(station, product) %>%
    dplyr::summarise(price = max(price_per_litre), .groups = "drop")

  message("[ms] prices rows=", nrow(prices))

  ## Estimate per missing station-day: sum over products of avg_daily * price
  agg <- merge(per_station_l, prices, by = c("station","product"), all.x = TRUE)
  agg$est_per_day <- agg$avg_daily * agg$price
  est_per_station <- agg %>% dplyr::group_by(station) %>%
    dplyr::summarise(est_lost_per_day = sum(est_per_day, na.rm = TRUE),
                     avg_litres_per_day = sum(avg_daily, na.rm = TRUE),
                     .groups = "drop")

  message("[ms] est_per_station rows=", nrow(est_per_station))
  ## Collapse missed dates per station into a single comma-separated string
  dates_per_station <- missing %>% dplyr::arrange(station, report_date) %>%
    dplyr::group_by(station) %>%
    dplyr::summarise(
      dates = paste(format(report_date, "%d %b"), collapse = ", "),
      .groups = "drop")

  ms <- missing %>% dplyr::group_by(station) %>%
    dplyr::summarise(missed_days = dplyr::n(), .groups = "drop")
  ms <- merge(ms, dates_per_station, by = "station", all.x = TRUE)
  ms <- merge(ms, est_per_station, by = "station", all.x = TRUE)
  message("[ms] ms after merge rows=", nrow(ms), " cols=", paste(names(ms), collapse=","))
  ms$est_lost_litres  <- ms$missed_days * ifelse(is.na(ms$avg_litres_per_day), 0, ms$avg_litres_per_day)
  ms$est_lost_revenue <- ms$missed_days * ifelse(is.na(ms$est_lost_per_day),   0, ms$est_lost_per_day)
  ## Fill NA only in numeric columns; keep dates as character.
  num_cols <- sapply(ms, is.numeric)
  for (cn in names(ms)[num_cols]) ms[[cn]][is.na(ms[[cn]])] <- 0
  ms <- ms[ms$missed_days > 0, ]
  ms <- ms[order(-ms$est_lost_revenue), ]
  total_rev_missed <- sum(ms$est_lost_revenue, na.rm = TRUE)
  message("[ms] done, final rows=", nrow(ms))
  list(table = ms, total_revenue = total_rev_missed)
}

## ---- multi-station trend chart (SVG) --------------------------------------
multi_station_trend <- function(rows, x_label_fn,
                                title = "", y_label = "Revenue",
                                width = 720, height = 240) {
  if (is.null(rows) || nrow(rows) == 0) return("")
  stations <- unique(rows$station)
  if (length(stations) > 5) {
    top5 <- rows %>% dplyr::group_by(station) %>%
      dplyr::summarise(t = sum(value, na.rm = TRUE), .groups = "drop") %>%
      dplyr::arrange(dplyr::desc(t)) %>%
      dplyr::slice(1:5)
    stations <- top5$station
    rows <- rows[rows$station %in% stations, ]
  }

  buckets <- sort(unique(rows$bucket))
  if (length(buckets) < 2) return("")
  W <- width; H <- height; PL <- 64; PR <- 16; PT <- 18; PB <- 42
  max_y <- max(rows$value, na.rm = TRUE)
  if (!is.finite(max_y) || max_y == 0) return("")

  x_scale <- function(i) PL + (W - PL - PR) * (i - 1) / max(1, length(buckets) - 1)
  y_scale <- function(v) H - PB - (H - PT - PB) * (v / max_y)

  y_steps <- pretty(c(0, max_y), n = 4)
  y_lines <- vapply(y_steps, function(v) sprintf(
    '<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#EFEDE6" stroke-width="0.5"/>
     <text x="%d" y="%.1f" font-size="10" fill="#8A8780" font-family="Inter" text-anchor="end">\u20A6%s</text>',
    PL, y_scale(v), W - PR, y_scale(v), PL - 8, y_scale(v) + 3,
    if (v >= 1e6) sprintf("%.1fM", v/1e6)
    else if (v >= 1e3) sprintf("%.0fk", v/1e3)
    else format(round(v), big.mark = ",")), character(1))

  x_labs <- vapply(seq_along(buckets), function(i) sprintf(
    '<text x="%.1f" y="%d" text-anchor="middle" font-size="10" fill="#8A8780" font-family="Inter">%s</text>',
    x_scale(i), H - 14, x_label_fn(buckets[i])), character(1))

  series_html <- ""
  legend_html <- ""
  for (idx in seq_along(stations)) {
    s <- stations[idx]
    color <- KF_PALETTE[((idx - 1) %% length(KF_PALETTE)) + 1]
    sub <- rows[rows$station == s, ]
    pts <- character(0); dots <- character(0)
    for (j in seq_along(buckets)) {
      m <- sub[sub$bucket == buckets[j], , drop = FALSE]
      v <- if (nrow(m) > 0) m$value[1] else NA
      if (!is.na(v)) {
        x <- x_scale(j); y <- y_scale(v)
        pts <- c(pts, sprintf("%.1f,%.1f", x, y))
        dots <- c(dots, sprintf(
          '<circle cx="%.1f" cy="%.1f" r="3" fill="%s" stroke="#fff" stroke-width="1.4"><title>%s \u00B7 %s: \u20A6%s</title></circle>',
          x, y, color, s, x_label_fn(buckets[j]),
          format(round(v), big.mark = ",")))
      }
    }
    if (length(pts) >= 2) {
      series_html <- paste0(series_html, sprintf(
        '<polyline points="%s" fill="none" stroke="%s" stroke-width="1.6" stroke-linejoin="round"/>%s',
        paste(pts, collapse = " "), color, paste(dots, collapse = "")))
    }
    legend_html <- paste0(legend_html, sprintf(
      '<span style="display:inline-flex;align-items:center;gap:6px;font-size:11px;color:#2A2A28;margin-right:14px;">
        <span style="display:inline-block;width:10px;height:2px;background:%s;"></span>%s
      </span>', color, s))
  }

  sprintf('<div style="margin-top:6px;">
    %s
    <svg viewBox="0 0 %d %d" preserveAspectRatio="none" style="width:100%%;height:%dpx;">
      %s %s %s
    </svg>
    <div style="margin-top:8px;text-align:center;">%s</div></div>',
    if (nzchar(title)) sprintf('<div style="font-size:12px;color:#5C5A52;margin-bottom:6px;">%s</div>', title) else "",
    W, H, height,
    paste(y_lines, collapse=""), series_html, paste(x_labs, collapse=""),
    legend_html)
}

## ---- WoW % per station bars ----------------------------------------------
wow_bars <- function(this_week, last_week, value_extractor, label_fmt,
                     accent_up = "#3D7050", accent_dn = "#A8323A") {
  if (is.null(this_week) || is.null(last_week)) return("")
  stations <- unique(c(this_week$station, last_week$station))
  results <- lapply(stations, function(s) {
    a <- sum(value_extractor(this_week, s), na.rm = TRUE)
    b <- sum(value_extractor(last_week, s), na.rm = TRUE)
    if (!is.finite(a) || !is.finite(b) || b == 0) return(NULL)
    pct <- 100 * (a - b) / b
    color <- if (pct >= 0) accent_up else accent_dn
    arrow <- if (pct >= 0) "\u25B2" else "\u25BC"
    list(pct = pct, html = sprintf(
      '<div style="display:grid;grid-template-columns:120px 70px 1fr;gap:8px;align-items:center;padding:5px 0;border-bottom:0.5px solid #F2F0EB;font-size:12.5px;">
        <div>%s</div>
        <div style="font-family:JetBrains Mono,monospace;color:%s;text-align:right;">%s %.1f%%</div>
        <div style="color:#8A8780;font-size:11px;">%s</div></div>',
      s, color, arrow, abs(pct), label_fmt(a, b)))
  })
  results <- Filter(Negate(is.null), results)
  if (length(results) == 0)
    return("<div style='font-size:12px;color:#8A8780;'>Insufficient data for comparison.</div>")
  ## Sort by pct descending — biggest positive first, biggest negative last
  results <- results[order(-vapply(results, function(x) x$pct, numeric(1)))]
  paste(vapply(results, function(x) x$html, character(1)), collapse = "")
}

## ---- HIGH-LEVEL CONSOLIDATED REPORT ----------------------------------------
generate_consolidated_report <- function(start_date, end_date,
                                          out_dir = "reports") {
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  sd <- as.Date(start_date); ed <- as.Date(end_date)

  weeks  <- list_weeks(sd, ed)
  months <- list_months(sd, ed)
  ## A "complete week" means we have ANY data for that week
  weeks_with_data <- Filter(function(w) {
    sql <- sprintf("select 1 from daily_reports where report_date between '%s'::date and '%s'::date limit 1;",
                   as.character(w$start), as.character(w$end))
    nrow(db_get(sql)) > 0
  }, weeks)
  months_with_data <- Filter(function(m) {
    sql <- sprintf("select 1 from daily_reports where report_date between '%s'::date and '%s'::date limit 1;",
                   as.character(m$start), as.character(m$end))
    nrow(db_get(sql)) > 0
  }, months)

  show_weekly_cmp  <- length(weeks_with_data)  >= 2
  show_monthly_cmp <- length(months_with_data) >= 2

  ## ---- Build CSS (paper colour updated) ----------------------------------
  css <- paste0('<style>
    @page { size: A4; margin: 20mm 18mm 22mm 18mm; }
    body{margin:0;padding:0;background:', kf_paper_v2, ';font-family:Inter,system-ui,-apple-system,sans-serif;color:#2A2A28;-webkit-font-smoothing:antialiased;}
    .wrap{max-width:780px;margin:0 auto;padding:42px 36px;}
    .cover{display:flex;justify-content:space-between;align-items:flex-end;padding-bottom:22px;border-bottom:0.5px solid #E8E6E1;}
    .brand{display:flex;align-items:center;gap:12px;}
    .brand-mark{width:36px;height:36px;background:#1F4D2C;color:#F4ECDB;border-radius:8px;display:inline-flex;align-items:center;justify-content:center;font-weight:700;font-size:18px;}
    .brand-name{font-size:16px;font-weight:500;}
    .brand-sub{font-size:10px;color:#8A8780;letter-spacing:0.14em;text-transform:uppercase;margin-top:2px;}
    .cover-right{text-align:right;}
    .cover-right .eyebrow{font-size:10px;color:#1F4D2C;letter-spacing:0.14em;text-transform:uppercase;font-weight:500;}
    .cover-right .period{font-size:22px;font-weight:500;margin-top:4px;letter-spacing:-0.015em;}
    .cover-right .range{font-size:12px;color:#8A8780;margin-top:2px;}
    h2{font-size:12.5px;font-weight:500;color:#1F4D2C;letter-spacing:0.13em;text-transform:uppercase;margin:30px 0 12px;}
    h3{font-size:13.5px;font-weight:500;margin:24px 0 10px;}
    .lead{font-size:11.5px;color:#8A8780;margin:-8px 0 14px;line-height:1.5;}
    .toc{background:#fff;border:0.5px solid #E8E6E1;border-radius:10px;padding:16px 20px;}
    .toc-item{display:flex;justify-content:space-between;padding:6px 0;border-bottom:0.5px solid #F2F0EB;font-size:13px;}
    .toc-item:last-child{border-bottom:none;}
    .toc-item a{color:#2A2A28;text-decoration:none;font-weight:500;}
    .toc-item a:hover{color:#1F4D2C;}
    .toc-item .date{color:#8A8780;font-size:11.5px;font-family:JetBrains Mono,monospace;}
    .kpi-grid{display:grid;grid-template-columns:repeat(4,1fr);gap:10px;margin-top:8px;}
    .kpi{background:#fff;border:0.5px solid #E8E6E1;border-radius:10px;padding:13px 14px;position:relative;overflow:hidden;}
    .kpi::before{content:"";position:absolute;left:0;top:0;bottom:0;width:3px;background:#1F4D2C;}
    .kpi.warn::before{background:#B8862B;}
    .kpi.alt::before{background:#A8323A;}
    .kpi-label{font-size:9px;color:#8A8780;letter-spacing:0.10em;text-transform:uppercase;font-weight:500;margin-bottom:6px;}
    .kpi-value{font-size:16px;font-weight:500;letter-spacing:-0.01em;line-height:1.1;font-variant-numeric:tabular-nums;}
    .kpi-foot{font-size:10px;color:#8A8780;margin-top:4px;}
    .delta-up{color:#3D7050;font-weight:500;}
    .delta-dn{color:#A8323A;font-weight:500;}
    table{width:100%;border-collapse:collapse;margin-top:6px;font-size:12.5px;font-variant-numeric:tabular-nums;}
    th{font-size:9.5px;color:#5C5A52;font-weight:500;letter-spacing:0.06em;text-transform:uppercase;text-align:left;padding:7px 9px;border-bottom:0.5px solid #E8E6E1;}
    td{padding:8px 9px;border-bottom:0.5px solid #F2F0EB;}
    td.num,th.num{text-align:right;font-family:JetBrains Mono,monospace;}
    .section{page-break-after:always;}
    .section:last-child{page-break-after:auto;}
    .footnote{font-size:10.5px;color:#8A8780;margin-top:8px;font-style:italic;line-height:1.5;}
    .week-block{margin-bottom:20px;}
    .summary{font-size:14px;line-height:1.7;color:#2A2A28;}
    .heat-row{display:grid;grid-template-columns:120px repeat(7,1fr);gap:2px;margin-bottom:2px;}
    .heat-row .lbl{font-size:11.5px;padding:6px 6px;font-weight:500;}
    .heat-row .cell{height:26px;display:flex;align-items:center;justify-content:center;font-size:10.5px;color:#2A2A28;border-radius:3px;font-family:JetBrains Mono,monospace;}
    .heat-head{display:grid;grid-template-columns:120px repeat(7,1fr);gap:2px;margin-bottom:4px;}
    .heat-head .hd{font-size:9.5px;color:#5C5A52;letter-spacing:0.06em;text-transform:uppercase;text-align:center;padding:4px 0;}
    .footer{margin-top:30px;padding-top:14px;border-top:0.5px solid #E8E6E1;font-size:10px;color:#8A8780;letter-spacing:0.06em;text-transform:uppercase;display:flex;justify-content:space-between;}
  </style>')

  fmt_n <- function(x) {
    x <- suppressWarnings(as.numeric(x))[1]
    if (is.na(x)) x <- 0
    paste0("\u20A6", format(round(x), big.mark=","))
  }
  fmt_l <- function(x) {
    x <- suppressWarnings(as.numeric(x))[1]
    if (is.na(x)) x <- 0
    paste0(format(round(x, 1), big.mark=",", nsmall=1), " L")
  }
  fmt_delta <- function(a, b, naira = TRUE) {
    a <- suppressWarnings(as.numeric(a))[1]
    b <- suppressWarnings(as.numeric(b))[1]
    if (is.na(a) || is.na(b) || !is.finite(a) || !is.finite(b) || b == 0)
      return('<span style="color:#8A8780;">\u2014</span>')
    pct <- 100 * (a - b) / b
    cls <- if (pct >= 0) "delta-up" else "delta-dn"
    arrow <- if (pct >= 0) "\u25B2" else "\u25BC"
    sprintf('<span class="%s">%s %.1f%%</span>', cls, arrow, abs(pct))
  }

  ## =====================================================================
  ## SECTION 1: Cover + TOC
  ## =====================================================================
  total_weeks  <- length(weeks_with_data)
  total_months <- length(months_with_data)

  toc_items <- character(0)
  if (show_monthly_cmp)
    toc_items <- c(toc_items, sprintf(
      '<div class="toc-item"><a href="#monthly-cmp">Monthly comparison</a><span class="date">%d months</span></div>',
      total_months))
  if (show_weekly_cmp)
    toc_items <- c(toc_items, sprintf(
      '<div class="toc-item"><a href="#weekly-cmp">Weekly comparison</a><span class="date">%d weeks</span></div>',
      total_weeks))
  ## Weeks in reverse chronological
  for (w in rev(weeks_with_data)) {
    toc_items <- c(toc_items, sprintf(
      '<div class="toc-item"><a href="#%s">%s</a><span class="date">%s \u2013 %s</span></div>',
      w$anchor, w$label,
      format(w$start, "%d %b"), format(w$end, "%d %b")))
  }

  cover_html <- paste0(
    '<div class="section"><div class="wrap">',
    '<div class="cover"><div class="brand">',
    '<div class="brand-mark">K</div>',
    '<div><div class="brand-name">KifayatOps</div>',
    '<div class="brand-sub">Station Operations</div></div></div>',
    '<div class="cover-right">',
    '<div class="eyebrow">Consolidated report</div>',
    '<div class="period">', format(ed, "%d %B %Y"), '</div>',
    '<div class="range">', format(sd, "%d %b %Y"), ' \u2013 ', format(ed, "%d %b %Y"),
    '</div></div></div>',

    '<h2>What\'s inside</h2>',
    '<p class="summary" style="font-size:13.5px;">This consolidated report contains every weekly snapshot since data capture began, with the most recent week at the top. ',
    if (show_weekly_cmp) 'Comparison sections sit at the top so trends across weeks ',
    if (show_weekly_cmp && show_monthly_cmp) 'and months ',
    if (show_weekly_cmp) 'are visible at a glance. ',
    'Click any item below to jump directly to that section.</p>',

    '<h2>Contents</h2>',
    '<div class="toc">', paste(toc_items, collapse = ""), '</div>',

    '</div></div>'
  )

  ## =====================================================================
  ## SECTION 2: Monthly comparison (if applicable)
  ## =====================================================================
  message("[consolidated] entering monthly section, show=", show_monthly_cmp)
  monthly_html <- ""
  if (show_monthly_cmp) {
    month_kpis <- lapply(months_with_data, function(m) {
      k <- week_kpis(m$start, m$end)
      list(label = m$label, anchor = m$anchor,
           revenue = k$revenue, litres = k$litres,
           cash_to_bank = k$cash_to_bank, expenses = k$expenses,
           gen_litres = k$gen_litres, gen_value = k$gen_value,
           combined_expenses = k$combined_expenses,
           high_issues = k$high_issues)
    })

    ## Revenue trend per station per month (top 5)
    rev_rows <- do.call(rbind, lapply(months_with_data, function(m) {
      d <- report_data(m$start, m$end)
      if (nrow(d$payments) == 0) return(NULL)
      r <- d$payments %>% dplyr::group_by(station) %>%
        dplyr::summarise(value = sum(revenue, na.rm = TRUE), .groups = "drop")
      r$bucket <- m$anchor
      r$bucket_label <- m$label
      r
    }))
    bucket_to_label <- setNames(
      vapply(months_with_data, function(m) m$label, character(1)),
      vapply(months_with_data, function(m) m$anchor, character(1))
    )
    trend_svg <- if (!is.null(rev_rows) && nrow(rev_rows) > 0)
      multi_station_trend(rev_rows,
        function(b) bucket_to_label[[b]] %||% b,
        title = "Revenue trend by station (top 5 by revenue)") else ""

    ## Litres MoM bars: compare last vs prior month
    if (length(months_with_data) >= 2) {
      m_last  <- months_with_data[[length(months_with_data)]]
      m_prior <- months_with_data[[length(months_with_data) - 1]]
      tl <- report_data(m_last$start,  m_last$end)$pump_litres %>%
        dplyr::group_by(station) %>%
        dplyr::summarise(l = sum(litres, na.rm = TRUE), .groups = "drop")
      tp <- report_data(m_prior$start, m_prior$end)$pump_litres %>%
        dplyr::group_by(station) %>%
        dplyr::summarise(l = sum(litres, na.rm = TRUE), .groups = "drop")
      litres_bars <- wow_bars(tl, tp,
        function(d, s) {
          v <- d$l[d$station == s]; if (length(v) == 0) 0 else v
        },
        function(a, b) sprintf("%s vs %s",
          format(round(a,1), big.mark=","), format(round(b,1), big.mark=",")))
    } else litres_bars <- ""

    ## Headline KPIs - last month vs prior
    if (length(month_kpis) >= 2) {
      a <- month_kpis[[length(month_kpis)]]
      b <- month_kpis[[length(month_kpis) - 1]]
      kpi_strip <- sprintf(
        '<div class="kpi-grid" style="grid-template-columns:repeat(3,1fr);">
          <div class="kpi"><div class="kpi-label">Revenue \u00B7 %s</div>
            <div class="kpi-value">%s</div><div class="kpi-foot">%s vs %s</div></div>
          <div class="kpi warn"><div class="kpi-label">Litres \u00B7 %s</div>
            <div class="kpi-value">%s</div><div class="kpi-foot">%s vs %s</div></div>
          <div class="kpi"><div class="kpi-label">Cash to bank</div>
            <div class="kpi-value">%s</div><div class="kpi-foot">%s</div></div>
          <div class="kpi alt"><div class="kpi-label">Cash expenses</div>
            <div class="kpi-value">%s</div><div class="kpi-foot">%s</div></div>
          <div class="kpi warn"><div class="kpi-label">Generator fuel (in-kind)</div>
            <div class="kpi-value">%s</div><div class="kpi-foot">%s &nbsp;\u00B7&nbsp; %s</div></div>
          <div class="kpi alt"><div class="kpi-label">Combined expenses</div>
            <div class="kpi-value">%s</div><div class="kpi-foot">%s</div></div>
        </div>',
        a$label, fmt_n(a$revenue), fmt_delta(a$revenue, b$revenue), fmt_n(b$revenue),
        a$label, fmt_l(a$litres),  fmt_delta(a$litres,  b$litres),  fmt_l(b$litres),
        fmt_n(a$cash_to_bank), fmt_delta(a$cash_to_bank, b$cash_to_bank),
        fmt_n(a$expenses),     fmt_delta(a$expenses, b$expenses),
        fmt_n(a$gen_value),    fmt_l(a$gen_litres), fmt_delta(a$gen_value, b$gen_value),
        fmt_n(a$combined_expenses), fmt_delta(a$combined_expenses, b$combined_expenses))
    } else kpi_strip <- ""

    ## Peak weekday heatmap (station × weekday avg revenue across all data)
    wd_data <- db_get(sprintf("
      select s.name as station,
             extract(isodow from dr.report_date)::int as dow,
             sum(pmt.reported_revenue) as rev
        from payments pmt
        join daily_reports dr on dr.id = pmt.report_id
        join stations s on s.id = dr.station_id
       where dr.report_date between '%s' and '%s'
       group by s.name, dow
       order by s.name, dow;",
      as.character(sd), as.character(ed)))

    heat_html <- ""
    if (nrow(wd_data) > 0) {
      max_v <- max(wd_data$rev, na.rm = TRUE)
      stations <- sort(unique(wd_data$station))
      day_lbl <- c("Mon","Tue","Wed","Thu","Fri","Sat","Sun")
      head_html <- paste0('<div class="heat-head"><div class="hd">Station</div>',
        paste(vapply(day_lbl, function(d) sprintf('<div class="hd">%s</div>', d),
                     character(1)), collapse = ""), '</div>')
      body_html <- paste(vapply(stations, function(s) {
        rs <- wd_data[wd_data$station == s, , drop = FALSE]
        cells <- vapply(1:7, function(d) {
          row <- rs[rs$dow == d, , drop = FALSE]
          v <- if (nrow(row) > 0) row$rev[1] else 0
          intensity <- if (max_v > 0) min(1, v / max_v) else 0
          alpha <- 0.10 + 0.80 * intensity
          textcol <- if (intensity > 0.55) "#fff" else "#2A2A28"
          val <- if (v >= 1e6) sprintf("%.1fM", v/1e6)
                 else if (v >= 1e3) sprintf("%.0fk", v/1e3)
                 else if (v > 0) format(round(v), big.mark=",")
                 else "\u2014"
          sprintf('<div class="cell" style="background:rgba(31,77,44,%.2f);color:%s;">%s</div>',
                  alpha, textcol, val)
        }, character(1))
        sprintf('<div class="heat-row"><div class="lbl">%s</div>%s</div>',
                s, paste(cells, collapse = ""))
      }, character(1)), collapse = "")
      heat_html <- paste0(head_html, body_html)
    }

    ## High-severity issues per month — thin trend
    issues_per_month <- vapply(months_with_data, function(m) {
      as.integer(db_get(sprintf(
        "select count(*) as n from issues i
           join daily_reports dr on dr.id = i.report_id
          where i.severity = 'high'
            and dr.report_date between '%s' and '%s';",
        as.character(m$start), as.character(m$end)))$n)
    }, integer(1))
    issue_svg <- overall_trend_with_pct(
      as.numeric(issues_per_month),
      vapply(months_with_data, function(m) format(m$start, "%b"), character(1)),
      title = "High-severity issues per month",
      y_format = "count", accent = "#5C5A52", height = 160)

    message("[monthly] step: overall trends")
    month_labels <- vapply(month_kpis, function(x) x$label, character(1))
    m_rev_series  <- vapply(month_kpis, function(x) as.numeric(x$revenue), numeric(1))
    m_lit_series  <- vapply(month_kpis, function(x) as.numeric(x$litres), numeric(1))
    m_exp_series  <- vapply(month_kpis, function(x) as.numeric(x$expenses), numeric(1))
    overall_m_rev_svg <- overall_trend_with_pct(m_rev_series, month_labels,
      title = "Network revenue across months", y_format = "naira", accent = "#1F4D2C")
    overall_m_lit_svg <- overall_trend_with_pct(m_lit_series, month_labels,
      title = "Network litres sold across months", y_format = "litres", accent = "#B8862B")
    overall_m_exp_svg <- overall_trend_with_pct(m_exp_series, month_labels,
      title = "Network expenses across months", y_format = "naira", accent = "#A8323A")

    ## Missed days + lost revenue trends per month
    missed_per_month <- lapply(months_with_data, function(m) {
      mw <- missed_sales(m$start, m$end)
      list(days = if (is.data.frame(mw$table) && nrow(mw$table) > 0)
                    sum(mw$table$missed_days) else 0L,
           lost = mw$total_revenue %||% 0)
    })
    m_missed_days <- vapply(missed_per_month, function(x) as.integer(x$days), integer(1))
    m_missed_rev  <- vapply(missed_per_month, function(x) as.numeric(x$lost), numeric(1))
    m_missed_days_svg <- overall_trend_with_pct(m_missed_days, month_labels,
      title = "Missed station-days per month", y_format = "count",
      accent = "#A8323A", height = 160)
    m_missed_rev_svg <- overall_trend_with_pct(m_missed_rev, month_labels,
      title = "Estimated lost revenue per month", y_format = "naira",
      accent = "#A8323A", height = 160)

    ## Generator-fuel trends per month
    gen_per_month <- vapply(months_with_data, function(m) {
      d <- report_data(m$start, m$end)
      if (is.null(d$generator) || nrow(d$generator) == 0) return(0)
      as.numeric(sum(d$generator$litres, na.rm = TRUE))
    }, numeric(1))
    gen_value_per_month <- vapply(months_with_data, function(m) {
      d <- report_data(m$start, m$end)
      if (is.null(d$generator) || nrow(d$generator) == 0) return(0)
      as.numeric(sum(d$generator$value_naira, na.rm = TRUE))
    }, numeric(1))
    m_gen_litres_svg <- overall_trend_with_pct(gen_per_month, month_labels,
      title = "Generator fuel litres per month", y_format = "litres",
      accent = "#B8862B", height = 160)
    m_gen_value_svg <- overall_trend_with_pct(gen_value_per_month, month_labels,
      title = "Generator fuel estimated value per month", y_format = "naira",
      accent = "#B8862B", height = 160)

    cur_m_lbl <- month_kpis[[length(month_kpis)]]$label
    prv_m_lbl <- month_kpis[[length(month_kpis) - 1]]$label

    monthly_html <- paste0(
      '<div class="section" id="monthly-cmp"><div class="wrap">',
      '<h2>Monthly comparison</h2>',
      '<p class="lead">Month-over-month KPI movement, station-level revenue trajectory, peak weekday patterns and issue cadence. Only included once at least two complete months of data exist.</p>',

      if (nzchar(kpi_strip)) paste0(sprintf('<h3>%s vs %s</h3>', cur_m_lbl, prv_m_lbl), kpi_strip) else "",

      if (nzchar(overall_m_rev_svg))
        paste0('<h3>Revenue \u00B7 network-wide trend</h3>',
               '<p class="lead">Total revenue per month with month-over-month % change at each point.</p>',
               overall_m_rev_svg) else "",

      if (nzchar(trend_svg)) paste0('<h3>Revenue \u00B7 per-station breakdown</h3>', trend_svg) else "",

      if (nzchar(overall_m_lit_svg))
        paste0('<h3>Litres sold \u00B7 network-wide trend</h3>',
               '<p class="lead">Total litres dispatched per month with MoM % change.</p>',
               overall_m_lit_svg) else "",

      if (nzchar(litres_bars)) paste0(sprintf('<h3>Litres sold \u00B7 %s vs %s, per station</h3>', cur_m_lbl, prv_m_lbl), litres_bars) else "",

      if (nzchar(overall_m_exp_svg))
        paste0('<h3>Expenses \u00B7 network-wide trend</h3>',
               '<p class="lead">Total expenses per month with MoM % change.</p>',
               overall_m_exp_svg) else "",

      if (nzchar(heat_html)) paste0('<h3>Peak weekday by station</h3>',
        '<p class="lead">Cumulative revenue by weekday across all data. Darker cells indicate the station\'s best-performing day(s).</p>',
        heat_html) else "",

      if (nzchar(m_missed_days_svg))
        paste0('<h3>Missed station-days \u00B7 trend across months</h3>',
               m_missed_days_svg) else "",
      if (nzchar(m_missed_rev_svg))
        paste0('<h3>Estimated lost revenue \u00B7 trend across months</h3>',
               m_missed_rev_svg) else "",

      if (nzchar(m_gen_litres_svg))
        paste0('<h3>Generator fuel litres \u00B7 trend across months</h3>',
               '<p class="lead">Total litres of PMS consumed by station generators per month. Value at current PMS price shown alongside each point.</p>',
               m_gen_litres_svg) else "",

      if (nzchar(issue_svg)) paste0('<h3>High-severity issues per month</h3>', issue_svg) else "",

      '</div></div>'
    )
  }

  ## =====================================================================
  ## SECTION 3: Weekly comparison (if applicable)
  ## =====================================================================
  message("[consolidated] entering weekly section, show=", show_weekly_cmp)
  weekly_html <- ""
  if (show_weekly_cmp) {
    message("[weekly] step 1: kpi list")
    week_kpi_list <- lapply(weeks_with_data, function(w) {
      k <- week_kpis(w$start, w$end)
      list(label = w$label, anchor = w$anchor,
           start = w$start, end = w$end,
           revenue = k$revenue, litres = k$litres,
           cash_to_bank = k$cash_to_bank, expenses = k$expenses,
           gen_litres = k$gen_litres, gen_value = k$gen_value,
           combined_expenses = k$combined_expenses,
           high_issues = k$high_issues)
    })

    message("[weekly] step 2: kpi strip")
    ## Headline: this week vs last
    a <- week_kpi_list[[length(week_kpi_list)]]
    b <- week_kpi_list[[length(week_kpi_list) - 1]]
    week_kpi_strip <- sprintf(
      '<div class="kpi-grid" style="grid-template-columns:repeat(3,1fr);">
        <div class="kpi"><div class="kpi-label">Revenue \u00B7 wk %s</div>
          <div class="kpi-value">%s</div><div class="kpi-foot">%s vs %s</div></div>
        <div class="kpi warn"><div class="kpi-label">Litres</div>
          <div class="kpi-value">%s</div><div class="kpi-foot">%s vs %s</div></div>
        <div class="kpi"><div class="kpi-label">Cash to bank</div>
          <div class="kpi-value">%s</div><div class="kpi-foot">%s</div></div>
        <div class="kpi alt"><div class="kpi-label">Cash expenses</div>
          <div class="kpi-value">%s</div><div class="kpi-foot">%s</div></div>
        <div class="kpi warn"><div class="kpi-label">Generator fuel (in-kind)</div>
          <div class="kpi-value">%s</div><div class="kpi-foot">%s &nbsp;\u00B7&nbsp; %s</div></div>
        <div class="kpi alt"><div class="kpi-label">Combined expenses</div>
          <div class="kpi-value">%s</div><div class="kpi-foot">%s</div></div>
      </div>',
      format(a$end, "%d %b"),
      fmt_n(a$revenue), fmt_delta(a$revenue, b$revenue), fmt_n(b$revenue),
      fmt_l(a$litres),  fmt_delta(a$litres,  b$litres),  fmt_l(b$litres),
      fmt_n(a$cash_to_bank), fmt_delta(a$cash_to_bank, b$cash_to_bank),
      fmt_n(a$expenses),     fmt_delta(a$expenses, b$expenses),
      fmt_n(a$gen_value),    fmt_l(a$gen_litres), fmt_delta(a$gen_value, b$gen_value),
      fmt_n(a$combined_expenses), fmt_delta(a$combined_expenses, b$combined_expenses))

    message("[weekly] step 3: trend rows")
    ## Revenue trend across all weeks (per station)
    week_rev_rows <- do.call(rbind, lapply(weeks_with_data, function(w) {
      d <- report_data(w$start, w$end)
      if (nrow(d$payments) == 0) return(NULL)
      r <- d$payments %>% dplyr::group_by(station) %>%
        dplyr::summarise(value = sum(revenue, na.rm = TRUE), .groups = "drop")
      r$bucket <- w$anchor; r
    }))
    bucket_to_lbl <- setNames(
      vapply(weeks_with_data, function(w) format(w$end, "%d %b"), character(1)),
      vapply(weeks_with_data, function(w) w$anchor, character(1)))

    message("[weekly] step 4: trend svg")
    week_trend_svg <- if (!is.null(week_rev_rows) && nrow(week_rev_rows) > 0)
      multi_station_trend(week_rev_rows,
        function(b) bucket_to_lbl[[b]] %||% b,
        title = "Revenue by station, week-by-week (top 5 by revenue)") else ""

    ## Litres WoW bars
    tl <- report_data(a$start, a$end)$pump_litres %>%
      dplyr::group_by(station) %>%
      dplyr::summarise(l = sum(litres, na.rm = TRUE), .groups = "drop")
    tp <- report_data(b$start, b$end)$pump_litres %>%
      dplyr::group_by(station) %>%
      dplyr::summarise(l = sum(litres, na.rm = TRUE), .groups = "drop")
    message("[weekly] step 5: litres WoW")
    litres_wow <- wow_bars(tl, tp,
      function(d, s) { v <- d$l[d$station == s]; if (length(v) == 0) 0 else v },
      function(a, b) sprintf("%s vs %s",
        format(round(a,1), big.mark=","), format(round(b,1), big.mark=",")))

    message("[weekly] step 6: revenue WoW")
    ## Revenue WoW bars
    tr <- report_data(a$start, a$end)$payments %>%
      dplyr::group_by(station) %>%
      dplyr::summarise(r = sum(revenue, na.rm = TRUE), .groups = "drop")
    pr <- report_data(b$start, b$end)$payments %>%
      dplyr::group_by(station) %>%
      dplyr::summarise(r = sum(revenue, na.rm = TRUE), .groups = "drop")
    rev_wow <- wow_bars(tr, pr,
      function(d, s) { v <- d$r[d$station == s]; if (length(v) == 0) 0 else v },
      function(a, b) sprintf("%s vs %s",
        format(round(a), big.mark=","), format(round(b), big.mark=",")))

    message("[weekly] step 7: missed sales")
    ## Missed sales for the most recent week
    ms <- missed_sales(a$start, a$end)
    ms_html <- if (nrow(ms$table) > 0) {
      rows <- paste(vapply(seq_len(nrow(ms$table)), function(i) sprintf(
        '<tr><td><strong>%s</strong></td><td class="num">%d</td><td style="font-size:11px;color:#5C5A52;">%s</td><td class="num">%s</td><td class="num">%s</td></tr>',
        ms$table$station[i], as.integer(ms$table$missed_days[i]),
        ms$table$dates[i] %||% "",
        fmt_l(ms$table$est_lost_litres[i]),
        fmt_n(ms$table$est_lost_revenue[i])), character(1)), collapse = "")
      paste0(
        '<table><thead><tr><th>Station</th><th class="num">Missed days</th><th>Dates</th><th class="num">Est. lost litres</th><th class="num">Est. lost revenue</th></tr></thead>',
        '<tbody>', rows, '</tbody></table>',
        sprintf('<p class="footnote">Total estimated revenue missed in week: <strong>%s</strong>. Methodology: per missing station-day, applied the station\'s average daily litres for the week (separated by product) multiplied by the prevailing per-product price effective for the station during the week.</p>',
                fmt_n(ms$total_revenue))
      )
    } else '<div style="font-size:12px;color:#8A8780;">No missed station-days this week \u2014 all active stations posted data every day.</div>'

    message("[weekly] step 7b: missed days trend across weeks")
    ## Trend of missed days and lost revenue per week
    week_labels <- vapply(week_kpi_list, function(x) format(x$end, "%d %b"), character(1))
    missed_per_week <- lapply(weeks_with_data, function(w) {
      mw <- missed_sales(w$start, w$end)
      list(days = if (is.data.frame(mw$table) && nrow(mw$table) > 0)
                    sum(mw$table$missed_days) else 0L,
           lost = mw$total_revenue %||% 0)
    })
    missed_days_series <- vapply(missed_per_week, function(x) as.integer(x$days), integer(1))
    missed_rev_series  <- vapply(missed_per_week, function(x) as.numeric(x$lost), numeric(1))

    missed_days_svg <- overall_trend_with_pct(missed_days_series, week_labels,
      title = "Missed station-days per week", y_format = "count",
      accent = "#A8323A", height = 160)
    missed_rev_svg <- overall_trend_with_pct(missed_rev_series, week_labels,
      title = "Estimated lost revenue per week", y_format = "naira",
      accent = "#A8323A", height = 160)

    ## Generator-fuel trend across weeks (network total litres per week)
    gen_per_week <- vapply(weeks_with_data, function(w) {
      d <- report_data(w$start, w$end)
      if (is.null(d$generator) || nrow(d$generator) == 0) return(0)
      as.numeric(sum(d$generator$litres, na.rm = TRUE))
    }, numeric(1))
    gen_value_per_week <- vapply(weeks_with_data, function(w) {
      d <- report_data(w$start, w$end)
      if (is.null(d$generator) || nrow(d$generator) == 0) return(0)
      as.numeric(sum(d$generator$value_naira, na.rm = TRUE))
    }, numeric(1))
    gen_litres_svg <- overall_trend_with_pct(gen_per_week, week_labels,
      title = "Generator fuel litres per week", y_format = "litres",
      accent = "#B8862B", height = 160)
    gen_value_svg <- overall_trend_with_pct(gen_value_per_week, week_labels,
      title = "Generator fuel estimated value per week", y_format = "naira",
      accent = "#B8862B", height = 160)

    ## High-sev issues mini-trend across weeks
    hi <- vapply(weeks_with_data, function(w) {
      as.integer(db_get(sprintf(
        "select count(*) as n from issues i
          join daily_reports dr on dr.id = i.report_id
         where i.severity = 'high'
           and dr.report_date between '%s' and '%s';",
        as.character(w$start), as.character(w$end)))$n)
    }, integer(1))
    issue_trend <- overall_trend_with_pct(
      as.numeric(hi),
      vapply(weeks_with_data, function(w) format(w$end, "%d %b"), character(1)),
      title = "High-severity issues per week",
      y_format = "count", accent = "#5C5A52", height = 160)

    message("[weekly] step 8: overall trend lines")
    ## Overall trend lines (network-wide) — raw value + WoW% labels on each point
    revenue_series  <- vapply(week_kpi_list, function(x) as.numeric(x$revenue),  numeric(1))
    litres_series   <- vapply(week_kpi_list, function(x) as.numeric(x$litres),   numeric(1))
    expenses_series <- vapply(week_kpi_list, function(x) as.numeric(x$expenses), numeric(1))

    overall_rev_svg <- overall_trend_with_pct(revenue_series, week_labels,
      title = "Network revenue across weeks", y_format = "naira",
      accent = "#1F4D2C")
    overall_litres_svg <- overall_trend_with_pct(litres_series, week_labels,
      title = "Network litres sold across weeks", y_format = "litres",
      accent = "#B8862B")
    overall_exp_svg <- overall_trend_with_pct(expenses_series, week_labels,
      title = "Network expenses across weeks", y_format = "naira",
      accent = "#A8323A")

    ## Date labels for sharper titles
    cur_lbl <- format(a$end, "%d %b")
    prv_lbl <- format(b$end, "%d %b")

    weekly_html <- paste0(
      '<div class="section" id="weekly-cmp"><div class="wrap">',
      '<h2>Weekly comparison</h2>',
      '<p class="lead">Week-over-week movement, station-level trends, missed-sales estimate, and issue trajectory. Only included once at least two weeks of data exist.</p>',

      '<h3>Latest week vs prior</h3>', week_kpi_strip,

      ## Overall trend lines
      if (nzchar(overall_rev_svg))
        paste0('<h3>Revenue \u00B7 network-wide trend</h3>',
               '<p class="lead">Total revenue per week with week-over-week percentage change labelled at each point.</p>',
               overall_rev_svg) else "",

      ## Per-station revenue trend
      if (nzchar(week_trend_svg)) paste0('<h3>Revenue \u00B7 per-station breakdown</h3>', week_trend_svg) else "",

      sprintf('<h3>Revenue \u00B7 week of %s vs week of %s, per station</h3>', cur_lbl, prv_lbl), rev_wow,

      if (nzchar(overall_litres_svg))
        paste0('<h3>Litres sold \u00B7 network-wide trend</h3>',
               '<p class="lead">Total litres dispatched per week with WoW % change at each point.</p>',
               overall_litres_svg) else "",

      sprintf('<h3>Litres sold \u00B7 week of %s vs week of %s, per station</h3>', cur_lbl, prv_lbl), litres_wow,

      if (nzchar(overall_exp_svg))
        paste0('<h3>Expenses \u00B7 network-wide trend</h3>',
               '<p class="lead">Total expenses per week with WoW % change at each point.</p>',
               overall_exp_svg) else "",

      if (nzchar(missed_days_svg))
        paste0('<h3>Missed station-days \u00B7 trend across weeks</h3>',
               '<p class="lead">Total station-days where no data was posted, per week. Detail for the most recent week sits in that week\'s section below.</p>',
               missed_days_svg) else "",
      if (nzchar(missed_rev_svg))
        paste0('<h3>Estimated lost revenue \u00B7 trend across weeks</h3>',
               missed_rev_svg) else "",

      if (nzchar(gen_litres_svg))
        paste0('<h3>Generator fuel litres \u00B7 trend across weeks</h3>',
               '<p class="lead">Total litres of PMS consumed by station generators per week. Value at current PMS price shown alongside each point.</p>',
               gen_litres_svg) else "",

      if (nzchar(issue_trend)) paste0('<h3>High-severity issues over time</h3>', issue_trend) else "",

      '</div></div>'
    )
  }

  ## =====================================================================
  ## SECTION 4: Weekly archive (reverse chronological)
  ## =====================================================================
  message("[consolidated] entering weekly archive, weeks=", length(weeks_with_data))
  weekly_archive_html <- ""
  archive_styles <- ""
  for (idx in seq_along(rev(weeks_with_data))) {
    w <- rev(weeks_with_data)[[idx]]
    block <- generate_report("Weekly", w$start, w$end, out_dir = tempdir())
    ## On first iteration, grab the <style>...</style> block once
    if (!nzchar(archive_styles)) {
      style_match <- regmatches(block$html_content,
        regexpr("(?s)<style>(.*?)</style>", block$html_content, perl = TRUE))
      if (length(style_match) > 0) archive_styles <- style_match
    }
    ## Extract <body><div class="wrap">...</div></body> inner from block$html
    inner <- block$html_content
    body_match <- regmatches(inner,
      regexpr("(?s)<body>(.*?)</body>", inner, perl = TRUE))
    if (length(body_match) > 0) {
      inner <- sub("<body>", "", body_match)
      inner <- sub("</body>", "", inner)
    }
    weekly_archive_html <- paste0(weekly_archive_html,
      sprintf('<div class="section" id="%s">', w$anchor),
      inner,
      '</div>')
  }

  ## Override old report's paper colour to the new one
  if (nzchar(archive_styles)) {
    archive_styles <- paste0(archive_styles,
      sprintf('<style>body{background:%s !important;} .wrap{background:%s !important;}</style>',
              kf_paper_v2, kf_paper_v2))
  }

  ## Combine
  full_html <- paste0(
    '<!DOCTYPE html><html><head><meta charset="utf-8"><title>KifayatOps Consolidated Report</title>',
    css,
    archive_styles,
    '</head><body>',
    cover_html,
    monthly_html,
    weekly_html,
    weekly_archive_html,
    '</body></html>')

  fname <- sprintf("KifayatOps_Consolidated_%s.html", format(ed, "%Y%m%d"))
  html_path <- file.path(out_dir, fname)
  writeLines(full_html, html_path, useBytes = TRUE)

  pdf_path <- sub("\\.html$", ".pdf", html_path)
  pdf_ok <- tryCatch({
    if (requireNamespace("pagedown", quietly = TRUE)) {
      pagedown::chrome_print(input = normalizePath(html_path),
        output = normalizePath(pdf_path, mustWork = FALSE),
        format = "pdf", verbose = 0, wait = 1)
      file.exists(pdf_path)
    } else FALSE
  }, error = function(e) FALSE)

  ## Build a full-range summary for the email
  total_summary <- week_kpis(sd, ed)
  list(html_path = html_path, html_content = full_html,
       pdf_path = if (isTRUE(pdf_ok)) pdf_path else NULL,
       summary = list(period = "Consolidated",
                      range_start = sd, range_end = ed,
                      revenue       = total_summary$revenue,
                      litres        = total_summary$litres,
                      pos           = total_summary$pos,
                      cash_to_bank  = total_summary$cash_to_bank,
                      expenses      = total_summary$expenses,
                      open_issues   = total_summary$high_issues,
                      total_weeks   = total_weeks,
                      total_months  = total_months,
                      show_weekly_cmp  = show_weekly_cmp,
                      show_monthly_cmp = show_monthly_cmp))
}


## ---- minor trend SVG helper (small inline line chart) ---------------------
make_minor_trend <- function(xs, ys, x_labels = NULL, label_y = "") {
  if (length(ys) < 2) return("")
  W <- 720; H <- 110; PL <- 50; PR <- 16; PT <- 14; PB <- 28
  max_y <- max(ys, 1)
  xs_n <- seq_along(ys)
  x_scale <- function(i) PL + (W - PL - PR) * (i - 1) / max(1, length(xs_n) - 1)
  y_scale <- function(v) H - PB - (H - PT - PB) * (v / max_y)

  pts <- paste(mapply(function(i, y) sprintf("%.1f,%.1f", x_scale(i), y_scale(y)),
                      xs_n, ys), collapse = " ")
  dots <- paste(mapply(function(i, y) sprintf(
    '<circle cx="%.1f" cy="%.1f" r="2.5" fill="#5C5A52"><title>%d</title></circle>',
    x_scale(i), y_scale(y), as.integer(y)), xs_n, ys), collapse = "")
  x_labs <- if (!is.null(x_labels))
    paste(vapply(seq_along(x_labels), function(i) sprintf(
      '<text x="%.1f" y="%d" text-anchor="middle" font-size="10" fill="#8A8780" font-family="Inter">%s</text>',
      x_scale(i), H - 10, x_labels[i]), character(1)), collapse = "")
    else ""
  y_max_lbl <- sprintf('<text x="%d" y="%.1f" font-size="10" fill="#8A8780" font-family="Inter" text-anchor="end">%d</text>',
    PL - 6, y_scale(max_y) + 3, as.integer(max_y))
  y_zero_lbl <- sprintf('<text x="%d" y="%.1f" font-size="10" fill="#8A8780" font-family="Inter" text-anchor="end">0</text>',
    PL - 6, y_scale(0) + 3)

  sprintf('<svg viewBox="0 0 %d %d" preserveAspectRatio="none" style="width:100%%;height:120px;">
    <line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#EFEDE6" stroke-width="0.5"/>
    <line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#EFEDE6" stroke-width="0.5"/>
    <polyline points="%s" fill="none" stroke="#5C5A52" stroke-width="1.4"/>%s%s%s%s</svg>',
    W, H, PL, y_scale(0), W - PR, y_scale(0),
    PL, y_scale(max_y), W - PR, y_scale(max_y),
    pts, dots, x_labs, y_max_lbl, y_zero_lbl)
}


## ---- Overall trend with raw value + %-change labels at each point ---------
overall_trend_with_pct <- function(values, labels, title = "",
                                   y_format = c("naira", "litres", "count"),
                                   accent = "#1F4D2C", height = 200) {
  y_format <- match.arg(y_format)
  if (length(values) < 2) return("")
  values <- as.numeric(values)
  if (all(is.na(values)) || max(values, na.rm = TRUE) <= 0) return("")

  W <- 720; H <- height; PL <- 70; PR <- 24; PT <- 24; PB <- 42
  max_y <- max(values, na.rm = TRUE) * 1.15
  x_scale <- function(i) PL + (W - PL - PR) * (i - 1) / max(1, length(values) - 1)
  y_scale <- function(v) H - PB - (H - PT - PB) * (v / max_y)

  fmt_v <- function(v) {
    if (is.na(v)) return("\u2014")
    if (y_format == "naira") {
      if (v >= 1e6) sprintf("\u20A6%.1fM", v/1e6)
      else if (v >= 1e3) sprintf("\u20A6%.0fk", v/1e3)
      else paste0("\u20A6", format(round(v), big.mark = ","))
    } else if (y_format == "litres") {
      if (v >= 1e3) sprintf("%.1fk L", v/1e3)
      else paste0(format(round(v, 1), big.mark = ","), " L")
    } else {
      as.character(as.integer(v))
    }
  }
  fmt_y_axis <- function(v) {
    if (y_format == "naira") {
      if (v >= 1e6) sprintf("\u20A6%.1fM", v/1e6)
      else if (v >= 1e3) sprintf("\u20A6%.0fk", v/1e3)
      else paste0("\u20A6", format(round(v), big.mark = ","))
    } else if (y_format == "litres") {
      if (v >= 1e3) sprintf("%.0fk", v/1e3)
      else format(round(v), big.mark = ",")
    } else {
      as.character(as.integer(v))
    }
  }

  ## Y-axis gridlines
  y_steps <- pretty(c(0, max_y), n = 4)
  y_lines <- vapply(y_steps, function(v) sprintf(
    '<line x1="%d" y1="%.1f" x2="%d" y2="%.1f" stroke="#EFEDE6" stroke-width="0.5"/>
     <text x="%d" y="%.1f" font-size="10" fill="#8A8780" font-family="Inter" text-anchor="end">%s</text>',
    PL, y_scale(v), W - PR, y_scale(v), PL - 8, y_scale(v) + 3, fmt_y_axis(v)),
    character(1))

  ## Polyline + points
  pts <- character(0)
  dots <- character(0)
  pct_labels <- character(0)
  for (i in seq_along(values)) {
    v <- values[i]
    if (!is.na(v)) {
      x <- x_scale(i); y <- y_scale(v)
      pts <- c(pts, sprintf("%.1f,%.1f", x, y))
      dots <- c(dots, sprintf(
        '<circle cx="%.1f" cy="%.1f" r="3.5" fill="%s" stroke="#FBFAF6" stroke-width="1.5"><title>%s: %s</title></circle>',
        x, y, accent, labels[i], fmt_v(v)))
      ## Value + %-change label vs prior point
      if (i > 1 && !is.na(values[i - 1]) && values[i - 1] != 0) {
        pct <- 100 * (v - values[i - 1]) / values[i - 1]
        color <- if (pct >= 0) "#3D7050" else "#A8323A"
        arrow <- if (pct >= 0) "\u25B2" else "\u25BC"
        pct_labels <- c(pct_labels, sprintf(
          '<text x="%.1f" y="%.1f" text-anchor="middle" font-size="9.5" font-family="Inter" font-weight="500">
            <tspan fill="#2A2A28">%s</tspan>
            <tspan fill="%s" dx="4">%s %.1f%%</tspan>
          </text>',
          x, y - 9, fmt_v(v), color, arrow, abs(pct)))
      } else {
        ## First point — show value only
        pct_labels <- c(pct_labels, sprintf(
          '<text x="%.1f" y="%.1f" text-anchor="middle" font-size="9.5" fill="#2A2A28" font-family="Inter" font-weight="500">%s</text>',
          x, y - 9, fmt_v(v)))
      }
    }
  }

  ## X-axis labels
  x_labs <- vapply(seq_along(labels), function(i) sprintf(
    '<text x="%.1f" y="%d" text-anchor="middle" font-size="10" fill="#8A8780" font-family="Inter">%s</text>',
    x_scale(i), H - 14, labels[i]), character(1))

  sprintf('<div style="margin-top:6px;">%s
    <svg viewBox="0 0 %d %d" preserveAspectRatio="none" style="width:100%%;height:%dpx;">
      %s
      <polyline points="%s" fill="none" stroke="%s" stroke-width="1.8" stroke-linejoin="round" stroke-linecap="round"/>
      %s %s %s
    </svg></div>',
    if (nzchar(title)) sprintf('<div style="font-size:12px;color:#5C5A52;margin-bottom:6px;">%s</div>', title) else "",
    W, H, height,
    paste(y_lines, collapse = ""),
    paste(pts, collapse = " "), accent,
    paste(dots, collapse = ""),
    paste(pct_labels, collapse = ""),
    paste(x_labs, collapse = ""))
}

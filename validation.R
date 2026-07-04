## ============================================================================
## R/validation.R — Validation engine
##
## Pure(ish) functions that, given a report_id, compute a tibble of issues
## with: issue_type, severity, variance, note. Caller writes them to the
## `issues` table.
##
## All checks are tank-aware where applicable. Cross-day continuity uses
## the previous calendar day's last block for the same tank.
## ============================================================================

## Rounding window: Kifayat rounds revenue UP to nearest \u20A610.
## A reported number is "acceptable" if exact <= reported <= ceil(exact/10)*10
## with a tiny tolerance (0.5).
in_rounding_window <- function(reported, exact, tol = 0.51) {
  if (is.na(reported) || is.na(exact)) return(NA)
  upper <- ceiling(exact / 10) * 10 + tol
  lower <- floor(exact) - tol
  reported >= lower && reported <= upper
}

## ---------------------------------------------------------------------------
## Main entry: returns a tibble of issues for one report. Empty if no issues.
## ---------------------------------------------------------------------------
validate_report <- function(report_id) {
  rep <- db_get(
    "select id, station_id, report_date, block_number
     from daily_reports where id = $1;",
    params = list(report_id)
  )
  if (nrow(rep) == 0) return(tibble::tibble())

  issues <- tibble::tibble(
    issue_type = character(),
    severity   = character(),
    variance   = numeric(),
    note       = character()
  )
  add_issue <- function(type, severity, variance = NA_real_, note = "") {
    issues <<- dplyr::bind_rows(issues, tibble::tibble(
      issue_type = type, severity = severity,
      variance = variance, note = note
    ))
  }

  ## Pull everything tied to this report ------------------------------------
  tanks_in_play <- db_get(
    "select distinct t.id as tank_id, t.tank_number, p.code as product_code
     from tanks t
     join products p on p.id = t.product_id
     left join dips d on d.tank_id = t.id and d.report_id = $1
     left join pump_sales ps on ps.report_id = $1
     left join pumps pm on pm.id = ps.pump_id
     where t.station_id = $2
       and (d.id is not null or pm.tank_id = t.id);",
    params = list(report_id, rep$station_id)
  )

  for (i in seq_len(nrow(tanks_in_play))) {
    tid    <- tanks_in_play$tank_id[i]
    tnum   <- tanks_in_play$tank_number[i]
    pcode  <- tanks_in_play$product_code[i]
    tlabel <- paste0(pcode, " Tank ", tnum)

    ## --- gather this tank's numbers for the report ------------------------
    dip <- db_get(
      "select opening_dip, closing_dip from dips where tank_id = $1 and report_id = $2;",
      params = list(tid, report_id)
    )
    open_dip  <- if (nrow(dip) > 0) dip$opening_dip[1]  else NA_real_
    close_dip <- if (nrow(dip) > 0) dip$closing_dip[1]  else NA_real_

    pump_litres <- db_get(
      "select coalesce(sum(ps.litres_sold), 0) as litres
       from pump_sales ps
       join pumps pm on pm.id = ps.pump_id
       where pm.tank_id = $1 and ps.report_id = $2;",
      params = list(tid, report_id)
    )$litres

    deduct_litres <- db_get(
      "select coalesce(sum(litres), 0) as litres
       from deductions where tank_id = $1 and report_id = $2;",
      params = list(tid, report_id)
    )$litres

    delivery_litres <- db_get(
      "select coalesce(sum(quantity_litres), 0) as in_l,
              coalesce(sum(shortage_litres), 0) as shortage
       from deliveries where tank_id = $1 and report_id = $2;",
      params = list(tid, report_id)
    )

    transload_litres <- db_get(
      "select coalesce(sum(quantity_litres), 0) as out_l
       from transloads where tank_id = $1 and report_id = $2;",
      params = list(tid, report_id)
    )$out_l

    net_litres <- pump_litres - deduct_litres
    delivered  <- delivery_litres$in_l - delivery_litres$shortage
    transloaded_out <- transload_litres

    ## --- check #1 / #2: same-day dip variance -----------------------------
    ## Opening dip is captured at start of this block; if a delivery already
    ## happened it is reflected in the opening dip itself. So same-day
    ## continuity is: opening - sold - transload = expected closing.
    if (!is.na(open_dip) && !is.na(close_dip)) {
      expected_close <- open_dip - pump_litres - transloaded_out
      variance <- close_dip - expected_close
      if (abs(variance) > 1) {
        add_issue(
          type = "dip_variance_same_day",
          severity = if (abs(variance) > 50) "high" else "medium",
          variance = variance,
          note = sprintf(
            "%s: opening %s, expected closing %s, reported closing %s",
            tlabel,
            fmt_litres(open_dip), fmt_litres(expected_close),
            fmt_litres(close_dip)
          )
        )
      }
    } else if (is.na(open_dip)) {
      add_issue(
        type = "missing_opening_dip",
        severity = "low",
        note = sprintf("%s: no opening dip recorded", tlabel)
      )
    }

    ## --- check #1a (same-day cross-block): if there's an earlier block today
    ## for this tank, the math should match against THAT closing dip, not
    ## yesterday's. Skip cross-day check entirely when same-day prior exists.
    same_day_prior <- db_get(
      "select d.closing_dip, dr.block_number
       from dips d
       join daily_reports dr on dr.id = d.report_id
       where d.tank_id = $1
         and dr.report_date = $2::date
         and dr.block_number < $3
         and d.closing_dip is not null
       order by dr.block_number desc
       limit 1;",
      params = list(tid, as.character(rep$report_date),
                    as.integer(rep$block_number))
    )

    if (nrow(same_day_prior) > 0 && !is.na(close_dip)) {
      prior_close <- same_day_prior$closing_dip[1]
      prior_block <- same_day_prior$block_number[1]

      ## Refill / dip reset between blocks?
      if (!is.na(open_dip) && abs(open_dip - prior_close) > 50) {
        add_issue(
          type = "tank_dip_reset",
          severity = "low",
          variance = open_dip - prior_close,
          note = sprintf(
            "%s: block %d closing %s, block %d opening %s (gap %s) \u2014 likely refill or new dip reading between blocks.",
            tlabel, as.integer(prior_block), fmt_litres(prior_close),
            as.integer(rep$block_number), fmt_litres(open_dip),
            fmt_litres(open_dip - prior_close))
        )
      } else {
        expected_close_xblock <- prior_close + delivered - pump_litres - transloaded_out
        variance_xblock <- close_dip - expected_close_xblock
        if (abs(variance_xblock) > 1) {
          add_issue(
            type = "dip_variance_cross_block",
            severity = if (abs(variance_xblock) > 50) "high" else "medium",
            variance = variance_xblock,
            note = sprintf(
              "%s: block %d closing %s + delivered %s - sold %s - transload %s = expected %s, block %d reported %s",
              tlabel, as.integer(prior_block), fmt_litres(prior_close),
              fmt_litres(delivered), fmt_litres(pump_litres),
              fmt_litres(transloaded_out),
              fmt_litres(expected_close_xblock),
              as.integer(rep$block_number), fmt_litres(close_dip))
          )
        }
      }
      ## Cross-day check is irrelevant when a same-day prior block exists
      yest <- data.frame()
    } else {

    ## --- check #1 (cross-day): only the immediately preceding day ---------
    yest <- db_get(
      "select d.closing_dip
       from dips d
       join daily_reports dr on dr.id = d.report_id
       where d.tank_id = $1
         and dr.report_date = ($2::date - interval '1 day')::date
         and d.closing_dip is not null
       order by dr.block_number desc
       limit 1;",
      params = list(tid, as.character(rep$report_date))
    )
    if (nrow(yest) > 0 && !is.na(close_dip)) {
      yest_close <- yest$closing_dip[1]

      ## If today has its own opening dip that doesn't match yesterday's closing
      ## (within tolerance), assume an undocumented refill happened overnight.
      ## Flag it informationally and skip the variance check.
      refill_detected <- FALSE
      if (!is.na(open_dip) && abs(open_dip - yest_close) > 50) {
        delta <- open_dip - yest_close
        add_issue(
          type = "tank_dip_reset",
          severity = "low",
          variance = delta,
          note = sprintf(
            "%s: yesterday closing %s, today opening %s (gap %s) \u2014 likely undocumented refill or new dip reading.",
            tlabel, fmt_litres(yest_close), fmt_litres(open_dip),
            fmt_litres(delta))
        )
        refill_detected <- TRUE
      }

      if (!refill_detected) {
        expected_close_xday <- yest_close + delivered - pump_litres - transloaded_out
        variance_xday <- close_dip - expected_close_xday
        if (abs(variance_xday) > 1) {
          add_issue(
            type = "dip_variance_cross_day",
            severity = if (abs(variance_xday) > 50) "high" else "medium",
            variance = variance_xday,
            note = sprintf(
              "%s: prior closing %s + delivered %s - sold %s - transload %s = expected %s, reported %s",
              tlabel,
              fmt_litres(yest_close), fmt_litres(delivered),
              fmt_litres(pump_litres), fmt_litres(transloaded_out),
              fmt_litres(expected_close_xday), fmt_litres(close_dip)
            )
          )
        }
      }
    }
    }  ## end else block (no same-day prior)

    ## --- check #4: revenue math per tank ----------------------------------
    pay <- db_get(
      "select pos_naira, cash_naira, reported_revenue
       from payments where tank_id = $1 and report_id = $2;",
      params = list(tid, report_id)
    )
    if (nrow(pay) > 0 && !is.na(pay$reported_revenue[1])) {
      price <- db_get(
        "select price_per_litre from prices
         where station_id = $1
           and product_id = (select product_id from tanks where id = $2)
           and effective_from <= $3
           and (effective_to is null or effective_to >= $3)
         order by effective_from desc limit 1;",
        params = list(rep$station_id, tid, as.character(rep$report_date))
      )
      if (nrow(price) > 0) {
        exact_rev <- net_litres * price$price_per_litre[1]
        reported  <- pay$reported_revenue[1]
        if (!isTRUE(in_rounding_window(reported, exact_rev))) {
          variance_n <- reported - exact_rev
          add_issue(
            type = "revenue_math",
            severity = if (abs(variance_n) >= 5000) "high" else "medium",
            variance = variance_n,
            note = sprintf(
              "%s: %s L \u00D7 \u20A6%s = expected %s, reported %s",
              tlabel,
              fmt_litres(net_litres),
              format(price$price_per_litre[1], big.mark = ","),
              fmt_naira(exact_rev), fmt_naira(reported)
            )
          )
        }
      }
    }

    ## --- check #6: delivery shortage logged ------------------------------
    if (delivery_litres$shortage > 0) {
      add_issue(
        type = "delivery_shortage",
        severity = if (delivery_litres$shortage > 200) "high" else "low",
        variance = -delivery_litres$shortage,
        note = sprintf("%s: shortage of %s on delivery",
                       tlabel, fmt_litres(delivery_litres$shortage))
      )
    }
  }

  ## --- check #5: cash math (report-wide) ---------------------------------
  totals <- db_get(
    "select coalesce(sum(reported_revenue), 0) as revenue,
            coalesce(sum(pos_naira),         0) as pos,
            coalesce(sum(cash_naira),        0) as cash
     from payments where report_id = $1;",
    params = list(report_id)
  )
  expenses_total <- db_get(
    "select coalesce(sum(amount_naira), 0) as amt from expenses where report_id = $1;",
    params = list(report_id)
  )$amt

  expected_cash <- totals$revenue - totals$pos - expenses_total
  reported_cash <- totals$cash
  if (reported_cash > 0 || expected_cash > 0) {
    if (abs(expected_cash - reported_cash) > 50) {
      add_issue(
        type = "cash_math",
        severity = if (abs(expected_cash - reported_cash) > 5000) "high" else "medium",
        variance = reported_cash - expected_cash,
        note = sprintf(
          "Expected cash %s, reported %s (revenue %s, POS %s, expenses %s)",
          fmt_naira(expected_cash), fmt_naira(reported_cash),
          fmt_naira(totals$revenue), fmt_naira(totals$pos),
          fmt_naira(expenses_total)
        )
      )
    }
  }

  ## --- check #9: pump meter sanity ---------------------------------------
  bad_pumps <- db_get(
    "select pm.pump_number, ps.opening_meter, ps.closing_meter
     from pump_sales ps
     join pumps pm on pm.id = ps.pump_id
     where ps.report_id = $1 and ps.closing_meter < ps.opening_meter;",
    params = list(report_id)
  )
  if (nrow(bad_pumps) > 0) {
    for (i in seq_len(nrow(bad_pumps))) {
      add_issue(
        type = "pump_meter_reverse",
        severity = "high",
        variance = bad_pumps$closing_meter[i] - bad_pumps$opening_meter[i],
        note = sprintf("P.%s: closing %s < opening %s",
                       bad_pumps$pump_number[i],
                       bad_pumps$closing_meter[i],
                       bad_pumps$opening_meter[i])
      )
    }
  }

  ## --- check #3: pump continuity vs prior day ----------------------------
  prior_meters <- db_get(
    "select ps.pump_id, ps.closing_meter as prior_close, pm.pump_number
     from pump_sales ps
     join pumps pm on pm.id = ps.pump_id
     join daily_reports dr on dr.id = ps.report_id
     where dr.station_id = $1
       and dr.report_date < $2
       and ps.pump_id in (
         select pump_id from pump_sales where report_id = $3
       )
     order by dr.report_date desc, dr.block_number desc;",
    params = list(rep$station_id, as.character(rep$report_date), report_id)
  )
  if (nrow(prior_meters) > 0) {
    prior_meters <- prior_meters %>%
      dplyr::group_by(pump_id) %>%
      dplyr::slice(1) %>%
      dplyr::ungroup()
    today_open <- db_get(
      "select pump_id, opening_meter, pm.pump_number
       from pump_sales ps join pumps pm on pm.id = ps.pump_id
       where report_id = $1;",
      params = list(report_id)
    )
    joined <- dplyr::inner_join(today_open, prior_meters, by = "pump_id")
    for (i in seq_len(nrow(joined))) {
      if (!is.na(joined$opening_meter[i]) && !is.na(joined$prior_close[i])) {
        diff <- joined$opening_meter[i] - joined$prior_close[i]
        if (diff < -0.5) {
          add_issue(
            type = "pump_continuity_gap",
            severity = "high",
            variance = diff,
            note = sprintf(
              "P.%s: today opening %s < prior closing %s",
              joined$pump_number.x[i],
              joined$opening_meter[i], joined$prior_close[i]
            )
          )
        }
      }
    }
  }

  issues
}

## ---------------------------------------------------------------------------
## Persist computed issues, replacing prior auto-generated ones.
## We keep resolved/ignored issues; only replace open ones.
## ---------------------------------------------------------------------------
persist_issues <- function(report_id, issues) {
  ## Wipe ALL issues for this report (open, resolved, ignored) and rewrite.
  db_exec("delete from issues where report_id = $1;",
          params = list(report_id))
  if (nrow(issues) == 0) return(invisible(0))

  ## Bulk insert: build single multi-row VALUES statement.
  n <- nrow(issues)
  placeholders <- paste(sprintf("($%d,$%d,$%d,$%d,$%d)",
                                seq(1, 5*n, 5),
                                seq(2, 5*n, 5),
                                seq(3, 5*n, 5),
                                seq(4, 5*n, 5),
                                seq(5, 5*n, 5)),
                        collapse = ", ")
  sql <- paste0(
    "insert into issues (report_id, issue_type, severity, variance, note) values ",
    placeholders, ";")
  params <- vector("list", 5 * n)
  for (i in seq_len(n)) {
    base <- (i - 1) * 5
    params[[base + 1]] <- report_id
    params[[base + 2]] <- issues$issue_type[i]
    params[[base + 3]] <- issues$severity[i]
    params[[base + 4]] <- if (is.na(issues$variance[i])) NA_real_ else issues$variance[i]
    params[[base + 5]] <- issues$note[i]
  }
  db_exec(sql, params = params)
  nrow(issues)
}

## ---------------------------------------------------------------------------
## Convenience: validate + persist in one call.
## ---------------------------------------------------------------------------
run_validation <- function(report_id) {
  issues <- validate_report(report_id)
  persist_issues(report_id, issues)
}

## ---------------------------------------------------------------------------
## Re-validate every existing report. Used once after deploying / fixing logic.
## ---------------------------------------------------------------------------
revalidate_all <- function() {
  ids <- db_get("select id from daily_reports order by report_date;")$id
  total <- 0
  for (rid in ids) total <- total + run_validation(rid)
  total
}

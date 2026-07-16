## ============================================================================
## R/validation.R — Validation engine (bulk-fetch, in-memory version)
##
## Design
##   * `revalidate_all()` and `run_validation()` both call `load_snapshot()`
##     which pulls every table in ~10 queries.
##   * All checks operate on in-memory data frames (dplyr::filter).
##   * Issues are wiped and re-written in one bulk statement per report.
##
## Two entry points:
##   * revalidate_all()               — validates every report (recommended)
##   * run_validation(report_id)      — single report (still bulk-fetches once)
## ============================================================================

## ---------- Rounding window: revenue is rounded UP to nearest ₦10 -----------
in_rounding_window <- function(reported, exact, tol = 0.51) {
  if (is.na(reported) || is.na(exact)) return(NA)
  upper <- ceiling(exact / 10) * 10 + tol
  lower <- floor(exact) - tol
  reported >= lower && reported <= upper
}

## ---------------------------------------------------------------------------
## Load ONE snapshot of every table needed by validation.
## Returns a list of tibbles.
## ---------------------------------------------------------------------------
load_snapshot <- function() {
  reports <- db_get("select id, station_id, report_date, block_number
                     from daily_reports;")
  reports$report_date <- as.Date(reports$report_date)
  reports$block_number <- as.integer(reports$block_number)

  prices <- db_get("select station_id, product_id,
                           effective_from, effective_to, price_per_litre
                    from prices;")
  prices$effective_from <- as.Date(prices$effective_from)
  prices$effective_to   <- as.Date(prices$effective_to)

  list(
    reports     = reports,
    stations    = db_get("select id, name from stations;"),
    tanks       = db_get("select id, station_id, product_id, tank_number
                         from tanks;"),
    products    = db_get("select id, code from products;"),
    pumps       = db_get("select id, tank_id, pump_number from pumps;"),
    dips        = db_get("select tank_id, report_id,
                                 opening_dip, closing_dip
                         from dips;"),
    pump_sales  = db_get("select pump_id, report_id,
                                 opening_meter, closing_meter,
                                 (closing_meter - opening_meter) as litres_sold
                         from pump_sales;"),
    deductions  = db_get("select tank_id, report_id, litres from deductions;"),
    deliveries  = db_get("select tank_id, report_id,
                                 quantity_litres, shortage_litres
                         from deliveries;"),
    transloads  = db_get("select tank_id, report_id, quantity_litres
                         from transloads;"),
    payments    = db_get("select tank_id, report_id,
                                 pos_naira, cash_naira, reported_revenue
                         from payments;"),
    expenses    = db_get("select report_id, amount_naira from expenses;"),
    prices      = prices
  )
}

## ---------------------------------------------------------------------------
## Validate a single report using an already-loaded snapshot.
## Returns a tibble of issues.
## ---------------------------------------------------------------------------
validate_report_local <- function(report_id, snap) {
  rep <- snap$reports[snap$reports$id == report_id, , drop = FALSE]
  if (nrow(rep) == 0) return(tibble::tibble())
  rep_date  <- as.Date(rep$report_date[1])
  rep_block <- as.integer(rep$block_number[1])
  station_id <- rep$station_id[1]

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

  ## ---- Determine tanks in play for this report -----------------------------
  dips_here <- snap$dips[snap$dips$report_id == report_id, , drop = FALSE]
  pumps_here <- snap$pumps
  psales_here <- snap$pump_sales[snap$pump_sales$report_id == report_id, , drop = FALSE]
  ## Tanks with dips OR with pump sales
  tanks_via_pumps <- unique(pumps_here$tank_id[pumps_here$id %in% psales_here$pump_id])
  tank_ids <- unique(c(dips_here$tank_id, tanks_via_pumps))
  tank_ids <- tank_ids[!is.na(tank_ids)]

  tanks_meta <- snap$tanks[snap$tanks$id %in% tank_ids &
                           snap$tanks$station_id == station_id, , drop = FALSE]

  for (i in seq_len(nrow(tanks_meta))) {
    tid  <- tanks_meta$id[i]
    tnum <- tanks_meta$tank_number[i]
    pid  <- tanks_meta$product_id[i]
    pcode <- snap$products$code[snap$products$id == pid][1]
    tlabel <- paste0(pcode, " Tank ", tnum)

    ## --- gather this tank's numbers for the report --------------------------
    dip <- dips_here[dips_here$tank_id == tid, , drop = FALSE]
    open_dip  <- if (nrow(dip) > 0) dip$opening_dip[1]  else NA_real_
    close_dip <- if (nrow(dip) > 0) dip$closing_dip[1]  else NA_real_

    ## Pump litres for this tank on this report
    tank_pumps <- pumps_here$id[pumps_here$tank_id == tid]
    ps_here <- psales_here[psales_here$pump_id %in% tank_pumps, , drop = FALSE]
    pump_litres <- sum(ps_here$litres_sold, na.rm = TRUE)

    ded_here <- snap$deductions[snap$deductions$tank_id == tid &
                                snap$deductions$report_id == report_id, ,
                                drop = FALSE]
    deduct_litres <- sum(ded_here$litres, na.rm = TRUE)

    del_here <- snap$deliveries[snap$deliveries$tank_id == tid &
                                snap$deliveries$report_id == report_id, ,
                                drop = FALSE]
    delivered_in <- sum(del_here$quantity_litres, na.rm = TRUE)
    delivered_shortage <- sum(del_here$shortage_litres, na.rm = TRUE)
    delivered <- delivered_in - delivered_shortage

    trans_here <- snap$transloads[snap$transloads$tank_id == tid &
                                  snap$transloads$report_id == report_id, ,
                                  drop = FALSE]
    transloaded_out <- sum(trans_here$quantity_litres, na.rm = TRUE)

    net_litres <- pump_litres - deduct_litres

    ## --- check #1 / #2: same-day dip variance -------------------------------
    if (!is.na(open_dip) && !is.na(close_dip)) {
      expected_close <- open_dip - pump_litres - transloaded_out
      variance <- close_dip - expected_close
      if (!is.na(variance) && abs(variance) > 1) {
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

    ## --- check #1a (same-day cross-block) ----------------------------------
    ## Look for earlier block on same date for this tank
    same_day_reports <- snap$reports[snap$reports$station_id == station_id &
                                     snap$reports$report_date == rep_date &
                                     as.integer(snap$reports$block_number) < rep_block, ,
                                     drop = FALSE]
    same_day_prior <- NULL
    if (nrow(same_day_reports) > 0) {
      candidate_dips <- snap$dips[snap$dips$tank_id == tid &
                                  snap$dips$report_id %in% same_day_reports$id &
                                  !is.na(snap$dips$closing_dip), , drop = FALSE]
      if (nrow(candidate_dips) > 0) {
        merged <- merge(candidate_dips, same_day_reports,
                        by.x = "report_id", by.y = "id")
        merged <- merged[order(-as.integer(merged$block_number)), ]
        same_day_prior <- merged[1, , drop = FALSE]
      }
    }

    if (!is.null(same_day_prior) && !is.na(close_dip)) {
      prior_close <- same_day_prior$closing_dip[1]
      prior_block <- as.integer(same_day_prior$block_number[1])

      if (!is.na(open_dip) && abs(open_dip - prior_close) > 50) {
        add_issue(
          type = "tank_dip_reset",
          severity = "low",
          variance = open_dip - prior_close,
          note = sprintf(
            "%s: block %d closing %s, block %d opening %s (gap %s) — likely refill or new dip reading between blocks.",
            tlabel, prior_block, fmt_litres(prior_close),
            rep_block, fmt_litres(open_dip),
            fmt_litres(open_dip - prior_close))
        )
      } else {
        expected_xblock <- prior_close + delivered - pump_litres - transloaded_out
        v_xblock <- close_dip - expected_xblock
        if (!is.na(v_xblock) && abs(v_xblock) > 1) {
          add_issue(
            type = "dip_variance_cross_block",
            severity = if (abs(v_xblock) > 50) "high" else "medium",
            variance = v_xblock,
            note = sprintf(
              "%s: block %d closing %s + delivered %s - sold %s - transload %s = expected %s, block %d reported %s",
              tlabel, prior_block, fmt_litres(prior_close),
              fmt_litres(delivered), fmt_litres(pump_litres),
              fmt_litres(transloaded_out),
              fmt_litres(expected_xblock),
              rep_block, fmt_litres(close_dip))
          )
        }
      }
    } else {
      ## --- check #1 (cross-day): immediately preceding day ------------------
      yest_date <- rep_date - 1
      yest_reports <- snap$reports[snap$reports$station_id == station_id &
                                   snap$reports$report_date == yest_date, ,
                                   drop = FALSE]
      yest_dip <- NULL
      if (nrow(yest_reports) > 0 && !is.na(close_dip)) {
        candidate_dips <- snap$dips[snap$dips$tank_id == tid &
                                    snap$dips$report_id %in% yest_reports$id &
                                    !is.na(snap$dips$closing_dip), , drop = FALSE]
        if (nrow(candidate_dips) > 0) {
          merged <- merge(candidate_dips, yest_reports,
                          by.x = "report_id", by.y = "id")
          merged <- merged[order(-as.integer(merged$block_number)), ]
          yest_dip <- merged[1, , drop = FALSE]
        }
      }

      if (!is.null(yest_dip)) {
        yest_close <- yest_dip$closing_dip[1]
        refill_detected <- FALSE
        if (!is.na(open_dip) && abs(open_dip - yest_close) > 50) {
          delta <- open_dip - yest_close
          add_issue(
            type = "tank_dip_reset",
            severity = "low",
            variance = delta,
            note = sprintf(
              "%s: yesterday closing %s, today opening %s (gap %s) — likely undocumented refill or new dip reading.",
              tlabel, fmt_litres(yest_close), fmt_litres(open_dip),
              fmt_litres(delta))
          )
          refill_detected <- TRUE
        }
        if (!refill_detected) {
          expected_xday <- yest_close + delivered - pump_litres - transloaded_out
          v_xday <- close_dip - expected_xday
          if (!is.na(v_xday) && abs(v_xday) > 1) {
            add_issue(
              type = "dip_variance_cross_day",
              severity = if (abs(v_xday) > 50) "high" else "medium",
              variance = v_xday,
              note = sprintf(
                "%s: prior closing %s + delivered %s - sold %s - transload %s = expected %s, reported %s",
                tlabel, fmt_litres(yest_close), fmt_litres(delivered),
                fmt_litres(pump_litres), fmt_litres(transloaded_out),
                fmt_litres(expected_xday), fmt_litres(close_dip)
              )
            )
          }
        }
      }
    }

    ## --- check #4: revenue math per tank -----------------------------------
    pay <- snap$payments[snap$payments$tank_id == tid &
                         snap$payments$report_id == report_id, , drop = FALSE]
    if (nrow(pay) > 0 && !is.na(pay$reported_revenue[1])) {
      pr <- snap$prices[snap$prices$station_id == station_id &
                        snap$prices$product_id == pid &
                        snap$prices$effective_from <= rep_date &
                        (is.na(snap$prices$effective_to) |
                         snap$prices$effective_to >= rep_date), ,
                        drop = FALSE]
      if (nrow(pr) > 0) {
        pr <- pr[order(pr$effective_from, decreasing = TRUE), ]
        price_ppl <- pr$price_per_litre[1]
        exact_rev <- net_litres * price_ppl
        reported  <- pay$reported_revenue[1]
        if (!isTRUE(in_rounding_window(reported, exact_rev))) {
          variance_n <- reported - exact_rev
          add_issue(
            type = "revenue_math",
            severity = if (abs(variance_n) >= 5000) "high" else "medium",
            variance = variance_n,
            note = sprintf(
              "%s: %s L × ₦%s = expected %s, reported %s",
              tlabel, fmt_litres(net_litres),
              format(price_ppl, big.mark = ","),
              fmt_naira(exact_rev), fmt_naira(reported)
            )
          )
        }
      }
    }

    ## --- check #6: delivery shortage logged --------------------------------
    if (delivered_shortage > 0) {
      add_issue(
        type = "delivery_shortage",
        severity = if (delivered_shortage > 200) "high" else "low",
        variance = -delivered_shortage,
        note = sprintf("%s: shortage of %s on delivery",
                       tlabel, fmt_litres(delivered_shortage))
      )
    }
  }

  ## --- check #5: cash math (report-wide) -----------------------------------
  pay_here <- snap$payments[snap$payments$report_id == report_id, , drop = FALSE]
  total_rev  <- sum(pay_here$reported_revenue, na.rm = TRUE)
  total_pos  <- sum(pay_here$pos_naira, na.rm = TRUE)
  total_cash <- sum(pay_here$cash_naira, na.rm = TRUE)
  exp_here <- snap$expenses[snap$expenses$report_id == report_id, , drop = FALSE]
  expenses_total <- sum(exp_here$amount_naira, na.rm = TRUE)

  expected_cash <- total_rev - total_pos - expenses_total
  reported_cash <- total_cash
  if (!is.na(reported_cash) && !is.na(expected_cash) &&
      (reported_cash > 0 || expected_cash > 0)) {
    if (!is.na(expected_cash - reported_cash) && abs(expected_cash - reported_cash) > 50) {
      add_issue(
        type = "cash_math",
        severity = if (abs(expected_cash - reported_cash) > 5000) "high" else "medium",
        variance = reported_cash - expected_cash,
        note = sprintf(
          "Expected cash %s, reported %s (revenue %s, POS %s, expenses %s)",
          fmt_naira(expected_cash), fmt_naira(reported_cash),
          fmt_naira(total_rev), fmt_naira(total_pos),
          fmt_naira(expenses_total)
        )
      )
    }
  }

  ## --- check #9: pump meter sanity ----------------------------------------
  psales_bad <- psales_here[!is.na(psales_here$closing_meter) &
                            !is.na(psales_here$opening_meter) &
                            psales_here$closing_meter < psales_here$opening_meter, ,
                            drop = FALSE]
  if (nrow(psales_bad) > 0) {
    for (j in seq_len(nrow(psales_bad))) {
      pnum <- snap$pumps$pump_number[snap$pumps$id == psales_bad$pump_id[j]][1]
      add_issue(
        type = "pump_meter_reverse",
        severity = "high",
        variance = psales_bad$closing_meter[j] - psales_bad$opening_meter[j],
        note = sprintf("P.%s: closing %s < opening %s",
                       pnum,
                       psales_bad$closing_meter[j],
                       psales_bad$opening_meter[j])
      )
    }
  }

  ## --- check #3: pump continuity vs prior day -----------------------------
  ## Prior day's last closing meter per pump for this station
  station_reports <- snap$reports[snap$reports$station_id == station_id &
                                  snap$reports$report_date < rep_date, ,
                                  drop = FALSE]
  if (nrow(station_reports) > 0 && nrow(psales_here) > 0) {
    prior_ps <- snap$pump_sales[snap$pump_sales$report_id %in% station_reports$id &
                                snap$pump_sales$pump_id %in% psales_here$pump_id, ,
                                drop = FALSE]
    if (nrow(prior_ps) > 0) {
      prior_ps <- merge(prior_ps, station_reports,
                        by.x = "report_id", by.y = "id")
      prior_ps <- prior_ps %>%
        dplyr::arrange(dplyr::desc(as.Date(report_date)),
                       dplyr::desc(as.integer(block_number))) %>%
        dplyr::group_by(pump_id) %>%
        dplyr::slice(1) %>%
        dplyr::ungroup()
      joined <- merge(psales_here[, c("pump_id","opening_meter")],
                      prior_ps[, c("pump_id","closing_meter")],
                      by = "pump_id")
      for (j in seq_len(nrow(joined))) {
        o  <- joined$opening_meter[j]
        pc <- joined$closing_meter[j]
        if (length(o) == 1 && length(pc) == 1 &&
            !is.na(o) && !is.na(pc)) {
          diff <- o - pc
          if (diff < -0.5) {
            pnum <- snap$pumps$pump_number[snap$pumps$id == joined$pump_id[j]][1]
            add_issue(
              type = "pump_continuity_gap",
              severity = "high",
              variance = diff,
              note = sprintf(
                "P.%s: today opening %s < prior closing %s",
                pnum, o, pc
              )
            )
          }
        }
      }
    }
  }

  issues
}

## ---------------------------------------------------------------------------
## Persist issues for a single report — wipes + bulk inserts.
## ---------------------------------------------------------------------------
persist_issues <- function(report_id, issues) {
  db_exec(sprintf("delete from issues where report_id = '%s';",
                  as.character(report_id)[1]))
  if (nrow(issues) == 0) return(invisible(0))

  esc <- function(x) {
    if (is.na(x)) return("NULL")
    gsub("'", "''", as.character(x), fixed = TRUE)
  }
  esc_str <- function(x) {
    if (is.na(x)) return("NULL")
    paste0("'", gsub("'", "''", as.character(x), fixed = TRUE), "'")
  }
  esc_num <- function(x) {
    if (is.na(x) || !is.finite(x)) return("NULL")
    format(x, scientific = FALSE)
  }

  rid_s <- esc_str(report_id)
  rows <- vapply(seq_len(nrow(issues)), function(i) sprintf(
    "(%s, %s, %s, %s, %s)",
    rid_s,
    esc_str(issues$issue_type[i]),
    esc_str(issues$severity[i]),
    esc_num(issues$variance[i]),
    esc_str(issues$note[i])
  ), character(1))

  sql <- paste0(
    "insert into issues (report_id, issue_type, severity, variance, note) values ",
    paste(rows, collapse = ", "), ";")
  db_exec(sql)
  nrow(issues)
}

## ---------------------------------------------------------------------------
## Single-report validation. Bulk-fetches once, validates one report.
## ---------------------------------------------------------------------------
run_validation <- function(report_id) {
  snap <- load_snapshot()
  issues <- validate_report_local(report_id, snap)
  persist_issues(report_id, issues)
}

## ---------------------------------------------------------------------------
## Re-validate every report in one pass.
## Single snapshot; loops in-memory; wipes + writes all issues in one txn.
## ---------------------------------------------------------------------------
revalidate_all <- function() {
  snap <- load_snapshot()
  report_ids <- snap$reports$id
  message("[validate] loaded snapshot: ", length(report_ids), " reports")

  ## Collect all issues per report
  all_issues <- list()
  for (rid in report_ids) {
    all_issues[[rid]] <- validate_report_local(rid, snap)
  }

  ## Wipe issues for these reports and bulk-write in one go
  ## (chunked to keep the SQL statement size sane)
  db_exec("delete from issues;")   # wipe all — safer & simpler
  total <- 0
  for (rid in report_ids) {
    iss <- all_issues[[rid]]
    if (nrow(iss) > 0) {
      persist_issues(rid, iss)
      total <- total + nrow(iss)
    }
  }
  message("[validate] wrote ", total, " issues")
  total
}

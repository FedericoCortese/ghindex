.years_before <- function(date, years) {
  # seq.Date handles calendar years; avoid Feb 29 rolling into March.
  year <- as.integer(format(date, "%Y")) - years
  monthday <- format(date, "%m-%d")
  target <- as.Date(sprintf("%04d-%s", year, monthday), format = "%Y-%m-%d")
  if (is.na(target)) target <- as.Date(sprintf("%04d-02-28", year))
  target
}

.price_frame <- function(x) {
  if (xts::is.xts(x) || inherits(x, "zoo")) {
    if (NCOL(x) != 1L) stop("Each asset must contain a single price column.")
    d <- data.frame(date = as.Date(zoo::index(x)), price = as.numeric(x))
  } else if (is.data.frame(x) && all(c("date", "price") %in% names(x))) {
    d <- data.frame(date = as.Date(x$date), price = as.numeric(x$price))
  } else stop("Use a named list of one-column xts series or date/price data frames.")
  if (!nrow(d) || anyNA(d$date) || anyDuplicated(d$date) || is.unsorted(d$date))
    stop("Each price series needs nonempty, unique, increasing dates.")
  if (any(!is.finite(d$price)) || any(d$price <= 0))
    stop("Missing, nonfinite or nonpositive prices: clean explicitly before screening.")
  d
}

.screen_one <- function(job, dates, horizons, types, inference, lag, level,
                         min_events, annualization, risk_free, min_coverage,
                         tolerance_days) {
  d <- job$data; symbol <- job$symbol
  rows <- vector("list", length(dates) * length(horizons) * length(types))
  pos <- 0L
  for (end in as.list(dates)) for (years in horizons) {
    end <- as.Date(end, origin = "1970-01-01")
    start <- .years_before(end, years)
    take <- which(d$date >= start & d$date <= end)
    w <- d[take, , drop = FALSE]
    reason <- if (!is.null(job$error)) "invalid_prices" else if (nrow(w) < 3L) "insufficient_history" else if (
      w$date[1L] > start + tolerance_days ||
      utils::tail(w$date, 1L) < end - tolerance_days ||
      nrow(w) < min_coverage * annualization * years) "insufficient_history" else "ok"
    ret <- if (reason == "ok") diff(log(w$price)) else numeric()
    vol <- if (length(ret)) stats::sd(ret) * sqrt(annualization) else NA_real_
    sharpe <- if (is.finite(vol) && vol > 0)
      (mean(ret) * annualization - risk_free) / vol else NA_real_
    for (type in types) {
      pos <- pos + 1L
      common <- data.frame(symbol = symbol, date = end, horizon = years,
                            type = type, n_prices = nrow(w), sharpe = sharpe,
                            volatility = 100 * vol, data_status = reason,
                            data_message = if (is.null(job$error)) "" else job$error)
      if (reason == "ok") {
        dd <- DD(w$price, type)
        common$discarded_episodes <- dd$n_discarded
        loss <- -dd$dd
        if (inference) {
          est <- gh_hac(loss, lag, level, min_events,
                         positions = dd$tmin, n_obs = nrow(w), type = type)
        } else {
          gh <- ghindex(loss, FALSE)
          est <- data.frame(index = c("g", "h"), estimate = c(gh$g, gh$h),
                            continuous = NA_real_, se = NA_real_, lower = NA_real_,
                            upper = NA_real_, n_events = length(loss), n_obs = nrow(w),
                            lag = NA_integer_, bandwidth = NA_real_, status = "not_requested",
                            time_scale = "observations")
        }
      } else {
        common$discarded_episodes <- NA_integer_
        est <- data.frame(index = c("g", "h"), estimate = NA_real_,
                               continuous = NA_real_, se = NA_real_, lower = NA_real_,
                               upper = NA_real_, n_events = NA_integer_, n_obs = nrow(w),
                               lag = NA_integer_, bandwidth = NA_real_, status = reason,
                               time_scale = "observations")
      }
      rows[[pos]] <- cbind(common[rep(1L, 2L), , drop = FALSE], est)
    }
  }
  do.call(rbind, rows)
}

screen_assets <- function(prices, horizons = c(1L, 3L, 5L), dates = NULL,
                           types = 1:3, inference = TRUE, lag = NULL, level = .95,
                           min_events = 30L, annualization = 252,
                           risk_free = 0, min_coverage = .8,
                           tolerance_days = 7L, workers = 1L) {
  if (!is.list(prices) || !length(prices) || is.null(names(prices)) ||
      anyNA(names(prices)) || any(!nzchar(names(prices))) || anyDuplicated(names(prices)))
    stop("prices must be a nonempty list with unique asset names.")
  if (!length(horizons) || anyNA(horizons) || any(horizons <= 0 | horizons != floor(horizons)))
    stop("horizons must contain positive whole calendar years.")
  if (!length(types) || anyNA(types) || any(!types %in% 1:3)) stop("Invalid types.")
  if (length(workers) != 1L || !is.finite(workers) || workers < 1 || workers != floor(workers))
    stop("workers must be a positive integer.")
  if (length(annualization) != 1L || !is.finite(annualization) || annualization <= 0 ||
      length(min_coverage) != 1L || !is.finite(min_coverage) || min_coverage <= 0 || min_coverage > 1 ||
      length(tolerance_days) != 1L || !is.finite(tolerance_days) || tolerance_days < 0 ||
      length(risk_free) != 1L || !is.finite(risk_free)) stop("Invalid coverage or return settings.")
  data <- lapply(prices, function(x) tryCatch(.price_frame(x), error = identity))
  bad <- vapply(data, inherits, logical(1), "error")
  if (all(bad)) stop("No valid price series: ", conditionMessage(data[[1L]]))
  if (is.null(dates)) dates <- max(vapply(data[!bad], function(d) as.numeric(utils::tail(d$date, 1L)), 0))
  dates <- sort(unique(as.Date(dates, origin = "1970-01-01")))
  if (!length(dates) || anyNA(dates)) stop("Supply valid evaluation dates.")
  jobs <- Map(function(symbol, data) {
    if (inherits(data, "error")) list(symbol = symbol,
      data = data.frame(date = as.Date(character()), price = numeric()),
      error = conditionMessage(data)) else list(symbol = symbol, data = data)
  }, names(data), data)
  args <- list(dates = dates, horizons = unique(horizons), types = unique(types),
               inference = inference, lag = lag, level = level, min_events = min_events,
               annualization = annualization, risk_free = risk_free,
               min_coverage = min_coverage, tolerance_days = tolerance_days)
  if (workers > 1L) {
    cluster <- parallel::makePSOCKcluster(min(workers, length(jobs)))
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    result <- do.call(parallel::parLapplyLB, c(list(cl = cluster, X = jobs,
                                                  fun = .screen_one), args))
  } else result <- do.call(lapply, c(list(X = jobs, FUN = .screen_one), args))
  out <- do.call(rbind, result)
  rownames(out) <- NULL
  attr(out, "settings") <- args
  out
}

risk_table <- function(results, date = max(results$date), wide = TRUE) {
  d <- results[results$date == as.Date(date) & !is.na(results$estimate), , drop = FALSE]
  if (!nrow(d)) stop("No assets with sufficient history at the selected date.")
  keys <- interaction(d$type, d$index, d$estimate, d$horizon, drop = TRUE)
  groups <- lapply(split(d, keys), function(z) {
    avg <- function(x) if (all(is.na(x))) NA_real_ else mean(x, na.rm = TRUE)
    data.frame(type = z$type[1L], index = z$index[1L], class = z$estimate[1L],
                horizon = z$horizon[1L], n_assets = nrow(z),
                assets = paste(sort(z$symbol), collapse = ", "),
                sharpe = avg(z$sharpe), volatility = avg(z$volatility),
                mean_se = avg(z$se), n_hac = sum(is.finite(z$se)))
  })
  out <- do.call(rbind, groups)
  out <- out[order(out$type, out$index, out$class, out$horizon), ]
  rownames(out) <- NULL
  if (!wide) return(out)
  # Explicit cross-grid: absent classes have count zero, not missing counts.
  keys <- unique(out[c("type", "index", "class")])
  grid <- merge(keys, data.frame(horizon = sort(unique(results$horizon))), by = NULL)
  out <- merge(grid, out, by = c("type", "index", "class", "horizon"), all.x = TRUE)
  out$n_assets[is.na(out$n_assets)] <- 0L
  out$n_hac[is.na(out$n_hac)] <- 0L
  out$assets[is.na(out$assets)] <- ""
  out <- stats::reshape(out, idvar = c("type", "index", "class"),
                         timevar = "horizon", direction = "wide")
  metrics <- c("n_assets", "assets", "sharpe", "volatility", "mean_se", "n_hac")
  columns <- unlist(lapply(sort(unique(results$horizon)), function(h)
    paste0(metrics, ".", h)))
  out <- out[order(out$type, out$index, out$class), c("type", "index", "class", columns)]
  rownames(out) <- NULL
  out
}

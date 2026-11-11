library(ghindex)

near <- function(a, b, tolerance = 1e-8) stopifnot(isTRUE(all.equal(a, b, tolerance = tolerance, check.attributes = FALSE)))
fails <- function(expr) stopifnot(inherits(tryCatch(force(expr), error = identity), "error"))

# Exact index thresholds, ties, zero and empty input; independent definitions.
stopifnot(identical(ghindex(numeric(), FALSE), list(g = 0L, h = 0L)))
near(unlist(ghindex(c(10, 5, 3, 1), FALSE)), c(4, 3))
near(unlist(ghindex(c(1, 1, 1), FALSE)), c(1, 1))
set.seed(17)
for (n in c(1, 2, 20, 200)) {
  d <- rexp(n, .15)
  ordered <- sort(d, decreasing = TRUE)
  expected_h <- max(c(0, which(vapply(seq_len(n), function(k) sum(d >= k) >= k, TRUE))))
  expected_g <- max(c(0, which(vapply(seq_len(n), function(k) sum(ordered[seq_len(k)]) >= k^2, TRUE))))
  near(unlist(ghindex(d, FALSE)), c(expected_g, expected_h))
}
fails(ghindex(c(NA, 1), FALSE)); fails(ghindex(-1, FALSE))

# Negative loss convention, paired episodes, original type-1/type-2 definitions.
p <- c(100, 110, 90, 115, 80, 120, 100)
near(DD(p, 1)$dd, 100 * (c(90, 80, 100) / c(110, 115, 120) - 1))
near(DD(p, 2)$dd, 100 * (c(90, 80) / c(110, 115) - 1))
plateau <- DD(c(90, 100, 95, 110, 110, 105, 115), 2)
near(plateau$dd, -5)
stopifnot(plateau$n_discarded == 1L, all(plateau$dd <= 0))
stopifnot(identical(DD(p, 2)$tmax, c(2L, 4L)))
stopifnot(length(DD(1:10, 1)$dd) == 0L, length(DD(1:10, 3)$dd) == 0L)
near(DD(c(100, 90, 80), 3)$dd, -20)
stopifnot(length(DD(rep(100, 8), 2)$dd) == 0L,
          length(DD(numeric(), 1)$dd) == 0L)
fails(DD(c(1, 0), 1)); fails(DD(c(1, NA), 2)); fails(DD(1:3, 4))
p3 <- c(90, 100, 80, 110, 95, 120, 105)
near(DD(p3, 3)$dd, 100 * (c(80, 95) / c(100, 110) - 1))
stopifnot(all(DD(p3, 3)$tmax < DD(p3, 3)$tmin))
mw <- gh_mw(p, c(NA, diff(log(p))), MW = length(p))
stopifnot(nrow(mw) == 1L)
near(mw$g2, ghindex(-DD(p, 2)$dd, FALSE)$g)

# Bartlett HAC against an independently assembled covariance using all lags.
score <- cbind(c(1, 0, 3, 2, 5, 1), c(0, 2, 0, 4, 1, 3))
centered <- sweep(score, 2, colMeans(score), "-")
expected <- crossprod(centered)
for (j in 1:2) {
  cross <- matrix(0, 2, 2)
  for (t in (j + 1):nrow(score)) cross <- cross + outer(centered[t, ], centered[t - j, ])
  expected <- expected + (1 - j / 3) * (cross + t(cross))
}
near(ghindex:::.nw_sum(score, 2)$covariance, expected)
stopifnot(min(eigen(expected, symmetric = TRUE)$values) > -1e-8)

# Smoothed h equation, fractional g equation, joint covariance and time scale.
loss <- abs(as.numeric(arima.sim(list(ar = .5), 400, sd = 2))) + 2
hac <- gh_hac(loss, lag = 4)
stopifnot(all(is.finite(hac$se)), all(hac$se > 0), all(hac$lag == 4))
hc <- hac$continuous[2]; b <- hac$bandwidth[2]
near(sum(pnorm((loss - hc) / b)), hc, tolerance = 1e-4)
gc <- hac$continuous[1]; k <- floor(gc); ordered <- sort(loss, decreasing = TRUE)
near(sum(ordered[seq_len(k)]) + (gc - k) * ordered[k + 1L], gc^2)
near(diag(attr(hac, "covariance")), hac$se^2)
stopifnot(all(eigen(attr(hac, "covariance"), symmetric = TRUE)$values > -1e-8))
calendar <- gh_hac(loss, lag = 4, positions = seq(2, 800, by = 2), n_obs = 800)
stopifnot(all(calendar$time_scale == "observations"), all(is.finite(calendar$se)))
stopifnot(all(gh_hac(1:10)$status == "too_few_events"))
stopifnot(all(gh_hac(rep(100, 40))$status == "boundary"))
stopifnot(all(gh_hac(loss, type = 3)$status == "record_process_approximation"))
fails(gh_hac(loss, lag = -1)); fails(gh_hac(loss, positions = seq_along(loss)))

# Full screening pipeline, calendar horizons, exclusions and independent groups.
dates <- seq(as.Date("2018-01-01"), as.Date("2025-12-31"), by = "day")
dates <- dates[as.POSIXlt(dates)$wday %in% 1:5]
prices <- list(A = data.frame(date = dates, price = 100 * exp(cumsum(rnorm(length(dates), 0, .02)))),
               B = data.frame(date = dates, price = 100 * exp(cumsum(rnorm(length(dates), 0, .01)))))
prices$IPO <- tail(prices$A, 150)
prices$BAD <- prices$B; prices$BAD$price[20] <- NA
result <- screen_assets(prices, dates = max(dates))
stopifnot(nrow(result) == 4 * 3 * 3 * 2,
          all(result$data_status[result$symbol == "IPO"] == "insufficient_history"),
          all(result$data_status[result$symbol == "BAD"] == "invalid_prices"))
long <- risk_table(result, wide = FALSE)
totals <- aggregate(n_assets ~ type + index + horizon, long, sum)
stopifnot(all(totals$n_assets == 2))
wide <- risk_table(result)
stopifnot(all(c("n_assets.1", "sharpe.3", "volatility.5") %in% names(wide)))
for (h in c(1, 3, 5)) near(sum(wide[[paste0("n_assets.", h)]]), 12)
near(ghindex:::.years_before(as.Date("2024-02-29"), 1), as.Date("2023-02-28"))
no_future <- screen_assets(prices[1:2], horizons = 1, dates = as.Date("2024-12-31"), inference = FALSE)
shortened <- lapply(prices[1:2], function(d) d[d$date <= as.Date("2024-12-31"), ])
near(no_future, screen_assets(shortened, horizons = 1, dates = as.Date("2024-12-31"), inference = FALSE))
serial <- screen_assets(prices[1:2], horizons = 1, dates = max(dates), inference = FALSE)
parallel <- screen_assets(prices[1:2], horizons = 1, dates = max(dates), inference = FALSE, workers = 2)
near(serial, parallel)
stopifnot(inherits(plot_groups(result), "ggplot"))
timeline <- screen_assets(prices[1:2], horizons = 1,
                         dates = as.Date(c("2025-06-30", "2025-12-31")), inference = FALSE)
stopifnot(inherits(plot_risk_time(timeline), "ggplot"))
pages <- plot_asset_risk(timeline, n_assets = 1)
stopifnot(length(pages) == 3L, length(attr(pages, "symbols")) == 1L,
          all(vapply(pages, inherits, logical(1), "ggplot")))
limits <- lapply(pages, function(p) p$scales$get_scales("fill")$limits)
stopifnot(all(vapply(limits, identical, logical(1), limits[[1]])))

# Offline directory parsing: footer, test exclusion, ETF option and ticker mapping.
fixture <- tempfile(fileext = ".txt")
writeLines(c("Symbol|Security Name|Test Issue|ETF", "AAA|Ordinary|N|N", "NA|Valid ticker|N|N",
             "BBB.B|Fund|N|Y", "TEST|Test|Y|N", "File Creation Time: 20261026|||"), fixture)
directory <- nasdaq_symbols(url = fixture)
stopifnot(nrow(directory) == 3L, "BBB-B" %in% directory$yahoo_symbol,
          "NA" %in% directory$yahoo_symbol,
          nrow(nasdaq_symbols(FALSE, fixture)) == 2L)
unlink(fixture)

# Cached download path is tested without network.
cache <- tempfile(); dir.create(cache)
symbol <- "AAA"
key <- paste(sprintf("%02x", as.integer(charToRaw(symbol))), collapse = "")
x <- xts::xts(101:110, as.Date("2025-01-01") + 0:9)
saveRDS(list(symbol = symbol, from = as.Date("2025-01-01"), to = as.Date("2025-01-10"),
             prices = x, used_close = FALSE), file.path(cache, paste0(key, ".rds")))
cached <- get_prices(symbol, "2025-01-02", "2025-01-09", cache_dir = cache, pause = 0)
stopifnot(NROW(cached[[1]]) == 8L, attr(cached, "download_log")$status == "cached")
parallel_cache <- get_prices(symbol, "2025-01-02", "2025-01-09", cache_dir = cache,
                            pause = 0, workers = 2)
near(parallel_cache, cached)
unlink(cache, recursive = TRUE)

# Risk and uncertainty maps share assets, while missing SEs remain missing.
se_pages <- plot_asset_risk(result, symbols = c("A", "B"), layout = "horizon", metric = "se")
stopifnot(identical(attr(se_pages, "symbols"), c("A", "B")))
for (p in se_pages) near(p$data$value, p$data$se)
missing_se <- plot_asset_risk(timeline, symbols = "A", metric = "se")
stopifnot(all(is.na(missing_se[[1]]$data$value)))
stopifnot(length(plot_risk_series(timeline, symbols = "A")) == 3L)

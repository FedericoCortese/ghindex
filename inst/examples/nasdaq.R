# ghindex: step-by-step analysis of 20 major Nasdaq-listed companies.
# Install ghindex first, then run from the project directory:
# Rscript inst/examples/nasdaq.R
library(quantmod)
library(ghindex)

# 1. Choose the securities and period --------------------------------------
# Fixed selection of 20 prominent large Nasdaq-listed companies, not a live
# market-cap ranking. One share class per company; edit this vector to analyse
# any other Yahoo-compatible tickers. No retrospective risk-based selection.
# Nasdaq listings: https://www.nasdaqtrader.com/dynamic/symdir/nasdaqlisted.txt
symbols <- c("NVDA", "AAPL", "MSFT", "AMZN", "GOOGL", "AVGO", "META", "TSLA",
             "MU", "AMD", "ASML", "INTC", "CSCO", "LRCX", "NFLX", "AMAT",
             "PLTR", "TXN", "COST", "TMUS")
as_of <- as.Date(Sys.getenv("GHINDEX_AS_OF", as.character(Sys.Date() - 1L)))
from <- seq(as_of, by = "-5 years", length.out = 2L)[2L] - 10L
horizons <- c(1L, 3L, 5L)
rolling_horizon <- 1L
out_dir <- file.path("output", "nasdaq")
cache_dir <- file.path("cache", "nasdaq20")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
options(timeout = 45)

# 2. Extract daily OHLCV data explicitly with quantmod ----------------------
# src='yahoo' selects the provider; from/to specify the requested date range.
# auto.assign=FALSE returns the data instead of creating global variables.
# Yahoo's 'to' boundary is exclusive, so request as_of + 1 for an inclusive end.
# Cache the complete OHLCV response to avoid downloading it on every run.
ohlc <- list()
log <- vector("list", length(symbols))
for (i in seq_along(symbols)) {
  symbol <- symbols[i]
  file <- file.path(cache_dir, paste0(symbol, ".rds"))
  cached <- if (file.exists(file)) readRDS(file) else NULL
  error <- ""
  hit <- !is.null(cached) && cached$from <= from && cached$to >= as_of
  value <- if (hit) cached$data else tryCatch(
    quantmod::getSymbols(symbol, src = "yahoo", from = from, to = as_of + 1L,
                        auto.assign = FALSE),
    error = function(e) { error <<- conditionMessage(e); NULL })
  if (!is.null(value)) {
    if (!hit) saveRDS(list(from = from, to = as_of, data = value), file)
    ohlc[[symbol]] <- value[paste0(from, "/", as_of)]
  }
  log[[i]] <- data.frame(symbol = symbol,
    status = if (is.null(value)) "failed" else if (hit) "cached" else "downloaded",
    message = error)
}
downloads <- do.call(rbind, log)
write.csv(downloads, file.path(out_dir, "downloads.csv"), row.names = FALSE)
if (!length(ohlc)) stop("No prices available; see downloads.csv.")
print(head(ohlc[[1L]]))  # Open, High, Low, Close, Volume and Adjusted columns.

# 3. Select the desired price series ---------------------------------------
# Ad() extracts Adjusted Close, accounting for provider-adjusted corporate
# actions. Use Cl() instead only if unadjusted Close is your intended input.
# Keep each asset as a separate one-column xts series. Do not merge and
# silently interpolate different trading histories.
prices <- lapply(ohlc, quantmod::Ad)
print(head(prices[[1L]]))
# Returns can also be inspected with quantmod (DD itself requires PRICES):
returns_example <- quantmod::dailyReturn(prices[[1L]], type = "log")
print(head(returns_example))

# 4. Estimate drawdowns, g/h and Newey-West SEs for one asset ----------------
# DD returns negative percentage changes; negate them for ghindex/gh_hac.
# The trough positions retain the observation-time spacing of loss episodes.
p <- prices[[1L]]
p <- p[paste0(seq(as_of, by = "-1 year", length.out = 2L)[2L], "/", as_of)]
drawdowns <- DD(as.numeric(p), type = 2)
losses <- -drawdowns$dd
print(ghindex(losses, out.plot = FALSE))
print(gh_hac(losses, positions = drawdowns$tmin, n_obs = NROW(p), type = 2))

# 5. Estimate all assets, all drawdown types and 1/3/5-year horizons ----------
# Each result has estimate, continuous approximation, se, lower/upper and status.
# SEs are linearized HAC approximations; unavailable SEs remain NA with a reason.
results <- screen_assets(prices, horizons = horizons, dates = as_of, inference = TRUE)
table <- risk_table(results)
print(table)
write.csv(results, file.path(out_dir, "estimates.csv"), row.names = FALSE)
write.csv(table, file.path(out_dir, "groups.csv"), row.names = FALSE)

# 6. Re-estimate through time using monthly endpoints -----------------------
# Every endpoint uses only its own trailing price window. Keeping inference=TRUE
# computes a new HAC SE at every endpoint, rather than reusing today's SE.
trading_dates <- as.Date(zoo::index(prices[[1L]]))
month_ends <- as.Date(tapply(as.numeric(trading_dates),
  format(trading_dates, "%Y-%m"), max), origin = "1970-01-01")
month_ends <- tail(month_ends, 24L)
history <- screen_assets(prices, horizons = rolling_horizon, dates = month_ends,
                         inference = TRUE)
write.csv(history, file.path(out_dir, "rolling_estimates.csv"), row.names = FALSE)

# 7. Compare estimated risk AND estimation uncertainty ----------------------
# Each report has one page per drawdown type and g/h in adjacent panels.
# Horizon maps: assets x 1/3/5 years. Time maps: assets x monthly endpoints.
# Risk uses a yellow/red scale; SE uses a separate blue scale. Missing SEs are
# grey/--, never zero. The same symbols and order are used throughout.
shown <- names(prices)
reports <- list(
  risk_horizons = plot_asset_risk(results, symbols = shown, layout = "horizon"),
  se_horizons = plot_asset_risk(results, symbols = shown, layout = "horizon", metric = "se"),
  risk_assets = plot_asset_risk(history, symbols = shown),
  se_assets = plot_asset_risk(history, symbols = shown, metric = "se"),
  risk_series = plot_risk_series(history,
    symbols = intersect(c("NVDA", "AAPL", "MSFT", "AMZN", "GOOGL", "META"), shown)))
for (report in names(reports)) for (page in names(reports[[report]]))
  ggplot2::ggsave(file.path(out_dir, paste0(report, "_", page, ".png")),
    reports[[report]][[page]], width = 12,
    height = if (report == "risk_series") 9 else 8, dpi = 150)
ggplot2::ggsave(file.path(out_dir, "groups.png"), plot_groups(results),
               width = 12, height = 13, dpi = 150)
saveRDS(list(results = results, history = history, table = table),
        file.path(out_dir, "results.rds"))
message("Outputs saved to ", normalizePath(out_dir))

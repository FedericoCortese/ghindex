# Nasdaq example: edit the settings below, then run the whole file.
# From the project folder in R: source("inst/examples/nasdaq.R")

# 1. Choose what to analyse ------------------------------------------------
# A fixed example of 20 major Nasdaq-listed companies (one ticker per company).
symbols <- c("NVDA", "AAPL", "MSFT", "AMZN", "GOOGL", "AVGO", "META", "TSLA",
             "MU", "AMD", "ASML", "INTC", "CSCO", "LRCX", "NFLX", "AMAT",
             "PLTR", "TXN", "COST", "TMUS")
as_of <- as.Date(Sys.getenv("GHINDEX_AS_OF", as.character(Sys.Date() - 1L)))
horizons <- c(1L, 3L, 5L)              # Years in the horizon maps
drawdown_types <- 1:3                   # Use c(1, 2) to omit type 3
rolling_horizon <- 1L                   # Years in each monthly window
rolling_months <- 24L                   # Number of monthly evaluations
series_symbols <- head(symbols, 6L)     # Tickers in the line plots

out_dir <- file.path("output", "nasdaq")
cache_dir <- file.path("cache", "nasdaq20")
years_needed <- max(horizons, rolling_horizon + ceiling(rolling_months / 12))
from <- seq(as_of, by = paste(-years_needed, "years"), length.out = 2L)[2L] - 10L
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)

# Load the package, or install this checkout into your personal R library.
local({
  if (requireNamespace("ghindex", quietly = TRUE)) return(invisible(NULL))

  root <- normalizePath(".")
  if (!file.exists("DESCRIPTION") ||
      !identical(unname(read.dcf("DESCRIPTION", fields = "Package")[1L, 1L]), "ghindex") ||
      !dir.exists("R")) {
    stop("ghindex is unavailable. Set your working directory to the ghindex project folder (the one containing DESCRIPTION), then rerun this script.", call. = FALSE)
  }

  lib <- path.expand(strsplit(Sys.getenv("R_LIBS_USER"), .Platform$path.sep, fixed = TRUE)[[1L]][1L])
  if (is.na(lib) || !nzchar(lib) || lib == "NULL")
    stop("Set R_LIBS_USER to a writable personal R library and rerun this script.", call. = FALSE)
  dir.create(lib, recursive = TRUE, showWarnings = FALSE)
  if (!dir.exists(lib) || file.access(lib, 2L) != 0L)
    stop("Cannot write to personal R library: ", lib, call. = FALSE)
  .libPaths(c(lib, .libPaths()))
  if (requireNamespace("ghindex", quietly = TRUE)) return(invisible(NULL))

  dependencies <- c("quantmod", "xts", "zoo", "ggplot2")
  missing <- dependencies[!vapply(dependencies, requireNamespace, logical(1), quietly = TRUE)]
  if (length(missing)) {
    command <- paste(deparse(missing), collapse = " ")
    stop("Install the missing R packages and rerun this script:\n  install.packages(",
         command, ', repos = "https://cloud.r-project.org")', call. = FALSE)
  }

  message("Installing ghindex from ", root, " into ", lib)
  utils::install.packages(root, lib = lib, repos = NULL, type = "source")
  if (!requireNamespace("ghindex", quietly = TRUE))
    stop("Could not load ghindex after installation. See the installation output above.", call. = FALSE)
})
library(ghindex)

# 2. Download prices -------------------------------------------------------
# getSymbols() returns daily Yahoo quotes as an xts object. The end date is
# exclusive, so use as_of + 1 to include as_of. Ad() selects Adjusted Close;
# use Cl() instead if you specifically want the unadjusted closing price.
download_quotes <- function(symbol) {
  cache_file <- file.path(cache_dir, paste0(symbol, ".rds"))
  cached <- if (file.exists(cache_file))
    tryCatch(readRDS(cache_file), error = function(e) NULL)
  if (!is.null(cached$data) && isTRUE(cached$from <= from && cached$to >= as_of)) {
    quotes <- cached$data
    status <- "cached"
  } else {
    quotes <- quantmod::getSymbols(
      symbol, src = "yahoo", from = from, to = as_of + 1L, auto.assign = FALSE
    )
    saveRDS(list(from = from, to = as_of, data = quotes), cache_file)
    status <- "downloaded"
  }
  price <- quantmod::Ad(quotes[paste0(from, "/", as_of)])
  if (!NROW(price)) stop("No prices in the requested date range.")
  list(price = price, status = status, message = "")
}

downloads <- setNames(lapply(symbols, function(symbol) {
  tryCatch(download_quotes(symbol), error = function(e) {
    warning(symbol, ": ", conditionMessage(e), call. = FALSE)
    list(price = NULL, status = "failed", message = conditionMessage(e))
  })
}), symbols)
download_log <- data.frame(
  symbol = symbols,
  status = vapply(downloads, function(x) x$status, character(1)),
  message = vapply(downloads, function(x) x$message, character(1)),
  row.names = NULL
)
write.csv(download_log, file.path(out_dir, "downloads.csv"), row.names = FALSE)
prices <- lapply(downloads[download_log$status != "failed"], function(x) x$price)
if (!length(prices)) stop("No prices available; see downloads.csv.")

# 3. Estimate g, h and their Newey-West standard errors ---------------------
# Each price series stays separate. screen_assets() flags invalid prices or
# insufficient history rather than filling in observations.
results <- screen_assets(
  prices, horizons = horizons, dates = as_of, types = drawdown_types,
  inference = TRUE
)
# Inspect the first asset: estimate is the index; se is its approximate SE.
# An unavailable SE is NA, with the reason recorded in status.
print(results[results$symbol == names(prices)[1L],
  c("symbol", "horizon", "type", "index", "estimate", "se", "status")])

# 4. Repeat the estimation at each monthly endpoint ------------------------
# Use the last observed trading date in each month, through as_of.
trading_dates <- as.Date(zoo::index(prices[[1L]]))
month_ends <- as.Date(tapply(as.numeric(trading_dates),
  format(trading_dates, "%Y-%m"), max), origin = "1970-01-01")
history <- screen_assets(
  prices, horizons = rolling_horizon, dates = tail(month_ends, rolling_months),
  types = drawdown_types, inference = TRUE
)

# 5. Save the estimates and figures ----------------------------------------
write.csv(results, file.path(out_dir, "estimates.csv"), row.names = FALSE)
write.csv(history, file.path(out_dir, "rolling_estimates.csv"), row.names = FALSE)
groups <- risk_table(results)
write.csv(groups, file.path(out_dir, "groups.csv"), row.names = FALSE)
saveRDS(list(results = results, history = history, table = groups),
        file.path(out_dir, "results.rds"))

# One figure per selected drawdown type, with g and h side by side.
# risk_horizons / se_horizons: different window lengths, same end date.
# risk_assets / se_assets: same window length, different monthly end dates.
# risk_series: line plots for the selected trajectory symbols.
shown <- names(prices)
reports <- list(
  risk_horizons = plot_asset_risk(results, symbols = shown, layout = "horizon"),
  se_horizons = plot_asset_risk(results, symbols = shown, layout = "horizon", metric = "se"),
  risk_assets = plot_asset_risk(history, symbols = shown),
  se_assets = plot_asset_risk(history, symbols = shown, metric = "se")
)
series_symbols <- intersect(series_symbols, shown)
if (length(series_symbols)) {
  reports$risk_series <- plot_risk_series(history, symbols = series_symbols)
}
figure_names <- character()
for (report in names(reports)) {
  for (page in names(reports[[report]])) {
    filename <- paste0(report, "_", page, ".png")
    figure_names <- c(figure_names, filename)
    ggplot2::ggsave(
      file.path(out_dir, filename),
      reports[[report]][[page]], width = 12,
      height = if (report == "risk_series") 9 else 8, dpi = 150
    )
  }
}
# Remove figures from previous runs for drawdown types no longer selected.
previous_figures <- list.files(out_dir,
  pattern = "^(risk_horizons|se_horizons|risk_assets|se_assets|risk_series)_type_[123][.]png$")
unlink(file.path(out_dir, setdiff(previous_figures, figure_names)))
message("Outputs saved to ", normalizePath(out_dir))

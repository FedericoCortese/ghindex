# ghindex: simulated price series and drawdown risk analysis.
# Run from the ghindex project folder: Rscript inst/examples/simulated.R

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


set.seed(2026)
out_dir <- file.path("output", "simulated")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
dates <- seq(as.Date("2019-01-01"), as.Date("2025-12-31"), by = "day")
dates <- dates[as.POSIXlt(dates)$wday %in% 1:5]
n <- length(dates)
market <- rnorm(n)
stress <- ifelse(dates >= as.Date("2025-04-01") & dates <= as.Date("2025-08-31"), 2.5, 1)
prices <- setNames(lapply(seq_len(60), function(i) {
  sigma <- c(.006, .015, .035)[(i - 1L) %/% 20L + 1L]
  innovation <- sigma * stress * (.35 * market + sqrt(1 - .35^2) * rnorm(n))
  returns <- as.numeric(stats::filter(innovation, filter = .25, method = "recursive")) + .0002
  data.frame(date = dates, price = 100 * exp(cumsum(returns)))
}), sprintf("Asset%02d", seq_len(60)))

# Full six-index classification and calendar-time Newey-West inference.
results <- screen_assets(prices, horizons = c(1, 3, 5), dates = max(dates))
table <- risk_table(results)
print(table)
utils::write.csv(table, file.path(out_dir, "groups.csv"), row.names = FALSE)

# Monthly composition of risk classes, using a fixed trailing one-year window.
month_ends <- as.Date(tapply(as.numeric(dates), format(dates, "%Y-%m"), max),
                       origin = "1970-01-01")
month_ends <- tail(month_ends, 24L)
history <- screen_assets(prices, horizons = 1, dates = month_ends, inference = TRUE)
# Use the same 20 illustrative assets in all risk and SE maps.
shown <- names(prices)[unique(round(seq(1, length(prices), length.out = 20)))]
reports <- list(
  risk_horizons = plot_asset_risk(results, symbols = shown, layout = "horizon"),
  se_horizons = plot_asset_risk(results, symbols = shown, layout = "horizon", metric = "se"),
  risk_assets = plot_asset_risk(history, symbols = shown),
  se_assets = plot_asset_risk(history, symbols = shown, metric = "se"),
  risk_series = plot_risk_series(history, symbols = shown[seq_len(6)]))
for (report in names(reports)) for (page in names(reports[[report]]))
  ggplot2::ggsave(file.path(out_dir, paste0(report, "_", page, ".png")),
    reports[[report]][[page]], width = 12,
    height = if (report == "risk_series") 9 else 8, dpi = 150)
write.csv(results, file.path(out_dir, "estimates.csv"), row.names = FALSE)
write.csv(history, file.path(out_dir, "rolling_estimates.csv"), row.names = FALSE)
saveRDS(list(results = results, history = history, table = table),
         file.path(out_dir, "results.rds"))
message("Outputs saved to ", normalizePath(out_dir))

# ghindex

Calculate the g- and h-index of financial drawdown sequences, compare asset
risk across investment horizons, and estimate Newey-West HAC standard errors.
The package supports three drawdown definitions, single-asset analysis,
multi-asset screening and rolling-window analysis.

## Installation

Requires R >= 4.1. In R or RStudio:

```r
install.packages(c("quantmod", "xts", "zoo", "ggplot2"),
                 repos = "https://cloud.r-project.org")
install.packages(".",
                 repos = NULL, type = "source")
library(ghindex)
help(package = "ghindex")
```

If you have built or downloaded the source archive, you can install it instead
(adjust the path to where you saved it):

```r
install.packages("output/ghindex_0.2.0.tar.gz",
                 repos = NULL, type = "source")
```

Install dependencies first: `repos = NULL` does not download them. The package
contains only R code. Reinstall and restart R after changing package code.

## From prices to drawdowns and index estimates

```r
library(quantmod)
library(ghindex)

# Request daily observations; getSymbols returns an xts OHLCV series.
quotes <- getSymbols("AAPL", src = "yahoo", from = "2020-01-01",
                     to = Sys.Date(), auto.assign = FALSE)
prices <- Ad(quotes)             # Adjusted Close; Cl(quotes) selects raw Close.
dd <- DD(as.numeric(prices), type = 2)
losses <- -dd$dd                 # Positive losses in percentage points.
ghindex(losses, out.plot = FALSE)
gh_hac(losses, positions = dd$tmin, n_obs = NROW(prices), type = 2)
```

For descending losses `d`, h is the largest integer `j` with `d[j] >= j`.
The g-index is the largest `j` with `sum(d[1:j]) >= j^2`. A 5% loss is supplied
as `5`, not `0.05`. Empty sequences have g = h = 0.

- **Type 1:** negative one-period price changes.
- **Type 2:** losses from strict interior peaks to subsequent strict troughs.
  Plateaus are excluded as turning points. A selected trough above its previous
  strict peak is not a loss; the excluded pair is counted in `n_discarded`.
- **Type 3:** the minimum between consecutive record peaks, normalized by the
  preceding peak. Record peaks must exceed all preceding prices and be followed
  by a decrease. Interior record detection starts at observation 2, with the
  initial observation as fallback. For a single peak, use its subsequent minimum;
  with multiple peaks, the final unfinished episode is excluded.

`DD` returns negative percentage drawdowns, aligned peak/trough positions and
optional diagnostic plots. These are episode definitions, not the daily running
maximum-drawdown process.

## Compare assets and horizons

```r
prices <- get_prices(c("AAPL", "MSFT", "NVDA"), from = "2020-01-01",
                     cache_dir = "cache/prices")
results <- screen_assets(prices, horizons = c(1, 3, 5), inference = TRUE)
risk_table(results)
plot_groups(results)

# One page per drawdown type, g/h side by side.
plot_asset_risk(results, layout = "horizon", symbols = names(prices))
plot_asset_risk(results, layout = "horizon", symbols = names(prices), metric = "se")
```

Inputs are named lists of single-column xts series or `date`/`price` data frames.
Prices must be positive, finite and chronologically ordered. Invalid data,
short histories and stale endpoints are flagged. Default coverage requires at
least 80% of 252 observations per year and observations within seven calendar
days of both window endpoints. Adjust coverage and annualization settings for
other trading calendars.

The group table has risk classes in rows and 1/3/5-year horizons in columns,
including asset counts and names, mean Sharpe ratio, volatility and available
SEs. Group statistics average individual asset statistics; they are not
portfolio statistics. Sharpe uses annualized mean log return minus the annual
continuously compounded risk-free rate, divided by annualized log-return
volatility. Volatility is expressed in percent. Index classes across different
horizons also reflect different numbers of episodes and are not interchangeable.

## Time-varying risk and uncertainty

Supply evaluation dates to recompute each index and SE from its own trailing
window. Use `inference = TRUE` to estimate uncertainty at every date.

```r
history <- screen_assets(prices, horizons = 1,
  dates = as.Date(c("2025-12-31", "2026-03-31", "2026-06-30")), inference = TRUE)
plot_asset_risk(history, symbols = names(prices))
plot_asset_risk(history, symbols = names(prices), metric = "se")
plot_risk_series(history, symbols = names(prices))
```

Risk maps use yellow to dark red for increasing g/h. Uncertainty maps use a
separate blue scale for increasing SE. Both keep the same asset order and a
common linear scale across drawdown types. Grey cells mean unavailable values,
not zero. Step plots show integer index trajectories. Overlapping windows are
dependent. Supplying `symbols` fixes the displayed sample; automatic selection
spans the latest mean index ranking among complete histories and is descriptive,
not an investment selection rule.

## Worked examples

Run from the project directory (the folder containing `DESCRIPTION`):

```sh
Rscript inst/examples/simulated.R
Rscript inst/examples/nasdaq.R
```

In R or RStudio, set the working directory to the project folder and use
`source("inst/examples/nasdaq.R")` or `source("inst/examples/simulated.R")`.
Each script checks whether `ghindex` can be loaded. If needed, it installs the
local source package into your personal R library (`R_LIBS_USER`). If a
dependency is missing, the script prints the command to install it.
Installed scripts are available under
`system.file("examples", package = "ghindex")`.

- **Simulated:** reproducible asset prices with different volatility levels,
  serial dependence and a stress period; runs offline.
- **Nasdaq:** a fixed selection of 20 prominent large Nasdaq-listed companies,
  one share class per company. This is an explicit illustration, not a live
  top-20 market-cap ranking. The English script walks through symbol/date
  selection, `quantmod::getSymbols`, `Ad`/`Cl`, returns, single-asset estimation,
  multi-asset screening, HAC inference and rolling-window plots. Change the
  `symbols` vector to analyse different securities. `GHINDEX_AS_OF` sets a
  reproducible endpoint; otherwise the script uses yesterday.

Each example writes to `output/simulated/` or `output/nasdaq/`:

| Files | Content |
| --- | --- |
| `groups.csv` | Class/horizon summary with Sharpe and volatility |
| `estimates.csv` | Individual estimates, SEs, intervals and diagnostic statuses |
| `rolling_estimates.csv` | Monthly rolling estimates and newly calculated SEs |
| `risk_horizons_type_*.png` | Assets x horizons, coloured by g/h |
| `se_horizons_type_*.png` | Matching assets x horizons maps, coloured by SE |
| `risk_assets_type_*.png` | Assets x time maps of g/h |
| `se_assets_type_*.png` | Matching assets x time maps of SE |
| `risk_series_type_*.png` | Monthly risk trajectories for six displayed assets |
| `results.rds` | Complete estimates, history and group table |

The wildcard covers drawdown types 1, 2 and 3. The Nasdaq example also saves a
download log. Changing the displayed sample does not change the estimation
sample. Data are cached to avoid repeat downloads.

## Standard errors and their interpretation

`gh_hac` returns exact integer indices, continuous approximations, linearized
Newey-West SEs, pointwise intervals and statuses. SEs and intervals refer to the
continuous approximations, not exact confidence sets for integer indices or
forecasts of future indices. The h estimating equation uses Gaussian smoothing;
g uses a fractional upper-tail sum. The g calculation is an operational
linearization of that functional, rather than an established result for every
drawdown-generating process.

Chronological score contributions use Bartlett HAC weights without prewhitening.
The default lag is `floor(4*(T/100)^(2/9))`, capped at `T-1`; set `lag` explicitly
to assess sensitivity. Without `positions` and `n_obs`, inference operates in
episode time conditional on episode count. With those arguments, contributions
are placed at trough positions with zeros at other observations.

The default minimum is 30 loss episodes. Small samples, boundaries and degenerate
scores return `NA` SEs with a reason in `status`. Type 3 uses status
`record_process_approximation`: record-dependent episodes do not automatically
satisfy the stationarity and weak-dependence conditions needed for HAC. Endpoint
selection also affects the episode process. Coverage for arbitrary financial
processes is not guaranteed; use the reported diagnostics and distinguish
unavailable SEs from low uncertainty. Intervals are pointwise, not simultaneous
comparisons across assets or dates.

References: [Pratelli et al. (2012)](https://arxiv.org/abs/1102.2701),
[Newey and West (1987)](https://www.nber.org/papers/t0055).

## Data access and computation

`get_prices` provides cached quantmod downloads for ticker vectors, or the full
current directory through `get_prices("NASDAQ", ...)`. Failures are logged.
Adjusted Close is used, with a logged Close fallback only if the adjusted column
is entirely unavailable. Current-universe historical studies have survivorship
bias. The worked Nasdaq example uses its explicit 20-company selection instead.

Screening and the download helper support PSOCK parallelism through `workers`.
Episode extraction is linear in price history length; sorting costs O(D log D)
and HAC costs O(T L) per window. No quadratic pairwise asset comparisons are
required. Configure worker counts according to memory and provider rate limits.

## Package files and checks

`R/` contains the implementation, `man/` the help pages, `inst/examples/` the
two worked examples and `tests/` the offline checks. Generated `cache/` and
`output/` directories are excluded from the package distribution.

```sh
R CMD build .
R CMD check --no-manual ghindex_0.2.0.tar.gz
```

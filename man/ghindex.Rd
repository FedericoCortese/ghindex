\name{ghindex}
\alias{ghindex}
\alias{DD}
\alias{gh_mw}
\alias{gh_hac}
\alias{nasdaq_symbols}
\alias{get_prices}
\alias{screen_assets}
\alias{risk_table}
\alias{plot_groups}
\alias{plot_risk_time}
\alias{plot_asset_risk}
\alias{plot_risk_series}
\title{Drawdown-Based Asset Risk Classification}
\description{Compute the g- and h-index for three drawdown definitions, screen
financial assets, estimate HAC uncertainty and display risk groups.}
\usage{
DD(x, type, out.plot = FALSE)
ghindex(x, out.plot = TRUE, legend = FALSE)
gh_mw(x, rx, MW = 500, out.plot = FALSE)
gh_hac(x, lag = NULL, level = .95, min_events = 30L,
       bandwidth = NULL, positions = NULL, n_obs = NULL, type = NULL)
nasdaq_symbols(include_etf = TRUE,
  url = "https://www.nasdaqtrader.com/dynamic/symdir/nasdaqlisted.txt")
get_prices(symbols, from, to = Sys.Date(), cache_dir = NULL,
           refresh = FALSE, retries = 2L, pause = .2, include_etf = TRUE,
           workers = 1L)
screen_assets(prices, horizons = c(1L, 3L, 5L), dates = NULL,
  types = 1:3, inference = TRUE, lag = NULL, level = .95,
  min_events = 30L, annualization = 252, risk_free = 0,
  min_coverage = .8, tolerance_days = 7L, workers = 1L)
risk_table(results, date = max(results$date), wide = TRUE)
plot_groups(results, date = max(results$date))
plot_risk_time(results, horizon = min(results$horizon))
plot_asset_risk(results, horizon = min(results$horizon), n_assets = 20L,
                symbols = NULL, metric = c("estimate", "se"),
                layout = c("time", "horizon"), date = max(results$date))
plot_risk_series(results, horizon = min(results$horizon), n_assets = 6L,
                 symbols = NULL)
}
\arguments{
\item{x}{For \code{DD} and \code{gh_mw}, positive finite prices. For
\code{ghindex} and \code{gh_hac}, nonnegative drawdown magnitudes in percentage
points. For inference, observations must remain in chronological order.}
\item{type, types}{Drawdown type(s), integers 1, 2 or 3. Optional in
\code{gh_hac}; type 3 identifies the record-process approximation.}
\item{out.plot, legend}{Whether to return a diagnostic plot or display its legend.}
\item{rx}{Returns aligned with prices; an initial missing return is allowed.}
\item{MW}{Moving window length in observations. Includes the final full window.}
\item{lag}{Nonnegative Bartlett truncation lag. Default:
\code{floor(4 * (n_obs / 100)^(2/9))}, capped at \code{n_obs - 1}.
This is a rule of thumb, not the automatic Newey-West (1994) plug-in bandwidth.}
\item{level}{Pointwise confidence level for the continuous approximations.}
\item{min_events}{Minimum episode count before attempting HAC inference.}
\item{bandwidth}{Positive Gaussian smoothing bandwidth for h inference;
defaults to \code{stats::bw.nrd0(x)}. Exact index estimates are never smoothed.}
\item{positions, n_obs}{Together, episode trough positions and full observation
count. Scores are inserted at those positions with zeros at other observations.
If omitted, HAC operates in episode time conditional on the episode count.}
\item{include_etf}{Include Nasdaq-listed ETFs; other security types are retained.}
\item{url}{Nasdaq directory URL, or a compatible local fixture path.}
\item{symbols}{Ticker vector or, for downloads, the special string \code{"NASDAQ"},
which resolves the full current Nasdaq listing directory, excluding test issues.
For risk pages, optionally choose the displayed assets explicitly.}
\item{from, to}{Inclusive calendar dates for requested prices.}
\item{cache_dir}{Optional directory for per-symbol RDS caches.}
\item{refresh}{Ignore cached prices and redownload.}
\item{retries}{Number of additional attempts per failed symbol.}
\item{pause}{Seconds between uncached symbol downloads.}
\item{prices}{Named list of one-column xts/zoo series, or data frames with
\code{date} and \code{price}. Invalid series are reported without aborting others.}
\item{horizons, horizon}{Trailing whole calendar years; default 1, 3 and 5.}
\item{dates, date}{Evaluation date(s). Default is the latest observation across
assets, or the latest date in results.}
\item{inference}{Calculate HAC uncertainty, or only exact classifications.}
\item{annualization}{Trading observations per year, default 252.}
\item{risk_free}{Annual continuously compounded risk-free rate, default zero.}
\item{min_coverage}{Minimum observations divided by annualization times years.}
\item{tolerance_days}{Maximum calendar-day gap at either window endpoint.}
\item{workers}{PSOCK worker count for screening or downloading, default one.}
\item{results}{Data frame returned by \code{screen_assets}.}
\item{n_assets}{Maximum automatically selected assets in the risk pages.}
\item{metric}{Colour cells by integer index estimate or Newey-West standard error.}
\item{layout}{Time maps for a fixed horizon, or horizon maps at one evaluation date.}
\item{wide}{Put horizons in columns; otherwise return one row per group/horizon.}
}
\details{
Type 1 counts negative one-period price changes. Type 2 uses strict interior
turning points; endpoints and plateaus are excluded. Type 3 uses strict record
peaks followed by a decrease and the minimum between consecutive record peaks.
Interior record detection starts at observation 2;
the initial observation is a fallback when none is detected. For a single peak,
the default uses only its subsequent minimum. For multiple peaks, the final
unfinished episode is excluded. This is not a daily running maximum-drawdown
process. Returned peak/trough vectors are aligned with individual episodes.
If an intervening plateau makes a selected minimum higher than its preceding
strict peak, that non-loss pair is excluded and counted in \code{n_discarded}
(\code{discarded_episodes} in screening results).

For ordered losses d, h is max(j: d[j] >= j), and g is
max(j: sum(d[1:j]) >= j^2), bounded by the observed episode count.
Empty inputs have both indices zero.

HAC uses a Gaussian-smoothed h estimating equation and a fractional upper-tail
sum for g. Bartlett Newey-West covariance is calculated on chronological score
contributions, without prewhitening. In observation time zero contributions
retain variation in episode occurrence, subject to regularity of the marked
episode process. Endpoint selection and record processes require separate
theoretical justification. In episode time inference conditions on the count.
Intervals refer to continuous approximations, not proven confidence sets for
the integer classifications. Small samples, boundaries and degenerate scores
produce missing standard errors with diagnostic statuses. Type 3 has status \code{record_process_approximation}, since record-dependent
episodes do not automatically satisfy stationarity or weak dependence.
Adequate observations near thresholds are required. Coverage for arbitrary
drawdown-generating processes is not guaranteed.

Sharpe is (mean daily log return times annualization minus risk_free) divided by
annualized log-return volatility. Volatility is reported in percent. Group
statistics are equal-weight means of individual asset statistics, not portfolio
Sharpe or portfolio volatility. Classes from different horizons are not directly
comparable: a longer history also supplies more episodes.

Download failures are retained in the download log attribute. Adjusted close is
used; an entirely unavailable adjusted column triggers an explicitly logged
close fallback. Missing or nonpositive observations are not silently filled.
Directory and caches are snapshots; current constituents create survivorship
bias in historical analyses. No delisted-universe reconstruction is performed.
}
\value{
\code{DD}: list with negative \code{dd}, \code{tmin}, \code{tmax},
\code{n_discarded} non-loss pairs and optional plot.
\code{ghindex}: list with \code{g}, \code{h}, optional \code{zoomplot}.
\code{gh_mw}: moving-window data frame with g1--g3, h1--h3, std and VaR.
\code{gh_hac}: two-row data frame of estimates, continuous estimates, standard
errors, intervals, sample counts, lag, bandwidth, time scale and status;
attribute \code{covariance} contains the joint g/h covariance approximation.
\code{nasdaq_symbols}: directory data frame, including \code{yahoo_symbol}.
\code{get_prices}: named list with \code{download_log} and \code{universe} attributes.
\code{plot_asset_risk}: named list of ggplots, one per drawdown type, with the
selected symbols in attribute \code{symbols}. Defaults to a fixed complete-history
sample spread across the latest mean g/h ranking over all types; this is a
retrospective display selection. Colours show index estimates or standard errors on a common linear scale
across drawdown pages. Missing standard errors are grey, not zero.
\code{plot_risk_series}: one step-trajectory plot per drawdown type, with
individual assets in rows and g/h in columns.
\code{screen_assets}: one row per asset/date/horizon/type/index, including
data eligibility and inference statuses. \code{risk_table}: group summary data
frame, with horizon suffixes in wide format. Plot functions return ggplot objects.
}
\references{
Pratelli, L., Baccini, A., Barabesi, L. and Marcheselli, M. (2012).
Statistical Analysis of the Hirsch Index. Scandinavian Journal of Statistics,
39, 681--694. \doi{10.1111/j.1467-9469.2011.00782.x}.

Newey, W. K. and West, K. D. (1987). A Simple, Positive Semi-Definite,
Heteroskedasticity and Autocorrelation Consistent Covariance Matrix.
Econometrica, 55, 703--708. \doi{10.2307/1913610}.
}
\examples{
p <- c(100, 110, 90, 115, 80, 120, 100)
d <- DD(p, type = 2)
ghindex(-d$dd, out.plot = FALSE)
set.seed(1)
gh_hac(abs(rnorm(200, 4, 2)))
}

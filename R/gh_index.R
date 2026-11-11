# Drawdown-based g- and h-index.
ghindex <- function(x, out.plot = TRUE, legend = FALSE) {
  x <- as.numeric(x)
  if (any(!is.finite(x)) || any(x < 0))
    stop("Supply finite, nonnegative losses in percentage points (e.g. 5, not .05).")
  sx <- sort(x, decreasing = TRUE)
  j <- seq_along(sx)
  out <- list(g = max(c(0L, j[cumsum(sx) >= j^2])),
              h = max(c(0L, j[sx >= j])))
  if (out.plot) {
    d <- data.frame(rank = j, loss = sx, average = cumsum(sx) / j)
    out$zoomplot <- ggplot2::ggplot(d, ggplot2::aes(x = rank)) +
      ggplot2::geom_line(ggplot2::aes(y = average, colour = "g")) +
      ggplot2::geom_line(ggplot2::aes(y = loss, colour = "h")) +
      ggplot2::geom_abline(slope = 1, intercept = 0) +
      ggplot2::geom_vline(xintercept = c(out$g, out$h), linetype = 3) +
      ggplot2::coord_cartesian(xlim = c(0, max(out$g, out$h) + 2)) +
      ggplot2::labs(y = "Drawdown (%)", colour = "Index") +
      ggplot2::theme_bw() +
      ggplot2::theme(legend.position = if (legend) "right" else "none")
  }
  out
}

# Bartlett Newey-West covariance of a SUM of chronological score vectors.
# No regression, prewhitening, or additional sandwich dependency is needed.
.nw_sum <- function(score, lag = NULL) {
  score <- as.matrix(score)
  n <- nrow(score)
  if (is.null(lag)) lag <- floor(4 * (n / 100)^(2 / 9))
  if (length(lag) != 1L || !is.finite(lag) || lag < 0 || lag != floor(lag))
    stop("lag must be a nonnegative integer.")
  lag <- min(as.integer(lag), n - 1L)
  u <- sweep(score, 2L, colMeans(score), "-")
  v <- crossprod(u)
  if (lag > 0L) for (ell in seq_len(lag)) {
    cross <- crossprod(u[(ell + 1L):n, , drop = FALSE],
                       u[seq_len(n - ell), , drop = FALSE])
    v <- v + (1 - ell / (lag + 1)) * (cross + t(cross))
  }
  list(covariance = v, lag = lag)
}

gh_hac <- function(x, lag = NULL, level = .95, min_events = 30L,
                   bandwidth = NULL, positions = NULL, n_obs = NULL,
                   type = NULL) {
  point <- ghindex(x, out.plot = FALSE)
  n <- length(x)
  if (length(level) != 1L || !is.finite(level) || level <= 0 || level >= 1)
    stop("level must be between 0 and 1.")
  if (length(min_events) != 1L || !is.finite(min_events) || min_events < 2)
    stop("min_events must be at least 2.")
  if (is.null(positions) != is.null(n_obs)) stop("Supply both positions and n_obs.")
  calendar <- !is.null(positions)
  if (!calendar) { positions <- seq_len(n); n_obs <- n }
  if (length(n_obs) != 1L || !is.finite(n_obs) || n_obs < n || n_obs != floor(n_obs) ||
      length(positions) != n || any(!is.finite(positions)) ||
      any(positions != floor(positions)) || any(positions < 1 | positions > n_obs) ||
      is.unsorted(positions, strictly = TRUE))
    stop("positions must be unique chronological integer positions in 1:n_obs.")
  out <- data.frame(index = c("g", "h"), estimate = c(point$g, point$h),
                    continuous = NA_real_, se = NA_real_, lower = NA_real_,
                    upper = NA_real_, n_events = n, n_obs = n_obs,
                    lag = NA_integer_, bandwidth = NA_real_,
                    status = "too_few_events",
                    time_scale = if (calendar) "observations" else "episodes")
  attr(out, "covariance") <- matrix(NA_real_, 2L, 2L,
                                    dimnames = list(c("g", "h"), c("g", "h")))
  if (n < min_events) return(out)
  if (is.null(bandwidth)) bandwidth <- stats::bw.nrd0(x)
  if (length(bandwidth) != 1L || !is.finite(bandwidth) || bandwidth <= 0)
    stop("bandwidth must be strictly positive.")
  out$bandwidth[2L] <- bandwidth
  sx <- sort(x, decreasing = TRUE)
  # The h estimating equation is smoothed only for inference, not classification.
  hc <- stats::uniroot(function(h) sum(stats::pnorm((x - h) / bandwidth)) - h,
                        c(0, n))$root
  wh <- stats::pnorm((x - hc) / bandwidth)
  ah <- 1 + sum(stats::dnorm((x - hc) / bandwidth)) / bandwidth
  # Integrated empirical upper quantile: fractional last observation.
  k <- point$g
  capped <- k == n
  if (capped) { gc <- n; q <- min(x) } else {
    q <- sx[k + 1L]
    total <- if (k) sum(sx[seq_len(k)]) else 0
    gc <- (q + sqrt(max(0, q^2 + 4 * (total - k * q)))) / 2
  }
  ag <- 2 * gc - q
  wg <- pmax(x - q, 0)
  score <- matrix(0, nrow = n_obs, ncol = 2L,
                  dimnames = list(NULL, c("g", "h")))
  score[positions, ] <- cbind(wg, wh)
  nw <- .nw_sum(score, lag)
  a <- c(ag, ah)
  covariance <- nw$covariance / outer(a, a)
  out$continuous <- c(gc, hc)
  out$se <- sqrt(pmax(0, diag(covariance)))
  out$lag <- nw$lag
  out$status <- if (!is.null(type) && type == 3L)
    "record_process_approximation" else "approximate"
  invalid <- c(capped || ag <= 1e-10 || point$g == 0,
               point$h == n || point$h == 0)
  degenerate <- !is.finite(out$se) | out$se <= 1e-12
  out$status[degenerate] <- "degenerate_score"
  out$status[invalid] <- "boundary"
  out$se[invalid | degenerate] <- NA_real_
  covariance[invalid | degenerate, ] <- NA_real_
  covariance[, invalid | degenerate] <- NA_real_
  z <- stats::qnorm((1 + level) / 2)
  # Pointwise intervals for the continuous approximations, not exact integer CIs.
  out$lower <- pmax(0, out$continuous - z * out$se)
  out$upper <- pmin(n, out$continuous + z * out$se)
  attr(out, "covariance") <- covariance
  out
}

gh_mw <- function(x, rx, MW = 500, out.plot = FALSE) {
  if (length(x) != length(rx) || length(MW) != 1L || MW < 2 ||
      MW != floor(MW) || MW > length(x)) stop("Invalid prices, returns or MW.")
  starts <- seq_len(length(x) - MW + 1L)
  rows <- lapply(starts, function(i) {
    w <- seq.int(i, length.out = MW)
    gh <- lapply(1:3, function(k) ghindex(-DD(x[w], k)$dd, out.plot))
    r <- rx[w]
    c(g1 = gh[[1]]$g, g2 = gh[[2]]$g, g3 = gh[[3]]$g,
      h1 = gh[[1]]$h, h2 = gh[[2]]$h, h3 = gh[[3]]$h,
      std = stats::sd(r, na.rm = TRUE) * 100,
      VaR = -stats::quantile(r, .05, na.rm = TRUE, names = FALSE) * 100)
  })
  as.data.frame(do.call(rbind, rows))
}

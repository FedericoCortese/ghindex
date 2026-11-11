# Drawdown definitions of Cortese, Bartolucci and Pennoni.
# Negative percentage drawdowns, in percentage points.
DD <- function(x, type, out.plot = FALSE) {
  x <- as.numeric(x)
  if (any(!is.finite(x)) || any(x <= 0))
    stop("Prices must be finite and strictly positive.")
  if (length(type) != 1L || !type %in% 1:3) stop("type must be 1, 2 or 3.")
  n <- length(x)
  peaks <- troughs <- integer()
  if (n >= 2L && type == 1L) {
    peaks <- which(diff(x) < 0)
    troughs <- peaks + 1L
  }
  if (n >= 3L && type == 2L) {
    mid <- 2:(n - 1L)
    p <- mid[x[mid] > x[mid - 1L] & x[mid] > x[mid + 1L]]
    v <- mid[x[mid] < x[mid - 1L] & x[mid] < x[mid + 1L]]
    # Strict turning points, endpoints and plateaus excluded by definition.
    if (length(p) && length(v)) {
      before <- findInterval(v, p)
      troughs <- v[before > 0L]
      peaks <- p[before[before > 0L]]
    }
  }
  if (n >= 2L && type == 3L) {
    # Record detection in linear time.
    p <- if (n >= 3L) {
      mid <- 2:(n - 1L)
      mid[x[mid] > cummax(x)[mid - 1L] & x[mid] > x[mid + 1L]]
    } else integer()
    if (!length(p)) p <- 1L
    if (length(p) == 1L) {
      candidate <- p:n
      v <- candidate[which.min(x[candidate])]
      if (v > p && x[v] < x[p]) {
        peaks <- p
        troughs <- v
      }
    } else {
      troughs <- vapply(seq_len(length(p) - 1L), function(j) {
        between <- seq.int(p[j] + 1L, p[j + 1L] - 1L)
        between[which.min(x[between])]
      }, integer(1))
      peaks <- p[-length(p)]
    }
    # Between record peaks only; when there is a
    # single peak, use its subsequent trough. No extra terminal episode.
  }
  # A missed flat-topped peak can leave a strict type-2 minimum above the
  # preceding strict maximum. Such a pair is not a loss episode.
  keep <- x[troughs] <= x[peaks]
  discarded <- sum(!keep)
  peaks <- peaks[keep]; troughs <- troughs[keep]
  dd <- 100 * (x[troughs] / x[peaks] - 1)
  out <- list(dd = dd, tmin = troughs, tmax = peaks, n_discarded = discarded)
  if (out.plot) {
    d <- data.frame(Time = seq_len(n), Price = x, category = "price")
    d$category[unique(peaks)] <- "max"
    d$category[unique(troughs)] <- "min"
    d$category[intersect(peaks, troughs)] <- "both"
    out$plot <- ggplot2::ggplot(d, ggplot2::aes(x = Time, y = Price)) +
      ggplot2::geom_line() +
      ggplot2::geom_point(ggplot2::aes(colour = category)) +
      ggplot2::scale_colour_manual(values = c(price = "grey50", max = "#00875A",
                                              min = "#D64545", both = "#DBA400")) +
      ggplot2::theme_bw() + ggplot2::labs(colour = NULL)
  }
  out
}

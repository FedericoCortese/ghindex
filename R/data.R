nasdaq_symbols <- function(include_etf = TRUE,
    url = "https://www.nasdaqtrader.com/dynamic/symdir/nasdaqlisted.txt") {
  d <- utils::read.delim(url, sep = "|", stringsAsFactors = FALSE,
                         check.names = FALSE, quote = "", na.strings = character())
  required <- c("Symbol", "Security Name", "Test Issue", "ETF")
  if (!all(required %in% names(d))) stop("Unexpected Nasdaq directory format.")
  d <- d[!is.na(d[["Test Issue"]]) & d[["Test Issue"]] == "N", , drop = FALSE]
  if (!include_etf) d <- d[d$ETF == "N", , drop = FALSE]
  d$yahoo_symbol <- gsub(".", "-", d$Symbol, fixed = TRUE)
  rownames(d) <- NULL
  d
}

get_prices <- function(symbols, from, to = Sys.Date(), cache_dir = NULL,
                       refresh = FALSE, retries = 2L, pause = .2,
                       include_etf = TRUE, workers = 1L) {
  from <- as.Date(from); to <- as.Date(to)
  if (length(from) != 1L || length(to) != 1L || is.na(from) || is.na(to) || from >= to)
    stop("from must precede to.")
  if (length(retries) != 1L || !is.finite(retries) || retries < 0 || retries != floor(retries) ||
      length(pause) != 1L || !is.finite(pause) || pause < 0)
    stop("Invalid retries or pause.")
  universe <- NULL
  if (length(symbols) == 1L && toupper(symbols) == "NASDAQ") {
    universe <- nasdaq_symbols(include_etf)
    symbols <- universe$yahoo_symbol
  }
  if (!is.character(symbols) || !length(symbols) || anyNA(symbols) || any(!nzchar(symbols)))
    stop("Supply ticker symbols or 'NASDAQ'.")
  symbols <- unique(symbols)
  if (length(workers) != 1L || !is.finite(workers) || workers < 1 || workers != floor(workers))
    stop("workers must be a positive integer.")
  if (!is.null(cache_dir)) dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  if (workers > 1L) {
    cluster <- parallel::makePSOCKcluster(min(workers, length(symbols)))
    on.exit(parallel::stopCluster(cluster), add = TRUE)
    pieces <- parallel::parLapplyLB(cluster, symbols,
      function(symbol, from, to, cache_dir, refresh, retries, pause) {
        suppressMessages(ghindex::get_prices(symbol, from, to, cache_dir,
          refresh, retries, pause, workers = 1L))
      }, from = from, to = to, cache_dir = cache_dir, refresh = refresh,
      retries = retries, pause = pause)
    prices <- list()
    for (i in seq_along(pieces)) if (length(pieces[[i]]))
      prices[[symbols[i]]] <- pieces[[i]][[1L]]
    attr(prices, "download_log") <- do.call(rbind,
      lapply(pieces, attr, "download_log"))
    attr(prices, "universe") <- universe
    message("Prices: ", length(symbols), "/", length(symbols),
              " (", length(prices), " available)")
    return(prices)
  }
  prices <- list(); logs <- vector("list", length(symbols))
  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    # Encode bytes: no collisions or filesystem interpretation of ticker names.
    key <- paste(sprintf("%02x", as.integer(charToRaw(symbol))), collapse = "")
    file <- if (!is.null(cache_dir)) file.path(cache_dir, paste0(key, ".rds")) else NULL
    cached <- if (!refresh && !is.null(file) && file.exists(file))
      tryCatch(readRDS(file), error = function(e) NULL) else NULL
    hit <- is.list(cached) && identical(cached$symbol, symbol) &&
      !is.null(cached$from) && cached$from <= from && cached$to >= to
    # Load xts methods also on cache-only workers, before zoo::index/subsetting.
    if (hit && !xts::is.xts(cached$prices)) hit <- FALSE
    value <- NULL; error <- ""; used_close <- FALSE
    if (hit) {
      value <- cached$prices
      used_close <- isTRUE(cached$used_close)
    } else {
      for (attempt in seq_len(retries + 1L)) {
        value <- tryCatch({
          raw <- quantmod::getSymbols(symbol, src = "yahoo", from = from,
              to = to + 1L, auto.assign = FALSE, warnings = FALSE)
          adjusted <- quantmod::Ad(raw)
          used_close <- !any(is.finite(as.numeric(adjusted)))
          if (used_close) adjusted <- quantmod::Cl(raw)
          if (!NROW(adjusted)) stop("Empty price response.")
          adjusted
        }, error = function(e) { error <<- conditionMessage(e); NULL })
        if (!is.null(value)) break
        if (attempt <= retries) Sys.sleep(min(30, 2^(attempt - 1L)))
      }
      if (!is.null(value) && !is.null(file))
        saveRDS(list(symbol = symbol, from = from, to = to, prices = value,
                     used_close = used_close), file)
      if (pause > 0) Sys.sleep(pause)
    }
    if (!is.null(value)) {
      dates <- as.Date(zoo::index(value))
      value <- value[dates >= from & dates <= to, , drop = FALSE]
      colnames(value) <- symbol
      prices[[symbol]] <- value
    }
    logs[[i]] <- data.frame(symbol = symbol,
      status = if (is.null(value)) "failed" else if (hit) "cached" else "downloaded",
      price_field = if (used_close) "close_fallback" else "adjusted",
      n = if (is.null(value)) 0L else NROW(value),
      message = if (is.null(value)) error else "", stringsAsFactors = FALSE)
    if (i %% 100L == 0L || i == length(symbols))
      message("Prices: ", i, "/", length(symbols), " (", length(prices), " available)")
  }
  attr(prices, "download_log") <- do.call(rbind, logs)
  attr(prices, "universe") <- universe
  prices
}

# Bindings used by ggplot2 data masking.
utils::globalVariables(c("Time", "Price", "category", "rank", "average", "loss",
  "horizon_label", "class", "n_assets", "label", "date", "share", "text_colour",
  "symbol", "estimate", "left", "right", "row", "value", "cell_label"))

.asset_subset <- function(d, n_assets, symbols, complete_cells) {
  if (!nrow(d) || !any(is.finite(d$estimate))) stop("No index estimates available.")
  if (length(n_assets) != 1L || !is.finite(n_assets) || n_assets < 1 ||
      n_assets != floor(n_assets)) stop("n_assets must be a positive integer.")
  if (is.null(symbols)) {
    valid <- d[is.finite(d$estimate), ]
    counts <- table(valid$symbol)
    eligible <- names(counts)[counts == complete_cells]
    latest <- valid[valid$date == max(d$date) & valid$symbol %in% eligible, ]
    if (!nrow(latest)) stop("No complete histories; supply symbols explicitly.")
    ranking <- stats::aggregate(estimate ~ symbol, latest, mean)
    ranking <- ranking[order(ranking$estimate, ranking$symbol), ]
    take <- unique(round(seq(1, nrow(ranking), length.out = min(n_assets, nrow(ranking)))))
    symbols <- ranking$symbol[take]
    note <- "Fixed complete-history sample spanning the latest mean index ranking."
  } else {
    symbols <- unique(as.character(symbols))
    if (!length(symbols) || anyNA(symbols) || any(!symbols %in% d$symbol))
      stop("Requested symbols must be present in results.")
    note <- "User-selected assets; identical asset order on every page."
  }
  d <- d[d$symbol %in% symbols, ]
  d$symbol <- factor(d$symbol, levels = rev(symbols))
  list(data = d, symbols = symbols, note = note)
}

plot_asset_risk <- function(results, horizon = min(results$horizon),
                            n_assets = 20L, symbols = NULL,
                            metric = c("estimate", "se"),
                            layout = c("time", "horizon"), date = max(results$date)) {
  metric <- match.arg(metric); layout <- match.arg(layout)
  d <- if (layout == "time") results[results$horizon == horizon, , drop = FALSE] else
    results[results$date == as.Date(date), , drop = FALSE]
  cells <- length(unique(d$date)) * length(unique(d$type)) * length(unique(d$index)) *
    length(unique(d$horizon))
  selected <- .asset_subset(d, n_assets, symbols, cells)
  d <- selected$data
  d$value <- d[[metric]]
  finite <- d$value[is.finite(d$value)]
  maximum <- if (length(finite)) max(finite) else 0
  upper <- if (maximum > 0) maximum else 1
  d$text_colour <- ifelse(is.finite(d$value) & d$value / upper > .55, "white", "#202020")
  d$cell_label <- ifelse(is.finite(d$value),
    if (metric == "se") sprintf("%.2f", d$value) else sprintf("%.0f", d$value), "--")
  d$horizon_label <- factor(paste0(d$horizon, "y"),
    levels = paste0(sort(unique(d$horizon)), "y"))
  dates <- sort(unique(d$date))
  time <- as.numeric(dates)
  edges <- if (length(time) == 1L) c(time - .5, time + .5) else
    c(time[1] - diff(time[1:2]) / 2,
      (utils::head(time, -1) + utils::tail(time, -1)) / 2,
      utils::tail(time, 1) + diff(utils::tail(time, 2)) / 2)
  column <- match(d$date, dates)
  d$left <- as.Date(edges[column], origin = "1970-01-01")
  d$right <- as.Date(edges[column + 1L], origin = "1970-01-01")
  d$row <- as.integer(d$symbol)
  colours <- if (metric == "estimate")
    c("#FFFFCC", "#FED976", "#FD8D3C", "#E31A1C", "#800026") else
    c("#F7FBFF", "#9ECAE1", "#4292C6", "#08519C")
  legend <- if (metric == "estimate") "Estimated g / h" else "Newey-West SE"
  heading <- if (metric == "estimate") "Drawdown risk" else "Estimation uncertainty"
  subtitle <- if (layout == "time") paste0("Trailing ", horizon, "y windows") else
    paste0("Evaluation date: ", as.Date(date))
  caption <- paste0("Shown: ", length(selected$symbols), " of ", length(unique(results$symbol)),
    " analysed assets. Common colour scale across drawdown types.\n", selected$note,
    if (metric == "se")
      "\nSEs refer to continuous approximations. Grey/--: unavailable, not zero; see status in results. Type 3: record-process approximation."
    else "\nHigher index = higher drawdown risk. Grey/--: unavailable estimate.")
  pages <- lapply(sort(unique(d$type)), function(type) {
    page <- d[d$type == type, ]
    if (layout == "time") {
      # Adjacent rectangles share the exact same date boundary.
      p <- ggplot2::ggplot(page, ggplot2::aes(fill = value)) +
        ggplot2::geom_rect(ggplot2::aes(xmin = left, xmax = right,
                                     ymin = row - .45, ymax = row + .45)) +
        ggplot2::scale_x_date(date_labels = "%Y-%m") +

        ggplot2::scale_y_continuous(breaks = seq_along(selected$symbols),
                                   labels = rev(selected$symbols))
    } else {
      p <- ggplot2::ggplot(page, ggplot2::aes(x = horizon_label, y = symbol, fill = value)) +
        ggplot2::geom_tile(colour = "white", height = .95) +
        ggplot2::geom_text(ggplot2::aes(label = cell_label, colour = text_colour), size = 3) +
        ggplot2::scale_colour_identity() +
        ggplot2::scale_y_discrete(drop = FALSE)
    }
    p + ggplot2::facet_grid(. ~ index) +
      ggplot2::scale_fill_gradientn(colours = colours, limits = c(0, upper),
        breaks = if (metric == "estimate") function(x) unique(round(pretty(x))) else ggplot2::waiver(),
        na.value = "grey80", name = legend) +
      ggplot2::labs(title = paste0(heading, " - drawdown ", type),
        subtitle = subtitle, x = if (layout == "horizon") "Trailing horizon" else NULL,
        y = NULL, caption = caption) + ggplot2::theme_minimal(base_size = 11) +
      ggplot2::theme(panel.grid = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_text(size = 9),
        plot.caption = ggplot2::element_text(size = 8, colour = "grey40", hjust = 0))
  })
  names(pages) <- paste0("type_", sort(unique(d$type)))
  attr(pages, "symbols") <- selected$symbols
  pages
}

plot_risk_series <- function(results, horizon = min(results$horizon),
                             n_assets = 6L, symbols = NULL) {
  d <- results[results$horizon == horizon, , drop = FALSE]
  cells <- length(unique(d$date)) * length(unique(d$type)) * length(unique(d$index))
  selected <- .asset_subset(d, n_assets, symbols, cells)
  d <- selected$data
  d <- d[order(d$symbol, d$index, d$date), ]
  maximum <- max(d$estimate, na.rm = TRUE)
  pages <- lapply(sort(unique(d$type)), function(type) {
    ggplot2::ggplot(d[d$type == type, ], ggplot2::aes(date, estimate, group = symbol)) +
      ggplot2::geom_step(colour = "#155E75", linewidth = .65, na.rm = TRUE) +
      ggplot2::facet_grid(symbol ~ index) +
      ggplot2::scale_y_continuous(limits = c(0, max(1, maximum)),
        breaks = function(x) unique(round(pretty(x, n = 3)))) +
      ggplot2::labs(title = paste0("Time-varying risk - drawdown ", type),
        subtitle = paste0("Monthly estimates from trailing ", horizon, "y windows"),
        x = NULL, y = "Estimated index",
        caption = paste0("Shown: ", length(selected$symbols), " of ", length(unique(results$symbol)),
          " assets. Common index scale across all pages.\nEach step is an integer estimate; overlapping windows are dependent.")) +
      ggplot2::theme_minimal(base_size = 10) +
      ggplot2::theme(panel.grid.minor = ggplot2::element_blank(),
        plot.caption = ggplot2::element_text(size = 8, colour = "grey40", hjust = 0))
  })
  names(pages) <- paste0("type_", sort(unique(d$type)))
  attr(pages, "symbols") <- selected$symbols
  pages
}

.class_fill <- function(maximum) {
  ggplot2::scale_fill_viridis_c(option = "C", direction = -1,
    limits = c(0, maximum), trans = "sqrt",
    labels = function(x) paste0(format(round(100 * x, 1), trim = TRUE), "%"),
    na.value = "grey85", name = "Assets in class (%)")
}

plot_groups <- function(results, date = max(results$date)) {
  d <- risk_table(results, date, wide = FALSE)
  d$total <- stats::ave(d$n_assets, interaction(d$type, d$index, d$horizon), FUN = sum)
  d$share <- d$n_assets / d$total
  maximum <- max(d$share)
  d$text_colour <- ifelse(sqrt(d$share / maximum) > .5, "white", "#202020")
  eligible <- unique(d[c("horizon", "total")])
  eligible <- eligible[order(eligible$horizon), ]
  asset_note <- paste0("Assets analysed: ", length(unique(results$symbol)),
    ". Eligible: ", paste0(eligible$horizon, "y = ", eligible$total, collapse = "; "), ".")
  d$horizon_label <- factor(paste0(d$horizon, "y"),
                              levels = paste0(sort(unique(d$horizon)), "y"))
  d$type <- paste("Drawdown", d$type)
  d$label <- sprintf("%d | %.2f | %.1f%%", d$n_assets, d$sharpe, d$volatility)
  ggplot2::ggplot(d, ggplot2::aes(x = horizon_label, y = class, fill = share)) +
    ggplot2::geom_tile(colour = "white") +
    ggplot2::geom_text(ggplot2::aes(label = label, colour = text_colour), size = 2.7) +
    ggplot2::scale_colour_identity() +
    ggplot2::facet_grid(type ~ index, scales = "free_y") +
    .class_fill(maximum) +
    ggplot2::scale_y_continuous(breaks = function(x) unique(round(pretty(x)))) +
    ggplot2::labs(x = "Trailing horizon", y = "Risk class (integer index)",
                   title = paste("Drawdown risk groups:", as.Date(date)),
                   caption = paste0(asset_note, "\nCells: asset count | mean individual annualized Sharpe | mean individual annualized volatility.\nGroups are descriptive; cells do not represent portfolio performance.")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(size = 8, colour = "grey40", hjust = 0))
}

plot_risk_time <- function(results, horizon = min(results$horizon)) {
  d <- results[results$horizon == horizon & !is.na(results$estimate), , drop = FALSE]
  if (!nrow(d)) stop("No eligible observations for this horizon.")
  d$class <- d$estimate
  counts <- stats::aggregate(list(n_assets = rep(1L, nrow(d))),
                             d[c("date", "type", "index", "class")], sum)
  totals <- stats::aggregate(n_assets ~ date + type + index, counts, sum)
  names(totals)[4L] <- "total"
  latest <- totals$total[totals$date == max(totals$date)]
  asset_note <- paste0("Assets analysed: ", length(unique(results$symbol)),
    ". Eligible per date: ", min(totals$total), "-", max(totals$total),
    "; latest: ", paste(unique(latest), collapse = "/"), ".")
  counts <- merge(counts, totals, by = c("date", "type", "index"))
  # Fill zero-frequency classes on the actual evaluation grid.
  classes <- unique(counts[c("type", "index", "class")])
  grid <- merge(classes, data.frame(date = sort(unique(d$date))), by = NULL)
  counts <- merge(grid, counts, by = c("date", "type", "index", "class"), all.x = TRUE)
  counts$n_assets[is.na(counts$n_assets)] <- 0L
  counts$total <- NULL
  counts <- merge(counts, totals, by = c("date", "type", "index"), all.x = TRUE)
  counts$share <- counts$n_assets / counts$total
  counts$type <- paste("Drawdown", counts$type)
  ggplot2::ggplot(counts, ggplot2::aes(x = date, y = class, fill = share)) +
    ggplot2::geom_tile(width = 32, height = 1) +
    ggplot2::facet_grid(type ~ index, scales = "free_y") +
    .class_fill(max(counts$share, na.rm = TRUE)) +
    ggplot2::scale_y_continuous(breaks = function(x) unique(round(pretty(x)))) +
    ggplot2::labs(x = NULL, y = "Risk class (integer index)",
                   title = paste0("Risk classes over time: trailing ", horizon, "y"),
                   caption = paste0(asset_note, "\nPercentages use eligible assets at each date. Grey: no eligible assets. Overlapping windows are not independent.")) +
    ggplot2::theme_minimal(base_size = 11) +
    ggplot2::theme(panel.grid = ggplot2::element_blank(),
                   plot.caption = ggplot2::element_text(size = 8, colour = "grey40", hjust = 0))
}

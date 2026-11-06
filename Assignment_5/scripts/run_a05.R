# Data filters, multi-timeframe OHLCV aggregation, indicators, and example rules.
# Source the indicator files first or run this file through run_a05() from the repo root.
filter_stock_data <- function(data, start_date = NULL, end_date = NULL,
                              timeframe = c("daily", "weekly", "monthly")) {
  timeframe <- match.arg(timeframe)
  if (!is.data.frame(data) || !all(c("Date", "Open", "High", "Low", "Close", "Volume", "Price") %in% names(data))) {
    stop("data must contain Date, Open, High, Low, Close, Volume, and Price columns.")
  }
  if (!is.null(start_date)) data <- data[data$Date >= as.Date(start_date), , drop = FALSE]
  if (!is.null(end_date)) data <- data[data$Date <= as.Date(end_date), , drop = FALSE]
  data <- data[order(data$Date), , drop = FALSE]
  if (nrow(data) == 0L) stop("No rows remain after applying the date filter.")
  if (timeframe == "daily") return(data)
  bucket <- if (timeframe == "weekly") format(data$Date, "%G-%V") else format(data$Date, "%Y-%m")
  groups <- split(seq_len(nrow(data)), factor(bucket, levels = unique(bucket)))
  rows <- lapply(groups, function(ix) {
    x <- data[ix, , drop = FALSE]
    data.frame(Date = x$Date[nrow(x)], Open = x$Open[1L], High = max(x$High, na.rm = TRUE),
               Low = min(x$Low, na.rm = TRUE), Close = x$Close[nrow(x)],
               Volume = sum(x$Volume, na.rm = TRUE), Price = x$Price[nrow(x)])
  })
  do.call(rbind, rows)
}

.a05_read_symbol <- function(symbol, data_dir, price_source) {
  path <- file.path(data_dir, paste0(symbol, "_daily_data.csv"))
  if (!file.exists(path)) stop(paste("A02 daily-data file not found:", path))
  raw <- read.csv(path, stringsAsFactors = FALSE, check.names = FALSE)
  date_col <- which(tolower(names(raw)) == "date")
  if (length(date_col) == 0L) stop(paste("Date column not found in", path))
  find_col <- function(suffix) {
    preferred <- paste0(symbol, ".", suffix)
    hit <- match(preferred, names(raw))
    if (!is.na(hit)) return(hit)
    hit <- which(tolower(names(raw)) == tolower(suffix))
    if (length(hit)) return(hit[1L])
    NA_integer_
  }
  get_num <- function(suffix, fallback = NA_real_) {
    j <- find_col(suffix)
    if (is.na(j)) return(rep(fallback, nrow(raw)))
    suppressWarnings(as.numeric(raw[[j]]))
  }
  source_col <- find_col(price_source)
  if (is.na(source_col)) stop(paste("Requested price source", price_source, "not found for", symbol))
  dates <- as.Date(as.character(raw[[date_col[1L]]]), tryFormats = c("%Y-%m-%d", "%m/%d/%Y", "%Y/%m/%d"))
  if (anyNA(dates)) stop(paste("Some dates could not be parsed in", path))
  out <- data.frame(Date = dates, Open = get_num("Open"), High = get_num("High"),
                    Low = get_num("Low"), Close = get_num("Close"),
                    Volume = get_num("Volume"), Price = suppressWarnings(as.numeric(raw[[source_col]])))
  out[order(out$Date), , drop = FALSE]
}

.a05_pad <- function(values, n, first_index = 1L) {
  out <- rep(NA_real_, n)
  if (length(values) > 0L) {
    ix <- seq.int(first_index, length.out = length(values))
    ix <- ix[ix <= n]
    out[ix] <- values[seq_along(ix)]
  }
  out
}

.a05_roll_sma <- function(x, period) {
  out <- rep(NA_real_, length(x))
  if (length(x) >= period) out[period:length(x)] <- sma(x, period)
  out
}

.a05_roll_stdev <- function(x, period) {
  out <- rep(NA_real_, length(x))
  if (length(x) >= period) {
    for (i in period:length(x)) out[i] <- stdev(x[(i - period + 1L):i])
  }
  out
}

.a05_roll_linreg <- function(x, period) {
  slopes <- rep(NA_real_, length(x)); fitted <- rep(NA_real_, length(x))
  if (length(x) >= period) {
    for (i in period:length(x)) {
      fit <- linreg(x[(i - period + 1L):i], regressionLength = period, regressionOffset = 0L)
      slopes[i] <- fit$slope
      fitted[i] <- fit$predicted_values[length(fit$predicted_values)]
    }
  }
  list(slope = slopes, fitted = fitted)
}

.a05_enrich <- function(data) {
  p <- data$Price; n <- length(p)
  ema12 <- ema(p, 12L); ema26 <- ema(p, 26L)
  macd_values <- macd(p, 12L, 26L, 9L)
  stoch <- stoch_rsi(p, 14L, 3L, 3L)
  stoch_k <- .a05_pad(stoch$k_line, n, 3L)
  stoch_d <- .a05_pad(stoch$d_line, n, 5L)
  cross <- crossover(ema12, ema26)
  under <- crossunder(ema12, ema26)
  rsi14 <- rsi(p, 14L)
  buy <- cross == "Up" & !is.na(rsi14) & rsi14 < 70
  sell <- under | (!is.na(rsi14) & rsi14 > 70)
  rule <- rep("Hold", n); rule[buy] <- "Buy"; rule[sell] <- "Sell"
  lr <- .a05_roll_linreg(p, 20L)
  data$SMA_20 <- .a05_roll_sma(p, 20L)
  data$EMA_12 <- ema12; data$EMA_26 <- ema26
  data$MACD_Line <- macd_values$macd_line
  data$MACD_Signal <- macd_values$signal_line
  data$MACD_Histogram <- macd_values$histogram
  data$StDev_20_Population <- .a05_roll_stdev(p, 20L)
  data$LinReg_Slope_20 <- lr$slope; data$LinReg_Fitted_20 <- lr$fitted
  data$RSI_14 <- rsi14; data$StochRSI_K_14_3 <- stoch_k; data$StochRSI_D_14_3_3 <- stoch_d
  data$EMA_Crossover <- cross; data$EMA_Crossunder <- under
  data$Example_Rule <- rule
  data
}

run_a05 <- function(repo_root = ".", data_dir = file.path(repo_root, "Assignment_5", "data"),
                    portfolio_file = file.path(repo_root, "Assignment_5", "portfolio.txt"),
                    start_date = NULL, end_date = NULL,
                    timeframe = c("daily", "weekly", "monthly"),
                    price_source = c("Adjusted", "Close")) {
  timeframe <- match.arg(timeframe); price_source <- match.arg(price_source)
  script_dir <- file.path(repo_root, "Assignment_5", "scripts")
  files <- c("sma.R", "ema.R", "macd.R", "stdev.R", "linreg.R", "rsi.R",
             "stoch_rsi.R", "crossover.R", "crossunder.R")
  for (f in files) source(file.path(script_dir, f), local = .GlobalEnv)
  if (!file.exists(portfolio_file)) stop(paste("Portfolio file not found:", portfolio_file))
  symbols <- trimws(readLines(portfolio_file, warn = FALSE, encoding = "UTF-8"))
  symbols <- symbols[nzchar(symbols) & !startsWith(symbols, "#")]
  if (length(symbols) == 0L) stop("No symbols found in portfolio.txt.")
  out_dir <- file.path(repo_root, "Assignment_5", "outputs")
  if (!dir.exists(out_dir)) dir.create(out_dir, recursive = TRUE)
  results <- setNames(vector("list", length(symbols)), symbols)
  for (symbol in symbols) {
    raw <- .a05_read_symbol(symbol, data_dir, price_source)
    filtered <- filter_stock_data(raw, start_date, end_date, timeframe)
    enriched <- .a05_enrich(filtered)
    output_path <- file.path(out_dir, paste0(symbol, "_", timeframe, "_indicators.csv"))
    write.csv(enriched, output_path, row.names = FALSE, na = "")
    cat("Saved", output_path, "\n")
    results[[symbol]] <- enriched
  }
  invisible(results)
}

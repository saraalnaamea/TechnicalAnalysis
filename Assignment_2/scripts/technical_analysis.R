# BDA400 Assignment 2 - Technical Analysis Using R
# Reads portfolio.txt, downloads daily Yahoo Finance data, calculates statistics,
# and writes CSV and PNG outputs. Run from the repository root.

required_packages <- c("quantmod", "TTR", "xts", "zoo")
missing_packages <- required_packages[!vapply(required_packages, requireNamespace,
                                               logical(1), quietly = TRUE)]
if (length(missing_packages) > 0) {
  stop("Missing packages: ", paste(missing_packages, collapse = ", "),
       ". Install quantmod and TTR first with install.packages().")
}

read_portfolio <- function(path = "Assignment_2/portfolio.txt") {
  if (!file.exists(path)) stop("Portfolio file not found: ", path)
  symbols <- readLines(path, warn = FALSE, encoding = "UTF-8")
  symbols <- sub("#.*$", "", symbols)
  symbols <- toupper(trimws(symbols))
  symbols <- symbols[nzchar(symbols)]
  symbols <- unique(symbols)
  if (length(symbols) == 0) stop("portfolio.txt contains no stock symbols.")
  symbols
}

load_stock_data <- function(portfolio_file = "Assignment_2/portfolio.txt",
                            from = Sys.Date() - 365,
                            to = Sys.Date()) {
  symbols <- read_portfolio(portfolio_file)
  stock_frames <- list()
  for (symbol in symbols) {
    message("Downloading ", symbol, " from Yahoo Finance...")
    raw <- tryCatch(
      quantmod::getSymbols(Symbols = symbol, src = "yahoo", from = from, to = to,
                           auto.assign = FALSE, warnings = TRUE),
      error = function(e) {
        warning("Could not download ", symbol, ": ", conditionMessage(e))
        NULL
      }
    )
    if (is.null(raw) || NROW(raw) == 0) next
    frame <- data.frame(
      Date = as.Date(zoo::index(raw)),
      as.data.frame(zoo::coredata(raw)),
      check.names = FALSE
    )
    stock_frames[[symbol]] <- frame
  }
  if (length(stock_frames) == 0) {
    stop("No stock data were downloaded. Check the symbols and internet connection.")
  }
  stock_frames
}

find_price_column <- function(stock_frame) {
  adjusted <- grep("Adjusted$", names(stock_frame), value = TRUE)
  if (length(adjusted) > 0) return(adjusted[[1]])
  closing <- grep("Close$", names(stock_frame), value = TRUE)
  if (length(closing) == 0) stop("No Adjusted or Close price column was found.")
  closing[[1]]
}

mode_rounded <- function(values, digits = 2L) {
  clean <- round(as.numeric(values), digits = digits)
  clean <- clean[is.finite(clean)]
  if (length(clean) == 0) return(NA_character_)
  counts <- table(formatC(clean, format = "f", digits = digits))
  highest <- max(counts)
  if (highest == 1) return("No repeated value")
  paste(names(counts)[counts == highest], collapse = ", ")
}

last_non_missing <- function(values) {
  values <- as.numeric(values)
  values <- values[is.finite(values)]
  if (length(values) == 0) return(NA_real_)
  tail(values, 1)
}

calculate_statistics <- function(stock_frames, moving_average_period = 20L) {
  rows <- lapply(names(stock_frames), function(symbol) {
    frame <- stock_frames[[symbol]]
    price_column <- find_price_column(frame)
    prices <- as.numeric(frame[[price_column]])
    valid <- is.finite(prices)
    dates <- as.Date(frame$Date[valid])
    prices <- prices[valid]
    if (length(prices) == 0) stop("No usable prices for ", symbol)
    sma <- TTR::SMA(prices, n = moving_average_period)
    data.frame(
      Ticker = symbol,
      Observations = length(prices),
      Start_Date = min(dates),
      End_Date = max(dates),
      Latest_Adjusted_Close = tail(prices, 1),
      Latest_SMA_20 = last_non_missing(sma),
      Mean_Adjusted_Close = mean(prices),
      Mode_Adjusted_Close_Rounded_2dp = mode_rounded(prices, 2L),
      Median_Adjusted_Close = median(prices),
      SD_Adjusted_Close = if (length(prices) > 1) stats::sd(prices) else NA_real_,
      stringsAsFactors = FALSE
    )
  })
  do.call(rbind, rows)
}

display_stock_data <- function(stock_frames, statistics,
                               output_dir = "Assignment_2/outputs",
                               moving_average_period = 20L) {
  dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  utils::write.csv(statistics,
                   file.path(output_dir, "statistics_summary.csv"),
                   row.names = FALSE, na = "")
  for (symbol in names(stock_frames)) {
    frame <- stock_frames[[symbol]]
    safe_symbol <- gsub("[^A-Za-z0-9._-]", "_", symbol)
    utils::write.csv(frame,
                     file.path(output_dir, paste0(safe_symbol, "_daily_data.csv")),
                     row.names = FALSE, na = "")
    price_column <- find_price_column(frame)
    prices <- as.numeric(frame[[price_column]])
    sma <- TTR::SMA(prices, n = moving_average_period)
    png(file.path(output_dir, paste0(safe_symbol, "_adjusted_close_sma20.png")),
        width = 1200, height = 750, res = 140)
    graphics::plot(frame$Date, prices, type = "l", col = "#173B70", lwd = 1.5,
                   xlab = "Date", ylab = "Adjusted closing price",
                   main = paste(symbol, "- Adjusted Close and 20-Day SMA"))
    available <- is.finite(sma)
    if (any(available)) {
      graphics::lines(frame$Date[available], sma[available],
                      col = "#D46B35", lwd = 2)
      graphics::legend("topleft", legend = c("Adjusted Close", "20-Day SMA"),
                       col = c("#173B70", "#D46B35"), lty = 1, bty = "n")
    } else {
      graphics::legend("topleft", legend = "Adjusted Close", col = "#173B70",
                       lty = 1, bty = "n")
      graphics::mtext("Not enough observations for the selected moving-average window",
                      side = 3, line = 0.2, cex = 0.75)
    }
    grDevices::dev.off()
    cat("\n", symbol, " - most recent rows\n", sep = "")
    print(utils::tail(frame, 5))
  }
  cat("\nSummary statistics\n")
  print(statistics, row.names = FALSE)
  message("CSV tables and chart files saved in: ", output_dir)
  invisible(statistics)
}

run_technical_analysis <- function(
  portfolio_file = "Assignment_2/portfolio.txt",
  from = Sys.Date() - 365,
  to = Sys.Date(),
  moving_average_period = 20L,
  output_dir = "Assignment_2/outputs"
) {
  stock_frames <- load_stock_data(portfolio_file, from, to)
  statistics <- calculate_statistics(stock_frames, moving_average_period)
  display_stock_data(stock_frames, statistics, output_dir, moving_average_period)
  invisible(list(data = stock_frames, statistics = statistics))
}

# Running this file with Rscript starts the analysis. When sourced interactively,
# call run_technical_analysis() after setting the working directory to the repo root.
if (sys.nframe() == 0L) {
  run_technical_analysis()
}

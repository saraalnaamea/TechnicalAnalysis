# Moving Average Convergence Divergence (MACD) - BDA400 Assignment 5
# Source ema.R before calling macd().
macd <- function(data, short_period, long_period, signal_period) {
  if (!exists("ema", mode = "function")) stop("Source ema.R before macd.R.")
  periods <- list(short_period = short_period, long_period = long_period,
                  signal_period = signal_period)
  for (nm in names(periods)) {
    x <- periods[[nm]]
    if (length(x) != 1L || is.na(x) || !is.numeric(x) || x < 1 || x != floor(x)) {
      stop(paste(nm, "must be a positive integer."))
    }
  }
  if (short_period >= long_period) stop("short_period must be less than long_period.")
  short_ema <- ema(data, as.integer(short_period))
  long_ema <- ema(data, as.integer(long_period))
  macd_line <- short_ema - long_ema
  signal_line <- ema(macd_line, as.integer(signal_period))
  histogram <- macd_line - signal_line
  list(macd_line = macd_line, signal_line = signal_line, histogram = histogram)
}

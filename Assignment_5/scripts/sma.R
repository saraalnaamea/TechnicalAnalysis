# Simple Moving Average (SMA) - BDA400 Assignment 5
# Uses only base R. A window containing NA returns NA.
sma <- function(data, period) {
  if (!is.numeric(data) || is.complex(data) || length(data) == 0L) {
    stop("data must be a non-empty numeric vector.")
  }
  if (any(is.infinite(data))) stop("data must not contain Inf or -Inf.")
  if (length(period) != 1L || is.na(period) || !is.numeric(period) ||
      period < 1 || period != floor(period)) {
    stop("period must be a positive integer.")
  }
  period <- as.integer(period)
  if (length(data) < period) {
    stop("Data length should be greater than or equal to the period.")
  }
  n_out <- length(data) - period + 1L
  sma_values <- rep(NA_real_, n_out)
  for (i in seq_len(n_out)) {
    current_window <- data[i:(i + period - 1L)]
    if (!anyNA(current_window)) sma_values[i] <- sum(current_window) / period
  }
  sma_values
}

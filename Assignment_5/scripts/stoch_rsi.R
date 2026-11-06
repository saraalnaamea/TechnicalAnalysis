# Stochastic RSI - BDA400 Assignment 5
# Follows the assignment pseudocode: normalize using the full valid RSI range,
# then smooth %K and %D with SMA. Source rsi.R and sma.R first.
stoch_rsi <- function(data, period, k_period, d_period) {
  if (!exists("rsi", mode = "function") || !exists("sma", mode = "function")) {
    stop("Source rsi.R and sma.R before stoch_rsi.R.")
  }
  for (entry in list(period = period, k_period = k_period, d_period = d_period)) {
    if (length(entry) != 1L || is.na(entry) || !is.numeric(entry) ||
        entry < 1 || entry != floor(entry)) stop("period, k_period, and d_period must be positive integers.")
  }
  rsi_values <- rsi(data, as.integer(period))
  valid <- rsi_values[!is.na(rsi_values)]
  if (length(valid) == 0L) {
    return(list(k_line = numeric(0), d_line = numeric(0)))
  }
  min_rsi <- min(valid)
  max_rsi <- max(valid)
  k_values <- rep(NA_real_, length(rsi_values))
  valid_index <- which(!is.na(rsi_values))
  if (max_rsi == min_rsi) {
    k_values[valid_index] <- 0.5
  } else {
    k_values[valid_index] <- (rsi_values[valid_index] - min_rsi) / (max_rsi - min_rsi)
  }
  if (length(k_values) < as.integer(k_period)) return(list(k_line = numeric(0), d_line = numeric(0)))
  k_line <- sma(k_values, as.integer(k_period))
  if (length(k_line) < as.integer(d_period)) d_line <- numeric(0) else d_line <- sma(k_line, as.integer(d_period))
  list(k_line = k_line, d_line = d_line)
}

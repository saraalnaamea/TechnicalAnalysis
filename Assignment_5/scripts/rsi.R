# Relative Strength Index (RSI) - BDA400 Assignment 5
# Wilder smoothing; the first RSI is seeded by the first 'period' price changes.
rsi <- function(data, period) {
  if (!is.numeric(data) || is.complex(data) || length(data) == 0L || anyNA(data)) {
    stop("data must be a non-empty numeric vector without NA values.")
  }
  if (any(!is.finite(data))) stop("data must contain only finite values.")
  if (length(period) != 1L || is.na(period) || !is.numeric(period) ||
      period < 1 || period != floor(period)) {
    stop("period must be a positive integer.")
  }
  period <- as.integer(period)
  rsi_values <- rep(NA_real_, length(data))
  if (length(data) <= period) return(rsi_values)
  diff_values <- diff(data)
  gains <- pmax(diff_values, 0)
  losses <- pmax(-diff_values, 0)
  avg_gain <- sum(gains[seq_len(period)]) / period
  avg_loss <- sum(losses[seq_len(period)]) / period
  calculate_rsi <- function(gain, loss) {
    if (loss == 0 && gain == 0) return(50)
    if (loss == 0) return(100)
    rs <- gain / loss
    100 - (100 / (1 + rs))
  }
  rsi_values[period + 1L] <- calculate_rsi(avg_gain, avg_loss)
  if (length(data) > period + 1L) {
    for (i in (period + 2L):length(data)) {
      change_index <- i - 1L
      avg_gain <- (avg_gain * (period - 1L) + gains[change_index]) / period
      avg_loss <- (avg_loss * (period - 1L) + losses[change_index]) / period
      rsi_values[i] <- calculate_rsi(avg_gain, avg_loss)
    }
  }
  rsi_values
}

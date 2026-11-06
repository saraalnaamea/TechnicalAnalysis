# Exponential Moving Average (EMA) - BDA400 Assignment 5
# The first observation seeds the recurrence, as specified in the assignment.
ema <- function(data, period) {
  if (!is.numeric(data) || is.complex(data) || length(data) == 0L || anyNA(data)) {
    stop("data must be a non-empty numeric vector without NA values.")
  }
  if (any(!is.finite(data))) stop("data must contain only finite values.")
  if (length(period) != 1L || is.na(period) || !is.numeric(period) ||
      period < 1 || period != floor(period)) {
    stop("period must be a positive integer.")
  }
  period <- as.integer(period)
  multiplier <- 2 / (period + 1)
  ema_values <- numeric(length(data))
  ema_values[1L] <- data[1L]
  if (length(data) > 1L) {
    for (i in 2:length(data)) {
      ema_values[i] <- (data[i] - ema_values[i - 1L]) * multiplier + ema_values[i - 1L]
    }
  }
  ema_values
}

# Population Standard Deviation - BDA400 Assignment 5
# The assignment formula divides by n (population SD), not n - 1.
stdev <- function(data) {
  if (!is.numeric(data) || is.complex(data) || length(data) == 0L || anyNA(data)) {
    stop("data must be a non-empty numeric vector without NA values.")
  }
  if (any(!is.finite(data))) stop("data must contain only finite values.")
  mean_value <- sum(data) / length(data)
  diff_values <- data - mean_value
  squared_diff <- diff_values * diff_values
  variance <- sum(squared_diff) / length(squared_diff)
  sqrt(variance)
}

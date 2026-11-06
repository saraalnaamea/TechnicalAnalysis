# Simple Linear Regression - BDA400 Assignment 5
# Uses the end-aligned subset and offset formula from the assignment pseudocode.
linreg <- function(regressionSource, regressionLength, regressionOffset) {
  if (!is.numeric(regressionSource) || is.complex(regressionSource) ||
      length(regressionSource) == 0L || anyNA(regressionSource)) {
    stop("regressionSource must be a non-empty numeric vector without NA values.")
  }
  if (any(!is.finite(regressionSource))) stop("regressionSource must contain only finite values.")
  if (length(regressionLength) != 1L || is.na(regressionLength) ||
      !is.numeric(regressionLength) || regressionLength < 2 ||
      regressionLength != floor(regressionLength)) {
    stop("regressionLength must be an integer of at least 2.")
  }
  if (length(regressionOffset) != 1L || is.na(regressionOffset) ||
      !is.numeric(regressionOffset) || regressionOffset < 0 ||
      regressionOffset != floor(regressionOffset)) {
    stop("regressionOffset must be a non-negative integer.")
  }
  n <- length(regressionSource)
  regressionLength <- as.integer(regressionLength)
  regressionOffset <- as.integer(regressionOffset)
  if (regressionLength > n) stop("regressionLength cannot be greater than the number of elements in regressionSource.")
  if (regressionOffset >= regressionLength) stop("regressionOffset must be less than regressionLength.")
  start_index <- max(1L, n - regressionLength + regressionOffset)
  end_index <- min(n, n - regressionOffset)
  source_subset <- regressionSource[start_index:end_index]
  if (length(source_subset) < 2L) stop("The selected regression window must contain at least two points.")
  index_values <- seq_along(source_subset)
  mean_index <- sum(index_values) / length(index_values)
  mean_source <- sum(source_subset) / length(source_subset)
  numerator <- sum((index_values - mean_index) * (source_subset - mean_source))
  denominator <- sum((index_values - mean_index)^2)
  if (denominator == 0) stop("The regression window must contain at least two distinct index positions.")
  slope <- numerator / denominator
  intercept <- mean_source - slope * mean_index
  predicted_values <- slope * index_values + intercept
  list(slope = slope, intercept = intercept, predicted_values = predicted_values)
}

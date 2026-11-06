# Crossover events - BDA400 Assignment 5
# Returns the assignment's three states: None, Up, or Down.
crossover <- function(arr1, arr2) {
  if (!is.numeric(arr1) || !is.numeric(arr2) || is.complex(arr1) || is.complex(arr2)) {
    stop("Both arrays must be numeric vectors.")
  }
  if (length(arr1) != length(arr2)) stop("Both arrays should have the same length.")
  if (length(arr1) == 0L) stop("Both arrays must contain at least one value.")
  if (any(is.infinite(arr1)) || any(is.infinite(arr2))) stop("Arrays must not contain Inf or -Inf.")
  signals <- rep("None", length(arr1))
  if (length(arr1) > 1L) {
    for (i in 2:length(arr1)) {
      vals <- c(arr1[i], arr2[i], arr1[i - 1L], arr2[i - 1L])
      if (anyNA(vals)) next
      if (arr1[i] > arr2[i] && arr1[i - 1L] <= arr2[i - 1L]) signals[i] <- "Up"
      else if (arr1[i] < arr2[i] && arr1[i - 1L] >= arr2[i - 1L]) signals[i] <- "Down"
    }
  }
  signals
}

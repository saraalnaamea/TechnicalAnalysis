# Base-R verification suite for Sara Alnaameh - BDA400 Assignment 5
# From the repository root: source("Assignment_5/scripts/test_a05.R"); run_a05_tests()
source(file.path("Assignment_5", "scripts", "sma.R"))
source(file.path("Assignment_5", "scripts", "ema.R"))
source(file.path("Assignment_5", "scripts", "macd.R"))
source(file.path("Assignment_5", "scripts", "stdev.R"))
source(file.path("Assignment_5", "scripts", "linreg.R"))
source(file.path("Assignment_5", "scripts", "rsi.R"))
source(file.path("Assignment_5", "scripts", "stoch_rsi.R"))
source(file.path("Assignment_5", "scripts", "crossover.R"))
source(file.path("Assignment_5", "scripts", "crossunder.R"))

.expect_error <- function(expr) inherits(tryCatch({ force(expr); NULL }, error = identity), "error")
run_a05_tests <- function() {
  sample <- c(20, 22, 21, 25, 27, 26, 30, 31)
  sma_expected <- if (length(sample) == 9L) c(12.3333333333,15.6666666667,17.6666666667,20,21.6666666667,23.6666666667,23.3333333333) else c(21,22.6666666667,24.3333333333,26,27.6666666667,29)
  stopifnot(isTRUE(all.equal(sma(sample, 3L), sma_expected, tolerance=1e-8)))
  cat("PASS: SMA rolling windows and expected values.\n")
  stopifnot(isTRUE(all.equal(ema(c(10,12,15), 3L), c(10,11,13))))
  cat("PASS: EMA seed and recurrence.\n")
  m <- macd(seq(100,130,by=5), 3L, 5L, 2L)
  stopifnot(all(c("macd_line","signal_line","histogram") %in% names(m)),
            length(m$macd_line)==7L, length(m$signal_line)==7L,
            isTRUE(all.equal(m$histogram, m$macd_line-m$signal_line)))
  cat("PASS: MACD components, lengths, and histogram identity.\n")
  stopifnot(isTRUE(all.equal(stdev(c(1,2,3)), sqrt(2/3))))
  cat("PASS: population standard deviation (denominator n).\n")
  fit <- linreg(c(2,4,6,8,10), 5L, 0L)
  stopifnot(abs(fit$slope-2)<1e-10, abs(fit$intercept)<1e-10,
            isTRUE(all.equal(fit$predicted_values, c(2,4,6,8,10))))
  cat("PASS: linear regression slope, intercept, and fitted sequence.\n")
  rv <- rsi(c(1,2,3,4,3,2,3,4), 3L)
  stopifnot(length(rv)==8L, all(is.na(rv[1:3])), rv[4]==100,
            all(rv[!is.na(rv)]>=0 & rv[!is.na(rv)]<=100))
  cat("PASS: RSI seed, Wilder smoothing range, and leading NA positions.\n")
  sr <- stoch_rsi(cumsum(c(1,2,1,3,2,4,1,3,2,5,1,4,3,2,5,1,4,2,6,1,3,4,2,5,1,4,3,6,2,1)), 5L, 3L, 3L)
  stopifnot(is.list(sr), all(c("k_line","d_line") %in% names(sr)), length(sr$k_line)>0L,
            length(sr$d_line)>0L, all(sr$k_line[!is.na(sr$k_line)]>=0 & sr$k_line[!is.na(sr$k_line)]<=1))
  cat("PASS: StochRSI %K/%D output and normalized range.\n")
  cv <- crossover(c(1,2,3,1), c(2,2,2,2))
  stopifnot(identical(cv, c("None","None","Up","Down")))
  cat("PASS: crossover Up/Down state transitions.\n")
  cu <- crossunder(c(2,2,1,3), c(1,2,2,2))
  stopifnot(identical(cu, c(FALSE,FALSE,TRUE,FALSE)))
  cat("PASS: crossunder boolean event vector.\n")
  stopifnot(.expect_error(sma(c(1,2),3L)), .expect_error(ema(c(1,Inf),2L)),
            .expect_error(macd(c(1,2,3),3L,2L,1L)), .expect_error(linreg(c(1,2),2L,2L)),
            .expect_error(crossover(1:3,1:2)))
  cat("PASS: invalid inputs and parameter checks.\n")
  cat("All Assignment 5 indicator checks passed.\n")
  invisible(TRUE)
}

# BDA400 Assignment 5 - Technical Analysis Using R

**Student:** Sara Alnaameh  
**Repository:** https://github.com/saraalnaamea/TechnicalAnalysis  
**Portfolio tickers:** SHOP.TO, RY.TO, TD.TO

## Contents

- `scripts/sma.R`, `ema.R`, `macd.R`, `stdev.R`, `linreg.R`, `rsi.R`, `stoch_rsi.R`, `crossover.R`, and `crossunder.R` contain the nine required base-R functions.
- `portfolio.txt` lists this version's stock symbols. `data/` holds its input CSV files.
- `scripts/run_a05.R` reads the portfolio and CSVs from this Assignment 5 folder, applies date and daily/weekly/monthly filters, calculates the indicators, applies example EMA/RSI rules, and writes one CSV per symbol.
- `scripts/test_a05.R` contains synthetic checks for each function and invalid inputs.
- `reports/` contains the assignment explanation PDF.
- `outputs/` receives generated indicator CSV files after running the pipeline.

## Requirements and run steps

No R packages are required. Place this `Assignment_5` folder in the root of the relevant `TechnicalAnalysis` repository. Before running the analysis, put one CSV per ticker in `Assignment_5/data/`, named `<ticker>_daily_data.csv`. Each CSV must contain Date, Open, High, Low, Close, Volume, and Adjusted columns; provider exports with ticker-prefixed price headers, such as `AAPL.Adjusted`, are also accepted. Dates should be parseable as YYYY-MM-DD. You may copy the student's own provider CSVs into this folder or export fresh data from a reputable provider. The scripts do not require or read an `Assignment_2` path.

In RStudio, set the working directory to the repository root and run:

```r
source("Assignment_5/scripts/test_a05.R")
run_a05_tests()
source("Assignment_5/scripts/run_a05.R")
run_a05(timeframe = "daily", price_source = "Adjusted")
```

Optional filters:

```r
run_a05(start_date = "2026-01-01", end_date = "2026-10-31", timeframe = "weekly", price_source = "Close")
```

The example rule labels a row `Buy` when EMA(12) crosses above EMA(26) while RSI(14) is below 70, and `Sell` when EMA(12) crosses below EMA(26) or RSI(14) is above 70. These are instructional signals, not investment advice.

## Implementation notes

- SMA returns one value per complete window (`n - period + 1`); windows containing `NA` return `NA`.
- EMA follows the provided pseudocode and seeds the first value with the first observation.
- MACD returns aligned MACD, signal, and histogram vectors.
- `stdev()` implements the assignment's population formula, dividing by `n`.
- RSI uses an initial period average and Wilder smoothing, returning leading `NA` values before it can be calculated.
- StochRSI follows the supplied pseudocode's full-series RSI normalization, then smooths `%K` and `%D` with SMA.
- `crossover()` returns `None`, `Up`, or `Down`; `crossunder()` returns a logical vector, following the assignment's formula section.
- The included test script has not been run in the student’s RStudio session. Run it and review its output before submission. Save actual RStudio evidence only if the instructor requests it.

Check the course AI-use policy and ensure you can explain the submitted code and results.

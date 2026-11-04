# Technical Analysis - BDA400 Assignment 2

Student: Sara Alnaameh

This repository contains Assignment 2 materials for the preliminary technical-analysis project.

## Repository setup

Create a public GitHub repository named `TechnicalAnalysis` under the student account. Keep this project in `Assignment_2/`.

Repository URL: https://github.com/saraalnaamea/TechnicalAnalysis

## Portfolio

Symbols in `Assignment_2/portfolio.txt`: SHOP.TO, RY.TO, TD.TO. These symbols are practice examples, not investment recommendations.

## Requirements

- R installed locally.
- RStudio Desktop installed locally.
- R packages `quantmod` and `TTR` (their dependencies are installed as needed).
- Internet access for Yahoo Finance data retrieval.

Install packages in the R console:

```r
install.packages(c("quantmod", "TTR"), repos = "https://cloud.r-project.org")
```

From the repository root, run:

```sh
Rscript Assignment_2/scripts/technical_analysis.R
```

The script reads the portfolio file, retrieves daily price history, and writes CSV summaries and PNG charts to `Assignment_2/outputs/`. Data availability depends on the source and connection at run time.

## Files

- `Assignment_2/portfolio.txt` - symbols, one per line.
- `Assignment_2/scripts/technical_analysis.R` - import, statistics, and display functions.
- `Assignment_2/reports/Alnaameh_Sara_CA_BDA400_A02.docx` - report and screenshot evidence locations.
- `Assignment_2/screenshots/README.md` - evidence checklist.
- `Assignment_2/outputs/` - generated results after running the script.

The report contains labeled screenshot placeholders. Replace them with genuine screenshots from the student computer before submission.

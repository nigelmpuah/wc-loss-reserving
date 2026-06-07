# Workers' Compensation Loss Reserving

Estimating unpaid claim liabilities for a workers' compensation book using
chain-ladder and Bornhuetter-Ferguson methods, with Mack standard errors for
uncertainty. Built on real NAIC Schedule P regulatory data.

## What this does

A property-casualty insurer collects premium up front but pays workers' comp
claims out over many years. The unpaid portion is the **reserve**, and
estimating it is the core job of a reserving actuary. This project:

1. Builds a cumulative paid-loss development triangle for a single carrier
   (Federal Insurance Company, a Chubb subsidiary) from the CAS data.
2. Computes volume-weighted age-to-age development factors and projects each
   accident year to ultimate (chain-ladder).
3. Cross-checks the ultimates and attaches Mack standard errors via the
   `ChainLadder` package.
4. Runs Bornhuetter-Ferguson as a second method and compares the two.

## Headline results

| Method | Total reserve ($000s) |
|---|---|
| Chain-ladder | 381,599 |
| Mack (IBNR) | ~349,217, CV ≈ 16% |
| Bornhuetter-Ferguson (naive ELR 63.7%) | 750,247 |
| Bornhuetter-Ferguson (corrected ELR 40.0%) | 471,488 |

The chain-ladder reserve of **$381.6M** is the credible central estimate. The
Bornhuetter-Ferguson comparison is the interesting part: with a naive expected
loss ratio derived only from the two oldest (high-loss-ratio) accident years,
BF nearly doubles chain-ladder. Re-deriving the ELR from the chain-ladder
ultimates across the whole book (40.0%) pulls BF back alongside chain-ladder —
a concrete illustration that **BF is only as good as its a priori assumption.**
See the report for the full discussion.

## Figures

| | |
|---|---|
| `figures/ibnr_by_accident_year.png` | Chain-ladder IBNR with Mack ± 1 S.E. bands |
| `figures/cl_vs_bf_reserves.png` | Chain-ladder vs naive Bornhuetter-Ferguson |
| `figures/cl_vs_bf_corrected.png` | CL vs naive BF vs corrected-ELR BF |

## Running it

The data is **not** included in this repo (the CAS database has its own usage
terms). Download it first:

1. Get the workers' comp file `wkcomp_pos_98-07.csv` from the
   [CAS Loss Reserve Database](https://www.casact.org/publications-research/research/research-resources/loss-reserving-data-pulled-naic-schedule-p).
2. Place it in the repo root (same folder as `wc_loss_reserving.R`).
3. Install dependencies and run:

```r
install.packages(c("tidyverse", "ChainLadder"))
source("wc_loss_reserving.R")
```

The script prints the triangle, development factors, reserve tables, and the
method comparison, and writes the figures as PNGs.

## Files

- `wc_loss_reserving.R` — full analysis (chain-ladder, Mack, Bornhuetter-Ferguson)
- `WC_Loss_Reserving_Report.pdf` — written report with methodology and discussion
- `figures/` — output plots

## Methods & data

- **Data:** CAS Loss Reserve Database, NAIC Schedule P, workers' compensation.
  Federal Insurance Company (group code 388), accident years 1998–2006.
- **Chain-ladder:** volume-weighted age-to-age factors, no tail factor.
- **Mack:** distribution-free standard errors; triangle trimmed to square for
  this step (drops only the fully mature final development column).
- **Bornhuetter-Ferguson:** a priori = premium × ELR, applied to the unreported
  share `(1 − 1/CDF)`. ELR derived two ways: from mature years (naive) and from
  chain-ladder ultimates over total premium (corrected).

All monetary figures are in thousands of dollars, consistent with the source.

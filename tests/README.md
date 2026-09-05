# Calculation tests

From the repository root, run:

```sh
Rscript tests/run_tests.R
```

These tests require R and ChainLadder. They use synthetic data only; the CAS
file is not required. They exercise the same functions as the main analysis.

The small four-year triangle has hand-calculated development factors of
91/60, 40/33, and 11/10. Its unpaid losses are 0, 22, 72, and 1472/9, totalling
2318/9 in arbitrary units. Both the manual calculation and package must match.
A wider version tests the final-development-column bug without using real data.

Other checks cover BF arithmetic, full ELR precision, numeric column ordering,
valuation cutoffs, duplicate and missing records, premium definitions, and zero
denominators. A paid-loss decrease must be retained and returned for investigation.

These are implementation checks. They do not validate reserve estimates against
subsequent runoff outcomes or establish that the statistical assumptions hold.

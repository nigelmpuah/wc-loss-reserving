# =============================================================================
# Workers' Compensation Loss Reserving
# Nigel Mpuah
# Data: CAS Loss Reserve Database, Schedule P (wkcomp_pos_98-07.csv)
# Chain-ladder and Bornhuetter-Ferguson reserves, Mack standard errors.
# =============================================================================

# ---- 0. Setup --------------------------------------------------------------
# install.packages(c("tidyverse", "ChainLadder"))
library(tidyverse)
suppressPackageStartupMessages(library(ChainLadder))

DATA_PATH <- "wkcomp_pos_98-07.csv"

# ---- 1. Load & clean -------------------------------------------------------
raw <- read_csv(DATA_PATH, show_col_types = FALSE)

# Rename to something readable.
wc <- raw %>%
  rename(
    group_code   = GRCODE,
    group_name   = GRNAME,
    acc_year     = AccidentYear,
    dev_year     = DevelopmentYear,
    dev_lag      = DevelopmentLag,
    incur_loss   = IncurredLosses,
    cum_paid     = CumPaidLoss,
    bulk_loss    = BulkLoss,
    prem_dir     = EarnedPremDIR,
    prem_ceded   = EarnedPremCeded,
    prem_net     = EarnedPremNet,
    single       = Single,
    posted_res   = PostedReserves2007
  )

# ---- 2. Pick a single company ----------------------------------------------
# Federal Insurance Company (Chubb subsidiary). Clean, high-volume book.
TARGET_CODE <- 388

co <- wc %>% filter(group_code == TARGET_CODE)
cat("Analyzing company:", unique(co$group_name), "\n")

# The file gives the full square; the real upper triangle is everything
# reported on or before the latest calendar year.
co <- co %>%
  mutate(cal_year = acc_year + dev_lag - 1)
latest_cal <- 2007

upper <- co %>% filter(cal_year <= latest_cal)

# ---- 3. Build the cumulative paid-loss triangle ----------------------------
tri_long <- upper %>%
  select(acc_year, dev_lag, cum_paid) %>%
  arrange(acc_year, dev_lag)

# Long -> wide: rows are accident years, columns are development lags.
tri <- tri_long %>%
  pivot_wider(names_from = dev_lag, values_from = cum_paid) %>%
  arrange(acc_year)

cat("\nCumulative paid-loss triangle:\n")
print(tri)

# Matrix form for the ChainLadder package.
tri_mat <- tri %>% select(-acc_year) %>% as.matrix()
rownames(tri_mat) <- tri$acc_year
tri_obj <- as.triangle(tri_mat)

# ---- 4. Age-to-age (link) ratios -------------------------------------------
# Two flavors of LDF: volume-weighted (the chain-ladder default) and a plain
# average of the individual ratios. Volume-weighted lets big years dominate.
ata <- function(triangle) {
  n <- ncol(triangle)
  vw  <- numeric(n - 1)
  sa  <- numeric(n - 1)
  for (j in seq_len(n - 1)) {
    num <- triangle[, j + 1]
    den <- triangle[, j]
    ok  <- !is.na(num) & !is.na(den) & den > 0
    vw[j] <- sum(num[ok]) / sum(den[ok])
    sa[j] <- mean(num[ok] / den[ok])
  }
  tibble(
    dev_from = seq_len(n - 1),
    dev_to   = seq_len(n - 1) + 1,
    ldf_vw   = vw,
    ldf_sa   = sa
  )
}

ldfs <- ata(tri_mat)

# Chain the LDFs into cumulative factors to ultimate. No tail factor here.
ldfs <- ldfs %>%
  mutate(
    cdf_vw = rev(cumprod(rev(ldf_vw))),
    cdf_sa = rev(cumprod(rev(ldf_sa)))
  )

cat("\nLink ratios and cumulative development factors:\n")
print(ldfs)

# ---- 5. Project ultimates & reserves (volume-weighted) ---------------------
n <- nrow(tri_mat)
ult_factors <- c(ldfs$cdf_vw, 1)   # final column is already at ultimate

# Latest paid figure on each row's diagonal, and how mature that row is.
latest_diag <- sapply(seq_len(n), function(i) {
  row <- tri_mat[i, ]
  tail(row[!is.na(row)], 1)
})
latest_age <- sapply(seq_len(n), function(i) {
  row <- tri_mat[i, ]
  max(which(!is.na(row)))
})

cdf_at_age <- ult_factors[latest_age]

# Ultimate = latest paid grossed up by its CDF; reserve is the gap.
reserve_tbl <- tibble(
  acc_year      = tri$acc_year,
  latest_paid   = latest_diag,
  age           = latest_age,
  cdf           = cdf_at_age,
  ultimate      = latest_paid * cdf,
  ibnr_reserve  = ultimate - latest_paid
)

cat("\nReserve summary (volume-weighted chain-ladder):\n")
print(reserve_tbl)
cat("\nTotal estimated reserve:",
    formatC(sum(reserve_tbl$ibnr_reserve), format = "d", big.mark = ","), "\n")

# ---- 6. Cross-check with ChainLadder + Mack standard errors ----------------
# Mack wants a square triangle. This book has 9 origin years but 10 dev
# columns, so drop the last column (only the mature 1998 cell, LDF ~ 1).
tri_obj <- as.triangle(tri_mat[, 1:nrow(tri_mat)])
mack <- MackChainLadder(tri_obj, est.sigma = "Mack")
cat("\n--- Mack Chain-Ladder ---\n")
print(summary(mack)$Totals)

# Sanity check: my hand-rolled ultimates vs the package's.
compare <- tibble(
  acc_year      = tri$acc_year,
  ult_manual    = reserve_tbl$ultimate,
  ult_mack      = summary(mack)$ByOrigin$Ultimate
) %>%
  mutate(diff = ult_manual - ult_mack)
cat("\nManual vs Mack ultimates:\n")
print(compare)

# ---- 7. Reserve uncertainty plot -------------------------------------------
plot_df <- summary(mack)$ByOrigin %>%
  as_tibble(rownames = "acc_year") %>%
  mutate(acc_year = as.integer(acc_year))

# IBNR per year with +/- one Mack standard error.
p <- ggplot(plot_df, aes(x = acc_year, y = IBNR)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_errorbar(aes(ymin = IBNR - `Mack.S.E`, ymax = IBNR + `Mack.S.E`),
                width = 0.3) +
  labs(
    title = "IBNR Reserve by Accident Year with Mack Standard Error",
    subtitle = paste("Company:", unique(co$group_name)),
    x = "Accident Year", y = "IBNR Reserve"
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 12)

ggsave("ibnr_by_accident_year.png", p, width = 8, height = 5, dpi = 150)
cat("\nSaved plot: ibnr_by_accident_year.png\n")

# ---- 7b. Bornhuetter-Ferguson method ---------------------------------------
# BF reserve = a priori ultimate * unreported share = ELR * premium * (1 - 1/CDF).
# Mature years are ~fully reported so BF ~ paid; green years lean on the
# a priori instead of grossing up a thin paid figure.

# Net earned premium is repeated down the dev lags, so grab one value per year.
prem_by_year <- co %>%
  group_by(acc_year) %>%
  summarise(premium = first(prem_net), .groups = "drop") %>%
  arrange(acc_year)

# ELR off the mature years (CDF < 1.05), where paid is a clean read on ultimate.
bf_base <- reserve_tbl %>%
  left_join(prem_by_year, by = "acc_year") %>%
  mutate(reported_pct = 1 / cdf)

mature <- bf_base %>% filter(cdf < 1.05)
elr <- sum(mature$latest_paid) / sum(mature$premium)
cat("\nDerived expected loss ratio (ELR) from mature years:",
    round(elr, 4), "\n")

bf <- bf_base %>%
  mutate(
    apriori_ult   = elr * premium,
    bf_ibnr       = apriori_ult * (1 - reported_pct),
    bf_ultimate   = latest_paid + bf_ibnr
  )

cat("\nBornhuetter-Ferguson summary:\n")
print(bf %>% select(acc_year, premium, reported_pct,
                     apriori_ult, latest_paid, bf_ultimate, bf_ibnr))
cat("\nTotal BF reserve:",
    formatC(sum(bf$bf_ibnr), format = "d", big.mark = ","), "\n")

# ---- 7c. Chain-Ladder vs Bornhuetter-Ferguson ------------------------------
comparison <- bf %>%
  transmute(
    acc_year,
    cl_ibnr  = ibnr_reserve,
    bf_ibnr,
    diff     = cl_ibnr - bf_ibnr
  )
cat("\nChain-Ladder vs Bornhuetter-Ferguson reserves:\n")
print(comparison)
cat("\nTotal CL reserve:", formatC(sum(comparison$cl_ibnr), format = "d", big.mark = ","),
    "| Total BF reserve:", formatC(sum(comparison$bf_ibnr), format = "d", big.mark = ","), "\n")

# Dodged bars so the two methods sit side by side per year.
comp_long <- comparison %>%
  select(acc_year, cl_ibnr, bf_ibnr) %>%
  pivot_longer(c(cl_ibnr, bf_ibnr), names_to = "method", values_to = "ibnr") %>%
  mutate(method = recode(method,
                         cl_ibnr = "Chain-Ladder",
                         bf_ibnr = "Bornhuetter-Ferguson"))

p2 <- ggplot(comp_long, aes(x = factor(acc_year), y = ibnr, fill = method)) +
  geom_col(position = position_dodge(width = 0.75), alpha = 0.9) +
  labs(
    title = "IBNR Reserve by Accident Year: Chain-Ladder vs Bornhuetter-Ferguson",
    subtitle = paste("Company:", unique(co$group_name),
                     "| ELR =", scales::percent(elr, accuracy = 0.1)),
    x = "Accident Year", y = "IBNR Reserve", fill = "Method"
  ) +
  scale_y_continuous(labels = scales::comma) +
  scale_fill_manual(values = c("Chain-Ladder" = "steelblue",
                               "Bornhuetter-Ferguson" = "darkorange")) +
  theme_minimal(base_size = 12) +
  theme(legend.position = "top")

ggsave("cl_vs_bf_reserves.png", p2, width = 9, height = 5, dpi = 150)
cat("\nSaved plot: cl_vs_bf_reserves.png\n")

cat("\nDone.\n")

# =============================================================================
# Workers' Compensation Loss Reserving
# Nigel Mpuah
# Data: CAS Loss Reserve Database, Schedule P (wkcomp_pos_98-07.csv)
# Chain-ladder and Bornhuetter-Ferguson reserves, Mack standard errors.
# =============================================================================

# ---- 0. Setup --------------------------------------------------------------
# Restore pinned dependencies with the documented build command.
suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(tibble)
  library(readr)
  library(ggplot2)
})
suppressPackageStartupMessages(library(ChainLadder))

source("R/reserving.R", local = TRUE)
DATA_PATH <- "wkcomp_pos_98-07.csv"
TARGET_CODE <- 388
latest_cal <- 2007

# ---- 1. Load & clean -------------------------------------------------------
raw <- read_csv(DATA_PATH, show_col_types = FALSE)
data_audit <- build_paid_triangle(raw, TARGET_CODE, latest_cal, horizon = 10L)
cat("Observed cells:", data_audit$observed_cells,
    "| Excluded later cells:", data_audit$excluded_future_cells, "\n")
if (nrow(data_audit$negative_increments)) {
  print(data_audit$negative_increments)
  warning("Negative paid increments retained. Investigate source records; do not automatically delete.\n",
          "The CSV does not establish their cause.", call. = FALSE)
}

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

# ---- 2. Pick the insurer group --------------------------------------------
# Code 388 is labelled Federal Ins Co Grp in the source (Single = 0).
co <- wc %>% filter(group_code == TARGET_CODE)
cat("Analyzing company:", unique(co$group_name), "\n")

# ---- 3. Build the cumulative paid-loss triangle ----------------------------
# The helper checks expected observations through the valuation year, orders
# development columns numerically, and excludes all later loss observations.
tri_mat <- data_audit$triangle
tri <- bind_cols(tibble(acc_year = as.integer(rownames(tri_mat))),
                 as_tibble(tri_mat))
cat("\nCumulative paid-loss triangle ($000s):\n")
print(tri)

# ---- 4. Age-to-age (link) ratios -------------------------------------------
# Two flavors of LDF: volume-weighted (the chain-ladder default) and a plain
# average of the individual ratios. Volume-weighted lets big years dominate.
ldfs <- ata(tri_mat)

# Chain the LDFs into cumulative factors to ultimate. No tail factor here.
ldfs <- ldfs %>%
  mutate(
    cdf_vw = rev(cumprod(rev(ldf_vw))),
    cdf_sa = rev(cumprod(rev(ldf_sa)))
  )

cat("\nLink ratios and cumulative development factors:\n")
print(ldfs)

# ---- 5. Project through development age 10 ---------------------------------
# The terminal CDF is one because no further development is assumed. This is
# a modelling horizon, not evidence that all workers' compensation claims settle.
reserve_tbl <- as_tibble(manual_chain_ladder(tri_mat)) %>%
  rename(unpaid_reserve = unpaid)
latest_age <- reserve_tbl$age
cat("\nReserve summary (volume-weighted chain-ladder; $000s):\n")
print(reserve_tbl)
cat("\nTotal estimated unpaid losses:",
    formatC(sum(reserve_tbl$unpaid_reserve), format = "d", big.mark = ","), "\n")

# ---- 6. Cross-check with ChainLadder + Mack standard errors ----------------
# MackChainLadder supports triangles with at least as many origin rows as
# development columns; it does not require every triangle to be square.
# This dataset has 9 rows and 10 columns. Every origin has reached age 2,
# so age 1 is not needed for any remaining projection. Keep ages 2-10:
# this preserves all latest payments and all factors used by the manual CL.
# No tail beyond age 10 is assumed; age 10 is a horizon, not proof of settlement.
mack <- mack_common_basis(tri_mat)
mack_mat <- mack$Triangle
mack_cols <- match(colnames(mack_mat), colnames(tri_mat))
mack_by_year <- summary(mack)$ByOrigin
stopifnot(identical(rownames(mack_by_year), as.character(reserve_tbl$acc_year)))

# Compare latest paid, ultimate, and unpaid losses on the same basis.
# Values are in $000s; this tolerance allows only floating-point differences.
compare <- tibble(
  acc_year      = reserve_tbl$acc_year,
  paid_manual   = reserve_tbl$latest_paid,
  paid_mack     = mack_by_year$Latest,
  ult_manual    = reserve_tbl$ultimate,
  ult_mack      = mack_by_year$Ultimate,
  unpaid_manual = reserve_tbl$unpaid_reserve,
  unpaid_mack   = mack_by_year$IBNR
) %>%
  mutate(diff = ult_manual - ult_mack)
stopifnot(
  max(abs(compare$paid_manual - compare$paid_mack)) < 1e-7,
  max(abs(compare$diff)) < 1e-7,
  max(abs(compare$unpaid_manual - compare$unpaid_mack)) < 1e-7,
  max(abs(mack$f[seq_len(ncol(mack_mat) - 1)] -
            ldfs$ldf_vw[mack_cols[-length(mack_cols)]])) < 1e-10
)
cat("\n--- Mack Chain-Ladder: common basis, through development age 10 ---\n")
print(summary(mack)$Totals)
cat("\nManual vs Mack on the same basis:\n")
print(compare)

# ---- 7. Reserve uncertainty plot -------------------------------------------
dir.create("figures", showWarnings = FALSE)
plot_df <- summary(mack)$ByOrigin %>%
  as_tibble(rownames = "acc_year") %>%
  mutate(acc_year = as.integer(acc_year))

# Paid-loss development estimates unpaid losses, including future payments on
# reported claims. The package calls this IBNR; the chart uses unpaid losses.
p <- ggplot(plot_df, aes(x = factor(acc_year), y = IBNR / 1000)) +
  geom_col(fill = "steelblue", alpha = 0.85) +
  geom_errorbar(aes(ymin = (IBNR - `Mack.S.E`) / 1000,
                    ymax = (IBNR + `Mack.S.E`) / 1000), width = 0.3) +
  labs(
    title = "Estimated unpaid losses by accident year",
    subtitle = paste(unique(co$group_name), "| Paid development through age 10"),
    x = "Accident year", y = "Unpaid losses ($ millions)",
    caption = "Bars: common-basis chain-ladder. Error bars: +/- one Mack standard error, not confidence intervals.\nNo development beyond age 10 is assumed."
  ) +
  scale_y_continuous(labels = scales::comma) +
  theme_minimal(base_size = 12)
ggsave("figures/unpaid_by_accident_year.png", p, width = 10, height = 6, dpi = 150)

# ---- 7b. Bornhuetter-Ferguson sensitivities ---------------------------------
# With paid-loss development, BF unpaid = premium * ELR * (1 - 1/CDF).
# Both ELRs below use this book's experience. Neither is an independent prior.
# Premium consistency and definitions were checked before building the triangle.
prem_by_year <- co %>%
  group_by(acc_year) %>%
  summarise(premium = first(prem_net), .groups = "drop") %>%
  arrange(acc_year)
bf_base <- reserve_tbl %>%
  left_join(prem_by_year, by = "acc_year") %>%
  mutate(paid_pct = 1 / cdf)
stopifnot(all(is.finite(bf_base$premium)), all(is.finite(bf_base$cdf)),
          all(bf_base$cdf > 0))

# The threshold selects 1998-2000 in the verified 2007 triangle. It does not
# establish that those years are fully settled. Retain the original paid/premium
# definition to reproduce the existing sensitivity without changing assumptions.
mature <- bf_base %>% filter(cdf < 1.05)
stopifnot(nrow(mature) > 0)
elr <- sum(mature$latest_paid) / sum(mature$premium)

# Use full precision. For this dataset, 40.027289% rounds to 40.0% for display.
# This is a sensitivity to a CL-derived ELR, not independent validation of CL.
cl_implied_elr <- sum(bf_base$ultimate) / sum(bf_base$premium)
bf <- bf_base %>%
  mutate(
    apriori_ult = elr * premium,
    bf_unpaid_amount = bf_unpaid(premium, elr, cdf),
    bf_ultimate = latest_paid + bf_unpaid_amount,
    cl_implied_expected_ultimate = cl_implied_elr * premium,
    cl_implied_bf_unpaid = bf_unpaid(premium, cl_implied_elr, cdf),
    cl_implied_bf_ultimate = latest_paid + cl_implied_bf_unpaid
  )

# A rounded 40% input is shown separately so displayed precision cannot silently
# change the calculation. BF unpaid scales linearly with ELR on this basis.
unpaid_premium <- sum(bf$premium * (1 - bf$paid_pct))
stopifnot(
  abs(sum(bf$bf_unpaid_amount) - elr * unpaid_premium) < 1e-7,
  abs(sum(bf$cl_implied_bf_unpaid) - cl_implied_elr * unpaid_premium) < 1e-7,
  all(bf$bf_unpaid_amount[bf$cdf == 1] == 0),
  all(bf$cl_implied_bf_unpaid[bf$cdf == 1] == 0)
)
bf_scenarios <- tibble(
  scenario = c("Mature-year paid ELR sensitivity", "CL-implied ELR sensitivity",
               "Rounded CL-implied ELR sensitivity (exactly 40%)"),
  ELR = c(elr, cl_implied_elr, 0.4),
  unpaid_000s = c(sum(bf$bf_unpaid_amount), sum(bf$cl_implied_bf_unpaid),
                  0.4 * unpaid_premium)
)
cat("\nAccident years meeting CDF < 1.05:", mature$acc_year, "\n")
cat("\nBF sensitivities (monetary values in $000s):\n")
print(as.data.frame(bf_scenarios), digits = 12, row.names = FALSE)
cat("\nBF results by accident year ($000s):\n")
print(bf %>% select(acc_year, premium, paid_pct, bf_unpaid_amount,
                    cl_implied_bf_unpaid, cl_implied_bf_ultimate))

# ---- 7c. Method comparison figures -----------------------------------------
comparison <- bf %>%
  transmute(acc_year, cl_unpaid = unpaid_reserve, mature_bf_unpaid = bf_unpaid_amount,
            cl_implied_bf_unpaid)
method_labels <- c(
  cl_unpaid = "Chain-ladder",
  mature_bf_unpaid = "Mature-year paid ELR sensitivity",
  cl_implied_bf_unpaid = "CL-implied ELR sensitivity"
)
comp_long <- comparison %>%
  pivot_longer(-acc_year, names_to = "method", values_to = "unpaid") %>%
  mutate(method = factor(method, levels = names(method_labels), labels = method_labels))
comparison_plot <- function(data, title, subtitle) {
  ggplot(data, aes(x = factor(acc_year), y = unpaid / 1000, fill = method)) +
    geom_col(position = position_dodge(width = 0.8), width = 0.75) +
    labs(title = title, subtitle = subtitle, x = "Accident year",
         y = "Unpaid losses ($ millions)", fill = NULL,
         caption = "BF = premium x ELR x (1 - 1/CDF). Both ELRs use this book's data; neither is an independent prior.\nValuation: year-end 2007. Development through age 10; no further tail.") +
    scale_y_continuous(labels = scales::comma) +
    scale_fill_manual(values = c("Chain-ladder" = "steelblue",
                                "Mature-year paid ELR sensitivity" = "darkorange",
                                "CL-implied ELR sensitivity" = "#238b45")) +
    theme_minimal(base_size = 12) +
    theme(legend.position = "bottom", legend.text = element_text(size = 10)) +
    guides(fill = guide_legend(nrow = 2, byrow = TRUE))
}
p2 <- comparison_plot(
  filter(comp_long, method != "CL-implied ELR sensitivity"),
  "Chain-ladder and BF with a mature-year paid ELR",
  paste(unique(co$group_name), "| Mature-year ELR:",
        scales::percent(elr, accuracy = 0.01))
)
p3 <- comparison_plot(
  comp_long, "Chain-ladder and Bornhuetter-Ferguson sensitivities",
  paste("Mature-year ELR:", scales::percent(elr, accuracy = 0.01),
        "| CL-implied ELR:", scales::percent(cl_implied_elr, accuracy = 0.01))
)
ggsave("figures/cl_vs_bf_reserves.png", p2, width = 11, height = 6.5, dpi = 150)
ggsave("figures/cl_vs_bf_sensitivity.png", p3, width = 11, height = 6.5, dpi = 150)
cat("\nSaved three PNG charts under figures/.\n")
cat("\nDone.\n")

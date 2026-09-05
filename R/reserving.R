# Shared calculation and data checks. No files are read or written here.

build_paid_triangle <- function(data, company_code, valuation_year, horizon = 10L) {
  required <- c("GRCODE", "GRNAME", "AccidentYear", "DevelopmentYear",
                "DevelopmentLag", "CumPaidLoss", "EarnedPremDIR",
                "EarnedPremCeded", "EarnedPremNet")
  missing <- setdiff(required, names(data))
  if (length(missing)) stop("Missing required fields: ", paste(missing, collapse = ", "))
  data <- as.data.frame(data)
  numeric_fields <- setdiff(required, "GRNAME")
  for (field in numeric_fields) {
    if (!is.numeric(data[[field]]) || any(!is.finite(data[[field]])))
      stop("Non-numeric or missing values in ", field)
  }
  for (field in c("GRCODE", "AccidentYear", "DevelopmentYear", "DevelopmentLag")) {
    if (any(data[[field]] != floor(data[[field]]))) stop("Non-integer values in ", field)
  }
  if (anyDuplicated(data[c("GRCODE", "AccidentYear", "DevelopmentLag")]))
    stop("Duplicate company/accident-year/development-lag records; investigate before fitting.")
  if (any(data$DevelopmentYear != data$AccidentYear + data$DevelopmentLag - 1))
    stop("DevelopmentYear does not match accident year plus development lag minus one.")
  co <- data[data$GRCODE == company_code, , drop = FALSE]
  if (!nrow(co)) stop("Company code has no records.")
  if (length(unique(co$GRNAME)) != 1L || anyNA(co$GRNAME) || any(!nzchar(co$GRNAME)))
    stop("Missing or inconsistent company name.")
  if (any(co$DevelopmentLag < 1 | co$DevelopmentLag > horizon))
    stop("Development lag outside the specified horizon.")
  premium_fields <- c("EarnedPremDIR", "EarnedPremCeded", "EarnedPremNet")
  for (year in unique(co$AccidentYear)) {
    for (field in premium_fields) {
      if (length(unique(co[co$AccidentYear == year, field])) != 1L)
        stop("Premium varies across development lags for accident year ", year, ": ", field)
    }
  }
  if (any(abs(co$EarnedPremDIR - co$EarnedPremCeded - co$EarnedPremNet) > 1e-8))
    stop("Direct and assumed premium minus ceded premium does not equal net premium.")
  if (any(co$EarnedPremNet <= 0)) stop("Non-positive net premium; investigate before deriving ELRs.")
  observed <- co[co$DevelopmentYear <= valuation_year, , drop = FALSE]
  if (!nrow(observed)) stop("No observations at the valuation date.")
  years <- seq.int(min(observed$AccidentYear), max(observed$AccidentYear))
  expected <- expand.grid(AccidentYear = years, DevelopmentLag = seq_len(horizon))
  expected <- expected[expected$AccidentYear + expected$DevelopmentLag - 1 <= valuation_year, ]
  key <- function(d) paste(d$AccidentYear, d$DevelopmentLag, sep = "/")
  missing <- setdiff(key(expected), key(observed))
  if (length(missing)) stop("Missing observed cells (year/lag): ", paste(missing, collapse = ", "))
  tri <- matrix(NA_real_, length(years), horizon, dimnames = list(years, seq_len(horizon)))
  tri[cbind(match(observed$AccidentYear, years), observed$DevelopmentLag)] <- observed$CumPaidLoss
  observed <- observed[order(observed$AccidentYear, observed$DevelopmentLag), ]
  observed$increment <- ave(observed$CumPaidLoss, observed$AccidentYear,
                            FUN = function(x) c(x[1], diff(x)))
  decreases <- observed[observed$DevelopmentLag > 1 & observed$increment < 0,
                        c("AccidentYear", "DevelopmentYear", "DevelopmentLag", "increment")]
  list(triangle = tri, observed = observed, negative_increments = decreases,
       observed_cells = nrow(observed), excluded_future_cells = nrow(co) - nrow(observed))
}

check_triangle <- function(triangle) {
  if (!is.matrix(triangle) || !is.numeric(triangle) || nrow(triangle) < 1 || ncol(triangle) < 2)
    stop("Expected a numeric matrix with at least two development columns.")
  if (any(!is.finite(triangle[!is.na(triangle)]))) stop("Non-finite triangle values.")
  if (any(triangle < 0, na.rm = TRUE)) stop("Negative cumulative paid losses; investigate before fitting.")
  ages <- suppressWarnings(as.integer(colnames(triangle)))
  if (length(ages) != ncol(triangle) || anyNA(ages) || any(diff(ages) != 1L))
    stop("Development columns must be consecutive and in numeric order.")
  for (i in seq_len(nrow(triangle))) {
    present <- unname(which(!is.na(triangle[i, ])))
    if (!length(present) || !identical(present, seq_len(max(present))))
      stop("Missing observed cells within triangle row ", i)
  }
  invisible(TRUE)
}

ata <- function(triangle) {
  check_triangle(triangle)
  n <- ncol(triangle)
  vw <- sa <- numeric(n - 1L)
  for (j in seq_len(n - 1L)) {
    den <- triangle[, j]; num <- triangle[, j + 1L]
    paired <- !is.na(den) & !is.na(num)
    if (!any(paired)) stop("No observed pairs for development column ", colnames(triangle)[j])
    if (any(den[paired] <= 0))
      stop("Non-positive development denominator at column ", colnames(triangle)[j],
           "; investigate rather than silently exclude records.")
    vw[j] <- sum(num[paired]) / sum(den[paired])
    sa[j] <- mean(num[paired] / den[paired])
  }
  data.frame(dev_from = as.integer(colnames(triangle)[-n]),
             dev_to = as.integer(colnames(triangle)[-1]), ldf_vw = vw, ldf_sa = sa)
}

manual_chain_ladder <- function(triangle) {
  factors <- ata(triangle)
  cdf <- c(rev(cumprod(rev(factors$ldf_vw))), 1)
  latest_col <- apply(triangle, 1, function(x) max(which(!is.na(x))))
  latest <- triangle[cbind(seq_len(nrow(triangle)), latest_col)]
  data.frame(acc_year = as.integer(rownames(triangle)), latest_paid = latest,
             age = as.integer(colnames(triangle))[latest_col], cdf = cdf[latest_col],
             ultimate = latest * cdf[latest_col],
             unpaid = latest * (cdf[latest_col] - 1), row.names = NULL)
}

mack_common_basis <- function(triangle) {
  check_triangle(triangle)
  earliest_latest <- min(apply(triangle, 1, function(x) max(which(!is.na(x)))))
  cols <- seq.int(earliest_latest, ncol(triangle))
  common <- triangle[, cols, drop = FALSE]
  if (ncol(common) < 3L || nrow(common) < ncol(common))
    stop("Triangle shape is not supported by this project's Mack comparison.")
  # For this annual-cohort project, require the standard observed triangle.
  expected <- col(common) <= pmin(ncol(common), nrow(common) + 1L - row(common))
  if (!identical(unname(!is.na(common)), unname(expected)))
    stop("Mack comparison needs a regular annual observed triangle.")
  ChainLadder::MackChainLadder(ChainLadder::as.triangle(common), alpha = 1,
                             est.sigma = "Mack", tail = FALSE)
}

bf_unpaid <- function(premium, elr, cdf) {
  if (length(elr) != 1L || !is.finite(elr) || elr < 0 ||
      length(premium) != length(cdf) || !length(premium) ||
      any(!is.finite(premium)) || any(premium <= 0) ||
      any(!is.finite(cdf)) || any(cdf <= 0)) stop("Invalid BF inputs.")
  premium * elr * (1 - 1 / cdf)
}

# Run from the repository root: Rscript tests/run_tests.R
# Synthetic data only. No CAS data or extra testing package is required.
source("R/reserving.R", local = TRUE)
passed <- 0L
check <- function(name, expr) {
  force(expr)
  passed <<- passed + 1L
  cat("PASS:", name, "\n")
}
equal <- function(actual, expected, tolerance = 1e-8) {
  stopifnot(isTRUE(all.equal(as.numeric(actual), as.numeric(expected), tolerance = tolerance)))
}
fails <- function(expr, message) {
  result <- tryCatch({ force(expr); NULL }, error = function(e) conditionMessage(e))
  if (is.null(result) || !grepl(message, result, fixed = TRUE))
    stop("Expected error containing: ", message, "; received: ", result)
}
tri <- matrix(c(100,150,180,198,
                120,180,220,NA,
                140,216,NA,NA,
                160,NA,NA,NA), nrow = 4, byrow = TRUE,
              dimnames = list(2000:2003, 1:4))
manual <- manual_chain_ladder(tri)
check("manual factors match hand calculations", {
  equal(ata(tri)$ldf_vw, c(91/60, 40/33, 11/10))
})
check("manual ultimates and unpaid match hand calculations", {
  equal(manual$ultimate, c(198, 242, 288, 2912/9))
  equal(manual$unpaid, c(0, 22, 72, 1472/9))
  equal(sum(manual$unpaid), 2318/9)
})
check("manual and package agree by origin on a common basis", {
  package <- summary(mack_common_basis(tri))$ByOrigin
  equal(package$Latest, manual$latest_paid)
  equal(package$Ultimate, manual$ultimate)
  equal(package$IBNR, manual$unpaid)
})
wide <- cbind(c(50,60,70,80), tri)
colnames(wide) <- 1:5
check("wide triangle keeps final development and all latest payments", {
  package <- summary(mack_common_basis(wide))$ByOrigin
  equal(package$Ultimate, manual$ultimate)
  equal(package$Latest, manual$latest_paid)
  equal(package$IBNR, manual_chain_ladder(wide)$unpaid)
})
check("dropping final development changes the answer", {
  old <- manual_chain_ladder(wide[,1:4])
  stopifnot(abs(sum(old$unpaid) - sum(manual$unpaid)) > 1)
  equal(old$latest_paid[1], 180)
  equal(manual$latest_paid[1], 198)
})
check("zero denominator fails instead of dropping the observation", {
  bad <- tri; bad[1,1] <- 0
  fails(ata(bad), "Non-positive development denominator")
})
check("negative paid increments remain in factor estimation", {
  decrease <- tri; decrease[2,3] <- 170
  equal(ata(decrease)$ldf_vw[2], 35/33)
})
check("an internal missing cell fails", {
  bad <- tri; bad[1,2] <- NA
  fails(ata(bad), "Missing observed cells")
})
check("unordered development columns fail", {
  fails(ata(tri[,c(1,3,2,4)]), "numeric order")
})
check("BF matches hand calculations and preserves ELR precision", {
  equal(bf_unpaid(c(1000,1000), 0.6, c(1,2)), c(0,300))
  equal(bf_unpaid(c(1000,1000), 0.4, c(1,2)), c(0,200))
  equal(bf_unpaid(1000, 0.40027, 2), 200.135)
  fails(bf_unpaid(c(1000,2000), 0.4, 2), "Invalid BF inputs")
})
# Ten development ages test numeric ordering (1,2,...,10), including shuffled
# source rows. Future cells contain sentinels and must not enter the triangle.
records <- expand.grid(AccidentYear = 2000:2003, DevelopmentLag = 1:10)
records$DevelopmentYear <- with(records, AccidentYear + DevelopmentLag - 1)
records$GRCODE <- 1L; records$GRNAME <- "Synthetic insurer"
records$CumPaidLoss <- records$DevelopmentLag * 100
records$EarnedPremDIR <- 1100; records$EarnedPremCeded <- 100
records$EarnedPremNet <- 1000
records$CumPaidLoss[records$DevelopmentYear > 2009] <- 999999
check("shuffled records produce numeric columns and exclude future data", {
  a <- build_paid_triangle(records, 1, 2009)
  b <- build_paid_triangle(records[nrow(records):1, ], 1, 2009)
  stopifnot(identical(a$triangle, b$triangle),
            identical(colnames(a$triangle), as.character(1:10)),
            a$observed_cells == 34L, a$excluded_future_cells == 6L,
            !any(a$triangle == 999999, na.rm = TRUE))
})
check("duplicate company/year/lag keys fail", {
  fails(build_paid_triangle(rbind(records, records[1, ]), 1, 2009), "Duplicate")
})
check("missing observed records fail but absent future records are allowed", {
  fails(build_paid_triangle(records[-1, ], 1, 2009), "Missing observed cells")
  observed <- records[records$DevelopmentYear <= 2009, ]
  equal(build_paid_triangle(observed, 1, 2009)$observed_cells, 34)
})
check("calendar-year mismatch and non-numeric input fail", {
  bad <- records; bad$DevelopmentYear[1] <- 2010
  fails(build_paid_triangle(bad, 1, 2009), "DevelopmentYear does not match")
  bad <- records; bad$CumPaidLoss <- as.character(bad$CumPaidLoss)
  fails(build_paid_triangle(bad, 1, 2009), "Non-numeric")
})
check("premium consistency and net definitions are checked", {
  bad <- records; bad$EarnedPremNet[1] <- 999
  fails(build_paid_triangle(bad, 1, 2009), "Premium varies")
  bad <- records; bad$EarnedPremNet <- 999
  fails(build_paid_triangle(bad, 1, 2009), "does not equal net premium")
})
check("paid decreases are returned for investigation without changing data", {
  bad <- records
  index <- which(bad$AccidentYear == 2000 & bad$DevelopmentLag == 3)
  bad$CumPaidLoss[index] <- 150
  result <- build_paid_triangle(bad, 1, 2009)
  equal(result$negative_increments$increment, -50)
  equal(result$triangle["2000", "3"], 150)
})
cat("\n", passed, " test groups passed. No runoff validation is performed by these tests.\n", sep = "")

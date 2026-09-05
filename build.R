# From the repository root: Rscript build.R
# In the RStudio Console: source("build.R")
build_project <- function() {
  if (!file.exists("renv.lock") || !file.exists("report/report.Rmd"))
    stop("Run this command from the repository root.")
  if (!requireNamespace("renv", quietly = TRUE)) source("renv/activate.R")
  renv::load(project = getwd())
  renv::restore(prompt = FALSE)
  lock <- renv::lockfile_read("renv.lock")
  if (as.character(getRversion()) != lock$R$Version)
    stop("Use R ", lock$R$Version, " for the recorded environment. renv does not install R.")
  # macOS desktop apps may not inherit the shell's Homebrew / TeX paths.
  # Extend this build process only; preserve existing executable precedence.
  if (identical(Sys.info()[["sysname"]], "Darwin")) {
    tool_dirs <- c("/opt/homebrew/bin", "/usr/local/bin", "/Library/TeX/texbin")
    current_path <- strsplit(Sys.getenv("PATH"), .Platform$path.sep, fixed = TRUE)[[1]]
    Sys.setenv(PATH = paste(unique(c(current_path, tool_dirs[dir.exists(tool_dirs)])),
                           collapse = .Platform$path.sep))
  }
  rmarkdown::find_pandoc(cache = FALSE)
  if (!rmarkdown::pandoc_available()) stop("Pandoc is required; install RStudio or Pandoc.")
  if (!nzchar(Sys.which("pdflatex")))
    stop("A LaTeX distribution with pdflatex is required. See DATA_AND_BUILD.md.")
  # Fix PDF metadata timestamps so the same inputs produce the same PDF bytes.
  Sys.setenv(SOURCE_DATE_EPOCH = "1199059200")
  if (!file.exists("wkcomp_pos_98-07.csv"))
    stop("Missing CAS CSV. Follow DATA_AND_BUILD.md; the dataset is not bundled.")
  input_sha256 <- digest::digest(file = "wkcomp_pos_98-07.csv", algo = "sha256")
  reference_sha256 <- "8d0b02bed0939e932f9078f65266e9a398f580f90e5227cde053dd5b520affef"
  if (input_sha256 != reference_sha256)
    stop("The CSV differs from the verified CAS release. Inspect the difference before rebuilding this report.")
  dir.create("outputs/tables", recursive = TRUE, showWarnings = FALSE)
  dir.create("outputs/report", showWarnings = FALSE)
  # Separate environments prevent synthetic test objects from replacing analysis data.
  test_env <- new.env(parent = globalenv())
  source("tests/run_tests.R", local = test_env)
  test_groups <- test_env$passed
  analysis <- new.env(parent = globalenv())
  log_connection <- file("outputs/analysis.log", open = "wt")
  sink(log_connection)
  tryCatch(source("wc_loss_reserving.R", local = analysis),
           finally = { sink(); close(log_connection) })
  # Complete reconciliation against the original age-9 Mack calculation.
  with(analysis, {
    legacy <- MackChainLadder(as.triangle(tri_mat[, 1:9]), alpha = 1,
                             est.sigma = "Mack", tail = FALSE)
    legacy_by_year <- summary(legacy)$ByOrigin
    legacy_manual <- manual_chain_ladder(tri_mat[, 1:9])
    stopifnot(max(abs(legacy_manual$ultimate - legacy_by_year$Ultimate)) < 1e-7)
    reserve_gap <- sum(reserve_tbl$unpaid_reserve) - sum(legacy_by_year$IBNR)
    ultimate_gap <- sum(reserve_tbl$ultimate) - sum(legacy_by_year$Ultimate)
    paid_gap <- sum(reserve_tbl$latest_paid) - sum(legacy_by_year$Latest)
    stopifnot(abs(reserve_gap - (ultimate_gap - paid_gap)) < 1e-7)
    basis_table <- data.frame(
      Basis = c("Original Mack: ages 1-9", "Manual CL: ages 1-10", "Mack: ages 2-10"),
      Latest_000s = c(sum(legacy_by_year$Latest), sum(reserve_tbl$latest_paid), sum(mack_by_year$Latest)),
      Projected_000s = c(sum(legacy_by_year$Ultimate), sum(reserve_tbl$ultimate), sum(mack_by_year$Ultimate)),
      Unpaid_000s = c(sum(legacy_by_year$IBNR), sum(reserve_tbl$unpaid_reserve), sum(mack_by_year$IBNR))
    )
    totals <- summary(mack)$Totals
    cl_total <- sum(reserve_tbl$unpaid_reserve)
    mack_se <- unname(totals["Mack S.E.:", 1])
    mack_cv <- unname(totals["CV(IBNR):", 1])
    headline <- data.frame(
      Method = c("Chain-ladder / Mack (common basis)", "BF: mature-year paid ELR sensitivity",
                 "BF: CL-implied ELR sensitivity"),
      Unpaid_000s = c(cl_total, sum(bf$bf_unpaid_amount), sum(bf$cl_implied_bf_unpaid))
    )
    by_year <- data.frame(Accident_year = reserve_tbl$acc_year, Age = reserve_tbl$age,
                         CDF = reserve_tbl$cdf, Projected_000s = reserve_tbl$ultimate,
                         Unpaid_000s = reserve_tbl$unpaid_reserve,
                         Mack_SE_000s = mack_by_year$Mack.S.E)
    reconciliation <- data.frame(Accident_year = reserve_tbl$acc_year,
      Latest_difference_000s = reserve_tbl$latest_paid - legacy_by_year$Latest,
      Projected_difference_000s = reserve_tbl$ultimate - legacy_by_year$Ultimate,
      Unpaid_difference_000s = reserve_tbl$unpaid_reserve - legacy_by_year$IBNR)
    audit_summary <- data.frame(
      Check = c("Input records", "Company records", "Observed cells through 2007",
                "Excluded later cells", "Duplicate company/year/lag records",
                "Missing required observed cells", "Zero paired denominators",
                "Negative observed paid increments", "Mature years: CDF < 1.05"),
      Result = c(nrow(raw), nrow(co), data_audit$observed_cells,
                 data_audit$excluded_future_cells, 0, 0, 0,
                 nrow(data_audit$negative_increments), paste(mature$acc_year, collapse = ", "))
    )
    table_objects <- list(headline = headline, chain_ladder_by_year = by_year,
                         development_factors = ldfs, bf_by_year = bf,
                         bf_scenarios = bf_scenarios, basis_reconciliation = basis_table,
                         reconciliation_by_year = reconciliation, data_checks = audit_summary,
                         negative_increments = data_audit$negative_increments,
                         manual_package_agreement = compare)
    for (name in names(table_objects))
      write.csv(table_objects[[name]], file.path("outputs/tables", paste0(name, ".csv")), row.names = FALSE)
  })
  analysis$input_sha256 <- input_sha256
  analysis$test_groups <- test_groups
  analysis$fmt <- function(x, digits = 3) formatC(x, format = "f", digits = digits, big.mark = ",")
  analysis$millions <- function(x) analysis$fmt(x / 1000)
  analysis$percent <- function(x, digits = 2) paste0(analysis$fmt(x * 100, digits), "%")
  # Both documents consume the same objects from this run, without typed-in results.
  knitr::knit("report/README.Rmd", output = "outputs/README.md", envir = analysis, quiet = TRUE)
  rmarkdown::render("report/report.Rmd", output_file = "WC_Loss_Reserving_Report.pdf",
                    output_dir = normalizePath("outputs/report"),
                    intermediates_dir = normalizePath("outputs/report"),
                    knit_root_dir = getwd(), envir = analysis, quiet = TRUE)
  stopifnot(file.copy("outputs/README.md", "README.md", overwrite = TRUE),
            file.copy("outputs/report/WC_Loss_Reserving_Report.pdf",
                      "WC_Loss_Reserving_Report.pdf", overwrite = TRUE))
  writeLines(c(paste("Input SHA-256:", input_sha256),
               paste("R:", getRversion()), paste("Pandoc:", rmarkdown::pandoc_version()),
               paste("LaTeX:", system2(Sys.which("pdflatex"), "--version", stdout = TRUE)[1]),
               paste("Synthetic test groups passed:", test_groups)), "outputs/build-info.txt")
  writeLines(capture.output(sessionInfo()), "outputs/session-info.txt")
  message("Build complete: tests, tables, three figures, README.md, and WC_Loss_Reserving_Report.pdf.")
  invisible(analysis)
}

# Use a fresh R process from the Console so already-loaded packages cannot mask
# the versions restored by renv. Command-line execution builds directly.
if (interactive()) {
  message("Building in a fresh R process. Output will appear when it finishes.")
  output <- suppressWarnings(system2(file.path(R.home("bin"), "Rscript"), "build.R",
                                     stdout = TRUE, stderr = TRUE))
  cat(output, sep = "\n")
  dir.create("outputs", showWarnings = FALSE)
  writeLines(output, "outputs/build-console.log")
  status <- attr(output, "status")
  if (!is.null(status) && status != 0L)
    stop("Build failed. The full error is printed above and saved in outputs/build-console.log.",
         call. = FALSE)
} else {
  build_project()
}

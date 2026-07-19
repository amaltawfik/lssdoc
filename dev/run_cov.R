# Local test-coverage runner for lssdoc.
#
# Measures line coverage over the test suite with covr and writes a
# human-readable summary. Intended as a quick local check before pushing;
# the authoritative number is produced by the Codecov GitHub Action.
#
# Run from the package root:
#   Rscript dev/run_cov.R
#
# Outputs (git-ignored, regenerated on each run):
#   dev/_cov.rds          covr coverage object (input to dev/cov_gaps.R)
#   dev/_cov_results.txt  total + per-file coverage percentages
#
# NOT_CRAN = "true" enables tests skipped on CRAN; covr.record_tests is off
# because per-test records are not needed for the summary.

Sys.setenv(NOT_CRAN = "true")
options(covr.record_tests = FALSE)
cov <- covr::package_coverage(type = "tests", quiet = TRUE)
saveRDS(cov, "dev/_cov.rds")
pc <- covr::coverage_to_list(cov)
con <- file("dev/_cov_results.txt", "w")
writeLines(sprintf("TOTAL %.2f", pc$totalcoverage), con)
fc <- sort(pc$filecoverage)
for (n in names(fc)) writeLines(sprintf("%6.2f  %s", fc[[n]], n), con)
close(con)
cat("DONE\n")

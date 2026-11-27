# A second wave of targeted tests: the audit printer's two terminal
# branches, the output-extension guard, and the audit-marker /
# URL-splitting helpers. (The audit detectors that only fire on
# malformed surveys are covered by test-audit-fixture.R, which audits
# the bundled audit_demo.lss.) These lift render_audit_section.R,
# render_cover.R and render_documentation.R toward full coverage.

# ---- print.lss_audit: the clean and the capped/overflow branches ---

fake_audit <- function(findings_df) {
  structure(
    list(
      file = "fake.lss",
      languages = c("en", "fr"),
      n_findings = nrow(findings_df),
      n_errors = sum(findings_df$severity == "error"),
      n_warnings = sum(findings_df$severity == "warning"),
      n_notes = sum(findings_df$severity == "note"),
      findings = findings_df
    ),
    class = "lss_audit"
  )
}

test_that("print.lss_audit reports a clean survey", {
  empty <- data.frame(severity = character(), check = character(),
                      location = character(), language = character(),
                      message = character(), stringsAsFactors = FALSE)
  out <- cli::cli_fmt(print(fake_audit(empty)))
  expect_true(any(grepl("No anomalies", out)))
})

test_that("print.lss_audit caps the list and summarizes the remainder", {
  df <- data.frame(
    severity = c("error", "warning", "note"),
    check    = c("c1", "c2", "c3"),
    location = c("Question 'Q1'", "Question 'Q2'", "Question 'Q3'"),
    language = c(NA, "fr", NA),
    message  = c("m1", "m2", "m3"),
    stringsAsFactors = FALSE
  )
  out <- cli::cli_fmt(print(fake_audit(df), n = 1L))
  expect_true(any(grepl("finding", out)))
  expect_true(any(grepl("more|as.data.frame", out)))
})

# ---- Output-extension guard ----------------------------------------

test_that("render_documentation rejects an output path that is neither .docx nor .pdf", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  expect_error(
    render_documentation(path, tempfile(fileext = ".txt")),
    class = "lssdoc_bad_output_ext"
  )
})

# ---- URL splitter on an empty fragment ------------------------------

test_that("lss_split_text_urls returns no chunks for an empty fragment", {
  props <- officer::fp_text()
  expect_identical(lss_split_text_urls("", props, props), list())
})

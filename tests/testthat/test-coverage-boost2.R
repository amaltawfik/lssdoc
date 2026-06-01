# A second wave of targeted tests, aimed at the audit detectors that
# only fire on malformed surveys, the audit printer's two terminal
# branches, the output-extension guard, and the audit-marker /
# URL-splitting helpers. These lift audit_lss.R, render_audit_section.R,
# render_cover.R and render_questionnaire.R toward full coverage.

# ---- Audit detectors that require a broken survey ------------------

test_that("audit_lss flags orphan references and missing required parts", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  skip_if(is.null(lss$answers) || nrow(lss$answers) == 0, "no answers in fixture")
  skip_if(is.null(lss$subquestions) || nrow(lss$subquestions) == 0,
          "no subquestions in fixture")

  b <- lss
  # Point one answer and one subquestion at a question id that does not
  # exist -> orphan_answer / orphan_subquestion.
  b$answers$qid[1] <- "999999"
  b$subquestions$parent_qid[1] <- "999999"
  # Strip every answer of one question that has answers -> missing_options.
  qa <- setdiff(unique(lss$answers$qid), "999999")[1]
  b$answers <- b$answers[b$answers$qid != qa, , drop = FALSE]

  au <- audit_lss(b)
  checks <- au$findings$check
  expect_true("orphan_answer" %in% checks)
  expect_true("orphan_subquestion" %in% checks)
  expect_true("missing_options" %in% checks)
})

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

test_that("render_questionnaire rejects an output path that is neither .docx nor .pdf", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  expect_error(
    render_questionnaire(path, tempfile(fileext = ".txt")),
    class = "lssdoc_bad_output_ext"
  )
})

# ---- Audit index / marker over subquestion and answer locations ----

test_that("lss_audit_index resolves subquestion and answer item codes, and the marker guards NULL", {
  findings <- data.frame(
    severity = c("error", "warning", "note", "note"),
    check    = c("a", "b", "c", "d"),
    location = c("Question 'Q1'", "Subquestion 'P1 / SQ01'",
                 "Answer 'Q2 = 1'", "Survey"),
    language = c(NA, NA, NA, NA),
    message  = c("m1", "m2", "m3", "m4"),
    stringsAsFactors = FALSE
  )
  idx <- lss_audit_index(list(findings = findings))
  expect_true("Q1" %in% names(idx$by_code))
  expect_true("P1_SQ01" %in% names(idx$by_code))
  expect_true("Q2" %in% names(idx$by_code))

  theme <- lss_render_theme()
  expect_null(lss_audit_marker(NULL, idx, theme))
  m <- lss_audit_marker("Q1", idx, theme)
  expect_true(grepl("audit finding", m$text))
})

# ---- URL splitter on an empty fragment ------------------------------

test_that("lss_split_text_urls returns no chunks for an empty fragment", {
  props <- officer::fp_text()
  expect_identical(lss_split_text_urls("", props, props), list())
})

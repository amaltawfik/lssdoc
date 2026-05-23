test_that("render_lss_audit_docx rejects bad inputs", {
  expect_error(
    render_lss_audit_docx(list(), tempfile(fileext = ".docx")),
    class = "lssdoc_bad_lss"
  )
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- parse_lss(path)
  expect_error(render_lss_audit_docx(lss, 123), class = "lssdoc_bad_output")
})

test_that("render_lss_audit_docx writes a focused audit document", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)

  res <- render_lss_audit_docx(parse_lss(path), out)
  expect_identical(res, out)
  expect_true(file.exists(out))

  s <- officer::docx_summary(officer::read_docx(out))
  txt <- s$text[!is.na(s$text)]
  # Contains the audit headline and at least one severity-specific subsection.
  expect_true(any(grepl("Audit findings", txt)))
  expect_true(any(grepl("Errors", txt)))
  # The full review's group headings are NOT present.
  group_h1 <- sum(
    s$content_type == "paragraph" &
      s$style_name == "heading 1" &
      grepl("^Audit|^Errors|^Warnings|^Notes", s$text)
  )
  expect_true(group_h1 >= 1)
})

test_that("render_lss_audit_docx on a clean survey says 'no anomalies'", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_lss_audit_docx(parse_lss(path), out)
  s <- officer::docx_summary(officer::read_docx(out))
  expect_true(any(grepl("No anomalies detected", s$text)))
})

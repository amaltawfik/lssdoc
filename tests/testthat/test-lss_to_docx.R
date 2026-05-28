test_that("lss_to_docx parses then renders to .docx", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  res <- lss_to_docx(path, out)
  expect_identical(res, out)
  expect_true(file.size(out) > 10000)
})

test_that("lss_audit_to_docx writes an audit-only document", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  # Force English chrome so the assertion can match the exact
  # heading text; the default chrome_lang follows the survey's
  # primary language and would translate it.
  res <- lss_audit_to_docx(path, out, chrome_lang = "en")
  expect_identical(res, out)
  s <- officer::docx_summary(officer::read_docx(out))
  expect_true(any(grepl("Audit findings", s$text)))
})

test_that("lss_docx_to_pdf reports a clear error when soffice is unavailable", {
  if (!is.null(lssdoc:::lss_find_soffice())) {
    skip("soffice is installed; this path is exercised in pipeline use")
  }
  expect_error(
    lss_docx_to_pdf(tempfile(fileext = ".docx"), tempfile(fileext = ".pdf")),
    class = "lssdoc_bad_input"
  )
})

test_that("lss_to_pdf and lss_audit_to_pdf produce PDFs when soffice is present", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  if (is.null(lssdoc:::lss_find_soffice())) {
    skip("LibreOffice not installed in this environment")
  }
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))

  pdf_full <- tempfile(fileext = ".pdf")
  pdf_aud  <- tempfile(fileext = ".pdf")
  on.exit({ unlink(pdf_full); unlink(pdf_aud) }, add = TRUE)

  lss_to_pdf(path, pdf_full)
  expect_true(file.exists(pdf_full))
  expect_true(file.size(pdf_full) > 50000)

  lss_audit_to_pdf(path, pdf_aud)
  expect_true(file.exists(pdf_aud))
})

test_that("render_lss_docx rejects objects that are not lss", {
  expect_error(
    render_lss_docx(list(), tempfile(fileext = ".docx")),
    class = "lssdoc_bad_lss"
  )
})

test_that("render_lss_docx validates its output argument", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- parse_lss(path)
  expect_error(render_lss_docx(lss, 123), class = "lssdoc_bad_output")
  expect_error(render_lss_docx(lss, c("a", "b")), class = "lssdoc_bad_output")
})

test_that("render_lss_docx produces a non-empty .docx for hesav (2 langs)", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)

  res <- render_lss_docx(parse_lss(path), out)
  expect_identical(res, out)
  expect_true(file.exists(out))
  expect_true(file.size(out) > 10000)

  # Inspect the produced document.
  s <- officer::docx_summary(officer::read_docx(out))
  h2 <- sum(s$content_type == "paragraph" & s$style_name == "heading 2", na.rm = TRUE)
  expect_identical(h2, 31L) # 31 main questions in hesav
  expect_true(sum(s$content_type == "table cell") > 100L)
})

test_that("render_lss_docx with show_audit = FALSE drops the audit section", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)

  render_lss_docx(parse_lss(path), out, show_audit = FALSE)
  s <- officer::docx_summary(officer::read_docx(out))
  # No "Audit findings" heading is present in the document.
  txt <- s$text[!is.na(s$text)]
  expect_false(any(grepl("Audit findings", txt)))
})

test_that("render_lss_docx errors with the right class when officer/flextable are missing", {
  skip("requires the suggested packages to be intentionally unavailable")
})

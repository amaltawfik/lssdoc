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
  # Groups now use Heading 1 (one per group, in the TOC); items use a
  # styled paragraph with our manual sequential number. We verify the
  # group H1 count plus the item expansion via the total number of table
  # cells produced (each item has its own meta table; arrays add a
  # shared answer scale on top).
  h1 <- sum(
    !is.na(s$style_name) & s$style_name == "heading 1", na.rm = TRUE
  )
  expect_gt(h1, 0L)
  expect_true(sum(s$content_type == "table cell") > 200L)
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

test_that("the cover page carries the new Survey ID and Last modified fields", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_lss_docx(parse_lss(path), out)
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Survey ID", txt))
  expect_true(grepl("Last modified", txt))
  expect_true(grepl("971193", txt))
})

test_that("render_lss_docx validates the logo argument", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- parse_lss(path)
  expect_error(
    render_lss_docx(lss, tempfile(fileext = ".docx"), logo = c("a", "b")),
    class = "lssdoc_bad_logo"
  )
  expect_error(
    render_lss_docx(lss, tempfile(fileext = ".docx"), logo = "nope.png"),
    class = "lssdoc_logo_not_found"
  )
  bad <- tempfile(fileext = ".gif")
  writeLines("x", bad)
  on.exit(unlink(bad), add = TRUE)
  expect_error(
    render_lss_docx(lss, tempfile(fileext = ".docx"), logo = bad),
    class = "lssdoc_bad_logo_format"
  )
})

test_that("a valid logo is embedded in the rendered document", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  logo <- tempfile(fileext = ".png")
  on.exit(unlink(logo), add = TRUE)
  grDevices::png(logo, width = 200, height = 100, bg = "white")
  graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
  graphics::text(0.5, 0.5, "L", cex = 6)
  grDevices::dev.off()

  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  size_no_logo <- file.size({
    f <- tempfile(fileext = ".docx")
    render_lss_docx(parse_lss(path), f)
    f
  })
  render_lss_docx(parse_lss(path), out, logo = logo)
  expect_true(file.size(out) > size_no_logo)
})

test_that("render_lss_docx errors with the right class when officer/flextable are missing", {
  skip("requires the suggested packages to be intentionally unavailable")
})

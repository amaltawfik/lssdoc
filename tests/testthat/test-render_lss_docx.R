test_that("render_lss_docx rejects objects that are not lss", {
  expect_error(
    render_lss_docx(list(), tempfile(fileext = ".docx")),
    class = "lssdoc_bad_lss"
  )
})

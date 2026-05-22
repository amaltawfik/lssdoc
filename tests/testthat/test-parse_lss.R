test_that("parse_lss validates its path argument", {
  expect_error(parse_lss(123), class = "lssdoc_bad_path")
  expect_error(parse_lss(c("a", "b")), class = "lssdoc_bad_path")
})

test_that("parse_lss errors on a missing file", {
  expect_error(
    parse_lss(tempfile(fileext = ".lss")),
    class = "lssdoc_file_not_found"
  )
})

test_that("the bundled example files exist", {
  expect_true(file.exists(
    system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  ))
})

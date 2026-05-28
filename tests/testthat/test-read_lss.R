test_that("read_lss validates its path argument", {
  expect_error(read_lss(123), class = "lssdoc_bad_path")
  expect_error(read_lss(c("a", "b")), class = "lssdoc_bad_path")
})

test_that("read_lss errors on a missing file", {
  expect_error(
    read_lss(tempfile(fileext = ".lss")),
    class = "lssdoc_file_not_found"
  )
})

test_that("read_lss errors on invalid XML", {
  bad <- tempfile(fileext = ".lss")
  writeLines("this is not xml <<<", bad)
  expect_error(read_lss(bad), class = "lssdoc_invalid_xml")
})

test_that("read_lss rejects XML that is not a survey export", {
  not_survey <- tempfile(fileext = ".lss")
  writeLines(
    "<document><LimeSurveyDocType>Token</LimeSurveyDocType></document>",
    not_survey
  )
  expect_error(read_lss(not_survey), class = "lssdoc_not_a_survey")
})

test_that("read_lss reads the bundled hesav example", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  expect_s3_class(lss, "lss")
  expect_identical(lss$languages, c("de", "fr"))
  expect_identical(lss$base_language, "fr")
  expect_identical(lss$doc_type, "Survey")

  expect_identical(nrow(lss$groups), 5L)
  expect_identical(nrow(lss$questions), 31L)
  expect_identical(nrow(lss$subquestions), 78L)
  expect_identical(nrow(lss$answers), 86L)
})

test_that("read_lss keeps localized text and distinguishes empty from absent", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  expect_setequal(unique(lss$question_l10ns$language), c("de", "fr"))
  expect_true(all(c("question", "help") %in% names(lss$question_l10ns)))
  expect_true(any(nzchar(lss$question_l10ns$question)))

  # A present-but-empty <help/> reads as "", never NA.
  expect_false(anyNA(lss$question_l10ns$help))
})

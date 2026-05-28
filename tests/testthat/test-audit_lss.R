test_that("audit_lss rejects objects that are not lss", {
  expect_error(audit_lss(list()), class = "lssdoc_bad_input")
})

test_that("a clean survey produces no findings", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  a <- audit_lss(read_lss(path))
  expect_s3_class(a, "lss_audit")
  expect_identical(a$n_findings, 0L)
  expect_s3_class(as.data.frame(a), "data.frame")
})

test_that("an empty boilerplate text is flagged as an error", {
  path <- system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  a <- audit_lss(read_lss(path))
  empties <- a$findings[a$findings$check == "empty_in_all_languages", ]
  expect_true(nrow(empties) >= 1)
  expect_true(all(empties$severity == "error"))
})

test_that("a missing translation in one language is detected", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Blank out the fr text of the first question that has one.
  l10n <- lss$question_l10ns
  fr <- which(l10n$language == "fr" & nzchar(l10n$question))[1]
  l10n$question[fr] <- ""
  lss$question_l10ns <- l10n

  a <- audit_lss(lss)
  miss <- a$findings[a$findings$check == "missing_translation", ]
  expect_true(any(miss$language == "fr"))
})

test_that("duplicate question codes are flagged as errors", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Force a duplicate variable code.
  lss$questions$title[2] <- lss$questions$title[1]

  a <- audit_lss(lss)
  dups <- a$findings[a$findings$check == "duplicate_code", ]
  expect_true(nrow(dups) >= 1)
  expect_true(all(dups$severity == "error"))
})

test_that("orphan subquestions are detected", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  lss$subquestions$parent_qid[1] <- "999999999"

  a <- audit_lss(lss)
  expect_true(any(a$findings$check == "orphan_subquestion"))
})

test_that("an empty equation text is a note, not an error", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)

  # Turn the first question into an empty equation in every language.
  lss$questions$type[1] <- "*"
  qid <- lss$questions$qid[1]
  is_q <- lss$question_l10ns$qid == qid
  lss$question_l10ns$question[is_q] <- ""

  a <- audit_lss(lss)
  this <- a$findings[
    a$findings$check == "empty_in_all_languages" &
      grepl(lss$questions$title[1], a$findings$location, fixed = TRUE),
  ]
  expect_true(nrow(this) >= 1)
  expect_true(all(this$severity == "note"))
})

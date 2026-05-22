test_that("the taxonomy is well formed", {
  tx <- lss_question_types()
  expect_true(all(
    c(
      "code", "label", "family",
      "has_answers", "has_subquestions", "has_scales", "display_only"
    ) %in% names(tx)
  ))
  expect_false(anyNA(tx$code))
  expect_false(any(duplicated(tx$code)))
  expect_type(tx$has_answers, "logical")
  expect_false(anyNA(tx[c("has_answers", "has_subquestions", "has_scales", "display_only")]))
})

test_that("structural flags match known types", {
  expect_true(lss_type_info("L")$has_answers)
  expect_false(lss_type_info("L")$has_subquestions)
  expect_true(lss_type_info("M")$has_subquestions)
  expect_true(lss_type_info("1")$has_scales)
  expect_true(lss_type_info("X")$display_only)
  expect_identical(lss_type_info("F")$family, "array")
})

test_that("lss_type_label is vectorized and degrades for unknown codes", {
  expect_identical(
    lss_type_label(c("L", "M", "Z")),
    c("List (radio)", "Multiple choice", "Unknown type (Z)")
  )
})

test_that("lss_question_label prefers the type code then the theme name", {
  # Known legacy code wins.
  expect_identical(lss_question_label("L", "listradio"), "List (radio)")
  # Unknown code falls back to a known theme name (plugin types).
  expect_identical(lss_question_label(NA, "ranking"), "Ranking")
  # Unknown code and unknown theme name still name both, never drops.
  expect_identical(
    lss_question_label("Z", "plugin_fancy"),
    "Unknown type (Z / plugin_fancy)"
  )
})

test_that("every question type in the bundled fixtures is recognized", {
  for (file in c("hesav_2026.lss", "limesurvey_survey_751689.lss")) {
    path <- system.file("extdata", file, package = "lssdoc")
    skip_if_not(file.exists(path))
    lss <- parse_lss(path)
    labels <- lss_question_label(
      lss$questions$type,
      lss$questions$question_theme_name
    )
    expect_false(any(grepl("Unknown", labels)), info = file)
  }
})

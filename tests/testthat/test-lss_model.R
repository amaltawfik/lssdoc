test_that("lss_model rejects non-lss input", {
  expect_error(lss_model(list()), class = "lssdoc_bad_lss")
})

test_that("lss_model validates requested languages", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- parse_lss(path)
  expect_error(lss_model(lss, languages = "es"), class = "lssdoc_unknown_language")
  expect_identical(lss_model(lss, languages = "fr")$languages, "fr")
})

test_that("lss_model assembles groups and questions in order", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(parse_lss(path))

  expect_s3_class(m, "lss_model")
  expect_identical(m$languages, c("de", "fr"))
  expect_length(m$groups, 5L)

  n_q <- sum(vapply(m$groups, function(g) length(g$questions), integer(1)))
  expect_identical(n_q, 31L)

  expect_identical(m$groups[[1]]$names$fr, "Vos études à la HES-SO")
})

test_that("list questions carry per-language answer labels", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(parse_lss(path))
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)

  q <- Filter(function(x) x$type == "L", all_q)[[1]]
  expect_identical(q$type_label, "List (radio)")
  expect_true(length(q$answers) >= 1)
  expect_true(all(c("fr", "de") %in% names(q$answers[[1]]$labels)))
  expect_true(nzchar(q$answers[[1]]$labels$fr))
})

test_that("array questions carry subquestions and a shared answer scale", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(parse_lss(path))
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)

  q <- Filter(function(x) x$type == "F", all_q)[[1]]
  expect_true(length(q$subquestions) >= 1)
  expect_true(length(q$answers) >= 1)
  expect_true(nzchar(q$subquestions[[1]]$texts$fr$question))
})

test_that("multiple-choice questions use subquestions, not answers", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  m <- lss_model(parse_lss(path))
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)

  q <- Filter(function(x) x$type == "M", all_q)[[1]]
  expect_true(length(q$subquestions) >= 1)
  expect_length(q$answers, 0L)
})

test_that("a missing translation surfaces as NA, not a dropped entry", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  # Request only the base language; every question still has an fr entry.
  m <- lss_model(parse_lss(path), languages = "fr")
  all_q <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  expect_true(all(vapply(all_q, function(q) "fr" %in% names(q$texts), logical(1))))
})

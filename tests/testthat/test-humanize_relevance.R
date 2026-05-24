test_that("trivial relevance values map to 'All'", {
  expect_identical(lssdoc:::lss_humanize_relevance("1"), "All")
  expect_identical(lssdoc:::lss_humanize_relevance(""), "All")
  expect_identical(lssdoc:::lss_humanize_relevance(NA_character_), "All")
  expect_identical(lssdoc:::lss_humanize_relevance(NULL), "All")
})

test_that("LimeSurvey is_empty / !is_empty idioms become plain text", {
  expect_identical(
    lssdoc:::lss_humanize_relevance("is_empty(foo.NAOK)"),
    "foo is empty"
  )
  expect_identical(
    lssdoc:::lss_humanize_relevance("!is_empty(foo.NAOK)"),
    "foo is answered"
  )
})

test_that("the !is_empty(X.NAOK) && (X.NAOK == N) idiom is collapsed", {
  # The exact pattern the LimeSurvey conditional designer emits.
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "(((!is_empty(filiere.NAOK) && (filiere.NAOK == 2))))"
    ),
    "filiere = 2"
  )
  # Right-side variant.
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "(filiere.NAOK == 2) && !is_empty(filiere.NAOK)"
    ),
    "filiere = 2"
  )
  # Other comparison operators.
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "!is_empty(age.NAOK) && (age.NAOK >= 18)"
    ),
    "age >= 18"
  )
})

test_that("the idiom is collapsed per variable, not across variables", {
  # Two different variables: not the LimeSurvey self-guard, must stay.
  expect_identical(
    lssdoc:::lss_humanize_relevance("!is_empty(a.NAOK) && (b.NAOK == 1)"),
    "a is answered AND (b = 1)"
  )
  # Chained guards: each variable's guard collapses with its own comparison.
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "!is_empty(a.NAOK) && (a.NAOK == 1) && !is_empty(b.NAOK) && (b.NAOK > 0)"
    ),
    "a = 1 AND b > 0"
  )
})

test_that("operators are normalized to plain English / unicode", {
  expect_match(
    lssdoc:::lss_humanize_relevance("foo.NAOK != 1"),
    "foo ≠ 1"
  )
  expect_match(
    lssdoc:::lss_humanize_relevance("foo.NAOK == 1 || bar.NAOK == 2"),
    "foo = 1 OR bar = 2"
  )
})

test_that("unparseable expressions are returned trimmed but otherwise intact", {
  # Function we do not know stays in place; .NAOK is still stripped.
  expect_identical(
    lssdoc:::lss_humanize_relevance("count(a.NAOK, b.NAOK) > 0"),
    "count(a, b) > 0"
  )
})

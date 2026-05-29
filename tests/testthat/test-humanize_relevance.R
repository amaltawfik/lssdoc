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
  # Other comparison operators. `>=` is rendered with the Unicode
  # math symbol U+2265 so the cell reads at a glance.
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "!is_empty(age.NAOK) && (age.NAOK >= 18)"
    ),
    "age \u2265 18"
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
    "foo \u2260 1"
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

test_that("repeated `X == v` clauses on the same variable collapse to a set", {
  expect_identical(
    lssdoc:::lss_humanize_relevance("a.NAOK == 1 || a.NAOK == 2"),
    "a \u2208 {1, 2}"
  )
  expect_identical(
    lssdoc:::lss_humanize_relevance("a.NAOK == 1 || a.NAOK == 2 || a.NAOK == 3"),
    "a \u2208 {1, 2, 3}"
  )
  # Negation form: `&&` and `!=` collapse to `X \u2209 {...}`.
  expect_identical(
    lssdoc:::lss_humanize_relevance("a.NAOK != 1 && a.NAOK != 2"),
    "a \u2209 {1, 2}"
  )
})

test_that("set collapse only fires on the SAME variable", {
  # Different variables stay disjoined.
  expect_identical(
    lssdoc:::lss_humanize_relevance("a.NAOK == 1 || b.NAOK == 2"),
    "a = 1 OR b = 2"
  )
})

test_that("paired bounds collapse to an encased range", {
  expect_identical(
    lssdoc:::lss_humanize_relevance("age.NAOK >= 18 && age.NAOK <= 65"),
    "18 \u2264 age \u2264 65"
  )
  # Strict variant.
  expect_identical(
    lssdoc:::lss_humanize_relevance("age.NAOK > 0 && age.NAOK < 100"),
    "0 < age < 100"
  )
  # Mixed strictness.
  expect_identical(
    lssdoc:::lss_humanize_relevance("age.NAOK >= 18 && age.NAOK < 100"),
    "18 \u2264 age < 100"
  )
})

test_that("range collapse only fires on the SAME variable", {
  expect_identical(
    lssdoc:::lss_humanize_relevance("a.NAOK >= 1 && b.NAOK <= 5"),
    "a \u2265 1 AND b \u2264 5"
  )
})

test_that("AND, OR, is answered, is empty localize via theme$chrome", {
  fr_theme <- list(chrome = lssdoc:::lss_chrome_strings("fr"))
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "!is_empty(a.NAOK) && b.NAOK == 1",
      theme = fr_theme
    ),
    "a est renseign\u00E9 ET b = 1"
  )
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "is_empty(a.NAOK) || b.NAOK == 1",
      theme = fr_theme
    ),
    "a est vide OU b = 1"
  )
  de_theme <- list(chrome = lssdoc:::lss_chrome_strings("de"))
  expect_identical(
    lssdoc:::lss_humanize_relevance(
      "a.NAOK == 1 || a.NAOK == 2",
      theme = de_theme
    ),
    "a \u2208 {1, 2}"
  )
  # Trivial expressions localize the `All` token.
  expect_identical(
    lssdoc:::lss_humanize_relevance("1", theme = fr_theme),
    "Toutes"
  )
})

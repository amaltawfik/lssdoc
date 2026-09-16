# `lss_spec()` is the authoring contract, and every rule it enforces is a
# sentence an author has to be able to act on. They are therefore checked one
# by one, on the smallest specification that can carry the rule -- no file is
# written and nothing is rendered, so the whole file costs milliseconds.
#
# The round trips that prove the contract holds live in `test-write_lss.R` and
# `test-as_lss_spec.R`; what is here is the refusal side.

one_group <- function(questions) list(list(title = "G", questions = questions))

a_text <- function(code = "Q1", ...) {
  utils::modifyList(
    list(code = code, kind = "text", text = "Une question ?"),
    list(...)
  )
}

a_single <- function(code = "Q1", options = NULL, ...) {
  # `options` is a parameter of its own: `modifyList()` merges two lists by
  # name and would leave the default list below untouched.
  q <- list(code = code, kind = "single", text = "Une question ?")
  q$options <- options %||% list(list(code = "1", text = "Oui"),
                                 list(code = "2", text = "Non"))
  utils::modifyList(q, list(...))
}

# The smallest valid specification, with one question replaced.
spec_with <- function(q, ...) {
  lss_spec(title = "T", groups = one_group(list(q)), ...)
}

# ---- the arguments themselves ------------------------------------------------

test_that("lss_spec refuses a title it cannot use", {
  expect_error(lss_spec(title = NULL, groups = one_group(list(a_text()))),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = 42, groups = one_group(list(a_text()))),
               class = "lssdoc_bad_spec")
  # a blank string is no title at all, and is caught after normalization
  expect_error(lss_spec(title = "   ", groups = one_group(list(a_text()))),
               class = "lssdoc_bad_spec")
})

test_that("lss_spec needs a non-empty list of groups", {
  expect_error(lss_spec(title = "T", groups = list()), class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = "G"), class = "lssdoc_bad_spec")
})

test_that("the declared languages are validated", {
  g <- one_group(list(a_text()))
  expect_error(lss_spec(title = "T", groups = g, language = ""),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, language = c("fr", "en")),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, language = NA_character_),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, languages = character()),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, languages = NA_character_),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, languages = c("fr", "")),
               class = "lssdoc_bad_spec")
})

# ---- localizable texts --------------------------------------------------------

test_that("a localizable text is a string, or a value keyed by language", {
  g <- one_group(list(a_text()))
  expect_error(lss_spec(title = "T", groups = g, welcome = 42),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, welcome = list()),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, welcome = list(fr = 1)),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, welcome = NA_character_),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, welcome = c(fr = "a", "b")),
               class = "lssdoc_bad_spec")
  expect_error(lss_spec(title = "T", groups = g, welcome = list(fr = "a", fr = "b")),
               class = "lssdoc_bad_spec")

  # an empty string carries no language of its own and no text either: it is
  # dropped, not refused
  expect_null(lss_spec(title = "T", groups = g, welcome = "")$welcome)
})

test_that("a localized text must give the primary language", {
  # everything else is bilingual, so the welcome text is the only field that
  # can raise: the primary language is the one write_lss() emits as the base
  expect_error(
    lss_spec(
      title = list(fr = "T", en = "T"),
      languages = c("fr", "en"),
      groups = list(list(title = list(fr = "G", en = "G"),
                         questions = list(a_text(text = list(fr = "Q", en = "Q"))))),
      welcome = list(en = "Hello")
    ),
    class = "lssdoc_bad_spec"
  )
})

test_that("loc_text reads a value that is not localized yet", {
  expect_identical(loc_text("abc"), "abc")
  expect_identical(loc_text(list()), "")
  expect_identical(loc_text(list(), default = "-"), "-")
  expect_identical(loc_text(NULL, default = "-"), "-")
})

# ---- groups, questions and their item lists ----------------------------------

test_that("a group needs at least one question", {
  expect_error(
    lss_spec(title = "T", groups = list(list(title = "G", questions = list()))),
    class = "lssdoc_bad_spec"
  )
})

test_that("an item of an option list is a string or a list", {
  expect_error(spec_with(a_single(options = list(1, 2))),
               class = "lssdoc_bad_spec")
})

test_that("an array says what its rows and its columns are", {
  # rows and columns both required
  expect_error(
    spec_with(list(code = "Q1", kind = "array", text = "Q ?",
                   columns = list(list(code = "1", text = "A")))),
    class = "lssdoc_bad_spec"
  )
  # a fixed-scale array carries rows only, and still needs them
  expect_error(
    spec_with(list(code = "Q1", kind = "array5", text = "Q ?")),
    class = "lssdoc_bad_spec"
  )
})

test_that("option codes are unique and every item carries a label", {
  expect_error(
    spec_with(a_single(options = list(list(code = "1", text = "Oui"),
                                      list(code = "1", text = "Non")))),
    class = "lssdoc_bad_spec"
  )
  expect_error(
    spec_with(a_single(options = list(list(code = "1", text = "Oui"),
                                      list(code = "2", text = "   ")))),
    class = "lssdoc_bad_spec"
  )
})

test_that("an array row or column cannot be the native other option", {
  expect_error(
    spec_with(list(code = "Q1", kind = "array", text = "Q ?",
                   rows = list(list(code = "R1", text = "Ligne"),
                               list(other = TRUE, text = "Autre")),
                   columns = list(list(code = "1", text = "A"),
                                  list(code = "2", text = "B")))),
    class = "lssdoc_bad_spec"
  )
})

test_that("an exclusive option only exists on a multiple choice", {
  expect_error(
    spec_with(a_single(options = list(list(code = "1", text = "Oui"),
                                      list(code = "2", text = "Non",
                                           exclusive = TRUE)))),
    class = "lssdoc_bad_spec"
  )
})

# ---- the native other option --------------------------------------------------

test_that("at most one option is the other option", {
  expect_error(
    spec_with(a_single(options = list(list(code = "1", text = "Oui"),
                                      list(other = TRUE, text = "Autre"),
                                      list(other = TRUE, text = "Encore")))),
    class = "lssdoc_bad_spec"
  )
})

test_that("the kinds that allow the other option are named from the table", {
  ranking <- list(code = "Q1", kind = "ranking", text = "Classez",
                  options = list(list(code = "1", text = "A"),
                                 list(code = "2", text = "B"),
                                 list(other = TRUE, text = "Autre")))
  err <- expect_error(spec_with(ranking), class = "lssdoc_bad_spec")
  expect_match(conditionMessage(err), "single", fixed = TRUE)

  # the sentence stays grammatical if the table ever holds a single such kind
  testthat::local_mocked_bindings(kinds_where = function(...) "single")
  err1 <- expect_error(spec_with(ranking), class = "lssdoc_bad_spec")
  expect_match(conditionMessage(err1), "single", fixed = TRUE)
})

test_that("other_position needs an other option, and a position it knows", {
  expect_error(spec_with(a_single(other_position = "end")),
               class = "lssdoc_bad_spec")
  expect_error(
    spec_with(a_single(options = list(list(code = "1", text = "Oui"),
                                      list(other = TRUE, text = "Autre")),
                       other_position = "au milieu")),
    class = "lssdoc_bad_spec"
  )
})

# ---- caps ---------------------------------------------------------------------

test_that("max_answers applies to the kinds that have a cap, and is a count", {
  expect_error(spec_with(a_single(max_answers = 1L)), class = "lssdoc_bad_spec")

  multiple <- function(...) {
    utils::modifyList(
      list(code = "Q1", kind = "multiple", text = "Q ?",
           options = list(list(code = "1", text = "A"),
                          list(code = "2", text = "B"),
                          list(code = "3", text = "C"))),
      list(...))
  }
  expect_error(spec_with(multiple(max_answers = 0L)), class = "lssdoc_bad_spec")
  expect_error(spec_with(multiple(max_answers = "deux")),
               class = "lssdoc_bad_spec")

  # a ranking may rank them all, and no more than all
  ranking <- list(code = "Q1", kind = "ranking", text = "Classez",
                  options = list(list(code = "1", text = "A"),
                                 list(code = "2", text = "B")),
                  max_answers = 3L)
  expect_error(spec_with(ranking), class = "lssdoc_bad_spec")
})

# ---- relevance ----------------------------------------------------------------

test_that("count() needs a multiple choice and = needs a single answer", {
  expect_error(
    lss_spec(title = "T", groups = one_group(list(
      a_single("Q1"),
      a_text("Q2", relevance = "count(Q1) >= 2")))),
    class = "lssdoc_bad_spec"
  )
})

test_that("a filter on the other option needs the target to have one", {
  expect_error(
    lss_spec(title = "T", groups = one_group(list(
      a_single("Q1"),
      a_text("Q2", relevance = "Q1 = autre")))),
    class = "lssdoc_bad_spec"
  )
})

# ---- the kinds table's own constructor ----------------------------------------

test_that("kind_def refuses a row the kinds table could not carry", {
  # `lss_kinds` is built by this constructor at package build time, so its
  # guard never runs at use time: it is the rule an added kind is held to,
  # and it is tested here rather than left to the next person to add one.
  row <- kind_def("demo", "L", "listradio", "Demo", "choice",
                  options = "required", rows = "ignored", min_options = 2L,
                  answers_from = "options", relevance_role = "scalar")
  expect_identical(row$kind, "demo")
  expect_identical(row$min_options, 2L)
  expect_identical(row$other_allowed, FALSE)

  # a type letter is one character
  expect_error(kind_def("demo", "LL", "listradio", "Demo"))
  # the three-valued fields take their three values
  expect_error(kind_def("demo", "L", "listradio", "Demo", options = "maybe"))
  # `options = "required"` and `min_options` are one rule, not two
  expect_error(kind_def("demo", "L", "listradio", "Demo", options = "required"))
  expect_error(kind_def("demo", "L", "listradio", "Demo", min_options = 2L))
  expect_error(kind_def("demo", "L", "listradio", "Demo",
                        relevance_role = "vector"))
  expect_error(kind_def("demo", "L", "listradio", "Demo",
                        max_answers_rule = "above_n"))
})

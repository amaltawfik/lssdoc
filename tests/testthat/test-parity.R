# The two layouts of render_documentation() hold the same content: every
# text of the survey that one layout prints, in every language of the
# document, the other prints too, and the two variable indexes list the same
# variables. The form may differ (a token for a sentence, a row for a card).
#
# This file is a net for whole texts: a short text such as "cm" can be
# found by accident in any document, so the precise checks of each element
# (the row, the cell) live in the tests of the code that prints it. The
# helpers are in helper-parity.R.

# The texts a layout does not print yet, named "<survey>: <id> <language>"
# as parity_missing() names them, by the sub-step that closes each gap.
# Each sub-step removes its entries when it closes them; the last one
# removes the list. A gap that is closed and still listed fails the test,
# so that the list never hides a text that is printed.
parity_known_gaps <- list(
  # 8.5 (GAPS B1): the cards print the name of a group in every language,
  # and neither layout strips a number the author typed in a group name.
  "8.5 group names" = list(
    cards = c(
      "demo: group 1 name fr", "demo: group 2 name fr",
      "demo: group 3 name fr", "demo: group 4 name fr",
      "demo: group 5 name fr", "demo: group 6 name fr",
      "parity: group 1 name fr", "parity: group 2 name fr",
      "parity: group 3 name fr", "parity: group 4 name fr",
      "parity: group 5 name en", "parity: group 5 name fr",
      "parity: group 6 name fr"
    ),
    table = c("parity: group 5 name en", "parity: group 5 name fr")
  )
)

parity_known <- function(template, survey) {
  k <- as.character(unlist(lapply(parity_known_gaps, function(g) {
    g[[template]]
  }), use.names = FALSE))
  k[startsWith(k, paste0(survey, ": "))]
}

test_that("parity_expected() reads the texts of the model", {
  lss <- parity_lss()
  ex <- parity_expected(lss)
  get <- function(id, lg) ex$text[ex$id == id & ex$lang == lg]
  # The HTML is flattened: one line per paragraph and per list item.
  expect_identical(
    get("healthself stem", "en"),
    paste("How is your health in general, today?",
          "Lung volume in dm3", "Blood CO2 level", sep = "\n")
  )
  expect_identical(get("sleephours suffix", "fr"), "heures par nuit")
  expect_identical(get("nationality other prompt", "fr"), "Autre\u00A0:")
  expect_identical(get("imc equation", "en"),
                   "round((weight / ((height/100)*(height/100))), 1)")
  # A text missing in one language is not expected in it.
  expect_length(get("occupation stem", "fr"), 0L)
  expect_length(get("occupation stem", "en"), 1L)
  # parity_found() looks for each line inside one entry, whitespace
  # squished.
  expect_true(parity_found("a b\nc", c("x a\u00A0 b y", "c")))
  expect_false(parity_found("a b\nc", c("x a b y")))
  # The value of an attribute fills its cell: it is marked `whole`, and a
  # whole text is found only as one entry, whitespace squished, never
  # inside another text.
  is_attr <- grepl(" (prefix|suffix|validation|equation)$", ex$id)
  expect_true(any(is_attr))
  expect_identical(ex$whole, is_attr)
  expect_true(parity_found("h", c("12 h a day", " h\u00A0"), whole = TRUE))
  expect_false(parity_found("h", c("12 h a day", "hours"), whole = TRUE))
  expect_true(parity_found("h", c("12 h a day", "hours")))
  expect_true(parity_found("hours a\nnight", "hours a night", whole = TRUE))
  expect_identical(
    parity_missing(data.frame(id = c("a", "b"), lang = "en", text = "h",
                              whole = c(FALSE, TRUE)),
                   list(content = "12 h a day")),
    "b en"
  )
  # The formula of a computed question is read as written, not as HTML.
  qa <- lss$question_attributes
  qa$value[qa$attribute == "equation" &
             qa$qid %in% lss$questions$qid[lss$questions$title == "imc"]] <-
    "{if(a<b, 1, 2)}"
  lss$question_attributes <- qa
  expect_identical(
    parity_expected(lss)$text[parity_expected(lss)$id == "imc equation"],
    c("if(a<b, 1, 2)", "if(a<b, 1, 2)")
  )
})

test_that("the cards and the table print the same texts and variables", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  surveys <- list(demo = demo_lss(), parity = parity_lss())
  for (s in names(surveys)) {
    ex <- parity_expected(surveys[[s]])
    docs <- lapply(c(cards = "cards", table = "table"), function(tpl) {
      parity_render(surveys[[s]], tpl, key = s)
    })
    for (tpl in names(docs)) {
      missing <- sprintf("%s: %s", s, parity_missing(ex, docs[[tpl]]))
      known <- parity_known(tpl, s)
      # A text the layout does not print, and that no sub-step has listed.
      expect_identical(setdiff(missing, known), character(0),
                       info = paste(s, tpl, "texts not printed"))
      # A gap that is listed and closed: remove it from parity_known_gaps.
      expect_identical(setdiff(known, missing), character(0),
                       info = paste(s, tpl, "listed gaps now printed"))
    }
    expect_identical(sort(parity_index_variables(docs$cards)),
                     sort(parity_index_variables(docs$table)),
                     info = paste(s, "variable index"))
  }
})

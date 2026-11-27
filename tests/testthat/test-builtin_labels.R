# The labels LimeSurvey builds in (Yes / No, Female / Male, Yes /
# Uncertain / No, Increase / Same / Decrease) and the default prompt of an
# "Other" field are read by the respondent: render_documentation() prints
# them in the language of each column when lssdoc has strings for it, and
# in the chrome language otherwise.

bl_theme <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}
BL_L3 <- c("en", "fr", "pt")
BL_AUTRE <- "Autre\u00A0:"

# ---- the two helpers -----------------------------------------------------

test_that("lss_builtin_lang keeps the primary subtag of a supported language", {
  expect_identical(lss_builtin_lang("fr"), "fr")
  expect_identical(lss_builtin_lang("FR"), "fr")
  expect_identical(lss_builtin_lang("de-informal"), "de")
  expect_identical(lss_builtin_lang("pt-BR"), NA_character_)
})

test_that("lss_builtin_strings gives the pack of the language, else the chrome", {
  en <- lss_chrome_strings("en")
  fr <- lss_chrome_strings("fr")
  expect_identical(lss_builtin_strings("de-informal", en), lss_chrome_strings("de"))
  expect_identical(lss_builtin_strings("FR", en), fr)
  # A language without strings keeps the chrome it is given, unchanged.
  expect_identical(lss_builtin_strings("pt-BR", en), en)
  expect_identical(lss_builtin_strings("pt", fr), fr)
})

test_that("every pack words the default prompt of an Other field", {
  prompts <- vapply(c("en", "fr", "de", "es", "it"), function(lg) {
    lss_chrome_strings(lg)$value_other_prompt
  }, character(1))
  expect_identical(
    unname(prompts),
    c("Other:", BL_AUTRE, "Sonstiges:", "Otro:", "Altro:")
  )
})

# ---- cards: Value rows of the labelled scales ----------------------------

test_that("the cards Value rows of a yes/no question follow each column", {
  rows <- lss_predefined_value_rows(list(type = "Y"), BL_L3, bl_theme())
  expect_length(rows, 3L)
  expect_identical(rows[[2]]$label, "Y")
  expect_identical(rows[[2]]$texts, list(en = "Yes", fr = "Oui", pt = "Yes"))
  expect_identical(rows[[3]]$label, "N")
  expect_identical(rows[[3]]$texts, list(en = "No", fr = "Non", pt = "No"))
})

test_that("the cards rows of an N-point scale keep their descriptor and blanks", {
  rows <- lss_predefined_value_rows(list(type = "B"), BL_L3, bl_theme())
  expect_length(rows, 11L)
  expect_identical(
    rows[[1]]$texts,
    stats::setNames(rep(list("10-point scale (1 to 10)"), 3L), BL_L3)
  )
  expect_true(isTRUE(rows[[1]]$span_note))
  expect_identical(rows[[2]]$label, "1")
  expect_identical(rows[[2]]$texts, list(en = "", fr = "", pt = ""))
  expect_true(isTRUE(rows[[2]]$blank))
})

# ---- table: Value rows built by emit_value_rows_for() ----------------------

test_that("the table Value rows of a yes/no question follow each column", {
  txt <- function(s) list(question = s, help = NULL)
  q <- list(
    type = "Y", code = "qY", mandatory = "N", relevance = NULL, other = "N",
    texts = list(en = txt("Smoker?"), fr = txt("Fumeur ?"), pt = txt("Fumante?")),
    answers = list(), scales = NULL, subquestions = list(), attributes = NULL
  )
  g <- list(questions = list(q))
  state <- lss_render_state(NULL)
  rows <- lss_table_template_rows_for_group(
    g, BL_L3, bl_theme(), show_help = TRUE, state = state, show_groups = FALSE
  )
  vals <- Filter(function(r) identical(r$kind, "value"), rows)
  expect_length(vals, 2L)
  expect_identical(vals[[1]]$code, "Y")
  expect_identical(vals[[1]]$labels, list(en = "Yes", fr = "Oui", pt = "Yes"))
  expect_identical(vals[[2]]$code, "N")
  expect_identical(vals[[2]]$labels, list(en = "No", fr = "Non", pt = "No"))
})

# ---- the prompt of an "Other" field --------------------------------------

test_that("lss_other_prompt words the default prompt in the language of the column", {
  en <- lss_chrome_strings("en")
  q <- list(attributes = NULL)
  expect_identical(lss_other_prompt(q, "en", en), "Other:")
  expect_identical(lss_other_prompt(q, "fr", en), BL_AUTRE)
  expect_identical(lss_other_prompt(q, "pt", en), "Other:")
  # A language without strings takes the prompt of the chrome language.
  expect_identical(lss_other_prompt(q, "pt", lss_chrome_strings("de")),
                   "Sonstiges:")
})

test_that("lss_other_prompt prefers the replacement text of the survey", {
  en <- lss_chrome_strings("en")
  attrs <- function(value, language) {
    list(attributes = data.frame(
      attribute = "other_replace_text", value = value, language = language,
      stringsAsFactors = FALSE
    ))
  }
  q_lang <- attrs(c("Please specify", "Bitte angeben"), c("en", "de"))
  expect_identical(lss_other_prompt(q_lang, "de", en), "Bitte angeben")
  expect_identical(lss_other_prompt(q_lang, "fr", en), BL_AUTRE)
  q_none <- attrs("Pr\u00e9cisez", "")
  expect_identical(lss_other_prompt(q_none, "fr", en), "Pr\u00e9cisez")
  expect_identical(lss_other_prompt(q_none, "en", en), "Pr\u00e9cisez")
  q_blank <- attrs("  ", "fr")
  expect_identical(lss_other_prompt(q_blank, "fr", en), BL_AUTRE)
})

# ---- rendered documents --------------------------------------------------

# Does a rendered document of the demo survey print the built-in labels in
# the column of each language? Cards: the Value rows of the card of
# chroniccondition, the Question row of the card of nationality[other].
# Table: the Value rows under the row of chroniccondition, the row of
# nationality[other]. The rows of that question, not any row of the
# document. The helpers are in helper-parity.R.
bl_labels_in_columns <- function(doc, template) {
  if (identical(template, "cards")) {
    has <- function(rows, cells) {
      any(vapply(rows, identical, logical(1), cells))
    }
    yn <- parity_card_rows(doc, "chroniccondition")
    other <- parity_card_rows(doc, "nationality[other]")
    return(c(
      yes = has(yn, c("Y", "Yes", "Oui")),
      no = has(yn, c("N", "No", "Non")),
      other = has(other, c("Question", "Other:", BL_AUTRE))
    ))
  }
  grid <- parity_table_grid(doc)
  has <- function(block, code, en, fr) {
    any(block[, "Value"] == code & block[, "en"] == en & block[, "fr"] == fr)
  }
  yn <- parity_grid_block(grid, "chroniccondition")
  other <- parity_grid_block(grid, "nationality[other]")
  c(
    yes = has(yn, "Y", "Yes", "Oui"),
    no = has(yn, "N", "No", "Non"),
    other = identical(unname(other[1L, c("en", "fr")]), c("Other:", BL_AUTRE))
  )
}

test_that("a rendered document prints the built-in labels in each column", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  for (tpl in c("cards", "table")) {
    doc <- parity_render(demo_lss(), tpl, key = "demo")
    expect_identical(bl_labels_in_columns(doc, tpl),
                     c(yes = TRUE, no = TRUE, other = TRUE), info = tpl)
  }
})

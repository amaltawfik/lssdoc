# The attributes of `show_attrs` (prefix, suffix, validation, the formula of
# a computed question, ...) in the table layout: the rows the cards print
# with lss_attr_rows(), at the same places, one annotation row each. Then
# the other rows the table prints as the cards do (8.3): the Other row, the
# comment row of a list with comment, the empty marker per part of a cell,
# the exclusive option.

PFX <- list("prefix", "approx.", "en")
PFX_FR <- list("prefix", "env.", "fr")
SFX <- list("suffix", "cm", "")

# ---- the rows ----------------------------------------------------------------

test_that("table rows: the attributes of a leaf question follow its Question row", {
  q <- ta_q("L", "q1", ta_attrs(PFX, PFX_FR, SFX),
            answers = list(ta_answer("A1"), ta_answer("A2")), other = "Y")
  rows <- ta_rows(list(q))
  expect_identical(ta_kinds(rows),
                   c("leaf", "attr", "attr", "value", "value", "other"))
  a <- ta_attr(rows)
  expect_identical(vapply(a, function(r) r$label, ""), c("Prefix", "Suffix"))
  expect_identical(vapply(a, function(r) r$attribute, ""), c("prefix", "suffix"))
  expect_identical(a[[1]]$texts, list(en = "approx.", fr = "env."))
  expect_false(a[[1]]$merge)
  # Texts of the author, not expressions.
  expect_identical(vapply(a, function(r) r$raw, NA), c(FALSE, FALSE))
  # The row without a language holds for every language: one merged cell.
  expect_identical(a[[2]]$texts, list(en = "cm", fr = "cm"))
  expect_true(a[[2]]$merge)
  # The same rows as the cards, from the same function.
  cards <- lss_attr_rows(q, TA_LANGS, ta_theme(), TA_ATTRS)
  expect_identical(lapply(a, function(r) r[c("label", "texts")]),
                   lapply(cards, function(r) r[c("label", "texts")]))
})

test_that("table rows: a list with comment prints no attribute on its comment row", {
  q <- ta_q("O", "q2", ta_attrs(SFX), answers = list(ta_answer("A1")))
  rows <- ta_rows(list(q))
  expect_identical(ta_kinds(rows), c("leaf", "attr", "value", "leaf"))
  expect_identical(rows[[4]]$variable, "q2[_Ccomment]")
})

test_that("table rows: the formula of a computed question is an attribute row", {
  q <- ta_q("*", "q3", ta_attrs(list("equation", "{a + b}", "")))
  rows <- ta_rows(list(q))
  expect_identical(ta_kinds(rows), c("leaf", "attr"))
  expect_identical(rows[[2]]$attribute, "equation")
  expect_identical(rows[[2]]$label, "Equation")
  expect_identical(rows[[2]]$texts, list(en = "a + b", fr = "a + b"))
  expect_true(rows[[2]]$merge)
  # The flag of lss_attr_rows() reaches the row: printed as written.
  expect_true(rows[[2]]$raw)
  expect_identical(ta_kinds(ta_rows(list(q), show_attrs = "prefix")), "leaf")
})

test_that("cards: an empty formula gives no row", {
  for (v in c("{}", "{ }", " { } ")) {
    q <- ta_q("*", "q3", ta_attrs(list("equation", v, "")))
    expect_length(lss_attr_rows(q, TA_LANGS, ta_theme(), TA_ATTRS), 0L)
  }
})

test_that("table rows: an empty formula gives no row", {
  for (v in c("{}", "{ }", " { } ")) {
    q <- ta_q("*", "q3", ta_attrs(list("equation", v, "")))
    expect_identical(ta_kinds(ta_rows(list(q))), "leaf", info = v)
  }
})

test_that("lss_attr_rows(): the formula row is marked raw, its text as written", {
  q <- ta_q("*", "q3", ta_attrs(list("equation", "{if(a<b, 1, 2)}", "")))
  r <- lss_attr_rows(q, TA_LANGS, ta_theme(), "equation")
  expect_length(r, 1L)
  expect_identical(r[[1]]$label, "Equation")
  expect_identical(r[[1]]$texts, list(en = "if(a<b, 1, 2)", fr = "if(a<b, 1, 2)"))
  expect_true(r[[1]]$raw)
  # Data only: the layout composes the text.
  expect_null(r[[1]]$paragraph)
  # Any other attribute is a text of the author: no flag.
  q <- ta_q("N", "q4", ta_attrs(SFX))
  s <- lss_attr_rows(q, TA_LANGS, ta_theme(), "suffix")
  expect_length(s, 1L)
  expect_false("raw" %in% names(s[[1]]))
  expect_null(s[[1]]$paragraph)
})

test_that("table rows: a multiple choice and a ranking print theirs under the parent row", {
  mc <- ta_q("M", "q4", ta_attrs(SFX),
             subquestions = list(ta_sq("S1"), ta_sq("S2")))
  expect_identical(ta_kinds(ta_rows(list(mc))),
                   c("leaf", "attr", "mc_option", "mc_option"))
  rk <- ta_q("R", "q5", ta_attrs(PFX),
             answers = list(ta_answer("A1"), ta_answer("A2")))
  expect_identical(ta_kinds(ta_rows(list(rk))),
                   c("leaf", "attr", "mc_option", "mc_option", "value", "value"))
})

test_that("table rows: each subquestion row carries its attributes, then the parent's", {
  arr <- ta_q("F", "q6", ta_attrs(SFX),
              answers = list(ta_answer("A1"), ta_answer("A2")),
              subquestions = list(
                ta_sq("S1", attributes = ta_attrs(list("prefix", "own", ""))),
                ta_sq("S2")
              ))
  rows <- ta_rows(list(arr))
  expect_identical(ta_kinds(rows), c(
    "subq", "attr", "attr", "value", "value",
    "subq", "attr", "value", "value"
  ))
  expect_identical(vapply(ta_attr(rows), function(r) r$texts$en, ""),
                   c("own", "cm", "cm"))
  # A multiple numeric: no Value rows.
  num <- ta_q("K", "q7", ta_attrs(PFX, SFX),
              subquestions = list(ta_sq("S1"), ta_sq("S2")))
  expect_identical(ta_kinds(ta_rows(list(num))),
                   c("subq", "attr", "attr", "subq", "attr", "attr"))
})

test_that("table rows: a dual scale and a two-dimensional array, row by row", {
  dual <- ta_q("1", "q8", ta_attrs(SFX),
               scales = list(list(ta_answer("A1")), list(ta_answer("B1", "1"))),
               answers = list(ta_answer("A1"), ta_answer("B1", "1")),
               subquestions = list(ta_sq("S1")))
  expect_identical(ta_kinds(ta_rows(list(dual))),
                   c("subq", "attr", "value", "subq", "attr", "value"))
  two_d <- ta_q(":", "q9", ta_attrs(SFX), subquestions = list(
    ta_sq("R1", attributes = ta_attrs(list("prefix", "row", ""))),
    ta_sq("C1", scale = "1", attributes = ta_attrs(list("prefix", "col", ""))),
    ta_sq("C2", scale = "1")
  ))
  rows <- ta_rows(list(two_d))
  expect_identical(ta_kinds(rows),
                   c("subq", "attr", "attr", "subq", "attr", "attr"))
  # The attributes of the row subquestion, as on the card of the cell.
  expect_identical(vapply(ta_attr(rows), function(r) r$texts$en, ""),
                   c("row", "cm", "row", "cm"))
})

test_that("table rows: no attribute row without show_attrs", {
  q <- ta_q("N", "q10", ta_attrs(PFX, SFX))
  expect_identical(ta_kinds(ta_rows(list(q), show_attrs = character(0))),
                   "leaf")
  expect_identical(ta_kinds(ta_rows(list(q), show_attrs = NULL)), "leaf")
})

# ---- the rendered table ------------------------------------------------------

test_that("table: the attribute rows of the parity survey, cell by cell", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- parity_render(parity_lss(), "table", key = "parity")
  grid <- parity_table_grid(doc)
  muted <- sub("^#", "", ta_theme()$color_muted)

  # A leaf question with a prefix and a suffix that differ by language:
  # one row each under the Question row, a cell per language.
  b <- parity_grid_block(grid, "physicalactivitydays")
  expect_identical(unname(b[, "Field"]), c("Question", "Prefix", "Suffix"))
  expect_identical(unname(b[2:3, "en"]), c("approx.", "day(s)"))
  expect_identical(unname(b[2:3, "fr"]), c("env.", "jour(s)"))
  expect_true(all(b[2:3, c("No", "Variable", "Type", "Mandatory", "Filter",
                           "Value")] == ""))
  rows <- ta_xml_rows(doc)
  k <- which(vapply(rows, function(r) identical(r$cells[1], "Prefix") &&
                      "approx." %in% r$cells, logical(1)))
  expect_length(k, 1L)
  expect_identical(rows[[k]]$cells[8:9], c("approx.", "env."))
  # Muted italic, as the other annotation rows.
  run <- ta_run(rows[[k]]$tcs[[8]])
  expect_true(run$italic)
  expect_identical(toupper(run$color), toupper(muted))

  # The same text in every language: the language columns merged.
  b <- parity_grid_block(grid, "height")
  expect_identical(unname(b[, "Field"]), c("Question", "Suffix"))
  k <- which(vapply(rows, function(r) identical(r$cells[1], "Suffix") &&
                      "cm" %in% r$cells, logical(1)))
  expect_length(k, 1L)
  expect_identical(length(rows[[k]]$cells), 8L)
  expect_identical(ta_span(rows[[k]]$tcs[[8]]), 2L)

  # A question shown as one row per subquestion: the attributes of the
  # parent under each subquestion row.
  sq_vars <- grep("^sleephours\\[", grid[, "Variable"], value = TRUE)
  expect_length(sq_vars, 2L)
  for (v in sq_vars) {
    b <- parity_grid_block(grid, v)
    expect_identical(unname(b[, "Field"]), c("Question", "Prefix", "Suffix"),
                     info = v)
    expect_identical(unname(b[2:3, "en"]), c("approximately", "hours a night"),
                     info = v)
    expect_identical(unname(b[2:3, "fr"]),
                     c("approximativement", "heures par nuit"), info = v)
  }
})

test_that("both layouts print a formula with a comparison as written", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- demo_lss()
  imc <- lss$questions$qid[lss$questions$title == "imc"]
  lss$questions <- lss$questions[lss$questions$qid == imc, , drop = FALSE]
  qa <- lss$question_attributes
  hit <- qa$qid == imc & qa$attribute == "equation"
  expect_true(any(hit))
  qa$value[hit] <- "{if(a<b, 1, 2)}"
  lss$question_attributes <- qa
  formula <- "if(a<b, 1, 2)"
  th <- ta_theme()
  rows_with <- function(doc, label) {
    trs <- xml2::xml_find_all(doc$xml, "//w:tbl/w:tr", parity_w)
    Filter(function(r) identical(r$cells[1], label), lapply(trs, function(tr) {
      tcs <- xml2::xml_find_all(tr, "w:tc", parity_w)
      list(tcs = tcs, cells = vapply(tcs, parity_node_text, ""))
    }))
  }

  # Cards: the Equation row reads the formula whole in each language
  # column, upright, in the code font and the text colour.
  hit <- rows_with(parity_render(lss, "cards"), "Equation")
  expect_length(hit, 1L)
  expect_identical(hit[[1]]$cells[2:3], c(formula, formula))
  for (tc in hit[[1]]$tcs[2:3]) {
    run <- ta_run(tc)
    expect_identical(run$font, th$font_code)
    expect_false(run$italic)
    expect_identical(toupper(run$color), toupper(sub("^#", "", th$color_text)))
  }

  # Table: the formula already prints whole, once across the languages.
  hit <- rows_with(parity_render(lss, "table"), "Equation")
  expect_length(hit, 1L)
  expect_true(formula %in% hit[[1]]$cells)
  expect_identical(ta_run(hit[[1]]$tcs[[which(hit[[1]]$cells == formula)]])$font,
                   th$font_code)
})

test_that("table: a language without the attribute shows the empty marker", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- demo_lss()
  lss$questions <- lss$questions[lss$questions$title == "physicalactivitydays", ,
                                 drop = FALSE]
  qa <- lss$question_attributes
  lss$question_attributes <- qa[!(qa$attribute == "prefix" & qa$language == "fr"), ,
                                drop = FALSE]
  grid <- parity_table_grid(parity_render(lss, "table"))
  b <- parity_grid_block(grid, "physicalactivitydays")
  expect_identical(unname(b[2, c("Field", "en", "fr")]),
                   c("Prefix", "approx.", ta_theme()$empty_marker))
})

# ---- 8.3: the Other row, the comment row, the empty marker per part, the
# exclusive option -------------------------------------------------------------

EXCL <- function(value) list("exclude_all_others", value, "")

test_that("the exclusive codes are separated by a semicolon", {
  q <- ta_q("M", "q1", ta_attrs(EXCL(" S8 ; S9;")))
  expect_identical(lss_exclusive_codes(q), c("S8", "S9"))
  ch <- lss_chrome_strings("en")
  # The grouped form: one sentence naming the options.
  expect_identical(lss_exclusive_options_text(q, ch),
                   "S8, S9: when checked, clears all other selections")
  expect_null(lss_exclusive_options_text(ta_q("M", "q2"), ch))
  # The form of a subquestion: only the named ones.
  expect_identical(lss_exclusive_text(q, list(code = "S9"), ch),
                   "When answered, clears all other answers of q1")
  expect_null(lss_exclusive_text(q, list(code = "S1"), ch))
  expect_identical(lss_exclusive_row(q, list(code = "S9"), TA_LANGS,
                                     ta_theme())$texts$fr,
                   "When answered, clears all other answers of q1")
})

test_that("table rows: the exclusive sentence of a multiple choice is the cards'", {
  mc <- ta_q("M", "q4", ta_attrs(EXCL("S1;S2")),
             subquestions = list(ta_sq("S1"), ta_sq("S2"), ta_sq("S3")))
  rows <- ta_rows(list(mc))
  expect_identical(ta_kinds(rows),
                   c("leaf", "mc_option", "mc_option", "mc_option", "mc_exclusive"))
  expect_identical(rows[[5]]$text,
                   "S1, S2: when checked, clears all other selections")
  expect_identical(rows[[5]]$text,
                   lss_exclusive_options_text(mc, ta_theme()$chrome))
})

test_that("table rows: an exclusive subquestion has its row where its card has it", {
  # After the attribute rows of the subquestion row, before its Value rows.
  arr <- ta_q("F", "q6", ta_attrs(SFX, EXCL("S2")),
              answers = list(ta_answer("A1"), ta_answer("A2")),
              subquestions = list(ta_sq("S1"), ta_sq("S2")))
  rows <- ta_rows(list(arr))
  expect_identical(ta_kinds(rows), c(
    "subq", "attr", "value", "value",
    "subq", "attr", "mc_exclusive", "value", "value"
  ))
  expect_identical(rows[[7]]$text, "When answered, clears all other answers of q6")
  # The same function as the card, under the same condition.
  for (sq in arr$subquestions) {
    card <- lss_exclusive_row(arr, sq, TA_LANGS, ta_theme())
    in_table <- any(vapply(seq_along(rows), function(i) {
      identical(rows[[i]]$kind, "mc_exclusive") &&
        identical(rows[[max(which(ta_kinds(rows)[seq_len(i)] == "subq"))]]$sq$code,
                  sq$code)
    }, logical(1)))
    expect_identical(in_table, !is.null(card), info = sq$code)
  }
  # A multiple numeric: no Value rows.
  num <- ta_q("K", "q7", ta_attrs(EXCL("S1")),
              subquestions = list(ta_sq("S1"), ta_sq("S2")))
  expect_identical(ta_kinds(ta_rows(list(num))), c("subq", "mc_exclusive", "subq"))
  # A dual scale: on the row of each scale of that subquestion.
  dual <- ta_q("1", "q8", ta_attrs(EXCL("S1")),
               scales = list(list(ta_answer("A1")), list(ta_answer("B1", "1"))),
               answers = list(ta_answer("A1"), ta_answer("B1", "1")),
               subquestions = list(ta_sq("S1"), ta_sq("S2")))
  expect_identical(ta_kinds(ta_rows(list(dual))), c(
    "subq", "mc_exclusive", "value", "subq", "mc_exclusive", "value",
    "subq", "value", "subq", "value"
  ))
  # A two-dimensional array: on every cell of the row subquestion.
  two_d <- ta_q(":", "q9", ta_attrs(EXCL("R1")), subquestions = list(
    ta_sq("R1"), ta_sq("R2"), ta_sq("C1", scale = "1"), ta_sq("C2", scale = "1")
  ))
  expect_identical(ta_kinds(ta_rows(list(two_d))), c(
    "subq", "mc_exclusive", "subq", "mc_exclusive", "subq", "subq"
  ))
  # A code that no subquestion has: no row, as on the cards.
  ghost <- ta_q("F", "q10", ta_attrs(EXCL("S9")),
                answers = list(ta_answer("A1")), subquestions = list(ta_sq("S1")))
  expect_identical(ta_kinds(ta_rows(list(ghost))), c("subq", "value"))
})

test_that("table rows: the comment row of a list with comment", {
  q <- ta_q("O", "q2", answers = list(ta_answer("A1")))
  q$texts$en$help <- "Pick one"
  q$texts$fr$help <- "Choisissez"
  rows <- ta_rows(list(q))
  expect_identical(ta_kinds(rows), c("leaf", "value", "leaf"))
  cr <- rows[[3]]
  expect_identical(cr$variable, "q2[_Ccomment]")
  # The label of the card of the comment, and no help of the question.
  expect_identical(cr$field, "Comment")
  expect_null(cr$help)
  expect_identical(cr$parent_text, list(en = "Stem q2", fr = "Libell\u00E9 q2"))
  # The list row keeps its help and has no label of its own.
  expect_null(rows[[1]]$field)
  expect_identical(rows[[1]]$help, list(en = "Pick one", fr = "Choisissez"))
})

test_that("table cell: a part one language lacks shows the empty marker", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  th <- ta_theme()
  mk <- th$empty_marker
  row <- list(
    kind = "subq",
    parent_text = list(en = "Stem", fr = ""),
    subq_text = list(en = "Row", fr = ""),
    help = list(en = "Help", fr = NA_character_)
  )
  en <- ta_chunks(lss_table_question_paragraph(row, "en", th, show_help = TRUE))
  fr <- ta_chunks(lss_table_question_paragraph(row, "fr", th, show_help = TRUE))
  expect_identical(en$txt, c("Stem", "\n", "Row", "\n", "\u00AB Help \u00BB"))
  expect_identical(fr$txt, c(mk, "\n", mk, "\n", mk))
  # Each marker in the style of its part, in the muted colour.
  expect_identical(fr$italic, c(FALSE, FALSE, TRUE, FALSE, TRUE))
  expect_identical(toupper(fr$color[c(1, 3, 5)]),
                   rep(toupper(th$color_muted), 3L))
  # One part only: the others keep their text.
  row$parent_text$fr <- "Tige"
  row$help$fr <- "Aide"
  fr <- ta_chunks(lss_table_question_paragraph(row, "fr", th, show_help = TRUE))
  expect_identical(fr$txt, c("Tige", "\n", mk, "\n", "\u00AB Aide \u00BB"))
  # A part that no language has: no line and no marker.
  row$subq_text <- list(en = "", fr = "")
  row$help <- list(en = "", fr = NULL)
  fr <- ta_chunks(lss_table_question_paragraph(row, "fr", th, show_help = TRUE))
  expect_identical(fr$txt, "Tige")
  # Without show_help, a missing help is not marked.
  row$help <- list(en = "Help", fr = "")
  fr <- ta_chunks(lss_table_question_paragraph(row, "fr", th, show_help = FALSE))
  expect_identical(fr$txt, "Tige")
  # The facet of a dual scale or of a column is part of the subquestion
  # line: with it, the line is never missing.
  row$subq_text <- list(en = "Row", fr = "")
  row$scale_header <- list(en = "Trust", fr = "Confiance")
  en <- ta_chunks(lss_table_question_paragraph(row, "en", th, show_help = FALSE))
  fr <- ta_chunks(lss_table_question_paragraph(row, "fr", th, show_help = FALSE))
  expect_identical(en$txt, c("Stem", "
", "Row (Trust)"))
  expect_identical(fr$txt, c("Tige", "
", "Confiance"))
})

test_that("table: the rows of the parity survey changed in 8.3, cell by cell", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- parity_lss()
  m <- lss_model(lss, TA_LANGS)
  qs <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  names(qs) <- vapply(qs, function(q) q$code, "")
  flat <- function(x) parity_squish(lss_html_to_text(x))
  mk <- ta_theme()$empty_marker
  grid <- parity_table_grid(parity_render(lss, "table", key = "parity"))
  sq <- function(g) parity_squish(g)

  # A4: the Other rows carry the token of a short free text.
  other <- which(grid[, "Type"] == "Text (other)")
  expect_length(other, 2L)
  expect_identical(unname(grid[other, "Value"]), c("[text]", "[text]"))

  # A5 with B2: the comment row of langhome, "Comment" in Field, the stem
  # without the help in the language cells.
  k <- which(grid[, "Variable"] == "langhome[_Ccomment]")
  expect_length(k, 1L)
  expect_identical(unname(grid[k, c("Field", "Value")]), c("Comment", "[text]"))
  for (lg in TA_LANGS) {
    expect_identical(unname(sq(grid[k, lg])), flat(qs$langhome$texts[[lg]]$question),
                     info = lg)
  }
  expect_true(nzchar(flat(qs$langhome$texts$en$help)))

  # A6: a stem, a subquestion and a help missing in French only.
  b <- parity_grid_block(grid, "occupation")
  expect_identical(strsplit(b[1, "fr"], "\n")[[1]][1], mk)
  d1 <- qs$dailyresources$subquestions[[1]]$code
  k <- which(grid[, "Variable"] == sprintf("dailyresources[%s]", d1))
  expect_identical(strsplit(grid[k, "fr"], "\n")[[1]][1:2],
                   c(lss_html_to_text(qs$dailyresources$texts$fr$question), mk))
  k <- min(grep("^nationality\\[", grid[, "Variable"])) - 1L
  expect_identical(unname(grid[k, "Field"]), "Question")
  fr_lines <- strsplit(grid[k, "fr"], "\n")[[1]]
  expect_identical(fr_lines, c(flat(qs$nationality$texts$fr$question), mk))
  expect_match(grid[k, "en"], "\u00BB$")

  # A7: the Exclusive row of the exclusive subquestion of dailyresources,
  # after its Question row and before its Value rows; none on the others.
  d2 <- qs$dailyresources$subquestions[[2]]$code
  for (s in qs$dailyresources$subquestions) {
    b <- parity_grid_block(grid, sprintf("dailyresources[%s]", s$code))
    ex <- which(b[, "Field"] == "Exclusive")
    if (identical(s$code, d2)) {
      expect_identical(ex, 2L)
      expect_identical(unname(b[3, "Field"]), "")
      expect_true(nzchar(b[3, "Value"]))
      expect_identical(unname(b[2, TA_LANGS]), rep(
        "When answered, clears all other answers of dailyresources", 2L))
    } else {
      expect_length(ex, 0L)
    }
  }

  # The two exclusive options of socialmedia, after its option rows.
  codes <- utils::tail(vapply(qs$socialmedia$subquestions, function(s) s$code, ""), 2L)
  k <- which(grid[, "Field"] == "Exclusive" &
               grid[, "en"] == sprintf("%s, %s: when checked, clears all other selections",
                                       codes[1], codes[2]))
  expect_length(k, 1L)
  last_option <- max(grep("^socialmedia\\[", grid[, "Variable"]))
  expect_identical(k, last_option + 1L)
})

test_that("cards: the exclusive sentences of the parity survey", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- parity_lss()
  m <- lss_model(lss, TA_LANGS)
  qs <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  names(qs) <- vapply(qs, function(q) q$code, "")
  doc <- parity_render(lss, "cards", key = "parity")
  label_of <- function(rows) vapply(rows, function(r) r[[1]], "")

  # The multiple choice: one row naming its two options.
  codes <- utils::tail(vapply(qs$socialmedia$subquestions, function(s) s$code, ""), 2L)
  rows <- parity_card_rows(doc, "socialmedia[*]")
  k <- which(label_of(rows) == "Exclusive")
  expect_length(k, 1L)
  expect_identical(rows[[k]][2], sprintf(
    "%s, %s: when checked, clears all other selections", codes[1], codes[2]))

  # The array: a row on the card of the exclusive subquestion only, before
  # its Value rows.
  d2 <- qs$dailyresources$subquestions[[2]]$code
  for (s in qs$dailyresources$subquestions) {
    rows <- parity_card_rows(doc, sprintf("dailyresources[%s]", s$code))
    k <- which(label_of(rows) == "Exclusive")
    if (identical(s$code, d2)) {
      expect_length(k, 1L)
      expect_identical(rows[[k]][2],
                       "When answered, clears all other answers of dailyresources")
      expect_identical(label_of(rows)[k + 1L], "Value")
    } else {
      expect_identical(length(k), 0L, info = s$code)
    }
  }
})

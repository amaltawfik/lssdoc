# The attributes of `show_attrs` (prefix, suffix, validation, the formula of
# a computed question, ...) in the table layout: the rows the cards print
# with lss_attr_rows(), at the same places, one annotation row each.

ta_theme <- function() {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings("en")
  th
}
TA_LANGS <- c("en", "fr")
TA_ATTRS <- c("prefix", "suffix", "other_replace_text", "validation",
              "equation")

ta_txt <- function(en, fr = en) {
  list(en = list(question = en, help = NULL),
       fr = list(question = fr, help = NULL))
}
ta_attrs <- function(...) {
  a <- list(...)
  if (length(a) == 0L) {
    return(data.frame(attribute = character(0), value = character(0),
                      language = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(a, function(x) {
    data.frame(attribute = x[[1]], value = x[[2]], language = x[[3]],
               stringsAsFactors = FALSE)
  }))
}
ta_answer <- function(code, scale = "0") {
  list(code = code, aid = paste0("a", code), scale_id = scale,
       labels = list(en = paste("Label", code), fr = paste("Libell\u00E9", code)))
}
ta_sq <- function(code, scale = "0", attributes = NULL) {
  list(code = code, texts = ta_txt(paste("Row", code), paste("Ligne", code)),
       relevance = "1", scale_id = scale, attributes = attributes)
}
ta_q <- function(type, code, attributes = ta_attrs(), answers = list(),
                 subquestions = list(), scales = NULL, other = "N") {
  list(qid = code, code = code, type = type, mandatory = "N",
       relevance = "1", group_relevance = "1", other = other,
       texts = ta_txt(paste("Stem", code), paste("Libell\u00E9", code)),
       answers = answers, scales = scales, subquestions = subquestions,
       attributes = attributes)
}
# The rows of the table for the questions `qs`, without the group row.
ta_rows <- function(qs, show_attrs = TA_ATTRS) {
  state <- lss_render_state(NULL)
  state$show_attrs <- show_attrs
  lss_table_template_rows_for_group(
    list(questions = qs), TA_LANGS, ta_theme(), show_help = TRUE,
    state = state, show_groups = FALSE
  )
}
ta_kinds <- function(rows) vapply(rows, function(r) r$kind, "")
ta_attr <- function(rows) Filter(function(r) identical(r$kind, "attr"), rows)

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

# The body rows of the dense table, each its w:tr node and the texts of its
# cells (one per w:tc, a merged cell once).
ta_xml_rows <- function(doc) {
  tbls <- xml2::xml_find_all(doc$xml, "//w:tbl[w:tblGrid[count(w:gridCol) = 9]]",
                             parity_w)
  expect_length(tbls, 1L)
  lapply(xml2::xml_find_all(tbls[[1]], "w:tr", parity_w), function(tr) {
    tcs <- xml2::xml_find_all(tr, "w:tc", parity_w)
    list(tcs = tcs, cells = vapply(tcs, parity_node_text, ""))
  })
}
ta_span <- function(tc) {
  v <- xml2::xml_attr(xml2::xml_find_first(tc, "w:tcPr/w:gridSpan", parity_w),
                      "val")
  if (is.na(v)) 1L else as.integer(v)
}
# The run properties of the first run of a cell: italic (a w:i without a
# value, or with a true one), colour, font.
ta_run <- function(tc) {
  rpr <- xml2::xml_find_first(tc, ".//w:r/w:rPr", parity_w)
  i <- xml2::xml_find_first(rpr, "w:i", parity_w)
  list(
    italic = !inherits(i, "xml_missing") &&
      xml2::xml_attr(i, "val") %in% c(NA, "true", "1", "on"),
    color = xml2::xml_attr(xml2::xml_find_first(rpr, "w:color", parity_w), "val"),
    font = xml2::xml_attr(xml2::xml_find_first(rpr, "w:rFonts", parity_w), "ascii")
  )
}

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

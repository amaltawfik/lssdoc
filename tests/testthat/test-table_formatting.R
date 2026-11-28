# The table layout keeps the inline formatting of the texts of the author,
# as the cards do: bold, italic, underline, superscript, subscript, line
# breaks and the items of a list, in the stem, the subquestion, the help,
# an answer label, an attribute, the prompt of an "Other" field and the row
# of a comment variable. One source: lss_compose_chunks(), which
# lss_compose() of the cards reads too. One rule for an italic of the
# author in both layouts: inside a text the layout sets in italic (the
# help, an attribute, the subquestion of the table, the description of a
# group in the cards) it is set upright. The texts are those parity_lss() adds
# (helper-parity.R); each check reads the runs of one cell of the document.

# The leaf cells (w:tc without a nested table) whose text holds `has`; with
# `field`, only those of a row whose first cell reads `field`.
tf_cells <- function(doc, has, field = NULL) {
  tcs <- xml2::xml_find_all(doc$xml, "//w:tc[not(.//w:tbl)]", parity_w)
  hit <- Filter(function(tc) grepl(has, parity_node_text(tc), fixed = TRUE), tcs)
  if (!is.null(field)) {
    hit <- Filter(function(tc) {
      first <- xml2::xml_find_first(tc, "parent::w:tr/w:tc[1]", parity_w)
      identical(parity_node_text(first), field)
    }, hit)
  }
  hit
}

# Is the mark `tag` of a run property on?
tf_on <- function(rpr, tag) {
  n <- xml2::xml_find_first(rpr, paste0("w:", tag), parity_w)
  !inherits(n, "xml_missing") &&
    !xml2::xml_attr(n, "val") %in% c("false", "0", "off", "none")
}

# The runs of a cell: their text ("\n" for a break) and their marks, "b",
# "i", "u", "sup", "sub" joined by commas, "" for none.
tf_runs <- function(tc) {
  rs <- xml2::xml_find_all(tc, ".//w:r", parity_w)
  text <- vapply(rs, function(r) {
    k <- xml2::xml_find_all(r, "w:t | w:br", parity_w)
    paste(ifelse(xml2::xml_name(k) == "br", "\n", xml2::xml_text(k)),
          collapse = "")
  }, "")
  marks <- vapply(rs, function(r) {
    rpr <- xml2::xml_find_first(r, "w:rPr", parity_w)
    va <- xml2::xml_attr(xml2::xml_find_first(rpr, "w:vertAlign", parity_w),
                         "val")
    paste(c(if (tf_on(rpr, "b")) "b", if (tf_on(rpr, "i")) "i",
            if (tf_on(rpr, "u")) "u",
            if (identical(va, "superscript")) "sup",
            if (identical(va, "subscript")) "sub"), collapse = ",")
  }, "")
  data.frame(text = text, marks = marks, stringsAsFactors = FALSE)
}

# The marks of the runs of `tc` whose text is `text` (one value when they
# agree).
tf_marks <- function(tc, text) {
  r <- tf_runs(tc)
  unique(r$marks[r$text == text])
}

# Check the marks of the runs named in `marks` (text = marks) in each cell
# of `tcs`, and their lines when `lines` is given.
tf_expect <- function(tcs, marks, lines = NULL, info = "") {
  expect_true(length(tcs) >= 1L, info = info)
  for (tc in tcs) {
    for (txt in names(marks)) {
      expect_identical(tf_marks(tc, txt), marks[[txt]],
                       info = paste(info, txt))
    }
    if (!is.null(lines)) {
      got <- strsplit(parity_node_text(tc), "\n", fixed = TRUE)[[1]]
      expect_identical(utils::tail(got, length(lines)), lines, info = info)
    }
  }
}

BUL <- "\u2022 "

test_that("lss_compose_chunks() gives the runs of a text over a base style", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  base <- officer::fp_text(font.family = "Arial", font.size = 8,
                           color = "#777777", italic = TRUE)
  html <- "<p>a <em>b</em> <strong>c</strong> x<sup>2</sup></p><ul><li>d<br><u>e</u>H<sub>2</sub></li></ul>"
  chunk_data <- function(cs) do.call(flextable::as_paragraph, cs)[[1]]
  d <- chunk_data(lss_compose_chunks(html, base))
  expect_identical(d$txt, c("a ", "b", " ", "c", " x", "2", "\n", BUL, "d",
                            "\n", "e", "H", "2"))
  # The italic of the author set upright inside the italic base; the break
  # between two blocks and the bullet upright, the break inside a block in
  # the style of its run.
  expect_identical(d$italic, c(TRUE, FALSE, TRUE, TRUE, TRUE, TRUE, FALSE,
                               FALSE, TRUE, TRUE, TRUE, TRUE, TRUE))
  expect_identical(d$bold, c(FALSE, FALSE, FALSE, TRUE, rep(FALSE, 9L)))
  expect_identical(d$underlined, c(rep(FALSE, 10L), TRUE, FALSE, FALSE))
  expect_identical(d$vertical.align[c(5, 6, 12, 13)],
                   c("baseline", "superscript", "baseline", "subscript"))
  expect_true(all(d$font.family == "Arial"))
  expect_true(all(d$font.size == 8))
  expect_true(all(d$color == "#777777"))
  # Upright base: the italic of the author is italic.
  up <- officer::fp_text(font.family = "Arial", font.size = 8)
  d <- chunk_data(lss_compose_chunks(html, up))
  expect_identical(d$italic[d$txt %in% c("a ", "b")], c(FALSE, TRUE))
  # Nothing to print: no chunk.
  expect_identical(lss_compose_chunks("", base), list())
  expect_identical(lss_compose_chunks(NA_character_, base), list())
})

test_that("the stem keeps its formatting in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  marks <- list(health = "b", "in general" = "i", today = "u", "3" = "sup",
                "2" = "sub")
  lines <- c("How is your health in general,", "today?",
             paste0(BUL, "Lung volume in dm3"), paste0(BUL, "Blood CO2 level"))
  for (tpl in c("cards", "table")) {
    doc <- parity_render(parity_lss(), tpl, key = "parity")
    tf_expect(tf_cells(doc, "How is your health"), marks, lines, info = tpl)
  }
})

test_that("an answer label keeps its formatting in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  marks <- list(Very = "b", good = "i", health = "u", "1" = "sup", "2" = "sub")
  lines <- c("Very good health1", "H2O", paste0(BUL, "no pain"))
  for (tpl in c("cards", "table")) {
    doc <- parity_render(parity_lss(), tpl, key = "parity")
    tcs <- tf_cells(doc, "Very good health1")
    expect_length(tcs, 1L)
    tf_expect(tcs, marks, lines, info = tpl)
  }
})

test_that("the subquestion keeps its formatting, upright inside the italic of the table", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  has <- "Left out of the H2O talka"
  lines <- c(has, "at work", paste0(BUL, "or at home"))
  # Cards: the Subquestion row, upright.
  doc <- parity_render(parity_lss(), "cards", key = "parity")
  tf_expect(tf_cells(doc, has, field = "Subquestion"),
            list(Left = "b", out = "i", the = "u", "2" = "sub", a = "sup"),
            lines, info = "cards")
  # Table: the subquestion line of the Question cell, in italic; the italic
  # of the author set upright.
  doc <- parity_render(parity_lss(), "table", key = "parity")
  tcs <- tf_cells(doc, has)
  expect_length(tcs, 1L)
  tf_expect(tcs, list(Left = "b,i", out = "", the = "i,u", "2" = "i,sub",
                      a = "i,sup"), info = "table")
  got <- strsplit(parity_node_text(tcs[[1]]), "\n", fixed = TRUE)[[1]]
  expect_identical(got[2:4], lines)
})

test_that("the help keeps its formatting, its italic upright in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lines <- c("Think of the last four weeks1.", "Not CO2.",
             paste0(BUL, "Count every day"), paste0(BUL, "Even short moments"))
  # The help is in italic in both layouts: the italic of the author is
  # set upright.
  marks <- list("last four weeks" = "", "1" = "i,sup", "2" = "i,sub",
                every = "b,i", short = "i,u")
  # Cards: the Help row of each subquestion card.
  doc <- parity_render(parity_lss(), "cards", key = "parity")
  tf_expect(tf_cells(doc, "Think of the last four weeks1.", field = "Help"),
            marks, lines, info = "cards")
  # Table: the help between guillemets in the cell of each subquestion.
  doc <- parity_render(parity_lss(), "table", key = "parity")
  tcs <- tf_cells(doc, "Think of the last four weeks1.")
  expect_length(tcs, 3L)
  lines[1] <- paste0("\u00AB ", lines[1])
  lines[4] <- paste0(lines[4], " \u00BB")
  tf_expect(tcs, marks, lines, info = "table")
})

test_that("an attribute keeps its formatting in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lines <- c("kg net body m3 CO2", "rounded", paste0(BUL, "no clothes"))
  # An attribute row is in italic in both layouts: the italic of the
  # author is set upright.
  marks <- list(kg = "b,i", net = "", body = "i,u", "3" = "i,sup",
                "2" = "i,sub")
  for (tpl in c("cards", "table")) {
    doc <- parity_render(parity_lss(), tpl, key = "parity")
    tcs <- tf_cells(doc, "kg net body", field = "Suffix")
    expect_length(tcs, 1L)
    tf_expect(tcs, marks, lines, info = tpl)
  }
})

test_that("the description of a group keeps its formatting in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  has <- "These questions are about your health and habits."
  # Cards: the description is in italic, the italic of the author upright;
  # with two languages in the names block of the group, with one in the
  # block of its language.
  cards <- list(c("en", "fr"), "en")
  for (langs in cards) {
    doc <- parity_render(parity_lss(), "cards", languages = langs,
                         key = "parity")
    tcs <- tf_cells(doc, has)
    expect_length(tcs, 1L)
    tf_expect(tcs, list(health = "b,i", habits = ""), has,
              info = paste("cards", length(langs)))
  }
  # Table: the description row is upright, the italic of the author in
  # italic.
  doc <- parity_render(parity_lss(), "table", key = "parity")
  tcs <- tf_cells(doc, has)
  expect_length(tcs, 1L)
  tf_expect(tcs, list(health = "b", habits = "i"), has, info = "table")
})

test_that("the prompt of an Other field keeps its formatting in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  marks <- list(Other = "b", role = "i", here = "u", "1" = "sup", "2" = "sub")
  lines <- c("Other role here1 H2O", "specify:")
  for (tpl in c("cards", "table")) {
    doc <- parity_render(parity_lss(), tpl, key = "parity")
    tcs <- tf_cells(doc, "Other role here1")
    expect_length(tcs, 1L)
    tf_expect(tcs, marks, lines, info = tpl)
  }
})

test_that("an option and the row of its comment variable keep the formatting", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  marks <- list(Face = "b", book = "u", "1" = "sup", or = "i", "2" = "sub")
  lines <- c("Facebook1", "or H2O")
  # Cards: the row of the option on the card of the question.
  doc <- parity_render(parity_lss(), "cards", key = "parity")
  tcs <- tf_cells(doc, "Facebook1\nor H2O")
  expect_length(tcs, 1L)
  tf_expect(tcs, marks, lines, info = "cards")
  # Table: the row of the option and the row of its comment variable.
  doc <- parity_render(parity_lss(), "table", key = "parity")
  tcs <- tf_cells(doc, "Facebook1\nor H2O")
  expect_length(tcs, 2L)
  expect_length(tf_cells(doc, "Facebook1\nor H2O", field = "Comment"), 1L)
  tf_expect(tcs, marks, lines, info = "table")
})

test_that("a formula is printed as written, never read as HTML", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- demo_lss()
  imc <- lss$questions$qid[lss$questions$title == "imc"]
  lss$questions <- lss$questions[lss$questions$qid == imc, , drop = FALSE]
  qa <- lss$question_attributes
  qa$value[qa$qid == imc & qa$attribute == "equation"] <- "{x + '<b>1</b>'}"
  lss$question_attributes <- qa
  formula <- "x + '<b>1</b>'"
  for (tpl in c("cards", "table")) {
    tcs <- tf_cells(parity_render(lss, tpl), formula, field = "Equation")
    expect_true(length(tcs) >= 1L, info = tpl)
    for (tc in tcs) {
      r <- tf_runs(tc)
      expect_identical(r$text, formula, info = tpl)
      expect_identical(r$marks, "", info = tpl)
    }
  }
})

test_that("table cell: a prompt that prints nothing gives the empty marker", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  th <- ta_theme()
  q <- ta_q("L", "q1", attributes = ta_attrs(
    list("other_replace_text", "<p> </p>", "en")
  ))
  row <- list(kind = "other", other_q = q)
  p <- ta_chunks(lss_table_question_paragraph(row, "en", th, show_help = TRUE))
  expect_identical(p$txt, th$empty_marker)
  # The cards print the marker for the same prompt (lss_compose()).
  expect_identical(lss_compose("<p> </p>", th)[[1]]$txt, th$empty_marker)
})

test_that("lss_compose_chunks() numbers the items of an ordered list", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  base <- officer::fp_text(font.family = "Arial", font.size = 8)
  chunk_txt <- function(html) {
    do.call(flextable::as_paragraph, lss_compose_chunks(html, base))[[1]]$txt
  }
  expect_identical(
    chunk_txt("<ol><li>a</li><li>b<ul><li>x</li></ul></li><li>c</li></ol>"),
    c("1. ", "a", "\n", "2. ", "b", "\n", paste0("  ", BUL), "x", "\n",
      "3. ", "c")
  )
  expect_identical(chunk_txt('<ol start="7"><li>a</li><li>b</li></ol>'),
                   c("7. ", "a", "\n", "8. ", "b"))
  expect_identical(
    chunk_txt("<ul><li>a<ol><li>x</li><li>y</li></ol></li></ul>"),
    c(BUL, "a", "\n", "  1. ", "x", "\n", "  2. ", "y")
  )
})

test_that("an ordered list prints its numbers in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  en <- c("1. Stand straight", "2. Take off", paste0("  ", BUL, "your shoes"),
          paste0("  ", BUL, "your hat"), "3. Read the scale")
  fr <- c("1. Tenez-vous droit", "2. Retirez",
          paste0("  ", BUL, "vos chaussures"),
          paste0("  ", BUL, "votre chapeau"), "3. Lisez la toise")
  marks <- list("1. " = "", straight = "b,i", "  \u2022 " = "")
  # Cards: the Help row of the card of height, one cell per language.
  doc <- parity_render(parity_lss(), "cards", key = "parity")
  tcs <- tf_cells(doc, "Stand straight", field = "Help")
  expect_length(tcs, 1L)
  tf_expect(tcs, marks, en, info = "cards en")
  tcs <- tf_cells(doc, "Tenez-vous droit", field = "Help")
  expect_length(tcs, 1L)
  tf_expect(tcs, list(droit = "b,i"), fr, info = "cards fr")
  # Table: the help between guillemets in the cell of height.
  doc <- parity_render(parity_lss(), "table", key = "parity")
  tcs <- tf_cells(doc, "Stand straight")
  expect_length(tcs, 1L)
  en[1] <- paste0("\u00AB ", en[1])
  en[5] <- paste0(en[5], " \u00BB")
  tf_expect(tcs, list(straight = "b,i"), en, info = "table en")
  tcs <- tf_cells(doc, "Tenez-vous droit")
  expect_length(tcs, 1L)
  fr[1] <- paste0("\u00AB ", fr[1])
  fr[5] <- paste0(fr[5], " \u00BB")
  tf_expect(tcs, list(droit = "b,i"), fr, info = "table fr")
  # The row of the cell is the Question row of height.
  grid <- parity_table_grid(doc)
  i <- grep("Stand straight", grid[, "en"], fixed = TRUE)
  expect_identical(grid[i, c("Field", "Variable")],
                   c(Field = "Question", Variable = "height"))
})

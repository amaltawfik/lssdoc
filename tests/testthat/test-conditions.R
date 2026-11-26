# The conditions LimeSurvey holds on a group and on a subquestion, printed by
# both renderers next to the condition of the question.
#
# No shipped fixture holds such a condition: they are set on a copy of the
# parsed demo survey, in the form LimeSurvey writes them. Group 3 ("Resources,
# constraints and daily life") is shown only to respondents aged 18 or more.
# Inside it, incomedecile has its own filter, and so do the row TIME of the
# array dailyresources and the option 1 of the multiple choice supportsources.
# The `;` array mediaquality (group 5) has a column with a condition, the `:`
# array adaptability (group 4) a row and a column with one. Group 6 has a
# condition that cannot be simplified. The questions the tests do not read are
# dropped to keep the renders short.

cond_w <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

cond_lss <- function() {
  lss <- demo_lss()
  m <- lss_model(lss)
  gid <- function(i) m$groups[[i]]$gid
  qid <- function(code) lss$questions$qid[lss$questions$title == code]
  set_sq <- function(parent, code, rel, scale = NULL) {
    hit <- lss$subquestions$parent_qid == qid(parent) &
      lss$subquestions$title == code
    if (!is.null(scale)) hit <- hit & lss$subquestions$scale_id == scale
    lss$subquestions$relevance[hit] <<- rel
  }
  lss$groups$grelevance[lss$groups$gid == gid(3)] <- "((age.NAOK >= 18))"
  lss$groups$grelevance[lss$groups$gid == gid(6)] <-
    "((count(healthself.NAOK, smoking.NAOK) > 1))"
  lss$questions$relevance[lss$questions$title == "incomedecile"] <-
    "((incomefeel.NAOK == \"4\"))"
  set_sq("dailyresources", "TIME", "((age.NAOK > 25))")
  set_sq("supportsources", "1", "((gender.NAOK == \"F\"))")
  set_sq("mediaquality", cond_col("mediaquality"), "((age.NAOK < 30))", "1")
  set_sq("adaptability", "NEW", "((age.NAOK > 20))", "0")
  set_sq("adaptability", cond_col("adaptability"), "((age.NAOK < 60))", "1")
  # Only the questions the tests read, so that a render takes seconds: the
  # six groups stay (group 2 empty), the other questions go.
  keep <- c("respondentrole", "age", "nationality", "langhome", "incomefeel",
            "incomedecile", "dailyresources", "supportsources", "workhours",
            "adaptability", "devicerank", "mediaquality", "surveyinterest")
  lss$questions <- lss$questions[lss$questions$title %in% keep, , drop = FALSE]
  lss
}

# The code of the first column (scale 1) of a two-dimensional array.
cond_col <- function(code) {
  lss <- demo_lss()
  q <- lss$questions$qid[lss$questions$title == code]
  sq <- lss$subquestions[lss$subquestions$parent_qid == q &
                           lss$subquestions$scale_id == "1", , drop = FALSE]
  sq$title[order(as.integer(sq$question_order))][1]
}

# The text of a table cell: its paragraphs joined by "\n", a line break
# as "\n".
cond_cell_text <- function(tc) {
  paras <- xml2::xml_find_all(tc, ".//w:p", cond_w)
  paste(vapply(paras, function(p) {
    bits <- xml2::xml_find_all(p, ".//w:r/w:t | .//w:r/w:br", cond_w)
    paste(ifelse(xml2::xml_name(bits) == "br", "\n", xml2::xml_text(bits)),
          collapse = "")
  }, ""), collapse = "\n")
}

# Every body row of every table: the texts of its cells.
cond_rows <- function(doc) {
  lapply(xml2::xml_find_all(doc, "//w:tbl/w:tr", cond_w), function(tr) {
    vapply(xml2::xml_find_all(tr, "w:tc", cond_w), cond_cell_text, "")
  })
}

# The text of each body paragraph (outside tables).
cond_paras <- function(doc) {
  vapply(xml2::xml_find_all(doc, "//w:body/w:p", cond_w), function(p) {
    bits <- xml2::xml_find_all(p, ".//w:r/w:t | .//w:r/w:br", cond_w)
    paste(ifelse(xml2::xml_name(bits) == "br", "\n", xml2::xml_text(bits)),
          collapse = "")
  }, "")
}

cond_render <- function(lss, ..., languages = "en") {
  out <- tempfile(fileext = ".docx")
  dir <- tempfile()
  on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
  suppressMessages(suppressWarnings(
    render_documentation(lss, out, languages = languages, chrome_lang = "en",
                         show_audit = FALSE, ...)
  ))
  utils::unzip(out, exdir = dir)
  xml2::read_xml(file.path(dir, "word", "document.xml"))
}

# The cells of the first row whose cell `j` is `value`.
cond_row_of <- function(rows, j, value) {
  hit <- Filter(function(r) length(r) >= j && identical(r[[j]], value), rows)
  if (length(hit) == 0L) return(NULL)
  hit[[1]]
}

ge <- "\u2265"

# ---- Model ------------------------------------------------------------------

test_that("the model carries the condition of a group to its questions", {
  m <- lss_model(cond_lss())
  g3 <- m$groups[[3]]
  expect_identical(g3$relevance, "((age.NAOK >= 18))")
  expect_true(all(vapply(g3$questions, function(q) {
    identical(q$group_relevance, "((age.NAOK >= 18))")
  }, logical(1))))
  expect_identical(m$groups[[1]]$relevance, "1")
  expect_identical(m$groups[[1]]$questions[[1]]$group_relevance, "1")

  lss <- demo_lss()
  lss$groups$grelevance <- NULL
  m <- lss_model(lss)
  expect_identical(m$groups[[1]]$relevance, NA_character_)
  expect_identical(m$groups[[1]]$questions[[1]]$group_relevance, NA_character_)
})

# ---- lss_filter_lines() -----------------------------------------------------

test_that("lss_has_condition() reads every form of 'no condition'", {
  for (x in list(NULL, character(0), NA, NA_character_, "", "  ", "1",
                 "((1))", " ( 1 ) ")) {
    expect_false(lss_has_condition(x), info = deparse(x))
  }
  expect_true(lss_has_condition("((age.NAOK >= 18))"))
})

test_that("lss_filter_lines() keeps the one unlabelled line without group or subquestion condition", {
  for (x in list(NA_character_, "", "1", NULL)) {
    expect_identical(lss_filter_lines(x),
                     list(list(label = NULL, plain = "All", raw = "1")))
  }
  # "((1))" prints as it always did.
  old <- lss_humanize_relevance(lss_strip_outer_parens("((1))"))
  expect_identical(lss_filter_lines("((1))")[[1]]$plain, old)
  expect_identical(
    lss_filter_lines("((x.NAOK == 2))", group_relevance = "1",
                     subq_relevance = c("", NA, "((1))")),
    list(list(label = NULL, plain = "x = 2", raw = "x.NAOK == 2"))
  )
})

test_that("lss_filter_lines() labels one line per level that has a condition", {
  g <- "((age.NAOK >= 18))"
  q <- "((x.NAOK == \"1\"))"
  s <- "((y.NAOK == 2))"
  labels <- function(lines) vapply(lines, function(l) l$label, "")
  plains <- function(lines) vapply(lines, function(l) l$plain, "")

  only_g <- lss_filter_lines("1", g)
  expect_identical(labels(only_g), "Group")
  expect_identical(plains(only_g), paste("age", ge, "18"))
  expect_identical(only_g[[1]]$raw, "age.NAOK >= 18")

  gq <- lss_filter_lines(q, g)
  expect_identical(labels(gq), c("Group", "Question"))
  expect_identical(plains(gq), c(paste("age", ge, "18"), "x = 1"))

  only_s <- lss_filter_lines("", NULL, s)
  expect_identical(labels(only_s), "Subquestion")
  expect_identical(plains(only_s), "y = 2")

  all3 <- lss_filter_lines(q, g, s)
  expect_identical(labels(all3), c("Group", "Question", "Subquestion"))

  two <- lss_filter_lines(NA, NULL, c(s, "((z.NAOK == 3))"))
  expect_identical(labels(two), c("Subquestion", "Subquestion"))
  expect_identical(plains(two), c("y = 2", "z = 3"))

  fr <- list(chrome = lss_chrome_strings("fr"))
  expect_identical(labels(lss_filter_lines(q, g, s, fr)),
                   c("Groupe", "Question", "Sous-question"))
})

test_that("the new chrome keys exist in five languages", {
  for (lg in c("en", "fr", "de", "es", "it")) {
    pack <- lss_chrome_strings(lg)
    for (k in c("filter_level_fmt", "q_group_filter_fmt", "q_group_filtered")) {
      expect_true(is.character(pack[[k]]) && nzchar(pack[[k]]), info = paste(lg, k))
    }
  }
  expect_identical(lss_chrome_strings("fr")$filter_level_fmt, "%s\u00A0: %s")
  expect_identical(lss_chrome_strings("fr")$q_group_filter_fmt,
                   "Groupe affich\u00E9 si %s")
})

# ---- render_documentation(), cards ------------------------------------------

test_that("cards: the Filter cell carries the group and subquestion lines", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- cond_render(cond_lss())
  rows <- cond_rows(doc)
  filter_of <- function(var) cond_row_of(rows, 2L, var)[[5]]
  g_line <- paste("Group:", "age", ge, "18")

  # A question in the conditional group no longer says "All".
  expect_identical(filter_of("incomefeel"), g_line)
  # A question with its own filter in that group: two labelled lines.
  expect_identical(filter_of("incomedecile"),
                   paste0(g_line, "\nQuestion: incomefeel = 4"))
  # The card of an array row with a condition.
  expect_identical(filter_of("dailyresources[TIME]"),
                   paste0(g_line, "\nSubquestion: age > 25"))
  # A cell of a two-dimensional array: its row and its column.
  col <- cond_col("adaptability")
  expect_identical(filter_of(paste0("adaptability[NEW_", col, "]")),
                   "Subquestion: age > 20\nSubquestion: age < 60")
  # Outside the group, nothing changes.
  expect_identical(filter_of("age"), "All")
  expect_identical(filter_of("workhours"), "workstatus = \"1\"")

  # The family card of the multiple choice: the note row, before nothing
  # else of its kind, holding the option code.
  note <- cond_row_of(rows, 1L, "Filter")
  expect_false(is.null(note))
  expect_identical(note[[2]], "1: gender = F")
  expect_identical(filter_of("supportsources[*]"), g_line)

  # The paragraph right under the group heading.
  paras <- cond_paras(doc)
  k <- which(paras == paste("Filter:", "age", ge, "18"))
  expect_length(k, 1L)
  expect_identical(paras[k - 1L], "Resources, constraints and daily life")
  # Group 6: the expression is printed as it is humanized.
  expect_true(any(startsWith(paras, "Filter: count(")))
})

test_that("cards: show_raw_filter adds the raw form under each line", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- cond_render(cond_lss(), show_raw_filter = TRUE)
  rows <- cond_rows(doc)
  filter_of <- function(var) cond_row_of(rows, 2L, var)[[5]]
  expect_identical(
    filter_of("incomedecile"),
    paste0("Group: age ", ge, " 18\nage.NAOK >= 18\n",
           "Question: incomefeel = 4\nincomefeel.NAOK == \"4\"")
  )
  # The one unlabelled line keeps today's raw line.
  expect_identical(filter_of("age"), "All\n1")
  note <- cond_row_of(rows, 1L, "Filter")
  expect_identical(note[[2]], "1: gender = F\ngender.NAOK == \"F\"")
  paras <- cond_paras(doc)
  expect_true(paste0("Filter: age ", ge, " 18\nage.NAOK >= 18") %in% paras)
})

test_that("cards: show_groups = FALSE keeps the group line and drops the paragraph", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- cond_render(cond_lss(), show_groups = FALSE)
  rows <- cond_rows(doc)
  expect_identical(cond_row_of(rows, 2L, "incomefeel")[[5]],
                   paste("Group:", "age", ge, "18"))
  expect_false(any(startsWith(cond_paras(doc), "Filter:")))
})

# ---- render_documentation(), table ------------------------------------------

test_that("table: the group_filter row, the Filter cells and the bookmarks", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- cond_render(cond_lss(), template = "table")
  rows <- cond_rows(doc)
  g_line <- paste("Group:", "age", ge, "18")
  filter_of <- function(var) cond_row_of(rows, 3L, var)[[6]]

  # The group_filter row: right under the group row, before the
  # description, on its own.
  k <- which(vapply(rows, function(r) {
    any(r == paste("Filter:", "age", ge, "18"))
  }, logical(1)))
  expect_length(k, 1L)
  expect_identical(rows[[k - 1L]][[1]], "Group")
  expect_true(any(grepl("Resources, constraints and daily life",
                        rows[[k - 1L]], fixed = TRUE)))
  expect_identical(rows[[k]][[1]], "")

  # Question rows and subquestion rows.
  expect_identical(filter_of("incomefeel"), g_line)
  expect_identical(filter_of("incomedecile"),
                   paste0(g_line, "\nQuestion: incomefeel = 4"))
  expect_identical(filter_of("dailyresources[TIME]"),
                   paste0(g_line, "\nSubquestion: age > 25"))
  col <- cond_col("adaptability")
  expect_identical(filter_of(paste0("adaptability[NEW_", col, "]")),
                   "Subquestion: age > 20\nSubquestion: age < 60")
  expect_identical(filter_of("age"), "All")

  # Option rows of the multiple choice: the one line of an option with a
  # condition, nothing for the others; the parent row has the group line.
  expect_identical(filter_of("supportsources[1]"), "Subquestion: gender = F")
  expect_identical(filter_of("supportsources[2]"), "")
  i <- which(vapply(rows, function(r) {
    length(r) >= 3L && identical(r[[3]], "supportsources[1]")
  }, logical(1)))
  expect_identical(rows[[i - 1L]][[6]], g_line)

  # Group bookmarks still sit on the rows that hold the group names.
  trs <- xml2::xml_find_all(doc, "//w:tbl/w:tr", cond_w)
  marked <- Filter(function(tr) {
    length(xml2::xml_find_all(
      tr, ".//w:bookmarkStart[starts-with(@w:name, 'lssdoc_group_')]", cond_w
    )) > 0L
  }, trs)
  expect_length(marked, 6L)
  firsts <- vapply(marked, function(tr) {
    cond_cell_text(xml2::xml_find_first(tr, "w:tc", cond_w))
  }, "")
  expect_true(all(firsts == "Group"))
})

test_that("table: raw forms, and show_groups = FALSE", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- cond_render(cond_lss(), template = "table", show_raw_filter = TRUE)
  rows <- cond_rows(doc)
  filter_of <- function(var) cond_row_of(rows, 3L, var)[[6]]
  expect_identical(filter_of("supportsources[1]"),
                   "Subquestion: gender = F\ngender.NAOK == \"F\"")
  expect_identical(filter_of("supportsources[2]"), "")
  expect_true(any(vapply(rows, function(r) {
    any(r == paste0("Filter: age ", ge, " 18\nage.NAOK >= 18"))
  }, logical(1))))

  doc <- cond_render(cond_lss(), template = "table", show_groups = FALSE)
  rows <- cond_rows(doc)
  expect_identical(cond_row_of(rows, 3L, "incomefeel")[[6]],
                   paste("Group:", "age", ge, "18"))
  expect_false(any(vapply(rows, function(r) {
    any(startsWith(r, "Filter:"))
  }, logical(1))))
})

# ---- render_documentation(), two languages ----------------------------------

# Where a row of a table puts its cells: for each cell, its text, the first
# grid column it covers and how many it covers (w:gridSpan, 1 by default).
cond_cell_spans <- function(tr) {
  tcs <- xml2::xml_find_all(tr, "w:tc", cond_w)
  span <- vapply(tcs, function(tc) {
    v <- xml2::xml_attr(xml2::xml_find_first(tc, "w:tcPr/w:gridSpan", cond_w),
                        "w:val", cond_w)
    if (is.na(v)) 1L else as.integer(v)
  }, integer(1))
  data.frame(text = vapply(tcs, cond_cell_text, ""),
             start = cumsum(c(1L, span))[seq_along(span)], span = span,
             stringsAsFactors = FALSE)
}

# The first row of every table whose cell texts satisfy `pred`.
cond_tr_where <- function(doc, pred) {
  for (tr in xml2::xml_find_all(doc, "//w:tbl/w:tr", cond_w)) {
    if (pred(vapply(xml2::xml_find_all(tr, "w:tc", cond_w), cond_cell_text, ""))) {
      return(tr)
    }
  }
  NULL
}

# The condition spans the two language columns: one cell, starting at the
# column headed "English" and covering it and the French one.
cond_expect_spans_languages <- function(tr, text) {
  tbl <- xml2::xml_find_first(tr, "ancestor::w:tbl[1]", cond_w)
  n_grid <- length(xml2::xml_find_all(tbl, "w:tblGrid/w:gridCol", cond_w))
  head <- cond_cell_spans(xml2::xml_find_first(tbl, "w:tr", cond_w))
  en <- head$start[head$text == "English"]
  expect_length(en, 1L)
  expect_identical(head$start[head$text == "Fran\u00e7ais"], en + 1L)
  cells <- cond_cell_spans(tr)
  hit <- cells[grepl(text, cells$text, fixed = TRUE), , drop = FALSE]
  expect_identical(nrow(hit), 1L)
  expect_identical(hit$text, text)
  expect_identical(hit$start, en)
  expect_identical(hit$span, 2L)
  expect_identical(sum(cells$span), n_grid)
}

test_that("two languages: a condition prints once, across the language columns", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- cond_lss()

  # Cards: the note row of the multiple choice.
  doc <- cond_render(lss, languages = c("en", "fr"))
  note <- cond_tr_where(doc, function(t) length(t) && identical(t[[1]], "Filter"))
  expect_false(is.null(note))
  expect_identical(cond_cell_spans(note)$text, c("Filter", "1: gender = F"))
  cond_expect_spans_languages(note, "1: gender = F")

  # Table: the row of the group condition.
  doc <- cond_render(lss, template = "table", languages = c("en", "fr"))
  g_text <- paste("Filter:", "age", ge, "18")
  row <- cond_tr_where(doc, function(t) any(t == g_text))
  expect_false(is.null(row))
  cond_expect_spans_languages(row, g_text)
  cells <- cond_cell_spans(row)
  expect_true(all(cells$text[cells$text != g_text] == ""))
})

# ---- render_questionnaire() -------------------------------------------------

test_that("questionnaire: the group paragraph and the filters of subquestions", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- cond_lss()
  out <- tempfile(fileext = ".docx")
  dir <- tempfile()
  on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
  suppressMessages(render_questionnaire(lss, out, language = "en"))
  utils::unzip(out, exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  ps <- xml2::xml_find_all(doc, "//w:body/w:p", cond_w)
  paras <- cond_paras(doc)

  # The group paragraph, under the heading, before the description, not in
  # the heading style.
  k <- which(paras == paste("Group shown if age", ge, "18"))
  expect_length(k, 1L)
  expect_identical(paras[k - 1L], "Resources, constraints and daily life")
  style <- function(p) {
    xml2::xml_attr(xml2::xml_find_first(p, "w:pPr/w:pStyle", cond_w),
                   "w:val", cond_w)
  }
  expect_identical(style(ps[[k - 1L]]), "lssdocQuestionGroup")
  expect_false(identical(style(ps[[k]]), "lssdocQuestionGroup"))
  # Its other form, when the expression cannot be simplified.
  expect_true("Group with a filter" %in% paras)

  # After a row of an array and after an option of a multiple choice.
  expect_true(any(grepl("\u00B7 if age > 25$", paras)))
  expect_true(any(grepl("\u00B7 if gender = F$", paras)))
  # The rows without a condition print no filter.
  expect_identical(sum(grepl("\u00B7 if age > 25", paras)), 1L)
  # A column of a ";" array: "label (filter)" after "For each row:".
  m <- lss_model(lss, "en")
  mq <- Filter(function(q) identical(q$code, "mediaquality"),
               unlist(lapply(m$groups, `[[`, "questions"), recursive = FALSE))[[1]]
  col <- Filter(function(s) identical(s$scale_id, "1"), mq$subquestions)[[1]]
  lab <- lss_q_clean(col$texts$en$question)
  expect_true(any(grepl(paste0(lab, " (if age < 30)"), paras, fixed = TRUE)))

  # Nothing of this without the codes.
  out2 <- tempfile(fileext = ".docx")
  dir2 <- tempfile()
  on.exit(unlink(c(out2, dir2), recursive = TRUE), add = TRUE)
  suppressMessages(render_questionnaire(lss, out2, language = "en",
                                        show_codes = FALSE))
  utils::unzip(out2, exdir = dir2)
  paras2 <- cond_paras(xml2::read_xml(file.path(dir2, "word", "document.xml")))
  expect_false(any(grepl("Group shown if|Group with a filter|if age|if gender",
                         paras2)))
  expect_true(any(grepl(lab, paras2, fixed = TRUE)))
})

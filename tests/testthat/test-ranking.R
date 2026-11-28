# The ranking question (type R) in the form LimeSurvey 7.12.1 gives it: its
# items are subquestions, its data has one column per rank, `Q[1]`, `Q[2]`
# and so on, as many as the items and at most `max_subquestions`. A file that
# holds the items as answer options is brought to that form in the model
# layer by lss_ranking_as_subquestions(). The fixtures are written by
# rk_xml() (helper-ranking.R).

rk_question <- function(m, code = "rk") {
  qs <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  qs[[which(vapply(qs, function(q) identical(q$code, code), logical(1)))]]
}

# The model without the ids of the items, which differ from form to form.
rk_model_sans_ids <- function(lss) {
  m <- lss_model(lss)
  m$file <- NULL
  m$groups <- lapply(m$groups, function(g) {
    g$questions <- lapply(g$questions, function(q) {
      q$subquestions <- lapply(q$subquestions, function(s) {
        s$qid <- NULL
        s
      })
      q
    })
    g
  })
  m
}

# The rows of the table layout from the Question row of `no` to the row
# before the next Question row.
rk_table_block <- function(doc, no = "1") {
  grid <- parity_table_grid(doc)
  i <- which(grid[, "Field"] == "Question" & grid[, "No"] == no)
  heads <- which(grid[, "Field"] %in% c("Question", "Group"))
  nxt <- heads[heads > i]
  grid[i:(if (length(nxt)) nxt[1] - 1L else nrow(grid)), , drop = FALSE]
}

# The texts of a readable questionnaire, one entry per paragraph or cell.
rk_questionnaire_text <- function(lss, language = "en") {
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(render_questionnaire(lss, out, language = language))
  s <- officer::docx_summary(officer::read_docx(out))
  s$text[!is.na(s$text) & nzchar(s$text)]
}

# ---- lss_ranking_as_subquestions() ----------------------------------------

test_that("the items of a ranking stored as answer options become subquestions", {
  lss <- rk_lss("answers")
  out <- lss_ranking_as_subquestions(lss)
  sq <- out$subquestions
  expect_identical(sq$qid, c("-101", "-102", "-103"))
  expect_identical(sq$parent_qid, rep("10", 3))
  expect_identical(sq$title, c("A", "B", "C"))
  expect_identical(sq$question_order, c("1", "2", "3"))
  expect_identical(sq$scale_id, rep("0", 3))
  expect_identical(sq$type, rep("R", 3))
  expect_identical(sq$relevance, rep("1", 3))
  expect_identical(sq$mandatory, rep("N", 3))
  expect_identical(sq$sid, rep("123456", 3))
  expect_identical(sq$gid, rep("1", 3))
  # One text per answer text, the answer text as the question, empty help
  # and script.
  ql <- out$question_l10ns[out$question_l10ns$qid %in% sq$qid, , drop = FALSE]
  expect_identical(ql$qid, rep(c("-101", "-102", "-103"), each = 2))
  expect_identical(ql$language, rep(c("en", "fr"), 3))
  expect_identical(ql$question[1:4], c("Apples", "Pommes",
                                       "<strong>Bread</strong> rolls",
                                       "Petits <strong>pains</strong>"))
  expect_identical(unique(ql$help), "")
  expect_identical(unique(ql$script), "")
  # The texts of the other questions are kept, in their place.
  expect_identical(out$question_l10ns[seq_len(nrow(lss$question_l10ns)), ],
                   lss$question_l10ns)
  # The answer rows and their texts are gone; the attributes are untouched.
  expect_identical(nrow(out$answers), 0L)
  expect_identical(nrow(out$answer_l10ns), 0L)
  expect_identical(out$question_attributes, lss$question_attributes)
  # No id of a converted row is one a real row can have.
  expect_false(any(sq$qid %in% c(lss$questions$qid, lss$answers$aid)))
})

test_that("an item without a text in one language gets no text in it", {
  out <- lss_ranking_as_subquestions(rk_lss("answers", drop = "B"))
  ql <- out$question_l10ns
  expect_identical(ql$language[ql$qid == "-102"], "en")
  q <- rk_question(lss_model(rk_lss("answers", drop = "B")))
  expect_identical(q$subquestions[[2]]$texts$fr$question, NA_character_)
})

test_that("in a file with both forms, the converted items join the subquestions", {
  lss <- rk_lss("both")
  expect_identical(lss$subquestions$title, "A")
  out <- lss_ranking_as_subquestions(lss)
  expect_identical(out$subquestions$qid, c("21", "-102", "-103"))
  expect_identical(out$subquestions$title, c("A", "B", "C"))
  # The new rows take the columns of the table; a column the converter does
  # not fill stays empty on them, and the row of the file keeps its values.
  expect_true(all(names(lss$subquestions) %in% names(out$subquestions)))
  expect_identical(out$subquestions$encrypted, c("N", NA, NA))
  expect_identical(out$subquestions[1, names(lss$subquestions)],
                   lss$subquestions[1, ])
  q <- rk_question(lss_model(lss))
  expect_identical(vapply(q$subquestions, function(s) s$code, ""),
                   c("A", "B", "C"))
})

test_that("a file without a subquestions section gets one", {
  lss <- rk_lss("answers", subquestions = FALSE)
  expect_null(lss$subquestions)
  out <- lss_ranking_as_subquestions(lss)
  expect_identical(out$subquestions$title, c("A", "B", "C"))
  expect_identical(
    names(out$subquestions),
    c("qid", "parent_qid", "sid", "gid", "type", "title", "mandatory",
      "question_order", "scale_id", "relevance")
  )
  # A table of texts without a script column gets one.
  lss$question_l10ns$script <- NULL
  out <- lss_ranking_as_subquestions(lss)
  expect_identical(out$question_l10ns$script,
                   c(rep(NA, 4), rep("", 6)))
  # A column the converter fills and the table of the questions lacks: the
  # new rows leave it empty.
  lss$questions$sid <- NULL
  out <- lss_ranking_as_subquestions(lss)
  expect_identical(out$subquestions$sid, rep(NA_character_, 3))
  expect_identical(out$subquestions$gid, rep("1", 3))
})

test_that("an object without a ranking in the old form comes back identical", {
  for (lss in list(rk_lss("R"), rk_lss("T"), small_lss())) {
    expect_identical(lss_ranking_as_subquestions(lss), lss)
  }
  lss <- rk_lss("R")
  lss$answers <- NULL
  lss$answer_l10ns <- NULL
  expect_identical(lss_ranking_as_subquestions(lss), lss)
  once <- lss_ranking_as_subquestions(rk_lss("answers"))
  expect_identical(lss_ranking_as_subquestions(once), once)
})

test_that("the demo survey keeps its other answer options", {
  lss <- demo_lss()
  out <- lss_ranking_as_subquestions(lss)
  rk <- lss$questions$qid[lss$questions$title == "devicerank"]
  expect_identical(out$answers, lss$answers[lss$answers$qid != rk, , drop = FALSE])
  expect_identical(nrow(out$subquestions), nrow(lss$subquestions) + 4L)
  expect_identical(out$subquestions$title[out$subquestions$parent_qid == rk],
                   c("SP", "PC", "TB", "TV"))
})

# ---- The three forms of a file ---------------------------------------------

test_that("the three forms of a ranking give the same model", {
  ref <- rk_model_sans_ids(rk_lss("answers"))
  q <- rk_question(ref)
  expect_length(q$answers, 0L)
  expect_identical(vapply(q$subquestions, function(s) s$code, ""),
                   c("A", "B", "C"))
  expect_identical(q$subquestions[[1]]$texts$fr$question, "Pommes")
  expect_identical(rk_model_sans_ids(rk_lss("R")), ref)
  expect_identical(rk_model_sans_ids(rk_lss("T")), ref)
})

test_that("the three forms of a ranking give the same documents", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  forms <- c("answers", "R", "T")
  for (tpl in c("cards", "table")) {
    docs <- lapply(forms, function(f) parity_render(rk_lss(f), tpl))
    for (k in 2:3) {
      expect_identical(docs[[k]]$content, docs[[1]]$content,
                       info = paste(tpl, forms[k]))
      expect_identical(parity_index_variables(docs[[k]]),
                       parity_index_variables(docs[[1]]),
                       info = paste(tpl, forms[k]))
    }
  }
  q <- lapply(forms, function(f) rk_questionnaire_text(rk_lss(f)))
  expect_identical(q[[2]], q[[1]])
  expect_identical(q[[3]], q[[1]])
  # With an order: answer_order in the old form, subquestion_order in the
  # forms of LimeSurvey 7.
  ordered <- function(f) {
    a <- if (identical(f, "answers")) "answer_order" else "subquestion_order"
    rk_lss(f, attrs = list(c(a, "alphabetical")))
  }
  for (tpl in c("cards", "table")) {
    docs <- lapply(forms, function(f) parity_render(ordered(f), tpl))
    expect_true(any(grepl("alphabetical order", docs[[1]]$content)), info = tpl)
    for (k in 2:3) {
      expect_identical(docs[[k]]$content, docs[[1]]$content,
                       info = paste("ordered", tpl, forms[k]))
    }
  }
})

# ---- What the documents print ----------------------------------------------

test_that("the card of a ranking prints its positions and its items", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- parity_render(rk_lss("R"), "cards")
  rows <- parity_card_rows(doc, "rk[*]")
  cells <- vapply(rows, function(r) r[1], "")
  p <- which(cells == "Positions (3)")
  expect_length(p, 1L)
  expect_identical(rows[(p + 1L):(p + 3L)],
                   list(c("1", "Rank 1", "Rank 1"), c("2", "Rank 2", "Rank 2"),
                        c("3", "Rank 3", "Rank 3")))
  v <- which(cells == "Value")
  expect_identical(rows[(v + 1L):(v + 3L)],
                   list(c("A", "Apples", "Pommes"),
                        c("B", "Bread rolls", "Petits pains"),
                        c("C", "Cheese", "Fromage")))
  expect_identical(sort(parity_index_variables(doc)),
                   c("nxt", "rk[1]", "rk[2]", "rk[3]"))
})

test_that("the table prints one row per rank, then the items", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- parity_render(rk_lss("R"), "table")
  b <- rk_table_block(doc)
  expect_identical(unname(b[2:4, "No"]), c("1.1", "1.2", "1.3"))
  expect_identical(unname(b[2:4, "Variable"]), c("rk[1]", "rk[2]", "rk[3]"))
  expect_identical(unname(b[2:4, "en"]), c("Rank 1", "Rank 2", "Rank 3"))
  expect_identical(unname(b[2:4, "fr"]), c("Rank 1", "Rank 2", "Rank 3"))
  expect_identical(unname(b[5:7, "Value"]), c("A", "B", "C"))
  expect_identical(unname(b[5:7, "en"]), c("Apples", "Bread rolls", "Cheese"))
  expect_identical(unname(b[5:7, "fr"]), c("Pommes", "Petits pains", "Fromage"))
  expect_identical(nrow(b), 7L)
  expect_identical(sort(parity_index_variables(doc)),
                   c("nxt", "rk[1]", "rk[2]", "rk[3]"))
})

test_that("the readable questionnaire lists the items of a ranking", {
  skip_on_cran()
  skip_if_not_installed("officer")
  text <- rk_questionnaire_text(rk_lss("R"), "fr")
  i <- grep("Classez ces aliments.", text, fixed = TRUE)
  expect_length(i, 1L)
  expect_identical(text[i + 1:3],
                   c("A\tPommes", "B\tPetits pains", "C\tFromage"))
})

test_that("the condition of an item is printed by no renderer", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- rk_lss("R", relevance = "nxt == \"x\"")
  q <- rk_question(lss_model(lss))
  expect_identical(q$subquestions[[2]]$relevance, "nxt == \"x\"")
  plain <- rk_lss("R")
  for (tpl in c("cards", "table")) {
    expect_identical(parity_render(lss, tpl)$content,
                     parity_render(plain, tpl)$content, info = tpl)
  }
  expect_identical(rk_questionnaire_text(lss), rk_questionnaire_text(plain))
})

test_that("a condition on a rank reads the same for both forms of the file", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  for (tpl in c("cards", "table")) {
    a <- parity_render(rk_lss("answers"), tpl)$content
    expect_identical(parity_render(rk_lss("R"), tpl)$content, a, info = tpl)
    expect_true("rk_1 = B" %in% a, info = tpl)
  }
  text <- rk_questionnaire_text(rk_lss("answers"))
  expect_true(any(grepl("nxt \u00B7 if rk_1 = B", text, fixed = TRUE)))
  expect_identical(rk_questionnaire_text(rk_lss("R")), text)
})

# ---- The number of positions -----------------------------------------------

test_that("max_subquestions caps the positions when it is a whole number above 0", {
  q <- rk_question(lss_model(rk_lss("R")))
  with_max <- function(v) {
    q$attributes <- data.frame(qid = "10", attribute = "max_subquestions",
                               value = v, language = "",
                               stringsAsFactors = FALSE)
    lss_ranking_positions(q)
  }
  expect_identical(lss_ranking_positions(q), 3L)
  expect_identical(
    vapply(c("2", "3", "5", "0", "", "abc", "2.5", " 2 ", "-1"), with_max, 1L,
           USE.NAMES = FALSE),
    c(2L, 3L, 3L, 3L, 3L, 3L, 3L, 2L, 3L)
  )
  q$subquestions <- list()
  expect_identical(with_max("2"), 0L)
})

test_that("both layouts, the index and the numbers follow max_subquestions", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  cases <- list(c("2", 2L), c("3", 3L), c("5", 3L), c("0", 3L), c("", 3L),
                c("abc", 3L))
  for (cs in cases) {
    n <- as.integer(cs[2])
    lss <- rk_lss("R", attrs = list(c("max_subquestions", cs[1])))
    vars <- paste0("rk[", seq_len(n), "]")
    cards <- parity_render(lss, "cards")
    rows <- parity_card_rows(cards, "rk[*]")
    cells <- vapply(rows, function(r) r[1], "")
    p <- which(cells == sprintf("Positions (%d)", n))
    expect_length(p, 1L)
    expect_identical(cells[p + seq_len(n)], as.character(seq_len(n)),
                     info = cs[1])
    expect_identical(cells[p + n + 1L], "Value", info = cs[1])
    expect_identical(sort(parity_index_variables(cards)), c("nxt", vars),
                     info = cs[1])
    table <- parity_render(lss, "table")
    b <- rk_table_block(table)
    ranks <- b[b[, "Variable"] != "" & b[, "Field"] != "Question", , drop = FALSE]
    expect_identical(unname(ranks[, "Variable"]), vars, info = cs[1])
    expect_identical(unname(ranks[, "No"]), paste0("1.", seq_len(n)),
                     info = cs[1])
    expect_identical(sort(parity_index_variables(table)), c("nxt", vars),
                     info = cs[1])
    # The items stay three, whatever the cap.
    expect_identical(sum(b[, "Value"] %in% c("A", "B", "C")), 3L)
  }
})

test_that("the No column of the table is as wide as the last rank needs", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  items <- lapply(1:12, function(i) c(paste0("I", i), paste("Item", i),
                                      paste("Objet", i)))
  widths <- vapply(c("9", ""), function(v) {
    lss <- rk_lss("R", items = items, attrs = list(c("max_subquestions", v)))
    out <- tempfile(fileext = ".docx")
    dir <- tempfile()
    on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
    suppressMessages(render_documentation(lss, out, template = "table",
                                          show_audit = FALSE))
    utils::unzip(out, files = "word/document.xml", exdir = dir)
    xml <- xml2::read_xml(file.path(dir, "word", "document.xml"))
    g <- xml2::xml_find_all(xml, "//w:tblGrid", parity_w)
    cols <- lapply(g, function(x) {
      as.numeric(xml2::xml_attr(xml2::xml_find_all(x, "w:gridCol", parity_w),
                                "w:w", parity_w))
    })
    big <- Filter(function(x) length(x) == 9L, cols)[[1]]
    big[[2]]
  }, numeric(1))
  expect_equal(unname(widths),
               round(c(max(0.30, lss_no_width("1.9", 8, 6)),
                       lss_no_width("1.12", 8, 6)) * 1440))
})

# ---- Names and order -------------------------------------------------------

test_that("the rank columns are named Q[i] and Q_i", {
  expect_identical(lss_variable_name("rk", 1L), "rk[1]")
  expect_identical(lss_variable_name("rk", 2L, style = "underscore"), "rk_2")
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- rk_lss("answers")
  for (tpl in c("cards", "table")) {
    doc <- parity_render(lss, tpl, variable_names = "underscore")
    expect_identical(sort(parity_index_variables(doc)),
                     c("nxt", "rk_1", "rk_2", "rk_3"), info = tpl)
  }
  b <- rk_table_block(parity_render(lss, "table", variable_names = "underscore"))
  expect_identical(unname(b[2:4, "Variable"]), c("rk_1", "rk_2", "rk_3"))
})

test_that("the converter renames the answer_order of a converted ranking", {
  attrs_of <- function(x) x$question_attributes[, c("attribute", "value")]
  lss <- rk_lss("answers", attrs = list(c("max_subquestions", "2"),
                                        c("answer_order", "random")))
  out <- lss_ranking_as_subquestions(lss)
  expect_identical(nrow(out$question_attributes), nrow(lss$question_attributes))
  expect_identical(out$question_attributes$attribute,
                   c("max_subquestions", "subquestion_order"))
  expect_identical(out$question_attributes$value, c("2", "random"))
  expect_identical(out$question_attributes[, c("qid", "language")],
                   lss$question_attributes[, c("qid", "language")])
  # A subquestion_order with a value wins: nothing is renamed.
  lss <- rk_lss("answers", attrs = list(c("answer_order", "random"),
                                        c("subquestion_order", "alphabetical")))
  expect_identical(lss_ranking_as_subquestions(lss)$question_attributes,
                   lss$question_attributes)
  # An empty subquestion_order does not.
  lss <- rk_lss("answers", attrs = list(c("subquestion_order", ""),
                                        c("answer_order", "random")))
  expect_identical(attrs_of(lss_ranking_as_subquestions(lss)),
                   data.frame(attribute = c("subquestion_order", "subquestion_order"),
                              value = c("", "random"),
                              stringsAsFactors = FALSE))
  # A ranking stored as subquestions is not touched.
  for (form in c("R", "T")) {
    lss <- rk_lss(form, attrs = list(c("answer_order", "random")))
    expect_identical(lss_ranking_as_subquestions(lss), lss, info = form)
  }
  # Without attributes, the items are converted all the same.
  lss <- rk_lss("answers")
  expect_null(lss$question_attributes)
  out <- lss_ranking_as_subquestions(lss)
  expect_null(out$question_attributes)
  expect_identical(out$subquestions$title, c("A", "B", "C"))
})

test_that("the order note of a ranking reads subquestion_order only", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  ch <- lss_chrome_strings("en")
  rnd <- ch$value_order_random
  alpha <- ch$value_order_alphabetical
  cases <- list(
    list("answers", list(c("answer_order", "random")), rnd),
    list("answers", list(c("answer_order", "alphabetical"),
                         c("subquestion_order", "random")), rnd),
    list("R", list(c("subquestion_order", "alphabetical")), alpha),
    list("R", list(c("answer_order", "random")), NULL),
    list("R", list(c("subquestion_order", "random"),
                   c("answer_order", "alphabetical")), rnd),
    list("R", list(c("subquestion_order", "normal"),
                   c("answer_order", "random")), NULL)
  )
  for (cs in cases) {
    lss <- rk_lss(cs[[1]], attrs = cs[[2]])
    info <- paste(cs[[1]], paste(unlist(cs[[2]]), collapse = " "))
    rows <- parity_card_rows(parity_render(lss, "cards"), "rk[*]")
    value <- Filter(function(r) identical(r[1], "Value"), rows)[[1]]
    b <- rk_table_block(parity_render(lss, "table"))
    order <- b[b[, "Field"] == "Order", , drop = FALSE]
    if (is.null(cs[[3]])) {
      expect_identical(value, c("Value", "", ""), info = info)
      expect_identical(nrow(order), 0L, info = info)
    } else {
      expect_identical(value, c("Value", cs[[3]], cs[[3]]), info = info)
      expect_identical(unname(order[, "en"]), cs[[3]], info = info)
    }
  }
})

test_that("the demo ranking keeps its alphabetical order in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  alpha <- lss_chrome_strings("en")$value_order_alphabetical
  lss <- demo_lss()
  rows <- parity_card_rows(parity_render(lss, "cards", key = "demo"),
                           "devicerank[*]")
  value <- Filter(function(r) identical(r[1], "Value"), rows)[[1]]
  expect_identical(value, c("Value", alpha, alpha))
  grid <- parity_table_grid(parity_render(lss, "table", key = "demo"))
  i <- which(grid[, "Variable"] == "devicerank[4]")
  nxt <- which(grid[, "Field"] == "Order")
  nxt <- nxt[nxt > i][1]
  expect_identical(unname(grid[nxt, "en"]), alpha)
  expect_true(all(grid[(i + 1L):(nxt - 1L), "Field"] == ""))
})

# ---- The audit -------------------------------------------------------------

test_that("the audit finds the same in both forms of a ranking", {
  ws <- rk_items()
  ws[[3]][1] <- "C D"
  dup <- rk_items()
  dup[[3]][1] <- "A"
  cases <- list(
    plain = list(),
    missing = list(drop = "B"),
    empty_label = list(items = c(rk_items()[1:2], list(c("C", "", "")))),
    duplicate = list(items = dup),
    space = list(items = ws),
    none = list(items = list())
  )
  for (nm in names(cases)) {
    a <- do.call(rk_lss, c(list("answers"), cases[[nm]]))
    r <- do.call(rk_lss, c(list("R"), cases[[nm]]))
    fa <- audit_lss(a)$findings
    expect_identical(audit_lss(r)$findings, fa, info = nm)
    expect_false("missing_options" %in% fa$check, info = nm)
  }
  find <- function(nm) {
    audit_lss(do.call(rk_lss, c(list("answers"), cases[[nm]])))$findings
  }
  expect_identical(nrow(find("plain")), 0L)
  f <- find("missing")
  expect_identical(f$location, "Subquestion 'rk / B'")
  expect_identical(f$language, "fr")
  f <- find("empty_label")
  expect_identical(f$location, "Subquestion 'rk / C'")
  expect_identical(f$severity, "warning")
  expect_true(any(find("duplicate")$message == "Duplicate subquestion code: 'A'."))
  expect_identical(find("space")$check, "code_whitespace")
  expect_identical(find("space")$location, "Subquestion 'C D'")
  f <- find("none")
  expect_identical(f$check, "missing_subquestions")
  expect_identical(f$message,
                   "Type 'Ranking' expects subquestions, but none are defined.")
})

test_that("an answer of no question is still an orphan", {
  lss <- rk_lss("answers")
  lss$answers$qid[3] <- "99"
  f <- audit_lss(lss)$findings
  expect_identical(f$check[f$check == "orphan_answer"], "orphan_answer")
  expect_identical(f$location[f$check == "orphan_answer"], "Answer 'C'")
})

# ---- The cover -------------------------------------------------------------

test_that("the cover counts the items of a ranking as subquestions", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  counts <- function(lss) {
    out <- tempfile(fileext = ".docx")
    on.exit(unlink(out), add = TRUE)
    suppressMessages(render_documentation(lss, out, show_audit = FALSE))
    rows <- unlist(parity_read_docx(out)$tables, recursive = FALSE)
    vapply(c("Subquestions", "Answer options"), function(lbl) {
      hit <- Filter(function(r) identical(r[1], lbl), rows)
      hit[[1]][2]
    }, "")
  }
  expect_identical(unname(counts(rk_lss("answers"))), c("3", "0"))
  expect_identical(unname(counts(rk_lss("R"))), c("3", "0"))
})

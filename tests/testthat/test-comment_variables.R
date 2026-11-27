# The comment variables: one free-text comment per option of a multiple
# choice with comments (type P), and the comment of a list with comment
# (type O). One helper names them, lss_comment_variable(); the table prints
# one row per comment variable of a type P, the card gives the pattern of
# their names, and both variable indexes list them.

cv_theme <- function(lang = "en", style = NULL) {
  th <- ta_theme()
  th$chrome <- lss_chrome_strings(lang)
  th$variable_names <- style
  th
}
# A type P question "q1" with two options, numbered 7 in a survey.
cv_q <- function(type = "P", other = "N") {
  ta_q(type, "q1", subquestions = list(ta_sq("SQ1"), ta_sq("SQ2")),
       other = other)
}
# The rows of the table for `q`, and the entries of the variable index.
cv_rows <- function(q, style = NULL) {
  state <- lss_render_state(NULL)
  state$numbers <- data.frame(qid = q$qid, no = 7L, label = "7",
                              stringsAsFactors = FALSE)
  state$show_attrs <- character(0)
  rows <- lss_table_template_rows_for_group(
    list(questions = list(q)), TA_LANGS, cv_theme(style = style),
    show_help = TRUE, state = state, show_groups = FALSE
  )
  list(rows = rows, index = state$index_entries)
}
cv_index <- function(entries) {
  stats::setNames(vapply(entries, function(e) as.character(e$no), ""),
                  vapply(entries, function(e) e$code, ""))
}

# ---- the names ----------------------------------------------------------------

test_that("lss_comment_variable() names the comment variables in both styles", {
  q <- list(code = "Q")
  # Type P: the subquestion code followed by "comment".
  expect_identical(lss_comment_variable(q, "SQ"), "Q[SQcomment]")
  expect_identical(lss_comment_variable(q, "SQ", "underscore"), "Q_SQcomment")
  expect_identical(lss_comment_variable(q, "other"), "Q[othercomment]")
  expect_identical(lss_comment_variable(q, "other", "underscore"),
                   "Q_othercomment")
  # The pattern the card prints.
  expect_identical(lss_comment_variable(q, "*"), "Q[*comment]")
  expect_identical(lss_comment_variable(q, "*", "underscore"), "Q_*comment")
  # Type O: the CSV export's name, and the Expression Manager's.
  expect_identical(lss_comment_variable(q), "Q[_Ccomment]")
  expect_identical(lss_comment_variable(q, style = "underscore"), "Q_comment")
  # A digit-only code keeps its leading zeros in the comment's name, while
  # the option's own name drops them (not checked against an export).
  expect_identical(lss_variable_name("Q", "001"), "Q[1]")
  expect_identical(lss_comment_variable(q, "001"), "Q[001comment]")
})

test_that("the sentence of the card is a format with the pattern of the names", {
  for (lg in c("en", "fr", "de", "es", "it")) {
    fmt <- lss_chrome_strings(lg)$value_multi_y_blank_with_comment
    expect_identical(lengths(regmatches(fmt, gregexpr("%", fmt, fixed = TRUE))),
                     1L, info = lg)
    expect_match(fmt, "%s$", info = lg)
    expect_false(grepl("[<>]", fmt), info = lg)
  }
  expect_identical(
    lss_chrome_strings("en")$value_multi_y_blank_with_comment,
    "Y = selected, blank = not selected. Each option also has a comment variable (free text): %s"
  )
  expect_identical(
    lss_chrome_strings("fr")$value_multi_y_blank_with_comment,
    paste0("Y = s\u00E9lectionn\u00E9, vide = non s\u00E9lectionn\u00E9. ",
           "Chaque option a aussi une variable de commentaire (texte libre)",
           "\u00A0: %s")
  )
  expect_identical(
    lss_chrome_strings("de")$value_multi_y_blank_with_comment,
    paste0("Y = ausgew\u00E4hlt, leer = nicht ausgew\u00E4hlt. Jede Option hat ",
           "zus\u00E4tzlich eine Kommentarvariable (Freitext): %s")
  )
  expect_identical(
    lss_chrome_strings("es")$value_multi_y_blank_with_comment,
    paste0("Y = seleccionado, vac\u00EDo = no seleccionado. Cada opci\u00F3n ",
           "tiene adem\u00E1s una variable de comentario (texto libre): %s")
  )
  expect_identical(
    lss_chrome_strings("it")$value_multi_y_blank_with_comment,
    paste0("Y = selezionato, vuoto = non selezionato. Ogni opzione ha inoltre ",
           "una variabile di commento (testo libero): %s")
  )
})

# ---- the cards ----------------------------------------------------------------

test_that("cards: the Value row of a type P gives the pattern of the names", {
  q <- cv_q()
  row <- lss_value_implicit_row(q, TA_LANGS, cv_theme())
  expect_identical(row$texts$en, paste0(
    "Y = selected, blank = not selected. Each option also has a comment ",
    "variable (free text): q1[*comment]"
  ))
  expect_identical(row$texts$fr, row$texts$en)
  row <- lss_value_implicit_row(q, TA_LANGS, cv_theme(style = "underscore"))
  expect_true(endsWith(row$texts$en, ": q1_*comment"))
  row <- lss_value_implicit_row(q, TA_LANGS, cv_theme("fr"))
  expect_true(endsWith(row$texts$en, "(texte libre)\u00A0: q1[*comment]"))
  # A multiple choice without comments keeps its sentence.
  row <- lss_value_implicit_row(cv_q("M"), TA_LANGS, cv_theme())
  expect_identical(row$texts$en, "Y = selected, blank = not selected")
})

test_that("cards: the index lists the comment variables at the question's number", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  card_index <- function(q, style = NULL) {
    state <- lss_render_state(NULL)
    state$numbers <- data.frame(qid = q$qid, no = 7L, label = "7",
                                stringsAsFactors = FALSE)
    lss_render_multiple_choice(officer::read_docx(), q, TA_LANGS,
                               cv_theme(style = style), show_help = TRUE,
                               show_attrs = character(0),
                               state = state)
    cv_index(state$index_entries)
  }
  expect_identical(card_index(cv_q()), c(
    "q1[SQ1]" = "7", "q1[SQ1comment]" = "7",
    "q1[SQ2]" = "7", "q1[SQ2comment]" = "7"
  ))
  expect_identical(card_index(cv_q(other = "Y")), c(
    "q1[SQ1]" = "7", "q1[SQ1comment]" = "7",
    "q1[SQ2]" = "7", "q1[SQ2comment]" = "7",
    "q1[othercomment]" = "7"
  ))
  expect_identical(names(card_index(cv_q(), "underscore")),
                   c("q1_SQ1", "q1_SQ1comment", "q1_SQ2", "q1_SQ2comment"))
  # A multiple choice without comments: the options only.
  expect_identical(names(card_index(cv_q("M"))), c("q1[SQ1]", "q1[SQ2]"))
})

# ---- the table ----------------------------------------------------------------

test_that("table rows: a comment row after each option of a type P", {
  res <- cv_rows(cv_q())
  rows <- res$rows
  expect_identical(ta_kinds(rows),
                   c("leaf", "mc_option", "leaf", "mc_option", "leaf"))
  for (k in 1:2) {
    opt <- rows[[2 * k]]
    cr <- rows[[2 * k + 1]]
    code <- paste0("SQ", k)
    expect_identical(cr$variable, sprintf("q1[%scomment]", code))
    # The number of its option, the label "Comment", a short free text.
    expect_identical(cr$no, opt$no)
    expect_identical(cr$no, sprintf("7.%d", k))
    expect_identical(cr$field, "Comment")
    expect_identical(cr$q$type, "S")
    expect_length(cr$q$answers, 0L)
    expect_identical(cr$type_label, "Text (short)")
    # The label of the option in the language cells, no help.
    expect_identical(cr$parent_text, list(en = paste("Row", code),
                                          fr = paste("Ligne", code)))
    expect_null(cr$help)
  }
  expect_identical(cv_index(res$index), c(
    "q1[SQ1]" = "7.1", "q1[SQ1comment]" = "7.1",
    "q1[SQ2]" = "7.2", "q1[SQ2comment]" = "7.2"
  ))
})

test_that("table rows: the Other of a type P has its comment row", {
  res <- cv_rows(cv_q(other = "Y"))
  rows <- res$rows
  expect_identical(ta_kinds(rows), c("leaf", "mc_option", "leaf", "mc_option",
                                     "leaf", "other", "leaf"))
  cr <- rows[[7]]
  expect_identical(cr$variable, "q1[othercomment]")
  expect_identical(cr$no, rows[[6]]$no)
  expect_identical(cr$no, "7")
  expect_identical(cr$field, "Comment")
  expect_identical(cr$q$type, "S")
  ch <- lss_chrome_strings("en")
  expect_identical(cr$parent_text, list(
    en = lss_other_prompt(cr$q, "en", ch), fr = lss_other_prompt(cr$q, "fr", ch)
  ))
  expect_null(cr$help)
  expect_identical(utils::tail(cv_index(res$index), 2L),
                   c("q1[other]" = "7", "q1[othercomment]" = "7"))
})

test_that("table rows: no comment row for a multiple choice without comments", {
  res <- cv_rows(cv_q("M", other = "Y"))
  expect_identical(ta_kinds(res$rows),
                   c("leaf", "mc_option", "mc_option", "other"))
  expect_identical(names(cv_index(res$index)),
                   c("q1[SQ1]", "q1[SQ2]", "q1[other]"))
})

test_that("table rows: the names of the comment variables in the underscore style", {
  res <- cv_rows(cv_q(other = "Y"), style = "underscore")
  expect_identical(names(cv_index(res$index)), c(
    "q1_SQ1", "q1_SQ1comment", "q1_SQ2", "q1_SQ2comment", "q1_other",
    "q1_othercomment"
  ))
  # The list with comment: the Expression Manager's name.
  o <- ta_q("O", "q2", answers = list(ta_answer("A1")))
  rows <- cv_rows(o, style = "underscore")$rows
  expect_identical(rows[[length(rows)]]$variable, "q2_comment")
  expect_identical(cv_rows(o)$rows[[length(rows)]]$variable, "q2[_Ccomment]")
})

test_that("table rows: a comment row is never mandatory", {
  skip_if_not_installed("officer")
  # A mandatory list with comment: the list row says Yes, its comment No.
  o <- ta_q("O", "q2", answers = list(ta_answer("A1")))
  o$mandatory <- "Y"
  rows <- cv_rows(o)$rows
  expect_identical(ta_kinds(rows), c("leaf", "value", "leaf"))
  expect_identical(rows[[1]]$mandatory_label, "Yes")
  expect_identical(rows[[3]]$mandatory_label, "No")
  # The meta band of the card of the comment says the same.
  seen <- NULL
  testthat::local_mocked_bindings(
    lss_render_question_meta_table = function(doc, theme, ..., mandatory) {
      seen <<- mandatory
      doc
    },
    lss_render_item_table = function(doc, ...) doc
  )
  state <- lss_render_state(NULL)
  lss_render_comment_item(officer::read_docx(), o, TA_LANGS, cv_theme(),
                          state = state)
  expect_identical(lss_yes_no(seen, cv_theme()), rows[[3]]$mandatory_label)
  # A mandatory multiple choice with comments and "Other": every comment
  # row says No, the question row Yes.
  p <- cv_q(other = "Y")
  p$mandatory <- "Y"
  rows <- cv_rows(p)$rows
  expect_identical(rows[[1]]$mandatory_label, "Yes")
  comments <- Filter(function(r) identical(r$field, "Comment"), rows)
  expect_length(comments, 3L)
  expect_identical(vapply(comments, function(r) r$mandatory_label, ""),
                   rep("No", 3L))
})

# ---- the rendered documents ---------------------------------------------------

# The variable index of a rendered document: No by Variable.
cv_doc_index <- function(doc) {
  idx <- Filter(function(t) identical(t[[1]], c("Variable", "No")), doc$tables)
  rows <- idx[[length(idx)]][-1L]
  stats::setNames(vapply(rows, function(r) r[[2]], ""),
                  vapply(rows, function(r) r[[1]], ""))
}

test_that("table: the comment rows of socialmedia, cell by cell", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- parity_lss()
  m <- lss_model(lss, TA_LANGS)
  qs <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  sm <- Filter(function(q) identical(q$code, "socialmedia"), qs)[[1]]
  expect_identical(sm$type, "P")
  doc <- parity_render(lss, "table", key = "parity")
  grid <- parity_table_grid(doc)
  index <- cv_doc_index(doc)
  for (sq in sm$subquestions) {
    opt <- which(grid[, "Variable"] == sprintf("socialmedia[%s]", sq$code))
    expect_length(opt, 1L)
    cr <- grid[opt + 1L, ]
    var <- sprintf("socialmedia[%scomment]", sq$code)
    expect_identical(unname(cr[c("Field", "No", "Variable", "Type", "Value")]),
                     c("Comment", unname(grid[opt, "No"]), var, "Text (short)",
                       "[text]"), info = sq$code)
    for (lg in TA_LANGS) {
      expect_identical(unname(parity_squish(cr[lg])),
                       parity_squish(lss_html_to_text(sq$texts[[lg]]$question)),
                       info = paste(sq$code, lg))
    }
    # The index: the comment at the number of its option.
    expect_identical(unname(index[var]), unname(grid[opt, "No"]), info = sq$code)
  }
})

test_that("cards: the card and the index of socialmedia", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- parity_lss()
  m <- lss_model(lss, TA_LANGS)
  qs <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  sm <- Filter(function(q) identical(q$code, "socialmedia"), qs)[[1]]
  doc <- parity_render(lss, "cards", key = "parity")
  rows <- parity_card_rows(doc, "socialmedia[*]")
  value <- Filter(function(r) identical(r[[1]], "Value"), rows)
  expect_length(value, 1L)
  sentence <- paste0("Y = selected, blank = not selected. Each option also ",
                     "has a comment variable (free text): socialmedia[*comment]")
  expect_identical(unname(parity_squish(value[[1]][-1L])),
                   rep(sentence, length(TA_LANGS)))
  # One Value row: the card names no comment variable one by one.
  names_one_by_one <- sprintf("socialmedia[%scomment]",
                              vapply(sm$subquestions, function(s) s$code, ""))
  expect_false(any(names_one_by_one %in% unlist(rows)))
  index <- cv_doc_index(doc)
  for (sq in sm$subquestions) {
    var <- sprintf("socialmedia[%scomment]", sq$code)
    expect_identical(unname(index[var]), unname(index[sprintf("socialmedia[%s]", sq$code)]),
                     info = sq$code)
  }
})

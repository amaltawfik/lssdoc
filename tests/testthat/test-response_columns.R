# The names of the response columns. With variable_names = "brackets" they
# are the headings of the CSV and Excel export of LimeSurvey 7.12.1 in its
# "question code" style, as its source code builds them (no real export
# was at hand). With "underscore" the same parts are joined by an
# underscore. These are the names of the Expression Manager, with two
# exceptions: the scales of a dual scale are counted from 1, as in the
# SPSS export, and `Q_otherchecked` is the name of the SPSS export, the
# Expression Manager having no variable for that column. Two columns of
# the export are documented in both layouts and in both variable indexes:
# the box of "Other" of a multiple choice (type M), `Q[otherchecked]`, and
# the number of files of a file upload (type |), `Q[filecount]`.

rc_theme <- function(lang = "en", style = NULL) {
  th <- ta_theme()
  th$chrome <- lss_chrome_strings(lang)
  th$variable_names <- style
  th
}
# A question of the row helpers, numbered 7 in a survey.
rc_state <- function(q) {
  state <- lss_render_state(NULL)
  state$numbers <- data.frame(qid = q$qid, no = 7L, label = "7",
                              stringsAsFactors = FALSE)
  state$show_attrs <- TA_ATTRS
  state
}
# The rows of the table for `q`, and the entries of the variable index.
rc_rows <- function(q, style = NULL) {
  state <- rc_state(q)
  rows <- lss_table_template_rows_for_group(
    list(questions = list(q)), TA_LANGS, rc_theme(style = style),
    show_help = TRUE, state = state, show_groups = FALSE
  )
  list(rows = rows, index = state$index_entries)
}
rc_index <- function(entries) {
  stats::setNames(vapply(entries, function(e) as.character(e$no), ""),
                  vapply(entries, function(e) e$code, ""))
}
# The cards of `q`: the arguments of each meta band, the rows of each item
# table, and the entries of the variable index, drawing nothing.
rc_cards <- function(q, style = NULL) {
  seen <- new.env()
  seen$meta <- list()
  seen$rows <- list()
  testthat::local_mocked_bindings(
    lss_render_question_meta_table = function(doc, theme, ...) {
      seen$meta[[length(seen$meta) + 1L]] <- list(...)
      doc
    },
    lss_render_item_table = function(doc, theme, langs, rows) {
      seen$rows[[length(seen$rows) + 1L]] <- rows
      doc
    }
  )
  state <- rc_state(q)
  lss_render_question_block(officer::read_docx(), q, TA_LANGS,
                            rc_theme(style = style), show_help = TRUE,
                            show_attrs = character(0),
                            show_technical_attrs = FALSE, state = state)
  list(meta = seen$meta, rows = seen$rows, index = rc_index(state$index_entries))
}
rc_mc <- function(type = "M", other = "Y") {
  ta_q(type, "q1", subquestions = list(ta_sq("SQ1"), ta_sq("SQ2")),
       other = other)
}
RC_CHECKED <- list(en = "Other (selected)", fr = "Autre (s\u00E9lectionn\u00E9)")

# ---- the names ----------------------------------------------------------------

test_that("the Other field is Q[other] for every type that has one", {
  for (type in c("L", "!", "M", "P")) {
    q <- list(code = "Q", type = type, other = "Y")
    expect_identical(lss_other_variable(q), "Q[other]", info = type)
    expect_identical(lss_other_variable(q, "underscore"), "Q_other", info = type)
  }
})

test_that("the comment of a list with comment is Q[comment]", {
  q <- list(code = "Q", type = "O")
  expect_identical(lss_comment_variable(q), "Q[comment]")
  expect_identical(lss_comment_variable(q, style = "underscore"), "Q_comment")
})

test_that("a code made of digits keeps its leading zeros", {
  expect_identical(lss_variable_name("Q", "001"), "Q[001]")
  expect_identical(lss_variable_name("Q", "001", style = "underscore"), "Q_001")
  expect_identical(lss_variable_name("Q", "001", 1L), "Q[001][1]")
  expect_identical(lss_variable_name("Q", "001", 2L, "underscore"), "Q_001_2")
  expect_identical(lss_variable_name("Q", "0"), "Q[0]")
  expect_identical(lss_variable_name("Q", "10"), "Q[10]")
  expect_identical(lss_comment_variable(list(code = "Q"), "001"), "Q[001comment]")
  # A rank stays a number.
  expect_identical(lss_variable_name("Q", 2L), "Q[2]")
})

test_that("Q[otherchecked] for a multiple choice with Other, and only there", {
  m <- list(code = "Q", type = "M", other = "Y")
  expect_identical(lss_other_checked_variable(m), "Q[otherchecked]")
  expect_identical(lss_other_checked_variable(m, "underscore"), "Q_otherchecked")
  expect_null(lss_other_checked_variable(utils::modifyList(m, list(other = "N"))))
  expect_null(lss_other_checked_variable(utils::modifyList(m, list(type = "P"))))
  for (type in c("L", "!", "O", "|")) {
    expect_null(lss_other_checked_variable(list(code = "Q", type = type,
                                                other = "Y")), info = type)
  }
})

test_that("Q[filecount] for a file upload, and only there", {
  f <- list(code = "Q", type = "|", other = "N")
  expect_identical(lss_file_count_variable(f), "Q[filecount]")
  expect_identical(lss_file_count_variable(f, "underscore"), "Q_filecount")
  for (type in c("S", "T", "M", "N")) {
    expect_null(lss_file_count_variable(list(code = "Q", type = type,
                                             other = "N")), info = type)
  }
})

test_that("the three labels in the five chrome languages", {
  expected <- list(
    en = c("Other (selected)", "File count", "Number of files uploaded"),
    fr = c("Autre (s\u00E9lectionn\u00E9)", "Nombre de fichiers",
           "Nombre de fichiers d\u00E9pos\u00E9s"),
    de = c("Sonstiges (gew\u00E4hlt)", "Dateianzahl",
           "Anzahl der hochgeladenen Dateien"),
    es = c("Otro (seleccionado)", "N\u00FAmero de archivos",
           "N\u00FAmero de archivos subidos"),
    it = c("Altro (selezionato)", "Numero di file", "Numero di file caricati")
  )
  for (lg in names(expected)) {
    ch <- lss_chrome_strings(lg)
    expect_identical(
      c(ch$value_other_checked, ch$item_file_count, ch$value_file_count),
      expected[[lg]], info = lg
    )
  }
  # The label of the box of "Other" follows each language column, and a
  # language without strings takes the chrome language.
  expect_identical(
    lss_other_checked_labels(c("en", "fr", "pt"), lss_chrome_strings("de")),
    list(en = "Other (selected)", fr = "Autre (s\u00E9lectionn\u00E9)",
         pt = "Sonstiges (gew\u00E4hlt)")
  )
})

# ---- otherchecked: the rows -------------------------------------------------------

test_that("table rows: otherchecked is the last option, before the notes", {
  q <- rc_mc()
  q$attributes <- ta_attrs(list("exclude_all_others", "SQ2", ""),
                           list("subquestion_order", "random", ""))
  res <- rc_rows(q)
  rows <- res$rows
  expect_identical(ta_kinds(rows), c("leaf", "mc_option", "mc_option",
                                     "mc_option", "mc_exclusive",
                                     "order_note", "other"))
  oc <- rows[[4]]
  expect_identical(oc$variable, "q1[otherchecked]")
  # The number of the question, as the Other row has.
  expect_identical(oc$no, "7")
  expect_identical(oc$no, rows[[7]]$no)
  expect_identical(oc$value_token, rows[[2]]$value_token)
  expect_identical(oc$value_token, "Y/blank")
  expect_identical(oc$labels, RC_CHECKED)
  expect_identical(rc_index(res$index), c(
    "q1[SQ1]" = "7.1", "q1[SQ2]" = "7.2", "q1[otherchecked]" = "7",
    "q1[other]" = "7"
  ))
  res <- rc_rows(q, style = "underscore")
  expect_identical(res$rows[[4]]$variable, "q1_otherchecked")
  expect_identical(names(rc_index(res$index)),
                   c("q1_SQ1", "q1_SQ2", "q1_otherchecked", "q1_other"))
})

test_that("table rows: no otherchecked for a P, nor for an M without Other", {
  for (q in list(rc_mc("P"), rc_mc("M", other = "N"))) {
    res <- rc_rows(q)
    vars <- vapply(res$rows, function(r) {
      if (is.null(r$variable)) "" else r$variable
    }, "")
    expect_false(any(grepl("otherchecked", vars)), info = q$type)
    expect_false(any(grepl("otherchecked", names(rc_index(res$index)))),
                 info = q$type)
  }
})

test_that("cards: otherchecked is the last row of the Options section", {
  skip_if_not_installed("officer")
  q <- rc_mc()
  q$attributes <- ta_attrs(list("exclude_all_others", "SQ2", ""))
  res <- rc_cards(q)
  rows <- res$rows[[1]]
  labels <- vapply(rows, function(r) r$label, "")
  # The count of the heading stays the number of the options of the survey.
  expect_identical(labels, c("Question", "Options (2)", "SQ1", "SQ2",
                             "otherchecked", "Exclusive", "Value"))
  oc <- rows[[5]]
  expect_identical(oc$texts, RC_CHECKED)
  expect_true(isTRUE(oc$code_row))
  expect_identical(oc$size, rows[[3]]$size)
  expect_identical(res$index, c(
    "q1[SQ1]" = "7", "q1[SQ2]" = "7", "q1[otherchecked]" = "7",
    "q1[other]" = "7"
  ))
  expect_identical(names(rc_cards(q, "underscore")$index),
                   c("q1_SQ1", "q1_SQ2", "q1_otherchecked", "q1_other"))
  # Neither a P nor an M without Other has the row.
  for (q in list(rc_mc("P"), rc_mc("M", other = "N"))) {
    res <- rc_cards(q)
    labels <- vapply(res$rows[[1]], function(r) r$label, "")
    expect_false("otherchecked" %in% labels, info = q$type)
    expect_false(any(grepl("otherchecked", names(res$index))), info = q$type)
  }
})

# ---- filecount: the rows ----------------------------------------------------------

test_that("table rows: the filecount row follows the file upload and its attributes", {
  q <- ta_q("|", "up1", attributes = ta_attrs(list("suffix", "PDF only", "")))
  q$mandatory <- "Y"
  res <- rc_rows(q)
  rows <- res$rows
  expect_identical(ta_kinds(rows), c("leaf", "attr", "leaf"))
  fc <- rows[[3]]
  expect_identical(fc$variable, "up1[filecount]")
  expect_identical(fc$field, "File count")
  expect_identical(fc$no, "7")
  expect_identical(fc$no, rows[[1]]$no)
  expect_identical(fc$type_label, "Number")
  expect_identical(fc$mandatory_label, "No")
  expect_identical(rows[[1]]$mandatory_label, "Yes")
  # The token of a number in the Value cell, the stem in the language
  # cells, no help.
  expect_identical(fc$q$type, "N")
  expect_length(fc$q$answers, 0L)
  expect_identical(fc$parent_text, list(en = "Stem up1", fr = "Libell\u00E9 up1"))
  expect_null(fc$help)
  expect_identical(rc_index(res$index), c("up1" = "7", "up1[filecount]" = "7"))
  expect_identical(names(rc_index(rc_rows(q, "underscore")$index)),
                   c("up1", "up1_filecount"))
  # The comment row of a list with comment keeps its label and type.
  o <- ta_q("O", "q2", answers = list(ta_answer("A1")))
  cr <- rc_rows(o)$rows[[3]]
  expect_identical(c(cr$field, cr$type_label, cr$q$type),
                   c("Comment", "Text (short)", "S"))
})

test_that("cards: the filecount card follows the card of the file upload", {
  skip_if_not_installed("officer")
  q <- ta_q("|", "up1")
  q$mandatory <- "Y"
  res <- rc_cards(q)
  expect_length(res$meta, 2L)
  meta <- res$meta[[2]]
  expect_identical(meta$item_no, "7")
  expect_identical(meta$item_no, res$meta[[1]]$item_no)
  expect_identical(meta$variable, "up1[filecount]")
  expect_identical(meta$type_label, "Number")
  expect_identical(meta$mandatory, "N")
  rows <- res$rows[[2]]
  expect_identical(vapply(rows, function(r) r$label, ""), c("Question", "Value"))
  expect_identical(rows[[1]]$texts, list(en = "Stem up1", fr = "Libell\u00E9 up1"))
  expect_identical(rows[[2]]$texts, list(en = "Number of files uploaded",
                                         fr = "Number of files uploaded"))
  expect_true(isTRUE(rows[[2]]$section_header))
  expect_true(isTRUE(rows[[2]]$span_note))
  expect_identical(res$index, c("up1" = "7", "up1[filecount]" = "7"))
  expect_identical(rc_cards(q, "underscore")$meta[[2]]$variable, "up1_filecount")
  # The card of the comment of a list with comment keeps its rows.
  o <- ta_q("O", "q2", answers = list(ta_answer("A1")))
  res <- rc_cards(o)
  expect_identical(res$meta[[2]]$variable, "q2[comment]")
  expect_identical(res$meta[[2]]$type_label, "Text (short)")
  expect_identical(vapply(res$rows[[2]], function(r) r$label, ""),
                   c("Question", "Comment"))
  expect_identical(res$rows[[2]][[2]]$texts$en, "Free text (single line)")
})

# ---- the rendered documents ---------------------------------------------------

# The demo survey with every name of this file: an "Other" on a list
# (respondentrole) and on a dropdown list (education), a list with comment
# (langhome), a multiple choice with "Other" (nationality), a multiple choice
# with comments and "Other" (socialmedia), a multiple choice without "Other"
# (supportsources), a code 001 in a multiple choice (supportsources), an
# array (dailyresources) and a multiple short text (weeklyhours), and a file
# upload (idscan). One question at least stays in every group.
rc_lss <- function() {
  hit <- lss_fixture_cache[["rc"]]
  if (!is.null(hit)) return(hit)
  lss <- demo_lss()
  qid <- function(code) lss$questions$qid[lss$questions$title == code]
  lss$questions$other[lss$questions$title %in% c("education", "socialmedia")] <- "Y"
  sm <- lss$questions$title == "socialmedia"
  lss$questions$type[sm] <- "P"
  lss$questions$question_theme_name[sm] <- "multiplechoice_with_comments"
  lss$subquestions$type[lss$subquestions$parent_qid == qid("socialmedia")] <- "P"
  for (code in c("supportsources", "dailyresources", "weeklyhours")) {
    s <- lss$subquestions
    k <- which(s$parent_qid == qid(code))
    k <- k[order(as.integer(s$question_order[k]))][1]
    lss$subquestions$title[k] <- "001"
  }
  lss <- parity_add_file_upload(lss, "idscan", "langhome", c(
    en = "Upload a scan of your identity card.",
    fr = "D\u00E9posez un scan de votre carte d'identit\u00E9."
  ))
  keep <- c("respondentrole", "education", "nationality", "langhome",
            "idscan", "healthself", "supportsources", "dailyresources",
            "weeklyhours", "socialmedia", "surveyinterest")
  lss$questions <- lss$questions[lss$questions$title %in% keep, , drop = FALSE]
  lss_fixture_cache[["rc"]] <- lss
  lss
}
# The variable index of a rendered document: No by Variable.
rc_doc_index <- function(doc) {
  idx <- Filter(function(t) identical(t[[1]], c("Variable", "No")), doc$tables)
  rows <- idx[[length(idx)]][-1L]
  stats::setNames(vapply(rows, function(r) r[[2]], ""),
                  vapply(rows, function(r) r[[1]], ""))
}
# The meta band of the card that names `variable`: its header and values.
rc_meta_band <- function(doc, variable) {
  hit <- which(vapply(doc$tables, function(t) {
    any(vapply(t, function(r) variable %in% r, logical(1)))
  }, logical(1)))
  t <- doc$tables[[hit[1]]]
  stats::setNames(t[[2]], t[[1]])
}

test_that("both layouts and both indexes print the names of the export", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- rc_lss()
  names_of <- c("respondentrole[other]", "education[other]",
                "langhome[comment]", "nationality[other]",
                "nationality[otherchecked]", "supportsources[001]",
                "dailyresources[001]", "weeklyhours[001]",
                "socialmedia[other]", "idscan", "idscan[filecount]")
  for (tpl in c("cards", "table")) {
    doc <- parity_render(lss, tpl, key = "rc")
    index <- rc_doc_index(doc)
    expect_true(all(names_of %in% names(index)), info = tpl)
    expect_false(any(grepl("[_C", names(index), fixed = TRUE)), info = tpl)
    expect_false(any(grepl("[_C", unlist(doc$tables), fixed = TRUE)), info = tpl)
    # One otherchecked: neither the P (socialmedia) nor the M without
    # "Other" (supportsources) has one.
    expect_identical(grep("otherchecked", names(index), value = TRUE),
                     "nationality[otherchecked]", info = tpl)
    # The leading zeros are kept in the index and in the document.
    expect_false(any(c("supportsources[1]", "dailyresources[1]",
                       "weeklyhours[1]") %in% names(index)), info = tpl)
    # otherchecked and filecount at the number of their question.
    expect_identical(index[["nationality[otherchecked]"]],
                     index[["nationality[other]"]], info = tpl)
    expect_identical(index[["idscan[filecount]"]], index[["idscan"]], info = tpl)
  }
  table <- parity_table_grid(parity_render(lss, "table", key = "rc"))
  expect_true(all(c("dailyresources[001]", "weeklyhours[001]",
                    "supportsources[001]") %in% table[, "Variable"]))
  cards <- parity_render(lss, "cards", key = "rc")
  expect_identical(rc_meta_band(cards, "dailyresources[001]")[["Variable"]],
                   "dailyresources[001]")
  expect_identical(rc_meta_band(cards, "weeklyhours[001]")[["Variable"]],
                   "weeklyhours[001]")
  expect_identical(rc_meta_band(cards, "langhome[comment]")[["Type"]],
                   "Text (short)")
  expect_identical(rc_meta_band(cards, "education[other]")[["Type"]],
                   "Text (other)")
})

test_that("the underscore style joins the same parts with an underscore", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- rc_lss()
  for (tpl in c("cards", "table")) {
    index <- rc_doc_index(parity_render(lss, tpl, key = "rc_underscore",
                                        variable_names = "underscore"))
    expect_true(all(c("respondentrole_other", "langhome_comment",
                      "nationality_otherchecked", "supportsources_001",
                      "dailyresources_001", "weeklyhours_001",
                      "idscan_filecount") %in% names(index)), info = tpl)
  }
})

test_that("cards: the Options section of nationality ends with otherchecked", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- rc_lss()
  m <- lss_model(lss, TA_LANGS)
  qs <- unlist(lapply(m$groups, function(g) g$questions), recursive = FALSE)
  nat <- Filter(function(q) identical(q$code, "nationality"), qs)[[1]]
  doc <- parity_render(lss, "cards", key = "rc")
  rows <- parity_card_rows(doc, "nationality[*]")
  first <- vapply(rows, function(r) r[[1]], "")
  head <- which(first == sprintf("Options (%d)", length(nat$subquestions)))
  expect_length(head, 1L)
  codes <- vapply(nat$subquestions, function(s) s$code, "")
  expect_identical(first[head + seq_along(codes)], codes)
  oc <- rows[[head + length(codes) + 1L]]
  # Its label in the language of each column.
  expect_identical(parity_squish(oc), c("otherchecked", "Other (selected)",
                                        "Autre (s\u00E9lectionn\u00E9)"))
  expect_identical(first[head + length(codes) + 2L], "Value")
})

test_that("table: the otherchecked row of nationality, cell by cell", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  grid <- parity_table_grid(parity_render(rc_lss(), "table", key = "rc"))
  k <- which(grid[, "Variable"] == "nationality[otherchecked]")
  expect_length(k, 1L)
  other <- which(grid[, "Variable"] == "nationality[other]")
  # After the last option of the survey.
  expect_identical(unname(grid[k - 1L, "Variable"]), "nationality[IT]")
  expect_true(other > k)
  expect_identical(
    unname(parity_squish(grid[k, ])),
    c("", unname(grid[other, "No"]), "nationality[otherchecked]", "", "", "",
      "Y/blank", "Other (selected)", "Autre (s\u00E9lectionn\u00E9)")
  )
})

test_that("the filecount card and row of idscan, cell by cell", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- rc_lss()
  stem <- c("Upload a scan of your identity card.",
            "D\u00E9posez un scan de votre carte d'identit\u00E9.")
  cards <- parity_render(lss, "cards", key = "rc")
  q_band <- rc_meta_band(cards, "idscan")
  band <- rc_meta_band(cards, "idscan[filecount]")
  expect_identical(unname(band[c("No", "Variable", "Type", "Mandatory")]),
                   c(unname(q_band[["No"]]), "idscan[filecount]", "Number", "No"))
  rows <- parity_card_rows(cards, "idscan[filecount]")
  expect_identical(parity_squish(rows[[2]]), c("Question", stem))
  expect_identical(parity_squish(rows[[3]]),
                   c("Value", rep("Number of files uploaded", 2L)))
  expect_length(rows, 3L)
  grid <- parity_table_grid(parity_render(lss, "table", key = "rc"))
  k <- which(grid[, "Variable"] == "idscan")
  expect_identical(unname(grid[k + 1L, "Variable"]), "idscan[filecount]")
  expect_identical(
    unname(parity_squish(grid[k + 1L, ])),
    c("File count", unname(grid[k, "No"]), "idscan[filecount]", "Number", "No",
      unname(parity_squish(grid[k, "Filter"])), "[num]", stem)
  )
})

test_that("table: the Field column fits File count in the five chrome languages", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- rc_lss()
  size <- lss_render_theme()$size_meta
  need <- function(label) {
    th <- lss_render_theme()
    (lss_q_text_width(label, th$font_body, size, bold = TRUE) + 6 + 1) / 72
  }
  for (lg in c("en", "fr", "de", "es", "it")) {
    out <- tempfile(fileext = ".docx")
    suppressMessages(render_documentation(
      lss, out, languages = TA_LANGS, template = "table", chrome_lang = lg,
      show_audit = FALSE
    ))
    doc <- parity_read_docx(out)
    unlink(out)
    label <- lss_chrome_strings(lg)$item_file_count
    tbls <- xml2::xml_find_all(doc$xml, "//w:tbl[w:tblGrid[count(w:gridCol) = 9]]",
                               parity_w)
    expect_length(tbls, 1L)
    field <- vapply(parity_table_rows(tbls[[1]]), function(r) r[[1]], "")
    expect_true(label %in% field, info = lg)
    g <- as.numeric(xml2::xml_attr(
      xml2::xml_find_all(tbls[[1]], "w:tblGrid/w:gridCol", parity_w), "w"
    ))
    widest <- max(vapply(strsplit(label, " ", fixed = TRUE)[[1]], need, 0))
    expect_gte(g[[1]], floor(widest * 1440))
    expect_lte(abs(sum(g) - lss_content_width_in("A4-landscape") * 1440), 8)
  }
})

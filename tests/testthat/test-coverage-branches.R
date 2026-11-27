# Branches that covr 4 counts on their own: the `else` of a one-line `if`
# and the arms of a `switch()` that the rest of the suite never reaches.
# Each test drives the branch through the function that owns it and checks
# what that branch produces.

# A small survey with a value the tests can edit, parsed through write/read.
branch_small_lss <- function() {
  spec <- lss_example_spec(kinds = c("single", "multiple", "text"),
                           languages = c("fr", "de"))
  file <- tempfile(fileext = ".lss")
  on.exit(unlink(file), add = TRUE)
  suppressMessages(write_lss(spec, file))
  read_lss(file)
}

# The value cell next to a label cell of a rendered table.
branch_cell_value <- function(doc, label) {
  sm <- officer::docx_summary(doc)
  cells <- sm[sm$content_type %in% "table cell", , drop = FALSE]
  hit <- cells[cells$text %in% label & cells$cell_id == 1L, , drop = FALSE]
  if (!nrow(hit)) return(NA_character_)
  row <- cells[cells$row_id == hit$row_id[[1L]] & cells$cell_id == 2L, ,
               drop = FALSE]
  row$text[[1L]]
}

# ---- as_lss_spec.R -----------------------------------------------------------

test_that("conv_structural names a subquestion by its code and a quota setting by its quota", {
  lss <- list(
    questions = data.frame(qid = "10", title = "Q1", stringsAsFactors = FALSE),
    subquestions = data.frame(qid = "11", parent_qid = "10", title = "SQ001",
                              relevance = "Q1 == 'A1'",
                              stringsAsFactors = FALSE),
    quota_languagesettings = data.frame(quotals_quota_id = "7",
                                        quotals_language = "en",
                                        quotals_urldescrip = "See the site",
                                        stringsAsFactors = FALSE))
  st <- conv_state()
  conv_structural(st, lss)
  refused <- conv_table(st)
  expect_identical(refused$code, "SQ001")
  expect_identical(refused$item, "subquestions.relevance")
  expect_length(st$notes, 1L)
  expect_match(st$notes, "quota_languagesettings.quotals_urldescrip", fixed = TRUE)
  expect_match(st$notes, "on quota 7", fixed = TRUE)
})

test_that("a question without a code is refused under its qid", {
  lss <- branch_small_lss()
  i <- which(lss$questions$title == "Q03Text")
  qid <- lss$questions$qid[[i]]
  lss$questions$title[[i]] <- ""
  out <- lss_unconvertible(lss)
  row <- out[out$code == paste0("qid ", qid), , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_identical(row$item, "code")
})

test_that("an orphan question without a code is refused under its qid", {
  lss <- branch_small_lss()
  i <- which(lss$questions$title == "Q03Text")
  qid <- lss$questions$qid[[i]]
  lss$questions$title[[i]] <- ""
  lss$questions$gid[[i]] <- "999999"
  out <- lss_unconvertible(lss)
  row <- out[out$code == paste0("qid ", qid), , drop = FALSE]
  expect_identical(nrow(row), 1L)
  expect_identical(row$item, "group")
  expect_match(row$reason, "belongs to no group")
})

test_that("a quota without a name is refused under its id", {
  lss <- branch_small_lss()
  lss$quotas$name <- ""
  lss$quotas$action <- "2"
  out <- lss_unconvertible(lss)
  row <- out[out$item == "quota", , drop = FALSE]
  expect_identical(row$code, paste0("quota ", lss$quotas$id[[1L]]))
  expect_match(row$reason, "terminate action")
})

# ---- audit_lss.R -------------------------------------------------------------

test_that("lss_audit_orphans flags every subquestion of a survey without questions", {
  findings <- lss_finding_collector()
  lss <- list(questions = NULL,
              subquestions = data.frame(qid = "11", parent_qid = "10",
                                        title = "SQ001",
                                        stringsAsFactors = FALSE))
  lss_audit_orphans(findings, lss)
  out <- findings$as_data_frame()
  expect_identical(out$check, "orphan_subquestion")
  expect_identical(out$location, "Subquestion 'SQ001'")
})

test_that("print.lss_audit with n = Inf lists every finding and its language", {
  df <- data.frame(
    severity = c("error", "warning"),
    check = c("c1", "c2"),
    location = c("Question 'Q1'", "Question 'Q2'"),
    language = c(NA, "fr"),
    message = c("first problem", "second problem"),
    stringsAsFactors = FALSE)
  audit <- structure(
    list(file = "fake.lss", languages = c("en", "fr"), n_findings = 2L,
         n_errors = 1L, n_warnings = 1L, n_notes = 0L, findings = df),
    class = "lss_audit")
  out <- cli::cli_fmt(print(audit, n = Inf))
  expect_true(any(grepl("Question 'Q2' [fr]", out, fixed = TRUE)))
  expect_true(any(grepl("first problem", out, fixed = TRUE)))
  expect_false(any(grepl("more finding", out, fixed = TRUE)))
})

# ---- chrome_strings.R --------------------------------------------------------

test_that("lss_chrome_strings falls back to English for an unknown language", {
  expect_identical(lss_chrome_strings("pt"), lss_chrome_strings("en"))
})

test_that("lss_localized_type_label maps the remaining type codes", {
  theme <- list(chrome = lss_chrome_strings("fr"))
  ch <- theme$chrome
  label <- function(type) lss_localized_type_label(list(type = type), theme)
  expect_identical(label("H"), ch$type_single_choice)
  expect_identical(label("P"), ch$type_multiple_choice_with_comment)
  expect_identical(label("|"), ch$type_file_upload)
  expect_identical(label("I"), ch$type_display)
})

# ---- read_form_docx.R --------------------------------------------------------

branch_form_ctx <- function() {
  ctx <- form_new_ctx("fixture.docx", collect = FALSE)
  ctx$languages <- "fr"
  ctx$where <- "Survey block"
  ctx
}

test_that("a Languages row holding only separators is refused on its own text", {
  ctx <- branch_form_ctx()
  raw <- list(list(field = "languages", lang = NA_character_, lines = " , ; ",
                   row = 1L, numbered = FALSE))
  err <- expect_error(form_set_languages(raw, ctx),
                      class = "lssdoc_bad_form_value")
  expect_match(conditionMessage(err), "is not a language code", fixed = TRUE)
  expect_identical(err$form_field, ctx$chrome$cover_languages)
})

test_that("form_check_count reads a missing minimum as one", {
  ctx <- branch_form_ctx()
  err <- expect_error(form_check_count(list(), NA, ctx, "Options", "single"),
                      class = "lssdoc_bad_form_value")
  expect_match(conditionMessage(err), "needs at least 1", fixed = TRUE)
  expect_null(form_check_count(list("a"), NA, ctx, "Options", "single"))
})

test_that("a spec refusal on the question text is reported under the form's word", {
  ctx <- branch_form_ctx()
  groups <- list(list(title = list(fr = "G"),
                      questions = list(list(code = "Q1", kind = "text",
                                            text = list(fr = "")))))
  err <- expect_error(
    form_assemble(list(title = list(fr = "T")), groups, list(), ctx),
    class = "lssdoc_bad_form_spec")
  expect_identical(err$form_code, "Q1")
  expect_identical(err$form_field, form_label_of("question", "wording", ctx$chrome))
})

test_that("a spec refusal that names no question is placed on the whole form", {
  ctx <- branch_form_ctx()
  groups <- list(list(title = list(fr = "G"),
                      questions = list(list(code = "Q1", kind = "text",
                                            text = list(fr = "Question ?")))))
  err <- expect_error(
    form_assemble(list(title = list(fr = "")), groups, list(), ctx),
    class = "lssdoc_bad_form_spec")
  expect_true(is.na(err$form_block))
  expect_true(is.na(err$form_code))
  expect_match(conditionMessage(err), "^The form")
})

test_that("print.lss_form_check with n = Inf lists every problem", {
  ctx <- form_new_ctx("form.docx", collect = TRUE)
  ctx$problems <- list(
    list(severity = "error", class = "lssdoc_bad_form_value",
         block = "Question Q1", code = "Q1", field = "Type",
         message = "first problem"),
    list(severity = "warning", class = "lssdoc_warning",
         block = NA_character_, code = NA_character_, field = NA_character_,
         message = "second problem"))
  check <- form_check_result(ctx)
  out <- cli::cli_fmt(print(check, n = Inf))
  expect_true(any(grepl("Question Q1: first problem", out, fixed = TRUE)))
  expect_true(any(grepl("second problem", out, fixed = TRUE)))
  expect_false(any(grepl("more problem", out, fixed = TRUE)))
})

# ---- read_lss.R --------------------------------------------------------------

test_that("print.lss counts zero for the tables a survey does not carry", {
  lss <- branch_small_lss()
  lss$subquestions <- NULL
  lss$answers <- NULL
  out <- cli::cli_fmt(print(lss))
  counts <- out[grepl("Counts", out, fixed = TRUE)]
  expect_match(counts, "Subquestions 0", fixed = TRUE)
  expect_match(counts, "Answers 0", fixed = TRUE)
})

# ---- render_cover.R ----------------------------------------------------------

test_that("the cover shows zero counts and the empty marker for a bare survey", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  lss <- branch_small_lss()
  model <- lss_model(lss)
  theme <- lss_render_theme()
  theme$chrome <- lss_chrome_strings("en")
  lss$subquestions <- NULL
  lss$answers <- NULL
  lss$surveys$sid <- ""
  doc <- lss_render_cover(officer::read_docx(), lss, model, theme)
  ch <- theme$chrome
  expect_identical(branch_cell_value(doc, ch$cover_subquestions), "0")
  expect_identical(branch_cell_value(doc, ch$cover_answer_options), "0")
  expect_identical(branch_cell_value(doc, ch$cover_survey_id), theme$empty_marker)
})

# ---- render_toc.R ------------------------------------------------------------

test_that("the quota table spells out an inactive quota and every action", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  lss <- demo_lss()
  theme <- lss_render_theme()
  theme$chrome <- lss_chrome_strings("en")
  ch <- theme$chrome
  lss$quotas$active <- c("0", "1")
  lss$quotas$action <- c("2", "9")
  lss$quota_members <- NULL
  doc <- lss_render_quotas(officer::read_docx(), lss, "en", theme)
  sm <- officer::docx_summary(doc)
  cells <- sm[sm$content_type %in% "table cell" & sm$is_header %in% FALSE, ]
  # row 1 is the header: the two quotas sit on rows 2 and 3
  name_1 <- cells$text[cells$row_id == 2L & cells$cell_id == 1L]
  expect_match(name_1, ch$quota_inactive, fixed = TRUE)
  expect_identical(cells$text[cells$row_id == 2L & cells$cell_id == 3L],
                   ch$quota_action_confirm)
  expect_identical(cells$text[cells$row_id == 3L & cells$cell_id == 3L], "9")
  expect_identical(cells$text[cells$cell_id == 4L],
                   rep(theme$empty_marker, 2L))
})

test_that("the consent box stays empty in a language without a label row", {
  skip_if_not_installed("officer"); skip_if_not_installed("flextable")
  lss <- demo_lss()
  theme <- lss_render_theme()
  theme$chrome <- lss_chrome_strings("en")
  doc <- lss_render_consent(officer::read_docx(), lss, c("en", "xx"), theme)
  sm <- officer::docx_summary(doc)
  label <- lss$survey_language_settings$surveyls_policy_notice_label[
    lss$survey_language_settings$surveyls_language == "en"]
  body <- sm[sm$content_type %in% "table cell" & sm$is_header %in% FALSE, ]
  row <- body[vapply(body$text, function(t) grepl(label, t, fixed = TRUE),
                     logical(1)), , drop = FALSE]
  expect_identical(nrow(row), 1L)
  other <- body[body$row_id == row$row_id & body$cell_id == 2L, , drop = FALSE]
  expect_identical(other$text, "")
})

# ---- write_lss.R -------------------------------------------------------------

test_that("loc_strict returns the first element of an unnamed list", {
  expect_identical(loc_strict(list("CHF"), "fr", "prefix"), "CHF")
})

test_that("add_section writes an empty cell for a field a row lacks", {
  doc <- xml2::xml_new_root("document")
  add_section(doc, "answers", c("aid", "code"), list(list(aid = 1L)))
  row <- xml2::xml_find_first(doc, "/document/answers/rows/row")
  expect_identical(xml2::xml_text(xml2::xml_find_first(row, "aid")), "1")
  expect_identical(xml2::xml_text(xml2::xml_find_first(row, "code")), "")
})

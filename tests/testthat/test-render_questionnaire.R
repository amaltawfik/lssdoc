# The readable questionnaire, render_questionnaire() (0.4.0, issue #9).
#
# The fixture is the package's example survey with one question of each
# authorable kind, in English (base) and French, edited after parsing into
# what the design of #9 asks a test survey to hold: a group description (the
# example has one per group), a shown equation (Q13), a hidden equation
# (Q17), a hidden short text (Q14), a mandatory (Q01) and a soft-mandatory
# (Q02) question, a prefix, a suffix and a validation tip (Q16), a filter that
# simplifies (Q02: Q01Single = 1) and one that does not (Q05: count(...)),
# lists of 12 (Q10, ranking), 13 (Q01) and 14 (Q02) options, a missing French
# text (Q16) and a missing French option label (Q01, option 3), and the types
# the demo survey lacks: P (Q04), H (Q05), | (Q15) and I (Q18).

q_fixture_lss <- function() {
  hit <- lss_fixture_cache[["readable"]]
  if (!is.null(hit)) {
    return(hit)
  }
  spec <- lss_example_spec(kinds = lss_kinds$kind, languages = c("en", "fr"))
  file <- tempfile(fileext = ".lss")
  on.exit(unlink(file), add = TRUE)
  suppressMessages(write_lss(spec, file))
  lss <- read_lss(file)
  qs <- lss$questions
  qid <- function(code) qs$qid[qs$title == code]
  set_type <- function(code, type) {
    qs$type[qs$title == code] <<- type
  }
  set_type("Q13Text", "*")
  set_type("Q17Date", "*")
  set_type("Q04Multiple", "P")
  set_type("Q05Array", "H")
  set_type("Q15Hugetext", "|")
  set_type("Q18Yesno", "I")
  qs$mandatory[qs$title == "Q02Dropdown"] <- "S"
  qs$other[qs$title %in% c("Q01Single", "Q02Dropdown")] <- "N"
  lss$questions <- qs

  # Option lists of 13, 14 and 12.
  options_of <- function(lss, code, n) {
    id <- qid(code)
    a <- lss$answers
    l <- lss$answer_l10ns
    tmpl <- a[a$qid == id, , drop = FALSE][1, ]
    old <- a$aid[a$qid == id]
    a <- a[a$qid != id, , drop = FALSE]
    l <- l[!l$aid %in% old, , drop = FALSE]
    start <- max(as.integer(c(lss$answers$aid, 0))) + 1L
    new <- tmpl[rep(1L, n), , drop = FALSE]
    new$aid <- as.character(start + seq_len(n) - 1L)
    new$code <- as.character(seq_len(n))
    new$sortorder <- as.character(seq_len(n) - 1L)
    lab <- do.call(rbind, lapply(c("en", "fr"), function(lg) {
      data.frame(
        id = paste0(lg, new$aid), aid = new$aid,
        answer = paste(if (lg == "en") "Option" else "Modalite", letters[seq_len(n)]),
        language = lg, stringsAsFactors = FALSE
      )
    }))
    lss$answers <- rbind(a, new)
    lss$answer_l10ns <- rbind(l, lab[, names(l)])
    lss
  }
  lss <- options_of(lss, "Q01Single", 13L)
  lss <- options_of(lss, "Q02Dropdown", 14L)
  lss <- options_of(lss, "Q10Ranking", 12L)

  # A French option label and a French question text left empty.
  al <- lss$answer_l10ns
  a3 <- lss$answers$aid[lss$answers$qid == qid("Q01Single") &
                          lss$answers$code == "3"]
  al$answer[al$aid == a3 & al$language == "fr"] <- ""
  lss$answer_l10ns <- al
  ql <- lss$question_l10ns
  ql$question[ql$qid == qid("Q16Numeric") & ql$language == "fr"] <- ""
  lss$question_l10ns <- ql

  qa <- lss$question_attributes
  add <- data.frame(
    qid = c(qid("Q14Shorttext"), qid("Q17Date"), rep(qid("Q16Numeric"), 6)),
    attribute = c("hidden", "hidden", "prefix", "prefix", "suffix", "suffix",
                  "em_validation_q_tip", "em_validation_q_tip"),
    value = c("1", "1", "CHF", "CHF", "per month", "par mois",
              "A whole number.", "Un nombre entier."),
    language = c("", "", "en", "fr", "en", "fr", "en", "fr"),
    stringsAsFactors = FALSE
  )
  lss$question_attributes <- rbind(qa, add[, names(qa)])
  lss_fixture_cache[["readable"]] <- lss
  lss
}

# Render quietly: no once-per-session message, no success alert.
q_render <- function(lss, ...) {
  out <- tempfile(fileext = ".docx")
  suppressMessages(render_questionnaire(lss, out, ...))
  out
}

# The XML parts of a rendered document.
q_parts <- function(path) {
  dir <- tempfile()
  utils::unzip(path, exdir = dir)
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  read <- function(f) xml2::read_xml(file.path(dir, "word", f))
  headers <- list.files(file.path(dir, "word"), pattern = "^header[0-9]*[.]xml$")
  list(
    doc = read("document.xml"),
    styles = read("styles.xml"),
    settings = read("settings.xml"),
    headers = vapply(headers, function(h) {
      paste(as.character(read(h)), collapse = "")
    }, character(1))
  )
}

W <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

# Text of each body paragraph, a tab as "\t".
q_paras <- function(doc) {
  vapply(xml2::xml_find_all(doc, "//w:body/w:p", W), function(p) {
    bits <- xml2::xml_find_all(p, ".//w:r/w:t | .//w:r/w:tab", W)
    paste(ifelse(xml2::xml_name(bits) == "tab", "\t", xml2::xml_text(bits)),
          collapse = "")
  }, character(1))
}

# Question number -> code, read from the question lines: the first run is a
# bare number in bold, the code is the run set in the code font.
q_numbered <- function(doc, font_code = "Consolas") {
  out <- character(0)
  for (p in xml2::xml_find_all(doc, "//w:body/w:p", W)) {
    r <- xml2::xml_find_first(p, "w:r[w:rPr/w:b[@w:val='true']]/w:t", W)
    first <- xml2::xml_text(r)
    if (is.na(first) || !grepl("^[0-9]+$", first)) next
    code_run <- xml2::xml_find_first(
      p, sprintf("w:r[w:rPr/w:rFonts/@w:ascii='%s']/w:t", font_code), W
    )
    out[[first]] <- trimws(xml2::xml_text(code_run))
  }
  out
}

# ---- Pure helpers (run on CRAN) ---------------------------------------------

test_that("the chrome of the readable questionnaire exists in five languages", {
  keys <- c(
    "q_section_welcome", "q_section_end",
    paste0(rep(c("q_n_question", "q_n_group", "q_n_display", "q_n_computed",
                 "q_n_hidden"), each = 2), c("_1", "_n")),
    "q_legend_in_fmt", "q_and", "q_legend_codes", "q_legend_mandatory",
    "q_legend_soft", "q_legend_language_fmt", "q_toc_question_fmt",
    "q_toc_questions_fmt", "q_tag_text", "q_tag_computed", "q_tag_hidden",
    "q_equation_hidden", "q_equation_shown", "q_several_answers",
    "q_several_comment", "q_ranking", "q_for_each_row", "q_answer",
    "q_options_total_fmt", "q_no_label", "q_other", "q_free_text",
    "q_filter_if_fmt", "q_filtered", "q_not_translated",
    "q_open_number", "q_open_short_text", "q_open_long_text", "q_open_date",
    "q_open_numbers", "q_open_short_texts", "q_open_number_range_fmt",
    "q_open_number_step_fmt", "q_open_checkbox", "q_columns",
    "q_for_each_cell", "q_open_file",
    "q_open_language", "q_open_free"
  )
  for (lg in c("en", "fr", "de", "es", "it")) {
    pack <- lss_chrome_strings(lg)
    expect_identical(setdiff(keys, names(pack)), character(0), info = lg)
    expect_true(all(vapply(pack[keys], function(v) {
      is.character(v) && length(v) == 1L && nzchar(v)
    }, logical(1))), info = lg)
    # The N-point choices print their positions, no longer a phrase.
    expect_false(any(c("q_open_five_point", "q_open_ten_point") %in%
                       names(pack)), info = lg)
  }
  fr <- lss_chrome_strings("fr")
  expect_identical(fr$q_answer, "R\u00E9ponse\u00A0:")
  expect_identical(fr$item_hidden, "Question masqu\u00E9e aux r\u00E9pondant\u00B7es")
  de <- lss_chrome_strings("de")
  expect_identical(de$item_hidden, "Frage f\u00FCr Befragte versteckt")
  expect_identical(de$cover_hidden_questions, "Versteckte Fragen")
})

test_that("the legend counts what the document prints", {
  en <- lss_chrome_strings("en")
  counts <- list(questions = 44L, groups = 6L, displays = 2L, computed = 1L,
                 hidden = 0L)
  expect_identical(
    lss_q_legend(en, counts, show_codes = FALSE),
    "44 questions in 6 groups, 2 text displays and 1 computed value."
  )
  one <- list(questions = 1L, groups = 1L, displays = 0L, computed = 0L,
              hidden = 1L)
  expect_identical(
    lss_q_legend(en, one, show_codes = FALSE),
    "1 question in 1 group and 1 hidden question."
  )
  bare <- list(questions = 3L, groups = 2L, displays = 0L, computed = 0L,
               hidden = 0L)
  expect_identical(lss_q_legend(en, bare, show_codes = FALSE),
                   "3 questions in 2 groups.")
  full <- lss_q_legend(en, counts, show_codes = TRUE, mandatory = TRUE,
                       soft = TRUE, language = "de")
  expect_identical(
    full,
    paste(
      "44 questions in 6 groups, 2 text displays and 1 computed value.",
      en$q_legend_codes, en$q_legend_mandatory, en$q_legend_soft,
      "Language: de."
    )
  )
  four <- list(questions = 5L, groups = 2L, displays = 1L, computed = 2L,
               hidden = 3L)
  expect_identical(
    lss_q_legend(lss_chrome_strings("fr"), four, show_codes = FALSE),
    paste0("5 questions en 2 groupes, 1 texte affich\u00E9, 2 calculs et ",
           "3 questions masqu\u00E9es.")
  )
})

test_that("a list is abridged only when that makes it shorter", {
  expect_identical(lss_q_abridge(12L, 12), 1:12)
  expect_identical(lss_q_abridge(13L, 12), 1:13)
  expect_identical(lss_q_abridge(14L, 12), c(1:6, NA, 9:14))
  expect_identical(lss_q_abridge(247L, 12), c(1:6, NA, 242:247))
  expect_identical(lss_q_abridge(30L, 20), c(1:10, NA, 21:30))
  expect_identical(lss_q_abridge(9L, 5), c(1:3, NA, 8:9))
  expect_identical(lss_q_abridge(500L, Inf), seq_len(500L))
  expect_identical(lss_q_abridge(0L, 12), integer(0))
})

test_that("widths are measured, or estimated without gdtools", {
  # Estimate: 0.56 em per character in the body font, 0.61 in the code font.
  expect_equal(lss_q_text_width(c("ab", "abcd"), "Calibri", 10, measure = FALSE),
               4 * 0.56 * 10)
  expect_equal(lss_q_text_width("abcd", "Consolas", 10, code = TRUE,
                                measure = FALSE), 4 * 0.61 * 10)
  expect_identical(lss_q_text_width(c(NA, ""), "Calibri", 10), 0)
  # The left column is 2.8 * base_size at least, wider for a long tag.
  expect_identical(lss_q_margin_width("9", "text", "Calibri", 10L,
                                      measure = FALSE), 28)
  wide <- lss_q_margin_width("44", "berechnet", "Calibri", 10L, measure = FALSE)
  expect_identical(wide, ceiling(9 * 0.56 * 8 + 6))
  expect_gt(wide, 28)
  expect_identical(lss_q_code_width(c("", NA), "Calibri", 10L), 0)
  expect_identical(lss_q_code_width("-oth-", "Calibri", 10L, measure = FALSE),
                   ceiling(5 * 0.56 * 8) + 5)
  skip_if_not_installed("gdtools")
  measured <- lss_q_text_width("berechnet", "Calibri", 8)
  expect_gt(measured, 20)
  expect_lt(measured, 60)
  # A failing measure falls back on the estimate.
  local_mocked_bindings(
    strings_sizes = function(...) stop("no font"), .package = "gdtools"
  )
  expect_equal(lss_q_text_width("abcd", "Calibri", 10, measure = TRUE),
               4 * 0.56 * 10)
})

test_that("the proofing language derives from the content language", {
  expect_identical(lss_q_proofing_lang("fr"), "fr-FR")
  expect_identical(lss_q_proofing_lang("de"), "de-DE")
  expect_identical(lss_q_proofing_lang("it"), "it-IT")
  expect_identical(lss_q_proofing_lang("es"), "es-ES")
  expect_identical(lss_q_proofing_lang("en"), "en-GB")
  expect_identical(lss_q_proofing_lang("de-CH"), "de-CH")
  expect_identical(lss_q_proofing_lang("pt-BR"), "pt-BR")
  expect_identical(lss_q_proofing_lang("zh-Hant-TW"), "zh-Hant-TW")
  expect_identical(lss_q_proofing_lang("de-informal"), "de-DE")
  expect_identical(lss_q_proofing_lang("nl"), "nl")
  expect_identical(lss_q_proofing_lang("fr", "fr-CH"), "fr-CH")
  # The chrome runs declare their language only when it differs.
  expect_null(lss_q_chrome_tag("fr", "fr-CH"))
  expect_identical(lss_q_chrome_tag("fr", "de-DE"), "fr-FR")
  expect_identical(lss_q_chrome_tag("en", "ja"), "en-GB")
})

test_that("a filter is printed plainly, or as 'filtered' when it cannot be", {
  theme <- list(chrome = lss_chrome_strings("en"))
  expect_null(lss_q_filter(NA, theme))
  expect_null(lss_q_filter("1", theme))
  expect_null(lss_q_filter("((1))", theme))
  expect_identical(lss_q_filter("Q11.NAOK == \"2\"", theme), "if Q11 = 2")
  expect_identical(
    lss_q_filter("Q1.NAOK == 1 || Q1.NAOK == 2", theme),
    "if Q1 \u2208 {1, 2}"
  )
  expect_identical(lss_q_filter("regexMatch(\"^a\", Q1.NAOK)", theme),
                   "if Q1 matches \"^a\"")
  expect_identical(
    lss_q_filter("count(Q4_1.NAOK, Q4_2.NAOK) >= 2", theme), "filtered"
  )
  expect_identical(lss_q_filter("Q1.shown == \"x\"", theme), "filtered")
  expect_identical(lss_q_filter("!(Q1.NAOK == 1)", theme), "filtered")
  fr <- list(chrome = lss_chrome_strings("fr"))
  expect_identical(lss_q_filter("Q11.NAOK == 2", fr), "si Q11 = 2")
})

test_that("texts lose their HTML, and French gets its no-break spaces", {
  expect_identical(lss_q_clean(NA), "")
  expect_identical(lss_q_clean("<p>Hello <b>you</b></p><p>there</p>"),
                   "Hello you there")
  expect_identical(lss_q_clean("Vraiment ?", french = TRUE),
                   "Vraiment\u00A0?")
  expect_identical(lss_q_clean("Vraiment ?"), "Vraiment ?")
  expect_identical(lss_q_french("\u00AB oui \u00BB"),
                   "\u00AB\u00A0oui\u00A0\u00BB")
  paras <- lss_q_html_paras(
    "<p><strong>Title</strong></p><p>One<br>Two</p><ul><li>item</li></ul>"
  )
  expect_identical(vapply(paras, `[[`, "", "text"),
                   c("Title", "One", "Two", "\u2022 item"))
  expect_identical(vapply(paras, `[[`, NA, "bold"),
                   c(TRUE, FALSE, FALSE, FALSE))
  # An ordered list: each item its number, a nested unordered list its
  # bullet, the parent goes on counting; the second line of an item has
  # neither.
  paras <- lss_q_html_paras(paste0(
    "<ol start=\"2\"><li>a<br>more</li><li>b<ul><li>x</li></ul></li>",
    "<li>c</li></ol>"
  ))
  expect_identical(vapply(paras, `[[`, "", "text"),
                   c("2. a", "more", "3. b", "\u2022 x", "4. c"))
  expect_identical(lss_q_html_paras(""), list())
  expect_identical(lss_q_html_paras("<p> </p>"), list())
  expect_identical(lss_q_html_paras("Vous ?", french = TRUE)[[1]]$text,
                   "Vous\u00A0?")
  expect_identical(lss_q_strip_code("Very satisfied 10", "10"),
                   "Very satisfied")
  expect_identical(lss_q_strip_code("10", "10"), "10")
  expect_identical(lss_q_strip_code("", "1"), "")
  expect_identical(lss_q_or(NULL, "x"), "x")
  expect_identical(lss_q_or(NA, "x"), "x")
  expect_identical(lss_q_or(2L, "x"), "2")
})

test_that("old arguments name render_documentation(), others are refused", {
  lss <- q_fixture_lss()
  out <- tempfile(fileext = ".docx")
  expect_error(
    render_questionnaire(lss, out, template = "table"),
    class = "lssdoc_function_renamed"
  )
  err <- expect_error(
    render_questionnaire(lss, out, languages = c("en", "fr")),
    class = "lssdoc_function_renamed"
  )
  msg <- conditionMessage(err)
  expect_match(msg, "render_documentation", fixed = TRUE)
  expect_match(msg, "language", fixed = TRUE)
  expect_match(msg, "in one language", fixed = TRUE)
  err <- expect_error(
    render_questionnaire(lss, out, show_audit = FALSE),
    class = "lssdoc_function_renamed"
  )
  expect_no_match(conditionMessage(err), "Its argument")
  expect_error(render_questionnaire(lss, out, colour = "red"),
               class = "lssdoc_unknown_argument")
  expect_error(
    render_questionnaire(lss, out, "fr", TRUE, TRUE, TRUE, TRUE, TRUE, TRUE,
                         TRUE, TRUE, TRUE, TRUE, 12L, NULL, NULL, NULL, 1.5,
                         0.75,
                         NULL, NULL, NULL, NULL, NULL, 10L, "extra"),
    class = "lssdoc_unknown_argument"
  )
  expect_false(file.exists(out))
})

test_that("the arguments are validated before anything is rendered", {
  lss <- q_fixture_lss()
  out <- tempfile(fileext = ".docx")
  err <- expect_error(render_questionnaire(lss, out, language = c("en", "fr")),
                      class = "lssdoc_one_language")
  expect_match(conditionMessage(err), "render_documentation", fixed = TRUE)
  expect_error(render_questionnaire(lss, out, language = "de"),
               class = "lssdoc_unknown_language")
  expect_error(render_questionnaire(lss, out, language = NA_character_),
               class = "lssdoc_bad_language")
  expect_error(render_questionnaire(lss, out, language = 1),
               class = "lssdoc_bad_language")
  expect_error(render_questionnaire(lss, 1), class = "lssdoc_bad_output")
  expect_error(render_questionnaire(lss, "x.odt"),
               class = "lssdoc_bad_output_ext")
  expect_error(render_questionnaire(lss, out, show_codes = NA),
               class = "lssdoc_bad_flag")
  expect_error(render_questionnaire(lss, out, show_toc = "yes"),
               class = "lssdoc_bad_flag")
  for (bad in list(1, 2.5, -Inf, NA, "12", c(3, 4))) {
    expect_error(render_questionnaire(lss, out, max_options = bad),
                 class = "lssdoc_bad_max_options")
  }
  expect_error(render_questionnaire(lss, out, title = 1),
               class = "lssdoc_bad_title")
  expect_error(render_questionnaire(lss, out, subtitle = c("a", "b")),
               class = "lssdoc_bad_subtitle")
  expect_error(render_questionnaire(lss, out, proofing_lang = "fr CH"),
               class = "lssdoc_bad_proofing_lang")
  expect_error(render_questionnaire(lss, out, proofing_lang = 1),
               class = "lssdoc_bad_proofing_lang")
  expect_error(render_questionnaire(lss, out, chrome_lang = "ja"),
               class = "lssdoc_bad_chrome_lang")
  expect_error(render_questionnaire(lss, out, base_size = 30),
               class = "lssdoc_bad_base_size")
  expect_error(render_questionnaire(lss, out, colors = list(prim = "#000")),
               class = "lssdoc_bad_colors")
  expect_error(render_questionnaire(lss, out, font = ""),
               class = "lssdoc_bad_font")
  expect_error(render_questionnaire(lss, out, logo = tempfile()),
               class = "lssdoc_logo_not_found")
  expect_error(render_questionnaire(42, out), class = "lssdoc_bad_input")
  expect_false(file.exists(out))
})

test_that("texts fall back on the base language and are recorded", {
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  ctx <- lss_q_context(lss, "fr")
  x <- lss_q_pick(ctx, list(fr = "", en = "<b>Hello</b>"), "Q1")
  expect_identical(x$text, "Hello")
  expect_true(x$missing)
  expect_identical(ctx$missing, "Q1")
  none <- lss_q_pick(ctx, list(fr = NA, en = NA), "Q2")
  expect_identical(none$text, "")
  attrs <- data.frame(
    qid = "1", attribute = c("prefix", "suffix", "suffix"),
    value = c("CHF", "per month", ""), language = c("", "en", "fr"),
    stringsAsFactors = FALSE
  )
  expect_identical(lss_q_attr(ctx, attrs, "prefix", "Q3")$text, "CHF")
  suf <- lss_q_attr(ctx, attrs, "suffix", "Q3")
  expect_identical(suf$text, "per month")
  expect_true(suf$missing)
  expect_identical(lss_q_attr(ctx, attrs, "other", "Q3")$text, "")
  expect_identical(lss_q_attr(ctx, NULL, "prefix", "Q3")$text, "")
  ctx$lss$survey_language_settings <- NULL
  expect_identical(lss_q_survey_text(ctx, "surveyls_welcometext", "w")$text, "")
  expect_identical(lss_q_title(ctx, ""), "(untitled survey)")
  # A survey without a usable base language takes its first language.
  odd <- lss
  odd$base_language <- NA_character_
  expect_identical(lss_q_context(odd, "fr")$base, odd$languages[1])
})

test_that("a run can declare its own language", {
  skip_if_not_installed("officer")
  ctx <- new.env()
  ctx$chrome_tag <- "fr-FR"
  plain <- lss_q_run(ctx, "Antwort", officer::fp_text())
  expect_false(inherits(plain, "lssdoc_lang_run"))
  tagged <- lss_q_run(ctx, "R\u00E9ponse", officer::fp_text(), chrome = TRUE)
  xml <- officer::to_wml(tagged)
  expect_match(xml, "<w:lang w:val=\"fr-FR\"/></w:rPr>", fixed = TRUE)
  bare <- structure(
    list(value = "x", pr = NULL, lang = "de-CH"),
    class = c("lssdoc_lang_run", "ftext", "cot", "run")
  )
  expect_match(officer::to_wml(bare), "<w:r><w:rPr><w:lang w:val=\"de-CH\"/></w:rPr>",
               fixed = TRUE)
})

test_that("paragraph properties go at their place in w:pPr", {
  ppr <- xml2::read_xml(paste0(
    "<w:pPr xmlns:w=\"", W[["w"]], "\"><w:pStyle w:val=\"x\"/>",
    "<w:spacing/><w:jc w:val=\"left\"/></w:pPr>"
  ))
  lss_q_ppr_add(ppr, "keepLines")
  lss_q_ppr_add(ppr, "suppressAutoHyphens")
  lss_q_ppr_add(ppr, "outlineLvl")
  lss_q_ppr_add(ppr, "keepLines")
  expect_identical(
    xml2::xml_name(xml2::xml_children(ppr)),
    c("pStyle", "keepLines", "suppressAutoHyphens", "spacing", "jc",
      "outlineLvl")
  )
  body <- xml2::read_xml(paste0(
    "<w:document xmlns:w=\"", W[["w"]], "\"><w:body><w:p/>",
    "<w:p><w:pPr><w:jc w:val=\"both\"/></w:pPr></w:p></w:body></w:document>"
  ))
  lss_q_finish_paragraphs(body)
  ps <- xml2::xml_find_all(body, "//w:p", W)
  expect_identical(xml2::xml_name(xml2::xml_children(xml2::xml_child(ps[[1]]))),
                   c("keepLines", "suppressAutoHyphens"))
  expect_identical(xml2::xml_name(xml2::xml_children(xml2::xml_child(ps[[2]]))),
                   c("keepLines", "jc"))
})

# Plain text of planned paragraphs, a tab as "\t".
plan_text <- function(paras) {
  vapply(paras, function(p) {
    paste(vapply(p$runs, function(r) {
      if (inherits(r, "run_tab")) "\t" else as.character(r$value)
    }, ""), collapse = "")
  }, "")
}

test_that("each type says what it asks, in the edge cases too", {
  skip_if_not_installed("officer")
  ctx <- lss_q_context(q_fixture_lss(), "fr", chrome_lang = "fr", measure = FALSE)
  tx <- function(fr, en = fr) list(fr = list(question = fr, help = NA),
                                   en = list(question = en, help = NA))
  q <- function(type, ...) {
    utils::modifyList(list(qid = "0", code = "QX", type = type, other = "N",
                           mandatory = "N", relevance = "1", texts = tx("Q ?"),
                           answers = NULL, subquestions = NULL,
                           attributes = NULL), list(...))
  }
  body <- function(...) plan_text(lss_q_body(ctx, q(...)))
  free <- "R\u00E9ponse\u00A0: r\u00E9ponse libre"
  # A list without options, a multiple choice without items.
  expect_identical(body("L"), free)
  expect_identical(body("M"), free)
  expect_identical(lss_q_option_paras(ctx, list()), list())
  # An option without a label in any language, without a scale id.
  blank <- list(code = "1", scale_id = NULL, labels = list(fr = "", en = ""))
  expect_identical(body("L", answers = list(blank)),
                   "1\t(sans libell\u00E9)")
  sq <- list(code = "A", scale_id = NA, texts = tx("Oui", "Yes"))
  expect_identical(body("M", subquestions = list(sq)), "A\tOui")
  # Multiple numbers and short texts without rows.
  expect_identical(body("K"), "R\u00E9ponse\u00A0: plusieurs nombres")
  expect_identical(body("Q"), "R\u00E9ponse\u00A0: plusieurs textes courts")
  # A prefix only in the base language, without a language column.
  attrs <- data.frame(qid = "0", attribute = "prefix", value = "CHF",
                      language = "en", stringsAsFactors = FALSE)
  expect_identical(body("N", attributes = attrs),
                   "R\u00E9ponse\u00A0: CHF [nombre] (non traduit)")
  attrs$language <- NULL
  expect_identical(body("N", attributes = attrs),
                   "R\u00E9ponse\u00A0: CHF [nombre]")
  # Dual scale: one block per scale, a header missing in French.
  ans <- list(
    list(code = "1", scale_id = "0", labels = list(fr = "Un", en = "One")),
    list(code = "1", scale_id = "1", labels = list(fr = "Bas", en = "Low"))
  )
  hdr <- data.frame(qid = "0", attribute = "dualscale_headerA",
                    value = "Trust", language = "en", stringsAsFactors = FALSE)
  expect_identical(
    body("1", answers = ans, subquestions = list(sq), attributes = hdr),
    c("A\tOui", "Pour chaque ligne\u00A0: Trust (non traduit)", "1\tUn",
      "Pour chaque ligne\u00A0:", "1\tBas")
  )
  # Array of texts: the columns listed like the rows, one missing in
  # French, then what a cell takes.
  col <- list(code = "C1", scale_id = "1", texts = tx("", "Today"))
  cell <- "Pour chaque ligne et chaque colonne\u00A0: "
  expect_identical(
    body(";", subquestions = list(sq, col)),
    c("A\tOui", "Colonnes\u00A0:", "C1\tToday (non traduit)",
      paste0(cell, "texte court"))
  )
  # Without any column: no "Columns:" line, "For each row:" says it.
  expect_identical(body(";", subquestions = list(sq)),
                   c("A\tOui", "Pour chaque ligne\u00A0: texte court"))
  expect_identical(body(":", subquestions = list(sq)),
                   c("A\tOui", "Pour chaque ligne\u00A0: nombre de 1 \u00E0 10"))
  # Every type has its "Answer:" string.
  expect_identical(
    vapply(c("N", "K", "S", "Q", "T", "U", "D", "|", "I", "5", "?"),
           lss_q_open_key, "", USE.NAMES = FALSE),
    c("q_open_number", "q_open_numbers", "q_open_short_text",
      "q_open_short_texts", "q_open_long_text", "q_open_long_text",
      "q_open_date", "q_open_file", "q_open_language", "q_open_free",
      "q_open_free")
  )
  expect_identical(lss_q_item(NA, "x")$code, "")
  expect_length(lss_q_builtin_items(ctx, "C"), 3L)
  # The N-point choices: their positions on one line, under the rows and
  # "For each row:" in an array.
  five <- "1\u2003\u20022\u2003\u20023\u2003\u20024\u2003\u20025"
  ten <- paste(1:10, collapse = "\u2003\u2002")
  expect_identical(body("A", subquestions = list(sq)),
                   c("A\tOui", "Pour chaque ligne\u00A0:", five))
  expect_identical(body("B", subquestions = list(sq)),
                   c("A\tOui", "Pour chaque ligne\u00A0:", ten))
  expect_identical(body("5"), five)
  # Yes/no and gender list their built-in options, female first.
  expect_identical(body("Y"), c("Y\tOui", "N\tNon"))
  expect_identical(body("G"), c("F\tFemme", "M\tHomme"))
  # Question line: a text display and a help text missing in French.
  num <- data.frame(status = "display", hidden = FALSE, no = NA_integer_)
  line <- plan_text(lss_q_question(ctx, q("X", texts = tx("", "Hello")), num))
  expect_identical(line, "texte\tHello (non traduit)  QX")
  num <- data.frame(status = "asked", hidden = FALSE, no = 3L)
  helped <- q("S", texts = list(fr = list(question = "Q ?", help = ""),
                                en = list(question = "Q?", help = "Help")))
  expect_identical(plan_text(lss_q_question(ctx, helped, num)),
                   c("3\tQ\u00A0?  QX", "Help (non traduit)",
                     "R\u00E9ponse\u00A0: texte court"))
})

test_that("the positions of an N-point choice read the same with and without codes", {
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  five <- "1\u2003\u20022\u2003\u20023\u2003\u20024\u2003\u20025"
  for (codes in c(TRUE, FALSE)) {
    ctx <- lss_q_context(lss, "fr", chrome_lang = "fr", show_codes = codes,
                         measure = FALSE)
    p <- lss_q_points_para(ctx, 5L)
    expect_identical(p$kind, "line")
    expect_identical(p$w, 0)
    expect_length(p$runs, 1L)
    expect_identical(plan_text(list(p)), five)
    expect_identical(plan_text(list(lss_q_points_para(ctx, 10L))),
                     paste(1:10, collapse = "\u2003\u2002"))
    q5 <- list(qid = "0", code = "QX", type = "5", texts = NULL,
               answers = NULL, subquestions = NULL, attributes = NULL)
    expect_identical(plan_text(lss_q_body(ctx, q5)), five)
  }
})

test_that("a cell of an array of numbers says what it takes, by LimeSurvey's rule", {
  skip_if_not_installed("officer")
  ctx <- lss_q_context(q_fixture_lss(), "fr", chrome_lang = "fr",
                       measure = FALSE)
  cell <- function(..., type = ":") {
    a <- c(...)
    attrs <- if (length(a)) {
      data.frame(qid = "0", attribute = names(a), value = unname(a),
                 language = "", stringsAsFactors = FALSE)
    }
    lss_q_cell_text(ctx, list(code = "QX", type = type, attributes = attrs))
  }
  # An array of texts.
  expect_identical(cell(type = ";"), "texte court")
  # The dropdown: 1 to 10 by default, the maximum alone, the minimum alone.
  expect_identical(cell(), "nombre de 1 \u00E0 10")
  expect_identical(cell(multiflexible_max = "5"), "nombre de 1 \u00E0 5")
  expect_identical(cell(multiflexible_min = "3"), "nombre de 3 \u00E0 13")
  expect_identical(cell(multiflexible_min = "0", multiflexible_max = "10"),
                   "nombre de 0 \u00E0 10")
  # A minimum not below the maximum: back to 1 to 10.
  expect_identical(cell(multiflexible_min = "10", multiflexible_max = "5"),
                   "nombre de 1 \u00E0 10")
  expect_identical(cell(multiflexible_min = "5", multiflexible_max = "5"),
                   "nombre de 1 \u00E0 10")
  # A step other than 1; a step of 0 or below counts as 1.
  expect_identical(
    cell(multiflexible_min = "0", multiflexible_max = "1",
         multiflexible_step = "0.5"),
    "nombre de 0 \u00E0 1, par pas de 0.5"
  )
  expect_identical(cell(multiflexible_step = "0"), "nombre de 1 \u00E0 10")
  expect_identical(cell(multiflexible_step = "-2"), "nombre de 1 \u00E0 10")
  # A value that is not a number counts as not set.
  expect_identical(cell(multiflexible_min = "x", multiflexible_max = "5"),
                   "nombre de 1 \u00E0 5")
  expect_identical(cell(multiflexible_step = "two"), "nombre de 1 \u00E0 10")
  expect_identical(cell(multiflexible_max = ""), "nombre de 1 \u00E0 10")
  # No scientific notation, no trailing zero; reverse changes nothing.
  expect_identical(
    cell(multiflexible_min = "0.50", multiflexible_max = "1000000",
         multiflexible_step = "100000", reverse = "1"),
    "nombre de 0.5 \u00E0 1000000, par pas de 100000"
  )
  # A checkbox, then input boxes, before the dropdown.
  expect_identical(cell(multiflexible_checkbox = "1", input_boxes = "1",
                        multiflexible_max = "5"),
                   "case \u00E0 cocher")
  expect_identical(cell(multiflexible_checkbox = "0", input_boxes = "1"),
                   "nombre")
  expect_identical(cell(input_boxes = "0", multiflexible_max = "4"),
                   "nombre de 1 \u00E0 4")
  # The same rule in English.
  en <- lss_q_context(q_fixture_lss(), "en", chrome_lang = "en",
                      measure = FALSE)
  q <- list(code = "QX", type = ":", attributes = data.frame(
    qid = "0", attribute = "multiflexible_step", value = "2", language = "",
    stringsAsFactors = FALSE
  ))
  expect_identical(lss_q_cell_text(en, q), "number from 1 to 10, in steps of 2")
  expect_identical(lss_q_cell_text(en, list(code = "QX", type = ":")),
                   "number from 1 to 10")
})

test_that("the formula of a computed question: the attribute, else the text", {
  eq <- function(value) {
    data.frame(qid = "0", attribute = c("hidden", "equation"),
               value = c("1", value), language = "", stringsAsFactors = FALSE)
  }
  tx <- list(en = list(question = "<p>{sum(a.NAOK, b.NAOK)}</p>"),
             fr = list(question = ""))
  # The attribute, its outer braces removed, before the text.
  q <- list(attributes = eq(" {round(w / h, 1)} "), texts = tx)
  expect_identical(lss_equation_formula(q, "en"), "round(w / h, 1)")
  expect_identical(lss_equation_formula(q), "round(w / h, 1)")
  # An empty attribute: the text in the language asked for, HTML stripped.
  q$attributes <- eq("  ")
  expect_identical(lss_equation_formula(q, "en"), "sum(a.NAOK, b.NAOK)")
  expect_identical(lss_equation_formula(q, "fr"), "")
  expect_identical(lss_equation_formula(q, "de"), "")
  expect_identical(lss_equation_formula(q), "")
  expect_identical(lss_equation_formula(list(attributes = NULL)), "")
  # The braces go only when the whole string is one expression.
  expect_identical(lss_strip_outer_braces("{a + b}"), "a + b")
  expect_identical(lss_strip_outer_braces("{ {a} }"), "{a}")
  expect_identical(lss_strip_outer_braces("Total: {sum(a,b)} points"),
                   "Total: {sum(a,b)} points")
  expect_identical(lss_strip_outer_braces("{a} + {b}"), "{a} + {b}")
  expect_identical(lss_strip_outer_braces("{a}}"), "{a}}")
  expect_identical(lss_strip_outer_braces("{a"), "{a")
  expect_identical(lss_strip_outer_braces(""), "")
})

test_that("a computed question gives its formula after its code", {
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  attrs <- data.frame(qid = "0", attribute = "equation",
                      value = "{weight / height}", language = "",
                      stringsAsFactors = FALSE)
  q <- function(...) {
    utils::modifyList(list(qid = "0", code = "QX", type = "*", other = "N",
                           mandatory = "N", relevance = "1",
                           texts = list(fr = list(question = "", help = NA)),
                           answers = NULL, subquestions = NULL,
                           attributes = attrs), list(...))
  }
  num <- data.frame(status = "equation", hidden = FALSE, no = NA_integer_)
  shown <- "calcul\taffich\u00E9 aux r\u00E9pondant\u00B7es"
  ctx <- lss_q_context(lss, "fr", chrome_lang = "fr", measure = FALSE)
  expect_identical(plan_text(lss_q_question(ctx, q(), num)),
                   paste0(shown, "  QX = weight / height"))
  # Without a formula, or without the codes: as before.
  expect_identical(
    plan_text(lss_q_question(ctx, q(attributes = NULL), num)),
    paste0(shown, "  QX")
  )
  bare <- lss_q_context(lss, "fr", chrome_lang = "fr", show_codes = FALSE,
                        measure = FALSE)
  expect_identical(plan_text(lss_q_question(bare, q(), num)), shown)
})

test_that("the quotas close the questionnaire, with the codes", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- demo_lss()
  en <- q_paras(q_parts(q_render(lss, language = "en"))$doc)
  k <- which(en == "Quotas")
  expect_length(k, 2L)
  # In the table of contents, without a range, after the end text.
  expect_identical(en[k[1] - 1L], "End of the questionnaire")
  # The section ends the document.
  expect_identical(
    en[k[2]:length(en)],
    c("Quotas",
      "\tfin2  if gender = M \u00B7 Limit: 10",
      "When full: terminate survey",
      "\tSorry your responses have exceeded a quota on this survey.",
      "\tfin  if respondentrole = ST AND education = 3 \u00B7 Limit: 0",
      "When full: terminate survey",
      "\tSorry your responses have exceeded a quota on this survey.")
  )
  fr <- q_paras(q_parts(q_render(lss, language = "fr"))$doc)
  k <- which(fr == "Quotas")
  expect_length(k, 2L)
  expect_identical(
    fr[k[2] + 0:3],
    c("Quotas",
      "\tfin2  si gender = M \u00B7 Limite\u00A0: 10",
      "Quand atteint\u00A0: terminer le questionnaire",
      "\tD\u00E9sol\u00E9 vos r\u00E9ponses d\u00E9passent un quota d\u00E9fini pour ce questionnaire")
  )
  # The heading is a heading: it has its bookmark.
  doc <- q_parts(q_render(lss, language = "en"))$doc
  expect_length(xml2::xml_find_all(
    doc, "//w:bookmarkStart[@w:name='lssdoc_section_quotas']", W
  ), 1L)
  # Its default follows show_codes; show_quotas decides.
  none <- q_paras(q_parts(q_render(lss, language = "en",
                                   show_codes = FALSE))$doc)
  expect_false("Quotas" %in% none)
  only <- q_paras(q_parts(q_render(lss, language = "en", show_codes = FALSE,
                                   show_quotas = TRUE))$doc)
  expect_true("\tfin2  if gender = M \u00B7 Limit: 10" %in% only)
  off <- q_paras(q_parts(q_render(lss, language = "en",
                                  show_quotas = FALSE))$doc)
  expect_false("Quotas" %in% off)
  expect_error(q_render(lss, show_quotas = NA), class = "lssdoc_bad_flag")
})

test_that("each quota: inactive, its action, its message, its members", {
  skip_if_not_installed("officer")
  lss <- demo_lss()
  q <- lss$quotas
  q$active[q$name == "fin2"] <- "0"
  q$action[q$name == "fin2"] <- "2"
  q$action[q$name == "fin"] <- "9"
  lss$quotas <- q
  ls <- lss$quota_languagesettings
  # In German (French is the base language): fin2 has no German message,
  # fin no message at all and a German name; fin loses its members.
  ls$quotals_message[ls$quotals_quota_id == "53" &
                       ls$quotals_language == "de"] <- ""
  ls$quotals_message[ls$quotals_quota_id == "52"] <- ""
  ls$quotals_name[ls$quotals_quota_id == "52" &
                    ls$quotals_language == "de"] <- "Ende <b>der Feldzeit</b>"
  lss$quota_languagesettings <- ls
  lss$quota_members <- lss$quota_members[lss$quota_members$quota_id != "52", ,
                                         drop = FALSE]
  ctx <- lss_q_context(lss, "de", chrome_lang = "fr", measure = FALSE)
  text <- plan_text(lss_q_quota_paras(ctx))
  expect_identical(text, c(
    "Quotas",
    "\tfin2  si gender = M \u00B7 Limite\u00A0: 10 \u00B7 inactif",
    "Quand atteint\u00A0: laisser le r\u00E9pondant modifier",
    paste0("\tD\u00E9sol\u00E9 vos r\u00E9ponses d\u00E9passent un quota ",
           "d\u00E9fini pour ce questionnaire (non traduit)"),
    "\tEnde der Feldzeit  Limite\u00A0: 0",
    "Quand atteint\u00A0: 9"
  ))
  expect_true("quota \"fin2\" (message)" %in% ctx$missing)
  # Without language settings: the structural names, no message.
  lss$quota_languagesettings <- NULL
  ctx <- lss_q_context(lss, "de", chrome_lang = "fr", measure = FALSE)
  text <- plan_text(lss_q_quota_paras(ctx))
  expect_identical(text[c(2L, 4L)],
                   c("\tfin2  si gender = M \u00B7 Limite\u00A0: 10 \u00B7 inactif",
                     "\tfin  Limite\u00A0: 0"))
  expect_length(text, 5L)
  # A survey without quota: no section.
  lss$quotas <- lss$quotas[0, , drop = FALSE]
  ctx <- lss_q_context(lss, "fr", chrome_lang = "fr", measure = FALSE)
  expect_identical(lss_q_quota_paras(ctx), list())
  body <- lss_q_plan_body(ctx, show_quotas = TRUE)
  expect_false("Quotas" %in% vapply(body$toc, `[[`, "", "name"))
  lss$quotas <- NULL
  ctx <- lss_q_context(lss, "fr", chrome_lang = "fr", measure = FALSE)
  expect_identical(lss_q_quota_paras(ctx), list())
})

test_that("a language without chrome takes the built-in labels in English", {
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  lss$languages <- c(lss$languages, "nl")
  ctx <- lss_q_context(lss, "nl", chrome_lang = "en", measure = FALSE)
  expect_identical(ctx$proofing, "nl")
  expect_identical(ctx$chrome_tag, "en-GB")
  items <- lss_q_builtin_items(ctx, "C")
  expect_identical(items[[1]]$label, "Yes")
  runs <- lss_q_item_runs(ctx, items[[1]], 10)
  expect_identical(runs[[3]]$lang, "en-GB")
  # A base language missing from the survey falls back on its first one.
  odd <- q_fixture_lss()
  odd$base_language <- NA_character_
  expect_error(
    render_questionnaire(odd, tempfile(fileext = ".docx"), max_options = 1),
    class = "lssdoc_bad_max_options"
  )
})

test_that("groups without questions, names or numbers still plan", {
  skip_if_not_installed("officer")
  ctx <- lss_q_context(q_fixture_lss(), "fr", chrome_lang = "fr", measure = FALSE)
  g <- ctx$model$groups
  first <- g[[1]]$questions[[1]]
  display <- g[[2]]$questions[[length(g[[2]]$questions)]]
  ctx$model$groups <- list(
    list(gid = "9", names = list(fr = "", en = ""), descriptions = list(),
         questions = list()),
    list(gid = "7", names = list(fr = NA, en = NA), descriptions = list(),
         questions = list(display)),
    list(gid = "8", names = list(fr = "", en = "Profile"),
         descriptions = list(), questions = list(first))
  )
  ctx$numbers <- lss_item_numbers(ctx$model)
  ls <- ctx$lss$survey_language_settings
  ls$surveyls_welcometext[ls$surveyls_language == "fr"] <- ""
  ctx$lss$survey_language_settings <- ls
  body <- lss_q_plan_body(ctx, show_consent = FALSE, show_endtext = FALSE)
  toc <- body$toc
  expect_identical(vapply(toc, `[[`, "", "name"),
                   c("Accueil", "Group 7", "Profile"))
  expect_identical(vapply(toc, `[[`, "", "range"), c("", "", "question 1"))
  text <- plan_text(body$paras)
  expect_true("Profile (non traduit)" %in% text)
  expect_gte(sum(grepl("(non traduit)", text, fixed = TRUE)), 2L)
  expect_identical(ctx$count$groups, 2L)
  # A survey with no numbered question still lays out its left column.
  ctx$numbers$no <- NA_integer_
  doc <- lss_q_emit(ctx, body$paras, title = "T")
  expect_s3_class(doc, "rdocx")
})

# ---- Rendering (not on CRAN) ------------------------------------------------

test_that("the welcome text numbers the items of an ordered list", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  ls <- lss$survey_language_settings
  ls$surveyls_welcometext <- paste0(
    "<ol><li>Read the page</li><li>Answer",
    "<ul><li>alone</li><li>calmly</li></ul></li>",
    "<li>Send</li></ol>"
  )
  lss$survey_language_settings <- ls
  out <- q_render(lss)
  on.exit(unlink(out), add = TRUE)
  paras <- q_paras(q_parts(out)$doc)
  i <- grep("Read the page", paras, fixed = TRUE)
  expect_length(i, 1L)
  expect_identical(sub("^.*\t", "", paras[i + 0:4]),
                   c("1. Read the page", "2. Answer", "\u2022 alone",
                     "\u2022 calmly", "3. Send"))
})

test_that("the message on the new name comes once per session", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  rlang::reset_message_verbosity("lssdoc_render_questionnaire_0.4.0")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  msgs <- character(0)
  withCallingHandlers(
    suppressWarnings(render_questionnaire(lss, out)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_true(any(grepl("render_documentation()", msgs, fixed = TRUE)))
  msgs <- character(0)
  withCallingHandlers(
    suppressWarnings(render_questionnaire(lss, out)),
    message = function(m) {
      msgs <<- c(msgs, conditionMessage(m))
      invokeRestart("muffleMessage")
    }
  )
  expect_false(any(grepl("render_documentation()", msgs, fixed = TRUE)))
})

test_that("the fixture renders in French with the defaults", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  warned <- 0L
  path <- withCallingHandlers(
    q_render(lss, language = "fr"),
    lssdoc_missing_translation = function(w) {
      warned <<- warned + 1L
      expect_match(conditionMessage(w), "Q16Numeric", fixed = TRUE)
      expect_match(conditionMessage(w), "Q01Single", fixed = TRUE)
      invokeRestart("muffleWarning")
    }
  )
  on.exit(unlink(path), add = TRUE)
  expect_identical(warned, 1L)
  parts <- q_parts(path)
  doc <- parts$doc
  text <- q_paras(doc)

  # Numbers: those of lss_item_numbers(), on the lines of their codes.
  numbers <- lss_item_numbers(lss_model(lss))
  asked <- numbers[numbers$status == "asked", ]
  expect_identical(q_numbered(doc),
                   stats::setNames(asked$code, as.character(asked$no)))

  # Headings with an outline level: a section, two groups, the end, the
  # quotas (the example survey has one).
  lvl <- xml2::xml_find_all(doc, "//w:body/w:p[w:pPr/w:outlineLvl[@w:val='0']]", W)
  expect_identical(
    vapply(lvl, xml2::xml_text, ""),
    c("Accueil", "Questions fermees",
      "Saisie libre, echelles simples et texte affiche", "Fin du questionnaire",
      "Quotas")
  )
  expect_true(all(xml2::xml_attr(
    xml2::xml_find_first(lvl, "w:pPr/w:pStyle", W), "w:val", W
  ) == "lssdocQuestionGroup"))
  # The running header refers to that style; the first page has none.
  expect_true(any(grepl("STYLEREF \"Question group\"", parts$headers,
                        fixed = TRUE)))
  expect_true(any(!grepl("STYLEREF", parts$headers, fixed = TRUE)))
  # Language declared to Word, hyphenation on.
  lang <- xml2::xml_find_first(parts$styles, "//w:docDefaults//w:lang", W)
  expect_identical(xml2::xml_attr(lang, "w:val", W), "fr-FR")
  expect_false(inherits(
    xml2::xml_find_first(parts$settings, "//w:autoHyphenation", W),
    "xml_missing"
  ))
  # Every paragraph keeps its lines; only justified text is hyphenated.
  ps <- xml2::xml_find_all(doc, "//w:body/w:p", W)
  expect_true(all(!vapply(xml2::xml_find_first(ps, "w:pPr/w:keepLines", W),
                          inherits, NA, "xml_missing")))
  justified <- !vapply(xml2::xml_find_first(ps, "w:pPr/w:jc[@w:val='both']", W),
                       inherits, NA, "xml_missing")
  suppressed <- !vapply(xml2::xml_find_first(ps, "w:pPr/w:suppressAutoHyphens", W),
                        inherits, NA, "xml_missing")
  expect_identical(suppressed, !justified)
  expect_true(any(justified))
  # No w:lang on the runs: the chrome is in the content language.
  expect_length(xml2::xml_find_all(doc, "//w:r/w:rPr/w:lang", W), 0L)

  # Title block and legend.
  expect_identical(text[1:2], c("Questionnaire d'exemple lssdoc", "Questionnaire"))
  legend <- text[3]
  expect_match(legend, "^17 questions en 2 groupes, 1 texte affich\u00E9, 2 calculs et 1 question masqu\u00E9e[.]")
  expect_match(legend, "En gris", fixed = TRUE)
  expect_match(legend, "* question obligatoire.", fixed = TRUE)
  expect_match(legend, "(*) r\u00E9ponse demand\u00E9e", fixed = TRUE)
  expect_match(legend, "Langue\u00A0: fr.", fixed = TRUE)
  expect_true("Table des mati\u00E8res" %in% text)
  expect_true(any(grepl("^Questions fermees\tquestions 1 \u00E0 12$", text)))

  # Question lines: mandatory marks, filter, mention, missing translation.
  line <- function(code) text[grepl(paste0("  ", code), text, fixed = TRUE)][1]
  expect_match(line("Q01Single"), "\u00A0*  Q01Single", fixed = TRUE)
  expect_match(line("Q02Dropdown"), "\u00A0(*)  Q02Dropdown \u00B7 si Q01Single = 1",
               fixed = TRUE)
  expect_match(line("Q05Array"), "Q05Array \u00B7 avec filtre", fixed = TRUE)
  expect_match(line("Q04Multiple"),
               "(plusieurs r\u00E9ponses possibles, chacune avec un commentaire)",
               fixed = TRUE)
  expect_match(line("Q10Ranking"), "(\u00E0 classer)", fixed = TRUE)
  expect_match(line("Q16Numeric"), "(non traduit)", fixed = TRUE)
  expect_true(any(text == "R\u00E9ponse\u00A0: CHF [nombre] par mois"))
  expect_true("Un nombre entier." %in% text)
  # Group description, under its heading, with the text tag.
  expect_true(any(text == "texte\tListes, cases a cocher, classements et tableaux."))

  # Hidden and computed lines, without numbers.
  # Their question text is their formula: they have no attribute equation.
  expect_true(any(grepl("^calcul\taffich\u00E9 aux r\u00E9pondant\u00B7es  Q13Text = .+$", text)))
  expect_true(any(grepl("^calcul\tmasqu\u00E9 aux r\u00E9pondant\u00B7es  Q17Date = .+$", text)))
  expect_true(any(grepl("^masqu\u00E9e\t.*  Q14Shorttext$", text)))

  # Lists: 12 and 13 in full, 14 abridged with its total.
  expect_true(all(paste0(1:13, "\tModalite ", letters[1:13])[-3] %in% text))
  expect_true("3\tOption c (non traduit)" %in% text)
  expect_identical(sum(text == "12\tModalite l"), 3L)
  expect_identical(sum(text == "13\tModalite m"), 2L)
  expect_identical(sum(text == "14\tModalite n"), 1L)
  # Q02 keeps 1 to 6 and 9 to 14: 7 and 8 come from Q01 and Q10 only.
  expect_identical(sum(text == "7\tModalite g"), 2L)
  expect_identical(sum(text == "8\tModalite h"), 2L)
  expect_true("(14 modalit\u00E9s)" %in% text)
  expect_true("\t\u2026" %in% text)

  # Types: H, P, I, |, O, A, B, C, E, K, Q, Y, G, 5, D, U.
  expect_true("R\u00E9ponse\u00A0: langue" %in% text)
  expect_true("R\u00E9ponse\u00A0: fichier" %in% text)
  expect_true(any(grepl("^Pour chaque ligne\u00A0:$", text)))
  expect_true(any(grepl("^Pour chaque ligne\u00A0: nombre$", text)))
  expect_true(any(grepl("^Pour chaque ligne\u00A0: texte court$", text)))
  expect_true("\tCommentaire (texte libre)" %in% text)
  expect_true("Y\tOui" %in% text)
  expect_true("I\tAugmenter" %in% text)
  expect_false("10\t10" %in% text)
  five <- paste(1:5, collapse = "\u2003\u2002")
  ten <- paste(1:10, collapse = "\u2003\u2002")
  rows <- which(text == "Pour chaque ligne\u00A0:")
  expect_true(five %in% text[rows + 1L])
  expect_true(ten %in% text[rows + 1L])
})

test_that("show_codes = FALSE leaves no code, show_hidden follows it", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  path <- suppressWarnings(q_render(lss, language = "fr", show_codes = FALSE))
  on.exit(unlink(path), add = TRUE)
  doc <- q_parts(path)$doc
  text <- q_paras(doc)
  expect_length(xml2::xml_find_all(doc, "//w:rFonts[@w:ascii='Consolas']", W), 0L)
  codes <- lss$questions$title
  expect_false(any(vapply(codes, function(cd) any(grepl(cd, text, fixed = TRUE)),
                          logical(1))))
  expect_false(any(grepl(" \u00B7 ", text, fixed = TRUE)))
  expect_false(any(grepl("En gris", text, fixed = TRUE)))
  # Hidden elements dropped, shown equation kept, numbers unchanged.
  expect_false(any(grepl("^masqu\u00E9e\t", text)))
  expect_false(any(grepl("masqu\u00E9 aux r\u00E9pondant", text)))
  expect_true(any(grepl("^calcul\taffich\u00E9 aux r\u00E9pondant\u00B7es$", text)))
  expect_match(text[3], "^17 questions en 2 groupes, 1 texte affich\u00E9 et 1 calcul[.]")
  expect_true(any(grepl("^17\t", text)))
  # Options without their code column, the label alone.
  expect_true("Modalite a" %in% text)
  # Codes off, hidden kept.
  path2 <- suppressWarnings(q_render(lss, language = "fr", show_codes = FALSE,
                                     show_hidden = TRUE))
  on.exit(unlink(path2), add = TRUE)
  text2 <- q_paras(q_parts(path2)$doc)
  expect_true(any(grepl("^masqu\u00E9e\t", text2)))
  expect_true(any(grepl("^calcul\tmasqu\u00E9 aux r\u00E9pondant\u00B7es$", text2)))
})

test_that("max_options sets where a list is abridged", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  text <- q_paras(q_parts(q_render(lss, max_options = Inf, show_toc = FALSE,
                                   show_help = FALSE, show_welcome = FALSE,
                                   show_consent = FALSE, show_endtext = FALSE,
                                   show_description = FALSE,
                                   show_group_description = FALSE))$doc)
  expect_true(all(paste0(1:14, "\tOption ", letters[1:14]) %in% text))
  expect_false(any(grepl("options)", text, fixed = TRUE)))
  expect_false("Table of contents" %in% text)
  text <- q_paras(q_parts(q_render(lss, max_options = 4))$doc)
  expect_true(all(c("(12 options)", "(13 options)", "(14 options)") %in% text))
  expect_false("3\tOption c" %in% text)
  expect_true("Answer: CHF [number] per month" %in% text)
})

test_that("the chrome can be in another language than the questions", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  logo <- tempfile(fileext = ".png")
  on.exit(unlink(logo), add = TRUE)
  grDevices::png(logo, width = 40, height = 20)
  graphics::par(mar = c(0, 0, 0, 0))
  graphics::plot.new()
  grDevices::dev.off()
  path <- suppressWarnings(q_render(
    lss, language = "fr", chrome_lang = "de", proofing_lang = "fr-CH",
    title = "Mon titre", subtitle = "Version 2", logo = logo, base_size = 11
  ))
  on.exit(unlink(path), add = TRUE)
  parts <- q_parts(path)
  text <- q_paras(parts$doc)
  expect_identical(
    xml2::xml_attr(xml2::xml_find_first(parts$styles, "//w:docDefaults//w:lang", W), "w:val", W),
    "fr-CH"
  )
  tagged <- xml2::xml_find_all(parts$doc, "//w:r[w:rPr/w:lang[@w:val='de-DE']]", W)
  expect_gt(length(tagged), 10L)
  tagged_text <- vapply(tagged, xml2::xml_text, "")
  expect_true("Antwort: " %in% tagged_text)
  expect_true("berechnet" %in% tagged_text)
  expect_false(any(grepl("Modalite", tagged_text, fixed = TRUE)))
  expect_identical(text[2:3], c("Mon titre", "Version 2"))
  expect_true("Begr\u00FC\u00DFung" %in% text)
  expect_true(any(grepl("^berechnet\t", text)))
  # The left column is wide enough for "berechnet".
  ind <- xml2::xml_find_first(parts$doc, paste0(
    "//w:body/w:p[w:r/w:t[.='berechnet']]/w:pPr/w:ind"
  ), W)
  hanging <- as.numeric(xml2::xml_attr(ind, "w:hanging", W)) / 20
  expect_gt(hanging, lss_q_text_width("berechnet", "Calibri", 9))
})

test_that("yes/no and gender list their options, the N-point scales their range", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- demo_lss()
  # The options under the line of their question, before the next question.
  after <- function(text, code, n = 2L) {
    i <- grep(paste0("  ", code, "$"), text)
    expect_length(i, 1L)
    text[i + seq_len(n)]
  }
  path <- suppressWarnings(q_render(lss, language = "en"))
  on.exit(unlink(path), add = TRUE)
  text <- q_paras(q_parts(path)$doc)
  expect_identical(after(text, "democonsent"), c("Y\tYes", "N\tNo"))
  expect_identical(after(text, "gender"), c("F\tFemale", "M\tMale"))
  expect_false(any(grepl("^Answer: (yes or no|gender)", text)))
  five <- paste(1:5, collapse = "\u2003\u2002")
  ten <- paste(1:10, collapse = "\u2003\u2002")
  # After its help text.
  expect_identical(after(text, "lifesatisfaction5pt", 2L)[[2]], five)
  expect_false(any(grepl("^Answer: .*choice", text)))
  rows <- which(text == "For each row:")
  expect_true(five %in% text[rows + 1L])
  expect_true(ten %in% text[rows + 1L])
  # The two-dimensional arrays: the columns, then what a cell takes.
  expect_identical(
    after(text, "adaptability", 7L)[4:7],
    c("Columns:", "NOW\tToday", "PAST\tFive years ago",
      "For each row and each column: number from 0 to 10")
  )
  expect_identical(after(text, "mediaquality", 6L)[[6]],
                   "For each row and each column: short text")
  # The hidden equation imc, with its formula.
  expect_true(any(grepl(
    "  imc = round((weight / ((height/100)*(height/100))), 1)",
    text, fixed = TRUE
  )))

  # Without codes the labels remain; in German they are German even when
  # the chrome is French: they belong to the questionnaire.
  path2 <- suppressWarnings(q_render(lss, language = "de", chrome_lang = "fr",
                                     show_codes = FALSE))
  on.exit(unlink(path2), add = TRUE)
  text2 <- q_paras(q_parts(path2)$doc)
  num <- grep("^[0-9]+\t", text2)
  opts <- function(k) text2[num[k] + 1:2]
  q_no <- function(code) {
    numbers <- lss_item_numbers(lss_model(lss))
    numbers$no[numbers$code == code]
  }
  expect_identical(opts(q_no("democonsent")), c("Ja", "Nein"))
  expect_identical(opts(q_no("gender")), c("Weiblich", "M\u00E4nnlich"))
  expect_false(any(grepl("^(Y|N|F|M)\t", text2)))
  expect_true(five %in% text2)
  expect_true("Colonnes\u00A0:" %in% text2)
})

test_that("a survey in one language has no language line, a lone group no TOC", {
  skip_on_cran()
  skip_if_not_installed("officer")
  spec <- lss_example_spec(kinds = c("single", "multiple"), languages = "en")
  file <- tempfile(fileext = ".lss")
  on.exit(unlink(file), add = TRUE)
  suppressMessages(write_lss(spec, file))
  lss <- read_lss(file)
  path <- q_render(lss, colors = list(primary = "#5C9F1A"), font = "Arial",
                   font_code = "Courier New")
  on.exit(unlink(path), add = TRUE)
  doc <- q_parts(path)$doc
  text <- q_paras(doc)
  expect_no_match(text[3], "Language:", fixed = TRUE)
  expect_false("Table of contents" %in% text)
  expect_identical(names(q_numbered(doc, "Courier New")), c("1", "2"))
  expect_gt(length(xml2::xml_find_all(doc, "//w:color[@w:val='5C9F1A']", W)), 0L)
})

test_that("the demo survey numbers its questions as the full document", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- demo_lss()
  path <- q_render(lss, language = "de", show_codes = TRUE)
  on.exit(unlink(path), add = TRUE)
  doc <- q_parts(path)$doc
  numbers <- lss_item_numbers(lss_model(lss))
  asked <- numbers[numbers$status == "asked", ]
  expect_identical(q_numbered(doc),
                   stats::setNames(asked$code, as.character(asked$no)))
  expect_identical(nrow(asked), 44L)
  text <- q_paras(doc)
  # Dual scale (one block per scale), array of texts and of numbers.
  expect_gte(sum(grepl("^F\u00FCr jede Zeile:", text)), 8L)
  expect_identical(sum(text == "Spalten:"), 2L)
  expect_true("F\u00FCr jede Zeile und jede Spalte: Zahl von 0 bis 10" %in% text)
  expect_true(any(grepl("^F\u00FCr jede Zeile und jede Spalte: Kurztext", text)))
})

test_that("a .pdf output goes through the .docx builder", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  out <- tempfile(fileext = ".pdf")
  called <- NULL
  local_mocked_bindings(.docx_to_pdf = function(docx, pdf) {
    called <<- c(file.exists(docx), pdf)
    invisible(pdf)
  })
  suppressWarnings(suppressMessages(render_questionnaire(lss, out)))
  expect_identical(called, c("TRUE", out))
})

test_that("the mention of the question type does not depend on the help text", {
  skip_on_cran()
  skip_if_not_installed("officer")
  lss <- demo_lss()
  mention <- "(several answers possible)"

  # nationality is a multiple choice whose help reads "Several answers
  # possible.". The help is the text of the survey and the mention comes
  # from the question type: the document prints both.
  path <- q_render(lss, language = "en")
  on.exit(unlink(path), add = TRUE)
  text <- q_paras(q_parts(path)$doc)
  at <- which(grepl("  nationality", text, fixed = TRUE))[1]
  expect_match(text[at], mention, fixed = TRUE)
  expect_identical(text[at + 1L], "Several answers possible.")

  # show_help = FALSE removes the help and leaves the mention.
  path2 <- q_render(lss, language = "en", show_help = FALSE)
  on.exit(unlink(path2), add = TRUE)
  text2 <- q_paras(q_parts(path2)$doc)
  at2 <- which(grepl("  nationality", text2, fixed = TRUE))[1]
  expect_match(text2[at2], mention, fixed = TRUE)
  expect_false("Several answers possible." %in% text2)
})

test_that("the Other option without a replacement text takes the content language", {
  skip_if_not_installed("officer")
  lss <- q_fixture_lss()
  q <- list(code = "Q", attributes = NULL)
  value <- function(runs) vapply(runs, function(r) r$value, "")
  # French content, English chrome: "Autre" is read by the respondent,
  # "(free text)" is lssdoc's and stays in the chrome language.
  ctx <- lss_q_context(lss, "fr", chrome_lang = "en", measure = FALSE)
  it <- lss_q_other_item(ctx, q, "other")
  expect_identical(it$label, "Autre")
  expect_identical(it$extra, "(free text)")
  expect_null(it$lang)
  runs <- lss_q_item_runs(ctx, it, 0)
  expect_identical(value(runs), c("Autre", " (free text)"))
  expect_null(runs[[1]]$lang)
  expect_identical(runs[[2]]$lang, ctx$chrome_tag)
  # The chrome render_questionnaire() takes by default for a French
  # questionnaire: the printed text is what it was.
  ctx <- lss_q_context(lss, "fr", chrome_lang = "fr", measure = FALSE)
  runs <- lss_q_item_runs(ctx, lss_q_other_item(ctx, q, "other"), 0)
  expect_identical(paste(value(runs), collapse = ""), "Autre (texte libre)")
})

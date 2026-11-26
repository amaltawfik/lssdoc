# Builder of the readable questionnaire (render_questionnaire()).
#
# Ported from the prototype of issue #9. The document is built in two
# passes: lss_q_plan_body() and lss_q_plan_top() turn the model into a list
# of paragraphs (runs and a layout kind), counting what they print;
# lss_q_emit() then writes them
# with officer, once the width of the left column is known. The numbers
# come from lss_item_numbers(), the table render_documentation() reads.

#' Render the readable questionnaire to a Word document (internal)
#'
#' The arguments are validated by [render_questionnaire()].
#'
#' @keywords internal
#' @noRd
.render_questionnaire_docx <- function(lss, output, language,
                                       show_codes = TRUE,
                                       show_hidden = show_codes,
                                       show_help = TRUE,
                                       show_group_description = TRUE,
                                       show_toc = TRUE,
                                       show_description = TRUE,
                                       show_welcome = TRUE,
                                       show_consent = TRUE,
                                       show_endtext = TRUE,
                                       max_options = 12L,
                                       title = NULL, subtitle = NULL,
                                       logo = NULL, logo_width = 1.5,
                                       logo_height = 0.75,
                                       font = NULL, font_code = NULL,
                                       colors = NULL, chrome_lang = "en",
                                       proofing_lang = NULL,
                                       base_size = 10L) {
  ctx <- lss_q_context(
    lss, language, chrome_lang = chrome_lang, proofing_lang = proofing_lang,
    base_size = base_size, font = font, font_code = font_code,
    colors = colors, show_codes = show_codes, show_hidden = show_hidden,
    show_help = show_help, max_options = max_options
  )
  body <- lss_q_plan_body(
    ctx,
    show_group_description = show_group_description,
    show_description = show_description, show_welcome = show_welcome,
    show_consent = show_consent, show_endtext = show_endtext
  )
  doc_title <- lss_q_title(ctx, title)
  top <- lss_q_plan_top(
    ctx, body, title = doc_title, subtitle = subtitle, logo = logo,
    logo_width = logo_width, logo_height = logo_height, show_toc = show_toc
  )

  missing <- unique(ctx$missing)
  if (length(missing) > 0L) {
    base <- ctx$base
    lssdoc_warn(
      c(
        "{length(missing)} text{?s} {?has/have} no translation in {.val {language}}: printed in the base language {.val {base}} and marked as not translated.",
        "i" = "Concerned: {.val {missing}}."
      ),
      class = "lssdoc_missing_translation"
    )
  }

  doc <- lss_q_emit(ctx, c(top, body$paras), title = doc_title)
  print(doc, target = output)

  abs_path <- tryCatch(
    normalizePath(output, winslash = "/", mustWork = TRUE),
    error = function(e) output
  )
  size_kb <- round(file.size(output) / 1024)
  cli::cli_alert_success("Saved {.file {abs_path}} ({size_kb} KB)")
  invisible(output)
}

#' Everything the plan and the emitter share
#'
#' An environment: the model in the content language and the base
#' language (for the fallback), the numbers, the theme, the chrome, the
#' language tags, the type properties, the switches, and what the plan
#' collects on its way (counts, tags printed, missing translations).
#'
#' @keywords internal
#' @noRd
lss_q_context <- function(lss, language, chrome_lang = "en",
                          proofing_lang = NULL, base_size = 10L,
                          font = NULL, font_code = NULL, colors = NULL,
                          show_codes = TRUE, show_hidden = show_codes,
                          show_help = TRUE, max_options = 12L,
                          measure = NULL) {
  ctx <- new.env(parent = emptyenv())
  base <- lss$base_language
  if (is.null(base) || is.na(base) || !nzchar(base) ||
      !base %in% lss$languages) {
    base <- lss$languages[1]
  }
  ctx$lss <- lss
  ctx$language <- language
  ctx$base <- base
  ctx$model <- lss_model(lss, languages = unique(c(language, base)))
  ctx$numbers <- lss_item_numbers(ctx$model)
  theme <- lss_render_theme(base_size)
  if (!is.null(font)) theme$font_body <- font
  if (!is.null(font_code)) theme$font_code <- font_code
  if (!is.null(colors)) theme <- utils::modifyList(theme, colors)
  theme$chrome <- lss_chrome_strings(chrome_lang)
  theme$chrome_lang <- chrome_lang
  ctx$theme <- theme
  ctx$chrome <- theme$chrome
  ctx$base_size <- as.integer(base_size)
  ctx$french <- grepl("^fr", tolower(language))
  ctx$proofing <- lss_q_proofing_lang(language, proofing_lang)
  ctx$chrome_tag <- lss_q_chrome_tag(chrome_lang, ctx$proofing)
  # The labels LimeSurvey builds in (yes / no / uncertain, increase / same
  # / decrease) are read by the respondent: they come in the content
  # language when the chrome has it, else in the chrome language.
  content_primary <- tolower(strsplit(language, "-", fixed = TRUE)[[1]][1])
  if (content_primary %in% c("en", "fr", "de", "es", "it")) {
    ctx$builtin <- lss_chrome_strings(content_primary)
    ctx$builtin_tag <- NULL
  } else {
    ctx$builtin <- theme$chrome
    ctx$builtin_tag <- ctx$chrome_tag
  }
  ctx$show_codes <- isTRUE(show_codes)
  ctx$show_hidden <- isTRUE(show_hidden)
  ctx$show_help <- isTRUE(show_help)
  ctx$max_options <- max_options
  ctx$measure <- measure
  ctx$props <- lss_q_props(theme, ctx$base_size)
  ctx$missing <- character(0)
  ctx$tags <- character(0)
  ctx$count <- list(questions = sum(ctx$numbers$status == "asked"),
                    groups = 0L, displays = 0L, computed = 0L, hidden = 0L)
  ctx$mandatory <- FALSE
  ctx$soft <- FALSE
  ctx
}

#' Text properties of the readable questionnaire
#'
#' Every size derives from `base_size`, as in the prototype: options,
#' subquestions and hints at base - 1; codes, tags, running header and
#' footer at base - 2; group headings at base + 3; title at base + 6. The
#' prototype's colours map to theme roles: its blue to `primary`, its
#' grey of the codes to `muted`, its darker grey of the text displays and
#' hints to `note`, its near-black to `text`, its link colour to `accent`.
#'
#' @keywords internal
#' @noRd
lss_q_props <- function(theme, base) {
  fp <- function(size, color, font = theme$font_body, bold = FALSE,
                 italic = FALSE) {
    officer::fp_text(font.family = font, font.size = size, color = color,
                     bold = bold, italic = italic)
  }
  list(
    title = fp(base + 6L, theme$color_primary, bold = TRUE),
    sub = fp(base + 1L, theme$color_primary),
    note = fp(base - 1L, theme$color_note),
    group = fp(base + 3L, theme$color_primary, bold = TRUE),
    num = fp(base, theme$color_primary, bold = TRUE),
    q = fp(base, theme$color_text),
    shown = fp(base, theme$color_note),
    shown_bold = fp(base, theme$color_note, bold = TRUE),
    tag = fp(base - 2L, theme$color_muted),
    var = fp(base - 2L, theme$color_muted, font = theme$font_code),
    opt = fp(base - 1L, theme$color_text),
    code = fp(base - 2L, theme$color_muted),
    hint = fp(base - 1L, theme$color_note, italic = TRUE),
    help = fp(base - 1L, theme$color_note),
    running = fp(base - 2L, theme$color_muted),
    toc_title = fp(base, theme$color_primary, bold = TRUE),
    toc = fp(base, theme$color_accent)
  )
}

#' A text run, declared in the chrome language when it needs to be
#'
#' `chrome = TRUE` marks a label lssdoc adds: when the chrome language
#' differs from the content language, the run carries its own `w:lang`.
#' `lang` gives the tag explicitly.
#'
#' @keywords internal
#' @noRd
lss_q_run <- function(ctx, text, prop, chrome = FALSE, lang = NULL) {
  r <- officer::ftext(text, prop = prop)
  tag <- if (!is.null(lang)) lang else if (isTRUE(chrome)) ctx$chrome_tag
  if (!is.null(tag)) {
    r$lang <- tag
    class(r) <- c("lssdoc_lang_run", class(r))
  }
  r
}

#' Write a run that declares its own language
#'
#' The run of officer, with `<w:lang w:val="..."/>` as the last child of
#' its `w:rPr`, so that Word checks the spelling of a chrome label in the
#' chrome language when it differs from the content language.
#'
#' @param x A run built by `lss_q_run()`.
#' @param add_ns,... Passed on to the method of officer.
#' @keywords internal
#' @noRd
#' @exportS3Method officer::to_wml
to_wml.lssdoc_lang_run <- function(x, add_ns = FALSE, ...) {
  out <- NextMethod()
  lang <- sprintf("<w:lang w:val=\"%s\"/>", x$lang)
  if (grepl("</w:rPr>", out, fixed = TRUE)) {
    sub("</w:rPr>", paste0(lang, "</w:rPr>"), out, fixed = TRUE)
  } else {
    sub("<w:r>", paste0("<w:r><w:rPr>", lang, "</w:rPr>"), out, fixed = TRUE)
  }
}

#' A text in the content language, or in the base language when missing
#'
#' `values` is a list named by language (the model's localized values).
#' A blank value in the content language falls back on the base language;
#' `where` is then recorded for the warning, and `missing = TRUE` asks
#' for the mark.
#'
#' @return A list with `text` (plain, one line, `""` when there is none),
#'   `html` (the raw text) and `missing`.
#' @keywords internal
#' @noRd
lss_q_pick <- function(ctx, values, where) {
  v <- values[[ctx$language]]
  if (!lss_q_blank(v)) {
    return(list(html = v, text = lss_q_clean(v, ctx$french), missing = FALSE))
  }
  if (!identical(ctx$language, ctx$base)) {
    b <- values[[ctx$base]]
    if (!lss_q_blank(b)) {
      ctx$missing <- c(ctx$missing, where)
      return(list(html = b, text = lss_q_clean(b, ctx$french),
                  missing = TRUE))
    }
  }
  list(html = NA_character_, text = "", missing = FALSE)
}

#' A localized question attribute (prefix, suffix, validation tip, ...)
#'
#' The row in the content language, else the row without a language,
#' else the row in the base language, which is then a missing
#' translation.
#'
#' @keywords internal
#' @noRd
lss_q_attr <- function(ctx, attrs, name, where) {
  none <- list(html = NA_character_, text = "", missing = FALSE)
  if (is.null(attrs) || !is.data.frame(attrs) || nrow(attrs) == 0L) {
    return(none)
  }
  rows <- attrs[attrs$attribute %in% name, , drop = FALSE]
  if (nrow(rows) == 0L) {
    return(none)
  }
  lang <- if ("language" %in% names(rows)) rows$language else rep("", nrow(rows))
  get <- function(lg) {
    v <- rows$value[lang %in% lg]
    v <- v[!is.na(v) & nzchar(trimws(v))]
    if (length(v)) v[1] else NA_character_
  }
  values <- list(get(ctx$language))
  names(values) <- ctx$language
  if (is.na(values[[1]])) {
    values[[1]] <- get(c("", NA))
  }
  if (!identical(ctx$language, ctx$base)) {
    values[[ctx$base]] <- get(ctx$base)
  }
  lss_q_pick(ctx, values, where)
}

#' A survey-level text (description, welcome, privacy notice, end text)
#' @keywords internal
#' @noRd
lss_q_survey_text <- function(ctx, field, where) {
  ls <- ctx$lss$survey_language_settings
  if (is.null(ls) || !(field %in% names(ls)) ||
      !("surveyls_language" %in% names(ls))) {
    return(list(html = NA_character_, text = "", missing = FALSE))
  }
  get <- function(lg) {
    v <- ls[[field]][ls$surveyls_language == lg]
    if (length(v)) v[1] else NA_character_
  }
  values <- stats::setNames(
    lapply(unique(c(ctx$language, ctx$base)), get),
    unique(c(ctx$language, ctx$base))
  )
  lss_q_pick(ctx, values, where)
}

#' The title of the document: the `title` argument, else the survey
#' title in the content language, else in the base language
#' @keywords internal
#' @noRd
lss_q_title <- function(ctx, title) {
  langs <- unique(c(ctx$language, ctx$base))
  titles <- lss_resolve_titles(title, ctx$lss, langs)
  values <- stats::setNames(as.list(unname(titles)), langs)
  t <- lss_q_pick(ctx, values, "title")$text
  if (!nzchar(t)) "(untitled survey)" else t
}

#' A paragraph of the plan
#'
#' `kind` sets the layout the emitter gives it: `"q"` (a line with the
#' left column: question, text display, section paragraph), `"line"` (an
#' option, with a code column of width `w`), `"hint"` (at the indent of
#' the options), `"heading"` (a group or section heading, with a
#' bookmark), `"free"` (its own `fp`).
#'
#' @keywords internal
#' @noRd
lss_q_para <- function(runs, kind, w = 0, top = NULL, keep = FALSE,
                       justify = FALSE, fp = NULL, bookmark = NULL,
                       text = NULL) {
  list(runs = runs, kind = kind, w = w, top = top, keep = keep,
       justify = justify, fp = fp, bookmark = bookmark, text = text)
}

#' Chain paragraphs with keep-with-next, the last one excepted
#' @keywords internal
#' @noRd
lss_q_keep_chain <- function(paras) {
  for (k in seq_along(paras)) paras[[k]]$keep <- k < length(paras)
  paras
}

#' The mark of a text printed in the base language
#' @keywords internal
#' @noRd
lss_q_missing_run <- function(ctx) {
  lss_q_run(ctx, paste0(" ", ctx$chrome$q_not_translated), ctx$props$hint,
            chrome = TRUE)
}

#' Plan the body: front matter, groups and questions, end matter
#'
#' @return A list: `paras`, the paragraphs; `toc`, one entry per section
#'   and group (`name`, `bookmark`, `range`).
#' @keywords internal
#' @noRd
lss_q_plan_body <- function(ctx, show_group_description = TRUE,
                            show_description = TRUE, show_welcome = TRUE,
                            show_consent = TRUE, show_endtext = TRUE) {
  ch <- ctx$chrome
  P <- ctx$props
  langs <- unique(c(ctx$language, ctx$base))
  text_block <- function(field, where, tag = ch$q_tag_text) {
    if (!field) return(NULL)
    x <- lss_q_survey_text(ctx, where[1], where[2])
    list(paras = lss_q_html_paras(x$html, ctx$french), missing = x$missing,
         tag = tag)
  }
  sections_start <- list()
  blocks <- Filter(Negate(is.null), list(
    text_block(isTRUE(show_description), c("surveyls_description", "survey description")),
    text_block(isTRUE(show_welcome), c("surveyls_welcometext", "welcome text"))
  ))
  blocks <- Filter(function(b) length(b$paras) > 0L, blocks)
  if (length(blocks)) {
    sections_start[[length(sections_start) + 1L]] <- list(
      name = ch$q_section_welcome, bookmark = lss_section_bookmark("welcome"),
      blocks = blocks
    )
  }
  if (isTRUE(show_consent) && lss_consent_present(ctx$lss, langs)) {
    notice <- text_block(TRUE, c("surveyls_policy_notice", "privacy notice"))
    label <- text_block(TRUE, c("surveyls_policy_notice_label", "consent label"),
                        tag = "\u25A1")
    label$asked <- TRUE
    blocks <- Filter(function(b) length(b$paras) > 0L, list(notice, label))
    if (length(blocks)) {
      sections_start[[length(sections_start) + 1L]] <- list(
        name = ch$consent_title, bookmark = lss_section_bookmark("consent"),
        blocks = blocks
      )
    }
  }
  section_end <- NULL
  if (isTRUE(show_endtext)) {
    blocks <- Filter(function(b) length(b$paras) > 0L,
                     list(text_block(TRUE, c("surveyls_endtext", "end text"))))
    if (length(blocks)) {
      section_end <- list(
        name = ch$q_section_end, bookmark = lss_section_bookmark("end"),
        blocks = blocks
      )
    }
  }

  paras <- list()
  toc <- list()
  for (s in sections_start) {
    paras <- c(paras, lss_q_section_paras(ctx, s))
    toc[[length(toc) + 1L]] <- list(name = s$name, bookmark = s$bookmark,
                                    range = "", chrome = TRUE)
  }
  numbers <- ctx$numbers
  for (gi in seq_along(ctx$model$groups)) {
    g <- ctx$model$groups[[gi]]
    if (length(g$questions) == 0L) next
    ctx$count$groups <- ctx$count$groups + 1L
    base_name <- lss_q_clean(g$names[[ctx$base]])
    gname <- lss_q_pick(ctx, g$names, sprintf("group \"%s\"", base_name))
    name <- if (nzchar(gname$text)) gname$text else paste("Group", g$gid)
    bookmark <- lss_group_bookmark(gi)
    runs <- list(lss_q_run(ctx, name, P$group))
    if (gname$missing) runs <- c(runs, list(lss_q_missing_run(ctx)))
    paras[[length(paras) + 1L]] <- lss_q_para(runs, "heading",
                                              bookmark = bookmark, text = name)
    gfilter <- lss_q_group_filter_para(ctx, g$relevance)
    if (!is.null(gfilter)) paras[[length(paras) + 1L]] <- gfilter
    if (isTRUE(show_group_description)) {
      d <- lss_q_pick(ctx, g$descriptions,
                      sprintf("group \"%s\" (description)", base_name))
      dp <- lss_q_html_paras(d$html, ctx$french)
      if (length(dp)) {
        paras <- c(paras, lss_q_block_paras(
          ctx, list(paras = dp, missing = d$missing, tag = ch$q_tag_text)
        ))
      }
    }
    for (q in g$questions) {
      i <- match(as.character(q$qid), numbers$qid)
      paras <- c(paras, lss_q_question(ctx, q, numbers[i, , drop = FALSE]))
    }
    nums <- numbers$no[numbers$qid %in% vapply(g$questions, function(q) {
      as.character(q$qid)
    }, character(1)) & !is.na(numbers$no)]
    range <- if (length(nums) == 0L) {
      ""
    } else if (length(nums) == 1L) {
      sprintf(ch$q_toc_question_fmt, nums[1])
    } else {
      sprintf(ch$q_toc_questions_fmt, min(nums), max(nums))
    }
    toc[[length(toc) + 1L]] <- list(name = name, bookmark = bookmark,
                                    range = range, chrome = FALSE)
  }
  if (!is.null(section_end)) {
    paras <- c(paras, lss_q_section_paras(ctx, section_end))
    toc[[length(toc) + 1L]] <- list(name = section_end$name,
                                    bookmark = section_end$bookmark,
                                    range = "", chrome = TRUE)
  }
  list(paras = paras, toc = toc)
}

#' A front or end matter section: its heading, then its blocks
#' @keywords internal
#' @noRd
lss_q_section_paras <- function(ctx, s) {
  heading <- lss_q_para(
    list(lss_q_run(ctx, s$name, ctx$props$group, chrome = TRUE)),
    "heading", bookmark = s$bookmark, text = s$name
  )
  c(list(heading), unlist(lapply(s$blocks, function(b) {
    lss_q_block_paras(ctx, b)
  }), recursive = FALSE))
}

#' The paragraphs of a text block, the tag in the left column of the first
#'
#' What is shown is grey and justified; the consent label, which is asked,
#' is black and left-aligned. An entirely bold paragraph is kept with the
#' next one.
#'
#' @keywords internal
#' @noRd
lss_q_block_paras <- function(ctx, b) {
  P <- ctx$props
  asked <- isTRUE(b$asked)
  if (!is.null(b$tag)) ctx$tags <- c(ctx$tags, b$tag)
  out <- list()
  for (j in seq_along(b$paras)) {
    pj <- b$paras[[j]]
    prop <- if (asked) {
      P$q
    } else if (isTRUE(pj$bold)) {
      P$shown_bold
    } else {
      P$shown
    }
    runs <- c(
      if (j == 1L) list(lss_q_run(ctx, b$tag, P$tag, chrome = TRUE)),
      list(officer::run_tab(), lss_q_run(ctx, pj$text, prop))
    )
    if (isTRUE(b$missing) && j == length(b$paras)) {
      runs <- c(runs, list(lss_q_missing_run(ctx)))
    }
    out[[j]] <- lss_q_para(runs, "q", top = if (j == 1L) 8 else 3,
                           keep = isTRUE(pj$bold), justify = !asked)
  }
  out
}

#' The paragraphs of one question: its line, then what it asks
#'
#' @param num The question's row of `lss_item_numbers()`.
#' @keywords internal
#' @noRd
lss_q_question <- function(ctx, q, num) {
  ch <- ctx$chrome
  P <- ctx$props
  hidden <- isTRUE(num$hidden[1])
  status <- num$status[1]
  if (hidden && !ctx$show_hidden) {
    return(list())
  }
  tx <- lss_q_pick(ctx, lapply(q$texts, function(t) t$question), q$code)
  head <- list()
  subs <- list()
  justify <- FALSE
  text_runs <- function(prop) {
    c(list(lss_q_run(ctx, tx$text, prop)),
      if (tx$missing) list(lss_q_missing_run(ctx)))
  }
  if (identical(status, "equation")) {
    ctx$tags <- c(ctx$tags, ch$q_tag_computed)
    ctx$count$computed <- ctx$count$computed + 1L
    head <- list(
      lss_q_run(ctx, ch$q_tag_computed, P$tag, chrome = TRUE),
      officer::run_tab(),
      lss_q_run(ctx, if (hidden) ch$q_equation_hidden else ch$q_equation_shown,
                P$hint, chrome = TRUE)
    )
  } else if (hidden) {
    ctx$tags <- c(ctx$tags, ch$q_tag_hidden)
    ctx$count$hidden <- ctx$count$hidden + 1L
    head <- c(list(lss_q_run(ctx, ch$q_tag_hidden, P$tag, chrome = TRUE),
                   officer::run_tab()),
              text_runs(P$q))
  } else if (identical(status, "display")) {
    ctx$tags <- c(ctx$tags, ch$q_tag_text)
    ctx$count$displays <- ctx$count$displays + 1L
    head <- c(list(lss_q_run(ctx, ch$q_tag_text, P$tag, chrome = TRUE),
                   officer::run_tab()),
              text_runs(P$shown))
    justify <- TRUE
  } else {
    head <- c(list(lss_q_run(ctx, as.character(num$no[1]), P$num),
                   officer::run_tab()),
              list(lss_q_run(ctx, tx$text, P$q)))
    mand <- toupper(lss_q_or(q$mandatory, ""))
    if (identical(mand, "Y")) {
      ctx$mandatory <- TRUE
      head <- c(head, list(lss_q_run(ctx, "\u00A0*", P$q)))
    } else if (identical(mand, "S")) {
      ctx$soft <- TRUE
      head <- c(head, list(lss_q_run(ctx, "\u00A0(*)", P$q)))
    }
    if (tx$missing) head <- c(head, list(lss_q_missing_run(ctx)))
    mention <- switch(
      EXPR = as.character(q$type),
      "M" = ch$q_several_answers,
      "P" = ch$q_several_comment,
      "R" = ch$q_ranking,
      NULL
    )
    if (!is.null(mention)) {
      head <- c(head, list(lss_q_run(ctx, paste0("  (", mention, ")"),
                                     P$hint, chrome = TRUE)))
    }
    if (ctx$show_help) {
      help <- lss_q_pick(ctx, lapply(q$texts, function(t) t$help), q$code)
      tip <- lss_q_attr(ctx, q$attributes, "em_validation_q_tip", q$code)
      for (h in list(help, tip)) {
        if (!nzchar(h$text)) next
        runs <- c(list(lss_q_run(ctx, h$text, P$help)),
                  if (h$missing) list(lss_q_missing_run(ctx)))
        subs[[length(subs) + 1L]] <- lss_q_para(runs, "hint")
      }
    }
    subs <- c(subs, lss_q_body(ctx, q))
  }
  if (ctx$show_codes) {
    head <- c(head, list(lss_q_run(ctx, paste0("  ", q$code), P$var)))
    filter <- lss_q_filter(q$relevance, ctx$theme)
    if (!is.null(filter)) {
      head <- c(head, list(lss_q_run(ctx, paste0(" \u00B7 ", filter), P$code,
                                     chrome = TRUE)))
    }
  }
  first <- lss_q_para(head, "q", keep = length(subs) > 0L, justify = justify)
  c(list(first), lss_q_keep_chain(subs))
}

#' One option of a list, before layout
#' @keywords internal
#' @noRd
lss_q_item <- function(code, label, missing = FALSE, chrome = FALSE,
                       extra = NULL, lang = NULL) {
  list(code = if (is.na(code)) "" else as.character(code),
       label = label, missing = missing, chrome = chrome, extra = extra,
       lang = lang)
}

#' The answer options of a question (one scale), as items
#' @keywords internal
#' @noRd
lss_q_answer_items <- function(ctx, q, scale = "0") {
  ans <- q$answers
  if (length(ans) == 0L) return(list())
  sid <- vapply(ans, function(a) {
    if (is.null(a$scale_id) || is.na(a$scale_id)) "0" else as.character(a$scale_id)
  }, character(1))
  lapply(ans[sid == scale], function(a) {
    x <- lss_q_pick(ctx, a$labels, q$code)
    lss_q_item(a$code, lss_q_strip_code(x$text, a$code), x$missing)
  })
}

#' The subquestions of a question (one scale), as items
#' @keywords internal
#' @noRd
lss_q_subq_items <- function(ctx, q, scale = "0") {
  sqs <- q$subquestions
  if (length(sqs) == 0L) return(list())
  sid <- vapply(sqs, function(s) {
    if (is.null(s$scale_id) || is.na(s$scale_id)) "0" else as.character(s$scale_id)
  }, character(1))
  lapply(sqs[sid == scale], function(s) {
    x <- lss_q_pick(ctx, lapply(s$texts, function(t) t$question), q$code)
    it <- lss_q_item(s$code, x$text, x$missing)
    # The subquestion's own condition, written with codes: printed only
    # when the codes are.
    if (ctx$show_codes) it$filter <- lss_q_filter(s$relevance, ctx$theme)
    it
  })
}

#' The paragraph of a group's condition, under the group heading
#'
#' `NULL` when the codes are not shown or the group has no condition.
#' Otherwise one run in the code style, `Group shown if <plain>`
#' (`q_group_filter_fmt`), or `Group with a filter` (`q_group_filtered`)
#' when the expression cannot be simplified, kept with what follows. A
#' paragraph of its own layout, never the style of the group heading,
#' which the running header reads through a STYLEREF field.
#'
#' @keywords internal
#' @noRd
lss_q_group_filter_para <- function(ctx, relevance) {
  if (!ctx$show_codes || !lss_has_condition(relevance)) return(NULL)
  raw <- trimws(lss_strip_outer_parens(trimws(as.character(relevance[1]))))
  plain <- lss_humanize_relevance(raw, ctx$theme)
  text <- if (lss_q_filter_unsimplified(plain)) {
    ctx$chrome$q_group_filtered
  } else {
    sprintf(ctx$chrome$q_group_filter_fmt, plain)
  }
  lss_q_para(list(lss_q_run(ctx, text, ctx$props$code, chrome = TRUE)),
             "hint", top = 3, keep = TRUE)
}

#' The "Other" option: its replacement text, else "Other", then
#' "(free text)"
#' @keywords internal
#' @noRd
lss_q_other_item <- function(ctx, q, code) {
  x <- lss_q_attr(ctx, q$attributes, "other_replace_text", q$code)
  lab <- sub("[[:space:]\u00A0]*:$", "", x$text, perl = TRUE)
  if (nzchar(lab)) {
    lss_q_item(code, lab, x$missing, extra = ctx$chrome$q_free_text)
  } else {
    lss_q_item(code, paste(ctx$chrome$q_other, ctx$chrome$q_free_text),
               chrome = TRUE)
  }
}

#' The options LimeSurvey builds in for a question type
#'
#' The labelled scales of the full document (`lss_predefined_labelled()`):
#' the arrays C and E, the yes/no question (Y) and the gender question (G).
#' A and B arrays print one line instead ("For each row: 5-point choice
#' (1 to 5)").
#'
#' @keywords internal
#' @noRd
lss_q_builtin_items <- function(ctx, type) {
  lapply(lss_predefined_labelled(type), function(cl) {
    lss_q_item(cl[1], ctx$builtin[[cl[2]]], lang = ctx$builtin_tag)
  })
}

#' The runs of an option line
#' @keywords internal
#' @noRd
lss_q_item_runs <- function(ctx, it, w) {
  P <- ctx$props
  label <- if (isTRUE(it$ellipsis)) {
    list(lss_q_run(ctx, "\u2026", P$opt))
  } else if (!nzchar(it$label)) {
    list(lss_q_run(ctx, ctx$chrome$q_no_label, P$hint, chrome = TRUE))
  } else {
    c(list(lss_q_run(ctx, it$label, P$opt, chrome = it$chrome, lang = it$lang)),
      if (!is.null(it$extra)) {
        list(lss_q_run(ctx, paste0(" ", it$extra), P$opt, chrome = TRUE))
      })
  }
  if (isTRUE(it$missing)) label <- c(label, list(lss_q_missing_run(ctx)))
  if (!is.null(it$filter)) {
    label <- c(label, list(lss_q_run(ctx, paste0(" \u00B7 ", it$filter),
                                     P$code, chrome = TRUE)))
  }
  if (w <= 0) return(label)
  c(if (nzchar(it$code)) list(lss_q_run(ctx, it$code, P$code)),
    list(officer::run_tab()), label)
}

#' An option list: one line per option, the codes in a column of their
#' own, abridged when `abridge` and longer than `max_options + 1`
#'
#' `tail` items (the comment line of a list with comment) follow the list
#' and are never abridged.
#'
#' @keywords internal
#' @noRd
lss_q_option_paras <- function(ctx, items, abridge = TRUE, tail = list()) {
  n <- length(items)
  if (n == 0L && length(tail) == 0L) return(list())
  keep <- if (abridge) lss_q_abridge(n, ctx$max_options) else seq_len(n)
  shown <- lapply(keep, function(k) {
    if (is.na(k)) list(code = "", label = "", ellipsis = TRUE) else items[[k]]
  })
  shown <- c(shown, tail)
  codes <- vapply(shown, function(it) it$code, character(1))
  w <- if (ctx$show_codes) {
    lss_q_code_width(codes, ctx$theme$font_body, ctx$base_size,
                     measure = ctx$measure)
  } else {
    0
  }
  if (!ctx$show_codes) {
    shown <- lapply(shown, function(it) {
      it$code <- ""
      it
    })
  }
  out <- lapply(shown, function(it) {
    lss_q_para(lss_q_item_runs(ctx, it, w), "line", w = w)
  })
  if (anyNA(keep)) {
    out[[length(out) + 1L]] <- lss_q_para(
      list(lss_q_run(ctx, sprintf(ctx$chrome$q_options_total_fmt, n),
                     ctx$props$hint, chrome = TRUE)),
      "hint"
    )
  }
  out
}

#' The "For each row:" line, set off by 4 points, with what follows it
#' @keywords internal
#' @noRd
lss_q_row_prefix <- function(ctx, after = list()) {
  P <- ctx$props
  lss_q_para(
    c(list(lss_q_run(ctx, ctx$chrome$q_for_each_row, P$hint, chrome = TRUE)),
      if (length(after)) c(list(lss_q_run(ctx, " ", P$hint)), after)),
    "hint", top = 4
  )
}

#' What follows "Answer:" for an input field: the type, in square brackets
#' between the prefix and the suffix when the question has them
#' @keywords internal
#' @noRd
lss_q_open_runs <- function(ctx, q, key) {
  P <- ctx$props
  open <- ctx$chrome[[key]]
  pre <- lss_q_attr(ctx, q$attributes, "prefix", q$code)
  suf <- lss_q_attr(ctx, q$attributes, "suffix", q$code)
  if (!nzchar(pre$text) && !nzchar(suf$text)) {
    return(list(lss_q_run(ctx, open, P$hint, chrome = TRUE)))
  }
  runs <- list()
  if (nzchar(pre$text)) runs <- c(runs, list(lss_q_run(ctx, paste0(pre$text, " "), P$hint)))
  runs <- c(runs, list(lss_q_run(ctx, paste0("[", open, "]"), P$hint, chrome = TRUE)))
  if (nzchar(suf$text)) runs <- c(runs, list(lss_q_run(ctx, paste0(" ", suf$text), P$hint)))
  if (pre$missing || suf$missing) runs <- c(runs, list(lss_q_missing_run(ctx)))
  runs
}

#' The key of the string that follows "Answer:" for a type
#' @keywords internal
#' @noRd
lss_q_open_key <- function(type) {
  switch(
    EXPR = as.character(type),
    "N" = "q_open_number", "K" = "q_open_numbers",
    "S" = "q_open_short_text", "Q" = "q_open_short_texts",
    "T" = "q_open_long_text", "U" = "q_open_long_text",
    "D" = "q_open_date", "5" = "q_open_five_point",
    "|" = "q_open_file", "I" = "q_open_language",
    "q_open_free"
  )
}

#' What a question asks, below its line, by type
#' @keywords internal
#' @noRd
lss_q_body <- function(ctx, q) {
  ch <- ctx$chrome
  P <- ctx$props
  type <- as.character(q$type)
  rows <- function() {
    lss_q_option_paras(ctx, lss_q_subq_items(ctx, q, "0"), abridge = FALSE)
  }
  answer_line <- function(key) {
    list(lss_q_para(
      c(list(lss_q_run(ctx, paste0(ch$q_answer, " "), P$hint, chrome = TRUE)),
        lss_q_open_runs(ctx, q, key)),
      "hint"
    ))
  }
  other <- identical(toupper(lss_q_or(q$other, "N")), "Y")
  out <- if (type %in% c("M", "P")) {
    items <- lss_q_subq_items(ctx, q, "0")
    if (other) items <- c(items, list(lss_q_other_item(ctx, q, "other")))
    lss_q_option_paras(ctx, items)
  } else if (type %in% c("L", "!", "R")) {
    items <- lss_q_answer_items(ctx, q, "0")
    if (other && type != "R") {
      items <- c(items, list(lss_q_other_item(ctx, q, "-oth-")))
    }
    lss_q_option_paras(ctx, items)
  } else if (identical(type, "O")) {
    comment <- lss_q_item("", paste(ch$item_comment, ch$q_free_text),
                          chrome = TRUE)
    lss_q_option_paras(ctx, lss_q_answer_items(ctx, q, "0"),
                       tail = list(comment))
  } else if (type %in% c("F", "H")) {
    c(rows(), list(lss_q_row_prefix(ctx)),
      lss_q_option_paras(ctx, lss_q_answer_items(ctx, q, "0")))
  } else if (type %in% c("A", "B")) {
    key <- if (identical(type, "B")) "q_open_ten_point" else "q_open_five_point"
    c(rows(), list(lss_q_row_prefix(
      ctx, list(lss_q_run(ctx, ch[[key]], P$hint, chrome = TRUE))
    )))
  } else if (type %in% c("C", "E")) {
    c(rows(), list(lss_q_row_prefix(ctx)),
      lss_q_option_paras(ctx, lss_q_builtin_items(ctx, type)))
  } else if (type %in% c("Y", "G")) {
    lss_q_option_paras(ctx, lss_q_builtin_items(ctx, type))
  } else if (identical(type, "1")) {
    out <- rows()
    scales <- sort(unique(vapply(q$answers, function(a) {
      lss_q_or(a$scale_id, "0")
    }, character(1))))
    for (k in seq_along(scales)) {
      hdr <- lss_q_attr(ctx, q$attributes,
                        if (k == 1L) "dualscale_headerA" else "dualscale_headerB",
                        q$code)
      after <- if (nzchar(hdr$text)) {
        c(list(lss_q_run(ctx, hdr$text, P$opt)),
          if (hdr$missing) list(lss_q_missing_run(ctx)))
      }
      out <- c(out, list(lss_q_row_prefix(ctx, after)),
               lss_q_option_paras(ctx, lss_q_answer_items(ctx, q, scales[k])))
    }
    out
  } else if (type %in% c(";", ":")) {
    cols <- lss_q_subq_items(ctx, q, "1")
    labels <- vapply(cols, function(it) it$label, character(1))
    filtered <- vapply(cols, function(it) !is.null(it$filter), logical(1))
    label_runs <- if (any(filtered)) {
      # A column with a condition of its own reads `label (filter)`.
      unlist(lapply(seq_along(cols), function(k) {
        c(if (k > 1L) list(lss_q_run(ctx, ", ", P$hint)),
          list(lss_q_run(ctx, labels[k], P$hint)),
          if (filtered[k]) {
            list(lss_q_run(ctx, paste0(" (", cols[[k]]$filter, ")"), P$code,
                           chrome = TRUE))
          })
      }), recursive = FALSE)
    } else {
      list(lss_q_run(ctx, paste(labels, collapse = ", "), P$hint))
    }
    after <- if (length(labels)) {
      c(label_runs,
        if (any(vapply(cols, function(it) it$missing, logical(1)))) {
          list(lss_q_missing_run(ctx))
        })
    }
    c(rows(), list(lss_q_row_prefix(ctx, after)))
  } else if (type %in% c("K", "Q")) {
    sub <- rows()
    if (length(sub)) {
      key <- if (identical(type, "K")) "q_open_number" else "q_open_short_text"
      c(sub, list(lss_q_row_prefix(ctx, lss_q_open_runs(ctx, q, key))))
    } else {
      answer_line(lss_q_open_key(type))
    }
  } else {
    answer_line(lss_q_open_key(type))
  }
  if (length(out) == 0L) out <- answer_line("q_open_free")
  out
}

#' Plan the top of the first page: logo, title, subtitle, legend, table of
#' contents
#' @keywords internal
#' @noRd
lss_q_plan_top <- function(ctx, body, title, subtitle = NULL, logo = NULL,
                           logo_width = 1.5, logo_height = 0.75,
                           show_toc = TRUE) {
  ch <- ctx$chrome
  P <- ctx$props
  out <- list()
  if (!is.null(logo)) {
    out[[length(out) + 1L]] <- lss_q_para(
      list(officer::external_img(src = logo, width = logo_width,
                                 height = logo_height)),
      "free", fp = officer::fp_par(padding.bottom = 6)
    )
  }
  out[[length(out) + 1L]] <- lss_q_para(
    list(lss_q_run(ctx, title, P$title)), "free",
    fp = officer::fp_par(padding.bottom = 2)
  )
  sub <- if (is.null(subtitle)) {
    lss_q_run(ctx, ch$questionnaire_title, P$sub, chrome = TRUE)
  } else {
    lss_q_run(ctx, subtitle, P$sub)
  }
  out[[length(out) + 1L]] <- lss_q_para(list(sub), "free",
                                        fp = officer::fp_par(padding.bottom = 6))
  legend <- lss_q_legend(
    ch, ctx$count, show_codes = ctx$show_codes,
    mandatory = ctx$mandatory, soft = ctx$soft,
    language = if (length(ctx$lss$languages) > 1L) ctx$language
  )
  out[[length(out) + 1L]] <- lss_q_para(
    list(lss_q_run(ctx, legend, P$note, chrome = TRUE)), "free",
    fp = officer::fp_par(padding.bottom = 4)
  )
  if (isTRUE(show_toc) && ctx$count$groups >= 2L) {
    out[[length(out) + 1L]] <- lss_q_para(
      list(lss_q_run(ctx, ch$toc_title, P$toc_title, chrome = TRUE)), "free",
      fp = officer::fp_par(padding.top = 8, padding.bottom = 2)
    )
    names <- vapply(body$toc, function(e) e$name, character(1))
    col <- (lss_q_text_width(names, ctx$theme$font_body, ctx$base_size,
                             measure = ctx$measure) + 14) / 72
    col <- min(col, ctx$theme$content_width_in - 1.25)
    for (e in body$toc) {
      link <- officer::hyperlink_ftext(href = paste0("#", e$bookmark),
                                       text = e$name, prop = P$toc)
      if (isTRUE(e$chrome) && !is.null(ctx$chrome_tag)) {
        link$lang <- ctx$chrome_tag
        class(link) <- c("lssdoc_lang_run", class(link))
      }
      out[[length(out) + 1L]] <- lss_q_para(
        c(list(link),
          if (nzchar(e$range)) {
            list(officer::run_tab(),
                 lss_q_run(ctx, e$range, P$running, chrome = TRUE))
          }),
        "free",
        fp = officer::fp_par(
          padding.top = 1, padding.bottom = 1,
          tabs = officer::fp_tabs(officer::fp_tab(pos = col, style = "left"))
        )
      )
    }
  }
  out
}

#' Name and id of the paragraph style of the group and section headings,
#' which the running header refers to
#' @keywords internal
#' @noRd
lss_q_heading_style <- function() {
  c(id = "lssdocQuestionGroup", name = "Question group")
}

#' Write the planned paragraphs with officer
#'
#' The width of the left column is fixed here, from the largest question
#' number and the tags the plan printed. Then the page (A4 portrait, the
#' margins of the full document, the running header and the footer), the
#' paragraph properties of the prototype (no paragraph split across two
#' pages, hyphenation for justified text only) and the language declared
#' in `styles.xml`.
#'
#' @keywords internal
#' @noRd
lss_q_emit <- function(ctx, paras, title) {
  P <- ctx$props
  theme <- ctx$theme
  numbers <- ctx$numbers$no[!is.na(ctx$numbers$no)]
  M <- lss_q_margin_width(
    if (length(numbers)) as.character(max(numbers)) else character(0),
    unique(ctx$tags), theme$font_body, ctx$base_size, measure = ctx$measure
  )
  style <- lss_q_heading_style()
  doc <- officer::read_docx()
  doc <- officer::docx_set_settings(doc, auto_hyphenation = TRUE)
  doc <- officer::docx_set_paragraph_style(
    doc, style_id = style[["id"]], style_name = style[["name"]],
    base_on = "Normal",
    fp_p = officer::fp_par(
      padding.top = 16, padding.bottom = 3, keep_with_next = TRUE,
      border.bottom = officer::fp_border(color = theme$color_primary, width = 1)
    ),
    fp_t = P$group
  )
  fp_for <- function(p) {
    align <- if (isTRUE(p$justify)) "justify" else "left"
    switch(
      p$kind,
      q = officer::fp_par(
        padding.top = if (is.null(p$top)) 8 else p$top, padding.bottom = 1,
        padding.left = M, hanging = M, keep_with_next = isTRUE(p$keep),
        text.align = align
      ),
      line = officer::fp_par(
        padding.top = 0, padding.bottom = 0, padding.left = M + p$w,
        hanging = p$w, keep_with_next = isTRUE(p$keep)
      ),
      hint = officer::fp_par(
        padding.top = if (is.null(p$top)) 0 else p$top, padding.bottom = 0,
        padding.left = M, keep_with_next = isTRUE(p$keep)
      ),
      p$fp
    )
  }
  for (p in paras) {
    if (identical(p$kind, "heading")) {
      doc <- officer::body_add_fpar(
        doc, do.call(officer::fpar, p$runs), style = style[["name"]]
      )
      doc <- lss_set_outline_level(doc, 1L)
      doc <- officer::body_bookmark(doc, p$bookmark)
    } else {
      doc <- officer::body_add_fpar(
        doc, do.call(officer::fpar, c(p$runs, list(fp_p = fp_for(p))))
      )
    }
  }
  lss_q_finish_paragraphs(officer::docx_body_xml(doc))

  width <- theme$content_width_in
  footer <- officer::block_list(officer::fpar(
    lss_q_run(ctx, lss_truncate_title(title), P$running),
    officer::run_tab(),
    officer::run_word_field("PAGE", prop = P$running),
    officer::ftext("/", prop = P$running),
    officer::run_word_field("NUMPAGES", prop = P$running),
    fp_p = officer::fp_par(
      tabs = officer::fp_tabs(officer::fp_tab(pos = width, style = "right"))
    )
  ))
  doc <- officer::body_set_default_section(doc, officer::prop_section(
    page_size = officer::page_size(width = 8.27, height = 11.69,
                                   orient = "portrait"),
    page_margins = officer::page_mar(
      top = 1.0, bottom = 1.0, left = lss_margin_side_in,
      right = lss_margin_side_in, header = 0.4, footer = 0.4, gutter = 0
    ),
    header_default = officer::block_list(officer::fpar(
      officer::run_word_field(
        sprintf("STYLEREF \"%s\"", style[["name"]]), prop = P$running
      ),
      fp_p = officer::fp_par(text.align = "right")
    )),
    header_first = officer::block_list(officer::fpar(
      officer::ftext("", prop = P$running)
    )),
    footer_default = footer,
    footer_first = footer
  ))
  lss_q_set_styles_lang(doc, ctx$proofing)
  doc
}

#' Add a child to a `w:pPr` at its place in the schema sequence
#'
#' Word reads the children of `w:pPr` in the order of CT_PPr. The new
#' element goes before the first existing child that the schema puts
#' after it. An element already present is left as it is.
#'
#' @keywords internal
#' @noRd
lss_q_ppr_add <- function(ppr, name) {
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  order <- c(
    "pStyle", "keepNext", "keepLines", "pageBreakBefore", "framePr",
    "widowControl", "numPr", "suppressLineNumbers", "pBdr", "shd", "tabs",
    "suppressAutoHyphens", "kinsoku", "wordWrap", "overflowPunct",
    "topLinePunct", "autoSpaceDE", "autoSpaceDN", "bidi", "adjustRightInd",
    "snapToGrid", "spacing", "ind", "contextualSpacing", "mirrorIndents",
    "suppressOverlap", "jc", "textDirection", "textAlignment",
    "textboxTightWrap", "outlineLvl", "divId", "cnfStyle", "rPr", "sectPr",
    "pPrChange"
  )
  kids <- xml2::xml_children(ppr)
  names <- xml2::xml_name(kids)
  if (name %in% names) return(invisible(ppr))
  pos <- match(name, order)
  later <- which(match(names, order) > pos)
  node <- xml2::as_xml_document(sprintf("<w:%s xmlns:w=\"%s\"/>", name, ns[["w"]]))
  if (length(later)) {
    xml2::xml_add_sibling(kids[[later[1]]], node, .where = "before")
  } else {
    xml2::xml_add_child(ppr, node)
  }
  invisible(ppr)
}

#' Paragraph properties of the prototype, on every body paragraph
#'
#' No paragraph breaks across two pages (`w:keepLines`), and hyphenation
#' only where the text is justified: every other paragraph gets
#' `w:suppressAutoHyphens`, so that a question is never hyphenated.
#'
#' @keywords internal
#' @noRd
lss_q_finish_paragraphs <- function(body) {
  w <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  ns <- c(w = w)
  for (p in xml2::xml_find_all(body, "//w:body/w:p", ns)) {
    ppr <- xml2::xml_find_first(p, "w:pPr", ns)
    if (inherits(ppr, "xml_missing")) {
      ppr <- xml2::xml_add_child(
        p, xml2::as_xml_document(sprintf("<w:pPr xmlns:w=\"%s\"/>", w)),
        .where = 0L
      )
      ppr <- xml2::xml_find_first(p, "w:pPr", ns)
    }
    lss_q_ppr_add(ppr, "keepLines")
    justified <- !inherits(
      xml2::xml_find_first(ppr, "w:jc[@w:val='both']", ns), "xml_missing"
    )
    if (!justified) lss_q_ppr_add(ppr, "suppressAutoHyphens")
  }
  invisible(body)
}

#' Declare the content language in `styles.xml`
#'
#' The officer template declares `en-US` in the default run properties,
#' so Word would check the spelling and hyphenate with English rules. The
#' first `w:lang` (the document defaults) gets the content tag.
#'
#' @keywords internal
#' @noRd
lss_q_set_styles_lang <- function(doc, tag) {
  path <- file.path(doc$package_dir, "word", "styles.xml")
  xs <- paste(readLines(path, warn = FALSE, encoding = "UTF-8"), collapse = "\n")
  xs <- sub("<w:lang w:val=\"[^\"]*\"", sprintf("<w:lang w:val=\"%s\"", tag), xs)
  writeLines(enc2utf8(xs), path, useBytes = TRUE)
  invisible(doc)
}

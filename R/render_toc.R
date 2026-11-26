# Table of contents, variable index and survey-level text blocks (welcome, description, endtext).
#
# Extracted from R/render_lss_docx.R.

#' Render the variable index at the end of the document
#'
#' One row per item with its variable code and number label, sorted
#' alphabetically (case-insensitive). The labels are the ones the renderer
#' printed in the `No` column of the card or row holding the variable and
#' on the item heading (`"28"`, `"28.1"`, an en dash for an unnumbered
#' question), so the reader can use them as cross-references.
#'
#' @keywords internal
#' @noRd
lss_render_index <- function(doc, entries, theme) {
  doc <- officer::body_add_break(doc)
  doc <- lss_render_section_heading(
    doc, theme, theme$chrome$variable_index_title,
    lss_section_bookmark("index")
  )
  codes <- vapply(entries, function(e) e$code, character(1))
  nos <- vapply(entries, function(e) {
    v <- e$no
    if (is.null(v) || length(v) == 0L || is.na(v[1])) "" else as.character(v[1])
  }, character(1))
  ord <- order(tolower(codes))
  df <- data.frame(
    Variable = codes[ord],
    No = nos[ord],
    stringsAsFactors = FALSE,
    check.names = FALSE
  )
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft,
    Variable = theme$chrome$meta_variable,
    No = theme$chrome$meta_no
  )
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  # Variable codes are identifiers; rendering them in the monospace font
  # disambiguates l/1/I, 0/O and makes underscores visible -- which
  # matters in a variable index where the reader scans for an exact
  # match.
  ft <- flextable::font(ft, j = "Variable", fontname = theme$font_code, part = "body")
  # Index reads at the body size (like the questions), so the back matter
  # is not visibly smaller than the content it points to.
  ft <- flextable::fontsize(ft, size = theme$size_question, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_primary, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band, part = "header")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "header")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  # Cell-symmetric: Variable header reads as a word and the body holds
  # monospace identifiers -- both left. No header sits above a column
  # of right-aligned digits -- both right.
  ft <- flextable::align(ft, align = "left",  j = "Variable", part = "all")
  ft <- flextable::align(ft, align = "right", j = "No",       part = "all")
  ft <- flextable::width(ft, j = "Variable", width = 2.6, unit = "in")
  ft <- flextable::width(ft, j = "No", width = 0.6, unit = "in")
  flextable::body_add_flextable(doc, ft, align = "left")
}

#' The membership condition of a quota, as plain text
#'
#' The members are grouped by question, in the order of their first
#' member, each question named by its code. One answer reads `Q = a`;
#' several answers of the same question are alternatives and read as a
#' set, `Q \u2208 {a, b}`, as lss_humanize_relevance() writes one. With
#' `label_lang`, each code is followed by its answer label in that
#' language, in parentheses, when it has one. The questions are joined by
#' the localized AND. A question that is not in the survey is named by its
#' id; an answer without a label shows its code alone.
#'
#' @param lss An `lss` object.
#' @param quota_id The `id` of the quota.
#' @param theme Optional theme, for the localized AND.
#' @param label_lang `NULL`, or the language of the answer labels.
#' @return A string, `""` for a quota without member.
#' @keywords internal
#' @noRd
lss_quota_condition <- function(lss, quota_id, theme = NULL,
                                label_lang = NULL) {
  members <- lss$quota_members
  if (is.null(members) || nrow(members) == 0L) return("")
  mem <- members[members$quota_id == quota_id, , drop = FALSE]
  if (nrow(mem) == 0L) return("")
  chrome <- lss_filter_chrome(theme)
  q_title <- function(qid) {
    i <- which(lss$questions$qid == qid)
    if (length(i)) lss$questions$title[i[1]] else qid
  }
  ans_label <- function(qid, code) {
    if (is.null(label_lang) || is.null(lss$answers) ||
        is.null(lss$answer_l10ns)) {
      return(NA_character_)
    }
    ai <- which(lss$answers$qid == qid & lss$answers$code == code)
    if (!length(ai)) return(NA_character_)
    aid <- lss$answers$aid[ai[1]]
    li <- which(lss$answer_l10ns$aid == aid &
                  lss$answer_l10ns$language == label_lang)
    if (length(li)) lss$answer_l10ns$answer[li[1]] else NA_character_
  }
  parts <- vapply(unique(mem$qid), function(qid) {
    codes <- unique(mem$code[mem$qid == qid])
    vals <- vapply(codes, function(code) {
      lbl <- ans_label(qid, code)
      if (!is.na(lbl) && nzchar(lbl)) sprintf("%s (%s)", code, lbl) else code
    }, character(1), USE.NAMES = FALSE)
    if (length(vals) == 1L) {
      sprintf("%s = %s", q_title(qid), vals)
    } else {
      sprintf("%s %s {%s}", q_title(qid), chrome$inset,
              paste(vals, collapse = ", "))
    }
  }, character(1), USE.NAMES = FALSE)
  paste(parts, collapse = sprintf(" %s ", chrome$and))
}

#' Render the quotas section (sampling caps) as back matter
#'
#' One block per quota: the localized name, a status line (active /
#' limit / action when full), the membership condition resolved to
#' question codes and answer labels, and the localized "quota full"
#' message. Skipped entirely when the survey defines no quotas, so a
#' survey without quotas gets no empty section.
#'
#' @keywords internal
#' @noRd
lss_render_quotas <- function(doc, lss, langs, theme) {
  quotas <- lss$quotas
  if (is.null(quotas) || nrow(quotas) == 0L) return(doc)
  chrome <- theme$chrome
  qls <- lss$quota_languagesettings

  qls_field <- function(quota_id, lang, field) {
    if (is.null(qls)) return(NA_character_)
    li <- which(qls$quotals_quota_id == quota_id & qls$quotals_language == lang)
    if (length(li) && field %in% names(qls)) qls[[field]][li[1]] else NA_character_
  }

  doc <- officer::body_add_break(doc)
  doc <- lss_render_section_heading(
    doc, theme, chrome$quotas_title, lss_section_bookmark("quotas")
  )

  # One row per quota in a single table: name (with active state), limit,
  # action when full, the membership condition resolved to question codes
  # and answer labels, and the localized "quota full" message stacked per
  # language in its cell.
  n <- nrow(quotas)
  name_v <- character(n); limit_v <- character(n)
  action_v <- character(n); cond_v <- character(n)
  msg_cells <- vector("list", n)
  cell_props <- officer::fp_text(font.family = theme$font_body,
                                 font.size = theme$size_question,
                                 color = theme$color_text)
  lang_props <- officer::fp_text(font.family = theme$font_body,
                                 font.size = theme$size_question,
                                 color = theme$color_muted, bold = TRUE)

  for (qi in seq_len(n)) {
    qrow <- quotas[qi, , drop = FALSE]
    qid_q <- qrow$id

    # Localized name: first non-empty quotals_name, else the structural name.
    name <- NA_character_
    for (lg in langs) {
      v <- qls_field(qid_q, lg, "quotals_name")
      if (!is.na(v) && nzchar(trimws(v))) { name <- v; break }
    }
    if (is.na(name) || !nzchar(trimws(name))) name <- qrow$name
    active_lbl <- if (identical(qrow$active, "1")) chrome$quota_active else chrome$quota_inactive
    name_v[qi] <- sprintf("%s (%s)", name, active_lbl)

    limit_v[qi] <- as.character(qrow$qlimit)
    action_v[qi] <- switch(as.character(qrow$action),
                           "1" = chrome$quota_action_terminate,
                           "2" = chrome$quota_action_confirm,
                           as.character(qrow$action))

    # The condition, with the answer labels of the first language.
    cond <- lss_quota_condition(lss, qid_q, theme, label_lang = langs[1])
    cond_v[qi] <- if (nzchar(cond)) cond else theme$empty_marker

    # Message cell: one line per language that carries a message.
    chunks <- list()
    for (lg in langs) {
      m <- qls_field(qid_q, lg, "quotals_message")
      if (is.na(m) || !nzchar(trimws(m))) next
      m <- gsub("[ \t\r\n]+", " ", trimws(gsub("<[^>]+>", " ", m)))
      if (length(chunks) > 0L) {
        chunks[[length(chunks) + 1L]] <- flextable::as_chunk("\n", props = cell_props)
      }
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
        sprintf("%s  ", lss_language_label(lg)), props = lang_props)
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(m, props = cell_props)
    }
    msg_cells[[qi]] <- if (length(chunks)) {
      do.call(flextable::as_paragraph, chunks)
    } else {
      flextable::as_paragraph(flextable::as_chunk(theme$empty_marker, props = cell_props))
    }
  }

  df <- data.frame(name = name_v, limit = limit_v, action = action_v,
                   condition = cond_v, message = "",
                   stringsAsFactors = FALSE, check.names = FALSE)
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft,
    name = chrome$quotas_title, limit = chrome$quota_limit,
    action = chrome$quota_when_full, condition = chrome$quota_condition,
    message = chrome$quota_message
  )
  for (qi in seq_len(n)) {
    ft <- flextable::compose(ft, i = qi, j = "message", value = msg_cells[[qi]])
  }
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  # Quotas read at the body size, like the questions.
  ft <- flextable::fontsize(ft, size = theme$size_question, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_primary, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band, part = "header")
  ft <- flextable::bold(ft, j = "name", part = "body")
  ft <- flextable::color(ft, j = "name", color = theme$color_primary, part = "body")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  ft <- flextable::vline_left(ft, border = thin, part = "all")
  ft <- flextable::vline_right(ft, border = thin, part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "header")
  ft <- flextable::padding(ft, padding = 3, part = "all")
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::align(ft, j = "limit", align = "center", part = "all")
  ft <- flextable::width(ft, j = "name", width = 1.00, unit = "in")
  ft <- flextable::width(ft, j = "limit", width = 0.55, unit = "in")
  ft <- flextable::width(ft, j = "action", width = 1.15, unit = "in")
  ft <- flextable::width(ft, j = "condition", width = 1.70, unit = "in")
  # message absorbs the surplus so the table spans the full body width:
  # 1.90 in in portrait (6.30 total), wider in landscape / A3.
  ft <- flextable::width(ft, j = "message",
                         width = theme$content_width_in - 4.40, unit = "in")
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc
}

#' TRUE when the survey carries a data-protection / consent notice worth
#' rendering (policy notice not turned off, and a notice or label present
#' in at least one displayed language). Shared by the consent renderer
#' and the table of contents so both agree on whether the section shows.
#' @keywords internal
#' @noRd
lss_consent_present <- function(lss, langs) {
  ls <- lss$survey_language_settings
  if (is.null(ls) || !("surveyls_language" %in% names(ls))) return(FALSE)
  show <- if (!is.null(lss$surveys) &&
              "showsurveypolicynotice" %in% names(lss$surveys)) {
    as.character(lss$surveys$showsurveypolicynotice[1])
  } else {
    NA_character_
  }
  if (!is.na(show) && identical(show, "0")) return(FALSE)
  getf <- function(field, lang) {
    if (!(field %in% names(ls))) return(NA_character_)
    i <- which(ls$surveyls_language == lang)
    if (length(i)) ls[[field]][i[1]] else NA_character_
  }
  has <- function(field) any(vapply(langs, function(lg) {
    v <- getf(field, lg); !is.na(v) && nzchar(trimws(v))
  }, logical(1)))
  has("surveyls_policy_notice") || has("surveyls_policy_notice_label")
}

#' Render the data-protection / consent block as front matter
#'
#' Surfaces the survey's privacy policy notice and its consent checkbox
#' label (`surveyls_policy_notice` / `surveyls_policy_notice_label`),
#' side by side across the displayed languages, with the consent
#' checkbox drawn as an empty box glyph before its label. This is the
#' gate the respondent meets before the questions, and the text an
#' ethics committee reviews most closely. Skipped when the survey turns
#' the policy notice off (`showsurveypolicynotice = 0`) or carries no
#' notice / label content.
#'
#' @keywords internal
#' @noRd
lss_render_consent <- function(doc, lss, langs, theme) {
  if (!lss_consent_present(lss, langs)) return(doc)
  ls <- lss$survey_language_settings
  getf <- function(field, lang) {
    if (!(field %in% names(ls))) return(NA_character_)
    i <- which(ls$surveyls_language == lang)
    if (length(i)) ls[[field]][i[1]] else NA_character_
  }
  has <- function(field) any(vapply(langs, function(lg) {
    v <- getf(field, lg); !is.na(v) && nzchar(trimws(v))
  }, logical(1)))

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- lss_render_section_heading(
    doc, theme, theme$chrome$consent_title, lss_section_bookmark("consent")
  )

  mk_ft <- function(cell_fun) {
    df <- as.data.frame(matrix("", nrow = 1, ncol = length(langs)),
                        stringsAsFactors = FALSE)
    names(df) <- langs
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(
      ft, values = stats::setNames(lss_language_label(langs), langs)
    )
    for (lg in langs) ft <- flextable::compose(ft, i = 1L, j = lg, value = cell_fun(lg))
    lss_table_polish(ft, theme, lang_cols = langs)
  }

  # The consent checkbox: an empty square (equal borders) rendered a few
  # points larger than the label so it reads as a real, tickable box
  # rather than a tiny glyph.
  if (has("surveyls_policy_notice_label")) {
    box_props <- officer::fp_text(font.family = theme$font_body,
                                  font.size = theme$size_heading1 + 2L,
                                  color = theme$color_text)
    lab_props <- officer::fp_text(font.family = theme$font_body,
                                  font.size = theme$size_question,
                                  bold = TRUE, color = theme$color_text)
    ft1 <- mk_ft(function(lg) {
      lab <- getf("surveyls_policy_notice_label", lg)
      lab <- if (is.na(lab)) "" else trimws(gsub("<[^>]+>", " ", lab))
      if (!nzchar(lab)) return(flextable::as_paragraph(flextable::as_chunk("")))
      flextable::as_paragraph(
        flextable::as_chunk("\u25a1  ", props = box_props),
        flextable::as_chunk(lab, props = lab_props)
      )
    })
    doc <- flextable::body_add_flextable(doc, ft1, align = "left")
  }
  # The data-protection notice text (HTML preserved via lss_compose).
  if (has("surveyls_policy_notice")) {
    ft2 <- mk_ft(function(lg) {
      lss_compose(getf("surveyls_policy_notice", lg), theme, size = theme$size_subq)
    })
    doc <- flextable::body_add_flextable(doc, ft2, align = "left")
  }
  doc
}

#' Render a static, always-populated table of contents
#'
#' Lists the sections of the document and, under the questionnaire, its
#' groups, one per line, each followed by the numbers of its questions
#' (`numbers`, computed from the model when `NULL`). The groups are listed
#' only when the document draws them (`show_groups`): their bookmarks sit
#' on the group headings (cards) or on the group rows (table).
#' We render a manual list instead of a Word TOC field for three reasons:
#' (1) Word's TOC field auto-refresh produces page numbers `1` everywhere
#' when refresh runs before pagination -- a well-known quirk; (2)
#' LibreOffice does not refresh field values on open or during headless
#' PDF conversion, so a TOC field would always look empty there; (3) a
#' static text list is always visible in every viewer without any
#' interaction. No page numbers (we cannot predict them without a real
#' Word render pass).
#'
#' @keywords internal
#' @noRd
lss_render_toc <- function(doc, model, theme, sections = list(),
                           numbers = NULL, show_groups = TRUE) {
  chrome <- theme$chrome
  # Same styled heading as every other section (14 pt bold primary with a
  # bottom filet and space above), so the table of contents is not glued
  # flush to the top-left corner and reads as part of the same document.
  doc <- lss_render_section_heading(
    doc, theme, chrome$toc_title, lss_section_bookmark("toc")
  )

  # Clickable entries in the accent colour, one step above the body size so
  # the contents list stays comfortably legible. Top-level sections are
  # bold; the questionnaire's groups are indented and regular weight. Each
  # entry points to the bookmark anchored on the matching heading.
  entry_size <- theme$size_question + 1L
  top_props <- officer::fp_text(
    font.family = theme$font_body, font.size = entry_size,
    color = theme$color_accent, bold = TRUE
  )
  grp_props <- officer::fp_text(
    font.family = theme$font_body, font.size = entry_size,
    color = theme$color_accent
  )
  # The question numbers of a group follow its name after a tab, outside
  # the link, in the muted colour, as in render_questionnaire().
  range_props <- officer::fp_text(
    font.family = theme$font_body, font.size = entry_size,
    color = theme$color_muted
  )
  entry <- function(doc, label, bookmark, props, indent = 0, range = "",
                    tab = NULL) {
    link <- officer::hyperlink_ftext(
      href = paste0("#", bookmark), text = label, prop = props
    )
    if (!nzchar(range)) {
      fp_p <- officer::fp_par(padding.top = 2, padding.bottom = 2,
                              padding.left = indent)
      return(officer::body_add_fpar(doc, officer::fpar(link, fp_p = fp_p)))
    }
    fp_p <- officer::fp_par(
      padding.top = 2, padding.bottom = 2, padding.left = indent,
      tabs = officer::fp_tabs(officer::fp_tab(pos = tab, style = "left"))
    )
    officer::body_add_fpar(
      doc,
      officer::fpar(link, officer::run_tab(),
                    officer::ftext(range, prop = range_props), fp_p = fp_p)
    )
  }

  if (isTRUE(sections$audit)) {
    doc <- entry(doc, chrome$audit_findings_title,
                 lss_section_bookmark("audit"), top_props)
  }
  if (isTRUE(sections$consent)) {
    doc <- entry(doc, chrome$consent_title,
                 lss_section_bookmark("consent"), top_props)
  }
  # Questionnaire section, then its groups indented beneath it.
  doc <- entry(doc, chrome$questionnaire_title,
               lss_section_bookmark("questionnaire"), top_props)
  # The groups only when the document draws them: their bookmarks sit on
  # the group headings (cards) or the group rows (table).
  if (isTRUE(show_groups) && length(model$groups)) {
    if (is.null(numbers)) numbers <- lss_item_numbers(model)
    primary <- model$languages[1]
    gnames <- vapply(model$groups, function(group) {
      gname <- group$names[[primary]]
      if (is.null(gname) || is.na(gname) || !nzchar(gname)) {
        gname <- paste0("Group ", group$gid)
      }
      lss_strip_group_number_prefix(gname)
    }, character(1))
    ranges <- vapply(model$groups, function(group) {
      qids <- vapply(group$questions, function(q) as.character(q$qid),
                     character(1))
      nums <- numbers$no[numbers$qid %in% qids & !is.na(numbers$no)]
      if (length(nums) == 0L) {
        ""
      } else if (length(nums) == 1L) {
        sprintf(chrome$q_toc_question_fmt, nums[1])
      } else {
        sprintf(chrome$q_toc_questions_fmt, min(nums), max(nums))
      }
    }, character(1))
    # The tab stop: past the longest group name, as in the questionnaire.
    tab <- (18 + lss_q_text_width(gnames, theme$font_body, entry_size) + 14) / 72
    tab <- min(tab, theme$content_width_in - 1.25)
    for (i in seq_along(model$groups)) {
      doc <- entry(doc, gnames[i], lss_group_bookmark(i), grp_props,
                   indent = 18, range = ranges[i], tab = tab)
    }
  }
  if (isTRUE(sections$quotas)) {
    doc <- entry(doc, chrome$quotas_title,
                 lss_section_bookmark("quotas"), top_props)
  }
  if (isTRUE(sections$index)) {
    doc <- entry(doc, chrome$variable_index_title,
                 lss_section_bookmark("index"), top_props)
  }
  doc
}

#' Side-by-side localized welcome text (omitted if all languages are empty)
#' @keywords internal
#' @noRd
lss_render_welcome <- function(doc, lss, langs, theme) {
  lss_render_localized_block(
    doc, lss, langs, theme,
    field = "surveyls_welcometext", title = theme$chrome$welcome_text_title
  )
}

#' Side-by-side localized survey description (the short multilingual
#' "what this survey is about" intro that LimeSurvey shows above the
#' welcome text on the landing page). Same rendering shape as
#' [lss_render_welcome()], just keyed on `surveyls_description`.
#' @keywords internal
#' @noRd
lss_render_description <- function(doc, lss, langs, theme) {
  lss_render_localized_block(
    doc, lss, langs, theme,
    field = "surveyls_description", title = theme$chrome$description_title
  )
}

#' Side-by-side localized end text
#' @keywords internal
#' @noRd
lss_render_endtext <- function(doc, lss, langs, theme) {
  lss_render_localized_block(
    doc, lss, langs, theme,
    field = "surveyls_endtext", title = theme$chrome$end_text_title
  )
}

#' Render a generic side-by-side block of localized HTML
#' @keywords internal
#' @noRd
lss_render_localized_block <- function(doc, lss, langs, theme, field, title) {
  ls_settings <- lss$survey_language_settings
  if (is.null(ls_settings) || nrow(ls_settings) == 0) {
    return(doc)
  }
  vals <- vapply(langs, function(lg) {
    v <- ls_settings[[field]][ls_settings$surveyls_language == lg]
    if (length(v) == 0) NA_character_ else v[1]
  }, character(1))
  any_present <- any(!is.na(vals) & nzchar(trimws(vals)))
  if (!any_present) return(doc)

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      title,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    ))
  )

  df <- as.data.frame(matrix("", nrow = 1, ncol = length(langs)), stringsAsFactors = FALSE)
  names(df) <- langs
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft,
    values = stats::setNames(lss_language_label(langs), langs)
  )
  for (lg in langs) {
    ft <- flextable::compose(
      ft, i = 1L, j = lg,
      value = lss_compose(vals[[lg]], theme, size = theme$size_question)
    )
  }
  ft <- lss_table_polish(ft, theme, lang_cols = langs)
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc
}


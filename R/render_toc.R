# Table of contents, variable index and survey-level text blocks (welcome, description, endtext).
#
# Extracted from R/render_lss_docx.R.

#' Render the variable index at the end of the document
#'
#' One row per item with its variable code and item number, sorted
#' alphabetically (case-insensitive). The numbers match the `No` column
#' of each item's meta table and the visible numeric prefix on the item
#' heading, so the reader can use them as cross-references.
#'
#' @keywords internal
#' @noRd
lss_render_index <- function(doc, entries, theme) {
  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      theme$chrome$variable_index_title,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    )),
    style = "heading 1"
  )
  codes <- vapply(entries, function(e) e$code, character(1))
  nos <- vapply(entries, function(e) as.integer(e$no), integer(1))
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
  ft <- flextable::fontsize(ft, size = theme$size_answer, part = "all")
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
  members <- lss$quota_members
  qls <- lss$quota_languagesettings

  # qid -> question code; (qid, code) -> localized answer label.
  q_title <- function(qid) {
    i <- which(lss$questions$qid == qid)
    if (length(i)) lss$questions$title[i[1]] else qid
  }
  ans_label <- function(qid, code, lang) {
    if (is.null(lss$answers) || is.null(lss$answer_l10ns)) return(NA_character_)
    ai <- which(lss$answers$qid == qid & lss$answers$code == code)
    if (!length(ai)) return(NA_character_)
    aid <- lss$answers$aid[ai[1]]
    li <- which(lss$answer_l10ns$aid == aid & lss$answer_l10ns$language == lang)
    if (length(li)) lss$answer_l10ns$answer[li[1]] else NA_character_
  }
  qls_field <- function(quota_id, lang, field) {
    if (is.null(qls)) return(NA_character_)
    li <- which(qls$quotals_quota_id == quota_id & qls$quotals_language == lang)
    if (length(li) && field %in% names(qls)) qls[[field]][li[1]] else NA_character_
  }

  doc <- officer::body_add_break(doc)
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      chrome$quotas_title,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    )),
    style = "heading 1"
  )

  name_props <- officer::fp_text(font.family = theme$font_body,
                                 font.size = theme$size_subq, bold = TRUE,
                                 color = theme$color_primary)
  muted_props <- officer::fp_text(font.family = theme$font_body,
                                  font.size = theme$size_meta,
                                  color = theme$color_muted)
  body_props <- officer::fp_text(font.family = theme$font_body,
                                 font.size = theme$size_meta,
                                 color = theme$color_text)

  for (qi in seq_len(nrow(quotas))) {
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
    action_lbl <- switch(as.character(qrow$action),
                         "1" = chrome$quota_action_terminate,
                         "2" = chrome$quota_action_confirm,
                         as.character(qrow$action))
    status <- sprintf("  —  %s · %s %s · %s: %s",
                      active_lbl, chrome$quota_limit, qrow$qlimit,
                      chrome$quota_when_full, action_lbl)

    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(name, prop = name_props),
        officer::ftext(status, prop = muted_props),
        fp_p = officer::fp_par(padding.top = 6, padding.bottom = 1)
      )
    )

    # Membership condition.
    mem <- if (!is.null(members)) members[members$quota_id == qid_q, , drop = FALSE] else members[0, ]
    if (!is.null(mem) && nrow(mem) > 0L) {
      conds <- vapply(seq_len(nrow(mem)), function(mi) {
        qc <- q_title(mem$qid[mi]); code <- mem$code[mi]
        lbl <- ans_label(mem$qid[mi], code, langs[1])
        if (!is.na(lbl) && nzchar(lbl)) {
          sprintf("%s = %s (%s)", qc, code, lbl)
        } else {
          sprintf("%s = %s", qc, code)
        }
      }, character(1))
      cond_str <- paste(conds, collapse = sprintf(" %s ", chrome$filter_and))
      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(sprintf("%s: ", chrome$quota_condition), prop = muted_props),
          officer::ftext(cond_str, prop = body_props),
          fp_p = officer::fp_par(padding.bottom = 1)
        )
      )
    }

    # Localized "quota full" message, one muted line per language.
    msg_lines <- list()
    for (lg in langs) {
      m <- qls_field(qid_q, lg, "quotals_message")
      if (is.na(m) || !nzchar(trimws(m))) next
      m <- trimws(gsub("<[^>]+>", " ", m))
      m <- gsub("[ \t\r\n]+", " ", m)
      msg_lines[[length(msg_lines) + 1L]] <- officer::fpar(
        officer::ftext(sprintf("%s  ", lss_language_label(lg)),
                       prop = officer::fp_text(font.family = theme$font_body,
                                               font.size = theme$size_meta,
                                               color = theme$color_muted, bold = TRUE)),
        officer::ftext(m, prop = body_props),
        fp_p = officer::fp_par(padding.bottom = 0)
      )
    }
    if (length(msg_lines) > 0L) {
      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(officer::ftext(sprintf("%s:", chrome$quota_message),
                                     prop = muted_props),
                      fp_p = officer::fp_par(padding.bottom = 0))
      )
      for (ln in msg_lines) doc <- officer::body_add_fpar(doc, ln)
    }
  }
  doc
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
  ls <- lss$survey_language_settings
  if (is.null(ls) || !("surveyls_language" %in% names(ls))) return(doc)
  show <- if (!is.null(lss$surveys) &&
              "showsurveypolicynotice" %in% names(lss$surveys)) {
    as.character(lss$surveys$showsurveypolicynotice[1])
  } else {
    NA_character_
  }
  if (!is.na(show) && identical(show, "0")) return(doc)

  getf <- function(field, lang) {
    if (!(field %in% names(ls))) return(NA_character_)
    i <- which(ls$surveyls_language == lang)
    if (length(i)) ls[[field]][i[1]] else NA_character_
  }
  has <- function(field) any(vapply(langs, function(lg) {
    v <- getf(field, lg); !is.na(v) && nzchar(trimws(v))
  }, logical(1)))
  if (!has("surveyls_policy_notice") && !has("surveyls_policy_notice_label")) {
    return(doc)
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      theme$chrome$consent_title,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    ))
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

  # The consent checkbox: an empty box glyph before the localized label.
  if (has("surveyls_policy_notice_label")) {
    ft1 <- mk_ft(function(lg) {
      lab <- getf("surveyls_policy_notice_label", lg)
      lab <- if (is.na(lab)) "" else trimws(gsub("<[^>]+>", " ", lab))
      flextable::as_paragraph(flextable::as_chunk(
        if (nzchar(lab)) paste0("☐  ", lab) else "",
        props = officer::fp_text(font.family = theme$font_body,
                                 font.size = theme$size_question,
                                 bold = TRUE, color = theme$color_text)
      ))
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
#' Lists the survey groups, one per line, with their sequential index.
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
lss_render_toc <- function(doc, model, theme) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        theme$chrome$toc_title,
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_heading1,
          bold = TRUE, color = theme$color_primary
        )
      )
    )
  )
  if (length(model$groups) == 0L) {
    return(doc)
  }
  primary <- model$languages[1]
  # TOC entries: clickable, in the accent color so the reader sees they
  # are hyperlinks. Each entry points to the bookmark we add on the
  # corresponding group heading in lss_render_group().
  link_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_question,
    color = theme$color_accent, underlined = FALSE
  )
  for (i in seq_along(model$groups)) {
    group <- model$groups[[i]]
    gname <- if (!is.null(group$names[[primary]])) group$names[[primary]] else NA
    if (is.null(gname) || is.na(gname) || !nzchar(gname)) {
      gname <- paste0("Group ", group$gid)
    }
    gname <- lss_strip_group_number_prefix(gname)
    entry_text <- sprintf("%d.  %s", i, gname)
    bookmark <- lss_group_bookmark(i)
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::hyperlink_ftext(
          href = paste0("#", bookmark),
          text = entry_text,
          prop = link_props
        ),
        fp_p = officer::fp_par(padding.top = 2, padding.bottom = 2)
      )
    )
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


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


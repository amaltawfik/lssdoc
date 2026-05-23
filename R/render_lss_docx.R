#' Render a parsed LimeSurvey structure to a Word document
#'
#' Build a professional `.docx` review document from an `lss` object,
#' displaying up to four languages side by side. Each question becomes a
#' compact `flextable` with a meta header (variable code, type, mandatory,
#' filter) shown once, language column headers, the question text per
#' language, and the subquestion or answer-option rows underneath -- codes
#' on the left, labels per language on the right. Headings, a metadata
#' cover page, an optional table of contents, and an optional audit summary
#' tie the document together. Rendering uses the suggested packages
#' \pkg{officer} and \pkg{flextable}; both must be installed.
#'
#' @param lss An `lss` object returned by [parse_lss()].
#' @param output Path to the `.docx` file to create.
#' @param languages Character vector of language codes to display, in order.
#'   Defaults to all languages found in the `.lss` file.
#' @param layout Reserved for future use. Currently `"side-by-side"` only.
#' @param show_audit Logical; include an audit summary section near the top
#'   and inline markers on questions that carry findings.
#' @param show_help Logical; include question help texts under the question
#'   text.
#' @param show_attrs Character vector of question attributes to display
#'   under the question text when present.
#' @param show_technical_attrs Logical; include technical attributes such as
#'   `answer_order` and `location_*`.
#' @param page_format Page format. `"auto"` picks portrait for one or two
#'   languages and landscape from three. Use `"A4-portrait"`,
#'   `"A4-landscape"`, or `"A3"` to force a layout.
#' @param logo Optional path to an image (PNG or JPEG) to display at the top
#'   of the cover page. The `.lss` file does not embed a logo, so this image
#'   must be supplied by the caller. `NULL` (default) keeps the cover
#'   logo-free, matching the neutral style of survey-methodology references
#'   (ESS, MOSAiCH, Panel).
#' @param logo_width,logo_height Image dimensions in inches. Defaults are
#'   tuned to a 2:1 logo (1.5 x 0.75 inches). Resize or pre-crop your image
#'   to fit if it has a different aspect ratio.
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
#'   package = "lssdoc"
#' ))
#' render_lss_docx(lss, tempfile(fileext = ".docx"))
#' }
#' @export
render_lss_docx <- function(
  lss,
  output,
  languages = NULL,
  layout = c("auto", "side-by-side", "stacked"),
  show_audit = TRUE,
  show_help = TRUE,
  show_attrs = c("prefix", "suffix", "other_replace_text", "validation"),
  show_technical_attrs = FALSE,
  page_format = c("auto", "A4-portrait", "A4-landscape", "A3"),
  logo = NULL,
  logo_width = 1.5,
  logo_height = 0.75
) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      "{.arg lss} must be an {.cls lss} object from {.fn parse_lss}.",
      class = "lssdoc_bad_lss"
    )
  }
  if (!is.character(output) || length(output) != 1L || is.na(output)) {
    lssdoc_abort(
      "{.arg output} must be a single file path.",
      class = "lssdoc_bad_output"
    )
  }
  layout <- rlang::arg_match(layout)
  page_format <- rlang::arg_match(page_format)
  lss_validate_logo(logo)
  for (pkg in c("officer", "flextable")) {
    if (!requireNamespace(pkg, quietly = TRUE)) {
      lssdoc_abort(
        c(
          "Rendering a {.file .docx} document requires the {.pkg {pkg}} package.",
          "i" = "Install it with {.run install.packages(\"{pkg}\")}."
        ),
        class = "lssdoc_missing_suggest"
      )
    }
  }

  model <- lss_model(lss, languages = languages)
  langs <- model$languages
  theme <- lss_render_theme()
  audit_idx <- if (isTRUE(show_audit)) {
    lss_audit_index(audit_lss(lss))
  } else {
    NULL
  }
  section <- lss_render_section_props(page_format, length(langs))

  doc <- officer::read_docx()
  doc <- lss_render_cover(
    doc, lss, model, theme,
    logo = logo, logo_width = logo_width, logo_height = logo_height
  )
  doc <- officer::body_add_break(doc)
  doc <- lss_render_toc(doc, theme)
  if (isTRUE(show_audit) && !is.null(audit_idx) && nrow(audit_idx$findings) > 0) {
    doc <- officer::body_add_break(doc)
    doc <- lss_render_audit_section(doc, audit_idx, theme)
  }
  doc <- lss_render_welcome(doc, lss, langs, theme)
  for (group in model$groups) {
    doc <- lss_render_group(
      doc, group, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx
    )
  }
  doc <- lss_render_endtext(doc, lss, langs, theme)

  doc <- officer::body_set_default_section(doc, section)
  print(doc, target = output)
  invisible(output)
}


# Theme and small helpers ------------------------------------------------

#' Centralized visual theme for the rendered document
#' @keywords internal
#' @noRd
lss_render_theme <- function() {
  list(
    color_primary = "#1F4E79",
    color_accent  = "#2E75B6",
    color_band    = "#D9E2F3",
    color_zebra   = "#F7F9FC",
    color_text    = "#222222",
    color_muted   = "#6E6E6E",
    color_white   = "#FFFFFF",
    color_warning = "#C45911",
    color_error   = "#9E1B1B",
    color_note    = "#5B5B5B",

    font_body = "Calibri",
    size_meta = 8,
    size_lang_header = 9,
    size_question = 10,
    size_subq = 9,
    size_answer = 9,
    size_help = 8,
    size_heading1 = 14,
    size_heading2 = 11,
    size_cover_title = 22,
    size_cover_subtitle = 12,
    size_cover_meta = 9,

    empty_marker = "\u2014"
  )
}

#' Validate the optional logo argument: NULL or an existing image path
#' @keywords internal
#' @noRd
lss_validate_logo <- function(logo) {
  if (is.null(logo)) {
    return(invisible())
  }
  if (!is.character(logo) || length(logo) != 1L || is.na(logo)) {
    lssdoc_abort(
      "{.arg logo} must be {.code NULL} or a single file path.",
      class = "lssdoc_bad_logo"
    )
  }
  if (!file.exists(logo)) {
    lssdoc_abort(
      "Cannot find a logo file at {.path {logo}}.",
      class = "lssdoc_logo_not_found"
    )
  }
  ext <- tolower(tools::file_ext(logo))
  if (!ext %in% c("png", "jpg", "jpeg")) {
    lssdoc_abort(
      c(
        "{.arg logo} must be a PNG or JPEG image; got {.val {ext}}.",
        "i" = "Convert your image to PNG or JPEG and retry."
      ),
      class = "lssdoc_bad_logo_format"
    )
  }
  invisible()
}

#' Human-readable label for a language code
#' @keywords internal
#' @noRd
lss_language_label <- function(code) {
  map <- c(
    fr = "Fran\u00e7ais",
    de = "Deutsch",
    en = "English",
    it = "Italiano",
    es = "Espa\u00f1ol",
    pt = "Portugu\u00eas",
    nl = "Nederlands",
    pl = "Polski",
    ru = "\u0420\u0443\u0441\u0441\u043a\u0438\u0439"
  )
  out <- unname(map[code])
  out[is.na(out)] <- code[is.na(out)]
  out
}

#' Pick page-section properties for the given format and language count
#' @keywords internal
#' @noRd
lss_render_section_props <- function(page_format, n_langs) {
  if (identical(page_format, "auto")) {
    page_format <- if (n_langs <= 2L) "A4-portrait" else "A4-landscape"
  }
  size <- switch(
    page_format,
    "A4-portrait"  = officer::page_size(width = 8.27, height = 11.69, orient = "portrait"),
    "A4-landscape" = officer::page_size(width = 11.69, height = 8.27, orient = "landscape"),
    "A3"           = officer::page_size(width = 16.53, height = 11.69, orient = "landscape")
  )
  officer::prop_section(
    page_size = size,
    page_margins = officer::page_mar(top = 0.7, bottom = 0.7, left = 0.6, right = 0.6)
  )
}

#' Index audit findings by question code for inline lookup
#' @keywords internal
#' @noRd
lss_audit_index <- function(audit) {
  fdf <- audit$findings
  fdf$question_code <- sub(
    "^(?:Question|Subquestion|Answer) '([^/= ]+).*$", "\\1",
    fdf$location, perl = TRUE
  )
  fdf$question_code[!grepl("^[A-Za-z0-9_]+$", fdf$question_code)] <- NA_character_
  by_code <- split(fdf, fdf$question_code)
  list(audit = audit, findings = fdf, by_code = by_code)
}

# Block-to-paragraph conversion -----------------------------------------

#' Convert an HTML fragment into a flextable paragraph
#'
#' Falls back to an em-dash placeholder when the fragment is empty so the
#' cell never looks accidentally blank.
#'
#' @keywords internal
#' @noRd
lss_compose <- function(html, theme,
                        size = theme$size_question,
                        color = theme$color_text,
                        italic_default = FALSE) {
  blocks <- lss_html_to_blocks(html)
  if (length(blocks) == 0) {
    return(flextable::as_paragraph(
      flextable::as_chunk(
        theme$empty_marker,
        props = officer::fp_text(
          font.family = theme$font_body, font.size = size,
          color = theme$color_muted
        )
      )
    ))
  }

  chunks <- list()
  for (bi in seq_along(blocks)) {
    if (bi > 1L) {
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
        "\n",
        props = officer::fp_text(
          font.family = theme$font_body, font.size = size, color = color
        )
      )
    }
    b <- blocks[[bi]]
    if (identical(b$type, "list_item")) {
      bullet <- if (isTRUE(b$ordered)) "1. " else "\u2022 "
      indent <- if (b$level > 1L) {
        strrep("  ", b$level - 1L)
      } else {
        ""
      }
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
        paste0(indent, bullet),
        props = officer::fp_text(
          font.family = theme$font_body, font.size = size, color = color
        )
      )
    }
    for (r in b$runs) {
      props <- officer::fp_text(
        font.family = theme$font_body,
        font.size = size,
        color = color,
        bold = isTRUE(r$bold),
        italic = isTRUE(r$italic) || isTRUE(italic_default),
        underlined = isTRUE(r$underline),
        vertical.align = if (isTRUE(r$superscript)) {
          "superscript"
        } else if (isTRUE(r$subscript)) {
          "subscript"
        } else {
          "baseline"
        }
      )
      if (isTRUE(r$linebreak)) {
        chunks[[length(chunks) + 1L]] <- flextable::as_chunk("\n", props = props)
      } else if (nzchar(r$text)) {
        chunks[[length(chunks) + 1L]] <- flextable::as_chunk(r$text, props = props)
      }
    }
  }
  do.call(flextable::as_paragraph, chunks)
}

#' One-line plain-text paragraph in the body font
#' @keywords internal
#' @noRd
lss_compose_plain <- function(text, theme, size = theme$size_meta,
                              color = theme$color_text,
                              bold = FALSE, italic = FALSE) {
  if (is.null(text) || is.na(text) || !nzchar(text)) {
    text <- theme$empty_marker
    color <- theme$color_muted
  }
  flextable::as_paragraph(flextable::as_chunk(
    text,
    props = officer::fp_text(
      font.family = theme$font_body, font.size = size, color = color,
      bold = bold, italic = italic
    )
  ))
}


# Cover, TOC, welcome, endtext ------------------------------------------

#' Cover page with the survey title in every language and a metadata table
#' @keywords internal
#' @noRd
lss_render_cover <- function(doc, lss, model, theme,
                             subtitle = "LimeSurvey questionnaire review",
                             logo = NULL,
                             logo_width = 1.5,
                             logo_height = 0.75) {
  ls_settings <- lss$survey_language_settings
  langs <- model$languages

  # Optional logo at the very top of the cover page.
  if (!is.null(logo)) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::external_img(
          src = logo, width = logo_width, height = logo_height
        ),
        fp_p = officer::fp_par(text.align = "center", padding.bottom = 6)
      )
    )
  }

  # Title in every language (largest font).
  for (lg in langs) {
    title <- if (!is.null(ls_settings) && nrow(ls_settings) > 0) {
      ls_settings$surveyls_title[ls_settings$surveyls_language == lg][1]
    } else {
      NA_character_
    }
    if (is.na(title) || !nzchar(title)) title <- "(untitled survey)"
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          title,
          prop = officer::fp_text(
            font.family = theme$font_body,
            font.size = theme$size_cover_title,
            color = theme$color_primary,
            bold = TRUE
          )
        ),
        fp_p = officer::fp_par(text.align = "center", padding.top = 6, padding.bottom = 2)
      )
    )
  }

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        subtitle,
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_cover_subtitle,
          color = theme$color_muted, italic = TRUE
        )
      ),
      fp_p = officer::fp_par(text.align = "center")
    )
  )

  # Metadata table.
  n_groups <- length(model$groups)
  n_q <- sum(vapply(model$groups, function(g) length(g$questions), integer(1)))
  n_subq <- if (is.null(lss$subquestions)) 0L else nrow(lss$subquestions)
  n_ans  <- if (is.null(lss$answers)) 0L else nrow(lss$answers)
  sid <- if (!is.null(lss$surveys) && "sid" %in% names(lss$surveys)) {
    lss$surveys$sid[1]
  } else {
    NA_character_
  }
  last_mod <- if (!is.null(lss$surveys) && "lastmodified" %in% names(lss$surveys)) {
    lss$surveys$lastmodified[1]
  } else {
    NA_character_
  }
  none <- theme$empty_marker

  meta <- data.frame(
    Field = c(
      "Source file", "Survey ID", "Languages", "Groups", "Questions",
      "Subquestions", "Answer options", "Last modified", "Generated"
    ),
    Value = c(
      basename(lss$file),
      if (is.na(sid) || !nzchar(sid)) none else sid,
      paste(langs, collapse = ", "),
      as.character(n_groups),
      as.character(n_q),
      as.character(n_subq),
      as.character(n_ans),
      if (is.na(last_mod) || !nzchar(last_mod)) none else last_mod,
      format(Sys.time(), "%Y-%m-%d %H:%M")
    ),
    stringsAsFactors = FALSE
  )
  ft <- flextable::flextable(meta)
  ft <- flextable::delete_part(ft, part = "header")
  ft <- flextable::bold(ft, j = 1, part = "body")
  ft <- flextable::color(ft, j = 1, color = theme$color_primary, part = "body")
  ft <- flextable::fontsize(ft, size = theme$size_cover_meta, part = "all")
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  ft <- flextable::width(ft, j = 1, width = 1.4, unit = "in")
  ft <- flextable::width(ft, j = 2, width = 3.2, unit = "in")
  ft <- flextable::border_remove(ft)
  ft <- flextable::hline(
    ft, border = officer::fp_border(color = theme$color_band, width = 0.5),
    part = "body"
  )
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2, part = "all")
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- flextable::body_add_flextable(doc, ft, align = "center")

  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Processed locally with lssdoc. Nothing is uploaded.",
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_meta,
          color = theme$color_muted, italic = TRUE
        )
      ),
      fp_p = officer::fp_par(text.align = "center")
    )
  )
  doc
}

#' Insert a Word table-of-contents field; updates on F9 in Word
#' @keywords internal
#' @noRd
lss_render_toc <- function(doc, theme) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Table of contents",
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_heading1,
          bold = TRUE, color = theme$color_primary
        )
      )
    )
  )
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(
        "Press F9 in Word to refresh page numbers after opening.",
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_meta,
          color = theme$color_muted, italic = TRUE
        )
      )
    )
  )
  doc <- officer::body_add_toc(doc, level = 2)
  doc
}

#' Side-by-side localized welcome text (omitted if all languages are empty)
#' @keywords internal
#' @noRd
lss_render_welcome <- function(doc, lss, langs, theme) {
  lss_render_localized_block(
    doc, lss, langs, theme,
    field = "surveyls_welcometext", title = "Welcome text"
  )
}

#' Side-by-side localized end text
#' @keywords internal
#' @noRd
lss_render_endtext <- function(doc, lss, langs, theme) {
  lss_render_localized_block(
    doc, lss, langs, theme,
    field = "surveyls_endtext", title = "End text"
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


# Group and question rendering ------------------------------------------

#' Render one group: heading, optional description, then each question
#' @keywords internal
#' @noRd
lss_render_group <- function(doc, group, langs, theme,
                             show_help, show_attrs, show_technical_attrs,
                             audit_idx) {
  gname <- lss_first_label(group$names, langs)
  if (is.na(gname)) gname <- paste0("Group ", group$gid)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      gname,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    )),
    style = "heading 1"
  )
  any_desc <- any(vapply(
    group$descriptions, function(v) !is.null(v) && !is.na(v) && nzchar(trimws(v)),
    logical(1)
  ))
  if (any_desc) {
    df <- as.data.frame(matrix("", nrow = 1, ncol = length(langs)), stringsAsFactors = FALSE)
    names(df) <- langs
    ft <- flextable::flextable(df)
    ft <- flextable::set_header_labels(
      ft, values = stats::setNames(lss_language_label(langs), langs)
    )
    for (lg in langs) {
      ft <- flextable::compose(
        ft, i = 1L, j = lg,
        value = lss_compose(group$descriptions[[lg]], theme, size = theme$size_subq, italic_default = TRUE)
      )
    }
    ft <- lss_table_polish(ft, theme, lang_cols = langs)
    doc <- flextable::body_add_flextable(doc, ft, align = "left")
  }

  for (q in group$questions) {
    doc <- lss_render_question(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx
    )
  }
  doc
}

#' Render one question as a self-contained flextable
#' @keywords internal
#' @noRd
lss_render_question <- function(doc, q, langs, theme,
                                show_help, show_attrs,
                                show_technical_attrs, audit_idx) {
  # Heading 2 with the question code as an anchor for the TOC.
  audit_marker <- lss_audit_marker(q$code, audit_idx, theme)
  heading_text <- if (is.null(audit_marker)) {
    q$code
  } else {
    paste0(q$code, "  ", audit_marker$text)
  }
  heading_prop <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_heading2,
    bold = TRUE,
    color = if (is.null(audit_marker)) theme$color_text else audit_marker$color
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(heading_text, prop = heading_prop)),
    style = "heading 2"
  )

  rows <- lss_question_rows(q, langs, theme,
                            show_help = show_help,
                            show_attrs = show_attrs,
                            show_technical_attrs = show_technical_attrs)
  df <- rows$df
  ft <- flextable::flextable(df)

  meta_line <- lss_question_meta(q, theme)
  ft <- flextable::add_header_lines(ft, values = meta_line)
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      list(code = "Code"),
      stats::setNames(as.list(lss_language_label(langs)), langs)
    )
  )

  # Compose rich cells.
  for (i in seq_len(nrow(df))) {
    rspec <- rows$specs[[i]]
    if (!is.null(rspec$code)) {
      ft <- flextable::compose(
        ft, i = i, j = "code",
        value = lss_compose_plain(
          rspec$code, theme, size = theme$size_meta,
          bold = TRUE, color = theme$color_primary
        )
      )
    }
    for (lg in langs) {
      ft <- flextable::compose(
        ft, i = i, j = lg,
        value = lss_compose(
          rspec$texts[[lg]], theme,
          size = rspec$size, color = rspec$color,
          italic_default = isTRUE(rspec$italic)
        )
      )
    }
  }

  ft <- lss_table_polish(ft, theme, lang_cols = langs, meta_header = TRUE)
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc
}

#' Build the data frame and per-row rendering spec for a question table
#' @keywords internal
#' @noRd
lss_question_rows <- function(q, langs, theme,
                              show_help, show_attrs,
                              show_technical_attrs) {
  rows <- list()

  # Row 1: question text (and optional help under it via paragraph break).
  q_texts <- stats::setNames(
    lapply(langs, function(lg) q$texts[[lg]]$question),
    langs
  )
  help_texts <- stats::setNames(
    lapply(langs, function(lg) q$texts[[lg]]$help),
    langs
  )
  if (isTRUE(show_help) && any(vapply(
    help_texts, function(h) !is.null(h) && !is.na(h) && nzchar(trimws(h)),
    logical(1)
  ))) {
    q_texts <- stats::setNames(lapply(langs, function(lg) {
      qh <- trimws(if (is.null(help_texts[[lg]])) "" else as.character(help_texts[[lg]]))
      qt <- if (is.null(q_texts[[lg]])) NA_character_ else q_texts[[lg]]
      if (nzchar(qh)) {
        paste0(
          if (is.na(qt)) "" else qt,
          "<br><i>", qh, "</i>"
        )
      } else {
        qt
      }
    }), langs)
  }
  rows[[length(rows) + 1L]] <- list(
    code = "Q",
    texts = q_texts,
    size = theme$size_question,
    color = theme$color_text,
    italic = FALSE
  )

  # Subquestions.
  if (length(q$subquestions) > 0) {
    for (s in q$subquestions) {
      stxt <- stats::setNames(lapply(langs, function(lg) s$texts[[lg]]$question), langs)
      rows[[length(rows) + 1L]] <- list(
        code = s$code,
        texts = stxt,
        size = theme$size_subq,
        color = theme$color_text,
        italic = FALSE
      )
    }
  }

  # Answer options (grouped by scale_id when dual scale).
  if (length(q$answers) > 0) {
    if (!is.null(q$scales) && length(q$scales) > 1L) {
      scale_ids <- names(q$scales)
      for (si in scale_ids) {
        rows[[length(rows) + 1L]] <- list(
          code = paste0("Scale ", as.integer(si) + 1L),
          texts = stats::setNames(
            lapply(langs, function(lg) NA_character_), langs
          ),
          size = theme$size_meta,
          color = theme$color_muted,
          italic = TRUE,
          scale_header = TRUE
        )
        for (a in q$scales[[si]]) {
          atxt <- stats::setNames(lapply(langs, function(lg) a$labels[[lg]]), langs)
          rows[[length(rows) + 1L]] <- list(
            code = a$code, texts = atxt,
            size = theme$size_answer, color = theme$color_text, italic = FALSE
          )
        }
      }
    } else {
      for (a in q$answers) {
        atxt <- stats::setNames(lapply(langs, function(lg) a$labels[[lg]]), langs)
        rows[[length(rows) + 1L]] <- list(
          code = a$code, texts = atxt,
          size = theme$size_answer, color = theme$color_text, italic = FALSE
        )
      }
    }
  }

  # Optional question attributes (prefix/suffix/other_replace_text/validation).
  if (length(show_attrs) > 0 && !is.null(q$attributes)) {
    attrs <- q$attributes
    keep <- attrs$attribute %in% show_attrs
    if (any(keep)) {
      attrs <- attrs[keep, , drop = FALSE]
      for (i in seq_len(nrow(attrs))) {
        atxt <- stats::setNames(lapply(langs, function(lg) {
          if (!nzchar(attrs$language[i]) || identical(attrs$language[i], lg)) {
            attrs$value[i]
          } else {
            NA_character_
          }
        }), langs)
        rows[[length(rows) + 1L]] <- list(
          code = attrs$attribute[i], texts = atxt,
          size = theme$size_meta, color = theme$color_muted, italic = TRUE
        )
      }
    }
  }

  df <- as.data.frame(
    matrix("", nrow = length(rows), ncol = 1L + length(langs)),
    stringsAsFactors = FALSE
  )
  names(df) <- c("code", langs)
  list(df = df, specs = rows)
}

#' Build the meta header line text for a question
#' @keywords internal
#' @noRd
lss_question_meta <- function(q, theme) {
  parts <- c(
    sprintf("Q %s", q$code),
    sprintf("QID %s", q$qid),
    sprintf("Type: %s", q$type_label),
    sprintf("Mandatory: %s", lss_yes_no(q$mandatory)),
    sprintf("Filter: %s", lss_relevance_label(q$relevance))
  )
  paste(parts, collapse = "  \u00b7  ")
}

#' Apply consistent visual polish to a flextable
#' @keywords internal
#' @noRd
lss_table_polish <- function(ft, theme, lang_cols, meta_header = FALSE,
                             has_code = meta_header) {
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  ft <- flextable::fontsize(ft, size = theme$size_answer, part = "body")
  ft <- flextable::fontsize(ft, size = theme$size_lang_header, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_text, part = "body")
  ft <- flextable::color(ft, color = theme$color_primary, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band, part = "header")
  if (isTRUE(meta_header)) {
    # The meta line is the first header row when add_header_lines was used.
    ft <- flextable::bg(ft, i = 1L, bg = theme$color_primary, part = "header")
    ft <- flextable::color(ft, i = 1L, color = theme$color_white, part = "header")
    ft <- flextable::fontsize(ft, i = 1L, size = theme$size_meta, part = "header")
    ft <- flextable::align(ft, i = 1L, align = "left", part = "header")
  }
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  ft <- flextable::hline_top(ft, border = officer::fp_border(color = theme$color_primary, width = 1.2), part = "header")
  ft <- flextable::hline_bottom(ft, border = officer::fp_border(color = theme$color_primary, width = 1.2), part = "body")
  if (isTRUE(has_code)) {
    ft <- flextable::align(ft, j = "code", align = "center", part = "body")
    ft <- flextable::width(ft, j = "code", width = 0.6, unit = "in")
  }
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2,
                           padding.left = 4, padding.right = 4, part = "all")
  # Equal-width language columns (best-effort; flextable will auto-fit in Word).
  for (lg in lang_cols) {
    ft <- flextable::width(ft, j = lg, width = 2.5, unit = "in")
  }
  ft
}

#' Audit marker for inline display next to a question heading
#' @keywords internal
#' @noRd
lss_audit_marker <- function(qcode, audit_idx, theme) {
  if (is.null(audit_idx) || is.null(qcode) || is.na(qcode)) return(NULL)
  rows <- audit_idx$by_code[[qcode]]
  if (is.null(rows) || nrow(rows) == 0) return(NULL)
  n <- nrow(rows)
  worst <- rows$severity[order(match(rows$severity, c("error", "warning", "note")))][1]
  list(
    text = sprintf(
      "(%d audit finding%s, worst: %s)",
      n, if (n == 1L) "" else "s", worst
    ),
    color = switch(
      worst,
      error = theme$color_error,
      warning = theme$color_warning,
      theme$color_note
    )
  )
}

#' Render the audit summary section
#' @keywords internal
#' @noRd
lss_render_audit_section <- function(doc, audit_idx, theme) {
  audit <- audit_idx$audit
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      "Audit findings",
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    )),
    style = "heading 1"
  )
  summary_line <- sprintf(
    "%d finding(s): %d error(s), %d warning(s), %d note(s).",
    audit$n_findings, audit$n_errors, audit$n_warnings, audit$n_notes
  )
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      summary_line,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_question,
        color = theme$color_text
      )
    ))
  )
  fdf <- audit$findings[, c("severity", "check", "location", "language", "message"), drop = FALSE]
  ft <- flextable::flextable(fdf)
  ft <- flextable::set_header_labels(
    ft,
    severity = "Severity", check = "Check",
    location = "Location", language = "Lang", message = "Message"
  )
  sev_colors <- c(error = theme$color_error,
                  warning = theme$color_warning,
                  note = theme$color_note)
  for (i in seq_len(nrow(fdf))) {
    ft <- flextable::color(
      ft, i = i, j = "severity",
      color = sev_colors[[fdf$severity[i]]], part = "body"
    )
    ft <- flextable::bold(ft, i = i, j = "severity", part = "body")
  }
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  ft <- flextable::fontsize(ft, size = theme$size_answer, part = "all")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_primary, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band, part = "header")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  ft <- flextable::width(ft, j = "severity", width = 0.8, unit = "in")
  ft <- flextable::width(ft, j = "check", width = 1.5, unit = "in")
  ft <- flextable::width(ft, j = "location", width = 2.0, unit = "in")
  ft <- flextable::width(ft, j = "language", width = 0.5, unit = "in")
  ft <- flextable::width(ft, j = "message", width = 3.5, unit = "in")
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc
}


# Small text helpers -----------------------------------------------------

#' Translate a LimeSurvey Y/N/blank value into a display label
#' @keywords internal
#' @noRd
lss_yes_no <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("\u2014")
  switch(toupper(x), Y = "yes", N = "no", x)
}

#' Display label for a relevance expression
#' @keywords internal
#' @noRd
lss_relevance_label <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("\u2014")
  if (identical(x, "1")) return("always shown")
  x
}

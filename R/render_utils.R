# Small render utilities: rendering state, group bookmark, language label, title truncation, rich-text composers, yes/no localization, group-prefix stripper.
#
# Extracted from R/render_lss_docx.R.




#' Mutable state passed through the render functions
#'
#' Holds the question numbers, computed once from the model by
#' `lss_item_numbers()` (no renderer counts items itself), the running
#' group index the group bookmarks and the table of contents rely on, and
#' the variable-index entries collected while rendering.
#'
#' @keywords internal
#' @noRd
lss_render_state <- function(model) {
  state <- new.env(parent = emptyenv())
  state$group_index <- 0L
  state$model <- model
  state$numbers <- lss_item_numbers(model)
  state$index_entries <- list()
  state
}

#' Build a response-variable name in the requested style
#'
#' Assembles the data-column name LimeSurvey gives a response variable
#' from its parts, in one of two styles:
#' * `"brackets"` (default) -- the exact form of the **CSV / Excel data
#'   export** header, so the rendered questionnaire and its variable index
#'   match the raw data file column for column: `parent[part]`,
#'   `parent[part][scale]` (e.g. `sleephours[WEEK]`,
#'   `trustinstitutions[PARL][1]`, `devicerank[59842]`).
#' * `"underscore"` -- the sanitized code form used by the Expression
#'   Manager / relevance equations and the SPSS / Stata / R exports:
#'   `parent_part`, `parent_part_scale`.
#'
#' `part` is the subquestion code, the multiple-choice answer code, the
#' literal `"other"` / `"_Cother"` / `"_Ccomment"` appendix marker, the
#' ranking slot's answer id, or `"*"` for the family placeholder shown in
#' a multiple-choice meta band. `scale` is the 1-based dual-scale index.
#'
#' @keywords internal
#' @noRd
lss_variable_name <- function(parent, part = NULL, scale = NULL,
                              style = "brackets") {
  brackets <- !identical(style, "underscore")
  wrap <- function(x) if (brackets) paste0("[", x, "]") else paste0("_", x)
  # LimeSurvey exports a purely-numeric subquestion code without its
  # leading zeros (`001` -> `1`), so normalize digit-only parts to match
  # the data file column.
  if (!is.null(part) && grepl("^[0-9]+$", part)) {
    part <- as.character(as.integer(part))
  }
  out <- parent
  if (!is.null(part)) out <- paste0(out, wrap(part))
  if (!is.null(scale)) out <- paste0(out, wrap(scale))
  out
}

#' Data-column name of a question's free-text "Other" response
#'
#' LimeSurvey names the "Other" appendix differently by family in the
#' CSV/Excel export: a multiple-choice "Other" is `code[other]`, while a
#' single-choice list "Other" is `code[_Cother]`. The `"underscore"`
#' style uses the clean code form `code_other` for both.
#'
#' @keywords internal
#' @noRd
lss_other_variable <- function(q, style = "brackets") {
  if (identical(style, "underscore")) {
    return(lss_variable_name(q$code, "other", style = style))
  }
  multiple <- identical(lss_type_info(q$type)$family, "multiple")
  lss_variable_name(q$code, if (multiple) "other" else "_Cother",
                    style = style)
}

#' Bookmark name for a group, used to wire TOC entries to group headings
#' @keywords internal
#' @noRd
lss_group_bookmark <- function(index) {
  sprintf("lssdoc_group_%d", as.integer(index))
}

#' Bookmark name for a top-level section (audit, consent, questionnaire,
#' quotas, index), used to wire the static TOC entries to the headings.
#' @keywords internal
#' @noRd
lss_section_bookmark <- function(name) {
  paste0("lssdoc_section_", name)
}

#' Render a top-level section heading (audit, consent, questionnaire,
#' quotas, index)
#'
#' A self-contained styled paragraph -- the package's own look (body
#' font, heading size, bold, primary colour, a thin primary underline)
#' rather than Word's "heading 1" style, so the rendering is identical
#' across Word, LibreOffice and the headless PDF path and does not
#' depend on the reference template's heading definition. The same look
#' as the group headings keeps the document typographically uniform.
#' `keep_with_next` stops the title from being stranded at the foot of a
#' page, and the bookmark anchors the matching static-TOC entry. Outline
#' level 1 lists the heading in Word's navigation pane, above the level-2
#' group headings.
#'
#' @keywords internal
#' @noRd
lss_render_section_heading <- function(doc, theme, text, bookmark) {
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(text, prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )),
      fp_p = officer::fp_par(
        padding.top = 18, padding.bottom = 6,
        border.bottom = officer::fp_border(color = theme$color_primary, width = 1),
        keep_with_next = TRUE
      )
    )
  )
  doc <- lss_set_outline_level(doc, 1L)
  officer::body_bookmark(doc, bookmark)
}

#' Give the paragraph at the cursor a Word outline level
#'
#' Word builds its navigation pane from the outline level of each
#' paragraph, whatever its style, and adds no list number for it, so a
#' styled heading keeps its look and still shows in the pane.
#' `officer::fp_par()` has no argument for the level: the `w:outlineLvl`
#' element is written into the `w:pPr` of the block at the cursor, i.e.
#' the paragraph just added. `level` is Word's level (1 to 9); the XML
#' value is 0-based. The CT_PPr sequence ends with `w:divId`,
#' `w:cnfStyle`, `w:rPr`, `w:sectPr` and `w:pPrChange`, so the element
#' goes before the first of them when one is present. An existing level
#' is replaced. A block that is not a paragraph (a table) is left as it
#' is: Word ignores outline levels inside tables.
#'
#' @keywords internal
#' @noRd
lss_set_outline_level <- function(doc, level) {
  w <- "http://schemas.openxmlformats.org/wordprocessingml/2006/main"
  ns <- c(w = w)
  p <- officer::docx_current_block_xml(doc)
  if (!identical(xml2::xml_name(p), "p")) return(doc)
  ppr <- xml2::xml_find_first(p, "w:pPr", ns)
  if (inherits(ppr, "xml_missing")) {
    ppr <- xml2::xml_add_child(
      p, xml2::as_xml_document(sprintf("<w:pPr xmlns:w=\"%s\"/>", w)),
      .where = 0L
    )
  }
  xml2::xml_remove(xml2::xml_find_all(ppr, "w:outlineLvl", ns))
  lvl <- xml2::as_xml_document(sprintf(
    "<w:outlineLvl xmlns:w=\"%s\" w:val=\"%d\"/>", w, as.integer(level) - 1L
  ))
  closer <- xml2::xml_find_first(
    ppr,
    paste0("*[self::w:divId or self::w:cnfStyle or self::w:rPr or ",
           "self::w:sectPr or self::w:pPrChange]"),
    ns
  )
  if (inherits(closer, "xml_missing")) {
    xml2::xml_add_child(ppr, lvl)
  } else {
    xml2::xml_add_sibling(closer, lvl, .where = "before")
  }
  doc
}

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




#' Truncate a title with an ASCII ellipsis if it exceeds the budget
#' @keywords internal
#' @noRd
lss_truncate_title <- function(text, max_chars = 80L) {
  if (is.null(text) || is.na(text) || !nzchar(text)) return("")
  text <- trimws(text)
  if (nchar(text) <= max_chars) return(text)
  paste0(substr(text, 1L, max_chars - 3L), "...")
}

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

#' Helper: TRUE if at least one element of a named list of strings is
#' non-empty after trimming.
#' @keywords internal
#' @noRd
lss_any_present <- function(values) {
  any(vapply(
    values,
    function(v) !is.null(v) && !is.na(v) && nzchar(trimws(as.character(v))),
    logical(1)
  ))
}

#' Translate a LimeSurvey Y/N/blank value into a display label
#'
#' When `theme` is supplied, the localized strings from
#' `theme$chrome$mandatory_yes` / `mandatory_no` / `mandatory_soft` are
#' used so the Mandatory cell reads in the chrome language. Without
#' `theme`, returns English (the legacy default used by audit-text
#' generation where the chrome is not threaded through).
#' @keywords internal
#' @noRd
lss_yes_no <- function(x, theme = NULL) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("\u2014")
  yes <- if (!is.null(theme)) theme$chrome$mandatory_yes else "Yes"
  no  <- if (!is.null(theme)) theme$chrome$mandatory_no  else "No"
  # "Soft" mandatory is a LimeSurvey UI affordance (warning but
  # submission allowed); semantically the variable IS optional in the
  # data (missing values possible). For a variable-centric review
  # document we collapse `S` -> No so the cell answers the question
  # "is the response guaranteed?". The original `S` value remains in
  # the .lss source for any reviewer who needs the UI distinction.
  switch(toupper(x), Y = yes, N = no, S = no, x)
}

#' Strip a leading author-written numeric prefix from a group name
#'
#' LimeSurvey authors often prefix their group names with their own
#' numbering ("1. Vos etudes", "Section A - Demographics"). Word adds
#' its own Heading 1 list number on top, leading to "1. 1. Vos etudes".
#' This helper removes the most common explicit-numbering prefixes so
#' Word's auto-number is the only visible one. Patterns recognized:
#' `N.`, `N)`, `N -`, `N:`, `N.M.`, and `Section X -` (where X is a
#' letter or roman numeral). Conservative -- leaves any other prefix
#' untouched.
#'
#' @keywords internal
#' @noRd
lss_strip_group_number_prefix <- function(name) {
  if (is.null(name) || is.na(name) || !nzchar(trimws(name))) {
    return(name)
  }
  s <- name
  patterns <- c(
    "^\\d+\\.\\d+\\.\\s+",
    "^\\d+\\.\\s+",
    "^\\d+\\)\\s+",
    "^\\d+\\s*[-\u2013\u2014]\\s+",
    "^\\d+:\\s+",
    "^Section\\s+[A-Z]+\\s*[-\u2013\u2014]\\s+"
  )
  for (p in patterns) {
    new_s <- sub(p, "", s, perl = TRUE)
    if (!identical(new_s, s)) {
      return(trimws(new_s))
    }
  }
  s
}

#' The formula of a computed question, as plain text
#'
#' The question attribute `equation` when it is not empty, else the
#' question text in `language` (HTML stripped as the questionnaire does),
#' trimmed. When the whole string is one `{...}` expression its outer
#' braces are removed; nothing else is rewritten. Shared by
#' render_questionnaire() and render_documentation().
#'
#' @param q A model question.
#' @param language The language of the text to fall back on; `NULL` reads
#'   the attribute only.
#' @return A string, `""` when there is no formula.
#' @keywords internal
#' @noRd
lss_equation_formula <- function(q, language = NULL) {
  attrs <- q$attributes
  s <- ""
  if (is.data.frame(attrs) && all(c("attribute", "value") %in% names(attrs))) {
    v <- attrs$value[attrs$attribute %in% "equation"]
    v <- v[!is.na(v) & nzchar(trimws(v))]
    if (length(v)) s <- trimws(v[1])
  }
  if (!nzchar(s) && !is.null(language)) {
    s <- lss_q_clean(q$texts[[language]]$question)
  }
  lss_strip_outer_braces(s)
}

#' Remove the braces around a string that is one `{...}` expression
#'
#' `"{a + b}"` gives `"a + b"`; `"{a} + {b}"` and `"Total: {a} points"`
#' are returned as they are.
#'
#' @keywords internal
#' @noRd
lss_strip_outer_braces <- function(s) {
  n <- nchar(s)
  if (n < 2L || !startsWith(s, "{") || !endsWith(s, "}")) {
    return(s)
  }
  ch <- strsplit(s, "", fixed = TRUE)[[1]]
  depth <- cumsum((ch == "{") - (ch == "}"))
  if (any(depth[-n] <= 0L) || depth[n] != 0L) {
    return(s)
  }
  trimws(substr(s, 2L, n - 1L))
}


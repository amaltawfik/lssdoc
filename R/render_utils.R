# Small render utilities: rendering state, group bookmark, language label, title truncation, rich-text composers, yes/no localization, group-prefix stripper.
#
# Extracted from R/render_lss_docx.R.




#' Mutable state passed through the render functions
#'
#' Holds the running item counter so the "No" column of each item's meta
#' table matches Word's Heading 1 auto-numbering of the same paragraph.
#'
#' @keywords internal
#' @noRd
lss_render_state <- function(model) {
  state <- new.env(parent = emptyenv())
  state$item_no <- 0L
  state$group_index <- 0L
  state$model <- model
  state$index_entries <- list()
  state
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


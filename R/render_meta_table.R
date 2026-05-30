# Meta-table rendering for cards items.
#
# Extracted from R/render_lss_docx.R.

#' Build the meta header line text for a question
#' @keywords internal
#' @noRd
lss_question_meta <- function(q, theme) {
  parts <- c(
    sprintf("Q %s", q$code),
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
  # Language column headers (`Francais`, `Deutsch`, `English`) sit
  # centered above the translation paragraph below: in a wide column
  # they read as a section title rather than a left-anchored label.
  # Same convention as ESS / MOSAiCH / Eurobarometer multilingual
  # codebooks. The optional `code` column header (short atom) stays
  # centered too (handled below); the meta header line, when present,
  # explicitly resets to left.
  ft <- flextable::align(ft, align = "center", part = "header")
  # The `Label` column (when present) holds row-type labels:
  # "Language", "Question", "Help", "Value 1", "Value 2", ... It
  # reads as a small left-margin index of row types, so both header
  # and body sit left.
  if ("Label" %in% colnames(ft$body$dataset)) {
    ft <- flextable::align(ft, j = "Label", align = "left", part = "all")
  }
  if (isTRUE(meta_header)) {
    # The meta line is the first header row when add_header_lines was used.
    ft <- flextable::bg(ft, i = 1L, bg = theme$color_primary, part = "header")
    ft <- flextable::color(ft, i = 1L, color = theme$color_white, part = "header")
    ft <- flextable::fontsize(ft, i = 1L, size = theme$size_meta, part = "header")
    ft <- flextable::align(ft, i = 1L, align = "left", part = "header")
  }
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  # Outer left/right borders close the table as a rectangle (otherwise
  # flextable draws only the inner vlines and the table looks open on the
  # sides, especially next to the cream meta-table body).
  ft <- flextable::vline_left(ft, border = thin, part = "all")
  ft <- flextable::vline_right(ft, border = thin, part = "all")
  # Editorial line hierarchy: keep ALL item borders at 0.5 pt soft gray
  # (theme$color_grid). The dark accent is reserved for group banners
  # only -- per-item primary outlines would multiply on dense pages and
  # produce visual noise. Pew/ESS/OECD questionnaires use the same
  # restraint: items differentiate by the cream meta body and the
  # inter-item spacer, not by heavy framing.
  if (isTRUE(has_code)) {
    # `code` column holds short atom values (answer codes like `1`, `Y`,
    # `A1`). Cell-symmetric: header and body both centered.
    ft <- flextable::align(ft, j = "code", align = "center", part = "all")
    ft <- flextable::width(ft, j = "code", width = 0.6, unit = "in")
  }
  # Body cells are top-aligned so multiline answer labels read from the
  # top down (question text, then subquestion / answer rows). The header
  # labels (language codes on the tinted band) get an explicit center
  # so they sit visually in the band instead of clinging to the top
  # edge -- noticeable when the body grows tall (long question text,
  # several answer modalities).
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "header")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2,
                           padding.left = 4, padding.right = 4, part = "all")
  # Distribute language columns so the total table width matches the
  # printable body width (theme$content_width_in). When a `code` column is
  # present (shared scale, leaf item answers) reserve 0.6 in for it first.
  code_reserve <- if (isTRUE(has_code)) 0.6 else 0
  lang_total <- theme$content_width_in - code_reserve
  lang_w <- lang_total / max(length(lang_cols), 1L)
  for (lg in lang_cols) {
    ft <- flextable::width(ft, j = lg, width = lang_w, unit = "in")
  }
  ft
}

#' Render the 5-column structured meta table for an item
#'
#' Columns: `No` (item number; empty for compound-parent banners since
#' those are not numbered items themselves), `Variable` (the LimeSurvey
#' variable code, `parent_subq` for subquestion items), `Type` (legacy
#' code + label), `Mand.` (mandatory: `yes`, `no`, `soft`), `Filter`.
#' The Filter cell shows the plain English form on top with the raw
#' LimeSurvey expression beneath (small italic gray) when `show_raw_filter`
#' is `TRUE`.
#'
#' Takes the fields explicitly rather than a `q` object so subquestion
#' items can pass their composite variable code with the parent's type /
#' mandatory / relevance (those are inherited from the parent in
#' LimeSurvey -- subquestions do not carry their own type or relevance).
#'
#' @keywords internal
#' @noRd
lss_render_question_meta_table <- function(doc, theme,
                                           item_no = NA_integer_,
                                           variable,
                                           type,
                                           type_label,
                                           mandatory,
                                           relevance,
                                           show_raw_filter = FALSE) {
  filter_raw <- if (is.null(relevance) || is.na(relevance) ||
                    !nzchar(relevance)) {
    "1"
  } else {
    # Strip the layers of decorative outer parentheses that LimeSurvey
    # adds around every relevance expression. The inner parens that
    # actually group conditions are preserved.
    lss_strip_outer_parens(relevance)
  }
  filter_plain <- lss_humanize_relevance(filter_raw, theme)
  # The legacy type code (L, M, F, ...) is implicit in the variable's
  # data and meaningful only to LimeSurvey insiders. The descriptive
  # label is what reviewers read; drop the code prefix to avoid the
  # redundant "L - List (radio)" form.
  type_full <- type_label
  no_value <- if (is.null(item_no) || is.na(item_no)) "" else as.character(item_no)

  df <- data.frame(
    No = no_value,
    Variable = variable,
    Type = type_full,
    Mandatory = lss_yes_no(mandatory, theme),
    Filter = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft,
    No        = theme$chrome$meta_no,
    Variable  = theme$chrome$meta_variable,
    Type      = theme$chrome$meta_type,
    Mandatory = theme$chrome$meta_mandatory,
    Filter    = theme$chrome$meta_filter
  )
  plain_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_meta,
    color = theme$color_text
  )
  # Raw expression rendered in the monospace face so operators and dots
  # like `!is_empty(X.NAOK) && (X.NAOK == 1)` stay readable.
  raw_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_meta - 1L,
    color = theme$color_muted, italic = TRUE
  )
  filter_chunks <- list(flextable::as_chunk(filter_plain, props = plain_props))
  show_raw <- isTRUE(show_raw_filter) && !identical(filter_plain, filter_raw)
  if (show_raw) {
    filter_chunks <- c(
      filter_chunks,
      list(flextable::as_chunk("\n", props = plain_props)),
      list(flextable::as_chunk(filter_raw, props = raw_props))
    )
  }
  ft <- flextable::compose(
    ft, i = 1L, j = "Filter",
    value = do.call(flextable::as_paragraph, filter_chunks)
  )

  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  ft <- flextable::fontsize(ft, size = theme$size_meta, part = "all")
  # No and Variable get a larger font so the start of a new question
  # stands out at scroll time, and Variable is bold so the variable
  # code reads as the question's anchor.
  ft <- flextable::fontsize(
    ft, j = c("No", "Variable"),
    size = theme$size_heading2, part = "body"
  )
  ft <- flextable::bold(ft, j = "Variable", part = "body")
  # Variable code is an identifier (e.g. `satisfaction_4`); monospace
  # disambiguates l/1/I, 0/O and keeps the underscore visible.
  ft <- flextable::font(ft, j = "Variable", fontname = theme$font_code, part = "body")
  # Dark petrol header (#1F4E5F) with white text gives every item a
  # clear "new variable" banner without competing with the group banner
  # above (which uses the deeper #133B52).
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_white, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band_dark, part = "header")
  # Light cream tint across the body row marks the start of a new item
  # without a redundant heading line. Resilient to page breaks because
  # the tint is on a single body row.
  ft <- flextable::bg(ft, i = 1L, bg = theme$color_zebra, part = "body")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  # Outer left/right borders so the item table reads as a closed rectangle
  # (without them, Word draws only the internal vlines and the table looks
  # open on the sides).
  ft <- flextable::vline_left(ft, border = thin, part = "all")
  ft <- flextable::vline_right(ft, border = thin, part = "all")
  # Vertical alignment: the meta table has a single body row with short
  # cells (No, Variable, Type, Mand.) plus a Filter cell that may grow
  # to two lines when the raw expression is shown beneath the plain
  # form. With `valign = "top"` everywhere, the short cells appear
  # to float at the top of the tall row and the header labels drift up
  # in the dark band. Centering both header and body keeps the short
  # cells vertically balanced against the Filter cell and gives the
  # dark-band labels their visual seat.
  ft <- flextable::valign(ft, valign = "center", part = "all")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  # Cell-symmetric alignment (Quarto gt / knitr / pandoc convention):
  # the header label takes the same alignment as the column body, so
  # each column reads as a coherent unit and the dark-band label sits
  # over its content.
  # - "No": right (digits stack as a column when scanning across cards)
  # - "Variable": left (monospace identifier; left preserves the prefix
  #   scan, e.g. q1 / q2 / semestre_1)
  # - "Type": center (short categorical token)
  # - "Mandatory": center (Yes / No)
  # - "Filter": left (expression reads left-to-right; operators stay
  #   anchored at the cell's left edge)
  ft <- flextable::align(ft, align = "right",  j = "No",        part = "all")
  ft <- flextable::align(ft, align = "left",   j = "Variable",  part = "all")
  ft <- flextable::align(ft, align = "center", j = "Type",      part = "all")
  ft <- flextable::align(ft, align = "center", j = "Mandatory", part = "all")
  ft <- flextable::align(ft, align = "left",   j = "Filter",    part = "all")
  # Column widths sum to theme$content_width_in (6.30 in). Calibrated to
  # the actual content using 11 pt Consolas (~0.092 in/char) for Variable
  # and 8 pt Calibri (~0.055 in/char) for the others:
  #   No        0.35  - holds up to 3 digits in 11 pt body font (max #999).
  #   Variable  2.05  - holds identifiers up to ~22 chars (e.g.
  #                     `semestrechargetravail_1`) without wrapping;
  #                     codes >=23 chars still wrap.
  #   Type      1.00  - holds 8 pt labels up to ~18 chars; common labels
  #                     ("Single choice", "Multiple choice", "Number")
  #                     fit on one line. Long localized variants
  #                     ("Choix multiple avec commentaire", 31 chars)
  #                     wrap to two lines, which is acceptable for a
  #                     rare type.
  #   Mandatory 0.70  - widest localized header is Italian
  #                     "Obbligatoria" (12 chars at 8 pt bold ~0.66 in);
  #                     0.70 fits with a thin margin.
  #   Filter    2.20  - holds the human-readable form on top and the raw
  #                     LimeSurvey expression in 7 pt italic mono below;
  #                     widened so 2-3 chained conditions fit on a
  #                     single line.
  ft <- flextable::width(ft, j = "No", width = 0.35, unit = "in")
  ft <- flextable::width(ft, j = "Variable", width = 2.05, unit = "in")
  ft <- flextable::width(ft, j = "Type", width = 1.00, unit = "in")
  ft <- flextable::width(ft, j = "Mandatory", width = 0.70, unit = "in")
  ft <- flextable::width(ft, j = "Filter", width = 2.20, unit = "in")
  # keepnext: the meta table is a header for the item table that
  # follows; Word must keep them on the same page so the dark band
  # never floats orphaned at the bottom.
  flextable::body_add_flextable(doc, ft, align = "left", keepnext = TRUE)
}


# Item rendering for the cards template: groups, leaf items, compound items, parent stems, subquestions, shared scales, answer rows, attribute rows, item-table assembly.
#
# Extracted from R/render_lss_docx.R.

#' Render one group: a banner, optional description, then each item
#'
#' Groups become a visible section banner (bold colored paragraph, not a
#' Heading style) followed by their items. Items themselves use Heading 1
#' so Word numbers them sequentially across the whole document, which keeps
#' the navigation index flat and ignores the group hierarchy in numbering.
#'
#' @keywords internal
#' @noRd
lss_render_group <- function(doc, group, langs, theme,
                             show_help, show_attrs, show_technical_attrs,
                             audit_idx, state,
                             show_groups = TRUE) {
  # The group index must still advance even when the banner itself is
  # hidden -- bookmarks, audit references and the TOC depend on it.
  state$group_index <- state$group_index + 1L
  # Render as a styled paragraph (no Heading 1 style) so Word does NOT
  # add its own list number on top of ours -- the auto-number Word
  # injects via the linked numbering definition uses a different font
  # face/size than our heading text, which looks inconsistent. Doing
  # the numbering manually keeps the whole heading typographically
  # uniform.
  #
  # The asymmetric padding (24 pt above, 8 pt below) signals "section
  # break" at the right strength: the air above is roughly three times
  # the air below, so the eye reads the title as bound to the questions
  # that follow it. The under-line at 1 pt gives a clean banner finish
  # without enclosing the title in a box -- thinner than the previous
  # 1.5 pt so it does not visually compete with the dark meta-table
  # header bands of the items below it.
  if (isTRUE(show_groups)) {
    gname <- lss_first_label(group$names, langs)
    if (is.na(gname)) gname <- paste0("Group ", group$gid)
    # Strip a leading numeric prefix written by the LimeSurvey author so
    # we do not get a doubled "1. 1. Vos etudes".
    gname <- lss_strip_group_number_prefix(gname)
    heading_text <- sprintf("%d. %s", state$group_index, gname)
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(
          heading_text,
          prop = officer::fp_text(
            font.family = theme$font_body, font.size = theme$size_heading1,
            bold = TRUE, color = theme$color_primary
          )
        ),
        fp_p = officer::fp_par(
          padding.top = 24, padding.bottom = 8,
          border.bottom = officer::fp_border(
            color = theme$color_primary, width = 1
          )
        )
      )
    )
    # Anchor the group heading with a bookmark so the manual TOC entries
    # can hyperlink to it.
    doc <- officer::body_bookmark(doc, lss_group_bookmark(state$group_index))
  }
  any_desc <- any(vapply(
    group$descriptions, function(v) !is.null(v) && !is.na(v) && nzchar(trimws(v)),
    logical(1)
  ))
  if (any_desc) {
    doc <- lss_render_lang_block(doc, group$descriptions, langs, theme,
                                 size = theme$size_subq, italic = TRUE)
  }

  for (q in group$questions) {
    doc <- lss_render_question_block(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx,
      state = state
    )
  }
  doc
}

#' Dispatch a parent question to leaf or compound rendering
#' @keywords internal
#' @noRd
lss_render_question_block <- function(doc, q, langs, theme,
                                      show_help, show_attrs,
                                      show_technical_attrs, audit_idx, state) {
  info <- lss_type_info(q$type)
  if (isTRUE(info$has_subquestions) && length(q$subquestions) > 0L) {
    doc <- lss_render_compound_question(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx,
      info = info,
      state = state
    )
  } else {
    doc <- lss_render_leaf_item(
      doc, q, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      show_technical_attrs = show_technical_attrs,
      audit_idx = audit_idx,
      item_code = q$code,
      texts_by_lang = lapply(langs, function(lg) q$texts[[lg]]$question),
      help_by_lang = lapply(langs, function(lg) q$texts[[lg]]$help),
      state = state
    )
  }
  # When the question's `other` flag is set, LimeSurvey adds a free-text
  # input as a sibling response variable named `parent_other`. We
  # document it as an item in its own right so the reader sees the
  # variable and the customized prompt.
  if (identical(q$other, "Y")) {
    doc <- lss_render_other_item(
      doc, q, langs, theme,
      audit_idx = audit_idx,
      state = state
    )
  }
  doc
}

#' Render a compound question (a parent with subquestions)
#'
#' Variable-centric rendering: each subquestion becomes its own
#' self-contained block (meta table + item table), with the parent stem
#' shown as the "Question" row, the subquestion label as a "Subquestion"
#' row, and the answer modalities (when any) repeated underneath. The
#' parent code itself is not surfaced as a meta entry because LimeSurvey
#' does not create a data variable named after the parent of an array,
#' multiple-choice, or multi-numerical question -- only the
#' `parent_subqcode` columns exist in the export. Repeating the stem and
#' the scale per subquestion is intentionally redundant: every variable
#' the reviewer can encounter in the dataset has all of its context in a
#' single block.
#'
#' @keywords internal
#' @noRd
lss_render_compound_question <- function(doc, q, langs, theme,
                                         show_help, show_attrs,
                                         show_technical_attrs, audit_idx,
                                         info, state) {
  for (sq in q$subquestions) {
    item_code <- paste0(q$code, "_", sq$code)
    doc <- lss_render_subq_item(
      doc, q, sq, langs, theme,
      item_code = item_code,
      show_help = show_help,
      show_attrs = show_attrs,
      audit_idx = audit_idx,
      state = state
    )
  }
  doc
}

#' Render the parent stem of a compound question as a small bordered block
#'
#' A meta line with the parent code and type, then a one-row flextable with
#' one column per language showing the stem text. Optional help and
#' attributes (prefix, suffix, validation, etc.) follow below.
#'
#' @keywords internal
#' @noRd
lss_render_parent_stem <- function(doc, q, langs, theme,
                                   show_help, show_attrs, audit_idx, state) {
  # No H1 emitted here, so the running item counter must NOT advance: the
  # parent stem is a banner, only its subq items below carry a number.
  doc <- lss_render_item_spacer(doc, theme)
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = NA_integer_,
    variable = q$code,
    type = q$type, type_label = lss_localized_type_label(q, theme),
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)

  texts_by_lang <- lapply(langs, function(lg) q$texts[[lg]]$question)
  help_by_lang <- lapply(langs, function(lg) q$texts[[lg]]$help)
  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = theme$chrome$item_question,
    texts = stats::setNames(texts_by_lang, langs),
    size = theme$size_question
  )
  if (isTRUE(show_help) && lss_any_present(help_by_lang)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_help,
      texts = stats::setNames(help_by_lang, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  coding <- lss_coding_row(q, langs, theme)
  if (!is.null(coding)) rows[[length(rows) + 1L]] <- coding
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  doc <- lss_render_item_table(doc, theme, langs, rows)
  doc
}

#' Render the LimeSurvey "Other:" text input as a standalone item
#'
#' When a question has `other = "Y"`, LimeSurvey generates an
#' additional response variable named `<parent>_other` that holds the
#' free text the respondent typed in the "Other:" field. The prompt
#' shown next to that input can be customized via the
#' `other_replace_text` attribute (per language). We surface this as
#' its own numbered item so the reader sees the variable code in the
#' index and meta table, alongside any other items.
#'
#' @keywords internal
#' @noRd
lss_render_other_item <- function(doc, q, langs, theme, audit_idx, state) {
  state$item_no <- state$item_no + 1L
  item_code <- paste0(q$code, "_other")
  state$index_entries[[length(state$index_entries) + 1L]] <- list(
    code = item_code, no = state$item_no
  )
  doc <- lss_render_item_spacer(doc, theme)

  # Look up the customized "Other:" prompt per language; fall back to a
  # generic "Other:" label when the attribute is missing.
  prompt_for_lang <- function(lg) {
    if (is.null(q$attributes) || nrow(q$attributes) == 0L) return("Other:")
    attrs <- q$attributes[q$attributes$attribute == "other_replace_text", , drop = FALSE]
    if (nrow(attrs) == 0L) return("Other:")
    lang_hit <- attrs$value[attrs$language == lg]
    if (length(lang_hit) > 0L && nzchar(trimws(lang_hit[1]))) return(lang_hit[1])
    empty_lang <- attrs$value[!nzchar(attrs$language)]
    if (length(empty_lang) > 0L && nzchar(trimws(empty_lang[1]))) return(empty_lang[1])
    "Other:"
  }
  texts_by_lang <- stats::setNames(lapply(langs, prompt_for_lang), langs)

  if (isTRUE(state$show_item_heading)) {
    heading_text <- sprintf("%d. %s", state$item_no, item_code)
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE, color = theme$color_text
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }

  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = item_code,
    type = "T", type_label = theme$chrome$type_text_other,
    mandatory = "N",
    relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)
  rows <- list(list(
    label = theme$chrome$item_question,
    texts = texts_by_lang,
    size = theme$size_question
  ))
  lss_render_item_table(doc, theme, langs, rows)
}

#' Render the shared answer scale of an array-style question
#' @keywords internal
#' @noRd
lss_render_shared_scale <- function(doc, q, langs, theme) {
  scales <- if (!is.null(q$scales) && length(q$scales) > 1L) q$scales else list(default = q$answers)
  for (si in seq_along(scales)) {
    answers <- scales[[si]]
    if (length(answers) == 0L) next
    title <- if (length(scales) > 1L) {
      sprintf("Shared answer scale %d", si)
    } else {
      "Shared answer scale"
    }
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(
        title,
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_meta,
          bold = TRUE, color = theme$color_primary
        )
      ))
    )
    doc <- lss_render_scale_table(doc, answers, langs, theme)
  }
  doc
}

#' Build the answer-scale flextable
#' @keywords internal
#' @noRd
lss_render_scale_table <- function(doc, answers, langs, theme) {
  df <- data.frame(
    code = vapply(answers, function(a) a$code, character(1)),
    stringsAsFactors = FALSE
  )
  for (lg in langs) df[[lg]] <- ""
  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      list(code = "Value"),
      stats::setNames(as.list(lss_language_label(langs)), langs)
    )
  )
  for (i in seq_along(answers)) {
    for (lg in langs) {
      ft <- flextable::compose(
        ft, i = i, j = lg,
        value = lss_compose(answers[[i]]$labels[[lg]], theme,
                            size = theme$size_answer)
      )
    }
  }
  ft <- lss_table_polish(ft, theme, lang_cols = langs, has_code = TRUE)
  flextable::body_add_flextable(doc, ft, align = "left")
}

#' Render a subquestion as a fully self-contained numbered item
#'
#' Each subquestion of a compound question (array, multiple choice,
#' multiple numerical, dual-scale array) is rendered as its own block
#' with the same shape as a leaf item:
#'
#' - meta table keyed by `parent_subqcode` (the actual data variable),
#' - item table whose first row ("Question") is the parent stem,
#'   second row ("Subquestion") is the subquestion label, optional
#'   "Help" row from the parent, then any subquestion-level attributes,
#'   then -- for types that carry an enumerated scale -- the parent's
#'   answer modalities repeated as a "Value" section + value rows.
#'
#' @keywords internal
#' @noRd
lss_render_subq_item <- function(doc, q, sq, langs, theme,
                                 item_code, show_help, show_attrs,
                                 audit_idx, state) {
  state$item_no <- state$item_no + 1L
  state$index_entries[[length(state$index_entries) + 1L]] <- list(
    code = item_code, no = state$item_no
  )
  doc <- lss_render_item_spacer(doc, theme)
  if (isTRUE(state$show_item_heading)) {
    audit_marker <- lss_audit_marker(item_code, audit_idx, theme)
    heading_text <- sprintf("%d. %s", state$item_no, item_code)
    if (!is.null(audit_marker)) {
      heading_text <- paste0(heading_text, "  ", audit_marker$text)
    }
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE,
      color = if (is.null(audit_marker)) theme$color_text else audit_marker$color
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = item_code,
    type = q$type, type_label = lss_localized_type_label(q, theme),
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)

  parent_text <- lapply(langs, function(lg) q$texts[[lg]]$question)
  parent_help <- lapply(langs, function(lg) q$texts[[lg]]$help)
  subq_text  <- lapply(langs, function(lg) sq$texts[[lg]]$question)

  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = theme$chrome$item_question,
    texts = stats::setNames(parent_text, langs),
    size = theme$size_question
  )
  if (lss_any_present(subq_text)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_subquestion,
      texts = stats::setNames(subq_text, langs),
      size = theme$size_subq
    )
  }
  if (isTRUE(show_help) && lss_any_present(parent_help)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_help,
      texts = stats::setNames(parent_help, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  # Subquestion-level attributes first, then parent-level (prefix,
  # suffix, validation, ...). `exclude_all_others*` are deliberately
  # filtered out of these generic loops: their meaning is "this single
  # subquestion is the exclusive one" and surfacing them on every
  # subquestion would lie about the rule. We emit one targeted row via
  # `lss_exclusive_row()` instead, only when THIS subquestion is the
  # named exclusive entry.
  rows <- c(rows, lss_attr_rows(sq, langs, theme, show_attrs))
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  exclusive <- lss_exclusive_row(q, sq, langs, theme)
  if (!is.null(exclusive)) rows[[length(rows) + 1L]] <- exclusive
  # Value section: enumerated codes (F, 1) or a single implicit-format
  # row describing the response shape (M/P "Y selected", K "Numeric",
  # ...).
  if (length(q$answers) > 0L) {
    rows <- c(rows, lss_answer_rows(q, langs, theme))
  } else {
    vrow <- lss_value_implicit_row(q, langs, theme)
    if (!is.null(vrow)) rows[[length(rows) + 1L]] <- vrow
  }
  doc <- lss_render_item_table(doc, theme, langs, rows)
  doc
}

#' Render a leaf question (no subquestions) as a numbered item
#' @keywords internal
#' @noRd
lss_render_leaf_item <- function(doc, q, langs, theme,
                                 show_help, show_attrs, show_technical_attrs,
                                 audit_idx, item_code,
                                 texts_by_lang, help_by_lang, state) {
  state$item_no <- state$item_no + 1L
  state$index_entries[[length(state$index_entries) + 1L]] <- list(
    code = item_code, no = state$item_no
  )
  doc <- lss_render_item_spacer(doc, theme)
  if (isTRUE(state$show_item_heading)) {
    audit_marker <- lss_audit_marker(item_code, audit_idx, theme)
    heading_text <- sprintf("%d. %s", state$item_no, item_code)
    if (!is.null(audit_marker)) {
      heading_text <- paste0(heading_text, "  ", audit_marker$text)
    }
    heading_prop <- officer::fp_text(
      font.family = theme$font_body, font.size = theme$size_heading2,
      bold = TRUE,
      color = if (is.null(audit_marker)) theme$color_text else audit_marker$color
    )
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(
        officer::ftext(heading_text, prop = heading_prop),
        fp_p = officer::fp_par(padding.top = 8, padding.bottom = 2)
      )
    )
  }

  # Structured meta table: No | Variable | Type | Mandatory | Filter
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = q$code,
    type = q$type, type_label = lss_localized_type_label(q, theme),
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  doc <- lss_render_intra_item_gap(doc, theme)

  # Build the unified item table: Question, optional Help, then one
  # row per answer option (for has_answers leaf types like L, !, O).
  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = theme$chrome$item_question,
    texts = stats::setNames(texts_by_lang, langs),
    size = theme$size_question
  )
  if (isTRUE(show_help) && lss_any_present(help_by_lang)) {
    rows[[length(rows) + 1L]] <- list(
      label = theme$chrome$item_help,
      texts = stats::setNames(help_by_lang, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  # Question attributes (prefix, suffix, validation, ...) as italic rows
  # inside the item table itself, between Help and the Value section.
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  # Value section: enumerated codes (L, !, F, 1) when q$answers is
  # populated; otherwise a single implicit-format row describing the
  # response shape (Y "Y = Yes, N = No", N "Numeric input", T "Free
  # text", ...). Skips entirely for X (boilerplate).
  if (length(q$answers) > 0L) {
    rows <- c(rows, lss_answer_rows(q, langs, theme))
  } else {
    vrow <- lss_value_implicit_row(q, langs, theme)
    if (!is.null(vrow)) rows[[length(rows) + 1L]] <- vrow
  }
  doc <- lss_render_item_table(doc, theme, langs, rows)
  doc
}

#' Build the rows that document the answer scale of a (sub)question
#'
#' Emits a "Value" section header followed by one row per answer option,
#' code on the left, label per language on the right. Splits into
#' "Value (scale 1)" / "Value (scale 2)" for dual-scale arrays (type 1).
#' Returns an empty list when the question carries no enumerated answers
#' (e.g. multiple-choice M, free numeric input K) -- in those cases the
#' coding row already documents the response value mapping.
#'
#' @keywords internal
#' @noRd
lss_answer_rows <- function(q, langs, theme) {
  if (length(q$answers) == 0L) return(list())
  out <- list()
  multi_scale <- !is.null(q$scales) && length(q$scales) > 1L
  bundles <- if (multi_scale) q$scales else list(q$answers)
  for (si in seq_along(bundles)) {
    answers <- bundles[[si]]
    if (length(answers) == 0L) next
    header_label <- if (multi_scale) {
      sprintf(theme$chrome$item_value_scale_fmt, si)
    } else {
      theme$chrome$item_value
    }
    out[[length(out) + 1L]] <- list(
      label = header_label,
      texts = stats::setNames(as.list(rep("", length(langs))), langs),
      size = theme$size_meta,
      section_header = TRUE
    )
    for (a in answers) {
      out[[length(out) + 1L]] <- list(
        label = a$code,
        texts = stats::setNames(lapply(langs, function(lg) a$labels[[lg]]), langs),
        size = theme$size_answer,
        value_row = TRUE
      )
    }
  }
  out
}

#' Render a unified item table with a left "Label" column
#'
#' Builds a single flextable per item with the layout
#' `Language | Fran\u00E7ais | Deutsch | ...` as header and one body row per
#' content element (`Question`, `Help`, `Value 1`, `Value 2`, ...).
#' Each row carries its own label so the document reads as
#' self-describing: the reviewer sees `Question:` and `Help:` rather
#' than having to infer it from position. This is the ESS/MOSAiCH
#' convention where each tabular row names what it represents.
#'
#' @param rows A list of `list(label = "...", texts = list(lang = "..."), size = ...)`.
#'   `texts` is a named list keyed by language code; `size` is the body
#'   font size for that row (defaults to `theme$size_question` when
#'   omitted). Rows missing or empty across every language are kept
#'   (we never silently drop content), with cells filled by the muted
#'   em-dash placeholder.
#' @keywords internal
#' @noRd
lss_render_item_table <- function(doc, theme, langs, rows) {
  if (length(rows) == 0L) return(doc)

  df <- data.frame(
    Label = vapply(rows, function(r) r$label, character(1)),
    stringsAsFactors = FALSE
  )
  for (lg in langs) df[[lg]] <- ""

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft,
    values = c(
      list(Label = theme$chrome$item_language),
      stats::setNames(as.list(lss_language_label(langs)), langs)
    )
  )
  for (i in seq_along(rows)) {
    sz <- if (!is.null(rows[[i]]$size)) rows[[i]]$size else theme$size_question
    italic <- isTRUE(rows[[i]]$italic)
    color <- if (!is.null(rows[[i]]$color)) rows[[i]]$color else theme$color_text
    is_section <- isTRUE(rows[[i]]$section_header)
    section_with_text <- isTRUE(rows[[i]]$section_with_text)
    for (lg in langs) {
      if (is_section && !section_with_text) {
        # Section header row: language cells stay truly blank (no em-dash
        # placeholder), so the row reads as a category label.
        ft <- flextable::compose(
          ft, i = i, j = lg,
          value = flextable::as_paragraph(flextable::as_chunk(""))
        )
      } else {
        ft <- flextable::compose(
          ft, i = i, j = lg,
          value = lss_compose(rows[[i]]$texts[[lg]], theme,
                              size = sz, color = color, italic_default = italic)
        )
      }
    }
  }
  ft <- lss_table_polish(ft, theme, lang_cols = langs)
  ft <- flextable::bold(ft, j = "Label", part = "body")
  ft <- flextable::color(ft, j = "Label", color = theme$color_primary, part = "body")
  ft <- flextable::align(ft, j = "Label", align = "left", part = "body")
  # Answer-code rows (label = "1", "2", ...) are centered to match the
  # shared scale convention from earlier renders: the Value section header
  # stays left, but each value code under it reads as a centered ticker.
  for (i in seq_along(rows)) {
    if (isTRUE(rows[[i]]$value_row)) {
      ft <- flextable::align(ft, i = i, j = "Label", align = "center", part = "body")
    }
  }
  # Match the meta table total width (theme$content_width_in) so the two
  # tables align visually. The Label column takes 1.0 in (same as the meta
  # table's Mandatory column) and the language columns split the rest.
  label_w <- 1.0
  total_w <- theme$content_width_in
  lang_w <- (total_w - label_w) / length(langs)
  ft <- flextable::width(ft, j = "Label", width = label_w, unit = "in")
  for (lg in langs) {
    ft <- flextable::width(ft, j = lg, width = lang_w, unit = "in")
  }
  # Section-header rows share the header's light-blue band background so
  # the Language / Value bands are visually consistent.
  for (i in seq_along(rows)) {
    if (isTRUE(rows[[i]]$section_header)) {
      ft <- flextable::bg(ft, i = i, bg = theme$color_band, part = "body")
    }
  }
  flextable::body_add_flextable(doc, ft, align = "left")
}

#' Item-table row describing the response coding for types where it is
#' implicit rather than listed as enumerated answers
#'
#' For multiple-choice questions and predefined types (Yes/No, 5-point,
#' gender), LimeSurvey stores responses with implicit codes (`Y/empty`,
#' `Y/N`, `1..5`, ...). The Value section of the item table either
#' lists the answer codes explicitly (for `has_answers` types) or stays
#' empty (for predefined types). This helper produces a small italic
#' "Coding" row that documents the value mapping for the latter case.
#'
#' @return A single row list compatible with `lss_render_item_table`,
#'   or `NULL` when the type has no implicit coding worth printing.
#' @keywords internal
#' @noRd
lss_coding_row <- function(q, langs, theme) {
  coding <- switch(
    q$type,
    "M" = "Y = selected, blank = not selected",
    "P" = "Y = selected, blank = not selected (plus a `<subq>comment` text variable)",
    "Y" = "Y = Yes, N = No",
    "G" = "M = Male, F = Female",
    "5" = "1, 2, 3, 4, 5 (1 = lowest, 5 = highest)",
    NULL
  )
  if (is.null(coding)) return(NULL)
  list(
    label = "Coding",
    texts = stats::setNames(rep(list(coding), length(langs)), langs),
    size = theme$size_meta,
    color = theme$color_muted,
    italic = TRUE
  )
}

#' Build a single-row "Value" section header carrying the response
#' format descriptor for question types that have no enumerated answer
#' table.
#'
#' Every variable in the dataset has a domain of valid responses; the
#' Value section of the item table is where reviewers expect to find
#' it. For enumerated types (L, !, F, 1) the rows come from
#' [lss_answer_rows()]; for the rest we emit a single section-header
#' row that holds the descriptor in the language columns (with the
#' band tone background, matching the enumerated case visually).
#' Returns `NULL` only for X (boilerplate/display-only), which stores
#' no response.
#'
#' @keywords internal
#' @noRd
lss_value_implicit_row <- function(q, langs, theme) {
  chrome <- theme$chrome
  text <- switch(
    q$type,
    # Multi-choice subquestions: each subq is a binary Y/blank flag.
    "M" = chrome$value_multi_y_blank,
    "P" = chrome$value_multi_y_blank_with_comment,
    # Pre-defined enumerated types with implicit (not stored) codes.
    "Y" = chrome$value_yes_no,
    "G" = chrome$value_gender,
    "5" = chrome$value_5point,
    # Numeric inputs. K shares N's descriptor; the multi-variable
    # fan-out is conveyed by the `Type` cell and the `parent_subq`
    # variable code, not by a parenthetical on Value.
    "N" = chrome$value_numeric_input,
    "K" = chrome$value_numeric_input,
    # Free-text inputs of varying length.
    "S" = chrome$value_free_text_short,
    "T" = chrome$value_free_text,
    "U" = chrome$value_free_text_long,
    # Date / time picker.
    "D" = chrome$value_date_input,
    # Equation: server-computed value, not respondent-entered.
    "*" = chrome$value_computed,
    # Ranking: respondent orders the subquestions.
    "R" = chrome$value_ranking,
    # File upload.
    "|" = chrome$value_file_upload,
    # Anything else (including X = boilerplate / display-only) gets no
    # Value section, since the variable carries no response in the data.
    NULL
  )
  if (is.null(text)) return(NULL)
  list(
    label = chrome$item_value,
    texts = stats::setNames(rep(list(text), length(langs)), langs),
    size = theme$size_answer,
    section_header = TRUE,
    section_with_text = TRUE
  )
}

#' Insert a small vertical spacer before each item so consecutive
#' meta tables do not touch.
#'
#' Carries `keep_with_next` so the spacer stays anchored to its own
#' meta table at the top of the next item, which in turn keeps with
#' the gap and the item table (full meta -> gap -> item chain on
#' a single page).
#' @keywords internal
#' @noRd
lss_render_item_spacer <- function(doc, theme) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(" ", prop = officer::fp_text(
        font.family = theme$font_body,
        font.size = theme$size_meta
      )),
      fp_p = officer::fp_par(padding.top = 14, padding.bottom = 0,
                             keep_with_next = TRUE)
    )
  )
}

#' Thin breathing space between the meta table and the item table
#' so they read as two separate panels rather than one continuous block.
#' Carries `keep_with_next` so Word does not break a page between the
#' meta table and the item table that follows: the dark band stays
#' anchored to its content.
#' @keywords internal
#' @noRd
lss_render_intra_item_gap <- function(doc, theme) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(" ", prop = officer::fp_text(
        font.family = theme$font_body,
        font.size = 4
      )),
      fp_p = officer::fp_par(keep_with_next = TRUE)
    )
  )
}

#' Render a one-row flextable with one column per language
#'
#' Generic helper used everywhere a snippet of text needs to be shown side
#' by side per language (stems, descriptions, help, subquestion items).
#'
#' @keywords internal
#' @noRd
lss_render_lang_block <- function(doc, texts_by_lang, langs, theme,
                                  size = theme$size_question,
                                  color = theme$color_text,
                                  italic = FALSE,
                                  show_header = FALSE) {
  df <- as.data.frame(
    matrix("", nrow = 1L, ncol = length(langs)),
    stringsAsFactors = FALSE
  )
  names(df) <- langs
  ft <- flextable::flextable(df)
  if (isTRUE(show_header)) {
    ft <- flextable::set_header_labels(
      ft, values = stats::setNames(lss_language_label(langs), langs)
    )
  } else {
    ft <- flextable::delete_part(ft, part = "header")
  }
  for (lg in langs) {
    ft <- flextable::compose(
      ft, i = 1L, j = lg,
      value = lss_compose(
        texts_by_lang[[lg]], theme,
        size = size, color = color, italic_default = italic
      )
    )
  }
  ft <- lss_table_polish(ft, theme, lang_cols = langs)
  flextable::body_add_flextable(doc, ft, align = "left")
}

#' Render a language block only when at least one language has non-empty text
#' @keywords internal
#' @noRd
lss_render_optional_lang_block <- function(doc, texts_by_lang, langs, theme,
                                           size, color, italic = FALSE) {
  any_present <- any(vapply(
    texts_by_lang,
    function(v) !is.null(v) && !is.na(v) && nzchar(trimws(as.character(v))),
    logical(1)
  ))
  if (!any_present) return(doc)
  lss_render_lang_block(doc, texts_by_lang, langs, theme,
                        size = size, color = color, italic = italic)
}

#' Build item-table rows for the requested question attributes
#'
#' Each rendered attribute becomes one labelled row inside the item
#' table (Prefix, Suffix, Validation, ...), in italic muted gray so it
#' stays visually distinct from the question text and the answer
#' values. Attributes that are empty in every language are skipped.
#' `other_replace_text` is omitted because it is documented as its own
#' numbered item via `lss_render_other_item()`.
#'
#' @keywords internal
#' @noRd
lss_attr_rows <- function(q, langs, theme, show_attrs) {
  if (length(show_attrs) == 0L || is.null(q$attributes)) {
    return(list())
  }
  attrs <- q$attributes
  rows <- list()
  for (attr_name in setdiff(show_attrs, "other_replace_text")) {
    hit <- attrs$attribute == attr_name
    if (!any(hit)) next
    matches <- attrs[hit, , drop = FALSE]
    per_lang <- vapply(langs, function(lg) {
      lang_hit <- matches$value[matches$language == lg]
      if (length(lang_hit) > 0L && nzchar(trimws(lang_hit[1]))) return(lang_hit[1])
      empty_lang <- matches$value[!nzchar(matches$language)]
      if (length(empty_lang) > 0L && nzchar(trimws(empty_lang[1]))) return(empty_lang[1])
      ""
    }, character(1))
    if (!lss_any_present(as.list(per_lang))) next

    # Skip technical attributes when they hold their default/inactive
    # value (so a reviewer never sees a noisy row like
    # `Exclude_all_others_auto = 0` that documents the absence of a
    # behavior). For attributes that ARE active, rewrite the value
    # into a sentence a methodologist can act on.
    fmt <- lss_format_attr(attr_name, per_lang, langs)
    if (is.null(fmt)) next

    rows[[length(rows) + 1L]] <- list(
      label = fmt$label,
      texts = fmt$texts,
      size = theme$size_meta,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  rows
}

#' Format a question/subquestion attribute for display in the item table
#'
#' Returns a `list(label, texts)` ready to be wrapped into an
#' attribute row, or `NULL` when the attribute should be hidden.
#'
#' `exclude_all_others` and `exclude_all_others_auto` are intentionally
#' suppressed here. They live on the parent question of a compound
#' multi-choice question, and surfacing them through the generic
#' attribute loop repeats the same exclusion notice on every
#' subquestion. They are handled specially in `lss_render_subq_item()`
#' where the renderer knows the current subquestion code and can
#' target the message at the right row only.
#'
#' All other attributes pass through with a Title-Case label and the
#' raw per-language value.
#'
#' @keywords internal
#' @noRd
lss_format_attr <- function(attr_name, per_lang, langs) {
  if (attr_name %in% c("exclude_all_others", "exclude_all_others_auto")) {
    return(NULL)
  }
  list(
    label = tools::toTitleCase(attr_name),
    texts = stats::setNames(as.list(per_lang), langs)
  )
}

#' If the parent question of a compound multi-choice question declares
#' an `exclude_all_others` attribute, emit an "Exclusive" row only on
#' the subquestion(s) whose code is named in the attribute value
#'
#' LimeSurvey stores `exclude_all_others` on the parent `qid` with the
#' value being one (or several, comma-separated) subquestion titles
#' that, when checked, clear every other selection. We use the
#' subquestion code to decide whether THIS subquestion is the
#' exclusive one, and only then render a single italic row that names
#' the parent variable so a reviewer knows what gets cleared.
#'
#' Returns `NULL` (i.e. no row) when the attribute is absent or when
#' the current subquestion is not in the exclusion list.
#'
#' @keywords internal
#' @noRd
lss_exclusive_row <- function(q, sq, langs, theme) {
  if (is.null(q$attributes) || nrow(q$attributes) == 0L) return(NULL)
  hit <- q$attributes[q$attributes$attribute == "exclude_all_others", , drop = FALSE]
  if (nrow(hit) == 0L) return(NULL)
  raw <- trimws(as.character(hit$value[1L]))
  if (!nzchar(raw)) return(NULL)
  targets <- trimws(strsplit(raw, ",", fixed = TRUE)[[1L]])
  if (!(sq$code %in% targets)) return(NULL)
  text <- sprintf(theme$chrome$exclusive_text_fmt, q$code)
  list(
    label = theme$chrome$item_exclusive,
    texts = stats::setNames(rep(list(text), length(langs)), langs),
    size = theme$size_meta,
    color = theme$color_muted,
    italic = TRUE
  )
}

#' Legacy: render attributes as small italic lines (kept for backward
#' compatibility, no longer called from the main render path).
#' @keywords internal
#' @noRd
lss_render_attrs <- function(doc, q, langs, theme, show_attrs) {
  for (row in lss_attr_rows(q, langs, theme, show_attrs)) {
    val <- paste(unlist(row$texts), collapse = " | ")
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(
        sprintf("%s: %s", row$label, val),
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_meta,
          color = theme$color_muted, italic = TRUE
        )
      ))
    )
  }
  doc
}


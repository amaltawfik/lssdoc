#' Render the survey content as a single dense codebook-style table
#'
#' Alternative to the per-item "cards" layout. Produces one big
#' flextable where each variable is a single row, the meta fields
#' (No, Variable, Type, Mandatory, Filter) sit in the first five
#' columns, and one Question column per content language holds the
#' question stem, the subquestion label when applicable, optional
#' help text, and the response modalities stacked underneath the
#' question text.
#'
#' Group banners become merged section rows inside the table with a
#' dark petrol band so a reviewer sees a clear visual break between
#' sections; the column header repeats on every page automatically
#' (flextable's default OOXML output).
#'
#' @keywords internal
#' @noRd
lss_render_table_template <- function(doc, rows, langs, theme,
                                      show_help, show_attrs, state) {
  if (length(rows) == 0L) return(doc)

  chrome <- theme$chrome
  meta_cols <- c("Field", "No", "Variable", "Type", "Mandatory",
                 "Filter", "Value")
  lang_cols <- paste0("Q_", langs)
  all_cols <- c(meta_cols, lang_cols)

  # ---- Build the data frame skeleton ------------------------------
  # Codebook layout: every variable produces one tinted Question row
  # carrying the meta (No, Variable, Type, Mandatory, Filter) and the
  # localized question text, followed by N white Value rows (one per
  # enumerated answer code), the rest empty. Section rows span the
  # whole table in a dark petrol band.
  df <- as.data.frame(
    matrix("", nrow = length(rows), ncol = length(all_cols)),
    stringsAsFactors = FALSE
  )
  names(df) <- all_cols

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (identical(r$kind, "section")) {
      # Section text in the first column so merge_at() below keeps
      # it visible after the row span collapses into one cell.
      df$Field[i] <- r$text
    } else if (identical(r$kind, "scale_header")) {
      # Dual-scale separator: announce "Value (scale N)" on the
      # Value column. Lang cells stay empty.
      df$Value[i] <- r$text
    } else if (identical(r$kind, "value")) {
      df$Value[i] <- as.character(r$code)
      # Labels per language composed via flextable::compose() below.
    } else {
      # Question row (leaf / subq / other).
      df$Field[i]     <- chrome$item_question
      df$No[i]        <- as.character(r$no)
      df$Variable[i]  <- r$variable
      df$Type[i]      <- r$type_label
      df$Mandatory[i] <- r$mandatory_label
      # Filter and Value cells composed below.
    }
  }

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft, values = stats::setNames(
      as.list(c(
        "",  # Field column header stays empty; the cell content speaks for itself.
        chrome$meta_no, chrome$meta_variable, chrome$meta_type,
        chrome$meta_mandatory, chrome$meta_filter, chrome$item_value,
        lss_language_label(langs)
      )),
      all_cols
    )
  )

  # ---- Compose rich cells per row ---------------------------------
  filter_plain_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_meta,
    color = theme$color_text
  )
  filter_raw_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_meta - 1L,
    color = theme$color_muted, italic = TRUE
  )

  value_code_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_meta,
    color = theme$color_primary, bold = TRUE
  )
  value_descriptor_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_meta,
    color = theme$color_muted, italic = TRUE
  )

  value_label_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_answer,
    color = theme$color_text
  )
  empty_marker_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_answer,
    color = theme$color_muted
  )

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    kind <- r$kind
    if (identical(kind, "section") || identical(kind, "scale_header")) {
      # Section is merged & styled in the polish step. Scale header
      # carries text already in df$Value -- no rich composition.
      next
    }

    if (identical(kind, "value")) {
      # Value row: codes in mono primary inside the Value column (df
      # already set the bare code), labels per language. Empty
      # cells fall back to the muted em-dash.
      for (lg in langs) {
        label <- lss_html_to_text(r$labels[[lg]])
        if (!nzchar(label)) {
          ft <- flextable::compose(
            ft, i = i, j = paste0("Q_", lg),
            value = flextable::as_paragraph(flextable::as_chunk(
              theme$empty_marker, props = empty_marker_props
            ))
          )
        } else {
          ft <- flextable::compose(
            ft, i = i, j = paste0("Q_", lg),
            value = flextable::as_paragraph(flextable::as_chunk(
              label, props = value_label_props
            ))
          )
        }
      }
      next
    }

    # Question row (leaf / subq / other) ----------------------------
    ft <- flextable::compose(
      ft, i = i, j = "Filter",
      value = lss_table_filter_paragraph(
        r$relevance, theme,
        plain_props = filter_plain_props,
        raw_props = filter_raw_props,
        show_raw = isTRUE(state$show_raw_filter)
      )
    )
    # For non-enumerated types (M/P/N/K/T/S/U/D/...), the implicit
    # response-domain descriptor goes in the Value cell of the
    # Question row. For enumerated types (L/F/1/...) the cell stays
    # empty because the codes appear in their own Value rows below.
    ft <- flextable::compose(
      ft, i = i, j = "Value",
      value = lss_table_value_paragraph(
        r, theme,
        code_props = value_code_props,
        descriptor_props = value_descriptor_props
      )
    )
    for (lg in langs) {
      ft <- flextable::compose(
        ft, i = i, j = paste0("Q_", lg),
        value = lss_table_question_paragraph(r, lg, theme,
                                             show_help = show_help)
      )
    }
  }

  # Polish applies row-type-aware styling (section merge + petrol
  # band, Q-row tint, scale_header tint, widths, etc.).
  ft <- lss_table_template_polish(ft, theme, rows, n_lang = length(langs))

  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc
}

#' Walk the model and produce the flat list of rows that will become
#' the codebook table -- alternating "section" markers (group
#' banners) and "item" rows (one per variable).
#'
#' Each "item" row is normalized in this builder so the main
#' renderer never has to look at the question structure again:
#' `no`, `variable` (the LimeSurvey data column, `parent_subq` for
#' compound subqs), `type_label`, `mandatory_label`, `relevance`,
#' plus pre-extracted texts per language (`parent_text`, `subq_text`,
#' `help`) and the question/subquestion model objects so the cell
#' composer can pull the answer scale or the implicit-coding
#' descriptor at render time.
#'
#' Also advances `state$item_no`, `state$group_index` and
#' `state$index_entries` so the rest of the document (TOC, Variable
#' index, navigation) stays consistent with the table layout. The
#' progress bar is updated at each group boundary.
#'
#' @keywords internal
#' @noRd
lss_table_template_rows_for_group <- function(g, langs, theme,
                                              show_help, state) {
  chrome <- theme$chrome
  rows <- list()
  gname <- lss_first_label(g$names, langs)
  if (is.na(gname)) gname <- paste0("Group ", g$gid)
  gname <- lss_strip_group_number_prefix(gname)
  state$group_index <- state$group_index + 1L
  rows[[length(rows) + 1L]] <- list(
    kind = "section",
    text = sprintf("%d. %s", state$group_index, gname)
  )

  # Build one Question row per variable (carrying the meta and the
  # question/subq/help text) followed by N Value rows (one per
  # enumerated answer code, each carrying its label per language).
  # Variables with no enumerated answers produce only the Question
  # row -- the implicit-coding descriptor sits in their Value cell.
  emit_question_row <- function(kind, no, variable, q, sq = NULL,
                                other_q = NULL) {
    list(
      kind = kind, no = no, variable = variable,
      type_label = if (identical(kind, "other")) chrome$type_text_other
                   else lss_localized_type_label(q, theme),
      mandatory_label = lss_yes_no(
        if (identical(kind, "other")) "N" else q$mandatory, theme
      ),
      relevance = q$relevance,
      parent_text = stats::setNames(
        lapply(langs, function(lg) q$texts[[lg]]$question), langs
      ),
      subq_text = if (!is.null(sq)) {
        stats::setNames(
          lapply(langs, function(lg) sq$texts[[lg]]$question), langs
        )
      } else NULL,
      help = if (identical(kind, "other")) NULL else {
        stats::setNames(
          lapply(langs, function(lg) q$texts[[lg]]$help), langs
        )
      },
      q = q, sq = sq, other_q = other_q
    )
  }

  emit_value_rows_for <- function(q) {
    # No value rows for non-enumerated types; the implicit descriptor
    # sits in the Question row's Value cell.
    if (length(q$answers) == 0L) return(list())
    multi_scale <- !is.null(q$scales) && length(q$scales) > 1L
    bundles <- if (multi_scale) q$scales else list(q$answers)
    out <- list()
    for (si in seq_along(bundles)) {
      ans <- bundles[[si]]
      if (length(ans) == 0L) next
      if (multi_scale) {
        # Dual-scale separator: a tinted scale-header row carrying
        # "Value (scale N)" on the left so the reader sees a break
        # between the two response axes.
        out[[length(out) + 1L]] <- list(
          kind = "scale_header",
          text = sprintf(chrome$item_value_scale_fmt, si)
        )
      }
      for (a in ans) {
        out[[length(out) + 1L]] <- list(
          kind = "value",
          code = a$code,
          labels = stats::setNames(
            lapply(langs, function(lg) a$labels[[lg]]), langs
          )
        )
      }
    }
    out
  }

  for (q in g$questions) {
      info <- lss_type_info(q$type)
      if (isTRUE(info$has_subquestions) && length(q$subquestions) > 0L) {
        for (sq in q$subquestions) {
          state$item_no <- state$item_no + 1L
          item_code <- paste0(q$code, "_", sq$code)
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = state$item_no
          )
          rows[[length(rows) + 1L]] <- emit_question_row(
            "subq", state$item_no, item_code, q = q, sq = sq
          )
          rows <- c(rows, emit_value_rows_for(q))
        }
        if (identical(q$other, "Y")) {
          state$item_no <- state$item_no + 1L
          item_code <- paste0(q$code, "_other")
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = state$item_no
          )
          rows[[length(rows) + 1L]] <- emit_question_row(
            "other", state$item_no, item_code, q = q, other_q = q
          )
        }
      } else {
        state$item_no <- state$item_no + 1L
        state$index_entries[[length(state$index_entries) + 1L]] <- list(
          code = q$code, no = state$item_no
        )
        rows[[length(rows) + 1L]] <- emit_question_row(
          "leaf", state$item_no, q$code, q = q
        )
        rows <- c(rows, emit_value_rows_for(q))
      }
  }
  rows
}

#' Build the flextable paragraph for the Filter cell of a row (the
#' codebook template). Mirrors the meta-table convention from the
#' cards path: human-readable form on top, raw LimeSurvey expression
#' beneath in a smaller italic mono.
#' @keywords internal
#' @noRd
lss_table_filter_paragraph <- function(relevance, theme,
                                       plain_props, raw_props,
                                       show_raw = TRUE) {
  filter_raw <- if (is.null(relevance) || is.na(relevance) ||
                    !nzchar(relevance)) {
    "1"
  } else {
    lss_strip_outer_parens(relevance)
  }
  filter_plain <- lss_humanize_relevance(filter_raw, theme)
  chunks <- list(flextable::as_chunk(filter_plain, props = plain_props))
  if (isTRUE(show_raw) && !identical(filter_plain, filter_raw)) {
    chunks <- c(
      chunks,
      list(flextable::as_chunk("\n", props = plain_props)),
      list(flextable::as_chunk(filter_raw, props = raw_props))
    )
  }
  do.call(flextable::as_paragraph, chunks)
}

#' Build the flextable paragraph for the Value cell of a row.
#'
#' Compact, language-independent summary of the response domain so a
#' reviewer can scan the codes column without reading the Question
#' columns. Conventions:
#'
#' * Enumerated answers with sequential integer codes (`1..N`) ->
#'   the range `1-N` in mono primary (e.g. "1-5", "1-7").
#' * Enumerated with non-sequential codes -> the codes joined by
#'   commas in mono primary (e.g. "1, 2, 99").
#' * Dual-scale arrays -> per-scale summary on its own line
#'   ("Scale 1: 1-5", "Scale 2: 1-3").
#' * Implicit codings (multi-choice, yes/no, gender, 5-point) ->
#'   short fixed token in mono primary ("Y/blank", "Y/N", "M/F",
#'   "1-5").
#' * Open-ended types -> descriptor in italic muted ("[num]",
#'   "[text]", "[date]", "[file]", "[calc]", "[rank]").
#' * Other item and section rows -> empty cell.
#'
#' @keywords internal
#' @noRd
lss_table_value_paragraph <- function(row, theme, code_props, descriptor_props) {
  # The codebook layout devotes a dedicated Value row to every
  # enumerated code, so the Question row's Value cell stays empty
  # for enumerated types -- only non-enumerated types receive an
  # inline descriptor in the Question row itself.
  if (identical(row$kind, "other")) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  q <- row$q
  if (length(q$answers) > 0L) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  # Implicit codings: short mono token.
  short_code <- switch(
    EXPR = q$type,
    "M" = "Y/blank",
    "P" = "Y/blank",
    "Y" = "Y/N",
    "G" = "M/F",
    "5" = "1-5",
    NULL
  )
  if (!is.null(short_code)) {
    return(flextable::as_paragraph(flextable::as_chunk(
      short_code, props = code_props
    )))
  }
  descriptor <- switch(
    EXPR = q$type,
    "N" = "[num]",
    "K" = "[num]",
    "S" = "[text]",
    "T" = "[text]",
    "U" = "[text]",
    "D" = "[date]",
    "*" = "[calc]",
    "R" = "[rank]",
    "|" = "[file]",
    "X" = "\u2014",
    "\u2014"
  )
  flextable::as_paragraph(flextable::as_chunk(
    descriptor, props = descriptor_props
  ))
}

#' Compact comma-or-range string for an enumerated answer list.
#'
#' If the codes are sequential integers `1..N` (or `0..N`) -> `"1-N"`
#' (or `"0-N"`). Otherwise -> the codes joined by ", ".
#'
#' @keywords internal
#' @noRd
lss_table_value_codes <- function(answers) {
  codes <- vapply(answers, function(a) as.character(a$code), character(1L))
  if (length(codes) == 0L) return("")
  nums <- suppressWarnings(as.integer(codes))
  if (!anyNA(nums) && length(nums) >= 2L) {
    sorted <- sort(nums)
    if (identical(sorted, seq.int(sorted[1L], sorted[length(sorted)]))) {
      return(sprintf("%d-%d", sorted[1L], sorted[length(sorted)]))
    }
  }
  paste(codes, collapse = ", ")
}

#' Build the flextable paragraph for one Question cell (one row,
#' one language).
#'
#' Stacks the question stem, the subquestion label (compound rows),
#' the help text (when present and `show_help`), and the response
#' modalities (the answer scale or the implicit-coding descriptor).
#' Each layer has its own typography: stem in the body color at
#' question size, subq in the same size but italic to distinguish,
#' help small and gray, answer codes in mono primary, answer labels
#' in body text.
#' @keywords internal
#' @noRd
lss_table_question_paragraph <- function(row, lg, theme, show_help) {
  size_q  <- theme$size_question
  size_sq <- theme$size_subq
  size_h  <- theme$size_help

  plain <- function(size = size_q, color = theme$color_text,
                    italic = FALSE, bold = FALSE,
                    font = theme$font_body) {
    officer::fp_text(
      font.family = font, font.size = size, color = color,
      italic = italic, bold = bold
    )
  }

  br <- function() {
    flextable::as_chunk("\n", props = plain())
  }

  chunks <- list()
  add_text <- function(text, props) {
    if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return()
    chunks[[length(chunks) + 1L]] <<- flextable::as_chunk(text, props = props)
  }
  add_line <- function(text, props) {
    if (is.null(text) || is.na(text) || !nzchar(trimws(text))) return()
    if (length(chunks) > 0L) chunks[[length(chunks) + 1L]] <<- br()
    add_text(text, props)
  }

  # Question stem (parent for compound rows, leaf question for leaf
  # rows). For the Other item the customized "Other:" prompt
  # replaces the stem.
  if (identical(row$kind, "other")) {
    add_text(lss_table_other_prompt(row$other_q, lg), plain())
  } else {
    add_text(lss_html_to_text(row$parent_text[[lg]]), plain())
  }

  # Subquestion label below the stem (compound rows only), in
  # italic so the eye separates "what's being asked" (stem) from
  # "what this row narrows it to" (subq).
  if (identical(row$kind, "subq")) {
    add_line(lss_html_to_text(row$subq_text[[lg]]),
             plain(size = size_sq, italic = TRUE))
  }

  # Help (optional), small muted italic.
  if (isTRUE(show_help) && !identical(row$kind, "other")) {
    help_text <- lss_html_to_text(row$help[[lg]])
    if (!is.null(help_text) && !is.na(help_text) && nzchar(trimws(help_text))) {
      add_line(
        paste0("\u00AB ", help_text, " \u00BB"),
        plain(size = size_h, color = theme$color_muted, italic = TRUE)
      )
    }
  }

  if (length(chunks) == 0L) {
    chunks <- list(flextable::as_chunk(
      theme$empty_marker,
      props = plain(color = theme$color_muted)
    ))
  }
  do.call(flextable::as_paragraph, chunks)
}

#' Resolve the customized prompt of the LimeSurvey "Other:" input
#' for a language, or fall back to a generic "Other:" label.
#' @keywords internal
#' @noRd
lss_table_other_prompt <- function(q, lg) {
  if (is.null(q$attributes) || nrow(q$attributes) == 0L) return("Other:")
  attrs <- q$attributes[q$attributes$attribute == "other_replace_text", ,
                        drop = FALSE]
  if (nrow(attrs) == 0L) return("Other:")
  lang_hit <- attrs$value[attrs$language == lg]
  if (length(lang_hit) > 0L && nzchar(trimws(lang_hit[1]))) return(lang_hit[1])
  empty_lang <- attrs$value[!nzchar(attrs$language)]
  if (length(empty_lang) > 0L && nzchar(trimws(empty_lang[1]))) {
    return(empty_lang[1])
  }
  "Other:"
}

#' Implicit-coding text for a question with no enumerated answers.
#' Mirrors the language map of `lss_value_implicit_row()` but
#' returns the descriptor as a single string suitable for
#' embedding inside a Question cell.
#' @keywords internal
#' @noRd
lss_table_implicit_value_text <- function(q, theme) {
  chrome <- theme$chrome
  switch(
    q$type,
    "M" = chrome$value_multi_y_blank,
    "P" = chrome$value_multi_y_blank_with_comment,
    "Y" = chrome$value_yes_no,
    "G" = chrome$value_gender,
    "5" = chrome$value_5point,
    "N" = chrome$value_numeric_input,
    "K" = chrome$value_numeric_input,
    "S" = chrome$value_free_text_short,
    "T" = chrome$value_free_text,
    "U" = chrome$value_free_text_long,
    "D" = chrome$value_date_input,
    "*" = chrome$value_computed,
    "R" = chrome$value_ranking,
    "|" = chrome$value_file_upload,
    NULL
  )
}

#' Apply the visual polish (band, borders, widths, section-row
#' merge and dark band) to the codebook flextable.
#' @keywords internal
#' @noRd
lss_table_template_polish <- function(ft, theme, rows, n_lang) {
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  ft <- flextable::fontsize(ft, size = theme$size_meta, part = "body")
  ft <- flextable::fontsize(ft, size = theme$size_lang_header, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_white, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band_dark, part = "header")

  # Field column body: bold primary so "Question" reads as a row
  # label, with the same petrol-band header (empty text) as the rest
  # of the meta header.
  ft <- flextable::bold(ft, j = "Field", part = "body")
  ft <- flextable::color(ft, j = "Field", color = theme$color_primary,
                         part = "body")

  # Variable column: monospace, bold, primary color.
  ft <- flextable::font(ft, j = "Variable", fontname = theme$font_code,
                        part = "body")
  ft <- flextable::bold(ft, j = "Variable", part = "body")
  ft <- flextable::color(ft, j = "Variable", color = theme$color_primary,
                         part = "body")

  # Value column body: monospace bold primary for the codes that sit
  # in the dedicated value rows.
  ft <- flextable::font(ft, j = "Value", fontname = theme$font_code,
                        part = "body")
  ft <- flextable::bold(ft, j = "Value", part = "body")
  ft <- flextable::color(ft, j = "Value", color = theme$color_primary,
                         part = "body")

  # Alignment.
  ft <- flextable::align(ft, align = "left", part = "all")
  ft <- flextable::align(ft, j = "No", align = "right", part = "all")
  ft <- flextable::align(ft, j = "Mandatory", align = "center", part = "all")
  ft <- flextable::align(ft, j = "Value", align = "center", part = "all")

  # Borders: soft grid only, no per-row primary outline.
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  ft <- flextable::vline_left(ft, border = thin, part = "all")
  ft <- flextable::vline_right(ft, border = thin, part = "all")

  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding.top = 3, padding.bottom = 3,
                           padding.left = 4, padding.right = 4, part = "all")

  # Column widths. Calibrated so:
  # - Bold 8 pt header text does not wrap. "Obligatoire" / "Pflicht-
  #   feld" need ~1.0 in; Field stays narrow (~0.75 in for the
  #   chrome$item_question string, e.g. "Question" / "Frage" /
  #   "Pregunta").
  # - Variable column accommodates the longest `parent_subq` codes
  #   on a single line (11 pt Consolas).
  # - Total <= landscape A4 body width (~9.73 in with 2.5 cm side
  #   margins); the remainder splits evenly between language columns.
  meta_w <- 0.95 + 0.35 + 1.45 + 1.15 + 1.15 + 1.05 + 0.70
  total_w <- if (n_lang >= 2L) 9.73 else theme$content_width_in
  lang_w <- max((total_w - meta_w) / max(n_lang, 1L), 1.3)
  ft <- flextable::width(ft, j = "Field",     width = 0.95, unit = "in")
  ft <- flextable::width(ft, j = "No",        width = 0.35, unit = "in")
  ft <- flextable::width(ft, j = "Variable",  width = 1.45, unit = "in")
  ft <- flextable::width(ft, j = "Type",      width = 1.15, unit = "in")
  ft <- flextable::width(ft, j = "Mandatory", width = 1.15, unit = "in")
  ft <- flextable::width(ft, j = "Filter",    width = 1.05, unit = "in")
  ft <- flextable::width(ft, j = "Value",     width = 0.70, unit = "in")
  for (idx in seq_len(n_lang)) {
    ft <- flextable::width(ft, j = 7L + idx, width = lang_w, unit = "in")
  }

  # Row-type indices for selective styling.
  kinds <- vapply(rows, function(r) as.character(r$kind), character(1L))
  section_idx <- which(kinds == "section")
  question_idx <- which(kinds %in% c("leaf", "subq", "other"))
  scale_header_idx <- which(kinds == "scale_header")

  # Question rows: tinted band so the eye finds the next variable
  # at a glance. The tint sits between the white value rows and the
  # dark petrol section banners, keeping the visual hierarchy
  # section > question > value.
  for (qi in question_idx) {
    ft <- flextable::bg(ft, i = qi, bg = theme$color_band, part = "body")
  }
  # Scale-header rows (dual-scale arrays only) get the lighter
  # zebra tint so they read as "subsection within the values".
  for (sh in scale_header_idx) {
    ft <- flextable::bg(ft, i = sh, bg = theme$color_zebra, part = "body")
    ft <- flextable::bold(ft, i = sh, j = "Value", part = "body")
  }

  # Section rows: merge every column into one span so the petrol band
  # reads as a banner that physically separates the variable groups.
  total_cols <- flextable::ncol_keys(ft)
  for (si in section_idx) {
    ft <- flextable::merge_at(ft, i = si, j = seq_len(total_cols),
                              part = "body")
    ft <- flextable::bg(ft, i = si, bg = theme$color_primary, part = "body")
    ft <- flextable::color(ft, i = si, color = theme$color_white, part = "body")
    ft <- flextable::bold(ft, i = si, part = "body")
    ft <- flextable::fontsize(ft, i = si, size = theme$size_heading2,
                              part = "body")
    ft <- flextable::align(ft, i = si, align = "left", part = "body")
    ft <- flextable::padding(ft, i = si, padding.top = 6, padding.bottom = 6,
                             padding.left = 8, padding.right = 8,
                             part = "body")
    # Override the mono/primary that the body-wide rules set on the
    # Field / Variable / Value columns so the banner text reads as
    # a heading (white on petrol).
    ft <- flextable::font(ft, i = si, fontname = theme$font_body,
                          part = "body")
    ft <- flextable::color(ft, i = si, color = theme$color_white,
                           part = "body")
  }

  ft
}

# Helpers ----------------------------------------------------------

#' Replace `NULL` or empty strings with a fallback. Used inside the
#' rich-cell composer to avoid `as_chunk(NA)` or `as_chunk(NULL)`
#' which flextable rejects.
#' @keywords internal
#' @noRd
`%||_%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || is.na(a) || !nzchar(a)) b else a
}

# lss_html_to_text() is defined in R/html.R; the codebook template
# relies on the canonical implementation to keep stem / subq /
# answer-label text identical to what the cards template renders.

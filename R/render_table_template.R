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
  meta_cols <- c("No", "Variable", "Type", "Mandatory", "Filter", "Value")
  lang_cols <- paste0("Q_", langs)
  all_cols <- c(meta_cols, lang_cols)

  # ---- Build the data frame skeleton ------------------------------
  # Every cell starts blank; the rich Question content is composed
  # below with flextable::compose so we can mix sizes and styles.
  df <- as.data.frame(
    matrix("", nrow = length(rows), ncol = length(all_cols)),
    stringsAsFactors = FALSE
  )
  names(df) <- all_cols

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (identical(r$kind, "section")) {
      # The section row text lives in the first column (No) so that
      # `merge_at()` below keeps it visible after the row span is
      # collapsed to a single cell.
      df$No[i] <- r$text
    } else {
      df$No[i]        <- as.character(r$no)
      df$Variable[i]  <- r$variable
      df$Type[i]      <- r$type_label
      df$Mandatory[i] <- r$mandatory_label
      # Filter cell composed below (human-readable on top, raw below).
    }
  }

  ft <- flextable::flextable(df)
  ft <- flextable::set_header_labels(
    ft, values = stats::setNames(
      as.list(c(
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

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (identical(r$kind, "section")) next

    # Filter cell: humanized form on top, raw expression below in
    # small italic mono. Same convention as the cards meta table.
    ft <- flextable::compose(
      ft, i = i, j = "Filter",
      value = lss_table_filter_paragraph(
        r$relevance, theme,
        plain_props = filter_plain_props,
        raw_props = filter_raw_props,
        show_raw = isTRUE(state$show_raw_filter)
      )
    )

    # Value cell: compact summary of the response domain. Codes in
    # mono primary ("1-5", "Y/blank"); descriptors for non-enumerated
    # types in italic muted ("[num]", "[text]", "[date]"). Empty for
    # section rows and for the standalone Other item.
    ft <- flextable::compose(
      ft, i = i, j = "Value",
      value = lss_table_value_paragraph(
        r, theme,
        code_props = value_code_props,
        descriptor_props = value_descriptor_props
      )
    )

    # Question cells, one per content language.
    for (lg in langs) {
      ft <- flextable::compose(
        ft, i = i, j = paste0("Q_", lg),
        value = lss_table_question_paragraph(r, lg, theme,
                                             show_help = show_help)
      )
    }
  }

  # ---- Section rows: merge horizontally + dark band --------------
  section_idx <- which(vapply(rows, function(r) identical(r$kind, "section"),
                              logical(1L)))
  ft <- lss_table_template_polish(ft, theme, section_idx,
                                   n_lang = length(langs))

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

  for (q in g$questions) {
      info <- lss_type_info(q$type)
      if (isTRUE(info$has_subquestions) && length(q$subquestions) > 0L) {
        for (sq in q$subquestions) {
          state$item_no <- state$item_no + 1L
          item_code <- paste0(q$code, "_", sq$code)
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = state$item_no
          )
          rows[[length(rows) + 1L]] <- list(
            kind = "subq",
            no = state$item_no,
            variable = item_code,
            type_label = lss_localized_type_label(q, theme),
            mandatory_label = lss_yes_no(q$mandatory, theme),
            relevance = q$relevance,
            parent_text = stats::setNames(
              lapply(langs, function(lg) q$texts[[lg]]$question), langs
            ),
            subq_text = stats::setNames(
              lapply(langs, function(lg) sq$texts[[lg]]$question), langs
            ),
            help = stats::setNames(
              lapply(langs, function(lg) q$texts[[lg]]$help), langs
            ),
            q = q, sq = sq
          )
        }
        if (identical(q$other, "Y")) {
          # Standalone Other item, as in the cards template.
          state$item_no <- state$item_no + 1L
          item_code <- paste0(q$code, "_other")
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = state$item_no
          )
          rows[[length(rows) + 1L]] <- list(
            kind = "other",
            no = state$item_no,
            variable = item_code,
            type_label = chrome$type_text_other,
            mandatory_label = lss_yes_no("N", theme),
            relevance = q$relevance,
            other_q = q
          )
        }
      } else {
        state$item_no <- state$item_no + 1L
        state$index_entries[[length(state$index_entries) + 1L]] <- list(
          code = q$code, no = state$item_no
        )
        rows[[length(rows) + 1L]] <- list(
          kind = "leaf",
          no = state$item_no,
          variable = q$code,
          type_label = lss_localized_type_label(q, theme),
          mandatory_label = lss_yes_no(q$mandatory, theme),
          relevance = q$relevance,
          parent_text = stats::setNames(
            lapply(langs, function(lg) q$texts[[lg]]$question), langs
          ),
          help = stats::setNames(
            lapply(langs, function(lg) q$texts[[lg]]$help), langs
          ),
          q = q
        )
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
  if (identical(row$kind, "section") || identical(row$kind, "other")) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  q <- row$q

  # Enumerated answers (single or dual scale).
  if (length(q$answers) > 0L) {
    multi_scale <- !is.null(q$scales) && length(q$scales) > 1L
    if (multi_scale) {
      chunks <- list()
      for (si in seq_along(q$scales)) {
        ans <- q$scales[[si]]
        if (length(ans) == 0L) next
        if (length(chunks) > 0L) {
          chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
            "\n", props = descriptor_props
          )
        }
        chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
          sprintf("S%d: %s", si, lss_table_value_codes(ans)),
          props = code_props
        )
      }
      return(do.call(flextable::as_paragraph, chunks))
    }
    return(flextable::as_paragraph(flextable::as_chunk(
      lss_table_value_codes(q$answers), props = code_props
    )))
  }

  # Implicit codings: short mono token.
  short_code <- switch(
    q$type,
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

  # Open-ended / non-coded types: italic muted descriptor.
  descriptor <- switch(
    q$type,
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
  size_a  <- theme$size_answer
  size_m  <- theme$size_meta

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

  # 1. Question stem (parent for compound rows, leaf question for leaf
  # rows). For the Other item we use the customized "Other:" prompt.
  if (identical(row$kind, "other")) {
    text <- lss_table_other_prompt(row$other_q, lg)
    add_text(text, plain())
  } else {
    text <- lss_html_to_text(row$parent_text[[lg]])
    add_text(text, plain())
  }

  # 2. Subquestion label (compound only).
  if (identical(row$kind, "subq")) {
    sq_text <- lss_html_to_text(row$subq_text[[lg]])
    add_line(sq_text, plain(size = size_sq, italic = TRUE))
  }

  # 3. Help (optional).
  if (isTRUE(show_help) && !identical(row$kind, "other")) {
    help_text <- lss_html_to_text(row$help[[lg]])
    if (!is.null(help_text) && !is.na(help_text) && nzchar(trimws(help_text))) {
      if (length(chunks) > 0L) chunks[[length(chunks) + 1L]] <- br()
      add_text(paste0("\u00AB ", help_text, " \u00BB"),
               plain(size = size_h, color = theme$color_muted, italic = TRUE))
    }
  }

  # 4. Response modalities: enumerated codes (q$answers) or the
  # implicit-coding descriptor. Each code on its own line, code in
  # mono primary + label in body. Skips for the Other item (the
  # value is the free-text input itself).
  if (!identical(row$kind, "other")) {
    if (length(row$q$answers) > 0L) {
      multi_scale <- !is.null(row$q$scales) && length(row$q$scales) > 1L
      bundles <- if (multi_scale) row$q$scales else list(row$q$answers)
      for (si in seq_along(bundles)) {
        ans <- bundles[[si]]
        if (length(ans) == 0L) next
        if (multi_scale) {
          # Scale separator label inside the same cell.
          if (length(chunks) > 0L) chunks[[length(chunks) + 1L]] <- br()
          add_text(
            sprintf(theme$chrome$item_value_scale_fmt, si),
            plain(size = size_m, color = theme$color_primary, bold = TRUE)
          )
        }
        for (a in ans) {
          if (length(chunks) > 0L) chunks[[length(chunks) + 1L]] <- br()
          chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
            sprintf("%s = ", a$code),
            props = plain(size = size_a, color = theme$color_primary,
                          bold = TRUE, font = theme$font_code)
          )
          chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
            lss_html_to_text(a$labels[[lg]]) %||_% "",
            props = plain(size = size_a)
          )
        }
      }
    } else {
      implicit <- lss_table_implicit_value_text(row$q, theme)
      if (!is.null(implicit)) {
        if (length(chunks) > 0L) chunks[[length(chunks) + 1L]] <- br()
        add_text(implicit,
                 plain(size = size_a, color = theme$color_muted, italic = TRUE))
      }
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
lss_table_template_polish <- function(ft, theme, section_idx, n_lang) {
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  ft <- flextable::fontsize(ft, size = theme$size_meta, part = "body")
  ft <- flextable::fontsize(ft, size = theme$size_lang_header, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_white, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band_dark, part = "header")

  # Variable column: monospace, bold, primary color.
  ft <- flextable::font(ft, j = "Variable", fontname = theme$font_code,
                        part = "body")
  ft <- flextable::bold(ft, j = "Variable", part = "body")
  ft <- flextable::color(ft, j = "Variable", color = theme$color_primary,
                         part = "body")

  # Alignment: No right, Mandatory and Value center, rest left.
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
  # - Bold 8 pt header text does not wrap. "Obligatoire" (FR, 11 ch)
  #   and "Pflichtfeld" (DE, 11 ch) both need ~1.0 in at 8 pt bold
  #   with padding; we give Mandatory 1.05 in for safety.
  # - "Valeur" / "Variable" / "Filtre" headers fit with margin.
  # - Variable column wide enough for the 20-char `parent_subq`
  #   identifiers (11 pt Consolas ~ 0.092 in/char => ~1.85 in;
  #   1.70 in accepts up to ~18 chars before wrap, an acceptable
  #   compromise for the dense codebook layout).
  # - The total stays at or below the landscape A4 body width
  #   (~9.73 in with 2.5 cm side margins); whatever remains splits
  #   evenly between language columns.
  meta_w <- 0.40 + 1.70 + 1.15 + 1.05 + 1.20 + 0.85
  total_w <- if (n_lang >= 2L) 9.73 else theme$content_width_in
  lang_w <- max((total_w - meta_w) / max(n_lang, 1L), 1.4)
  ft <- flextable::width(ft, j = "No",        width = 0.40, unit = "in")
  ft <- flextable::width(ft, j = "Variable",  width = 1.70, unit = "in")
  ft <- flextable::width(ft, j = "Type",      width = 1.15, unit = "in")
  ft <- flextable::width(ft, j = "Mandatory", width = 1.05, unit = "in")
  ft <- flextable::width(ft, j = "Filter",    width = 1.20, unit = "in")
  ft <- flextable::width(ft, j = "Value",     width = 0.85, unit = "in")
  for (idx in seq_len(n_lang)) {
    ft <- flextable::width(ft, j = 6L + idx, width = lang_w, unit = "in")
  }

  # Section rows: merge every column into one span so the petrol band
  # reads as a banner that physically separates the variable groups.
  # `merge_at()` is explicit about the cell range to merge -- unlike
  # `merge_h()` which only collapses cells that share an identical
  # value. The text lives in the first column (No) by construction
  # in the row builder.
  n_cols <- length(all_cols_for_merge <- NULL)  # placeholder; we read ncol(ft)
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
    # Variable column so the bandeau text reads as a heading
    # (white on petrol) rather than a code identifier.
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

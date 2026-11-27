#' Render the survey content as a single dense table
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
  # Table layout: every variable produces one tinted Question row
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
    if (identical(r$kind, "description")) {
      df$Field[i] <- chrome$description_title
    } else if (identical(r$kind, "welcome")) {
      df$Field[i] <- chrome$welcome_text_title
    } else if (identical(r$kind, "endtext")) {
      df$Field[i] <- chrome$end_text_title
    } else if (identical(r$kind, "group")) {
      df$Field[i] <- chrome$item_group
      # Group name composed below per language via flextable::compose.
    } else if (identical(r$kind, "group_description")) {
      # Field stays blank: the row reads as the group banner's intro
      # text. The localized description is composed below.
    } else if (identical(r$kind, "group_filter")) {
      # Field stays blank: the group's condition is composed below,
      # across the language columns.
    } else if (identical(r$kind, "scale_header")) {
      # Dual-scale separator: announce "Value (scale N)" on the
      # Value column. Lang cells stay empty.
      df$Value[i] <- r$text
    } else if (identical(r$kind, "value")) {
      df$Value[i] <- as.character(r$code)
      # Labels per language composed via flextable::compose() below.
    } else if (identical(r$kind, "mc_option")) {
      # Multiple-choice option (or ranking position): its sub-item number
      # (`28.1`) in the No column, the option's full variable code in the
      # Variable column and its binary coding token in the Value column,
      # so the row is a self-contained dictionary entry. The option
      # label per language is composed below.
      df$No[i]       <- lss_table_no_cell(r$no)
      df$Variable[i] <- r$variable
      df$Value[i]    <- r$value_token
    } else if (identical(r$kind, "mc_exclusive")) {
      # Exclusive-option note: a localized sentence in the Field column,
      # spanning nothing else; language cells stay empty.
      df$Field[i] <- chrome$item_exclusive
    } else if (identical(r$kind, "order_note")) {
      # Answer / option order note: a chrome annotation spanning the
      # language columns (composed below), under a "Value" / "Options"
      # Field label carried on the row.
      df$Field[i] <- r$field_label
    } else if (identical(r$kind, "attr")) {
      # An attribute of `show_attrs` (prefix, suffix, the formula of a
      # computed question, ...): its label in the Field column, its text
      # composed below in the language columns; the meta, Filter and Value
      # cells stay empty.
      df$Field[i] <- r$label
    } else {
      # Question row (leaf / subq / other); the comment field of a list
      # with comment carries its own label ("Comment").
      df$Field[i]     <- if (!is.null(r$field)) r$field else chrome$item_question
      df$No[i]        <- lss_table_no_cell(r$no)
      df$Variable[i]  <- r$variable
      df$Type[i]      <- r$type_label
      df$Mandatory[i] <- r$mandatory_label
      # Filter and Value cells composed below.
    }
  }

  ft <- flextable::flextable(df)
  # In the dense table template the Mandatory header uses the
  # abbreviated chrome string (`Mand.` / `Oblig.` / `Pflicht` /
  # `Obblig.`) so the column can stay narrow and the question
  # languages get >=50% of the page width. The cards template keeps
  # the full localized word.
  ft <- flextable::set_header_labels(
    ft, values = stats::setNames(
      as.list(c(
        chrome$item_field,
        chrome$meta_no, chrome$meta_variable, chrome$meta_type,
        chrome$meta_mandatory_short, chrome$meta_filter, chrome$item_value,
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
  # Raw expression in the monospace face, muted + italic so it reads as
  # secondary to the humanized form above it. Held at size_meta - 1 (the
  # legibility floor for a code expression in this dense table); Consolas
  # already looks a touch larger than Calibri at an equal size, so this
  # reads about level with the body rather than larger.
  filter_raw_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_meta - 1L,
    color = theme$color_muted, italic = TRUE
  )

  # The dense table uses ONE body size everywhere (question stems,
  # option / answer labels, welcome / end text, the value descriptors and
  # the chrome annotation rows), matching the meta and value cells -- a
  # uniform table reads as a data dictionary rather than prose. The size
  # auto-reduces by 1 pt from three languages on, the same rule
  # lss_table_template_polish() applies to the meta/value columns, so
  # composed and non-composed cells stay in lockstep.
  body_size <- if (length(langs) >= 3L) theme$size_meta - 1L else theme$size_meta

  # Codes in the dense table use the body font (not the monospace
  # font): Consolas is wide and pushes the long variable names to wrap,
  # eating width the language columns need. Bold + primary still mark
  # these cells as codes. (The cards template keeps the monospace face,
  # where width is not as constrained.)
  value_code_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_primary
  )
  # Value-cell descriptors ([num], [text], ...) and the chrome annotation
  # rows (exclusive note, answer/option order note) share the body size so
  # they never read a point larger than the surrounding cells, including
  # when the table steps down to 7 pt for three or four languages.
  value_descriptor_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_muted, italic = TRUE
  )

  value_label_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_text
  )
  # The formula of a computed question: the code font, upright.
  equation_props <- officer::fp_text(
    font.family = theme$font_code, font.size = body_size,
    color = theme$color_text
  )
  empty_marker_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_muted
  )

  plain_lang_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_text
  )
  # Group name sits at the table body size (not a heading size): the
  # bold primary colour and the saturated group banner already mark it
  # as a section divider, so a larger size would just break the even
  # rhythm of the dense table rows.
  group_name_props <- officer::fp_text(
    font.family = theme$font_body, font.size = body_size,
    color = theme$color_primary, bold = TRUE
  )
  # The label of a level of condition (Group, Question, Subquestion) in a
  # Filter cell, muted so the expression stays the anchor.
  filter_label_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_meta,
    color = theme$color_muted
  )

  for (i in seq_along(rows)) {
    r <- rows[[i]]
    kind <- r$kind
    if (identical(kind, "scale_header")) {
      # Scale header carries text already in df$Value -- no rich
      # composition. Polish will tint the row.
      next
    }

    if (identical(kind, "welcome") || identical(kind, "endtext") ||
        identical(kind, "description") || identical(kind, "group_description")) {
      # Welcome / End text / group intro: full HTML content per language
      # via the same lss_compose() helper the cards template uses
      # for the side-by-side block, so paragraphs and formatting
      # are preserved.
      for (lg in langs) {
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = lss_compose(r$text_by_lang[[lg]], theme,
                              size = body_size)
        )
      }
      next
    }

    if (identical(kind, "group_filter")) {
      # The group's condition, `Filter: <plain>`, upright, the label
      # muted, at the body size; the raw form follows when asked. The
      # same chrome sentence in every language column; polish merges them.
      ln <- lss_filter_lines(NA_character_, r$relevance, NULL, theme)[[1L]]
      ln$label <- chrome$meta_filter
      gf_props <- officer::fp_text(
        font.family = theme$font_body, font.size = body_size,
        color = theme$color_text
      )
      gf_label_props <- officer::fp_text(
        font.family = theme$font_body, font.size = body_size,
        color = theme$color_muted
      )
      chunks <- lss_filter_chunks(
        list(ln), theme, plain_props = gf_props, raw_props = filter_raw_props,
        label_props = gf_label_props, show_raw = isTRUE(state$show_raw_filter)
      )
      for (lg in langs) {
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = do.call(flextable::as_paragraph, chunks)
        )
      }
      next
    }

    if (identical(kind, "group")) {
      # Group row: localized name in bold primary per language, the
      # muted empty marker where a language has none.
      for (lg in langs) {
        name <- r$name_by_lang[[lg]]
        chunk <- if (is.null(name) || is.na(name) || !nzchar(name)) {
          flextable::as_chunk(theme$empty_marker, props = empty_marker_props)
        } else {
          flextable::as_chunk(name, props = group_name_props)
        }
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = flextable::as_paragraph(chunk)
        )
      }
      next
    }

    if (identical(kind, "value") || identical(kind, "mc_option")) {
      # Value row (enumerated answer code) OR multiple-choice option
      # row: the language columns carry the answer / option label, its
      # formatting kept (lss_compose_chunks()). `df` already holds the code
      # (Value column) or the full option variable name (Variable column).
      # Empty cells fall back to the muted em-dash.
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
            value = do.call(flextable::as_paragraph, lss_compose_chunks(
              r$labels[[lg]], value_label_props
            ))
          )
        }
      }
      # An option of a multiple choice with a condition of its own prints
      # that one labelled line in its Filter cell; without one the cell
      # stays empty (the parent row carries the group and question lines).
      filter_lines <- lss_table_filter_row_lines(r, theme)
      if (!is.null(filter_lines)) {
        ft <- flextable::compose(
          ft, i = i, j = "Filter",
          value = lss_table_filter_paragraph(
            NA_character_, theme,
            plain_props = filter_plain_props,
            raw_props = filter_raw_props,
            show_raw = isTRUE(state$show_raw_filter),
            label_props = filter_label_props,
            lines = filter_lines
          )
        )
      }
      next
    }

    if (identical(kind, "attr")) {
      # An attribute row: the text of each language at the body size,
      # muted italic like the other annotation rows, its formatting kept
      # (lss_compose_chunks(): an italic of the author set upright), the
      # empty marker where a language has none. A raw text (the formula of
      # a computed question) is printed as written, in the code font, never
      # read as HTML. Polish merges the language columns when the text is
      # the same in all of them.
      is_formula <- isTRUE(r$raw)
      for (lg in langs) {
        text <- r$texts[[lg]]
        chunks <- if (!is_formula) {
          lss_compose_chunks(text, value_descriptor_props)
        } else if (nzchar(text)) {
          list(flextable::as_chunk(text, props = equation_props))
        }
        if (length(chunks) == 0L) {
          chunks <- list(
            flextable::as_chunk(theme$empty_marker, props = empty_marker_props)
          )
        }
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = do.call(flextable::as_paragraph, chunks)
        )
      }
      next
    }

    if (identical(kind, "mc_exclusive") || identical(kind, "order_note")) {
      # Chrome annotation rows (exclusive-option note; answer / option
      # order note): the localized sentence in each language column,
      # muted italic, mirroring the cards layout. The Field column
      # already carries the row label ("Exclusive" / "Value" / "Options").
      # Polish merges the language columns so it shows once.
      for (lg in langs) {
        ft <- flextable::compose(
          ft, i = i, j = paste0("Q_", lg),
          value = flextable::as_paragraph(flextable::as_chunk(
            r$text, props = value_descriptor_props
          ))
        )
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
        show_raw = isTRUE(state$show_raw_filter),
        label_props = filter_label_props,
        lines = lss_table_filter_row_lines(r, theme)
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
                                             show_help = show_help,
                                             body_size = body_size)
      )
    }
  }

  # Polish applies row-type-aware styling (section merge + petrol
  # band, Q-row tint, scale_header tint, widths, etc.).
  ft <- lss_table_template_polish(ft, theme, rows, n_lang = length(langs))

  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  lss_table_bookmark_groups(doc, rows)
}

#' Bookmark the group rows of the table just inserted
#'
#' The table of contents links each group to `lss_group_bookmark(<index>)`.
#' flextable has no bookmark chunk, so the bookmark goes into the XML of
#' the table at the cursor: in the first paragraph of each body row of kind
#' `"group"` (header rows, which carry `w:tblHeader`, are skipped), in the
#' form `officer::body_bookmark()` writes. The id derives from the name,
#' which is unique in the document, so it cannot collide with the uuids
#' officer gives its own bookmarks.
#'
#' @keywords internal
#' @noRd
lss_table_bookmark_groups <- function(doc, rows) {
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  tbl <- officer::docx_current_block_xml(doc)
  trs <- xml2::xml_find_all(tbl, "w:tr[not(w:trPr/w:tblHeader)]", ns)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (!identical(r$kind, "group") || is.null(r$group_index)) next
    p <- xml2::xml_find_first(trs[[i]], ".//w:p", ns)
    name <- lss_group_bookmark(r$group_index)
    start <- xml2::read_xml(sprintf(
      "<w:bookmarkStart xmlns:w=\"%s\" w:id=\"%s\" w:name=\"%s\"/>",
      ns[["w"]], name, name
    ))
    end <- xml2::read_xml(sprintf(
      "<w:bookmarkEnd xmlns:w=\"%s\" w:id=\"%s\"/>", ns[["w"]], name
    ))
    # After the paragraph properties, when there are some.
    xml2::xml_add_child(p, start,
                        .where = length(xml2::xml_find_all(p, "w:pPr", ns)))
    xml2::xml_add_child(p, end)
  }
  doc
}

#' Build the Welcome / End text row for the table, when
#' the survey has non-empty content in at least one displayed
#' language. Returns `NULL` if all languages are empty.
#'
#' `field` is the `survey_language_settings` column (typically
#' `surveyls_welcometext` or `surveyls_endtext`); `kind` is the row
#' marker ("welcome" / "endtext") used by the rendering loop.
#' @keywords internal
#' @noRd
lss_table_text_row <- function(lss, langs, field, kind) {
  ls <- lss$survey_language_settings
  if (is.null(ls) || nrow(ls) == 0L) return(NULL)
  vals <- vapply(langs, function(lg) {
    v <- ls[[field]][ls$surveyls_language == lg]
    if (length(v) == 0L) NA_character_ else v[1]
  }, character(1))
  if (!any(!is.na(vals) & nzchar(trimws(vals)))) return(NULL)
  list(
    kind = kind,
    text_by_lang = stats::setNames(as.list(vals), langs)
  )
}

#' Walk the model and produce the flat list of rows that will become
#' the table -- alternating "section" markers (group
#' banners) and "item" rows (one per variable).
#'
#' Each "item" row is normalized in this builder so the main
#' renderer never has to look at the question structure again:
#' `no`, `variable` (the LimeSurvey data column, `parent_subq` for
#' compound subqs), `type_label`, `mandatory_label`, `relevance`,
#' `group_relevance`, `subq_relevance` (the condition of the
#' subquestion(s) a row stands for), `hidden`, plus pre-extracted texts
#' per language (`parent_text`, `subq_text`, `help`) and the
#' question/subquestion model objects so
#' the cell composer can pull the answer scale or the implicit-coding
#' descriptor at render time.
#'
#' `no` is the number label read from `state$numbers` (see
#' `lss_item_label()`): the question's number on a leaf row, on the
#' parent row of a multiple choice or a ranking and on an "Other" or
#' comment row; `<question>.<k>` on the k-th row standing for one
#' subquestion (array row, option, rank position); an en dash throughout
#' an unnumbered question.
#'
#' The group row holds the name of the group in each language as the
#' author wrote it, with no number: `NA` (printed as the empty marker)
#' for a language without a name when another language has one,
#' "Group <gid>" in every column when none has one.
#'
#' Also advances `state$group_index`, which names the bookmark of the
#' group row (`lss_group_bookmark()`), the target of the table of
#' contents. The progress bar is updated at each group boundary.
#'
#' @keywords internal
#' @noRd
lss_table_template_rows_for_group <- function(g, langs, theme,
                                              show_help, state,
                                              show_groups = TRUE,
                                              show_group_description = TRUE) {
  chrome <- theme$chrome
  rows <- list()
  # The group index advances even when the row itself is hidden, so
  # that the bookmarks of the groups keep the count of the cards.
  state$group_index <- state$group_index + 1L
  if (isTRUE(show_groups)) {
    # Group row: Field = "Group" label, language columns hold the
    # name as written, NA (the empty marker) where a language has none,
    # "Group <gid>" in every column when no language has one.
    group_names <- stats::setNames(
      lapply(langs, function(lg) {
        raw <- g$names[[lg]]
        if (lss_any_present(list(raw))) raw else NA_character_
      }),
      langs
    )
    if (!lss_any_present(group_names)) {
      group_names[] <- list(lss_group_title(g, langs))
    }
    rows[[length(rows) + 1L]] <- list(
      kind = "group",
      name_by_lang = group_names,
      group_index = state$group_index
    )
    # The group's condition, when it has one: a row right under the
    # banner, before the description, drawn like the description row.
    if (lss_has_condition(g$relevance)) {
      rows[[length(rows) + 1L]] <- list(
        kind = "group_filter",
        relevance = g$relevance
      )
    }
    # Group description (when the author wrote one): a row right under
    # the banner carrying the localized intro text, mirroring what the
    # cards template renders below the group title. Skipped when every
    # language is blank, so a group without a description adds no row.
    desc_vals <- vapply(langs, function(lg) {
      v <- if (!is.null(g$descriptions[[lg]])) g$descriptions[[lg]] else NA_character_
      if (is.null(v) || is.na(v)) NA_character_ else as.character(v)
    }, character(1))
    if (isTRUE(show_group_description) &&
        any(!is.na(desc_vals) & nzchar(trimws(desc_vals)))) {
      rows[[length(rows) + 1L]] <- list(
        kind = "group_description",
        text_by_lang = stats::setNames(as.list(desc_vals), langs)
      )
    }
  }

  # Build one Question row per variable (carrying the meta and the
  # question/subq/help text) followed by N Value rows (one per
  # enumerated answer code, each carrying its label per language).
  # Variables with no enumerated answers produce only the Question
  # row -- the implicit-coding descriptor sits in their Value cell.
  emit_question_row <- function(kind, no, variable, q, sq = NULL,
                                other_q = NULL) {
    list(
      kind = kind, no = no, variable = variable,
      hidden = lss_question_hidden(q$attributes),
      type_label = if (identical(kind, "other")) chrome$type_text_other
                   else lss_localized_type_label(q, theme),
      mandatory_label = lss_yes_no(
        if (identical(kind, "other")) "N" else q$mandatory, theme
      ),
      relevance = q$relevance,
      group_relevance = q$group_relevance,
      # A row that stands for one subquestion carries its condition too
      # (a cell of a two-dimensional array adds its column's below).
      subq_relevance = if (!is.null(sq)) sq$relevance else NULL,
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
    # Predefined labelled scales (C/E/Y/G) store no answers in the .lss but
    # carry fixed localizable codes -- list them like a stored scale so the
    # dense table documents the value domain instead of leaving it blank.
    # Each language column takes the labels in its own language, or in the
    # chrome language when lssdoc has no strings for it.
    if (length(q$answers) == 0L) {
      labelled <- lss_predefined_labelled(q$type)
      if (is.null(labelled)) return(list())
      packs <- lapply(langs, lss_builtin_strings, chrome = chrome)
      out <- list()
      for (cl in labelled) {
        out[[length(out) + 1L]] <- list(
          kind = "value",
          code = cl[1],
          labels = stats::setNames(
            lapply(packs, function(p) p[[cl[2]]]), langs
          )
        )
      }
      return(out)
    }
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

  # Value rows for a plain answer list (one scale of a dual-scale array),
  # without the "Value (scale N)" separators that emit_value_rows_for adds
  # for the combined view -- here each scale is its own single-choice row.
  emit_values <- function(ans) {
    out <- list()
    for (a in ans) {
      out[[length(out) + 1L]] <- list(
        kind = "value",
        code = a$code,
        labels = stats::setNames(
          lapply(langs, function(lg) a$labels[[lg]]), langs
        )
      )
    }
    out
  }

  # The attribute rows of `x` (a question or a subquestion): the rows
  # lss_attr_rows() gives the cards for the attributes of `show_attrs`, in
  # the same order, each with the name of its attribute, `raw` (TRUE for an
  # expression, the formula of a computed question, drawn as written in the
  # code font) and `merge`, TRUE when its text is the same in every
  # language.
  attr_rows <- function(x) {
    out <- list()
    for (a in unique(state$show_attrs)) {
      for (ar in lss_attr_rows(x, langs, theme, a)) {
        out[[length(out) + 1L]] <- list(
          kind = "attr", attribute = a, label = ar$label, texts = ar$texts,
          raw = isTRUE(ar$raw),
          merge = length(unique(unlist(ar$texts))) == 1L
        )
      }
    }
    out
  }

  # The "Exclusive" row of a row that stands for subquestion `sq`, where
  # the card of that subquestion prints it (lss_exclusive_text()); an empty
  # list otherwise.
  exclusive_rows <- function(q, sq) {
    text <- lss_exclusive_text(q, sq, chrome)
    if (is.null(text)) return(list())
    list(list(kind = "mc_exclusive", text = text))
  }

  # An answer / option order annotation row, or NULL when the order is
  # the (silent) default. `attr` is answer_order (leaf single choice) or
  # subquestion_order (multiple choice); `field_label` is the row label
  # shown in the Field column ("Value" / "Options").
  order_note_row <- function(q, attr, field_label) {
    note <- lss_answer_order_note(q, theme, attr = attr)
    if (is.null(note)) return(NULL)
    list(kind = "order_note", text = note, field_label = field_label)
  }

  # The row of a comment variable, registered in the variable index under
  # `no`: a short free text (its Value cell takes the token of type S),
  # with the label "Comment" in the Field column, as on the card of the
  # comment of a list, never mandatory (a comment field is not, whatever
  # the question says, as the card of the comment and the "Other" row
  # print), and without the help of the question. The language
  # cells hold `texts`, by default the stem (the comment of a list with
  # comment); the comment of an option of a multiple choice with comments
  # passes the label of the option, the comment of its "Other" the prompt.
  # `sq`, the option, adds its condition to the Filter cell.
  emit_comment_row <- function(q, no, variable, texts = NULL, sq = NULL) {
    state$index_entries[[length(state$index_entries) + 1L]] <- list(
      code = variable, no = no
    )
    cr <- emit_question_row("leaf", no, variable, q = q, sq = sq)
    cr$q$type <- "S"
    cr$q$answers <- list()
    cr$type_label <- chrome$type_text_short
    cr$mandatory_label <- lss_yes_no("N", theme)
    cr$field <- chrome$item_comment
    cr$help <- NULL
    if (!is.null(texts)) cr$parent_text <- texts
    cr
  }

  # One grouped block for a multiple-choice question: a parent
  # Question row (stem once, Variable = parent_*, the implicit
  # "Y/blank" coding in the Value cell) followed by one row per option
  # carrying that option's FULL variable code -- so the dense table
  # keeps every exported variable on its own findable row -- with the
  # option label in the language columns. The parent row carries the
  # question's number (`28`), each option row its sub-item number
  # (`28.1`, `28.2`, ...), and each option variable is registered in the
  # variable index under that sub-item number. With comments (type P),
  # each option row and the "Other" row are followed by the row of their
  # comment variable, at the same number.
  emit_multiple_choice <- function(q) {
    comments <- identical(q$type, "P")
    out <- list()
    parent_no <- lss_item_label(state$numbers, q$qid)
    # The parent row is a pure question header: stem, type, filter, but
    # NO variable code and NO value. In this dense table every option
    # row below is a real variable that carries its own name AND its own
    # value domain, so the header itself documents neither -- it is not a
    # data column. `mc_parent = TRUE` tells the renderer to leave its
    # Value cell blank (lss_table_value_paragraph would otherwise emit
    # the type's implicit token here).
    prow <- emit_question_row("leaf", parent_no, "", q = q)
    prow$mc_parent <- TRUE
    out[[length(out) + 1L]] <- prow
    out <- c(out, attr_rows(q))
    # Each option's binary coding token (Y = selected, blank = not).
    # Documented per option, in the Value column, so every variable row
    # is a self-contained dictionary entry (name | value domain | label).
    value_token <- "Y/blank"
    for (k in seq_along(q$subquestions)) {
      sq <- q$subquestions[[k]]
      item_code <- lss_variable_name(q$code, sq$code,
                                     style = theme$variable_names)
      option_no <- lss_item_label(state$numbers, q$qid, k)
      state$index_entries[[length(state$index_entries) + 1L]] <- list(
        code = item_code, no = option_no
      )
      labels <- stats::setNames(
        lapply(langs, function(lg) sq$texts[[lg]]$question), langs
      )
      out[[length(out) + 1L]] <- list(
        kind = "mc_option",
        no = option_no,
        variable = item_code,
        value_token = value_token,
        relevance = sq$relevance,
        labels = labels
      )
      if (comments) {
        out[[length(out) + 1L]] <- emit_comment_row(
          q, option_no, lss_comment_variable(q, sq$code, theme$variable_names),
          texts = labels, sq = sq
        )
      }
    }
    excl <- lss_exclusive_options_text(q, chrome)
    if (!is.null(excl)) {
      out[[length(out) + 1L]] <- list(kind = "mc_exclusive", text = excl)
    }
    onr <- order_note_row(q, "subquestion_order", chrome$item_order)
    if (!is.null(onr)) out[[length(out) + 1L]] <- onr
    if (identical(q$other, "Y")) {
      item_code <- lss_other_variable(q, theme$variable_names)
      state$index_entries[[length(state$index_entries) + 1L]] <- list(
        code = item_code, no = parent_no
      )
      out[[length(out) + 1L]] <- emit_question_row(
        "other", parent_no, item_code, q = q, other_q = q
      )
      if (comments) {
        out[[length(out) + 1L]] <- emit_comment_row(
          q, parent_no, lss_comment_variable(q, "other", theme$variable_names),
          texts = stats::setNames(
            lapply(langs, function(lg) lss_other_prompt(q, lg, chrome)), langs
          )
        )
      }
    }
    out
  }

  # One grouped block for a ranking question: a parent stem row (the
  # question's number), one position column per item (named by the
  # item's answer id, the CSV form `code[aid]`, carrying its rank and its
  # sub-item number `28.1`, `28.2`, ...), then the rankable items as value
  # rows (the codes that can appear as a cell's value).
  emit_ranking <- function(q) {
    out <- list()
    parent_no <- lss_item_label(state$numbers, q$qid)
    prow <- emit_question_row("leaf", parent_no, "", q = q)
    prow$mc_parent <- TRUE
    out[[length(out) + 1L]] <- prow
    out <- c(out, attr_rows(q))
    for (i in seq_along(q$answers)) {
      a <- q$answers[[i]]
      item_code <- lss_variable_name(q$code, a$aid, style = theme$variable_names)
      position_no <- lss_item_label(state$numbers, q$qid, i)
      state$index_entries[[length(state$index_entries) + 1L]] <- list(
        code = item_code, no = position_no
      )
      out[[length(out) + 1L]] <- list(
        kind = "mc_option", no = position_no,
        variable = item_code, value_token = "",
        labels = stats::setNames(
          rep(list(sprintf(chrome$item_rank_fmt, i)), length(langs)), langs
        )
      )
    }
    out <- c(out, emit_values(q$answers))
    onr <- order_note_row(q, "answer_order", chrome$item_order)
    if (!is.null(onr)) out[[length(out) + 1L]] <- onr
    out
  }

  for (q in g$questions) {
      info <- lss_type_info(q$type)
      # The question's own number: leaf row, parent row, "Other" and
      # comment rows. A row standing for one subquestion gets the sub-item
      # number `<question>.<k>`, k counting the question's sub-item rows in
      # the order they are emitted.
      q_no <- lss_item_label(state$numbers, q$qid)
      if (identical(info$family, "multiple") &&
          length(q$subquestions) > 0L) {
        rows <- c(rows, emit_multiple_choice(q))
      } else if (identical(q$type, "R")) {
        rows <- c(rows, emit_ranking(q))
      } else if (isTRUE(info$has_subquestions) && length(q$subquestions) > 0L) {
        # Dual-scale arrays (type 1): one row per (subquestion x scale),
        # each a single-choice variable named `<q>_<subq>_<0|1>`, with the
        # scale (dualscale header) shown in the Question cell and just that
        # scale's answers below -- mirroring the cards layout.
        dual_scale <- isTRUE(info$has_scales) &&
          !is.null(q$scales) && length(q$scales) > 1L
        sq_scale <- function(s) {
          # Defensive default: a subquestion without a scale_id maps to scale
          # "0". covr cannot trace this closure because it is only ever invoked
          # through vapply()/Filter(), so its body reads as uncovered even when
          # the enclosing 2-D array path runs; excluded from coverage.
          # nocov start
          if (is.null(s$scale_id) || is.na(s$scale_id)) "0" else as.character(s$scale_id)
          # nocov end
        }
        two_d <- !dual_scale &&
          length(unique(vapply(q$subquestions, sq_scale, character(1)))) > 1L
        k <- 0L
        if (two_d) {
          # Two-dimensional array: rows (scale 0) x columns (scale 1) ->
          # one variable per cell, `parent[row_col]`, with the column label
          # in the Question cell (reusing the scale_header line).
          rows_sq <- Filter(function(s) sq_scale(s) == "0", q$subquestions)
          cols_sq <- Filter(function(s) sq_scale(s) != "0", q$subquestions)
          for (rsq in rows_sq) for (csq in cols_sq) {
            k <- k + 1L
            sub_no <- lss_item_label(state$numbers, q$qid, k)
            item_code <- lss_variable_name(
              q$code, paste0(rsq$code, "_", csq$code),
              style = theme$variable_names
            )
            state$index_entries[[length(state$index_entries) + 1L]] <- list(
              code = item_code, no = sub_no
            )
            qr <- emit_question_row("subq", sub_no, item_code,
                                    q = q, sq = rsq)
            qr$subq_relevance <- c(rsq$relevance, csq$relevance)
            qr$scale_header <- stats::setNames(
              lapply(langs, function(lg) csq$texts[[lg]]$question), langs
            )
            rows[[length(rows) + 1L]] <- qr
            # The attributes of the row subquestion, then the question's,
            # then the Exclusive row, as on the card of the cell.
            rows <- c(rows, attr_rows(rsq), attr_rows(q),
                      exclusive_rows(q, rsq))
            rows <- c(rows, emit_value_rows_for(q))
          }
        } else {
        for (sq in q$subquestions) {
          if (dual_scale) {
            for (si in seq_along(q$scales)) {
              k <- k + 1L
              sub_no <- lss_item_label(state$numbers, q$qid, k)
              item_code <- lss_variable_name(q$code, sq$code, si,
                                             theme$variable_names)
              state$index_entries[[length(state$index_entries) + 1L]] <- list(
                code = item_code, no = sub_no
              )
              qr <- emit_question_row("subq", sub_no, item_code,
                                      q = q, sq = sq)
              hdr <- lss_dualscale_header(q, si, langs)
              qr$scale_header <- if (!is.null(hdr)) {
                hdr
              } else {
                stats::setNames(
                  rep(list(paste(chrome$item_scale, si)), length(langs)), langs
                )
              }
              rows[[length(rows) + 1L]] <- qr
              rows <- c(rows, attr_rows(sq), attr_rows(q),
                        exclusive_rows(q, sq))
              rows <- c(rows, emit_values(q$scales[[si]]))
            }
          } else {
            k <- k + 1L
            sub_no <- lss_item_label(state$numbers, q$qid, k)
            item_code <- lss_variable_name(q$code, sq$code,
                                           style = theme$variable_names)
            state$index_entries[[length(state$index_entries) + 1L]] <- list(
              code = item_code, no = sub_no
            )
            rows[[length(rows) + 1L]] <- emit_question_row(
              "subq", sub_no, item_code, q = q, sq = sq
            )
            # The attributes of the subquestion, then the question's, then
            # the Exclusive row, as on the card of the subquestion.
            rows <- c(rows, attr_rows(sq), attr_rows(q),
                      exclusive_rows(q, sq))
            rows <- c(rows, emit_value_rows_for(q))
          }
        }
        }
        if (identical(q$other, "Y")) {
          item_code <- lss_other_variable(q, theme$variable_names)
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = q_no
          )
          rows[[length(rows) + 1L]] <- emit_question_row(
            "other", q_no, item_code, q = q, other_q = q
          )
        }
      } else {
        state$index_entries[[length(state$index_entries) + 1L]] <- list(
          code = q$code, no = q_no
        )
        rows[[length(rows) + 1L]] <- emit_question_row(
          "leaf", q_no, q$code, q = q
        )
        rows <- c(rows, attr_rows(q))
        rows <- c(rows, emit_value_rows_for(q))
        onr <- order_note_row(q, "answer_order", chrome$item_order)
        if (!is.null(onr)) rows[[length(rows) + 1L]] <- onr
        # Single-choice "Other" free-text column.
        if (identical(q$other, "Y")) {
          item_code <- lss_other_variable(q, theme$variable_names)
          state$index_entries[[length(state$index_entries) + 1L]] <- list(
            code = item_code, no = q_no
          )
          rows[[length(rows) + 1L]] <- emit_question_row(
            "other", q_no, item_code, q = q, other_q = q
          )
        }
        # List-with-comment (type O) free-text comment column, with the
        # stem in the language cells.
        if (identical(q$type, "O")) {
          rows[[length(rows) + 1L]] <- emit_comment_row(
            q, q_no, lss_comment_variable(q, style = theme$variable_names)
          )
        }
      }
  }
  rows
}

#' The lines of the Filter cell of a table row
#'
#' The one rule, read by the rendering loop to fill the cell and by
#' `lss_table_template_polish()` to size the column. A question row
#' (`leaf`, `subq`, `other`) prints the levels of its question, group and
#' subquestion(s). An option of a multiple choice prints the one line of
#' its own condition, and nothing without one (its parent row carries the
#' group and question lines). Every other row leaves the cell empty.
#'
#' @return A list of `lss_filter_lines()` lines, or `NULL` when the cell
#'   stays empty.
#' @keywords internal
#' @noRd
lss_table_filter_row_lines <- function(r, theme) {
  if (identical(r$kind, "mc_option")) {
    if (!lss_has_condition(r$relevance)) {
      return(NULL)
    }
    return(lss_filter_lines(NA_character_, NULL, r$relevance, theme))
  }
  if (!r$kind %in% c("leaf", "subq", "other")) {
    return(NULL)
  }
  lss_filter_lines(r$relevance, r$group_relevance, r$subq_relevance, theme)
}

#' Build the flextable paragraph for the Filter cell of a row (the
#' table template). Mirrors the meta-table convention from the
#' cards path: the lines of `lss_filter_lines()` one under the other
#' (the group, question and subquestion levels), each human-readable
#' form with its raw LimeSurvey expression beneath in a smaller italic
#' mono when `show_raw`. `lines` defaults to the lines of the three
#' relevance arguments; the rendering loop passes those of
#' `lss_table_filter_row_lines()`.
#' @keywords internal
#' @noRd
lss_table_filter_paragraph <- function(relevance, theme,
                                       plain_props, raw_props,
                                       show_raw = TRUE,
                                       group_relevance = NULL,
                                       subq_relevance = NULL,
                                       label_props = plain_props,
                                       lines = lss_filter_lines(
                                         relevance, group_relevance,
                                         subq_relevance, theme
                                       )) {
  chunks <- lss_filter_chunks(lines, theme, plain_props = plain_props,
                              raw_props = raw_props, label_props = label_props,
                              show_raw = show_raw)
  do.call(flextable::as_paragraph, chunks)
}

#' Build the flextable paragraph for the Value cell of a row.
#'
#' Compact, language-independent summary of the response domain so a
#' reviewer can scan the codes column without reading the Question
#' columns. Conventions:
#'
#' * Enumerated answers, and the labelled predefined scales (yes/no,
#'   gender, the arrays C and E) -> empty cell: each code has its own
#'   Value row below.
#' * Implicit codings (multiple choice, 5-point and 10-point scales) ->
#'   short fixed token in mono primary ("Y/blank", "1-5", "1-10").
#' * Open-ended types -> descriptor in italic muted ("[num]",
#'   "[text]", "[date]", "[file]", "[calc]", "[rank]").
#' * The "Other" field -> the descriptor of a short free text ("[text]"),
#'   whatever the type of its question.
#'
#' @keywords internal
#' @noRd
lss_table_value_paragraph <- function(row, theme, code_props, descriptor_props) {
  # The table layout devotes a dedicated Value row to every
  # enumerated code, so the Question row's Value cell stays empty
  # for enumerated types -- only non-enumerated types receive an
  # inline descriptor in the Question row itself.
  # Multiple-choice parent header: not a data column, so it documents no
  # value domain of its own -- each option row below carries its Y/blank
  # token instead.
  if (isTRUE(row$mc_parent)) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  # The "Other" field is a single-line free text: it takes the token of a
  # short free text (type S), as the comment row of a list with comment.
  q <- if (identical(row$kind, "other")) list(type = "S") else row$q
  # Enumerated stored answers, and the labelled predefined scales
  # (C/E/Y/G), are documented in their own Value rows below, so the
  # Question row's Value cell stays empty for them.
  if (length(q$answers) > 0L || !is.null(lss_predefined_labelled(q$type))) {
    return(flextable::as_paragraph(flextable::as_chunk(
      "", props = descriptor_props
    )))
  }
  # Implicit codings: short mono token. The numeric N-point scales carry
  # no labels, so a compact range token in the Value cell is the dense
  # equivalent of the cards "%d-point scale" descriptor: 5-point single
  # choice (5) and 5-point array (A) -> "1-5"; 10-point array (B) -> "1-10".
  short_code <- switch(
    EXPR = q$type,
    "M" = "Y/blank",
    "P" = "Y/blank",
    "5" = "1-5",
    "A" = "1-5",
    "B" = "1-10",
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
    ":" = "[num]",
    "S" = "[text]",
    "T" = "[text]",
    "U" = "[text]",
    ";" = "[text]",
    "Q" = "[text]",
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
#' Stacks the question stem (or the prompt of the "Other" field), the
#' subquestion label (compound rows) and the help text (when present and
#' `show_help`), each on its own line, each with the formatting of the
#' author (`lss_compose_chunks()`). Each layer has its own typography:
#' stem in the body color, subq in the same size but italic to
#' distinguish, help muted italic; inside an italic layer an italic of
#' the author is set upright.
#' @keywords internal
#' @noRd
lss_table_question_paragraph <- function(row, lg, theme, show_help,
                                         body_size = theme$size_meta) {
  # One uniform size for stem, subquestion label and help; the visual
  # distinction between them is carried by style (italic, muted), not
  # by size, so the dense table stays uniform.
  size_q  <- body_size
  size_sq <- body_size
  size_h  <- body_size

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
  # A literal chunk in `props` (guillemets, parentheses, the empty marker,
  # the hidden note), as a list of one chunk.
  lit <- function(text, props) {
    list(flextable::as_chunk(text, props = props))
  }
  # The chunks of a text of the author in `props`, its formatting kept
  # (lss_compose_chunks(): in a part set in italic, an italic of the author
  # is set upright).
  runs <- function(html, props) {
    lss_compose_chunks(html, props)
  }
  present <- function(text) {
    !is.null(text) && !is.na(text) && nzchar(trimws(text))
  }

  chunks <- list()
  # A line of the cell: the chunks `cs`, after a break when the cell
  # already holds a line.
  add_line <- function(cs) {
    if (length(cs) == 0L) return()
    if (length(chunks) > 0L) chunks[[length(chunks) + 1L]] <<- br()
    chunks <<- c(chunks, cs)
  }
  # One part of the cell (stem, subquestion, help) on its line: `text`, the
  # part in this language as plain text, says whether it is there, `cs`
  # are its chunks. When this language lacks it and another language of
  # the document has it (`all_langs`, the part in every language as plain
  # text), the empty marker takes its place, in `marker_props`: the rule of
  # the cards, where a row that one language fills shows the marker in the
  # empty cells of the others (lss_compose()).
  add_part <- function(text, cs, all_langs, marker_props) {
    if (present(text)) {
      add_line(cs)
    } else if (lss_any_present(all_langs)) {
      add_line(lit(theme$empty_marker, marker_props))
    }
  }
  flat <- function(x) {
    lapply(x, function(v) if (is.null(v)) NULL else lss_html_to_text(v))
  }

  # Question stem (parent for compound rows, leaf question for leaf
  # rows). For the Other item the prompt of the "Other:" field
  # replaces the stem.
  if (identical(row$kind, "other")) {
    add_line(runs(lss_other_prompt(row$other_q, lg, theme$chrome), plain()))
  } else {
    add_part(lss_html_to_text(row$parent_text[[lg]]),
             runs(row$parent_text[[lg]], plain()),
             flat(row$parent_text), plain(color = theme$color_muted))
  }

  # Subquestion label below the stem (compound rows only), in italic so
  # the eye separates "what's being asked" (stem) from "what this row
  # narrows it to" (subq). The second-axis facet -- a dual-scale header or
  # a 2-D array column -- is appended in parentheses ("Parliament (Trust)",
  # "Trying new things (Today)"), with no empty parentheses when absent.
  if (identical(row$kind, "subq")) {
    subq_line <- function(l) {
      s <- lss_html_to_text(row$subq_text[[l]])
      f <- if (!is.null(row$scale_header)) {
        lss_html_to_text(row$scale_header[[l]])
      } else {
        ""
      }
      s_ok <- !is.na(s) && nzchar(trimws(s))
      f_ok <- !is.na(f) && nzchar(trimws(f))
      if (f_ok) {
        if (s_ok) paste0(s, " (", f, ")") else f
      } else {
        s
      }
    }
    # The same line in chunks: the subquestion and the facet with their
    # formatting, the parentheses in the props of the line.
    sq_props <- plain(size = size_sq, italic = TRUE)
    sq_html <- row$subq_text[[lg]]
    f_html <- if (!is.null(row$scale_header)) row$scale_header[[lg]] else NULL
    sq_cs <- if (present(lss_html_to_text(f_html))) {
      if (present(lss_html_to_text(sq_html))) {
        c(runs(sq_html, sq_props), lit(" (", sq_props), runs(f_html, sq_props),
          lit(")", sq_props))
      } else {
        runs(f_html, sq_props)
      }
    } else {
      runs(sq_html, sq_props)
    }
    add_part(subq_line(lg), sq_cs,
             lapply(names(row$subq_text), subq_line),
             plain(size = size_sq, color = theme$color_muted, italic = TRUE))
  }

  # Help (optional), small muted italic, between guillemets.
  if (isTRUE(show_help) && !identical(row$kind, "other")) {
    help_props <- plain(size = size_h, color = theme$color_muted, italic = TRUE)
    add_part(lss_html_to_text(row$help[[lg]]),
             c(lit("\u00AB ", help_props), runs(row$help[[lg]], help_props),
               lit(" \u00BB", help_props)),
             flat(row$help), help_props)
  }

  if (length(chunks) == 0L) {
    chunks <- list(flextable::as_chunk(
      theme$empty_marker,
      props = plain(color = theme$color_muted)
    ))
  }

  # A question hidden from respondents says so on a last muted italic
  # line of the cell, in the chrome language, like the help line above.
  if (isTRUE(row$hidden)) {
    add_line(lit(theme$chrome$item_hidden,
                 plain(color = theme$color_muted, italic = TRUE)))
  }
  do.call(flextable::as_paragraph, chunks)
}

#' Text of the No cell of a table row: the number label, or empty when
#' the row carries none (`NULL` or `NA`)
#' @keywords internal
#' @noRd
lss_table_no_cell <- function(no) {
  if (is.null(no) || length(no) == 0L || is.na(no[1])) "" else as.character(no[1])
}

#' The prompt of the LimeSurvey "Other:" input in one language
#'
#' The `other_replace_text` of the question for `lg`, else its row
#' without a language, else the default prompt of LimeSurvey
#' (`value_other_prompt`) in `lg`, or in the chrome language when lssdoc
#' has no strings for `lg` (`lss_builtin_strings()`). Shared by the
#' cards and the table layouts.
#' @param q A question of the model.
#' @param lg Length-one character: the language of the column.
#' @param chrome The chrome strings of the document.
#' @keywords internal
#' @noRd
lss_other_prompt <- function(q, lg, chrome) {
  default <- lss_builtin_strings(lg, chrome)$value_other_prompt
  if (is.null(q$attributes) || nrow(q$attributes) == 0L) return(default)
  attrs <- q$attributes[q$attributes$attribute == "other_replace_text", ,
                        drop = FALSE]
  if (nrow(attrs) == 0L) return(default)
  lang_hit <- attrs$value[attrs$language == lg]
  if (length(lang_hit) > 0L && nzchar(trimws(lang_hit[1]))) return(lang_hit[1])
  empty_lang <- attrs$value[!nzchar(attrs$language)]
  if (length(empty_lang) > 0L && nzchar(trimws(empty_lang[1]))) {
    return(empty_lang[1])
  }
  default
}

#' Apply the visual polish (band, borders, widths, section-row
#' merge and dark band) to the table flextable.
#' @keywords internal
#' @noRd
lss_table_template_polish <- function(ft, theme, rows, n_lang) {
  ft <- flextable::font(ft, fontname = theme$font_body, part = "all")
  # Auto-reduce body and header font sizes when the table holds 3+
  # languages so the translation paragraphs keep enough breathing
  # room per cell. With 2 languages each gets >=2.6 in at 8 pt body;
  # with 3 languages 1.75 in, with 4 languages 1.32 in -- 7 pt body
  # is the editorial floor (matches GESIS / OECD survey documentation for
  # multi-column layouts).
  body_size <- if (n_lang >= 3L) theme$size_meta - 1L else theme$size_meta
  header_size <- if (n_lang >= 3L) theme$size_lang_header - 1L else theme$size_lang_header
  ft <- flextable::fontsize(ft, size = body_size, part = "body")
  ft <- flextable::fontsize(ft, size = header_size, part = "header")
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_white, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band_dark, part = "header")

  # Field column body: bold primary so "Question" reads as a row
  # label, with the same petrol-band header (empty text) as the rest
  # of the meta header.
  ft <- flextable::bold(ft, j = "Field", part = "body")
  ft <- flextable::color(ft, j = "Field", color = theme$color_primary,
                         part = "body")

  # Variable column: body font (Calibri), bold, primary. The dense
  # table deliberately avoids the monospace face here -- Consolas is
  # wide and wraps the longer parent_subqcode names, stealing width from
  # the language columns; bold + primary already signal "code". The
  # cards meta band keeps the monospace face (width is not as tight).
  ft <- flextable::font(ft, j = "Variable", fontname = theme$font_body,
                        part = "body")
  ft <- flextable::bold(ft, j = "Variable", part = "body")
  ft <- flextable::color(ft, j = "Variable", color = theme$color_primary,
                         part = "body")

  # Value column body: body font, primary, NOT bold -- the answer codes
  # / coding tokens are secondary reference (the variable name is the
  # emphasized identifier), so a regular weight keeps the column light.
  # The dual-scale "Value (scale N)" header rows are re-bolded below.
  ft <- flextable::font(ft, j = "Value", fontname = theme$font_body,
                        part = "body")
  ft <- flextable::color(ft, j = "Value", color = theme$color_primary,
                         part = "body")

  # Alignment per column, content-driven:
  # - Field, Variable: left both (text / identifier)
  # - No: right both (digits stack as a column)
  # - Type, Mandatory: center both (short categorical tokens)
  # - Value: header center (the word "Value" / "Valeur" / "Wert"
  #   reads as a section title), body right (numeric answer codes
  #   align as a column, Stata / SPSS / GESIS convention)
  # - Language columns: header center (`Francais`, `Deutsch`,
  #   `English` read as section titles above the translation
  #   paragraph), body left (text reads L->R)
  ft <- flextable::align(ft, align = "left",   part = "all")
  ft <- flextable::align(ft, j = "No",        align = "right",  part = "all")
  ft <- flextable::align(ft, j = "Type",      align = "center", part = "all")
  ft <- flextable::align(ft, j = "Mandatory", align = "center", part = "all")
  ft <- flextable::align(ft, j = "Value",     align = "right",  part = "body")
  ft <- flextable::align(ft, j = "Value",     align = "center", part = "header")
  # Language columns are the last `n_lang` columns of the table (the
  # meta columns occupy positions 1:7). Body stays left (translations
  # read L->R); header centered so the language name sits as a
  # section title above the column.
  n_meta <- 7L
  lang_j <- seq.int(n_meta + 1L, length.out = n_lang)
  ft <- flextable::align(ft, j = lang_j, align = "center", part = "header")
  ft <- flextable::align(ft, j = lang_j, align = "left",   part = "body")

  # Borders: soft grid only, no per-row primary outline.
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  ft <- flextable::vline_left(ft, border = thin, part = "all")
  ft <- flextable::vline_right(ft, border = thin, part = "all")

  # Body cells are top-aligned so multiline language content reads from
  # the top. Header labels sit centered in the tinted band.
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "header")
  ft <- flextable::padding(ft, padding.top = 2, padding.bottom = 2,
                           padding.left = 3, padding.right = 3, part = "all")

  # Column widths. Tight on the meta columns to give the language
  # columns >=50% of the total width (the translation paragraphs
  # are the primary content of the table). Short header labels
  # are allowed to wrap to two lines ("Single | choice") -- the
  # visual cost is small and the question-column gain is large.
  #   Field     0.62  - the minimum: "Welcome" (7) and "Question" (8)
  #                     fit on one line. Wider when the widest word of a
  #                     label of this table needs it ("Commentaire",
  #                     "Reihenfolge"), so that a label never breaks
  #                     inside a word (lss_table_field_width(), bold at
  #                     the body size); meta_w grows by the same amount,
  #                     taken from the language columns.
  #   No        0.30  - the minimum. Wider when the widest label of this
  #                     table needs it (lss_no_width() at the body size,
  #                     with the 3 pt padding on each side); meta_w grows
  #                     by the same amount, taken from the language
  #                     columns.
  #   Variable  1.30  - common `parent_subq` codes fit on a single
  #                     11 pt Consolas line; codes >14 chars wrap.
  #   Type      0.62  - one-word labels ("Computed", "Number",
  #                     "Display", "Ranking") stay on one line
  #                     ("Computed", 8 chars at 8 pt, needs ~0.50 in
  #                     plus cell padding, which 0.55 clipped);
  #                     multi-word labels ("Single choice") still
  #                     wrap, an acceptable cost on the per-variable
  #                     header row.
  #   Mandatory 0.50  - uses the abbreviated header
  #                     (`meta_mandatory_short`); the widest
  #                     localized variant ("Pflicht", 7 chars at
  #                     8 pt bold ~0.42 in) fits with thin margin.
  #   Filter    0.58  - the minimum: "All" fits, and a real condition
  #                     (`workstatus = 1`) wrapped already at 0.65, so
  #                     ceding 0.07 in to Type costs nothing visible.
  #                     Wider when the widest level label this table
  #                     prints ("Subquestion:") needs it, so that it
  #                     never breaks inside a word
  #                     (lss_filter_label_width() at the size of the
  #                     label, with the 3 pt padding on each side);
  #                     meta_w grows by the same amount, taken from the
  #                     language columns.
  #   Value     0.55  - 1-3 digit codes in 11 pt Consolas bold
  #                     ("1", "12", "999").
  # Total meta = 4.47 at the minimum widths of Field, No and Filter; the
  # language columns split the remaining width.
  # The total follows the page orientation (theme$content_width_in: 6.30 in
  # portrait, 9.72 in A4 landscape, 14.56 in A3) and NEVER the language
  # count -- four languages fit on an A4 portrait page; reach for landscape
  # via page_format when the columns get too tight, not automatically. The
  # 0.40 in floor is a hard legibility minimum (the body font already steps
  # down to 7 pt from three languages on); if it binds, the page format is
  # too narrow for that many languages and landscape is the fix.
  no_labels <- vapply(rows, function(r) lss_table_no_cell(r$no), character(1L))
  no_w <- max(0.30, lss_no_width(no_labels, body_size, 6))
  # The labels as the Filter cells print them, without the condition, at
  # the size and weight of their chunk (filter_label_props: body font,
  # theme$size_meta, regular).
  fmt <- theme$chrome$filter_level_fmt
  filter_labels <- unlist(lapply(rows, function(r) {
    lines <- Filter(function(ln) !is.null(ln$label),
                    lss_table_filter_row_lines(r, theme))
    vapply(lines, function(ln) {
      trimws(sprintf(fmt, ln$label, ""))
    }, character(1L))
  }))
  filter_w <- max(0.58, lss_filter_label_width(
    filter_labels, theme$font_body, theme$size_meta, 6
  ))
  # The labels of the Field column as the body prints them.
  kinds <- vapply(rows, function(r) as.character(r$kind), character(1L))
  field_w <- lss_table_field_width(ft$body$dataset$Field, kinds, theme,
                                   body_size)
  meta_w <- field_w + no_w + 1.30 + 0.62 + 0.50 + filter_w + 0.55
  total_w <- theme$content_width_in
  lang_w <- max((total_w - meta_w) / max(n_lang, 1L), 0.40)
  ft <- flextable::width(ft, j = "Field",     width = field_w, unit = "in")
  ft <- flextable::width(ft, j = "No",        width = no_w, unit = "in")
  ft <- flextable::width(ft, j = "Variable",  width = 1.30, unit = "in")
  ft <- flextable::width(ft, j = "Type",      width = 0.62, unit = "in")
  ft <- flextable::width(ft, j = "Mandatory", width = 0.50, unit = "in")
  ft <- flextable::width(ft, j = "Filter",    width = filter_w, unit = "in")
  ft <- flextable::width(ft, j = "Value",     width = 0.55, unit = "in")
  for (idx in seq_len(n_lang)) {
    ft <- flextable::width(ft, j = 7L + idx, width = lang_w, unit = "in")
  }

  # Row-type indices for selective styling. The hierarchy is
  # group (most saturated) > question (zebra) > value (white);
  # welcome / endtext sit outside the data flow with accent borders.
  group_idx        <- which(kinds == "group")
  group_desc_idx   <- which(kinds == "group_description")
  group_filter_idx <- which(kinds == "group_filter")
  question_idx     <- which(kinds %in% c("leaf", "subq", "other"))
  scale_header_idx <- which(kinds == "scale_header")
  welcome_idx      <- which(kinds %in% c("welcome", "endtext", "description"))
  exclusive_idx    <- which(kinds == "mc_exclusive")
  order_note_idx   <- which(kinds == "order_note")
  attr_merge_idx   <- which(kinds == "attr" &
                              vapply(rows, function(r) isTRUE(r$merge), logical(1L)))

  # Annotation rows (exclusive-option note, answer / option order note)
  # carry the same chrome sentence in every language column (a structural
  # annotation, not a translation), so merge the language columns into one
  # spanning cell -- the note shows once across the content width instead
  # of being repeated per language. An attribute row is merged when its
  # text is the same in every language (a formula, a suffix "cm").
  if (n_lang > 1L) {
    for (ei in c(exclusive_idx, order_note_idx, group_filter_idx,
                 attr_merge_idx)) {
      ft <- flextable::merge_at(ft, i = ei, j = lang_j, part = "body")
    }
  }

  # Question rows: lighter zebra tint so the eye reads them as
  # "secondary" relative to group banners but still distinct from
  # the white value rows.
  for (qi in question_idx) {
    ft <- flextable::bg(ft, i = qi, bg = theme$color_zebra, part = "body")
  }
  # Scale-header rows (dual-scale arrays only) reuse the zebra tint
  # but bold the Value cell so they read as "subsection within the
  # values".
  for (sh in scale_header_idx) {
    ft <- flextable::bg(ft, i = sh, bg = theme$color_zebra, part = "body")
    ft <- flextable::bold(ft, i = sh, j = "Value", part = "body")
  }
  # Group rows: medium tint + 1.0 pt primary top filet (drawn as
  # the bottom border of the row above) to signal "new section
  # starts here" without dominating the page like the old dark
  # petrol banner did. Width harmonized with the cards template
  # group filet (1 pt) so the two layouts feel like the same
  # document family. When the group is the very first body row
  # the filet is dropped -- the table's natural top border already
  # marks the document opening.
  primary_filet <- officer::fp_border(color = theme$color_primary,
                                      width = 1)
  for (gi in group_idx) {
    ft <- flextable::bg(ft, i = gi, bg = theme$color_band, part = "body")
    if (gi > 1L) {
      ft <- flextable::hline(ft, i = gi - 1L, border = primary_filet,
                             part = "body")
    }
    ft <- flextable::padding(ft, i = gi, padding.top = 6, padding.bottom = 6,
                             padding.left = 4, padding.right = 4,
                             part = "body")
  }
  # Group description rows: same band tint as the group banner above so
  # the two read as a single section block; lighter padding since the
  # intro text follows the title directly.
  for (gd in c(group_desc_idx, group_filter_idx)) {
    ft <- flextable::bg(ft, i = gd, bg = theme$color_band, part = "body")
    ft <- flextable::padding(ft, i = gd, padding.top = 4, padding.bottom = 6,
                             padding.left = 4, padding.right = 4,
                             part = "body")
  }
  # Welcome / End text rows: white background framed by 1 pt accent
  # borders so they read as "preface" / "epilogue" content sitting
  # outside the table's data flow. Same row-1 guard as for the
  # group filet.
  accent_border <- officer::fp_border(color = theme$color_accent, width = 1)
  for (wi in welcome_idx) {
    if (wi > 1L) {
      ft <- flextable::hline(ft, i = wi - 1L, border = accent_border,
                             part = "body")
    }
    ft <- flextable::hline(ft, i = wi, border = accent_border,
                           part = "body")
    ft <- flextable::padding(ft, i = wi, padding.top = 8, padding.bottom = 8,
                             padding.left = 4, padding.right = 4,
                             part = "body")
  }

  ft
}

# Helpers ----------------------------------------------------------

#' Width of the Field column of the table, in inches
#'
#' 0.62 in, or more when the widest word of a label needs it, so that no
#' label breaks inside a word. The labels are measured as the body prints
#' them (body font, bold, `size`) by `lss_filter_label_width()`, with the
#' left and right padding of their row: 4 pt on each side for the rows of a
#' group, the description, the welcome and the end text, 3 pt for the
#' others.
#'
#' @param labels The texts of the Field column, one per body row.
#' @param kinds The kinds of the rows, in the same order.
#' @param size The size of the body, in points.
#' @param measure Passed to `lss_filter_label_width()`.
#' @keywords internal
#' @noRd
lss_table_field_width <- function(labels, kinds, theme, size, measure = NULL) {
  wide <- kinds %in% c("group", "description", "welcome", "endtext")
  need <- function(x, padding) {
    lss_filter_label_width(x, theme$font_body, size, padding,
                           measure = measure, bold = TRUE)
  }
  max(0.62, need(labels[!wide], 6), need(labels[wide], 8))
}

#' Replace `NULL` or empty strings with a fallback. Used inside the
#' rich-cell composer to avoid `as_chunk(NA)` or `as_chunk(NULL)`
#' which flextable rejects.
#' @keywords internal
#' @noRd
`%||_%` <- function(a, b) {
  if (is.null(a) || length(a) == 0L || is.na(a) || !nzchar(a)) b else a
}

# lss_html_to_text() is defined in R/html.R; the table template
# relies on the canonical implementation to keep stem / subq /
# answer-label text identical to what the cards template renders.

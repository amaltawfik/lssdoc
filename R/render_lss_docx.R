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
#' @param languages Character vector of language codes to display, **in the
#'   order they will appear as columns**. Use this both to subset (display
#'   only the languages you want) and to order them; for example
#'   `c("fr", "de")` puts French first, while `c("de", "fr")` puts German
#'   first. The first language is treated as the primary language: the
#'   question heading shown in the table of contents includes the question
#'   text in that language, and group headings fall back to it. Requesting a
#'   language absent from the survey is an error (`lssdoc_unknown_language`).
#'   Defaults to all languages found in the `.lss` file, in the order of the
#'   `<languages>` section.
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
#' @param show_toc Logical; include a table of contents listing the groups
#'   of the survey. Skipped automatically when the survey has fewer than
#'   two groups (a single-group survey makes the TOC redundant). Items
#'   themselves are not in the TOC; use `show_index` for a navigable
#'   variable index.
#' @param show_index Logical; append a variable index at the end of the
#'   document listing every item code with its item number, sorted
#'   alphabetically. Useful for cross-referencing a specific variable.
#' @param show_header_title Logical; show the survey title at the top
#'   right of every page, one line per displayed language. Long titles
#'   are truncated to 80 characters with a trailing ellipsis. Default
#'   `TRUE`. When `FALSE`, only the `X/Y` page counter shows at the
#'   bottom right.
#' @param show_source Logical; show the **Source file** name and the
#'   **Survey ID** rows in the cover metadata table. Default `TRUE`
#'   keeps them for traceability; pass `FALSE` to hide both (some
#'   reviewers prefer not to expose the LimeSurvey survey id or the
#'   internal filename).
#' @param show_item_heading Logical; show a bold heading
#'   `"N. variable"` above each item. Default `FALSE`: the meta table
#'   starts each item directly, for a compact layout. Set to `TRUE` to
#'   add the heading line for scroll-time navigation; the item number is
#'   already present in the meta table's `No` column and in the variable
#'   index so the heading is redundant for cross-reference purposes.
#' @param show_raw_filter Logical; when `TRUE` (the default) the Filter
#'   cell of each meta table shows the human-readable form on top and the
#'   raw LimeSurvey relevance expression in smaller italic gray underneath.
#'   Set to `FALSE` for a cleaner cell that shows only the plain form (the
#'   raw expression is still shown when it could not be simplified).
#' @param title Optional override of the survey title shown on the cover
#'   page and the top-right header. `NULL` (default) uses the per-
#'   language titles from the `.lss` survey settings. Pass a single
#'   string to use the same title in every displayed language, or a
#'   named character vector keyed by language code (e.g.
#'   `c(fr = "Mon titre", de = "Mein Titel")`) for per-language overrides.
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
  show_attrs = c("prefix", "suffix", "other_replace_text", "validation",
                 "exclude_all_others", "exclude_all_others_auto"),
  show_technical_attrs = FALSE,
  page_format = c("auto", "A4-portrait", "A4-landscape", "A3"),
  show_toc = TRUE,
  show_index = TRUE,
  show_header_title = TRUE,
  show_source = TRUE,
  show_item_heading = FALSE,
  show_raw_filter = TRUE,
  title = NULL,
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
  state <- lss_render_state(model)
  state$show_raw_filter <- isTRUE(show_raw_filter)
  state$show_item_heading <- isTRUE(show_item_heading)
  resolved_titles <- lss_resolve_titles(title, lss, langs)
  section <- lss_render_section_props(
    page_format, length(langs),
    theme = theme,
    header_titles = if (isTRUE(show_header_title)) {
      unname(resolved_titles)
    } else {
      character(0)
    }
  )

  doc <- officer::read_docx()
  doc <- lss_render_cover(
    doc, lss, model, theme,
    logo = logo, logo_width = logo_width, logo_height = logo_height,
    show_source = isTRUE(show_source),
    titles = resolved_titles
  )
  doc <- officer::body_add_break(doc)
  if (isTRUE(show_toc) && length(model$groups) >= 2L) {
    doc <- lss_render_toc(doc, model, theme)
    doc <- officer::body_add_break(doc)
  }
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
      audit_idx = audit_idx,
      state = state
    )
  }
  doc <- lss_render_endtext(doc, lss, langs, theme)
  if (isTRUE(show_index) && length(state$index_entries) > 0L) {
    doc <- lss_render_index(doc, state$index_entries, theme)
  }

  doc <- officer::body_set_default_section(doc, section)
  print(doc, target = output)
  # Make Word and LibreOffice refresh fields (TOC, PAGE, NUMPAGES) when
  # the document is opened, so the reader does not need to press F9 and
  # so headless PDF conversion picks up the populated TOC.
  lss_inject_update_fields(output)
  invisible(output)
}

#' Inject `<w:updateFields w:val="true"/>` into the .docx settings.xml
#'
#' This Word setting tells the reader application (Word, LibreOffice) to
#' refresh every field in the document when it is opened. For our use
#' that means the table of contents and the page-number fields populate
#' immediately, without the reader having to press F9. It also propagates
#' to headless PDF conversion via LibreOffice, which honors the flag.
#'
#' Post-processes the .docx zip in place: unzips to a temp directory,
#' modifies `word/settings.xml`, repacks. Silent no-op if the file does
#' not contain a settings.xml (it always does for officer output, but
#' we stay defensive).
#'
#' @keywords internal
#' @noRd
lss_inject_update_fields <- function(docx_path) {
  if (!file.exists(docx_path)) return(invisible(docx_path))
  # Silently no-op when `zip` is missing; the .docx is still valid, just
  # without auto-refresh. (In practice `zip` is always available when
  # `officer` is, because officer imports it.)
  if (!requireNamespace("zip", quietly = TRUE)) {
    return(invisible(docx_path))
  }
  docx_path <- normalizePath(docx_path, mustWork = TRUE)
  tmp <- tempfile()
  dir.create(tmp)
  on.exit(unlink(tmp, recursive = TRUE), add = TRUE)

  utils::unzip(docx_path, exdir = tmp)

  # Step 1: settings.xml -- tell Word to update fields on open.
  settings_file <- file.path(tmp, "word", "settings.xml")
  if (file.exists(settings_file)) {
    raw_in <- readBin(settings_file, what = "raw",
                      n = file.info(settings_file)$size)
    content <- rawToChar(raw_in)
    content <- gsub("<w:updateFields\\b[^/]*/>", "", content, perl = TRUE)
    content <- sub(
      "</w:settings>",
      "<w:updateFields w:val=\"true\"/></w:settings>",
      content, fixed = TRUE
    )
    writeBin(charToRaw(content), settings_file, useBytes = TRUE)
  }

  # Step 2: mark every field 'dirty' so Word refreshes them on open.
  # Fields live in document.xml (the TOC) but ALSO in footerN.xml /
  # headerN.xml (the PAGE and NUMPAGES fields), so we patch every XML
  # part that might contain a fldChar.
  word_dir <- file.path(tmp, "word")
  field_files <- list.files(
    word_dir,
    pattern = "^(document|footer[0-9]*|header[0-9]*)\\.xml$",
    full.names = TRUE
  )
  for (f in field_files) {
    raw_in <- readBin(f, what = "raw", n = file.info(f)$size)
    content <- rawToChar(raw_in)
    # Strip any existing w:dirty on begin fldChars so the subsequent add
    # does not duplicate.
    content <- gsub(
      '(<w:fldChar [^/>]*?w:fldCharType="begin"[^/>]*?)\\s+w:dirty="[^"]*"([^/>]*?)/>',
      '\\1\\2/>',
      content, perl = TRUE
    )
    # Add w:dirty="1" to every begin fldChar.
    content <- gsub(
      '(<w:fldChar [^/>]*?w:fldCharType="begin"[^/>]*?)/>',
      '\\1 w:dirty="1"/>',
      content, perl = TRUE
    )
    writeBin(charToRaw(content), f, useBytes = TRUE)
  }

  # Repack: switch to the temp dir so the relative paths returned by
  # list.files are stored as-is inside the .docx zip
  # ('word/settings.xml', etc.), matching the layout Word and LibreOffice
  # expect. `all.files = TRUE` is critical: a .docx contains hidden
  # entries like `_rels/.rels` whose basename starts with a dot.
  if (file.exists(docx_path)) file.remove(docx_path)
  files <- list.files(tmp, recursive = TRUE, all.files = TRUE,
                      no.. = TRUE)
  old_wd <- getwd()
  setwd(tmp)
  on.exit(setwd(old_wd), add = TRUE)
  zip::zip(zipfile = docx_path, files = files, compression_level = 9)
  invisible(docx_path)
}

#' Resolve the final per-language titles from the user `title` argument
#'
#' Returns a named character vector keyed by `langs`. When `title` is
#' `NULL`, the survey-language settings of the `.lss` are used. A bare
#' string is used for every language. A named vector is matched on
#' language code; missing entries fall back to the `.lss` setting.
#'
#' @keywords internal
#' @noRd
lss_resolve_titles <- function(title, lss, langs) {
  defaults <- lss_header_titles(lss, langs)
  names(defaults) <- langs
  if (is.null(title)) {
    return(defaults)
  }
  if (length(title) == 1L && is.null(names(title))) {
    return(stats::setNames(rep(as.character(title), length(langs)), langs))
  }
  out <- defaults
  for (lg in intersect(langs, names(title))) {
    out[[lg]] <- as.character(title[[lg]])
  }
  out
}

#' Per-language survey titles for the header, in the order of `langs`.
#'
#' Empty / missing titles produce empty strings (the header helper
#' skips them); the order of the returned vector matches `langs` so
#' the primary language shows on the top line.
#'
#' @keywords internal
#' @noRd
lss_header_titles <- function(lss, langs) {
  ls_settings <- lss$survey_language_settings
  if (is.null(ls_settings) || nrow(ls_settings) == 0L) {
    return(rep("", length(langs)))
  }
  vapply(langs, function(lg) {
    v <- ls_settings$surveyls_title[ls_settings$surveyls_language == lg]
    if (length(v) == 0L) "" else as.character(v[1])
  }, character(1))
}

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
#'
#' Also installs the default footer with a centered "Page X of Y" field so
#' every page carries a number. The first page (cover) keeps the same
#' footer; turning it off on the cover would require a Word "different
#' first page" section setting that `prop_section()` does not expose.
#'
#' @keywords internal
#' @noRd
lss_render_section_props <- function(page_format, n_langs,
                                     theme = lss_render_theme(),
                                     header_titles = character(0)) {
  if (identical(page_format, "auto")) {
    page_format <- if (n_langs <= 2L) "A4-portrait" else "A4-landscape"
  }
  size <- switch(
    page_format,
    "A4-portrait"  = officer::page_size(width = 8.27, height = 11.69, orient = "portrait"),
    "A4-landscape" = officer::page_size(width = 11.69, height = 8.27, orient = "landscape"),
    "A3"           = officer::page_size(width = 16.53, height = 11.69, orient = "landscape")
  )
  margin <- 0.6
  officer::prop_section(
    page_size = size,
    page_margins = officer::page_mar(top = 0.7, bottom = 0.7, left = margin, right = margin),
    header_default = lss_build_header(theme, header_titles),
    footer_default = lss_build_footer(theme)
  )
}

#' Page header with the survey title(s), right-aligned, one line per language.
#'
#' Each language gets its own line so long titles never collide. Titles
#' longer than 80 characters are truncated with a trailing ellipsis to
#' keep the header to a single visual line per language.
#'
#' @param header_titles Character vector, one entry per displayed language.
#'   Empty entries are skipped. When the vector is empty (the user opted
#'   out via `show_header_title = FALSE`), the header is left blank.
#' @keywords internal
#' @noRd
lss_build_header <- function(theme, header_titles = character(0)) {
  # Return NULL (not an empty block_list) when there is no title to
  # show; officer chokes on an empty header_default during section
  # processing.
  if (length(header_titles) == 0L) return(NULL)
  muted <- officer::fp_text(
    font.family = theme$font_body,
    font.size = theme$size_meta,
    color = theme$color_muted
  )
  right_align <- officer::fp_par(text.align = "right")
  pars <- lapply(header_titles, function(t) {
    if (is.null(t) || is.na(t) || !nzchar(trimws(t))) return(NULL)
    officer::fpar(
      officer::ftext(lss_truncate_title(t), prop = muted),
      fp_p = right_align
    )
  })
  pars <- Filter(Negate(is.null), pars)
  if (length(pars) == 0L) return(NULL)
  do.call(officer::block_list, pars)
}

#' Page footer with a compact `X/Y` page counter, right-aligned.
#' @keywords internal
#' @noRd
lss_build_footer <- function(theme) {
  muted <- officer::fp_text(
    font.family = theme$font_body,
    font.size = theme$size_meta,
    color = theme$color_muted
  )
  officer::block_list(
    officer::fpar(
      officer::run_word_field("PAGE", prop = muted),
      officer::ftext("/", prop = muted),
      officer::run_word_field("NUMPAGES", prop = muted),
      fp_p = officer::fp_par(text.align = "right")
    )
  )
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
      "Variable index",
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
  ft <- flextable::align(ft, align = "right", j = "No", part = "all")
  ft <- flextable::width(ft, j = "Variable", width = 2.6, unit = "in")
  ft <- flextable::width(ft, j = "No", width = 0.6, unit = "in")
  flextable::body_add_flextable(doc, ft, align = "left")
}

#' Index audit findings by question code for inline lookup
#' @keywords internal
#' @noRd
lss_audit_index <- function(audit) {
  fdf <- audit$findings
  fdf$item_code <- vapply(fdf$location, function(loc) {
    if (grepl("^Question '[^']+'$", loc)) {
      sub("^Question '([^']+)'$", "\\1", loc)
    } else if (grepl("^Subquestion '[^/]+ / .+'$", loc)) {
      # Match the item-centric code used in the renderer: parent_subq.
      sub("^Subquestion '([^/]+) / (.+)'$", "\\1_\\2", loc)
    } else if (grepl("^Answer '[^=]+ = .+'$", loc)) {
      # Findings on answer options attach to the parent question.
      sub("^Answer '([^=]+) = .+'$", "\\1", loc)
    } else {
      NA_character_
    }
  }, character(1), USE.NAMES = FALSE)
  fdf$item_code[!grepl("^[A-Za-z0-9_]+$", fdf$item_code)] <- NA_character_
  by_code <- split(fdf, fdf$item_code)
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
                             logo_height = 0.75,
                             show_source = TRUE,
                             titles = NULL) {
  ls_settings <- lss$survey_language_settings
  langs <- model$languages
  if (is.null(titles)) {
    titles <- stats::setNames(lss_header_titles(lss, langs), langs)
  }

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
    title <- titles[[lg]]
    if (is.null(title) || is.na(title) || !nzchar(title)) {
      title <- "(untitled survey)"
    }
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

  field_pairs <- list()
  if (isTRUE(show_source)) {
    field_pairs[["Source file"]] <- basename(lss$file)
    field_pairs[["Survey ID"]]   <- if (is.na(sid) || !nzchar(sid)) none else sid
  }
  field_pairs[["Languages"]]      <- paste(langs, collapse = ", ")
  field_pairs[["Groups"]]         <- as.character(n_groups)
  field_pairs[["Questions"]]      <- as.character(n_q)
  field_pairs[["Subquestions"]]   <- as.character(n_subq)
  field_pairs[["Answer options"]] <- as.character(n_ans)
  field_pairs[["Last modified"]]  <- if (is.na(last_mod) || !nzchar(last_mod)) none else last_mod
  field_pairs[["Generated"]]      <- format(Sys.time(), "%Y-%m-%d %H:%M")

  meta <- data.frame(
    Field = names(field_pairs),
    Value = unlist(field_pairs, use.names = FALSE),
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
        "Table of contents",
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
                             audit_idx, state) {
  gname <- lss_first_label(group$names, langs)
  if (is.na(gname)) gname <- paste0("Group ", group$gid)
  # Strip a leading numeric prefix written by the LimeSurvey author so we
  # do not get a doubled "1. 1. Vos etudes".
  gname <- lss_strip_group_number_prefix(gname)
  state$group_index <- state$group_index + 1L
  heading_text <- sprintf("%d. %s", state$group_index, gname)
  doc <- officer::body_add_par(doc, "", style = "Normal")
  # Render as a styled paragraph (no Heading 1 style) so Word does NOT
  # add its own list number on top of ours -- the auto-number Word
  # injects via the linked numbering definition uses a different font
  # face/size than our heading text, which looks inconsistent. Doing
  # the numbering manually keeps the whole heading typographically
  # uniform.
  doc <- officer::body_add_fpar(
    doc,
    officer::fpar(officer::ftext(
      heading_text,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    ))
  )
  # Anchor the group heading with a bookmark so the manual TOC entries
  # can hyperlink to it.
  doc <- officer::body_bookmark(doc, lss_group_bookmark(state$group_index))
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

#' Render a compound question (a parent with subquestions, ESS/Mosaich style)
#'
#' Emits a parent stem banner, then the shared answer scale (when the type
#' carries one in `q$answers`), then each subquestion as its own numbered
#' item (Heading 1) with the full LimeSurvey response variable code
#' (`parent_subqcode`).
#'
#' @keywords internal
#' @noRd
lss_render_compound_question <- function(doc, q, langs, theme,
                                         show_help, show_attrs,
                                         show_technical_attrs, audit_idx,
                                         info, state) {
  doc <- lss_render_parent_stem(doc, q, langs, theme,
                                show_help = show_help,
                                show_attrs = show_attrs,
                                audit_idx = audit_idx,
                                state = state)
  if (isTRUE(info$has_answers) && length(q$answers) > 0L) {
    doc <- lss_render_shared_scale(doc, q, langs, theme)
  }
  for (sq in q$subquestions) {
    item_code <- paste0(q$code, "_", sq$code)
    item_help <- lapply(langs, function(lg) sq$texts[[lg]]$help)
    item_text <- lapply(langs, function(lg) sq$texts[[lg]]$question)
    doc <- lss_render_subq_item(
      doc, q, sq, langs, theme,
      item_code = item_code,
      texts_by_lang = item_text,
      help_by_lang = item_help,
      show_help = show_help,
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
    type = q$type, type_label = q$type_label,
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )

  texts_by_lang <- lapply(langs, function(lg) q$texts[[lg]]$question)
  help_by_lang <- lapply(langs, function(lg) q$texts[[lg]]$help)
  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = "Question",
    texts = stats::setNames(texts_by_lang, langs),
    size = theme$size_question
  )
  if (isTRUE(show_help) && lss_any_present(help_by_lang)) {
    rows[[length(rows) + 1L]] <- list(
      label = "Help",
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
    type = "T", type_label = "Other - free text",
    mandatory = "N",
    relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  rows <- list(list(
    label = "Question",
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

#' Render a subquestion as a numbered item
#' @keywords internal
#' @noRd
lss_render_subq_item <- function(doc, q, sq, langs, theme,
                                 item_code, texts_by_lang, help_by_lang,
                                 show_help, audit_idx, state) {
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
  # Each subquestion is documented as its own item: meta table with the
  # composite variable code (parent_subq) and the type / mandatory /
  # filter inherited from the parent (LimeSurvey subquestions do not
  # carry their own).
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = item_code,
    type = q$type, type_label = q$type_label,
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )
  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = "Question",
    texts = stats::setNames(texts_by_lang, langs),
    size = theme$size_subq
  )
  if (isTRUE(show_help) && lss_any_present(help_by_lang)) {
    rows[[length(rows) + 1L]] <- list(
      label = "Help",
      texts = stats::setNames(help_by_lang, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
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

  # Structured meta table: No | Variable | Type | Mand. | Filter
  doc <- lss_render_question_meta_table(
    doc, theme,
    item_no = state$item_no,
    variable = q$code,
    type = q$type, type_label = q$type_label,
    mandatory = q$mandatory, relevance = q$relevance,
    show_raw_filter = isTRUE(state$show_raw_filter)
  )

  # Build the unified item table: Question, optional Help, then one
  # row per answer option (for has_answers leaf types like L, !, O).
  rows <- list()
  rows[[length(rows) + 1L]] <- list(
    label = "Question",
    texts = stats::setNames(texts_by_lang, langs),
    size = theme$size_question
  )
  if (isTRUE(show_help) && lss_any_present(help_by_lang)) {
    rows[[length(rows) + 1L]] <- list(
      label = "Help",
      texts = stats::setNames(help_by_lang, langs),
      size = theme$size_help,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  # For predefined leaf types (Y, 5, G) the response coding is implicit.
  # Show it as a small italic row so reviewers see the value scheme.
  coding <- lss_coding_row(q, langs, theme)
  if (!is.null(coding)) rows[[length(rows) + 1L]] <- coding
  # Question attributes (prefix, suffix, validation, ...) as italic rows
  # inside the item table itself, between Help and the Value section.
  rows <- c(rows, lss_attr_rows(q, langs, theme, show_attrs))
  if (length(q$answers) > 0L) {
    # "Value" is a section label spanning the language columns blank;
    # the actual answer codes (1, 2, ...) follow below as their own
    # labelled rows.
    rows[[length(rows) + 1L]] <- list(
      label = "Value",
      texts = stats::setNames(as.list(rep("", length(langs))), langs),
      size = theme$size_meta,
      section_header = TRUE
    )
    for (a in q$answers) {
      rows[[length(rows) + 1L]] <- list(
        label = a$code,
        texts = stats::setNames(lapply(langs, function(lg) a$labels[[lg]]), langs),
        size = theme$size_answer
      )
    }
  }
  doc <- lss_render_item_table(doc, theme, langs, rows)
  doc
}

#' Render a unified item table with a left "Label" column
#'
#' Builds a single flextable per item with the layout
#' `Language | Français | Deutsch | ...` as header and one body row per
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
      list(Label = "Language"),
      stats::setNames(as.list(lss_language_label(langs)), langs)
    )
  )
  for (i in seq_along(rows)) {
    sz <- if (!is.null(rows[[i]]$size)) rows[[i]]$size else theme$size_question
    italic <- isTRUE(rows[[i]]$italic)
    color <- if (!is.null(rows[[i]]$color)) rows[[i]]$color else theme$color_text
    is_section <- isTRUE(rows[[i]]$section_header)
    for (lg in langs) {
      if (is_section) {
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
  ft <- flextable::width(ft, j = "Label", width = 0.9, unit = "in")
  # Light tint on section-header rows to set them apart from content.
  for (i in seq_along(rows)) {
    if (isTRUE(rows[[i]]$section_header)) {
      ft <- flextable::bg(ft, i = i, bg = theme$color_zebra, part = "body")
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

#' Insert a small vertical spacer before each item so consecutive
#' meta tables do not touch.
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
      fp_p = officer::fp_par(padding.top = 6, padding.bottom = 0)
    )
  )
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
    rows[[length(rows) + 1L]] <- list(
      label = tools::toTitleCase(attr_name),
      texts = stats::setNames(as.list(per_lang), langs),
      size = theme$size_meta,
      color = theme$color_muted,
      italic = TRUE
    )
  }
  rows
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
  switch(toupper(x), Y = "yes", N = "no", S = "soft", x)
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

#' Display label for a relevance expression
#' @keywords internal
#' @noRd
lss_relevance_label <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("\u2014")
  if (identical(x, "1")) return("All")
  x
}

#' Best-effort translation of a LimeSurvey relevance expression into plain
#' English
#'
#' Recognized patterns: `is_empty(X.NAOK)` -> "X is empty";
#' `!is_empty(X.NAOK)` -> "X is answered"; `X.NAOK == N` -> "X = N";
#' `X.NAOK != N` -> "X != N"; `&&` -> "AND"; `||` -> "OR". The function
#' strips obviously balanced outer parentheses. When the expression cannot
#' be matched it is returned unchanged, so the raw text is never lost.
#'
#' @param x A character relevance expression as stored in LimeSurvey.
#' @return A single human-readable string. `"All"` for `1`, empty, or `NA`.
#' @keywords internal
#' @noRd
lss_humanize_relevance <- function(x) {
  if (is.null(x) || is.na(x) || !nzchar(x) || identical(x, "1")) {
    return("All")
  }
  s <- as.character(x)
  s <- lss_strip_outer_parens(s)

  # Collapse the LimeSurvey "answered-and-equals" idiom on the SAME
  # variable. LimeSurvey's conditional designer always emits
  # `!is_empty(X.NAOK) && (X.NAOK OP value)` as a defensive guard, even
  # though the comparison alone is enough semantically. For human review
  # the guard is noise, so we drop it. The collapse repeats so chained
  # conditions on different variables each get simplified.
  idiom_left <- paste0(
    "!\\s*is_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)\\s*&&\\s*",
    "\\(\\s*\\1\\.NAOK\\s*(==|!=|>=|<=|>|<)\\s*([^)&|]+)\\s*\\)"
  )
  idiom_right <- paste0(
    "\\(\\s*([A-Za-z0-9_]+)\\.NAOK\\s*(==|!=|>=|<=|>|<)\\s*([^)&|]+)\\s*\\)",
    "\\s*&&\\s*!\\s*is_empty\\(\\1\\.NAOK\\)"
  )
  for (i in seq_len(5L)) {
    before <- s
    s <- gsub(idiom_left, "\\1.NAOK \\2 \\3", s, perl = TRUE)
    s <- gsub(idiom_right, "\\1.NAOK \\2 \\3", s, perl = TRUE)
    if (identical(s, before)) break
  }

  s <- gsub("!\\s*is_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)", "\\1 is answered",
            s, perl = TRUE)
  s <- gsub("\\bis_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)", "\\1 is empty",
            s, perl = TRUE)
  s <- gsub("([A-Za-z0-9_]+)\\.NAOK", "\\1", s, perl = TRUE)
  s <- gsub("\\s*&&\\s*", " AND ", s)
  s <- gsub("\\s*\\|\\|\\s*", " OR ", s)
  s <- gsub("\\s*!=\\s*", " \u2260 ", s)
  s <- gsub("\\s*==\\s*", " = ", s)
  s <- lss_strip_outer_parens(s)
  trimws(s)
}

#' Strip balanced outer parentheses up to a few levels deep
#' @keywords internal
#' @noRd
lss_strip_outer_parens <- function(s) {
  for (i in seq_len(8L)) {
    inner <- sub("^\\s*\\((.*)\\)\\s*$", "\\1", s, perl = TRUE)
    if (identical(inner, s) || !lss_parens_balanced(inner)) break
    s <- inner
  }
  s
}

#' Check whether parentheses are balanced in a string
#' @keywords internal
#' @noRd
lss_parens_balanced <- function(s) {
  depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch == "(") {
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
      if (depth < 0L) return(FALSE)
    }
  }
  depth == 0L
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
                                           show_raw_filter = TRUE) {
  filter_raw <- if (is.null(relevance) || is.na(relevance) ||
                    !nzchar(relevance)) {
    "1"
  } else {
    relevance
  }
  filter_plain <- lss_humanize_relevance(filter_raw)
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
    Mandatory = lss_yes_no(mandatory),
    Filter = "",
    check.names = FALSE,
    stringsAsFactors = FALSE
  )

  ft <- flextable::flextable(df)
  plain_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_meta,
    color = theme$color_text
  )
  raw_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_meta - 1L,
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
  ft <- flextable::bold(ft, part = "header")
  ft <- flextable::color(ft, color = theme$color_primary, part = "header")
  ft <- flextable::bg(ft, bg = theme$color_band, part = "header")
  ft <- flextable::border_remove(ft)
  thin <- officer::fp_border(color = "#BFBFBF", width = 0.5)
  ft <- flextable::hline(ft, border = thin, part = "all")
  ft <- flextable::vline(ft, border = thin, part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  ft <- flextable::align(ft, align = "center", j = c("No", "Mandatory"), part = "all")
  ft <- flextable::width(ft, j = "No", width = 0.4, unit = "in")
  ft <- flextable::width(ft, j = "Variable", width = 1.5, unit = "in")
  ft <- flextable::width(ft, j = "Type", width = 1.5, unit = "in")
  ft <- flextable::width(ft, j = "Mandatory", width = 1.0, unit = "in")
  ft <- flextable::width(ft, j = "Filter", width = 2.0, unit = "in")
  flextable::body_add_flextable(doc, ft, align = "left")
}

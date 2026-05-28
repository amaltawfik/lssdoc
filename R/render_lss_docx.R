#' Render a questionnaire to a Word document (internal)
#'
#' Internal Word-document renderer used by [render_questionnaire()] for the
#' `.docx` branch. The user-facing API and argument documentation live on
#' [render_questionnaire()].
#'
#' @keywords internal
#' @noRd
.render_questionnaire_docx <- function(
  lss,
  output,
  languages = NULL,
  template = c("cards", "table"),
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
  show_groups = TRUE,
  show_welcome = TRUE,
  show_endtext = TRUE,
  show_description = TRUE,
  show_privacy_settings = FALSE,
  show_admin_settings = FALSE,
  title = NULL,
  logo = NULL,
  logo_width = 1.5,
  logo_height = 0.75,
  font = NULL,
  font_code = NULL,
  colors = NULL,
  authors = NULL,
  description = NULL,
  chrome_lang = NULL
) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      "{.arg lss} must be an {.cls lss} object from {.fn read_lss}.",
      class = "lssdoc_bad_lss"
    )
  }
  if (!is.character(output) || length(output) != 1L || is.na(output)) {
    lssdoc_abort(
      "{.arg output} must be a single file path.",
      class = "lssdoc_bad_output"
    )
  }
  template <- rlang::arg_match(template)
  layout <- rlang::arg_match(layout)
  page_format <- rlang::arg_match(page_format)
  # The dense `table` template needs landscape width for 2+ content
  # languages so the side-by-side Question columns do not collapse.
  # Auto-promote unless the caller pinned the format explicitly.
  effective_langs <- if (is.null(languages)) lss$languages else languages
  if (identical(template, "table") &&
      identical(page_format, "auto") &&
      length(effective_langs) >= 2L) {
    page_format <- "A4-landscape"
  }
  lss_validate_logo(logo)
  lss_validate_font(font, "font")
  lss_validate_font(font_code, "font_code")
  colors <- lss_validate_colors(colors)
  authors <- lss_normalize_authors(authors)
  description <- lss_normalize_description(description)
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

  # Build the model first so we know the group count and can budget the
  # progress bar correctly. This step is fast (< 1 s).
  model <- lss_model(lss, languages = languages)
  langs <- model$languages
  theme <- lss_render_theme()
  if (!is.null(font)) theme$font_body <- font
  if (!is.null(font_code)) theme$font_code <- font_code
  if (!is.null(colors)) theme <- utils::modifyList(theme, colors)
  # Resolve the chrome language now that we know the content languages,
  # then attach the localized chrome strings to the theme so every
  # renderer helper can pick them up via `theme$chrome$<key>` without
  # plumbing an extra argument.
  chrome_lang <- lss_resolve_chrome_lang(chrome_lang, langs)
  theme$chrome <- lss_chrome_strings(chrome_lang)
  theme$chrome_lang <- chrome_lang
  n_groups <- length(model$groups)

  # Single progress bar covering the entire render. The total is
  # `n_groups + 3` ticks: one for audit, one for the cover/TOC/welcome
  # block, n_groups for each group's items, and one for the file write.
  # The status text changes per phase, so the user sees both a smoothly
  # filling bar AND a label for what is happening right now -- without
  # the multi-line "step ... done" cascade that the previous design
  # produced. cli detects non-interactive sessions automatically and
  # downgrades to plain messages; suppressMessages() silences entirely.
  total <- n_groups + 3L
  cli::cli_progress_bar(
    name = "Rendering questionnaire",
    total = total,
    clear = TRUE
  )

  cli::cli_progress_update(
    set = 0L,
    status = if (isTRUE(show_audit)) "Running audit" else "Skipping audit"
  )
  audit_idx <- if (isTRUE(show_audit)) {
    lss_audit_index(audit_lss(lss))
  } else {
    NULL
  }
  state <- lss_render_state(model)
  state$show_raw_filter <- isTRUE(show_raw_filter)
  state$show_item_heading <- isTRUE(show_item_heading)
  state$show_attrs <- show_attrs
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

  cli::cli_progress_update(set = 1L, status = "Cover, TOC and welcome")
  doc <- officer::read_docx()
  doc <- lss_render_cover(
    doc, lss, model, theme,
    logo = logo, logo_width = logo_width, logo_height = logo_height,
    show_source = isTRUE(show_source),
    show_privacy_settings = isTRUE(show_privacy_settings),
    show_admin_settings = isTRUE(show_admin_settings),
    titles = resolved_titles,
    authors = authors,
    description = description
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

  if (identical(template, "table")) {
    # Dense codebook layout: description / welcome / group / question /
    # value / endtext all become rows of one big flextable when their
    # respective `show_*` flag is on. Skipped rows simply drop out of
    # the row list.
    table_rows <- list()
    if (isTRUE(show_description)) {
      desc_row <- lss_table_text_row(
        lss, langs, "surveyls_description", "description"
      )
      if (!is.null(desc_row)) {
        table_rows[[length(table_rows) + 1L]] <- desc_row
      }
    }
    if (isTRUE(show_welcome)) {
      welcome_row <- lss_table_text_row(
        lss, langs, "surveyls_welcometext", "welcome"
      )
      if (!is.null(welcome_row)) {
        table_rows[[length(table_rows) + 1L]] <- welcome_row
      }
    }
    for (i in seq_along(model$groups)) {
      cli::cli_progress_update(
        set = 2L + i - 1L,
        status = sprintf("Group %d/%d", i, n_groups)
      )
      table_rows <- c(
        table_rows,
        lss_table_template_rows_for_group(
          model$groups[[i]], langs, theme,
          show_help = show_help, show_groups = show_groups, state = state
        )
      )
    }
    if (isTRUE(show_endtext)) {
      endtext_row <- lss_table_text_row(
        lss, langs, "surveyls_endtext", "endtext"
      )
      if (!is.null(endtext_row)) {
        table_rows[[length(table_rows) + 1L]] <- endtext_row
      }
    }
    doc <- lss_render_table_template(
      doc, table_rows, langs, theme,
      show_help = show_help,
      show_attrs = show_attrs,
      state = state
    )
  } else {
    if (isTRUE(show_description)) {
      doc <- lss_render_description(doc, lss, langs, theme)
    }
    if (isTRUE(show_welcome)) {
      doc <- lss_render_welcome(doc, lss, langs, theme)
    }
    for (i in seq_along(model$groups)) {
      cli::cli_progress_update(
        set = 2L + i - 1L,
        status = sprintf("Group %d/%d", i, n_groups)
      )
      doc <- lss_render_group(
        doc, model$groups[[i]], langs, theme,
        show_help = show_help,
        show_attrs = show_attrs,
        show_technical_attrs = show_technical_attrs,
        show_groups = show_groups,
        audit_idx = audit_idx,
        state = state
      )
    }
  }
  # End text already rendered as a row inside the codebook table
  # when template == "table"; only the cards layout needs the
  # separate body-level paragraph here, and only when the caller
  # has not opted out via `show_endtext = FALSE`.
  if (!identical(template, "table") && isTRUE(show_endtext)) {
    doc <- lss_render_endtext(doc, lss, langs, theme)
  }
  if (isTRUE(show_index) && length(state$index_entries) > 0L) {
    doc <- lss_render_index(doc, state$index_entries, theme)
  }

  cli::cli_progress_update(
    set = 2L + n_groups,
    status = sprintf("Writing %s", basename(output))
  )
  doc <- officer::body_set_default_section(doc, section)
  print(doc, target = output)
  # Make Word and LibreOffice refresh fields (TOC, PAGE, NUMPAGES) when
  # the document is opened, so the reader does not need to press F9 and
  # so headless PDF conversion picks up the populated TOC.
  lss_inject_update_fields(output)
  cli::cli_progress_update(set = total)
  cli::cli_progress_done()

  abs_path <- tryCatch(
    normalizePath(output, winslash = "/", mustWork = TRUE),
    error = function(e) output
  )
  size_kb <- round(file.size(output) / 1024)
  cli::cli_alert_success(
    "Saved {.file {abs_path}} ({size_kb} KB, {n_groups} group{?s})"
  )
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
    # Editorial petrol-blue palette tuned for scientific questionnaire
    # documentation. Replaces the earlier Office-blue family (#1F4E79 /
    # #2E75B6) which read as "Microsoft default". The petrol primary +
    # blue-green accent + soft blue-gray grid family follow the pattern
    # used by Pew Research / OECD / ESS publications.
    color_primary = "#133B52",
    color_accent  = "#3A7C8C",
    color_band    = "#E9F2F6",
    # Dark table header tone (secondary of the petrol family). Reserved
    # for the meta-table column headers so every item opens with a
    # white-on-dark "new variable" banner. Distinct from color_primary
    # so the group banners (1.5 pt #133B52 line + #133B52 title text)
    # keep their visual dominance over per-item banners.
    color_band_dark = "#1F4E5F",
    color_zebra   = "#F4F8FA",
    color_grid    = "#D3DCE2",
    color_text    = "#222222",
    color_muted   = "#6E6E6E",
    color_white   = "#FFFFFF",
    color_warning = "#C45911",
    color_error   = "#9E1B1B",
    color_note    = "#5B5B5B",

    # Single source of truth for the printable body width. Page margins of
    # 2.5 cm on A4 leave 6.30 in for content; the meta table, item table,
    # welcome/end-text blocks, and shared-scale table all use this width so
    # that group headings, the TOC, and every table land on the same left
    # and right edges. Changing this value alone re-aligns the whole document.
    content_width_in = 6.30,

    # Body font: Calibri is pre-installed on Windows (and metric-substituted
    # with Carlito on Mac/Linux LibreOffice), so column widths computed at
    # render time match what the reader will see in every environment.
    # Override at the call site via render_questionnaire(font = "...") when the
    # reader's machine has a preferred face installed (Source Sans 3,
    # IBM Plex Sans, a corporate brand font, ...).
    font_body = "Calibri",
    # Monospace font used on code-like cells (the variable name column, the
    # raw relevance expression, the variable index entries). Consolas is
    # installed on every recent Windows; LibreOffice substitutes a similar
    # mono face, and Word on macOS picks Menlo. Override via
    # render_questionnaire(font_code = "JetBrains Mono") for sharper code style.
    font_code = "Consolas",
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

#' Validate and normalize the `authors` argument
#'
#' Accepts three input shapes and returns a single normalized form: a
#' `list` (possibly empty) of `list(name, affiliation, orcid)`, each
#' a single character string (`""` when the field was not supplied).
#' Returning the same shape regardless of input means the renderer
#' does not need to branch on the API form.
#'
#' Recognized shapes:
#' - `NULL` -> `NULL` (renderer omits the block);
#' - **character vector** (named or unnamed): each entry becomes one
#'   author, with `affiliation = ""` if unnamed and `orcid = ""` always;
#' - **list of named lists**: each element is treated as a structured
#'   author with the fields `name` (required), `affiliation`, and
#'   `orcid` (both optional).
#'
#' @keywords internal
#' @noRd
lss_normalize_authors <- function(authors) {
  if (is.null(authors)) return(NULL)
  fail <- function() {
    lssdoc_abort(
      c(
        "{.arg authors} must be {.code NULL}, a character vector, or a list of named lists.",
        "i" = "See {.fn render_questionnaire} for the accepted shapes."
      ),
      class = "lssdoc_bad_authors"
    )
  }
  if (is.character(authors)) {
    if (any(is.na(authors))) fail()
    nm <- names(authors)
    out <- lapply(seq_along(authors), function(i) {
      name <- if (!is.null(nm) && nzchar(nm[i])) nm[i] else as.character(authors[i])
      affil <- if (!is.null(nm) && nzchar(nm[i])) as.character(authors[i]) else ""
      list(name = name, affiliation = affil, orcid = "")
    })
    return(out)
  }
  if (is.list(authors)) {
    out <- lapply(authors, function(a) {
      if (!is.list(a) || is.null(a$name) || !nzchar(trimws(as.character(a$name)))) fail()
      list(
        name = as.character(a$name),
        affiliation = if (is.null(a$affiliation)) "" else as.character(a$affiliation),
        orcid = if (is.null(a$orcid)) "" else as.character(a$orcid)
      )
    })
    return(out)
  }
  fail()
}

#' Normalize the `description` argument: NULL or a single non-empty
#' string. Returns NULL for missing/empty input so the renderer can
#' simply skip the block.
#'
#' @keywords internal
#' @noRd
lss_normalize_description <- function(description) {
  if (is.null(description)) return(NULL)
  if (!is.character(description) || length(description) != 1L ||
      is.na(description)) {
    lssdoc_abort(
      "{.arg description} must be {.code NULL} or a single string.",
      class = "lssdoc_bad_description"
    )
  }
  if (!nzchar(trimws(description))) return(NULL)
  description
}

#' Validate the optional font arguments: NULL or a single non-empty string.
#' We do not check that the font is installed (impossible to enumerate
#' reliably across Word / LibreOffice / Pages); if the reader's machine is
#' missing it, their application substitutes its own fallback face.
#' @keywords internal
#' @noRd
lss_validate_font <- function(value, arg_name) {
  if (is.null(value)) return(invisible())
  if (!is.character(value) || length(value) != 1L || is.na(value) ||
      !nzchar(trimws(value))) {
    lssdoc_abort(
      "{.arg {arg_name}} must be {.code NULL} or a single non-empty string.",
      class = "lssdoc_bad_font"
    )
  }
  invisible()
}

#' Validate and normalize the `colors` argument
#'
#' Returns a named list keyed by `color_<name>` (the actual theme
#' keys) so the renderer can `modifyList(theme, colors)` without
#' re-mapping. Accepts `NULL` (no override), a list with the eight
#' palette names (`primary`, `accent`, `band`, `band_dark`, `zebra`,
#' `grid`, `text`, `muted`), each value a hex color string
#' (`"#XXXXXX"` or `"#XXX"`).
#'
#' Unknown names are rejected with a classed condition so a typo
#' (e.g. `"primay"`) never silently turns into a no-op.
#'
#' @keywords internal
#' @noRd
lss_validate_colors <- function(colors) {
  if (is.null(colors)) return(NULL)
  if (!is.list(colors) || is.null(names(colors)) ||
      any(!nzchar(names(colors)))) {
    lssdoc_abort(
      "{.arg colors} must be {.code NULL} or a named list of hex colors.",
      class = "lssdoc_bad_colors"
    )
  }
  allowed <- c("primary", "accent", "band", "band_dark", "zebra",
               "grid", "text", "muted")
  unknown <- setdiff(names(colors), allowed)
  if (length(unknown) > 0L) {
    lssdoc_abort(
      c(
        "{.arg colors} has unknown name{?s}: {.val {unknown}}.",
        "i" = "Accepted: {.val {allowed}}."
      ),
      class = "lssdoc_bad_colors"
    )
  }
  hex_re <- "^#(?:[0-9a-fA-F]{3}|[0-9a-fA-F]{6})$"
  bad_hex <- vapply(
    colors,
    function(v) {
      !is.character(v) || length(v) != 1L || is.na(v) ||
        !grepl(hex_re, v)
    },
    logical(1L)
  )
  if (any(bad_hex)) {
    lssdoc_abort(
      c(
        "Every value in {.arg colors} must be a hex string ({.code \"#XXXXXX\"} or {.code \"#XXX\"}).",
        "i" = "Invalid: {.val {names(colors)[bad_hex]}}."
      ),
      class = "lssdoc_bad_colors"
    )
  }
  stats::setNames(colors, paste0("color_", names(colors)))
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
  # 2.5 cm side margins on A4 portrait leave exactly theme$content_width_in
  # (6.30 in) for body content, so the meta table, item table, welcome
  # block, shared scale, and any other 6.30-in panel align flush with the
  # left and right margins. Top and bottom margins are slightly larger
  # (1.0 in) than the sides so the running header and body keep some air
  # between them; otherwise the first line of body content lands right
  # under the title strip.
  margin_side <- 0.98
  margin_vert <- 1.0
  officer::prop_section(
    page_size = size,
    page_margins = officer::page_mar(
      top = margin_vert, bottom = margin_vert,
      left = margin_side, right = margin_side,
      header = 0.4, footer = 0.4
    ),
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
                             subtitle = NULL,
                             logo = NULL,
                             logo_width = 1.5,
                             logo_height = 0.75,
                             show_source = TRUE,
                             show_privacy_settings = FALSE,
                             show_admin_settings = FALSE,
                             titles = NULL,
                             authors = NULL,
                             description = NULL) {
  if (is.null(subtitle)) subtitle <- theme$chrome$cover_subtitle_review
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

  doc <- lss_render_authors_block(doc, authors, theme)
  doc <- lss_render_description_block(doc, description, theme)

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

  chrome <- theme$chrome
  field_pairs <- list()
  if (isTRUE(show_source)) {
    field_pairs[[chrome$cover_source_file]] <- basename(lss$file)
    field_pairs[[chrome$cover_survey_id]]   <- if (is.na(sid) || !nzchar(sid)) none else sid
  }
  field_pairs[[chrome$cover_languages]]      <- paste(langs, collapse = ", ")
  field_pairs[[chrome$cover_groups]]         <- as.character(n_groups)
  field_pairs[[chrome$cover_questions]]      <- as.character(n_q)
  field_pairs[[chrome$cover_subquestions]]   <- as.character(n_subq)
  field_pairs[[chrome$cover_answer_options]] <- as.character(n_ans)
  field_pairs[[chrome$cover_last_modified]]  <- if (is.na(last_mod) || !nzchar(last_mod)) none else last_mod
  field_pairs[[chrome$cover_generated]]      <- format(Sys.time(), "%Y-%m-%d %H:%M")

  # Optional administrative settings (alias, end URL, active flag).
  # `surveyls_alias`, `surveyls_url`, `surveyls_urldescription` live on
  # `survey_language_settings`; `active` lives on `surveys`. Each row
  # is emitted only when the underlying value is non-empty so the
  # block stays compact for surveys that do not configure them.
  if (isTRUE(show_admin_settings)) {
    primary <- langs[1L]
    ls_primary <- if (!is.null(ls_settings) && nrow(ls_settings) > 0L) {
      ls_settings[ls_settings$surveyls_language == primary, , drop = FALSE]
    } else NULL
    pull_ls <- function(col) {
      if (is.null(ls_primary) || nrow(ls_primary) == 0L) return("")
      v <- ls_primary[[col]]
      if (is.null(v) || length(v) == 0L || is.na(v[1])) return("")
      v[1]
    }
    pull_survey <- function(col) {
      if (is.null(lss$surveys) || !(col %in% names(lss$surveys))) return("")
      v <- lss$surveys[[col]][1L]
      if (is.null(v) || is.na(v)) return("") else v
    }
    alias <- pull_ls("surveyls_alias")
    if (nzchar(trimws(alias))) {
      field_pairs[[chrome$cover_alias]] <- alias
    }
    end_url <- pull_ls("surveyls_url")
    if (nzchar(trimws(end_url))) {
      field_pairs[[chrome$cover_end_url]] <- end_url
    }
    end_url_desc <- pull_ls("surveyls_urldescription")
    if (nzchar(trimws(end_url_desc))) {
      field_pairs[[chrome$cover_end_url_description]] <- end_url_desc
    }
    active <- pull_survey("active")
    if (nzchar(trimws(active))) {
      field_pairs[[chrome$cover_active]] <- lss_yes_no(active, theme)
    }
  }

  # Optional privacy / tracking settings (anonymized, save partial,
  # datestamp, IP, referrer). LimeSurvey stores them on the
  # `surveys` table as Y/N flags. Each row uses the localized yes/no
  # token so the block reads in the chrome language.
  if (isTRUE(show_privacy_settings)) {
    pull_survey <- function(col) {
      if (is.null(lss$surveys) || !(col %in% names(lss$surveys))) return("")
      v <- lss$surveys[[col]][1L]
      if (is.null(v) || is.na(v)) return("") else v
    }
    yn_row <- function(label, col) {
      raw <- pull_survey(col)
      if (!nzchar(trimws(raw))) return()
      field_pairs[[label]] <<- lss_yes_no(raw, theme)
    }
    yn_row(chrome$cover_anonymized,        "anonymized")
    yn_row(chrome$cover_save_partial,      "save")
    yn_row(chrome$cover_timestamp,         "datestamp")
    yn_row(chrome$cover_ip_recorded,       "ipaddr")
    yn_row(chrome$cover_referrer_recorded, "refurl")
  }

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

#' Render the authors block on the cover page
#'
#' Each author becomes one centered line: `Name \u2014 Affiliation` (the
#' em-dash is omitted when affiliation is empty). When an ORCID iD is
#' supplied, a smaller monospace line below shows
#' `ORCID 0000-0000-0000-0000` as a hyperlink to
#' `https://orcid.org/<id>`. The whole block is muted gray so it
#' supports the title typographically rather than competing with it.
#'
#' @keywords internal
#' @noRd
lss_render_authors_block <- function(doc, authors, theme) {
  if (is.null(authors) || length(authors) == 0L) return(doc)
  name_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta + 1L,
    color = theme$color_text
  )
  affil_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta,
    color = theme$color_muted, italic = TRUE
  )
  orcid_label_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_cover_meta - 1L,
    color = theme$color_muted
  )
  orcid_link_props <- officer::fp_text(
    font.family = theme$font_code, font.size = theme$size_cover_meta - 1L,
    color = theme$color_accent, underlined = TRUE
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")
  for (i in seq_along(authors)) {
    a <- authors[[i]]
    chunks <- list(officer::ftext(a$name, prop = name_props))
    if (nzchar(trimws(a$affiliation))) {
      chunks[[length(chunks) + 1L]] <- officer::ftext(
        paste0("  \u2014  ", a$affiliation), prop = affil_props
      )
    }
    doc <- officer::body_add_fpar(
      doc,
      do.call(officer::fpar, c(chunks, list(
        fp_p = officer::fp_par(
          text.align = "center",
          padding.top = if (i == 1L) 6 else 2,
          padding.bottom = if (nzchar(trimws(a$orcid))) 0 else 2
        )
      )))
    )
    if (nzchar(trimws(a$orcid))) {
      orcid_id <- trimws(a$orcid)
      orcid_url <- paste0("https://orcid.org/", orcid_id)
      doc <- officer::body_add_fpar(
        doc,
        officer::fpar(
          officer::ftext(paste0(theme$chrome$orcid_label, " "), prop = orcid_label_props),
          officer::hyperlink_ftext(
            href = orcid_url, text = orcid_id, prop = orcid_link_props
          ),
          fp_p = officer::fp_par(text.align = "center", padding.bottom = 2)
        )
      )
    }
  }
  doc
}

#' Render the optional free-form description block on the cover page
#'
#' Splits the input string on newlines (each becomes a centered line).
#' Within each line, any `http://` or `https://` URL token is rendered
#' as a clickable hyperlink (officer's `hyperlink_ftext`), so a DOI
#' permalink or article URL becomes navigable. Trailing punctuation
#' (`.,;:)`) attached to a URL is stripped from the link target and
#' kept as plain text so the reader sees `(see https://example.org).`
#' with the URL only spanning the actual address.
#'
#' @keywords internal
#' @noRd
lss_render_description_block <- function(doc, description, theme) {
  if (is.null(description) || !nzchar(trimws(description))) return(doc)
  text_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta,
    color = theme$color_muted, italic = TRUE
  )
  link_props <- officer::fp_text(
    font.family = theme$font_body, font.size = theme$size_cover_meta,
    color = theme$color_accent, italic = TRUE
  )
  doc <- officer::body_add_par(doc, "", style = "Normal")
  lines <- strsplit(description, "\n", fixed = TRUE)[[1L]]
  for (li in seq_along(lines)) {
    chunks <- lss_split_text_urls(lines[li], text_props, link_props)
    if (length(chunks) == 0L) next
    doc <- officer::body_add_fpar(
      doc,
      do.call(officer::fpar, c(chunks, list(
        fp_p = officer::fp_par(
          text.align = "center",
          padding.top = if (li == 1L) 8 else 0,
          padding.bottom = 2
        )
      )))
    )
  }
  doc
}

#' Split a text fragment into alternating plain-text and hyperlink
#' chunks based on detected `http(s)://...` URLs.
#'
#' Trailing punctuation common at sentence ends is moved out of the
#' link so the URL target stays clean. Returns a list of
#' `officer::ftext()` / `officer::hyperlink_ftext()` chunks suitable
#' for splicing into `officer::fpar()`.
#'
#' @keywords internal
#' @noRd
lss_split_text_urls <- function(text, text_props, link_props) {
  if (!nzchar(text)) return(list())
  pattern <- "https?://[^\\s]+"
  m <- gregexpr(pattern, text, perl = TRUE)[[1L]]
  if (m[1L] == -1L) {
    return(list(officer::ftext(text, prop = text_props)))
  }
  starts <- as.integer(m)
  lens   <- attr(m, "match.length")
  out <- list()
  cursor <- 1L
  for (k in seq_along(starts)) {
    s <- starts[k]
    e <- s + lens[k] - 1L
    if (s > cursor) {
      out[[length(out) + 1L]] <- officer::ftext(
        substr(text, cursor, s - 1L), prop = text_props
      )
    }
    url <- substr(text, s, e)
    # Strip trailing sentence punctuation from the link target.
    trail <- ""
    while (nchar(url) > 0L &&
           substr(url, nchar(url), nchar(url)) %in% c(".", ",", ";", ":", ")", "]", "}", "!", "?")) {
      trail <- paste0(substr(url, nchar(url), nchar(url)), trail)
      url <- substr(url, 1L, nchar(url) - 1L)
    }
    out[[length(out) + 1L]] <- officer::hyperlink_ftext(
      href = url, text = url, prop = link_props
    )
    if (nzchar(trail)) {
      out[[length(out) + 1L]] <- officer::ftext(trail, prop = text_props)
    }
    cursor <- e + 1L
  }
  if (cursor <= nchar(text)) {
    out[[length(out) + 1L]] <- officer::ftext(
      substr(text, cursor, nchar(text)), prop = text_props
    )
  }
  out
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
      fp_p = officer::fp_par(padding.top = 14, padding.bottom = 0)
    )
  )
}

#' Thin breathing space between the meta table and the item table
#' so they read as two separate panels rather than one continuous block.
#' @keywords internal
#' @noRd
lss_render_intra_item_gap <- function(doc, theme) {
  officer::body_add_fpar(
    doc,
    officer::fpar(
      officer::ftext(" ", prop = officer::fp_text(
        font.family = theme$font_body,
        font.size = 4
      ))
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
    ft <- flextable::align(ft, j = "code", align = "center", part = "body")
    ft <- flextable::width(ft, j = "code", width = 0.6, unit = "in")
  }
  ft <- flextable::valign(ft, valign = "top", part = "all")
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
      theme$chrome$audit_findings_title,
      prop = officer::fp_text(
        font.family = theme$font_body, font.size = theme$size_heading1,
        bold = TRUE, color = theme$color_primary
      )
    )),
    style = "heading 1"
  )
  summary_line <- sprintf(
    theme$chrome$audit_summary_fmt,
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
    severity = theme$chrome$audit_col_severity,
    check    = theme$chrome$audit_col_check,
    location = theme$chrome$audit_col_location,
    language = theme$chrome$audit_col_language,
    message  = theme$chrome$audit_col_message
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
  thin <- officer::fp_border(color = theme$color_grid, width = 0.5)
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

#' Display label for a relevance expression
#'
#' When `theme` is supplied, the localized "All" string from
#' `theme$chrome$filter_all` is used; otherwise English (audit text
#' generation does not thread the chrome through).
#' @keywords internal
#' @noRd
lss_relevance_label <- function(x, theme = NULL) {
  if (is.null(x) || is.na(x) || !nzchar(x)) return("\u2014")
  if (identical(x, "1")) {
    return(if (!is.null(theme)) theme$chrome$filter_all else "All")
  }
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
lss_humanize_relevance <- function(x, theme = NULL) {
  if (is.null(x) || is.na(x) || !nzchar(x) || identical(x, "1")) {
    return(if (!is.null(theme)) theme$chrome$filter_all else "All")
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
  # Comparison operators rendered with Unicode math symbols so they read
  # at a glance for a methodologist: U+2260 (not-equal), U+2264 (<=),
  # U+2265 (>=). Order matters: substitute the two-character forms
  # first so the single `==` rule does not consume the `=` of `!=` /
  # `<=` / `>=`.
  s <- gsub("\\s*!=\\s*", " \u2260 ", s)
  s <- gsub("\\s*<=\\s*", " \u2264 ", s)
  s <- gsub("\\s*>=\\s*", " \u2265 ", s)
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
  ft <- flextable::valign(ft, valign = "top", part = "all")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  # Pro alignment by content type:
  # - "No" (item number) is a number, right-aligned like in the variable
  #   index and financial tables -- digits stack on the units column so
  #   1, 12, 587 read as a clean scan.
  # - "Mandatory" is a short yes/no token: centered.
  # - All other columns hold text (codes, type label, filter expression),
  #   left-aligned by default.
  ft <- flextable::align(ft, align = "right",  j = "No", part = "all")
  ft <- flextable::align(ft, align = "center", j = "Mandatory", part = "all")
  # Column widths sum to theme$content_width_in (6.30 in). Calibrated to
  # the actual content using 11 pt Consolas (~0.092 in/char) for Variable
  # and 8 pt Calibri (~0.055 in/char) for the others:
  #   No        0.35  - holds up to 3 digits in 11 pt body font (max #999).
  #   Variable  1.95  - holds identifiers up to ~20 chars (e.g.
  #                     `semestrechargetrav_1`) without wrapping the
  #                     trailing digit; codes >=21 chars still wrap.
  #   Type      1.25  - holds 8 pt labels up to ~22 chars; long labels
  #                     ("Multiple choice with comments", 28 chars) wrap
  #                     to a second line, which is acceptable.
  #   Mandatory 0.70  - header "Mandatory" (9 chars at 8 pt bold ~0.55 in)
  #                     fits; body content is a tight yes/no token.
  #   Filter    2.05  - holds the human-readable form on top and the raw
  #                     LimeSurvey expression in 7 pt italic mono below.
  ft <- flextable::width(ft, j = "No", width = 0.35, unit = "in")
  ft <- flextable::width(ft, j = "Variable", width = 1.95, unit = "in")
  ft <- flextable::width(ft, j = "Type", width = 1.25, unit = "in")
  ft <- flextable::width(ft, j = "Mandatory", width = 0.70, unit = "in")
  ft <- flextable::width(ft, j = "Filter", width = 2.05, unit = "in")
  flextable::body_add_flextable(doc, ft, align = "left")
}

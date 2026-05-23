#' Render the audit alone as a focused Word document
#'
#' Build a short, action-oriented `.docx` that contains only the audit
#' findings: the same cover page as the full review document, the summary
#' counts, then one table per severity (errors, warnings, notes) listing
#' every finding with its location and message. Use this for QA follow-up
#' or to share the issues with a colleague without distributing the full
#' questionnaire.
#'
#' @param lss An `lss` object returned by [parse_lss()].
#' @param output Path to the `.docx` file to create.
#' @param languages Character vector of language codes for the cover page.
#'   Defaults to all languages of the survey.
#' @param logo Optional path to a PNG or JPEG image to display at the top
#'   of the cover page. `NULL` (default) keeps the cover logo-free.
#' @param logo_width,logo_height Image dimensions in inches. Defaults
#'   tuned to a 2:1 logo (1.5 x 0.75 inches).
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' lss <- parse_lss(system.file("extdata", "limesurvey_survey_751689.lss",
#'   package = "lssdoc"
#' ))
#' render_lss_audit_docx(lss, tempfile(fileext = ".docx"))
#' }
#' @export
render_lss_audit_docx <- function(lss, output, languages = NULL,
                                  logo = NULL,
                                  logo_width = 1.5,
                                  logo_height = 0.75) {
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
  theme <- lss_render_theme()
  audit <- audit_lss(lss)

  doc <- officer::read_docx()
  doc <- lss_render_cover(
    doc, lss, model, theme,
    subtitle = "Questionnaire audit report",
    logo = logo, logo_width = logo_width, logo_height = logo_height
  )
  doc <- officer::body_add_break(doc)
  doc <- lss_render_audit_full(doc, audit, theme)

  section <- lss_render_section_props("A4-portrait", length(model$languages))
  doc <- officer::body_set_default_section(doc, section)
  print(doc, target = output)
  invisible(output)
}

#' Render the full audit body: summary plus one table per severity
#' @keywords internal
#' @noRd
lss_render_audit_full <- function(doc, audit, theme) {
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

  if (audit$n_findings == 0) {
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(
        "No anomalies detected.",
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_question,
          color = theme$color_text, italic = TRUE
        )
      ))
    )
    return(doc)
  }

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

  sev_meta <- list(
    error = list(title = "Errors", color = theme$color_error),
    warning = list(title = "Warnings", color = theme$color_warning),
    note = list(title = "Notes", color = theme$color_note)
  )
  for (sev in names(sev_meta)) {
    rows <- audit$findings[audit$findings$severity == sev, , drop = FALSE]
    if (nrow(rows) == 0) next
    doc <- officer::body_add_par(doc, "", style = "Normal")
    doc <- officer::body_add_fpar(
      doc,
      officer::fpar(officer::ftext(
        sprintf("%s (%d)", sev_meta[[sev]]$title, nrow(rows)),
        prop = officer::fp_text(
          font.family = theme$font_body, font.size = theme$size_heading2,
          bold = TRUE, color = sev_meta[[sev]]$color
        )
      )),
      style = "heading 2"
    )
    ft <- flextable::flextable(
      rows[, c("check", "location", "language", "message"), drop = FALSE]
    )
    ft <- flextable::set_header_labels(
      ft, check = "Check", location = "Location",
      language = "Lang", message = "Message"
    )
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
    ft <- flextable::width(ft, j = "check", width = 1.6, unit = "in")
    ft <- flextable::width(ft, j = "location", width = 2.2, unit = "in")
    ft <- flextable::width(ft, j = "language", width = 0.5, unit = "in")
    ft <- flextable::width(ft, j = "message", width = 3.6, unit = "in")
    doc <- flextable::body_add_flextable(doc, ft, align = "center")
  }
  doc
}

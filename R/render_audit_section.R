# The audit section of render_documentation(): the findings of audit_lss()
# in one table near the top of the document.
#
# Extracted from R/render_lss_docx.R.

#' Render the audit summary section
#'
#' `audit` is the result of `audit_lss()`: a summary line of its counts,
#' then one row per finding.
#' @keywords internal
#' @noRd
lss_render_audit_section <- function(doc, audit, theme) {
  doc <- lss_render_section_heading(
    doc, theme, theme$chrome$audit_findings_title,
    lss_section_bookmark("audit")
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
  # The machine-readable `check` id (e.g. "array_scale_missing_answers")
  # is dropped from the table: the message already states the issue in
  # plain language, so the id was reviewer-facing jargon eating width.
  fdf <- audit$findings[, c("severity", "location", "language", "message"), drop = FALSE]
  ft <- flextable::flextable(fdf)
  ft <- flextable::set_header_labels(
    ft,
    severity = theme$chrome$audit_col_severity,
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
  ft <- flextable::vline(ft, border = thin, part = "all")
  ft <- flextable::vline_left(ft, border = thin, part = "all")
  ft <- flextable::vline_right(ft, border = thin, part = "all")
  ft <- flextable::valign(ft, valign = "top", part = "body")
  ft <- flextable::valign(ft, valign = "center", part = "header")
  ft <- flextable::padding(ft, padding = 2, part = "all")
  # Cell-symmetric for the readable columns (severity, check, location,
  # message): header and body both left -- the column label reads as a
  # word above text paragraphs. The "language" column is asymmetric on
  # purpose: header left (the word "Language") above body codes
  # centered (the two-letter atoms `fr`, `de`, `en`).
  ft <- flextable::align(ft, align = "left",   part = "all")
  ft <- flextable::align(ft, align = "center", j = "language", part = "body")
  # Column widths sum to theme$content_width_in (6.30 in) so the audit
  # table matches the body width instead of overflowing the page. The
  # `language` column is wide enough for the full "Language" header
  # (it was 0.5 in, which wrapped the final letter).
  ft <- flextable::width(ft, j = "severity", width = 0.70, unit = "in")
  ft <- flextable::width(ft, j = "location", width = 1.85, unit = "in")
  ft <- flextable::width(ft, j = "language", width = 0.70, unit = "in")
  # message absorbs the surplus so the table spans the full body width:
  # 3.05 in in portrait (6.30 total), wider in landscape / A3.
  ft <- flextable::width(ft, j = "message",
                         width = theme$content_width_in - 3.25, unit = "in")
  doc <- flextable::body_add_flextable(doc, ft, align = "left")
  doc
}


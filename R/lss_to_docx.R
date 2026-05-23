#' Convert a `.lss` file to a Word review document
#'
#' One-shot pipeline: parse the LimeSurvey `.lss` file with [parse_lss()]
#' and render it to a `.docx` review document with [render_lss_docx()]. Use
#' the underlying functions directly when you need finer control over the
#' audit or the layout.
#'
#' @param input Path to a `.lss` file.
#' @param output Path to the `.docx` file to create.
#' @param ... Additional arguments forwarded to [render_lss_docx()].
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' lss_to_docx(
#'   system.file("extdata", "hesav_2026.lss", package = "lssdoc"),
#'   "rapport.docx"
#' )
#' }
#' @export
lss_to_docx <- function(input, output, ...) {
  render_lss_docx(parse_lss(input), output = output, ...)
}

#' Convert a `.lss` file to a PDF review document
#'
#' Same pipeline as [lss_to_docx()], but the generated `.docx` is then
#' converted to PDF locally via [lss_docx_to_pdf()] (LibreOffice or Word).
#' Nothing leaves the user's machine.
#'
#' @param input Path to a `.lss` file.
#' @param output Path to the `.pdf` file to create.
#' @param ... Additional arguments forwarded to [render_lss_docx()].
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' lss_to_pdf(
#'   system.file("extdata", "hesav_2026.lss", package = "lssdoc"),
#'   "rapport.pdf"
#' )
#' }
#' @export
lss_to_pdf <- function(input, output, ...) {
  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)
  render_lss_docx(parse_lss(input), output = tmp_docx, ...)
  lss_docx_to_pdf(tmp_docx, output)
}

#' Convert a `.lss` file to a Word audit-only document
#'
#' Pipeline counterpart of [lss_to_docx()] for the focused audit report:
#' parses the file, runs the audit, and writes a `.docx` that contains only
#' the audit findings. Use this for QA follow-up.
#'
#' @inheritParams lss_to_docx
#' @param ... Additional arguments forwarded to [render_lss_audit_docx()].
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' lss_audit_to_docx(
#'   system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc"),
#'   "audit.docx"
#' )
#' }
#' @export
lss_audit_to_docx <- function(input, output, ...) {
  render_lss_audit_docx(parse_lss(input), output = output, ...)
}

#' Convert a `.lss` file to a PDF audit-only document
#'
#' Same as [lss_audit_to_docx()] but converts the resulting `.docx` to PDF
#' via [lss_docx_to_pdf()].
#'
#' @inheritParams lss_to_docx
#' @param ... Additional arguments forwarded to [render_lss_audit_docx()].
#'
#' @return The `output` path, invisibly.
#'
#' @examples
#' \dontrun{
#' lss_audit_to_pdf(
#'   system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc"),
#'   "audit.pdf"
#' )
#' }
#' @export
lss_audit_to_pdf <- function(input, output, ...) {
  tmp_docx <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp_docx), add = TRUE)
  render_lss_audit_docx(parse_lss(input), output = tmp_docx, ...)
  lss_docx_to_pdf(tmp_docx, output)
}

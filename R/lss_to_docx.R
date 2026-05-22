#' Convert a LimeSurvey `.lss` file to a Word review document
#'
#' Convenience wrapper that runs the full pipeline in one call: it parses the
#' `.lss` file with [parse_lss()] and renders it to a `.docx` file with
#' [render_lss_docx()]. Use the underlying functions directly when you need
#' finer control over the audit or the layout.
#'
#' @param input Path to a `.lss` file.
#' @param output Path to the `.docx` file to create.
#' @param ... Additional arguments passed on to [render_lss_docx()], such as
#'   `languages`, `layout`, or `page_format`.
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
  lss <- parse_lss(input)
  render_lss_docx(lss, output = output, ...)
}

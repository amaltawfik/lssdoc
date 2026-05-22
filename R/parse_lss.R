#' Parse a LimeSurvey `.lss` file
#'
#' Read a LimeSurvey survey structure export (`.lss`, an XML file) and turn
#' it into a structured `lss` object that the rest of the package can audit
#' and render. Parsing is fully local: the file is never uploaded anywhere.
#'
#' @param path Path to a `.lss` file.
#'
#' @return An object of class `lss`: a list holding the survey languages,
#'   groups, questions, subquestions, answers, and the raw attributes, with
#'   user-supplied text (titles, labels, help) preserved verbatim.
#'
#' @details
#' The `.lss` format is a LimeSurvey XML export containing, among others, the
#' `<languages>`, `<groups>`, `<questions>`, `<subquestions>`, `<answers>`,
#' and `<question_attributes>` sections. `parse_lss()` reads these into tidy
#' tables without mutating any user-facing identifier or text.
#'
#' @examples
#' \dontrun{
#' lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
#'   package = "lssdoc"
#' ))
#' }
#' @export
parse_lss <- function(path) {
  if (!is.character(path) || length(path) != 1L) {
    lssdoc_abort(
      "{.arg path} must be a single file path.",
      class = "lssdoc_bad_path"
    )
  }
  if (!file.exists(path)) {
    lssdoc_abort(
      "Cannot find a file at {.path {path}}.",
      class = "lssdoc_file_not_found"
    )
  }
  lssdoc_abort(
    "{.fn parse_lss} is not implemented yet.",
    class = "lssdoc_not_implemented"
  )
}

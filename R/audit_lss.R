#' Audit a parsed LimeSurvey structure for reviewable anomalies
#'
#' Inspect an `lss` object and flag anomalies that can be detected without
#' any AI: missing translations, empty labels, duplicate codes, broken
#' subquestion or answer references, inconsistent option sets across
#' languages, and similar structural issues. The result is meant to guide a
#' human reviewer, not to silently correct anything.
#'
#' @param lss An `lss` object returned by [parse_lss()].
#'
#' @return An object of class `lss_audit`: a list of detected anomalies with
#'   a `print()` method that summarises them for the console.
#'
#' @examples
#' \dontrun{
#' lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
#'   package = "lssdoc"
#' ))
#' audit <- audit_lss(lss)
#' print(audit)
#' }
#' @export
audit_lss <- function(lss) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      "{.arg lss} must be an {.cls lss} object from {.fn parse_lss}.",
      class = "lssdoc_bad_lss"
    )
  }
  lssdoc_abort(
    "{.fn audit_lss} is not implemented yet.",
    class = "lssdoc_not_implemented"
  )
}

#' @export
print.lss_audit <- function(x, ...) {
  cli::cli_abort(
    "{.fn print.lss_audit} is not implemented yet.",
    class = "lssdoc_not_implemented"
  )
}

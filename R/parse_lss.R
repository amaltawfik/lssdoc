#' Parse a LimeSurvey `.lss` file
#'
#' Read a LimeSurvey survey structure export (`.lss`, an XML file) and turn
#' it into a structured `lss` object that the rest of the package can audit
#' and render. Parsing is fully local: the file is never uploaded anywhere.
#'
#' @param path Path to a `.lss` file.
#'
#' @return An object of class `lss`: a list holding the survey languages and
#'   metadata plus one data frame per `.lss` section. Structural sections
#'   (`surveys`, `groups`, `questions`, `subquestions`, `answers`,
#'   `question_attributes`, `conditions`) are kept separate from the
#'   localized text sections (`survey_language_settings`, `group_l10ns`,
#'   `question_l10ns`, `answer_l10ns`), which carry the per-language titles,
#'   labels, and help texts. All values are read verbatim as character.
#'
#' @details
#' The `.lss` format is a LimeSurvey XML export. Since DBVersion 4xx/7xx the
#' translatable text lives in dedicated localization sections
#' (`*_l10ns`), keyed by language, while the structural sections hold
#' identifiers and settings. `parse_lss()` reads every section into a tidy
#' data frame without mutating any user-facing identifier or text. A field
#' that is present but empty (e.g. `<help/>`) is read as `""`; a field that
#' is absent from a row is read as `NA`.
#'
#' @examples
#' lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
#'   package = "lssdoc"
#' ))
#' lss$languages
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

  doc <- tryCatch(
    xml2::read_xml(path),
    error = function(e) {
      lssdoc_abort(
        c(
          "{.path {path}} is not valid XML.",
          "x" = conditionMessage(e)
        ),
        class = "lssdoc_invalid_xml"
      )
    }
  )

  doc_type <- lss_scalar(doc, "LimeSurveyDocType")
  if (is.na(doc_type) || doc_type != "Survey") {
    lssdoc_abort(
      c(
        "{.path {path}} does not look like a LimeSurvey survey export.",
        "i" = "Expected {.field LimeSurveyDocType} {.val Survey}, but found
               {.val {doc_type}}."
      ),
      class = "lssdoc_not_a_survey"
    )
  }

  languages <- xml2::xml_text(
    xml2::xml_find_all(doc, "/document/languages/language")
  )
  surveys <- lss_section(doc, "surveys")
  base_language <- if (!is.null(surveys) && "language" %in% names(surveys)) {
    surveys$language[1]
  } else {
    NA_character_
  }

  structure(
    list(
      file = path,
      db_version = lss_scalar(doc, "DBVersion"),
      doc_type = doc_type,
      languages = languages,
      base_language = base_language,
      surveys = surveys,
      survey_language_settings = lss_section(doc, "surveys_languagesettings"),
      groups = lss_section(doc, "groups"),
      group_l10ns = lss_section(doc, "group_l10ns"),
      questions = lss_section(doc, "questions"),
      question_l10ns = lss_section(doc, "question_l10ns"),
      subquestions = lss_section(doc, "subquestions"),
      answers = lss_section(doc, "answers"),
      answer_l10ns = lss_section(doc, "answer_l10ns"),
      question_attributes = lss_section(doc, "question_attributes"),
      conditions = lss_section(doc, "conditions")
    ),
    class = "lss"
  )
}

#' Read a scalar element directly under `<document>`
#'
#' @return A length-one character, or `NA_character_` if the element is
#'   absent.
#' @keywords internal
#' @noRd
lss_scalar <- function(doc, name) {
  node <- xml2::xml_find_first(doc, paste0("/document/", name))
  if (inherits(node, "xml_missing")) {
    return(NA_character_)
  }
  xml2::xml_text(node)
}

#' Read one `<fields>`/`<rows>` section into a data frame
#'
#' Returns a data frame with one column per declared `<fieldname>` and one
#' row per `<row>`, all character. A present-but-empty element reads as `""`;
#' an element absent from a row reads as `NA`. Returns `NULL` if the section
#' is missing, and a zero-row data frame (with columns) if it declares
#' fields but has no rows.
#'
#' @keywords internal
#' @noRd
lss_section <- function(doc, name) {
  node <- xml2::xml_find_first(doc, paste0("/document/", name))
  if (inherits(node, "xml_missing")) {
    return(NULL)
  }

  fields <- xml2::xml_text(xml2::xml_find_all(node, "./fields/fieldname"))
  if (length(fields) == 0) {
    return(NULL)
  }

  rows <- xml2::xml_find_all(node, "./rows/row")
  cols <- lapply(fields, function(field) {
    if (length(rows) == 0) {
      return(character(0))
    }
    vapply(
      rows,
      function(row) {
        cell <- xml2::xml_find_first(row, paste0("./", field))
        if (inherits(cell, "xml_missing")) {
          NA_character_
        } else {
          xml2::xml_text(cell)
        }
      },
      character(1)
    )
  })

  names(cols) <- fields
  as.data.frame(cols, stringsAsFactors = FALSE, check.names = FALSE)
}

#' @export
print.lss <- function(x, ...) {
  cli::cli_h1("LimeSurvey structure (lss)")
  cli::cli_text("{.field File}: {.path {x$file}}")
  cli::cli_text("{.field Languages}: {.val {x$languages}}")
  if (!is.na(x$base_language)) {
    cli::cli_text("{.field Base language}: {.val {x$base_language}}")
  }
  n <- function(df) if (is.null(df)) 0L else nrow(df)
  counts <- c(
    Groups = n(x$groups),
    Questions = n(x$questions),
    Subquestions = n(x$subquestions),
    Answers = n(x$answers)
  )
  cli::cli_text(
    "{.field Counts}: ",
    paste(names(counts), counts, sep = " ", collapse = ", ")
  )
  invisible(x)
}

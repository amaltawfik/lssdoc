#' Render a LimeSurvey questionnaire as it reads, to Word or PDF
#'
#' Build a short document that shows a questionnaire the way a respondent
#' goes through it, in one language: each question with its number and its
#' text, then its subquestions and its answer options, one per line. The
#' codes of LimeSurvey are printed in small gray type next to what they
#' name. It is the copy to read through, and the one to hand to an ethics
#' committee or a translator. For the full documentation of a survey, with
#' the type, the filter expression and the attributes of every question and
#' with several languages side by side, see [render_documentation()].
#' Rendering uses the suggested package \pkg{officer}, which must be
#' installed. When \pkg{gdtools} is installed too, it measures the text
#' to align the codes. Without it the widths are estimated.
#'
#' @param input Either a path to a `.lss` file or an `lss` object returned
#'   by [read_lss()].
#' @param output Path of the file to create. The extension sets the
#'   format, as in [render_documentation()]: `.docx` writes a Word
#'   document, `.pdf` converts that document locally with LibreOffice (or
#'   Word, on Windows).
#' @param language Language code of the questionnaire to render, one only.
#'   `NULL` (default) takes the base language of the survey. A survey in
#'   three languages gives three calls and three files. To see languages
#'   side by side, use [render_documentation()]. A text that has no
#'   translation in `language` is printed in the base language and marked
#'   as not translated, and a warning lists the elements concerned.
#' @param show_codes Logical. If `TRUE` (default), print the codes of
#'   LimeSurvey in small gray type: the code of each question, which is
#'   the name of its variable, the code of each subquestion and of each
#'   answer option, and the conditions under which a group, a question or
#'   a subquestion is shown, which are written with those codes
#'   (`if Q11 = 2`). The formula of a computed question comes with them.
#'   `FALSE` removes them all.
#' @param show_hidden Logical. Whether to print the questions that
#'   LimeSurvey hides from respondents (attribute `hidden`). Each takes
#'   one line, without a number. Defaults to `show_codes`, so that
#'   `show_codes = FALSE` alone gives the copy a respondent reads.
#' @param show_help Logical. If `TRUE` (default), print the help text and
#'   the validation tip of a question under its text.
#' @param show_group_description Logical. If `TRUE` (default), print the
#'   description of a group under its heading.
#' @param show_toc Logical. If `TRUE` (default), print a table of contents
#'   of the groups under the title, each group with the numbers of its
#'   questions. It is skipped when the survey has fewer than two groups.
#' @param show_description,show_welcome,show_consent,show_endtext Logical,
#'   all `TRUE` by default. The texts a respondent reads before the first
#'   question and after the last one: the description of the survey, its
#'   welcome text, its privacy notice with the label of the consent
#'   checkbox, and its end text.
#' @param max_options Number of answer options a long list is cut down to,
#'   `12` by default, which keeps a scale from 0 to 10 with a "does not
#'   apply" option complete. An abridged list keeps half of these options
#'   from its start and half from its end, around an ellipsis, and gives
#'   the total. A list is abridged only when that makes it shorter, so a
#'   list of `max_options + 1` options is still printed in full. `Inf`
#'   never abridges. The rows of an array are always printed in full.
#' @param title Title of the document. `NULL` (default) takes the title of
#'   the survey in `language`.
#' @param subtitle Line printed under the title. `NULL` (default) prints
#'   "Questionnaire" in the chrome language. The `.lss` file stores no
#'   subtitle.
#' @param logo Optional path to a PNG or JPEG image printed above the
#'   title. The `.lss` file embeds no logo.
#' @param logo_width,logo_height Size of the logo in inches, `1.5` and
#'   `0.75` by default.
#' @param font,font_code Body font and font of the codes. `NULL` (default)
#'   keeps Calibri and Consolas, as in [render_documentation()].
#' @param colors Optional named list of hex colors, with the names
#'   [render_documentation()] accepts. This document uses `"primary"` for
#'   its title, its headings and the question numbers, `"accent"` for the
#'   links of the table of contents, `"text"` for what is asked and
#'   `"muted"` for the codes.
#' @param chrome_lang Language of the labels the document adds, such as
#'   "Answer:", "For each row:" and the legend: one of `"en"`, `"fr"`,
#'   `"de"`, `"es"`, `"it"`. `NULL` (default) follows `language` when it
#'   is one of them and falls back to `"en"`. With `language = "de"` and
#'   `chrome_lang = "fr"` the questions are in German and the labels in
#'   French.
#' @param proofing_lang Language declared to Word for its spelling checker
#'   and its hyphenation, as a tag such as `"fr-CH"` or `"de-CH"`. `NULL`
#'   (default) derives it from `language`: `fr` gives `fr-FR`.
#' @param base_size Size of the question text in points, `10` by default.
#'   Every other size follows from it. Accepted range: 7 to 16.
#' @param ... Not used. It catches the arguments of the function that
#'   carried this name before lssdoc 0.4.0, now [render_documentation()],
#'   so that the error can name that function.
#'
#' @section What the document shows:
#' Questions are numbered from 1 to n as in [render_documentation()], one
#' number per question put to the respondent. A text display and an
#' equation keep their place without a number, and the left column says
#' "text" or "computed" where the number would be. A mandatory question
#' carries an asterisk after its text.
#'
#' The answer options come below the question, one per line. A multiple
#' choice lists its options and says that several answers are possible. An
#' array lists its rows, then the options that apply to each row. A yes/no
#' question and a gender question list the two options LimeSurvey builds
#' in. A 5-point choice gives its positions on one line, "1 2 3 4 5", and
#' an array of 5-point or 10-point choices does the same under its rows.
#' An array of numbers or of texts lists its rows, then its columns, then
#' what each cell takes: "For each row and each column: number from 0 to
#' 10". A question without options says what it expects, for instance
#' "Answer: number", with the prefix and the suffix of the input field
#' when it has them.
#'
#' The conditions set in LimeSurvey are printed with the codes, in the
#' same gray type: after the code of a question ("if Q11 = 2"), after the
#' label of a subquestion and under the title of a group ("Group shown if
#' Q11 = 2"). A condition that cannot be put in that plain form shows as
#' "filtered", or "Group with a filter" for a group:
#' [render_documentation()] prints its expression. A computed question
#' gives its formula after its code ("score = Q1 + Q2"), as LimeSurvey
#' holds it.
#'
#' What the respondent only reads (the welcome text, the privacy notice,
#' the text displays, the group descriptions) is set in gray and
#' justified. What is asked is left-aligned.
#'
#' The type of a question, its attributes, the audit, the variable index
#' and the quotas belong to [render_documentation()].
#'
#' @section Change in lssdoc 0.4.0:
#' Before 0.4.0, `render_questionnaire()` rendered the full documentation
#' of a survey. That function is now [render_documentation()], with the
#' same arguments. A call that passes one of those arguments here
#' (`languages`, `template`, `show_audit` and the others) stops with an
#' error that names it. During the 0.4.x releases the first call of a
#' session prints a message that recalls the change.
#'
#' @return The `output` path, invisibly.
#'
#' @seealso [render_documentation()] for the full documentation of a
#'   survey; [render_audit()] for the audit report; [read_lss()] to parse
#'   a survey once and render it several times.
#'
#' @examples
#' \donttest{
#' if (requireNamespace("officer", quietly = TRUE)) {
#'   lss <- read_lss(
#'     system.file("extdata", "demo_survey.lss", package = "lssdoc")
#'   )
#'
#'   # The questionnaire in the base language of the survey
#'   out <- tempfile(fileext = ".docx")
#'   render_questionnaire(lss, out)
#'
#'   # The German version without codes: the copy a respondent reads
#'   out_de <- tempfile(fileext = ".docx")
#'   render_questionnaire(lss, out_de, language = "de", show_codes = FALSE)
#'
#'   unlink(c(out, out_de))
#' }
#' }
#' @export
render_questionnaire <- function(
  input,
  output,
  language = NULL,
  show_codes = TRUE,
  show_hidden = show_codes,
  show_help = TRUE,
  show_group_description = TRUE,
  show_toc = TRUE,
  show_description = TRUE,
  show_welcome = TRUE,
  show_consent = TRUE,
  show_endtext = TRUE,
  max_options = 12L,
  title = NULL,
  subtitle = NULL,
  logo = NULL,
  logo_width = 1.5,
  logo_height = 0.75,
  font = NULL,
  font_code = NULL,
  colors = NULL,
  chrome_lang = NULL,
  proofing_lang = NULL,
  base_size = 10L,
  ...
) {
  # `...` only catches the arguments of the function that carried this name
  # before 0.4.0. Checked first, before the survey is read, so that an old
  # call stops on the rename and not on some later argument.
  dots <- names(match.call(expand.dots = FALSE)$...)
  n_dots <- length(match.call(expand.dots = FALSE)$...)
  if (n_dots > 0L) {
    if (is.null(dots)) dots <- rep("", n_dots)
    old <- setdiff(
      names(formals(render_documentation)),
      names(formals(render_questionnaire))
    )
    renamed <- dots[dots %in% old]
    if (length(renamed) > 0L) {
      name <- renamed[1]
      lssdoc_abort(
        c(
          "{.arg {name}} is an argument of {.fn render_documentation}, the new name of the function that rendered the full document before lssdoc 0.4.0.",
          "i" = paste0(
            "Since 0.4.0, {.fn render_questionnaire} renders the questionnaire as it reads, in one language.",
            if (identical(name, "languages")) " Its argument is {.arg language}." else ""
          )
        ),
        class = "lssdoc_function_renamed"
      )
    }
    unknown <- dots[nzchar(dots)]
    if (length(unknown) > 0L) {
      lssdoc_abort(
        "{.arg {unknown}} {?is/are} not {?an argument/arguments} of {.fn render_questionnaire}.",
        class = "lssdoc_unknown_argument"
      )
    }
    lssdoc_abort(
      "{.fn render_questionnaire} takes no unnamed argument after {.arg output}.",
      class = "lssdoc_unknown_argument"
    )
  }

  lss <- lss_resolve_input(input)
  if (!is.character(output) || length(output) != 1L || is.na(output)) {
    lssdoc_abort(
      "{.arg output} must be a single file path.",
      class = "lssdoc_bad_output"
    )
  }
  format <- lss_detect_output_format(output)

  # One content language per document.
  if (is.null(language)) {
    language <- lss$base_language
    if (is.null(language) || is.na(language) || !nzchar(language)) {
      language <- lss$languages[1]
    }
  }
  if (!is.character(language) || anyNA(language) || length(language) == 0L) {
    lssdoc_abort(
      "{.arg language} must be {.code NULL} or a single language code.",
      class = "lssdoc_bad_language"
    )
  }
  if (length(language) > 1L) {
    lssdoc_abort(
      c(
        "{.arg language} must be a single language code, not {length(language)}.",
        "i" = "A survey in several languages gives one document per language.",
        "i" = "To see languages side by side, use {.fn render_documentation}."
      ),
      class = "lssdoc_one_language"
    )
  }
  lss_resolve_languages(lss, language)

  flags <- list(
    show_codes = show_codes, show_hidden = show_hidden, show_help = show_help,
    show_group_description = show_group_description, show_toc = show_toc,
    show_description = show_description, show_welcome = show_welcome,
    show_consent = show_consent, show_endtext = show_endtext
  )
  for (flag in names(flags)) {
    value <- flags[[flag]]
    if (!is.logical(value) || length(value) != 1L || is.na(value)) {
      lssdoc_abort(
        "{.arg {flag}} must be {.code TRUE} or {.code FALSE}.",
        class = "lssdoc_bad_flag"
      )
    }
  }
  if (!is.numeric(max_options) || length(max_options) != 1L ||
      is.na(max_options) || max_options < 2 ||
      (is.finite(max_options) && max_options != round(max_options))) {
    lssdoc_abort(
      "{.arg max_options} must be a whole number of at least 2, or {.code Inf}.",
      class = "lssdoc_bad_max_options"
    )
  }
  if (!is.null(title) &&
      (!is.character(title) || length(title) == 0L || anyNA(title))) {
    lssdoc_abort(
      "{.arg title} must be {.code NULL} or a string.",
      class = "lssdoc_bad_title"
    )
  }
  if (!is.null(subtitle) &&
      (!is.character(subtitle) || length(subtitle) != 1L || is.na(subtitle))) {
    lssdoc_abort(
      "{.arg subtitle} must be {.code NULL} or a single string.",
      class = "lssdoc_bad_subtitle"
    )
  }
  lss_validate_logo(logo)
  lss_validate_font(font, "font")
  lss_validate_font(font_code, "font_code")
  colors <- lss_validate_colors(colors)
  chrome_lang <- lss_resolve_chrome_lang(chrome_lang, language)
  if (!is.null(proofing_lang) &&
      (!is.character(proofing_lang) || length(proofing_lang) != 1L ||
       is.na(proofing_lang) ||
       !grepl("^[A-Za-z]{2,3}(-[A-Za-z0-9]{1,8})*$", proofing_lang))) {
    lssdoc_abort(
      c(
        "{.arg proofing_lang} must be {.code NULL} or a language tag such as {.val fr-CH}.",
        "i" = "Got {.val {proofing_lang}}."
      ),
      class = "lssdoc_bad_proofing_lang"
    )
  }
  proofing_lang <- lss_q_proofing_lang(language, proofing_lang)
  base_size <- lss_validate_base_size(base_size)
  if (!requireNamespace("officer", quietly = TRUE)) {
    lssdoc_abort(                                       # nocov start
      c(
        "Rendering a {.file .docx} document requires the {.pkg officer} package.",
        "i" = "Install it with {.run install.packages(\"officer\")}."
      ),
      class = "lssdoc_missing_suggest"
    )                                                   # nocov end
  }

  # The name changed meaning in 0.4.0: say so once per session, during the
  # 0.4.x releases.
  rlang::inform(
    paste(
      "Since lssdoc 0.4.0, render_questionnaire() renders the questionnaire",
      "as it reads, in one language. The full documentation now comes from",
      "render_documentation()."
    ),
    .frequency = "once",
    .frequency_id = "lssdoc_render_questionnaire_0.4.0"
  )

  args <- list(
    lss = lss, output = output, language = language,
    show_codes = show_codes, show_hidden = show_hidden, show_help = show_help,
    show_group_description = show_group_description, show_toc = show_toc,
    show_description = show_description, show_welcome = show_welcome,
    show_consent = show_consent, show_endtext = show_endtext,
    max_options = max_options, title = title, subtitle = subtitle,
    logo = logo, logo_width = logo_width, logo_height = logo_height,
    font = font, font_code = font_code, colors = colors,
    chrome_lang = chrome_lang, proofing_lang = proofing_lang,
    base_size = base_size
  )
  do.call(
    if (identical(format, "pdf"))
      .render_questionnaire_pdf
    else
      .render_questionnaire_docx,
    args
  )
}

#' Render the readable questionnaire as PDF via a temporary .docx
#'
#' @keywords internal
#' @noRd
.render_questionnaire_pdf <- function(lss, output, ...) {
  tmp <- tempfile(fileext = ".docx")
  on.exit(unlink(tmp), add = TRUE)
  .render_questionnaire_docx(lss, tmp, ...)
  .docx_to_pdf(tmp, output)
}

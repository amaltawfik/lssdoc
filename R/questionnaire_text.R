# Text helpers of the readable questionnaire (render_questionnaire()): plain
# text from LimeSurvey HTML, French typography, the language declared to
# Word, the plain filter, abridged lists, the legend and the text widths the
# layout is built on. All pure, so they are tested without rendering.

#' Whether a value is missing or blank
#' @keywords internal
#' @noRd
lss_q_blank <- function(x) {
  is.null(x) || length(x) == 0L || is.na(x[1]) || !nzchar(trimws(x[1]))
}

#' A LimeSurvey text on one line: HTML stripped, spaces collapsed
#'
#' The paragraphs and line breaks of the HTML become spaces, as in the
#' prototype of the readable questionnaire. `french = TRUE` applies the
#' French no-break spaces of `lss_q_french()`.
#'
#' @keywords internal
#' @noRd
lss_q_clean <- function(html, french = FALSE) {
  if (lss_q_blank(html)) {
    return("")
  }
  s <- lss_html_to_text(as.character(html[1]))
  s <- trimws(gsub("[[:space:]\u00A0]+", " ", s, perl = TRUE))
  if (isTRUE(french)) s <- lss_q_french(s)
  s
}

#' French typography: a no-break space before `? ! ; :` and `right guillemet`, and
#' after `the left guillemet`, so that a question mark never starts a line
#' @keywords internal
#' @noRd
lss_q_french <- function(s) {
  s <- gsub("[ \u00A0]+(?=[?!;:\u00BB])", "\u00A0", s, perl = TRUE)
  gsub("(?<=\u00AB)[ \u00A0]+", "\u00A0", s, perl = TRUE)
}

#' The paragraphs of an HTML text, for the front and end matter
#'
#' One element per paragraph of the HTML, a line break starting a new
#' one, each with its plain text and whether it is entirely bold (a small
#' heading, kept with the paragraph that follows it). A list item keeps a
#' bullet. Empty paragraphs are dropped.
#'
#' @keywords internal
#' @noRd
lss_q_html_paras <- function(html, french = FALSE) {
  if (lss_q_blank(html)) {
    return(list())
  }
  out <- list()
  for (b in lss_html_to_blocks(as.character(html[1]))) {
    pieces <- list(list())
    for (r in b$runs) {
      if (isTRUE(r$linebreak)) {
        pieces[[length(pieces) + 1L]] <- list()
      } else {
        pieces[[length(pieces)]][[length(pieces[[length(pieces)]]) + 1L]] <- r
      }
    }
    for (k in seq_along(pieces)) {
      runs <- pieces[[k]]
      txt <- paste(vapply(runs, function(r) r$text, character(1)), collapse = "")
      txt <- trimws(gsub("[[:space:]\u00A0]+", " ", txt, perl = TRUE))
      if (!nzchar(txt)) next
      if (isTRUE(french)) txt <- lss_q_french(txt)
      if (identical(b$type, "list_item") && k == 1L) {
        txt <- paste0("\u2022 ", txt)
      }
      filled <- Filter(function(r) nzchar(trimws(r$text)), runs)
      bold <- length(filled) > 0L &&
        all(vapply(filled, function(r) isTRUE(r$bold), logical(1)))
      out[[length(out) + 1L]] <- list(text = txt, bold = bold)
    }
  }
  out
}

#' Drop a code that a label repeats at its end ("Very satisfied 10")
#' @keywords internal
#' @noRd
lss_q_strip_code <- function(label, code) {
  if (!nzchar(label) || is.na(code) || !nzchar(code)) {
    return(label)
  }
  tail <- paste0(" ", code)
  if (endsWith(label, tail) && nchar(label) > nchar(tail)) {
    label <- trimws(substr(label, 1L, nchar(label) - nchar(tail)))
  }
  label
}

#' Language tag declared to Word for the content
#'
#' An explicit `proofing_lang` is used as it is. Otherwise the tag derives
#' from `language`: a code that already carries a region (`de-CH`,
#' `pt-BR`, `zh-Hant-TW`) is used as it is; any other suffix
#' (`de-informal`, `sr-Latn`) is dropped; the five languages of the chrome
#' get their usual region (`fr` gives `fr-FR`, `en` gives `en-GB`, as in
#' the prototype); any other code is passed bare.
#'
#' @keywords internal
#' @noRd
lss_q_proofing_lang <- function(language, proofing_lang = NULL) {
  if (!is.null(proofing_lang)) {
    return(proofing_lang)
  }
  parts <- strsplit(language, "-", fixed = TRUE)[[1]]
  if (length(parts) > 1L &&
      any(grepl("^([A-Z]{2}|[0-9]{3})$", parts[-1L]))) {
    return(language)
  }
  primary <- tolower(parts[1])
  known <- c(fr = "fr-FR", de = "de-DE", it = "it-IT", es = "es-ES",
             en = "en-GB")
  if (primary %in% names(known)) known[[primary]] else primary
}

#' Language tag of the chrome runs, or `NULL` when they need none
#'
#' The chrome runs declare their own language only when the chrome
#' language differs from the primary subtag of the content tag.
#'
#' @keywords internal
#' @noRd
lss_q_chrome_tag <- function(chrome_lang, proofing_lang) {
  primary <- tolower(strsplit(proofing_lang, "-", fixed = TRUE)[[1]][1])
  if (identical(primary, chrome_lang)) {
    return(NULL)
  }
  lss_q_proofing_lang(chrome_lang)
}

#' Whether a plain filter still carries Expression Manager syntax
#'
#' `lss_humanize_relevance()` returns what it cannot translate unchanged.
#' The plain form is kept when, set literals (`\u2208 {1, 2}`) and quoted
#' patterns aside, it has no function call (`count(`, `sum(`), no
#' attribute of a variable (`.shown`, `.valueNAOK`), no braces and no
#' operator left in its raw form (`!`, `&&`, `||`, `==`).
#'
#' @keywords internal
#' @noRd
lss_q_filter_unsimplified <- function(plain) {
  s <- gsub("[\u2208\u2209]\\s*\\{[^}]*\\}", "", plain, perl = TRUE)
  s <- gsub("\"[^\"]*\"", "", s, perl = TRUE)
  grepl("[A-Za-z_][A-Za-z0-9_]*\\s*\\(|[{}!&|]|==|[A-Za-z0-9_]\\.[A-Za-z_]",
        s, perl = TRUE)
}

#' The filter of a question as the readable questionnaire prints it
#'
#' `NULL` for a question always shown. Otherwise `if Q11 = 2` (the plain
#' form of the full document, `q_filter_if_fmt`), or `filtered` alone
#' (`q_filtered`) when the expression cannot be simplified.
#'
#' @keywords internal
#' @noRd
lss_q_filter <- function(relevance, theme) {
  if (lss_q_blank(relevance)) {
    return(NULL)
  }
  raw <- trimws(lss_strip_outer_parens(as.character(relevance[1])))
  if (identical(raw, "1") || !nzchar(raw)) {
    return(NULL)
  }
  plain <- lss_humanize_relevance(raw, theme)
  if (lss_q_filter_unsimplified(plain)) {
    return(theme$chrome$q_filtered)
  }
  sprintf(theme$chrome$q_filter_if_fmt, plain)
}

#' Which options of a list are printed
#'
#' A list is abridged only when that makes it shorter: longer than
#' `max_options + 1`. It then keeps `ceiling(max_options / 2)` options
#' from its start and `floor(max_options / 2)` from its end, around an
#' `NA` that stands for the ellipsis line.
#'
#' @param n Number of options.
#' @param max_options A whole number of at least 2, or `Inf`.
#' @return Integer positions, with one `NA` when the list is abridged.
#' @keywords internal
#' @noRd
lss_q_abridge <- function(n, max_options) {
  if (is.infinite(max_options) || n <= max_options + 1) {
    return(seq_len(n))
  }
  head_n <- ceiling(max_options / 2)
  tail_n <- floor(max_options / 2)
  c(seq_len(head_n), NA_integer_, seq.int(n - tail_n + 1L, n))
}

#' The legend under the title
#'
#' `"44 questions in 6 groups, 2 text displays and 1 computed value."`:
#' each count singular when it is 1, the counts above zero after the main
#' clause, the last two joined by `q_and`. Then the sentence on the codes
#' (with `show_codes`), the meaning of each mandatory mark the document
#' prints, and the language rendered (`language`, when the survey has
#' several).
#'
#' @param chrome The chrome strings.
#' @param counts A list with `questions`, `groups`, `displays`, `computed`
#'   and `hidden`: what the document prints.
#' @keywords internal
#' @noRd
lss_q_legend <- function(chrome, counts, show_codes = TRUE,
                         mandatory = FALSE, soft = FALSE,
                         language = NULL) {
  n_of <- function(key, n) {
    paste(n, chrome[[paste0(key, if (identical(as.integer(n), 1L)) "_1" else "_n")]])
  }
  items <- sprintf(
    chrome$q_legend_in_fmt,
    n_of("q_n_question", counts$questions),
    n_of("q_n_group", counts$groups)
  )
  for (k in c("display", "computed", "hidden")) {
    n <- counts[[switch(k, display = "displays", k)]]
    if (!is.null(n) && n > 0L) items <- c(items, n_of(paste0("q_n_", k), n))
  }
  first <- if (length(items) == 1L) {
    items
  } else {
    paste0(
      paste(items[-length(items)], collapse = ", "),
      " ", chrome$q_and, " ", items[length(items)]
    )
  }
  paste(
    c(
      paste0(first, "."),
      if (isTRUE(show_codes)) chrome$q_legend_codes,
      if (isTRUE(mandatory)) chrome$q_legend_mandatory,
      if (isTRUE(soft)) chrome$q_legend_soft,
      if (!is.null(language)) sprintf(chrome$q_legend_language_fmt, language)
    ),
    collapse = " "
  )
}

#' Width of the widest of some strings, in points
#'
#' Measured with \pkg{gdtools} when it is installed, as the prototype does.
#' Without it, or when the measure fails, estimated: 0.56 em per character
#' in the body font, 0.61 em in the code font.
#'
#' @param measure `NULL` to measure when \pkg{gdtools} is installed, `FALSE`
#'   to force the estimate.
#' @keywords internal
#' @noRd
lss_q_text_width <- function(x, font, size, bold = FALSE, code = FALSE,
                             measure = NULL) {
  x <- as.character(x)
  x <- x[!is.na(x) & nzchar(x)]
  if (length(x) == 0L) {
    return(0)
  }
  if (is.null(measure)) {
    measure <- requireNamespace("gdtools", quietly = TRUE)
  }
  if (isTRUE(measure)) {
    w <- tryCatch(
      gdtools::strings_sizes(x, fontname = font, fontsize = size,
                             bold = bold)$width * 72,
      error = function(e) NULL
    )
    if (!is.null(w) && length(w) == length(x) && all(is.finite(w))) {
      return(max(w))
    }
  }
  max(nchar(x)) * (if (isTRUE(code)) 0.61 else 0.56) * size
}

#' Width of the left column, in points
#'
#' The column holds the question numbers, in bold at the body size, and the
#' tags ("text", "computed", "hidden", the consent box), at the body size
#' minus 2. It is the prototype's `2.8 * base_size` at least, wider when
#' the widest of the largest number and the tags printed needs it, with 6
#' points of air.
#'
#' @keywords internal
#' @noRd
lss_q_margin_width <- function(numbers, tags, font, base_size,
                               measure = NULL) {
  w <- max(
    lss_q_text_width(numbers, font, base_size, bold = TRUE, measure = measure),
    lss_q_text_width(tags, font, base_size - 2L, measure = measure)
  )
  max(round(2.8 * base_size), ceiling(w + 6))
}

#' Width of the code column of a list, in points
#'
#' The widest code of the list in the body font at the body size minus 2,
#' plus 5 points; `0` when the list has no code.
#'
#' @keywords internal
#' @noRd
lss_q_code_width <- function(codes, font, base_size, measure = NULL) {
  w <- lss_q_text_width(codes, font, base_size - 2L, measure = measure)
  if (w <= 0) 0 else ceiling(w) + 5
}

#' The first value of `x` as a string, or `default` when it is missing
#' @keywords internal
#' @noRd
lss_q_or <- function(x, default) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) default else as.character(x[1])
}

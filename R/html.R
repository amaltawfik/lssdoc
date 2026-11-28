#' Convert an HTML fragment into a renderer-agnostic block model
#'
#' LimeSurvey stores question, help, and answer texts as HTML fragments. To
#' reproduce them faithfully in Word without coupling the parser to a
#' specific output backend, `lss_html_to_blocks()` turns a fragment into a
#' simple intermediate model: a list of *blocks* (paragraphs or list items),
#' each holding a sequence of *runs* (a piece of text plus its formatting
#' marks). The renderer maps blocks and runs onto Word paragraphs and styled
#' text runs.
#'
#' Supported inline formatting: bold (`b`, `strong`), italic (`i`, `em`),
#' underline (`u`, `ins`), superscript (`sup`), subscript (`sub`). Supported
#' block structure: paragraphs (`p`, `div`, headings), line breaks (`br`),
#' and ordered/unordered lists (`ol`, `ul`, `li`) with nesting. Unknown tags
#' are transparent: their text content is kept. Whitespace is collapsed the
#' way a browser would, and HTML entities are decoded.
#'
#' The items of an ordered list are numbered in Arabic digits, counted
#' upwards from 1, or from the `start` attribute of the `ol` when it holds
#' an integer (up to nine digits, an optional sign). Each list has its own
#' count: the item that follows a nested list takes the next number of its
#' own list. The attributes `value` (on `li`), `type` and `reversed`, and
#' the CSS list styles, are not read.
#'
#' @param html A length-one character HTML fragment. `NA`, `NULL`, or an
#'   all-whitespace string yields an empty list.
#'
#' @return A list of blocks. Each block is a list with `type`
#'   (`"paragraph"` or `"list_item"`), `level` (integer nesting depth for
#'   list items, 0 otherwise), `ordered` (logical), `index` (the number of
#'   an item of an ordered list, `NA_integer_` otherwise), and `runs`. Each
#'   run is a list with `text` and the logical marks `bold`, `italic`,
#'   `underline`, `superscript`, `subscript`, plus an optional `linebreak`
#'   flag.
#'
#' @keywords internal
#' @noRd
lss_html_to_blocks <- function(html) {
  if (is.null(html) || length(html) == 0) {
    return(list())
  }
  html <- html[[1]]
  if (is.na(html) || !nzchar(trimws(html))) {
    return(list())
  }

  doc <- xml2::read_html(
    paste0("<!DOCTYPE html><html><body>", html, "</body></html>")
  )
  body <- xml2::xml_find_first(doc, "//body")

  state <- new.env(parent = emptyenv())
  state$blocks <- list()
  state$runs <- list()
  state$type <- "paragraph"
  state$level <- 0L
  state$ordered <- FALSE
  state$index <- NA_integer_
  state$dirty <- FALSE

  flush <- function() {
    runs <- lss_trim_runs(state$runs)
    if (length(runs) > 0) {
      state$blocks <- c(state$blocks, list(list(
        type = state$type,
        level = state$level,
        ordered = state$ordered,
        index = state$index,
        runs = runs
      )))
    }
    state$runs <- list()
    state$dirty <- FALSE
  }

  add_run <- function(text, marks) {
    state$runs <- c(state$runs, list(c(list(text = text), marks)))
    state$dirty <- TRUE
  }

  block_tags <- c(
    "p", "div", "blockquote", "section", "article",
    "h1", "h2", "h3", "h4", "h5", "h6", "tr", "pre"
  )
  mark_for <- list(
    b = "bold", strong = "bold",
    i = "italic", em = "italic",
    u = "underline", ins = "underline",
    sup = "superscript", sub = "subscript"
  )

  default_marks <- list(
    bold = FALSE, italic = FALSE, underline = FALSE,
    superscript = FALSE, subscript = FALSE
  )

  # `items`: the count of the list the walk is in (an environment, one per
  # `ol` or `ul`), NULL outside a list.
  walk <- function(node, marks, level, ordered, items = NULL) {
    for (ch in xml2::xml_contents(node)) {
      nt <- xml2::xml_type(ch)
      if (nt == "text") {
        txt <- gsub("[ \t\r\n]+", " ", xml2::xml_text(ch))
        if (nzchar(txt)) add_run(txt, marks)
      } else if (nt == "element") {
        name <- tolower(xml2::xml_name(ch))
        if (name == "br") {
          state$runs <- c(
            state$runs,
            list(c(list(text = "", linebreak = TRUE), marks))
          )
          state$dirty <- TRUE
        } else if (name %in% c("ul", "ol")) {
          flush()
          count <- new.env(parent = emptyenv())
          count$n <- lss_list_start(ch) - 1L
          walk(ch, marks, level + 1L, identical(name, "ol"), count)
        } else if (name == "li") {
          flush()
          if (!is.null(items)) items$n <- items$n + 1L
          state$type <- "list_item"
          state$level <- level
          state$ordered <- ordered
          state$index <- if (ordered) items$n else NA_integer_
          walk(ch, marks, level, ordered, items)
          flush()
          state$type <- "paragraph"
          state$level <- 0L
          state$ordered <- FALSE
          state$index <- NA_integer_
        } else if (name %in% block_tags) {
          flush()
          is_heading <- grepl("^h[1-6]$", name)
          walk(ch, modifyList(marks, list(bold = marks$bold || is_heading)),
               level, ordered, items)
          flush()
        } else if (!is.null(mark_for[[name]])) {
          mark <- stats::setNames(list(TRUE), mark_for[[name]])
          walk(ch, modifyList(marks, mark), level, ordered, items)
        } else {
          # Unknown/transparent tag (span, a, font, mark, ...): keep content.
          walk(ch, marks, level, ordered, items)
        }
      }
    }
  }

  walk(body, default_marks, 0L, FALSE)
  flush()
  state$blocks
}

#' The first number of a list: its `start` attribute when that holds an
#' integer (up to nine digits, an optional sign, spaces around), 1 otherwise
#' @keywords internal
#' @noRd
lss_list_start <- function(node) {
  start <- trimws(xml2::xml_attr(node, "start"))
  if (grepl("^[+-]?[0-9]{1,9}$", start)) as.integer(start) else 1L
}

#' The marker that starts an item of a list: its number and a full stop in
#' an ordered list ("2. "), a bullet otherwise. Read by `lss_compose_chunks()`
#' and `lss_q_html_paras()`.
#' @keywords internal
#' @noRd
lss_list_marker <- function(block) {
  if (isTRUE(block$ordered)) paste0(block$index, ". ") else "\u2022 "
}

#' Trim leading/trailing whitespace runs in a block
#' @keywords internal
#' @noRd
lss_trim_runs <- function(runs) {
  if (length(runs) == 0) {
    return(runs)
  }
  # Drop runs that are pure empty text and not line breaks.
  keep <- vapply(
    runs,
    function(r) isTRUE(r$linebreak) || nzchar(r$text),
    logical(1)
  )
  runs <- runs[keep]
  if (length(runs) == 0) {
    return(runs)
  }
  # Trim leading space on the first text run and trailing on the last.
  first <- which(!vapply(runs, function(r) isTRUE(r$linebreak), logical(1)))
  if (length(first) > 0) {
    runs[[first[1]]]$text <- sub("^ +", "", runs[[first[1]]]$text)
    last <- first[length(first)]
    runs[[last]]$text <- sub(" +$", "", runs[[last]]$text)
  }
  runs
}

#' Flatten an HTML fragment to plain text
#'
#' Paragraph and list-item blocks are separated by newlines; inline runs are
#' concatenated. Useful for the audit and for any context that needs the text
#' without formatting. Returns `""` for empty input.
#'
#' @param html A length-one character HTML fragment.
#' @return A length-one character string.
#' @keywords internal
#' @noRd
lss_html_to_text <- function(html) {
  blocks <- lss_html_to_blocks(html)
  if (length(blocks) == 0) {
    return("")
  }
  lines <- vapply(
    blocks,
    function(b) {
      paste0(vapply(
        b$runs,
        function(r) if (isTRUE(r$linebreak)) "\n" else r$text,
        character(1)
      ), collapse = "")
    },
    character(1)
  )
  paste(lines, collapse = "\n")
}

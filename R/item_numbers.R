# Question numbers shared by every renderer: which questions are numbered,
# their number, the label of a sub-item, and the hidden-question test.

#' Number the questions of a survey
#'
#' LimeSurvey stores no question numbers, only `group_order` and
#' `question_order`. The document numbers the questions put to the
#' respondent, 1 to n, in the order of the groups and of the questions
#' within each group (the order of `lss_model()`). A text display (type
#' `X`), an equation (type `*`) and a question hidden from respondents
#' (see `lss_question_hidden()`) get no number but keep their place. A
#' hidden text display counts as a text display and a hidden equation as
#' an equation, once.
#'
#' The numbers depend on the survey structure alone: the displayed
#' languages, `show_groups` and the template never move them, and no
#' renderer keeps a counter of its own. Every renderer reads them from
#' this table, through `lss_item_label()`.
#'
#' @param model An `lss_model`, or `NULL` for an empty table.
#' @return A data frame with one row per question, in document order:
#'   `qid`, `code`, `status` (`"asked"`, `"display"`, `"equation"` or
#'   `"hidden"`), `hidden` (the `hidden` attribute is set), `no` (integer,
#'   `NA` for an unnumbered question) and `label` (`"28"`, or an en dash
#'   for an unnumbered question).
#' @keywords internal
#' @noRd
lss_item_numbers <- function(model) {
  qs <- if (is.null(model)) {
    list()
  } else {
    unlist(lapply(model$groups, function(g) g$questions), recursive = FALSE)
  }
  pick <- function(field) {
    vapply(
      qs,
      function(q) {
        v <- q[[field]]
        if (is.null(v) || length(v) == 0L) NA_character_ else as.character(v[1])
      },
      character(1)
    )
  }
  type <- pick("type")
  hidden <- vapply(
    qs,
    function(q) lss_question_hidden(q$attributes),
    logical(1)
  )
  status <- ifelse(
    type %in% "*",
    "equation",
    ifelse(type %in% "X", "display", ifelse(hidden, "hidden", "asked"))
  )
  asked <- status == "asked"
  no <- rep(NA_integer_, length(qs))
  no[asked] <- seq_len(sum(asked))
  data.frame(
    qid = pick("qid"),
    code = pick("code"),
    status = as.character(status),
    hidden = hidden,
    no = no,
    label = as.character(ifelse(asked, as.character(no), "\u2013")),
    stringsAsFactors = FALSE
  )
}

#' Number label of a question, or of one of its sub-items
#'
#' Looks the question up in the table built by `lss_item_numbers()`.
#' Without `k`, returns the question's own number (`"28"`), which every
#' card or row standing for the whole question carries, the "Other" and
#' comment fields included. With `k`, returns the label of the k-th
#' sub-item (`"28.3"`): a card or row standing for one subquestion, in
#' rendering order (subquestion order; for a dual-scale array, each
#' subquestion's scales in turn; for a two-dimensional array, each row's
#' columns in turn; for a ranking, the positions). An unnumbered question
#' and all its sub-items get the en dash. A question absent from the
#' table (a renderer called without a survey) gets `NA`, shown as an
#' empty cell.
#'
#' @param numbers The data frame returned by `lss_item_numbers()`.
#' @param qid The question id.
#' @param k The sub-item rank, from 1, or `NULL` for the question itself.
#' @keywords internal
#' @noRd
lss_item_label <- function(numbers, qid, k = NULL) {
  if (is.null(numbers) || is.null(qid) || length(qid) == 0L || is.na(qid[1])) {
    return(NA_character_)
  }
  i <- match(as.character(qid[1]), numbers$qid)
  if (is.na(i)) {
    return(NA_character_)
  }
  if (is.na(numbers$no[i]) || is.null(k)) {
    return(numbers$label[i])
  }
  paste0(numbers$no[i], ".", as.integer(k))
}

#' Whether a question is hidden from respondents
#'
#' LimeSurvey hides a question with the attribute `hidden` set to `1`
#' (rows of `question_attributes` with `attribute == "hidden"`); the
#' question is still evaluated, so an equation or a prefilled value keeps
#' working, but the respondent never sees it.
#'
#' @param attributes The question's rows of `question_attributes` (the
#'   `attributes` field of a model question), or `NULL`.
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
lss_question_hidden <- function(attributes) {
  if (
    is.null(attributes) ||
      !is.data.frame(attributes) ||
      nrow(attributes) == 0L ||
      !all(c("attribute", "value") %in% names(attributes))
  ) {
    return(FALSE)
  }
  v <- attributes$value[attributes$attribute %in% "hidden"]
  any(trimws(as.character(v)) %in% "1")
}

#' Item heading text: `"28. code"`, or the bare code when unnumbered
#'
#' The bold heading `show_item_heading = TRUE` adds above each card. An
#' unnumbered item (en dash) or one with no number at all (`NA`) shows its
#' variable alone, with no number and no leading dot.
#'
#' @keywords internal
#' @noRd
lss_item_heading_text <- function(label, code) {
  if (
    is.null(label) ||
      is.na(label) ||
      !nzchar(label) ||
      identical(label, "\u2013")
  ) {
    return(code)
  }
  paste0(label, ". ", code)
}

#' Width a No cell needs for its widest number label
#'
#' A deterministic estimate, the same on every machine (no font
#' measurement): each digit counts 0.56 em and each period 0.28 em at the
#' cell's font size, Arial-like metrics about 10% wider than Calibri, which
#' leaves room for other fonts; an en dash counts as a digit. The cell's
#' left and right padding and 1 pt are added, and the result is rounded up
#' to 0.01 in. `"28.1"` at 10 pt with 2 pt of padding on each side needs
#' 24.6 pt, so 0.35 in.
#'
#' @param labels Character vector of number labels; `NA` and `""` count
#'   for nothing.
#' @param size Font size of the cell, in points.
#' @param padding Left plus right padding of the cell, in points.
#' @return The width in inches the widest label needs, `0` when there is
#'   no label.
#' @keywords internal
#' @noRd
lss_no_width <- function(labels, size, padding) {
  labels <- as.character(labels)
  labels <- labels[!is.na(labels) & nzchar(labels)]
  if (length(labels) == 0L) {
    return(0)
  }
  periods <- nchar(gsub("[^.]", "", labels))
  digits <- nchar(labels) - periods
  pt <- max(digits * 0.56 + periods * 0.28) * size + padding + 1
  ceiling(round(pt / 72 * 100, 6)) / 100
}

#' Number of sub-item cards the cards template gives a question
#'
#' Mirrors `lss_render_question_block()` and
#' `lss_render_compound_question()`: a ranking, a multiple choice and a
#' question without subquestions are one card (0 sub-items); an array, a
#' multiple short text or a multiple numerical input gets one card per
#' subquestion, per subquestion and scale for a dual-scale array, per row
#' and column for a two-dimensional array.
#'
#' @keywords internal
#' @noRd
lss_cards_subitem_count <- function(q) {
  info <- lss_type_info(q$type)
  sqs <- q$subquestions
  if (
    identical(q$type, "R") ||
      !isTRUE(info$has_subquestions) ||
      length(sqs) == 0L ||
      identical(info$family, "multiple")
  ) {
    return(0L)
  }
  if (isTRUE(info$has_scales) && !is.null(q$scales) && length(q$scales) > 1L) {
    return(length(sqs) * length(q$scales))
  }
  scale <- vapply(
    sqs,
    function(s) {
      if (is.null(s$scale_id) || is.na(s$scale_id)) {
        "0"
      } else {
        as.character(s$scale_id)
      }
    },
    character(1)
  )
  if (length(unique(scale)) > 1L) {
    return(sum(scale == "0") * sum(scale != "0"))
  }
  length(sqs)
}

#' The widest number label the cards template prints in a document
#'
#' Known before the first card, so every meta table of the document can
#' share one No width. For each question: its last sub-item label
#' (`"28.12"`) when the cards split it (see `lss_cards_subitem_count()`),
#' else its own label (`"28"`, or the en dash of an unnumbered question);
#' the "Other" and comment cards carry the question's label, never wider.
#' Exact, except that a two-dimensional array with no row or no column
#' (no sub-item card) still counts its own number, which can only
#' over-estimate.
#'
#' @param model An `lss_model`.
#' @param numbers The table returned by `lss_item_numbers()` for `model`.
#' @return One label, or `""` when the survey has no question.
#' @keywords internal
#' @noRd
lss_cards_widest_label <- function(model, numbers) {
  qs <- unlist(
    lapply(model$groups, function(g) g$questions),
    recursive = FALSE
  )
  labels <- vapply(
    qs,
    function(q) {
      label <- lss_item_label(numbers, q$qid)
      if (is.na(label)) {
        return("")
      }
      n <- lss_cards_subitem_count(q)
      if (n > 0L && !identical(label, "\u2013")) {
        paste0(label, ".", n)
      } else {
        label
      }
    },
    character(1)
  )
  if (length(labels) == 0L) {
    return("")
  }
  periods <- nchar(gsub("[^.]", "", labels))
  labels[which.max((nchar(labels) - periods) * 0.56 + periods * 0.28)]
}

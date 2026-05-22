#' Assemble a denormalized, render-ready model from an `lss` object
#'
#' Join the structural and localized sections of a parsed `.lss` into a
#' per-group, per-question model keyed by language. This is the backbone
#' consumed by [audit_lss()] and [render_lss_docx()]: it resolves the
#' display order, attaches each question's localized texts, answer options,
#' subquestions, and attributes, and labels the question type. Language
#' identifiers and all user text are preserved verbatim.
#'
#' @param lss An `lss` object from [parse_lss()].
#' @param languages Character vector of language codes to include, in order.
#'   Defaults to all languages of the survey. Requesting a language absent
#'   from the survey is an error.
#'
#' @return An object of class `lss_model`: a list with `file`, `languages`,
#'   `base_language`, and `groups`. Each group has `gid`, `order`, localized
#'   `names`/`descriptions` (named by language), and `questions`. Each
#'   question has `qid`, `code`, `type`, `type_label`, `mandatory`,
#'   `relevance`, localized `texts` (each language a list of `question` and
#'   `help`), `answers`, `subquestions`, `scales`, and `attributes`.
#'
#' @keywords internal
#' @noRd
lss_model <- function(lss, languages = NULL) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      "{.arg lss} must be an {.cls lss} object from {.fn parse_lss}.",
      class = "lssdoc_bad_lss"
    )
  }
  langs <- lss_resolve_languages(lss, languages)

  groups <- lss$groups
  group_order <- order(suppressWarnings(as.integer(groups$group_order)))
  groups <- groups[group_order, , drop = FALSE]

  questions <- lss$questions
  q_order <- order(suppressWarnings(as.integer(questions$question_order)))
  questions <- questions[q_order, , drop = FALSE]

  group_models <- lapply(seq_len(nrow(groups)), function(i) {
    gid <- groups$gid[i]
    gtext <- lss_localized(
      lss$group_l10ns, "gid", gid, langs,
      c("group_name", "description")
    )
    gquestions <- questions[questions$gid == gid, , drop = FALSE]
    list(
      gid = gid,
      order = groups$group_order[i],
      names = lapply(gtext, function(x) x$group_name),
      descriptions = lapply(gtext, function(x) x$description),
      questions = lapply(
        seq_len(nrow(gquestions)),
        function(j) lss_question_model(lss, gquestions[j, , drop = FALSE], langs)
      )
    )
  })

  structure(
    list(
      file = lss$file,
      languages = langs,
      base_language = lss$base_language,
      groups = group_models
    ),
    class = "lss_model"
  )
}

#' Resolve and validate the requested languages
#' @keywords internal
#' @noRd
lss_resolve_languages <- function(lss, languages) {
  available <- lss$languages
  if (is.null(languages)) {
    return(available)
  }
  missing <- setdiff(languages, available)
  if (length(missing) > 0) {
    lssdoc_abort(
      c(
        "Requested language{?s} not in the survey: {.val {missing}}.",
        "i" = "Available: {.val {available}}."
      ),
      class = "lssdoc_unknown_language"
    )
  }
  languages
}

#' Build the model for a single (main) question
#' @keywords internal
#' @noRd
lss_question_model <- function(lss, qrow, langs) {
  qid <- qrow$qid
  info <- lss_type_info(qrow$type)

  answers <- NULL
  scales <- NULL
  if (isTRUE(info$has_answers) && !is.null(lss$answers)) {
    answers <- lss_answer_models(lss, qid, langs)
    if (isTRUE(info$has_scales)) {
      scales <- split(answers, vapply(answers, function(a) a$scale_id, character(1)))
    }
  }

  subquestions <- NULL
  if (isTRUE(info$has_subquestions) && !is.null(lss$subquestions)) {
    subquestions <- lss_subquestion_models(lss, qid, langs)
  }

  attrs <- NULL
  if (!is.null(lss$question_attributes)) {
    attrs <- lss$question_attributes[lss$question_attributes$qid == qid, , drop = FALSE]
  }

  texts <- lss_localized(
    lss$question_l10ns, "qid", qid, langs, c("question", "help")
  )

  list(
    qid = qid,
    code = qrow$title,
    type = qrow$type,
    type_label = lss_question_label(qrow$type, qrow$question_theme_name),
    theme = qrow$question_theme_name,
    mandatory = qrow$mandatory,
    relevance = qrow$relevance,
    other = qrow$other,
    texts = texts,
    answers = answers,
    scales = scales,
    subquestions = subquestions,
    attributes = attrs
  )
}

#' Build the per-language answer-option models for a question
#' @keywords internal
#' @noRd
lss_answer_models <- function(lss, qid, langs) {
  ans <- lss$answers[lss$answers$qid == qid, , drop = FALSE]
  if (nrow(ans) == 0) {
    return(list())
  }
  ord <- order(
    suppressWarnings(as.integer(ans$scale_id)),
    suppressWarnings(as.integer(ans$sortorder))
  )
  ans <- ans[ord, , drop = FALSE]
  lapply(seq_len(nrow(ans)), function(i) {
    aid <- ans$aid[i]
    labels <- lss_localized(lss$answer_l10ns, "aid", aid, langs, "answer")
    list(
      aid = aid,
      code = ans$code[i],
      scale_id = ans$scale_id[i],
      sortorder = ans$sortorder[i],
      labels = lapply(labels, function(x) x$answer)
    )
  })
}

#' Build the per-language subquestion models for a question
#' @keywords internal
#' @noRd
lss_subquestion_models <- function(lss, qid, langs) {
  sq <- lss$subquestions[lss$subquestions$parent_qid == qid, , drop = FALSE]
  if (nrow(sq) == 0) {
    return(list())
  }
  ord <- order(
    suppressWarnings(as.integer(sq$scale_id)),
    suppressWarnings(as.integer(sq$question_order))
  )
  sq <- sq[ord, , drop = FALSE]
  lapply(seq_len(nrow(sq)), function(i) {
    sqid <- sq$qid[i]
    texts <- lss_localized(lss$question_l10ns, "qid", sqid, langs, c("question", "help"))
    list(
      qid = sqid,
      code = sq$title[i],
      scale_id = sq$scale_id[i],
      relevance = sq$relevance[i],
      texts = texts
    )
  })
}

#' Collect localized columns for one key across languages
#'
#' Returns a list named by language; each element is a named list of the
#' requested columns. A language with no matching row yields `NA` values, so
#' missing translations are visible to the audit rather than silently
#' dropped.
#'
#' @keywords internal
#' @noRd
lss_localized <- function(l10n, key_col, key, langs, cols) {
  out <- lapply(langs, function(lg) {
    values <- stats::setNames(as.list(rep(NA_character_, length(cols))), cols)
    if (!is.null(l10n)) {
      hit <- l10n[[key_col]] == key & l10n$language == lg
      hit[is.na(hit)] <- FALSE
      if (any(hit)) {
        row <- l10n[which(hit)[1], , drop = FALSE]
        for (cc in cols) {
          if (cc %in% names(row)) values[[cc]] <- row[[cc]]
        }
      }
    }
    values
  })
  stats::setNames(out, langs)
}

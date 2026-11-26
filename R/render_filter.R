# Filter humanizer for LimeSurvey relevance expressions.
#
# Extracted from R/render_lss_docx.R; the functions here translate the
# raw LimeSurvey conditional syntax (e.g. !is_empty(Q1.NAOK) && (Q1.NAOK
# == 1)) into the editorial mathematical form (Q1 = 1) used in the
# Filter cell of every rendered question. Audited and tested in
# tests/testthat/test-humanize_relevance.R.

#' Best-effort translation of a LimeSurvey relevance expression into a
#' human-readable form.
#'
#' Recognized patterns:
#' * `is_empty(X.NAOK)` -> "X is empty"
#' * `!is_empty(X.NAOK)` -> "X is answered"
#' * `strlen(X.NAOK) == 0` -> "X is empty"; `strlen(X.NAOK) > 0` ->
#'   "X is answered" (the same predicates written via string length)
#' * `intval(X.NAOK) OP N` -> "X OP N" (the integer cast is invisible
#'   to a methodologist)
#' * `regexMatch("pat", X.NAOK)` -> "X matches \"pat\""
#' * `that.X.NAOK` -> "X" (LimeSurvey group references lose the
#'   structural prefix)
#' * `X.NAOK == N` -> "X = N"; `X.NAOK != N` -> "X \u2260 N";
#'   `X.NAOK <= N` -> "X \u2264 N" (idem `>=`, `<`, `>`)
#' * `&&` -> "AND"; `||` -> "OR"
#' * Multiple disjunctions on the **same variable** collapse to set
#'   notation: `X == 1 || X == 2 || X == 3` -> "X \u2208 {1, 2, 3}"
#' * Two bounds on the **same variable** collapse to an encased range:
#'   `X >= 18 && X <= 65` -> "18 \u2264 X \u2264 65"
#'
#' All Boolean and predicate tokens (`AND`, `OR`, `is answered`,
#' `is empty`, `matches`) localize via `theme$chrome$filter_*` when a
#' theme is supplied. The function strips balanced outer parentheses.
#' When the expression cannot be matched it is returned unchanged so
#' the raw text is never lost.
#'
#' @param x A character relevance expression as stored in LimeSurvey.
#' @param theme Optional theme list. When `NULL`, English chrome is used.
#' @return A single human-readable string. The localized "All" token for
#'   `1`, empty, or `NA`.
#' @keywords internal
#' @noRd
lss_humanize_relevance <- function(x, theme = NULL) {
  chrome <- lss_filter_chrome(theme)
  if (is.null(x) || is.na(x) || !nzchar(x) || identical(x, "1")) {
    return(chrome$all)
  }
  s <- as.character(x)
  s <- lss_strip_outer_parens(s)

  # Step 0: preprocess LimeSurvey wrappers and group references so the
  # subsequent steps see canonical `X.NAOK` references.
  # - `that.Q1.NAOK` is a group reference (LimeSurvey routes inside
  #   one group via `that.code`); the prefix is structural noise.
  # - `intval(X.NAOK)` wraps a string value into an integer for
  #   comparison; the cast is invisible to a methodologist.
  # - `regexMatch("pat", X.NAOK)` becomes `X matches "pat"` so the
  #   reader sees a predicate, not a function call.
  # - `strlen(X.NAOK) > 0` is the LimeSurvey idiom for "answered";
  #   `strlen(X.NAOK) == 0` for "empty". Map both to the same chrome
  #   strings as `is_empty()`.
  s <- gsub("\\bthat\\.([A-Za-z0-9_]+)", "\\1", s, perl = TRUE)
  s <- gsub("\\bintval\\s*\\(\\s*([A-Za-z0-9_]+\\.NAOK)\\s*\\)", "\\1",
            s, perl = TRUE)
  s <- gsub("\\bregexMatch\\s*\\(\\s*\"([^\"]*)\"\\s*,\\s*([A-Za-z0-9_]+)\\.NAOK\\s*\\)",
            paste0("\\2 ", chrome$matches, " \"\\1\""), s, perl = TRUE)
  s <- gsub("\\bstrlen\\s*\\(\\s*([A-Za-z0-9_]+)\\.NAOK\\s*\\)\\s*>\\s*0",
            paste0("\\1 ", chrome$answered), s, perl = TRUE)
  s <- gsub("\\bstrlen\\s*\\(\\s*([A-Za-z0-9_]+)\\.NAOK\\s*\\)\\s*(==|<=)\\s*0",
            paste0("\\1 ", chrome$empty), s, perl = TRUE)

  # Step 1: collapse the LimeSurvey "answered-and-equals" idiom on the
  # SAME variable. LimeSurvey's conditional designer emits
  # `!is_empty(X.NAOK) && (X.NAOK OP value)` as a defensive guard;
  # for human review the guard is noise so we drop it.
  idiom_left <- paste0(
    "!\\s*is_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)\\s*&&\\s*",
    "\\(\\s*\\1\\.NAOK\\s*(==|!=|>=|<=|>|<)\\s*([^)&|]+)\\s*\\)"
  )
  idiom_right <- paste0(
    "\\(\\s*([A-Za-z0-9_]+)\\.NAOK\\s*(==|!=|>=|<=|>|<)\\s*([^)&|]+)\\s*\\)",
    "\\s*&&\\s*!\\s*is_empty\\(\\1\\.NAOK\\)"
  )
  for (i in seq_len(5L)) {
    before <- s
    s <- gsub(idiom_left, "\\1.NAOK \\2 \\3", s, perl = TRUE)
    s <- gsub(idiom_right, "\\1.NAOK \\2 \\3", s, perl = TRUE)
    if (identical(s, before)) break
  }

  # Step 1b: drop the quotes LimeSurvey puts around a scalar answer code
  # in a comparison (`X.NAOK == "1"`). They are string literals in the
  # Expression Manager, but a bare code reads cleaner for a reviewer
  # (`workstatus = 1`, not `workstatus = "1"`) and the Value section
  # already documents the code. Quotes are kept around values that contain
  # whitespace (so multi-word strings stay delimited) and around regex
  # patterns (`X matches "pat"`, already converted in step 0 and never
  # adjacent to a comparison operator, so untouched here).
  s <- gsub("(\\.NAOK\\s*(?:==|!=|>=|<=|>|<)\\s*)\"([^\"\\s]+)\"",
            "\\1\\2", s, perl = TRUE)

  # Step 2: collapse `X == a || X == b || X == c` (same variable)
  # to "X \u2208 {a, b, c}". Same for negation with `!=` and `&&` ->
  # "X \u2209 {a, b, c}". Repeat while the pattern keeps shrinking
  # so chained sets of any length collapse.
  for (i in seq_len(8L)) {
    before <- s
    s <- lss_collapse_set(s, op = "==", join = "||")
    s <- lss_collapse_set(s, op = "!=", join = "&&")
    if (identical(s, before)) break
  }

  # Step 3: collapse `X >= a && X <= b` (same variable) to
  # "a \u2264 X \u2264 b" (and the strict variants).
  s <- lss_collapse_range(s)

  # Step 4: predicate forms.
  s <- gsub("!\\s*is_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)",
            paste0("\\1 ", chrome$answered), s, perl = TRUE)
  s <- gsub("\\bis_empty\\(([A-Za-z0-9_]+)\\.NAOK\\)",
            paste0("\\1 ", chrome$empty), s, perl = TRUE)
  s <- gsub("([A-Za-z0-9_]+)\\.NAOK", "\\1", s, perl = TRUE)
  s <- gsub("\\s*&&\\s*", paste0(" ", chrome$and, " "), s)
  s <- gsub("\\s*\\|\\|\\s*", paste0(" ", chrome$or, " "), s)
  # Step 5: comparison operators rendered with Unicode math symbols
  # so they read at a glance for a methodologist: U+2260 (\u2260),
  # U+2264 (\u2264), U+2265 (\u2265). Order matters: substitute the
  # two-character forms first so the single `==` rule does not consume
  # the `=` of `!=` / `<=` / `>=`.
  s <- gsub("\\s*!=\\s*", " \u2260 ", s)
  s <- gsub("\\s*<=\\s*", " \u2264 ", s)
  s <- gsub("\\s*>=\\s*", " \u2265 ", s)
  s <- gsub("\\s*==\\s*", " = ", s)
  s <- lss_strip_outer_parens(s)
  trimws(s)
}

#' Localized filter token chrome with English fallback
#'
#' @keywords internal
#' @noRd
lss_filter_chrome <- function(theme = NULL) {
  defaults <- list(
    all      = "All",
    and      = "AND",
    or       = "OR",
    answered = "is answered",
    empty    = "is empty",
    matches  = "matches",
    inset    = "\u2208",
    notinset = "\u2209"
  )
  if (is.null(theme) || is.null(theme$chrome)) return(defaults)
  pick <- function(key, fallback) {
    v <- theme$chrome[[paste0("filter_", key)]]
    if (is.null(v) || !nzchar(v)) fallback else v
  }
  list(
    all      = pick("all",      defaults$all),
    and      = pick("and",      defaults$and),
    or       = pick("or",       defaults$or),
    answered = pick("answered", defaults$answered),
    empty    = pick("empty",    defaults$empty),
    matches  = pick("matches",  defaults$matches),
    inset    = defaults$inset,
    notinset = defaults$notinset
  )
}

#' Collapse repeated `X OP a JOIN X OP b JOIN X OP c` on the same
#' variable into set notation `X \u2208 {a, b, c}` (or `\u2209` when
#' negation).
#'
#' @keywords internal
#' @noRd
lss_collapse_set <- function(s, op, join) {
  # Match the simplest 2-term pair first; the outer loop in the caller
  # extends the captured set on subsequent iterations by re-matching
  # the produced set against another `X == v` clause.
  var_pat <- "([A-Za-z0-9_]+)(?:\\.NAOK)?"
  val_pat <- "([^)&|\\s]+)"
  op_re <- gsub("([=!<>])", "\\\\\\1", op, perl = TRUE)
  join_re <- if (identical(join, "||")) "\\|\\|" else "&&"
  # Pair pattern: X OP a JOIN X OP b
  pair_re <- paste0(
    "\\b", var_pat, "\\s*", op_re, "\\s*", val_pat,
    "\\s*", join_re, "\\s*",
    "\\b\\1(?:\\.NAOK)?", "\\s*", op_re, "\\s*", val_pat
  )
  # When the rhs is already an existing set marker (built on a previous
  # iteration), append the next value into the set.
  set_token <- if (identical(op, "==")) "\u2208" else "\u2209"
  s <- gsub(
    pair_re,
    paste0("\\1 ", set_token, " {\\2, \\3}"),
    s, perl = TRUE
  )
  # Extension pattern: existing set `X \u2208 {...}` JOIN `X OP v`
  ext_re <- paste0(
    "\\b", var_pat, "\\s+", set_token, "\\s+\\{([^}]+)\\}",
    "\\s*", join_re, "\\s*",
    "\\b\\1(?:\\.NAOK)?", "\\s*", op_re, "\\s*", val_pat
  )
  s <- gsub(
    ext_re,
    paste0("\\1 ", set_token, " {\\2, \\3}"),
    s, perl = TRUE
  )
  s
}

#' Collapse `X >= a && X <= b` (same variable) to `a \u2264 X \u2264 b`.
#' Supports the four combinations of strict / non-strict bounds.
#'
#' @keywords internal
#' @noRd
lss_collapse_range <- function(s) {
  var_pat <- "([A-Za-z0-9_]+)(?:\\.NAOK)?"
  val_pat <- "([^)&|\\s]+)"
  # X (>=|>) a && X (<=|<) b -> a (\u2264|<) X (\u2264|<) b
  patterns <- list(
    c(">=", "<=", "\u2264", "\u2264"),
    c(">=", "<",  "\u2264", "<"),
    c(">",  "<=", "<",      "\u2264"),
    c(">",  "<",  "<",      "<")
  )
  for (p in patterns) {
    lo_op <- p[1]; hi_op <- p[2]; lo_disp <- p[3]; hi_disp <- p[4]
    lo_re <- gsub("([=<>])", "\\\\\\1", lo_op, perl = TRUE)
    hi_re <- gsub("([=<>])", "\\\\\\1", hi_op, perl = TRUE)
    pat <- paste0(
      "\\b", var_pat, "\\s*", lo_re, "\\s*", val_pat,
      "\\s*&&\\s*",
      "\\b\\1(?:\\.NAOK)?", "\\s*", hi_re, "\\s*", val_pat
    )
    s <- gsub(
      pat,
      paste0("\\2 ", lo_disp, " \\1 ", hi_disp, " \\3"),
      s, perl = TRUE
    )
  }
  s
}

#' Strip balanced outer parentheses up to a few levels deep
#' @keywords internal
#' @noRd
lss_strip_outer_parens <- function(s) {
  for (i in seq_len(8L)) {
    inner <- sub("^\\s*\\((.*)\\)\\s*$", "\\1", s, perl = TRUE)
    if (identical(inner, s) || !lss_parens_balanced(inner)) break
    s <- inner
  }
  s
}

#' Check whether parentheses are balanced in a string
#' @keywords internal
#' @noRd
lss_parens_balanced <- function(s) {
  depth <- 0L
  for (ch in strsplit(s, "", fixed = TRUE)[[1]]) {
    if (ch == "(") {
      depth <- depth + 1L
    } else if (ch == ")") {
      depth <- depth - 1L
      if (depth < 0L) return(FALSE)
    }
  }
  depth == 0L
}


#' Does a relevance expression carry a condition?
#'
#' The one place that decides it, at every level (group, question,
#' subquestion). `NULL`, `NA`, the empty string and `"1"` mean "always
#' shown", once the expression is trimmed and stripped of its outer
#' parentheses (LimeSurvey writes `((1))` as often as `1`).
#'
#' @param x A relevance expression (only its first element is read).
#' @return `TRUE` or `FALSE`.
#' @keywords internal
#' @noRd
lss_has_condition <- function(x) {
  if (is.null(x) || length(x) == 0L || is.na(x[1])) return(FALSE)
  s <- trimws(lss_strip_outer_parens(trimws(as.character(x[1]))))
  nzchar(s) && !identical(s, "1")
}

#' The lines of a Filter cell, from the three levels of condition
#'
#' A question is asked when its group, itself and, for one subquestion,
#' that subquestion are all relevant. When neither the group nor a
#' subquestion has a condition, the cell prints the question's own filter
#' on one unlabelled line, exactly as before the group and subquestion
#' levels were read: the "All" token when it has none. Otherwise one
#' labelled line per level that has a condition, in the order group,
#' question, subquestion(s); `subq_relevance` may hold several
#' expressions (the row and the column of a two-dimensional array).
#'
#' @param relevance The question's relevance.
#' @param group_relevance The relevance of its group, or `NULL`.
#' @param subq_relevance The relevance of the subquestion(s) the item
#'   stands for, or `NULL`.
#' @param theme Optional theme list; its chrome gives the labels and the
#'   plain tokens. English when `NULL`.
#' @return A list of lines, each a list with `label` (`NULL` for the
#'   unlabelled line), `plain` (through `lss_humanize_relevance()`) and
#'   `raw` (outer parentheses stripped).
#' @keywords internal
#' @noRd
lss_filter_lines <- function(relevance, group_relevance = NULL,
                             subq_relevance = NULL, theme = NULL) {
  subq <- Filter(lss_has_condition, as.list(as.character(subq_relevance)))
  if (!lss_has_condition(group_relevance) && length(subq) == 0L) {
    raw <- if (is.null(relevance) || is.na(relevance) ||
               !nzchar(relevance)) {
      "1"
    } else {
      lss_strip_outer_parens(relevance)
    }
    return(list(list(label = NULL,
                     plain = lss_humanize_relevance(raw, theme),
                     raw = raw)))
  }
  chrome <- if (is.null(theme) || is.null(theme$chrome)) {
    lss_chrome_strings("en")
  } else {
    theme$chrome
  }
  line <- function(label, x) {
    raw <- trimws(lss_strip_outer_parens(trimws(as.character(x[1]))))
    list(label = label, plain = lss_humanize_relevance(raw, theme), raw = raw)
  }
  out <- list()
  if (lss_has_condition(group_relevance)) {
    out[[length(out) + 1L]] <- line(chrome$item_group, group_relevance)
  }
  if (lss_has_condition(relevance)) {
    out[[length(out) + 1L]] <- line(chrome$item_question, relevance)
  }
  for (s in subq) {
    out[[length(out) + 1L]] <- line(chrome$item_subquestion, s)
  }
  out
}

#' The flextable chunks of filter lines, one line under the other
#'
#' A labelled line is `sprintf(filter_level_fmt, label, plain)`, the label
#' and its separator in `label_props`, the expression in `plain_props`.
#' With `show_raw`, the raw expression of a line follows that line, in
#' `raw_props`, when it differs from the plain form.
#'
#' @keywords internal
#' @noRd
lss_filter_chunks <- function(lines, theme, plain_props, raw_props,
                              label_props = plain_props, show_raw = FALSE) {
  fmt <- theme$chrome$filter_level_fmt
  chunks <- list()
  for (k in seq_along(lines)) {
    ln <- lines[[k]]
    if (k > 1L) {
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk("\n", props = plain_props)
    }
    if (!is.null(ln$label)) {
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(
        sprintf(fmt, ln$label, ""), props = label_props
      )
    }
    chunks[[length(chunks) + 1L]] <- flextable::as_chunk(ln$plain, props = plain_props)
    if (isTRUE(show_raw) && !identical(ln$plain, ln$raw)) {
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk("\n", props = plain_props)
      chunks[[length(chunks) + 1L]] <- flextable::as_chunk(ln$raw, props = raw_props)
    }
  }
  chunks
}

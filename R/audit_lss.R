#' Audit a parsed LimeSurvey structure for reviewable anomalies
#'
#' Inspect an `lss` object and flag anomalies that can be detected without
#' any AI. The audit is meant to guide a human reviewer, not to silently
#' correct anything: every finding names a precise location and a severity.
#'
#' Checks performed:
#' * **Missing translations** -- a question, help, answer, or subquestion
#'   text that exists in at least one language but is empty in another.
#' * **Empty in all languages** -- a translatable text that is empty in every
#'   language.
#' * **Duplicate codes** -- a question variable code repeated in the survey,
#'   or an answer/subquestion code repeated within one question.
#' * **Missing options for the type** -- a question whose type requires
#'   answer options or subquestions but has none (per the type taxonomy).
#' * **Orphan references** -- a subquestion or answer that points to a
#'   question that does not exist.
#'
#' @param lss An `lss` object returned by [parse_lss()].
#'
#' @return An object of class `lss_audit`: a list with `file`, `languages`,
#'   summary counts, and a `findings` data frame (`severity`, `check`,
#'   `location`, `language`, `message`). It has a `print()` method and an
#'   `as.data.frame()` method.
#'
#' @examples
#' lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
#'   package = "lssdoc"
#' ))
#' audit <- audit_lss(lss)
#' print(audit)
#' @export
audit_lss <- function(lss) {
  if (!inherits(lss, "lss")) {
    lssdoc_abort(
      "{.arg lss} must be an {.cls lss} object from {.fn parse_lss}.",
      class = "lssdoc_bad_lss"
    )
  }

  langs <- lss$languages
  model <- lss_model(lss, languages = langs)

  findings <- lss_finding_collector()

  for (group in model$groups) {
    gname <- lss_first_label(group$names, langs)
    lss_audit_text(
      findings, group$names, langs,
      location = lss_locate("Group", gname),
      kind = "group name",
      empty_severity = "warning"
    )

    for (q in group$questions) {
      qloc <- lss_locate("Question", q$code)

      # Question text and help across languages. An equation legitimately
      # carries no display text (its formula lives in the attributes), so an
      # empty equation text is a note rather than an error.
      q_text <- lapply(langs, function(l) q$texts[[l]]$question)
      names(q_text) <- langs
      lss_audit_text(
        findings, q_text, langs,
        location = qloc, kind = "question text",
        empty_severity = if (identical(q$type, "*")) "note" else "error"
      )

      q_help <- lapply(langs, function(l) q$texts[[l]]$help)
      names(q_help) <- langs
      lss_audit_text(
        findings, q_help, langs,
        location = qloc, kind = "help text",
        empty_severity = NA, # help may legitimately be empty everywhere
        missing_severity = "note"
      )

      # Type expectations.
      info <- lss_type_info(q$type)
      if (isTRUE(info$has_answers) && length(q$answers) == 0) {
        findings$add(
          "warning", "missing_options", qloc, NA_character_,
          sprintf(
            "Type '%s' expects answer options, but none are defined.",
            q$type_label
          )
        )
      }
      if (isTRUE(info$has_subquestions) && length(q$subquestions) == 0) {
        findings$add(
          "warning", "missing_subquestions", qloc, NA_character_,
          sprintf(
            "Type '%s' expects subquestions, but none are defined.",
            q$type_label
          )
        )
      }

      # Answer-option labels and duplicate answer codes (per scale).
      if (length(q$answers) > 0) {
        lss_audit_codes(
          findings,
          codes = vapply(q$answers, function(a) a$code, character(1)),
          groups = vapply(q$answers, function(a) a$scale_id, character(1)),
          location = qloc, kind = "answer code"
        )
        for (a in q$answers) {
          lss_audit_text(
            findings, a$labels, langs,
            location = lss_locate("Answer", paste0(q$code, " = ", a$code)),
            kind = "answer text",
            empty_severity = "warning"
          )
        }
      }

      # Subquestion texts and duplicate subquestion codes.
      if (length(q$subquestions) > 0) {
        lss_audit_codes(
          findings,
          codes = vapply(q$subquestions, function(s) s$code, character(1)),
          groups = vapply(q$subquestions, function(s) s$scale_id, character(1)),
          location = qloc, kind = "subquestion code"
        )
        for (s in q$subquestions) {
          s_text <- lapply(langs, function(l) s$texts[[l]]$question)
          names(s_text) <- langs
          lss_audit_text(
            findings, s_text, langs,
            location = lss_locate("Subquestion", paste0(q$code, " / ", s$code)),
            kind = "subquestion text",
            empty_severity = "warning"
          )
        }
      }
    }
  }

  # Survey-wide duplicate question codes.
  if (!is.null(lss$questions)) {
    lss_audit_codes(
      findings,
      codes = lss$questions$title,
      groups = rep("", nrow(lss$questions)),
      location = lss_locate("Survey", NA),
      kind = "question code"
    )
  }

  # Orphan structural references.
  lss_audit_orphans(findings, lss)

  findings_df <- findings$as_data_frame()
  structure(
    list(
      file = lss$file,
      languages = langs,
      n_findings = nrow(findings_df),
      n_errors = sum(findings_df$severity == "error"),
      n_warnings = sum(findings_df$severity == "warning"),
      n_notes = sum(findings_df$severity == "note"),
      findings = findings_df
    ),
    class = "lss_audit"
  )
}

#' Mutable collector for audit findings
#' @keywords internal
#' @noRd
lss_finding_collector <- function() {
  store <- new.env(parent = emptyenv())
  store$rows <- list()
  list(
    add = function(severity, check, location, language, message) {
      store$rows[[length(store$rows) + 1L]] <- data.frame(
        severity = severity,
        check = check,
        location = location,
        language = language,
        message = message,
        stringsAsFactors = FALSE
      )
      invisible()
    },
    as_data_frame = function() {
      if (length(store$rows) == 0) {
        return(data.frame(
          severity = character(0),
          check = character(0),
          location = character(0),
          language = character(0),
          message = character(0),
          stringsAsFactors = FALSE
        ))
      }
      out <- do.call(rbind, store$rows)
      sev_rank <- match(out$severity, c("error", "warning", "note"))
      out[order(sev_rank, out$check, out$location), , drop = FALSE]
    }
  )
}

#' Build a concise human-readable location string
#' @keywords internal
#' @noRd
lss_locate <- function(kind, code) {
  if (is.null(code) || is.na(code) || !nzchar(code)) {
    return(kind)
  }
  paste0(kind, " '", code, "'")
}

#' First non-empty localized label across languages
#' @keywords internal
#' @noRd
lss_first_label <- function(values_by_lang, langs) {
  for (l in langs) {
    v <- values_by_lang[[l]]
    if (!is.null(v) && !is.na(v) && nzchar(trimws(v))) {
      return(v)
    }
  }
  NA_character_
}

#' Check a localized text set for missing translations / emptiness
#'
#' @param missing_severity Severity for a translation missing in some but not
#'   all languages. Defaults to `"warning"`.
#' @param empty_severity Severity for a text empty in every language, or `NA`
#'   to skip that check (e.g. optional help text).
#' @keywords internal
#' @noRd
lss_audit_text <- function(findings, values_by_lang, langs, location, kind,
                           empty_severity = "warning",
                           missing_severity = "warning") {
  present <- vapply(langs, function(l) {
    v <- values_by_lang[[l]]
    !is.null(v) && !is.na(v) && nzchar(trimws(v))
  }, logical(1))

  if (!any(present)) {
    if (!is.na(empty_severity)) {
      findings$add(
        empty_severity, "empty_in_all_languages", location, NA_character_,
        sprintf("The %s is empty in every language.", kind)
      )
    }
    return(invisible())
  }
  if (!all(present)) {
    missing <- langs[!present]
    for (l in missing) {
      findings$add(
        missing_severity, "missing_translation", location, l,
        sprintf("The %s is missing in '%s' but present in other languages.", kind, l)
      )
    }
  }
  invisible()
}

#' Flag duplicate codes within groups
#' @keywords internal
#' @noRd
lss_audit_codes <- function(findings, codes, groups, location, kind) {
  codes <- as.character(codes)
  key <- paste(groups, codes, sep = "\r")
  dup <- key %in% key[duplicated(key)]
  for (d in unique(codes[dup])) {
    findings$add(
      "error", "duplicate_code", location, NA_character_,
      sprintf("Duplicate %s: '%s'.", kind, d)
    )
  }
  invisible()
}

#' Flag subquestions and answers that reference a missing question
#' @keywords internal
#' @noRd
lss_audit_orphans <- function(findings, lss) {
  qids <- if (is.null(lss$questions)) character(0) else lss$questions$qid

  if (!is.null(lss$subquestions) && nrow(lss$subquestions) > 0) {
    orphan <- !(lss$subquestions$parent_qid %in% qids)
    for (i in which(orphan)) {
      findings$add(
        "error", "orphan_subquestion",
        lss_locate("Subquestion", lss$subquestions$title[i]), NA_character_,
        sprintf(
          "Subquestion points to question id '%s', which does not exist.",
          lss$subquestions$parent_qid[i]
        )
      )
    }
  }

  if (!is.null(lss$answers) && nrow(lss$answers) > 0) {
    orphan <- !(lss$answers$qid %in% qids)
    for (i in which(orphan)) {
      findings$add(
        "error", "orphan_answer",
        lss_locate("Answer", lss$answers$code[i]), NA_character_,
        sprintf(
          "Answer points to question id '%s', which does not exist.",
          lss$answers$qid[i]
        )
      )
    }
  }
  invisible()
}

#' @export
print.lss_audit <- function(x, ...) {
  cli::cli_h1("lssdoc audit")
  cli::cli_text("{.field File}: {.path {x$file}}")
  cli::cli_text("{.field Languages}: {.val {x$languages}}")

  if (x$n_findings == 0) {
    cli::cli_alert_success("No anomalies detected.")
    return(invisible(x))
  }

  cli::cli_text(
    "{.strong {x$n_findings}} finding{?s}: ",
    "{x$n_errors} error{?s}, {x$n_warnings} warning{?s}, {x$n_notes} note{?s}."
  )

  symbols <- c(error = "x", warning = "warning", note = "i")
  for (i in seq_len(nrow(x$findings))) {
    f <- x$findings[i, ]
    where <- if (is.na(f$language)) f$location else paste0(f$location, " [", f$language, "]")
    cli::cli_bullets(stats::setNames(
      paste0("{.strong ", where, "}: ", f$message),
      symbols[[f$severity]]
    ))
  }
  invisible(x)
}

#' @export
as.data.frame.lss_audit <- function(x, ...) {
  x$findings
}

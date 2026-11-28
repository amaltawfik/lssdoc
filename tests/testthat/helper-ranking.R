# A small two-language survey with one ranking, written as a `.lss` file in
# one of the forms a file can hold a ranking in, and read back:
# - "answers": the items as answer options (LimeSurvey 6 and the versions
#   before it, the first beta of 7.0.0);
# - "R": the items as subquestions of type R (LimeSurvey 7.12.1);
# - "T": the items as subquestions of type T (the form of a file with
#   DBVersion 703 that ships with LimeSurvey);
# - "both": the first items as subquestions of type R, the others as answer
#   options.
# The ranking `rk` is followed by a short text `nxt` whose condition is on a
# rank (`nxt_relevance`). `attrs` holds the attribute rows of the ranking, as
# c(attribute, value) pairs. `drop` names an item whose French text is left
# out ("B" leaves out the French row of item B). `subquestions = FALSE`
# writes no <subquestions> section at all (form "answers" only).
# `relevance` is the condition of item B when it is a subquestion.

rk_items <- function() {
  list(
    c("A", "Apples", "Pommes"),
    c("B", "<strong>Bread</strong> rolls", "Petits <strong>pains</strong>"),
    c("C", "Cheese", "Fromage")
  )
}

rk_section <- function(name, rows) {
  if (is.null(rows)) return(character(0))
  fields <- unique(unlist(lapply(rows, names)))
  out <- c(paste0(" <", name, ">"), "  <fields>",
           paste0("   <fieldname>", fields, "</fieldname>"), "  </fields>",
           "  <rows>")
  for (r in rows) {
    cells <- vapply(names(r), function(f) {
      sprintf("    <%s><![CDATA[%s]]></%s>", f, r[[f]], f)
    }, "")
    out <- c(out, "   <row>", unname(cells), "   </row>")
  }
  c(out, "  </rows>", paste0(" </", name, ">"))
}

rk_xml <- function(form = "answers", items = rk_items(), attrs = list(),
                   drop = NULL, subquestions = TRUE, relevance = "1",
                   nxt_relevance = "rk_1.NAOK == \"B\"") {
  langs <- c("en", "fr")
  n <- length(items)
  as_answer <- switch(form, answers = rep(TRUE, n), both = seq_len(n) > 1L,
                      rep(FALSE, n))
  sq_type <- if (identical(form, "T")) "T" else "R"
  q <- function(qid, type, title, order, theme, relevance) {
    list(qid = qid, parent_qid = "0", sid = "123456", gid = "1", type = type,
         title = title, other = "N", mandatory = "N",
         question_order = order, scale_id = "0", same_default = "0",
         relevance = relevance, question_theme_name = theme)
  }
  l10n <- list(
    list(id = "1", qid = "10", question = "Rank these foods.", help = "",
         script = "", language = "en"),
    list(id = "2", qid = "10", question = "Classez ces aliments.", help = "",
         script = "", language = "fr"),
    list(id = "3", qid = "11", question = "Why bread?", help = "",
         script = "", language = "en"),
    list(id = "4", qid = "11", question = "Pourquoi le pain ?", help = "",
         script = "", language = "fr")
  )
  sq <- list()
  ans <- list()
  al10n <- list()
  for (i in seq_len(n)) {
    it <- items[[i]]
    for (k in seq_along(langs)) {
      if (identical(drop, it[1]) && identical(langs[k], "fr")) next
      if (as_answer[i]) {
        al10n[[length(al10n) + 1L]] <- list(
          id = as.character(200 + 10 * i + k), aid = as.character(100 + i),
          answer = it[1 + k], language = langs[k]
        )
      } else {
        l10n[[length(l10n) + 1L]] <- list(
          id = as.character(300 + 10 * i + k), qid = as.character(20 + i),
          question = it[1 + k], help = "", script = "", language = langs[k]
        )
      }
    }
    if (as_answer[i]) {
      ans[[length(ans) + 1L]] <- list(
        aid = as.character(100 + i), qid = "10", code = it[1],
        sortorder = as.character(i), assessment_value = "0", scale_id = "0"
      )
    } else {
      sq[[length(sq) + 1L]] <- list(
        qid = as.character(20 + i), parent_qid = "10", sid = "123456",
        gid = "1", type = sq_type, title = it[1], other = "N",
        mandatory = "N", question_order = as.character(i - 1L),
        scale_id = "0", same_default = "0",
        relevance = if (identical(it[1], "B")) relevance else "1",
        encrypted = "N", same_script = "0"
      )
    }
  }
  qa <- lapply(attrs, function(a) {
    list(qid = "10", attribute = a[1], value = a[2], language = "")
  })
  c(
    '<?xml version="1.0" encoding="UTF-8"?>',
    "<document>",
    " <LimeSurveyDocType>Survey</LimeSurveyDocType>",
    " <DBVersion>700</DBVersion>",
    " <languages>", "  <language>en</language>", "  <language>fr</language>",
    " </languages>",
    rk_section("surveys", list(list(
      sid = "123456", language = "en", additional_languages = "fr",
      active = "N"
    ))),
    rk_section("surveys_languagesettings", list(
      list(surveyls_survey_id = "123456", surveyls_language = "en",
           surveyls_title = "Foods"),
      list(surveyls_survey_id = "123456", surveyls_language = "fr",
           surveyls_title = "Aliments")
    )),
    rk_section("groups", list(list(
      gid = "1", sid = "123456", group_order = "1", randomization_group = "",
      grelevance = "1"
    ))),
    rk_section("group_l10ns", list(
      list(id = "1", gid = "1", group_name = "Food", description = "",
           language = "en"),
      list(id = "2", gid = "1", group_name = "Nourriture", description = "",
           language = "fr")
    )),
    rk_section("questions", list(
      q("10", "R", "rk", "1", "ranking", "1"),
      q("11", "S", "nxt", "2", "shortfreetext", nxt_relevance)
    )),
    rk_section("question_l10ns", l10n),
    if (subquestions || length(sq)) {
      if (length(sq)) {
        rk_section("subquestions", sq)
      } else {
        c(" <subquestions>", "  <fields>",
          paste0("   <fieldname>", c("qid", "parent_qid", "sid", "gid",
                                      "type", "title", "question_order",
                                      "scale_id", "relevance"),
                 "</fieldname>"),
          "  </fields>", "  <rows>", "  </rows>", " </subquestions>")
      }
    },
    if (length(ans)) rk_section("answers", ans),
    if (length(al10n)) rk_section("answer_l10ns", al10n),
    if (length(qa)) rk_section("question_attributes", qa),
    "</document>"
  )
}

# The survey of rk_xml(), read back.
rk_lss <- function(...) {
  f <- tempfile(fileext = ".lss")
  on.exit(unlink(f), add = TRUE)
  writeLines(rk_xml(...), f)
  read_lss(f)
}

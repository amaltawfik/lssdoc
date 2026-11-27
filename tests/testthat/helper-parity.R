# The two layouts of render_documentation() hold the same content: what the
# cards print about the survey, the table prints too, in its denser form.
# These helpers render a survey in both layouts and read the documents back
# with xml2, so that a test can find a text, a cell or a row; and they build
# `parity_lss()`, the demo survey changed to carry every case the two
# layouts have to agree on.

parity_w <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

# The text of a node: its runs joined, a line break as "\n", its paragraphs
# joined by "\n".
parity_node_text <- function(node) {
  paras <- xml2::xml_find_all(node, "descendant-or-self::w:p", parity_w)
  paste(vapply(paras, function(p) {
    bits <- xml2::xml_find_all(p, ".//w:r/w:t | .//w:r/w:br", parity_w)
    paste(ifelse(xml2::xml_name(bits) == "br", "\n", xml2::xml_text(bits)),
          collapse = "")
  }, ""), collapse = "\n")
}

# The rows of a table, each the texts of its cells. A cell that spans
# several grid columns gives its text to each of them, so that every row of
# a table has as many entries as the table has columns.
parity_table_rows <- function(tbl) {
  lapply(xml2::xml_find_all(tbl, "w:tr", parity_w), function(tr) {
    tcs <- xml2::xml_find_all(tr, "w:tc", parity_w)
    unlist(lapply(tcs, function(tc) {
      span <- xml2::xml_attr(
        xml2::xml_find_first(tc, "w:tcPr/w:gridSpan", parity_w), "val"
      )
      rep(parity_node_text(tc), if (is.na(span)) 1L else as.integer(span))
    }))
  })
}

# A rendered document, read back:
# - `tables`: every top-level table of the body, in order, as its rows;
# - `content`: the texts of the questionnaire section, one entry per body
#   paragraph and per table cell, from the heading "Questionnaire" to the
#   next section (quotas or variable index). The cover, the table of
#   contents, the consent and the back matter are left out, so that a text
#   they print is not taken for a text of the questionnaire;
# - `xml`: the body of the document (word/document.xml), for a test that
#   reads the properties of a cell or a run.
parity_read_docx <- function(path) {
  dir <- tempfile()
  on.exit(unlink(dir, recursive = TRUE), add = TRUE)
  utils::unzip(path, exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  kids <- xml2::xml_find_all(doc, "/w:document/w:body/*", parity_w)
  marks <- vapply(kids, function(k) {
    b <- xml2::xml_find_first(k, ".//w:bookmarkStart", parity_w)
    if (inherits(b, "xml_missing")) NA_character_ else xml2::xml_attr(b, "name")
  }, character(1))
  start <- which(marks %in% lss_section_bookmark("questionnaire"))[1]
  end <- which(marks %in% lss_section_bookmark(c("quotas", "index")))
  end <- end[end > start]
  end <- if (length(end)) end[1] - 1L else length(kids)
  content <- unlist(lapply(kids[start:end], function(k) {
    if (identical(xml2::xml_name(k), "tbl")) {
      vapply(xml2::xml_find_all(k, ".//w:tc", parity_w), parity_node_text, "")
    } else if (identical(xml2::xml_name(k), "p")) {
      parity_node_text(k)
    }
  }))
  is_tbl <- xml2::xml_name(kids) == "tbl"
  list(
    tables = lapply(kids[is_tbl], parity_table_rows),
    content = content,
    xml = doc
  )
}

# Render `lss` in one layout and read the document back. Two languages, the
# chrome in English, no audit section. With `key`, the result is cached for
# the session (in the cache of helper-fixtures.R), so that the test files
# that read the same render share it.
parity_render <- function(lss, template, languages = c("en", "fr"),
                          key = NULL, ...) {
  if (!is.null(key)) {
    key <- paste("render", key, template, paste(languages, collapse = "-"))
    hit <- lss_fixture_cache[[key]]
    if (!is.null(hit)) return(hit)
  }
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(suppressWarnings(render_documentation(
    lss, out, languages = languages, template = template,
    chrome_lang = "en", show_audit = FALSE, ...
  )))
  res <- parity_read_docx(out)
  if (!is.null(key)) lss_fixture_cache[[key]] <- res
  res
}

# The dense table of the table layout as a character matrix: one row per
# body row, the columns named Field, No, Variable, Type, Mandatory, Filter,
# Value and then the codes of the languages.
parity_table_grid <- function(doc, languages = c("en", "fr")) {
  cols <- c("Field", "No", "Variable", "Type", "Mandatory", "Filter",
            "Value", languages)
  big <- Filter(function(t) {
    length(t) > 1L && length(t[[1]]) == length(cols) &&
      identical(t[[1]][3], "Variable")
  }, doc$tables)
  stopifnot(length(big) == 1L)
  rows <- big[[1]][-1L]
  grid <- matrix(unlist(rows), ncol = length(cols), byrow = TRUE)
  colnames(grid) <- cols
  grid
}

# The rows of the item table of the card that names `variable` in its meta
# band (the table that follows the band).
parity_card_rows <- function(doc, variable) {
  hit <- which(vapply(doc$tables, function(t) {
    any(vapply(t, function(r) variable %in% r, logical(1)))
  }, logical(1)))
  stopifnot(length(hit) >= 1L, hit[1] < length(doc$tables))
  doc$tables[[hit[1] + 1L]]
}

# The rows of the table layout that belong to the row whose Variable is
# `variable`: that row and the rows under it up to the next Question row
# or group row.
parity_grid_block <- function(grid, variable) {
  i <- which(grid[, "Variable"] == variable)
  stopifnot(length(i) == 1L)
  heads <- which(grid[, "Field"] %in% c("Question", "Group"))
  nxt <- heads[heads > i]
  last <- if (length(nxt)) nxt[1] - 1L else nrow(grid)
  grid[i:last, , drop = FALSE]
}

# The variables of the variable index (the last table, headed Variable, No).
parity_index_variables <- function(doc) {
  idx <- Filter(function(t) identical(t[[1]], c("Variable", "No")), doc$tables)
  stopifnot(length(idx) >= 1L)
  vapply(idx[[length(idx)]][-1L], function(r) r[[1]], "")
}

# Whitespace, the no-break spaces included, as one plain space.
parity_squish <- function(x) {
  trimws(gsub("[[:space:]\u00A0\u202F]+", " ", x, perl = TRUE))
}

# Is `text` printed in `content`? Each line of the text (a paragraph, an
# item of a list) must be found inside one entry, whitespace squished. With
# `whole`, for a text that fills its cell in both layouts (the value of an
# attribute), the whole text, whitespace squished, must be equal to one
# entry: a short text such as "h" is then not found inside another one.
# The bullet that both layouts put before an item of a list is not part of
# the text: it is dropped from the entries before a whole text is compared.
parity_found <- function(text, content, whole = FALSE) {
  hay <- parity_squish(content)
  if (isTRUE(whole)) {
    hay <- parity_squish(gsub("(^|\n)\u2022 ", "\\1", content))
    return(parity_squish(text) %in% hay)
  }
  lines <- parity_squish(strsplit(text, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  all(vapply(lines, function(l) any(grepl(l, hay, fixed = TRUE)), logical(1)))
}

# The value of an attribute in language `lg`: the row of that language, else
# the row without a language, else "".
parity_attr_value <- function(attrs, name, lg) {
  if (is.null(attrs) || !nrow(attrs)) return("")
  hit <- attrs[attrs$attribute %in% name, , drop = FALSE]
  v <- hit$value[hit$language %in% lg]
  if (length(v) && nzchar(trimws(v[1]))) return(v[1])
  v <- hit$value[!nzchar(hit$language)]
  if (length(v) && nzchar(trimws(v[1]))) return(v[1])
  ""
}

# Every text of the survey that the documentation is meant to print, in each
# language: one row per text, `id` naming it ("<question> stem",
# "<question>/<subquestion> subquestion", "<question>:<code> answer",
# "group <n> name", ...), `text` the plain text, read by the package's
# own HTML flattener, and `whole` TRUE for the value of an attribute, which
# fills its cell in both layouts (see parity_found()). The formula of a
# computed question is not HTML: it is read as written. Empty texts are left
# out: they have nothing to find.
parity_expected <- function(lss, languages = c("en", "fr"),
                            show_attrs = c("prefix", "suffix", "validation",
                                           "equation")) {
  m <- lss_model(lss, languages)
  chrome <- lss_chrome_strings("en")
  out <- list()
  add <- function(id, lg, html, whole = FALSE, raw = FALSE) {
    if (is.null(html) || length(html) == 0L || is.na(html[1])) return()
    text <- if (raw) html[1] else lss_html_to_text(html[1])
    if (!nzchar(trimws(text))) return()
    out[[length(out) + 1L]] <<- data.frame(
      id = id, lang = lg, text = text, whole = whole,
      stringsAsFactors = FALSE
    )
  }
  attrs_of <- function(id, x, lg) {
    for (a in show_attrs) {
      v <- parity_attr_value(x$attributes, a, lg)
      formula <- identical(a, "equation")
      if (formula && nzchar(v)) v <- lss_equation_formula(x)
      add(paste(id, a), lg, v, whole = TRUE, raw = formula)
    }
  }
  for (gi in seq_along(m$groups)) {
    g <- m$groups[[gi]]
    for (lg in languages) {
      add(sprintf("group %d name", gi), lg, g$names[[lg]])
      add(sprintf("group %d description", gi), lg, g$descriptions[[lg]])
    }
    for (q in g$questions) {
      for (lg in languages) {
        add(paste(q$code, "stem"), lg, q$texts[[lg]]$question)
        add(paste(q$code, "help"), lg, q$texts[[lg]]$help)
        attrs_of(q$code, q, lg)
        if (identical(q$other, "Y")) {
          add(paste(q$code, "other prompt"), lg,
              lss_other_prompt(q, lg, chrome))
        }
        for (sq in q$subquestions) {
          id <- paste0(q$code, "/", sq$code)
          add(paste(id, "subquestion"), lg, sq$texts[[lg]]$question)
          attrs_of(id, sq, lg)
        }
        for (a in q$answers) {
          add(sprintf("%s:%s:%s answer", q$code, a$scale_id, a$code), lg,
              a$labels[[lg]])
        }
      }
    }
  }
  do.call(rbind, out)
}

# The texts of `expected` that `doc` does not print, as "<id> <lang>".
parity_missing <- function(expected, doc) {
  found <- mapply(parity_found, expected$text, expected$whole,
                  MoreArgs = list(content = doc$content))
  paste(expected$id, expected$lang)[!found]
}

# The demo survey, changed so that it carries every case the two layouts
# must agree on. What the demo already has, and is kept: a prefix and a
# suffix on a leaf question (physicalactivitydays), suffixes (age, height,
# weight, workhours), the formula of a computed question (imc), an "Other"
# field on a single choice (respondentrole) and on a multiple choice
# (nationality), a list with comment (langhome, type O), helps, group names
# that differ by language. What is added here:
# - a prefix and a suffix on a question with subquestions (sleephours, a
#   multiple numeric);
# - a multiple choice with comments (socialmedia becomes type P);
# - inline formatting (bold, italic, underline, superscript, subscript, a
#   line break, a bulleted list) in a stem and an answer label
#   (healthself), in a help and a subquestion (loneliness), in an attribute
#   (the suffix of weight); bold, italic, underline, superscript, subscript
#   and a line break in the prompt of an "Other" field (respondentrole) and
#   in the label of an option of the multiple choice with comments
#   (socialmedia, its first option), which the row of its comment variable
#   repeats;
# - a text missing in French only: the stem of occupation, the first
#   subquestion of dailyresources, the first answer of incomefeel;
# - a description on the second group, with a bold and an italic word;
# - a group name that begins with a number the author typed ("1) ", fifth
#   group);
# - two exclusive options, separated by a semicolon, on the multiple choice
#   socialmedia (its last two options), and one exclusive subquestion on the
#   array dailyresources (its second subquestion);
# - a help missing in French only (nationality);
# - a help on the list with comment langhome, which the row of its comment
#   field does not repeat.
# The questions that carry none of these cases are dropped, one or more
# questions stay in every group, so that a render takes seconds.
parity_lss <- function() {
  hit <- lss_fixture_cache[["parity"]]
  if (!is.null(hit)) return(hit)
  lss <- demo_lss()
  qid <- function(code) lss$questions$qid[lss$questions$title == code]
  set_text <- function(id, lg, col, value) {
    hit <- lss$question_l10ns$qid == id & lss$question_l10ns$language == lg
    stopifnot(sum(hit) == 1L)
    lss$question_l10ns[[col]][hit] <<- value
  }
  # Prefix and suffix of a multiple numeric.
  add_attr <- function(id, attribute, value, language) {
    lss$question_attributes <<- rbind(
      lss$question_attributes,
      data.frame(qid = id, attribute = attribute, value = value,
                 language = language, stringsAsFactors = FALSE)
    )
  }
  add_attr(qid("sleephours"), "prefix", "approximately", "en")
  add_attr(qid("sleephours"), "prefix", "approximativement", "fr")
  add_attr(qid("sleephours"), "suffix", "hours a night", "en")
  add_attr(qid("sleephours"), "suffix", "heures par nuit", "fr")
  # A multiple choice with comments.
  sm <- lss$questions$title == "socialmedia"
  lss$questions$type[sm] <- "P"
  lss$questions$question_theme_name[sm] <- "multiplechoice_with_comments"
  lss$subquestions$type[lss$subquestions$parent_qid == qid("socialmedia")] <- "P"
  # Inline formatting.
  set_text(qid("healthself"), "en", "question", paste0(
    "<p>How is your <strong>health</strong> <em>in general</em>,<br>",
    "<u>today</u>?</p><ul><li>Lung volume in dm<sup>3</sup></li>",
    "<li>Blood CO<sub>2</sub> level</li></ul>"
  ))
  set_text(qid("healthself"), "fr", "question", paste0(
    "<p>Comment est votre <strong>sant\u00E9</strong> <em>en g\u00E9n\u00E9ral</em>,<br>",
    "<u>aujourd'hui</u>\u00A0?</p><ul><li>Volume pulmonaire en dm<sup>3</sup></li>",
    "<li>Taux de CO<sub>2</sub> sanguin</li></ul>"
  ))
  hs <- lss$answers$aid[lss$answers$qid == qid("healthself")][1]
  al <- lss$answer_l10ns$aid == hs
  lss$answer_l10ns$answer[al & lss$answer_l10ns$language == "en"] <- paste0(
    "<strong>Very</strong> <em>good</em> <u>health</u><sup>1</sup><br>",
    "H<sub>2</sub>O<ul><li>no pain</li></ul>"
  )
  lss$answer_l10ns$answer[al & lss$answer_l10ns$language == "fr"] <- paste0(
    "<strong>Tr\u00E8s</strong> <em>bonne</em> <u>sant\u00E9</u><sup>1</sup><br>",
    "H<sub>2</sub>O<ul><li>sans douleur</li></ul>"
  )
  set_text(qid("loneliness"), "en", "help", paste0(
    "<p>Think of the <em>last four weeks</em><sup>1</sup>.<br>",
    "Not CO<sub>2</sub>.</p>",
    "<ul><li>Count <strong>every</strong> day</li><li>Even <u>short</u> moments</li></ul>"
  ))
  set_text(qid("loneliness"), "fr", "help", paste0(
    "<p>Pensez aux <em>quatre derni\u00E8res semaines</em><sup>1</sup>.<br>",
    "Pas le CO<sub>2</sub>.</p>",
    "<ul><li>Comptez <strong>chaque</strong> jour</li><li>M\u00EAme les <u>brefs</u> moments</li></ul>"
  ))
  lsq <- lss$subquestions$qid[lss$subquestions$parent_qid == qid("loneliness")][1]
  set_text(lsq, "en", "question", paste0(
    "<strong>Left</strong> <em>out</em> of <u>the</u> H<sub>2</sub>O ",
    "talk<sup>a</sup><br>at work<ul><li>or at home</li></ul>"
  ))
  set_text(lsq, "fr", "question", paste0(
    "<strong>Laiss\u00E9</strong> <em>de c\u00F4t\u00E9</em> de <u>la</u> ",
    "discussion H<sub>2</sub>O<sup>a</sup><br>au travail",
    "<ul><li>ou \u00E0 la maison</li></ul>"
  ))
  # An attribute: the suffix of weight, one text per language.
  qa <- lss$question_attributes
  lss$question_attributes <- qa[!(qa$qid == qid("weight") &
                                    qa$attribute == "suffix"), , drop = FALSE]
  add_attr(qid("weight"), "suffix", paste0(
    "<strong>kg</strong> <em>net</em> <u>body</u> m<sup>3</sup> ",
    "CO<sub>2</sub><br>rounded<ul><li>no clothes</li></ul>"
  ), "en")
  add_attr(qid("weight"), "suffix", paste0(
    "<strong>kg</strong> <em>net</em> <u>corps</u> m<sup>3</sup> ",
    "CO<sub>2</sub><br>arrondi<ul><li>sans v\u00EAtements</li></ul>"
  ), "fr")
  # The prompt of an "Other" field.
  add_attr(qid("respondentrole"), "other_replace_text", paste0(
    "<strong>Other</strong> <em>role</em> <u>here</u><sup>1</sup> ",
    "H<sub>2</sub>O<br>specify:"
  ), "en")
  add_attr(qid("respondentrole"), "other_replace_text", paste0(
    "<strong>Autre</strong> <em>r\u00F4le</em> <u>ici</u><sup>1</sup> ",
    "H<sub>2</sub>O<br>pr\u00E9cisez\u00A0:"
  ), "fr")
  # The label of an option of the multiple choice with comments.
  fb <- lss$subquestions$qid[lss$subquestions$parent_qid == qid("socialmedia")][1]
  set_text(fb, "en", "question",
           "<strong>Face</strong><u>book</u><sup>1</sup><br><em>or</em> H<sub>2</sub>O")
  set_text(fb, "fr", "question",
           "<strong>Face</strong><u>book</u><sup>1</sup><br><em>ou</em> H<sub>2</sub>O")
  # A text missing in French only.
  set_text(qid("occupation"), "fr", "question", "")
  dsq <- lss$subquestions$qid[lss$subquestions$parent_qid == qid("dailyresources")]
  set_text(dsq[1], "fr", "question", "")
  ia <- lss$answers$aid[lss$answers$qid == qid("incomefeel")][1]
  lss$answer_l10ns$answer[lss$answer_l10ns$aid == ia &
                            lss$answer_l10ns$language == "fr"] <- ""
  # A description on the second group, a number typed by the author on the
  # fifth.
  gid <- lss$groups$gid[order(as.integer(lss$groups$group_order))]
  set_group <- function(g, lg, col, value) {
    hit <- lss$group_l10ns$gid == g & lss$group_l10ns$language == lg
    lss$group_l10ns[[col]][hit] <<- value
  }
  set_group(gid[2], "en", "description",
            "<p>These questions are about your <strong>health</strong> and <em>habits</em>.</p>")
  set_group(gid[2], "fr", "description",
            "<p>Ces questions portent sur votre <strong>sant\u00E9</strong> et vos <em>habitudes</em>.</p>")
  for (lg in c("en", "fr")) {
    hit <- lss$group_l10ns$gid == gid[5] & lss$group_l10ns$language == lg
    lss$group_l10ns$group_name[hit] <- paste0("1) ", lss$group_l10ns$group_name[hit])
  }
  # Exclusive options: the attribute replaced by one row without a language.
  set_exclusive <- function(id, value) {
    qa <- lss$question_attributes
    lss$question_attributes <<- qa[!(qa$qid == id &
                                       qa$attribute == "exclude_all_others"), ,
                                   drop = FALSE]
    add_attr(id, "exclude_all_others", value, "")
  }
  sq_codes <- function(code) {
    s <- lss$subquestions[lss$subquestions$parent_qid == qid(code), , drop = FALSE]
    s$title[order(as.integer(s$question_order))]
  }
  sm_codes <- sq_codes("socialmedia")
  set_exclusive(qid("socialmedia"),
                paste(utils::tail(sm_codes, 2L), collapse = ";"))
  set_exclusive(qid("dailyresources"), sq_codes("dailyresources")[2])
  # A help missing in French only.
  set_text(qid("nationality"), "fr", "help", "")
  set_text(qid("langhome"), "en", "help", "Choose the language you use most.")
  set_text(qid("langhome"), "fr", "help",
           "Choisissez la langue que vous utilisez le plus.")
  keep <- c("respondentrole", "age", "height", "weight", "imc",
            "nationality", "langhome", "healthself", "physicalactivitydays",
            "sleephours", "loneliness", "chroniccondition", "incomefeel",
            "dailyresources", "occupation", "adaptability", "socialmedia",
            "trustinstitutions", "devicerank", "surveyinterest")
  lss$questions <- lss$questions[lss$questions$title %in% keep, , drop = FALSE]
  lss_fixture_cache[["parity"]] <- lss
  lss
}

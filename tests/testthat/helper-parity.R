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
# item of a list) must be found inside one entry, whitespace squished.
parity_found <- function(text, content) {
  lines <- parity_squish(strsplit(text, "\n", fixed = TRUE)[[1]])
  lines <- lines[nzchar(lines)]
  hay <- parity_squish(content)
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
# "group <n> name", ...) and `text` the plain text, read by the package's
# own HTML flattener. Empty texts are left out: they have nothing to find.
parity_expected <- function(lss, languages = c("en", "fr"),
                            show_attrs = c("prefix", "suffix", "validation",
                                           "equation")) {
  m <- lss_model(lss, languages)
  chrome <- lss_chrome_strings("en")
  out <- list()
  add <- function(id, lg, html) {
    if (is.null(html) || length(html) == 0L || is.na(html[1])) return()
    text <- lss_html_to_text(html[1])
    if (!nzchar(trimws(text))) return()
    out[[length(out) + 1L]] <<- data.frame(
      id = id, lang = lg, text = text, stringsAsFactors = FALSE
    )
  }
  attrs_of <- function(id, x, lg) {
    for (a in show_attrs) {
      v <- parity_attr_value(x$attributes, a, lg)
      if (identical(a, "equation") && nzchar(v)) v <- lss_equation_formula(x)
      add(paste(id, a), lg, v)
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
  found <- vapply(expected$text, parity_found, logical(1),
                  content = doc$content)
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
#   bulleted list) in a stem and an answer label (healthself), in a help and
#   a subquestion (loneliness);
# - a text missing in French only: the stem of occupation, the first
#   subquestion of dailyresources, the first answer of incomefeel;
# - a description on the second group;
# - a group name that begins with a number the author typed ("1) ", fifth
#   group).
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
    "<p>How is your <strong>health</strong> <em>in general</em>, ",
    "<u>today</u>?</p><ul><li>Lung volume in dm<sup>3</sup></li>",
    "<li>Blood CO<sub>2</sub> level</li></ul>"
  ))
  set_text(qid("healthself"), "fr", "question", paste0(
    "<p>Comment est votre <strong>sant\u00E9</strong> <em>en g\u00E9n\u00E9ral</em>, ",
    "<u>aujourd'hui</u>\u00A0?</p><ul><li>Volume pulmonaire en dm<sup>3</sup></li>",
    "<li>Taux de CO<sub>2</sub> sanguin</li></ul>"
  ))
  hs <- lss$answers$aid[lss$answers$qid == qid("healthself")][1]
  al <- lss$answer_l10ns$aid == hs
  lss$answer_l10ns$answer[al & lss$answer_l10ns$language == "en"] <-
    "<strong>Very</strong> good<sup>1</sup>"
  lss$answer_l10ns$answer[al & lss$answer_l10ns$language == "fr"] <-
    "<strong>Tr\u00E8s</strong> bonne<sup>1</sup>"
  set_text(qid("loneliness"), "en", "help", paste0(
    "<p>Think of the <em>last four weeks</em>.</p>",
    "<ul><li>Count <strong>every</strong> day</li><li>Even <u>short</u> moments</li></ul>"
  ))
  set_text(qid("loneliness"), "fr", "help", paste0(
    "<p>Pensez aux <em>quatre derni\u00E8res semaines</em>.</p>",
    "<ul><li>Comptez <strong>chaque</strong> jour</li><li>M\u00EAme les <u>brefs</u> moments</li></ul>"
  ))
  lsq <- lss$subquestions$qid[lss$subquestions$parent_qid == qid("loneliness")][1]
  set_text(lsq, "en", "question", "Left <em>out</em> of H<sub>2</sub>O talk")
  set_text(lsq, "fr", "question", "Laiss\u00E9 <em>de c\u00F4t\u00E9</em> du H<sub>2</sub>O")
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
            "<p>These questions are about your <strong>health</strong>.</p>")
  set_group(gid[2], "fr", "description",
            "<p>Ces questions portent sur votre <strong>sant\u00E9</strong>.</p>")
  for (lg in c("en", "fr")) {
    hit <- lss$group_l10ns$gid == gid[5] & lss$group_l10ns$language == lg
    lss$group_l10ns$group_name[hit] <- paste0("1) ", lss$group_l10ns$group_name[hit])
  }
  keep <- c("respondentrole", "age", "height", "weight", "imc",
            "nationality", "langhome", "healthself", "physicalactivitydays",
            "sleephours", "loneliness", "chroniccondition", "incomefeel",
            "dailyresources", "occupation", "adaptability", "socialmedia",
            "trustinstitutions", "devicerank", "surveyinterest")
  lss$questions <- lss$questions[lss$questions$title %in% keep, , drop = FALSE]
  lss_fixture_cache[["parity"]] <- lss
  lss
}

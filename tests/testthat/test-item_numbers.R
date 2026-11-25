# A survey with every case the numbering distinguishes: a single choice with
# "Other", a list with comment, a multiple choice with "Other", a three-row
# array, a ranking, then a group opened by a text display and holding an
# equation, a hidden short text, a numeric input (hidden = 0, so asked) and a
# hidden equation. lss_spec() authors no equation and no `hidden` attribute,
# so both are set on the parsed object.
numbering_lss <- function() {
  hit <- lss_fixture_cache[["numbering"]]
  if (!is.null(hit)) {
    return(hit)
  }
  spec <- lss_example_spec(
    kinds = c(
      "single",
      "singlecomment",
      "multiple",
      "array",
      "ranking",
      "text",
      "shorttext",
      "numeric",
      "date",
      "display"
    ),
    languages = c("en", "fr")
  )
  file <- tempfile(fileext = ".lss")
  on.exit(unlink(file), add = TRUE)
  suppressMessages(write_lss(spec, file))
  lss <- read_lss(file)
  qs <- lss$questions
  qs$question_order[qs$title == "Q10Display"] <- "0"
  eq <- qs$title %in% c("Q06Text", "Q09Date")
  qs$type[eq] <- "*"
  qs$question_theme_name[eq] <- "equation"
  lss$questions <- qs
  hid <- data.frame(
    qid = qs$qid[match(c("Q07Shorttext", "Q09Date", "Q08Numeric"), qs$title)],
    attribute = "hidden",
    value = c("1", "1", "0"),
    language = "",
    stringsAsFactors = FALSE
  )
  qa <- lss$question_attributes
  lss$question_attributes <- rbind(qa, hid[, names(qa)])
  lss_fixture_cache[["numbering"]] <- lss
  lss
}

DASH <- "\u2013"
HIDDEN_EN <- "Question hidden from respondents"

# Rows of every table of a rendered document, in document order.
docx_rows <- function(s) {
  tb <- s[s$content_type %in% "table cell", , drop = FALSE]
  tb <- tb[order(tb$table_index, tb$row_id, tb$cell_id), , drop = FALSE]
  key <- paste(tb$table_index, tb$row_id)
  lapply(unique(key), function(k) {
    r <- tb[key == k, , drop = FALSE]
    list(
      table = r$table_index[1],
      header = isTRUE(r$is_header[1]),
      cells = r$text
    )
  })
}

render_summary <- function(lss, ...) {
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(render_documentation(lss, out, ...))
  officer::docx_summary(officer::read_docx(out))
}

# The body row under each header row that starts with `first`, keyed by the
# cell at `key_col`; `value_col` is the value kept.
rows_under <- function(rows, first, key_col, value_col) {
  out <- character(0)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (!r$header || !identical(r$cells[seq_along(first)], first)) {
      next
    }
    j <- i + 1L
    while (
      j <= length(rows) && rows[[j]]$table == r$table && !rows[[j]]$header
    ) {
      cells <- rows[[j]]$cells
      if (length(cells) >= max(key_col, value_col)) {
        out[[length(out) + 1L]] <- cells[[value_col]]
        names(out)[length(out)] <- cells[[key_col]]
      }
      j <- j + 1L
    }
  }
  out
}

# Cards: the No of each meta band, and the variables whose band carries a
# footer line (the hidden mention).
card_numbers <- function(rows, no = "No") {
  meta <- character(0)
  footer <- character(0)
  for (i in seq_along(rows)) {
    r <- rows[[i]]
    if (!r$header || !identical(r$cells[1:2], c(no, "Variable"))) {
      next
    }
    body <- rows[[i + 1L]]$cells
    meta[[body[[2]]]] <- body[[1]]
    if (i + 2L > length(rows)) {
      next
    }
    nxt <- rows[[i + 2L]]
    if (nxt$table == r$table && !nxt$header) {
      footer[[body[[2]]]] <- nxt$cells[[1]]
    }
  }
  list(meta = meta, footer = footer)
}

# The cover metadata, the document's first table: field -> value.
cover_fields <- function(rows) {
  first <- Filter(function(r) r$table == rows[[1]]$table, rows)
  vapply(first, function(r) r$cells[[2]], character(1)) |>
    stats::setNames(vapply(first, function(r) r$cells[[1]], character(1)))
}

cards_expected <- c(
  "Q01Single" = "1",
  "Q01Single[_Cother]" = "1",
  "Q02Singlecomment" = "2",
  "Q02Singlecomment[_Ccomment]" = "2",
  "Q03Multiple[*]" = "3",
  "Q03Multiple[other]" = "3",
  "Q04Array[1]" = "4.1",
  "Q04Array[2]" = "4.2",
  "Q04Array[3]" = "4.3",
  "Q05Ranking[*]" = "5",
  "Q10Display" = DASH,
  "Q06Text" = DASH,
  "Q07Shorttext" = DASH,
  "Q08Numeric" = "6",
  "Q09Date" = DASH
)

# ---- The pure numbering ---------------------------------------------------

test_that("lss_item_numbers numbers the questions asked, in document order", {
  nums <- lss_item_numbers(lss_model(numbering_lss()))
  expect_identical(
    nums$code,
    c(
      "Q01Single",
      "Q02Singlecomment",
      "Q03Multiple",
      "Q04Array",
      "Q05Ranking",
      "Q10Display",
      "Q06Text",
      "Q07Shorttext",
      "Q08Numeric",
      "Q09Date"
    )
  )
  expect_identical(
    nums$status,
    c(
      rep("asked", 5),
      "display",
      "equation",
      "hidden",
      "asked",
      "equation"
    )
  )
  expect_identical(nums$hidden, c(rep(FALSE, 7), TRUE, FALSE, TRUE))
  expect_identical(
    nums$label,
    c("1", "2", "3", "4", "5", DASH, DASH, DASH, "6", DASH)
  )
  expect_identical(max(nums$no, na.rm = TRUE), sum(nums$status == "asked"))
})

test_that("the numbers do not depend on the displayed languages", {
  lss <- numbering_lss()
  expect_identical(
    lss_item_numbers(lss_model(lss, "fr")),
    lss_item_numbers(lss_model(lss, c("en", "fr")))
  )
})

test_that("lss_item_label gives the question, a sub-item, an en dash or NA", {
  nums <- lss_item_numbers(lss_model(numbering_lss()))
  qid <- function(code) nums$qid[nums$code == code]
  expect_identical(lss_item_label(nums, qid("Q04Array")), "4")
  expect_identical(lss_item_label(nums, qid("Q04Array"), 3L), "4.3")
  expect_identical(lss_item_label(nums, qid("Q07Shorttext")), DASH)
  expect_identical(lss_item_label(nums, qid("Q07Shorttext"), 2L), DASH)
  expect_identical(lss_item_label(nums, "no-such-qid"), NA_character_)
  expect_identical(lss_item_label(nums, NULL), NA_character_)
  expect_identical(lss_item_label(nums, NA_character_), NA_character_)
  expect_identical(nrow(lss_item_numbers(NULL)), 0L)
  expect_type(lss_item_numbers(NULL)$label, "character")
})

test_that("lss_question_hidden reads the hidden attribute only", {
  attrs <- function(attribute, value) {
    data.frame(
      qid = rep("1", length(value)),
      attribute = attribute,
      value = value,
      language = rep("", length(value)),
      stringsAsFactors = FALSE
    )
  }
  expect_true(lss_question_hidden(attrs("hidden", "1")))
  expect_true(lss_question_hidden(attrs(c("prefix", "hidden"), c("1", " 1 "))))
  expect_false(lss_question_hidden(attrs("hidden", "0")))
  expect_false(lss_question_hidden(attrs("hide_tip", "1")))
  expect_false(lss_question_hidden(attrs(character(0), character(0))))
  expect_false(lss_question_hidden(NULL))
})

test_that("the demo survey's last number is its count of questions asked", {
  nums <- lss_item_numbers(lss_model(demo_lss()))
  expect_identical(sum(nums$status == "asked"), 44L)
  expect_identical(max(nums$no, na.rm = TRUE), 44L)
  expect_identical(sum(nums$status == "display"), 2L)
  # `imc` is a hidden equation: counted once, as an equation.
  expect_identical(nums$status[nums$code == "imc"], "equation")
  expect_true(nums$hidden[nums$code == "imc"])
  expect_identical(sum(nums$status == "hidden"), 0L)
})

test_that("table rows number dual-scale and two-dimensional cells in order", {
  model <- lss_model(demo_lss(), languages = "en")
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings("en")
  state <- lss_render_state(model)
  rows <- unlist(
    lapply(model$groups, function(g) {
      lss_table_template_rows_for_group(
        g,
        "en",
        th,
        show_help = FALSE,
        state = state
      )
    }),
    recursive = FALSE
  )
  nums <- state$numbers
  for (code in c("trustinstitutions", "adaptability")) {
    sub <- Filter(
      function(r) {
        identical(r$kind, "subq") &&
          startsWith(r$variable, paste0(code, "["))
      },
      rows
    )
    no <- vapply(sub, function(r) r$no, character(1))
    expect_identical(
      no,
      paste0(nums$no[nums$code == code], ".", seq_along(no)),
      info = code
    )
    expect_true(length(no) > 1L, info = code)
  }
})

# ---- Rendered documents ----------------------------------------------------

test_that("cards carry question numbers, sub-item numbers and en dashes", {
  testthat::skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  s <- render_summary(
    numbering_lss(),
    template = "cards",
    chrome_lang = "en",
    show_item_heading = TRUE
  )
  rows <- docx_rows(s)

  cards <- card_numbers(rows)
  expect_identical(cards$meta, cards_expected)
  expect_identical(names(cards$footer), c("Q07Shorttext", "Q09Date"))
  expect_true(all(cards$footer == HIDDEN_EN))

  paras <- s$text[s$content_type %in% "paragraph"]
  for (h in c(
    "1. Q01Single",
    "1. Q01Single[_Cother]",
    "3. Q03Multiple",
    "4.1. Q04Array[1]",
    "4.3. Q04Array[3]",
    "5. Q05Ranking",
    "Q10Display",
    "Q06Text",
    "Q07Shorttext",
    "6. Q08Numeric",
    "Q09Date"
  )) {
    expect_true(h %in% paras, info = h)
  }
  expect_false(any(grepl(paste0("^(", DASH, "|NA)"), paras)))

  index <- rows_under(rows, c("Variable", "No"), 1L, 2L)
  expect_identical(
    unname(index[c(
      "Q03Multiple[1]",
      "Q03Multiple[4]",
      "Q03Multiple[other]",
      "Q04Array[2]",
      "Q02Singlecomment[_Ccomment]",
      "Q07Shorttext",
      "Q08Numeric"
    )]),
    c("3", "3", "3", "4.2", "2", DASH, "6")
  )
  expect_true(all(index[startsWith(names(index), "Q05Ranking[")] == "5"))

  cover <- cover_fields(rows)
  expect_identical(
    unname(cover[c("Questions", "Display", "Computed", "Hidden questions")]),
    c("6", "1 (not numbered)", "2 (not numbered)", "1 (not numbered)")
  )
  last <- max(as.integer(sub("\\..*$", "", cards$meta[cards$meta != DASH])))
  expect_identical(as.character(last), unname(cover["Questions"]))
})

test_that("the table template numbers parent, option and rank rows", {
  testthat::skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  s <- render_summary(numbering_lss(), template = "table", chrome_lang = "en")
  rows <- docx_rows(s)
  head_i <- which(vapply(
    rows,
    function(r) {
      r$header && identical(r$cells[1:3], c("Field", "No", "Variable"))
    },
    logical(1)
  ))
  expect_length(head_i, 1L)
  body <- Filter(function(r) r$table == rows[[head_i]]$table && !r$header, rows)
  numbered <- Filter(
    function(r) nzchar(r$cells[[2]]) || nzchar(r$cells[[3]]),
    body
  )
  no <- vapply(numbered, function(r) r$cells[[2]], character(1))
  var <- vapply(numbered, function(r) r$cells[[3]], character(1))
  expect_identical(
    no,
    c(
      "1",
      "1",
      "2",
      "2",
      "3",
      "3.1",
      "3.2",
      "3.3",
      "3.4",
      "3",
      "4.1",
      "4.2",
      "4.3",
      "5",
      "5.1",
      "5.2",
      "5.3",
      "5.4",
      "5.5",
      DASH,
      DASH,
      DASH,
      "6",
      DASH
    )
  )
  expect_identical(
    var[c(1:10, 20:24)],
    c(
      "Q01Single",
      "Q01Single[_Cother]",
      "Q02Singlecomment",
      "Q02Singlecomment[_Ccomment]",
      "",
      "Q03Multiple[1]",
      "Q03Multiple[2]",
      "Q03Multiple[3]",
      "Q03Multiple[4]",
      "Q03Multiple[other]",
      "Q10Display",
      "Q06Text",
      "Q07Shorttext",
      "Q08Numeric",
      "Q09Date"
    )
  )
  expect_true(all(startsWith(var[15:19], "Q05Ranking[")))

  # The mention closes every language cell of the hidden questions' rows,
  # and no other row.
  with_mention <- vapply(
    body,
    function(r) {
      any(grepl(HIDDEN_EN, r$cells, fixed = TRUE))
    },
    logical(1)
  )
  hidden_rows <- body[with_mention]
  expect_identical(
    vapply(hidden_rows, function(r) r$cells[[3]], character(1)),
    c("Q07Shorttext", "Q09Date")
  )
  for (r in hidden_rows) {
    lang_cells <- utils::tail(r$cells, 2L)
    expect_true(all(endsWith(lang_cells, HIDDEN_EN)))
  }

  index <- rows_under(rows, c("Variable", "No"), 1L, 2L)
  expect_identical(
    unname(index[c(
      "Q03Multiple[1]",
      "Q03Multiple[4]",
      "Q03Multiple[other]",
      "Q09Date"
    )]),
    c("3.1", "3.4", "3", DASH)
  )
  expect_identical(
    unname(index[startsWith(names(index), "Q05Ranking[")]),
    paste0("5.", 1:5)
  )
})

test_that("show_groups, languages and the chrome language move no number", {
  testthat::skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  s <- render_summary(
    numbering_lss(),
    template = "cards",
    chrome_lang = "fr",
    languages = "fr",
    show_groups = FALSE
  )
  rows <- docx_rows(s)
  cards <- card_numbers(rows, no = "N\u00B0")
  expect_identical(cards$meta, cards_expected)
  expect_true(all(cards$footer == "Question masqu\u00E9e aux r\u00E9pondants"))
  cover <- cover_fields(rows)
  expect_identical(
    unname(cover[c(
      "Questions",
      "Affichage",
      "Calcul",
      "Questions masqu\u00E9es"
    )]),
    c(
      "6",
      "1 (sans num\u00E9ro)",
      "2 (sans num\u00E9ro)",
      "1 (sans num\u00E9ro)"
    )
  )
})

test_that("cards number dual-scale and two-dimensional cells in order", {
  testthat::skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  model <- lss_model(demo_lss(), languages = "en")
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings("en")
  state <- lss_render_state(model)
  qs <- unlist(lapply(model$groups, function(g) g$questions), recursive = FALSE)
  codes <- vapply(qs, function(q) q$code, character(1))
  doc <- officer::read_docx()
  for (code in c("trustinstitutions", "adaptability")) {
    doc <- lss_render_question_block(
      doc,
      qs[[match(code, codes)]],
      "en",
      th,
      show_help = FALSE,
      show_attrs = character(0),
      show_technical_attrs = FALSE,
      audit_idx = NULL,
      state = state
    )
  }
  vars <- vapply(state$index_entries, function(e) e$code, character(1))
  nos <- vapply(state$index_entries, function(e) e$no, character(1))
  for (code in c("trustinstitutions", "adaptability")) {
    no <- nos[startsWith(vars, paste0(code, "["))]
    expect_identical(
      no,
      paste0(state$numbers$no[state$numbers$code == code], ".", seq_along(no)),
      info = code
    )
  }
})

# ---- No column width ---------------------------------------------------------

test_that("lss_no_width follows the digit and period metrics", {
  expect_equal(lss_no_width("28.1", 10, 4), 0.35)
  expect_equal(max(0.35, lss_no_width("28.1", 10, 4)), 0.35)
  expect_equal(lss_no_width("28.10", 10, 4), 0.42)
  expect_equal(lss_no_width("101.12", 10, 4), 0.50)
  expect_equal(lss_no_width("28.1", 12, 4), 0.40)
  expect_equal(lss_no_width(c("9", "28.10", NA, ""), 10, 4), 0.42)
  expect_equal(lss_no_width(DASH, 10, 4), lss_no_width("7", 10, 4))
  expect_identical(lss_no_width(character(0), 10, 4), 0)
})

test_that("the widest cards label counts sub-items as the cards do", {
  sq <- function(n, scale = "0", prefix = "R") {
    lapply(seq_len(n), function(i) {
      list(code = paste0(prefix, i), scale_id = scale)
    })
  }
  q <- function(qid, type, ...) {
    c(list(qid = qid, code = paste0("q", qid), type = type), list(...))
  }
  ans <- function(n) lapply(seq_len(n), function(i) list(aid = i, code = i))
  arr12 <- q("a", "F", subquestions = sq(12), answers = ans(3))
  dual <- q(
    "d",
    "1",
    subquestions = sq(3),
    answers = ans(4),
    scales = list("0" = ans(2), "1" = ans(2))
  )
  two_d <- q("t", ":", subquestions = c(sq(3), sq(4, "1", "C")))
  mc15 <- q("m", "M", subquestions = sq(15))
  rank12 <- q("r", "R", answers = ans(12))
  hidden12 <- q(
    "h",
    "F",
    subquestions = sq(12),
    answers = ans(3),
    attributes = data.frame(
      qid = "h",
      attribute = "hidden",
      value = "1",
      language = "",
      stringsAsFactors = FALSE
    )
  )
  widest <- function(...) {
    m <- list(groups = list(list(questions = list(...))))
    lss_cards_widest_label(m, lss_item_numbers(m))
  }
  expect_identical(widest(arr12), "1.12")
  expect_identical(widest(dual), "1.6")
  expect_identical(widest(two_d), "1.12")
  expect_identical(widest(mc15), "1")
  expect_identical(widest(rank12), "1")
  expect_identical(widest(hidden12), DASH)
  expect_identical(widest(rank12, mc15, dual, hidden12, arr12), "4.12")
  nine <- lapply(1:9, function(i) q(paste0("n", i), "N"))
  expect_identical(do.call(widest, c(nine, list(dual, two_d))), "11.12")
  expect_identical(
    vapply(
      list(arr12, dual, two_d, mc15, rank12),
      lss_cards_subitem_count,
      integer(1)
    ),
    c(12L, 6L, 12L, 0L, 0L)
  )
  expect_identical(widest(), "")
})

# Nine numeric questions, then a 12-row array numbered 10.1 to 10.12.
wide_lss <- function() {
  hit <- lss_fixture_cache[["wide"]]
  if (!is.null(hit)) {
    return(hit)
  }
  qs <- lapply(1:9, function(i) {
    list(code = paste0("n", i), kind = "numeric", text = paste("Number", i))
  })
  qs[[10]] <- list(
    code = "grid",
    kind = "array",
    text = "Rate each row.",
    rows = lapply(1:12, function(i) list(text = paste("Row", i))),
    columns = list(list(text = "Low"), list(text = "High"))
  )
  spec <- lss_spec(
    title = "Wide numbers",
    languages = "en",
    groups = list(list(title = "All", questions = qs))
  )
  file <- tempfile(fileext = ".lss")
  on.exit(unlink(file), add = TRUE)
  suppressMessages(write_lss(spec, file))
  lss <- read_lss(file)
  lss_fixture_cache[["wide"]] <- lss
  lss
}

# Grid column widths (twips) of every table of a rendered document.
render_grids <- function(lss, ...) {
  out <- tempfile(fileext = ".docx")
  dir <- tempfile()
  on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
  suppressMessages(render_documentation(lss, out, ...))
  utils::unzip(out, files = "word/document.xml", exdir = dir)
  xml <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  lapply(xml2::xml_find_all(xml, "//w:tblGrid", ns), function(g) {
    as.numeric(xml2::xml_attr(xml2::xml_find_all(g, "w:gridCol", ns), "w:w", ns))
  })
}

# The meta bands of the cards: five columns, Variable at 2.30 in.
meta_grids <- function(grids) {
  Filter(function(g) length(g) == 5L && g[[2]] == round(2.30 * 1440), grids)
}

test_that("the No column widens for the widest label, in both templates", {
  testthat::skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- wide_lss()

  meta <- meta_grids(render_grids(lss, template = "cards"))
  expect_true(length(meta) >= 12L)
  no <- vapply(meta, function(g) g[[1]], numeric(1))
  expect_length(unique(no), 1L)
  expect_equal(no[[1]], round(lss_no_width("10.12", 10, 4) * 1440))
  expect_gt(no[[1]], 0.35 * 1440)
  for (g in meta) {
    expect_lte(abs(sum(g) - lss_content_width_in("A4-portrait") * 1440), 5)
  }

  grids <- render_grids(lss, template = "table")
  big <- Filter(function(g) length(g) == 8L, grids)
  expect_length(big, 1L)
  big <- big[[1]]
  expect_equal(big[[2]], round(lss_no_width("10.12", 8, 6) * 1440))
  expect_gt(big[[2]], 0.30 * 1440)
  expect_lte(abs(sum(big) - lss_content_width_in("A4-landscape") * 1440), 8)
})

test_that("the demo survey keeps the minimum No widths", {
  testthat::skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  meta <- meta_grids(render_grids(demo_lss(), template = "cards"))
  expect_true(length(meta) > 0L)
  expect_true(all(vapply(meta, function(g) g[[1]], numeric(1)) == 504))
  grids <- render_grids(demo_lss(), template = "table")
  big <- Filter(function(g) length(g) == 11L, grids)
  expect_length(big, 1L)
  expect_identical(big[[1]][[2]], 432)
})

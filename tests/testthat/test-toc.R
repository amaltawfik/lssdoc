# The table of contents of render_documentation(): the question numbers after
# each group, and no internal link that leads nowhere.

toc_w <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

# Render quietly and return the body XML of the document. officer writes an
# internal link as a relationship whose target is "#<bookmark>": the target
# is copied onto the link as w:anchor, the attribute Word itself uses, so the
# checks below read one attribute.
toc_render <- function(lss, ...) {
  out <- tempfile(fileext = ".docx")
  dir <- tempfile()
  on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
  suppressMessages(suppressWarnings(
    render_documentation(lss, out, chrome_lang = "en", ...)
  ))
  utils::unzip(out, exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  rels <- xml2::xml_ns_strip(xml2::read_xml(
    file.path(dir, "word", "_rels", "document.xml.rels")
  ))
  rel <- xml2::xml_find_all(rels, "//Relationship")
  target <- stats::setNames(xml2::xml_attr(rel, "Target"),
                            xml2::xml_attr(rel, "Id"))
  for (h in xml2::xml_find_all(doc, "//w:hyperlink[@r:id]", toc_w_r)) {
    t <- target[[xml2::xml_attr(h, "r:id", toc_w_r)]]
    if (startsWith(t, "#")) {
      xml2::xml_set_attr(h, "w:anchor", substring(t, 2), ns = toc_w)
    }
  }
  doc
}

toc_w_r <- c(
  toc_w,
  r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
)

# One row per group entry of the table of contents: its anchor, the text of
# the link and the text that follows the link in the same paragraph.
toc_groups <- function(doc) {
  links <- xml2::xml_find_all(
    doc, "//w:hyperlink[starts-with(@w:anchor, 'lssdoc_group_')]", toc_w
  )
  data.frame(
    anchor = xml2::xml_attr(links, "w:anchor", toc_w),
    name = xml2::xml_text(links),
    range = vapply(links, function(l) {
      paste(xml2::xml_text(xml2::xml_find_all(
        xml2::xml_parent(l), "w:r/w:t", toc_w
      )), collapse = "")
    }, ""),
    stringsAsFactors = FALSE
  )
}

# The small survey with its last question turned into a text display: the
# second group then holds no numbered question.
toc_small_lss <- function() {
  lss <- small_lss(c("en", "fr"))
  last <- lss_model(lss)$groups[[2]]$questions
  qids <- vapply(last, function(q) as.character(q$qid), "")
  lss$questions$type[lss$questions$qid %in% qids] <- "X"
  lss
}

test_that("the TOC gives the question numbers after each group, out of the link", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- demo_lss()
  for (tpl in c("cards", "table")) {
    doc <- toc_render(lss, template = tpl)
    g <- toc_groups(doc)
    expect_identical(nrow(g), 6L, info = tpl)
    expect_identical(g$anchor[1], "lssdoc_group_1", info = tpl)
    expect_identical(g$range[1], "questions 1 to 10", info = tpl)
    expect_false(any(grepl("question", g$name, fixed = TRUE)), info = tpl)
    # The range is a muted run after a tab, not part of the hyperlink.
    p <- xml2::xml_parent(xml2::xml_find_first(
      doc, "//w:hyperlink[@w:anchor='lssdoc_group_1']", toc_w
    ))
    expect_false(inherits(xml2::xml_find_first(p, "w:r/w:tab", toc_w),
                          "xml_missing"))
    expect_length(xml2::xml_find_all(p, "w:hyperlink//w:tab", toc_w), 0L)
  }
  # A group without a numbered question carries no range; one with a
  # single question says "question".
  doc <- toc_render(toc_small_lss(), template = "cards")
  g <- toc_groups(doc)
  expect_identical(g$range, c("questions 1 to 2", ""))
  p <- xml2::xml_parent(xml2::xml_find_first(
    doc, "//w:hyperlink[@w:anchor='lssdoc_group_2']", toc_w
  ))
  expect_length(xml2::xml_find_all(p, "w:r", toc_w), 0L)
})

test_that("every internal link points to one bookmark, in every layout", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- small_lss(c("en", "fr"))
  for (tpl in c("cards", "table")) {
    for (sg in c(TRUE, FALSE)) {
      info <- sprintf("template = %s, show_groups = %s", tpl, sg)
      doc <- toc_render(lss, template = tpl, show_groups = sg)
      anchors <- xml2::xml_attr(
        xml2::xml_find_all(doc, "//w:hyperlink[@w:anchor]", toc_w),
        "w:anchor", toc_w
      )
      marks <- xml2::xml_attr(
        xml2::xml_find_all(doc, "//w:bookmarkStart", toc_w), "w:name", toc_w
      )
      expect_gt(length(anchors), 0L)
      expect_identical(setdiff(anchors, marks), character(0), info = info)
      expect_identical(anyDuplicated(marks), 0L, info = info)
      ids <- xml2::xml_attr(
        xml2::xml_find_all(doc, "//w:bookmarkStart", toc_w), "w:id", toc_w
      )
      expect_identical(anyDuplicated(ids), 0L, info = info)
      expect_identical(any(grepl("^lssdoc_group_", anchors)), sg, info = info)
    }
  }
})

test_that("the table template bookmarks each group on its own row", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- demo_lss()
  doc <- toc_render(lss, template = "table")
  marks <- xml2::xml_find_all(
    doc, "//w:tbl//w:bookmarkStart[starts-with(@w:name, 'lssdoc_group_')]",
    toc_w
  )
  expect_length(marks, 6L)
  names <- toc_groups(doc)$name
  for (i in seq_along(marks)) {
    expect_identical(xml2::xml_attr(marks[[i]], "w:name", toc_w),
                     sprintf("lssdoc_group_%d", i))
    row <- xml2::xml_find_first(marks[[i]], "ancestor::w:tr[1]", toc_w)
    # The row prints the name as written, with no number in front.
    expect_true(startsWith(xml2::xml_text(row), paste0("Group", names[i])))
    # Start and end in the first paragraph of the row.
    p <- xml2::xml_find_first(row, ".//w:p", toc_w)
    id <- xml2::xml_attr(marks[[i]], "w:id", toc_w)
    expect_identical(xml2::xml_parent(marks[[i]]), p)
    expect_false(inherits(xml2::xml_find_first(
      p, sprintf("w:bookmarkEnd[@w:id='%s']", id), toc_w
    ), "xml_missing"))
  }
  # No bookmark in the header row.
  expect_length(xml2::xml_find_all(
    doc, "//w:tr[w:trPr/w:tblHeader]//w:bookmarkStart", toc_w
  ), 0L)
})

test_that("show_groups = FALSE lists no group in the table of contents", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- small_lss(c("en", "fr"))
  for (tpl in c("cards", "table")) {
    doc <- toc_render(lss, template = tpl, show_groups = FALSE)
    toc <- xml2::xml_find_all(doc, "//w:p[w:hyperlink]", toc_w)
    text <- vapply(toc, xml2::xml_text, "")
    expect_true("Questionnaire" %in% text, info = tpl)
    model <- lss_model(lss)
    gnames <- vapply(model$groups, function(g) g$names[["en"]], "")
    expect_false(any(text %in% gnames), info = tpl)
    expect_identical(nrow(toc_groups(doc)), 0L, info = tpl)
  }
})

test_that("lss_render_toc() computes the numbers when none are given", {
  skip_if_not_installed("officer")
  lss <- small_lss(c("en", "fr"))
  model <- lss_model(lss)
  theme <- lss_render_theme()
  theme$chrome <- lss_chrome_strings("en")
  body <- officer::docx_body_xml(
    lss_render_toc(officer::read_docx(), model, theme)
  )
  runs <- xml2::xml_text(xml2::xml_find_all(
    body, "//w:p[w:r/w:tab]/w:r[last()]/w:t", toc_w
  ))
  expect_identical(runs, c("questions 1 to 2", "question 3"))
  # A group without a name in the first language takes its name in the
  # next one, and is named after its id when no language has one.
  nameless <- model
  nameless$groups[[1]]$names[[model$languages[1]]] <- ""
  body <- officer::docx_body_xml(
    lss_render_toc(officer::read_docx(), nameless, theme)
  )
  links <- xml2::xml_text(xml2::xml_find_all(body, "//w:hyperlink", toc_w))
  expect_true(model$groups[[1]]$names[[model$languages[2]]] %in% links)
  nameless$groups[[1]]$names[[model$languages[2]]] <- ""
  body <- officer::docx_body_xml(
    lss_render_toc(officer::read_docx(), nameless, theme)
  )
  links <- xml2::xml_text(xml2::xml_find_all(body, "//w:hyperlink", toc_w))
  expect_true(paste0("Group ", model$groups[[1]]$gid) %in% links)
  # Without the groups, the questionnaire entry alone.
  body <- officer::docx_body_xml(
    lss_render_toc(officer::read_docx(), model, theme, show_groups = FALSE)
  )
  expect_length(xml2::xml_find_all(body, "//w:p[w:hyperlink]", toc_w), 1L)
})

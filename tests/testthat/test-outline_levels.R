# Outline levels on the section and group headings, which Word's
# navigation pane is built from.

w_ns <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")

outline_document_xml <- function(lss, ...) {
  out <- tempfile(fileext = ".docx")
  dir <- tempfile()
  on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
  suppressMessages(render_documentation(lss, out, ...))
  utils::unzip(out, files = "word/document.xml", exdir = dir)
  xml2::read_xml(file.path(dir, "word", "document.xml"))
}

# The w:val of the outline level of every heading carrying a bookmark that
# starts with `prefix`, named by the rest of the bookmark name. A heading
# is a paragraph of the body. The table layout also puts a bookmark on each
# of its group rows, for the table of contents: those sit inside a table
# and are not read here.
outline_levels <- function(x, prefix) {
  bm <- xml2::xml_find_all(
    x,
    sprintf(
      "//w:p[not(ancestor::w:tbl)]/w:bookmarkStart[starts-with(@w:name, '%s')]",
      prefix
    ),
    w_ns
  )
  lvl <- xml2::xml_find_first(xml2::xml_parent(bm), "w:pPr/w:outlineLvl", w_ns)
  stats::setNames(
    xml2::xml_attr(lvl, "w:val", w_ns),
    sub(prefix, "", xml2::xml_attr(bm, "w:name", w_ns), fixed = TRUE)
  )
}

expect_outline <- function(x, sections, n_groups) {
  sec <- outline_levels(x, "lssdoc_section_")
  grp <- outline_levels(x, "lssdoc_group_")
  expect_identical(names(sec), sections)
  expect_identical(unname(sec), rep("0", length(sections)))
  expect_identical(names(grp), as.character(seq_len(n_groups)))
  expect_identical(unname(grp), rep("1", n_groups))
  expect_length(
    xml2::xml_find_all(x, "//w:outlineLvl", w_ns), length(sections) + n_groups
  )
  expect_length(xml2::xml_find_all(x, "//w:tbl//w:outlineLvl", w_ns), 0L)
  expect_length(
    xml2::xml_find_all(
      x,
      paste0("//w:pPr/w:outlineLvl[preceding-sibling::w:rPr or ",
             "preceding-sibling::w:sectPr or preceding-sibling::w:pPrChange]"),
      w_ns
    ),
    0L
  )
}

test_that("cards: sections are at outline level 1 and groups at level 2", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- small_lss()
  lss$surveys$showsurveypolicynotice <- "2"
  lss$survey_language_settings$surveyls_policy_notice <- "Data protection."
  x <- outline_document_xml(lss)
  expect_outline(
    x, c("toc", "consent", "questionnaire", "quotas", "index"),
    n_groups = length(lss_model(lss)$groups)
  )
})

test_that("cards with show_groups = FALSE: only the sections get a level", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  x <- outline_document_xml(flawed_lss(), show_groups = FALSE)
  expect_outline(
    x, c("toc", "audit", "questionnaire", "index"), n_groups = 0L
  )
})

test_that("table: group rows stay in the table, only the sections get a level", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  x <- outline_document_xml(small_lss(), template = "table")
  expect_outline(
    x, c("toc", "questionnaire", "quotas", "index"), n_groups = 0L
  )
})

test_that("lss_set_outline_level() keeps the CT_PPr order and one level", {
  skip_on_cran()
  skip_if_not_installed("officer")
  pPr_children <- function(doc) {
    p <- officer::docx_current_block_xml(doc)
    xml2::xml_name(xml2::xml_children(xml2::xml_find_first(p, "w:pPr", w_ns)))
  }
  doc <- officer::body_add_fpar(
    officer::read_docx(), officer::fpar(officer::ftext("Heading"))
  )
  ppr <- xml2::xml_find_first(
    officer::docx_current_block_xml(doc), "w:pPr", w_ns
  )
  xml2::xml_add_child(ppr, xml2::as_xml_document(
    sprintf("<w:rPr xmlns:w=\"%s\"><w:b/></w:rPr>", w_ns[["w"]])
  ))
  doc <- lss_set_outline_level(doc, 1L)
  doc <- lss_set_outline_level(doc, 2L)
  kids <- pPr_children(doc)
  expect_identical(utils::tail(kids, 2L), c("outlineLvl", "rPr"))
  expect_identical(sum(kids == "outlineLvl"), 1L)
  expect_identical(
    xml2::xml_attr(xml2::xml_find_first(ppr, "w:outlineLvl", w_ns),
                   "w:val", w_ns),
    "1"
  )

  doc <- lss_set_outline_level(officer::body_end_section_continuous(doc), 1L)
  expect_identical(pPr_children(doc), c("outlineLvl", "sectPr"))

  doc <- officer::body_add_par(doc, "No properties")
  xml2::xml_remove(xml2::xml_find_first(
    officer::docx_current_block_xml(doc), "w:pPr", w_ns
  ))
  doc <- lss_set_outline_level(doc, 1L)
  expect_identical(pPr_children(doc), "outlineLvl")

  doc <- officer::body_add_table(doc, data.frame(a = 1))
  n <- length(xml2::xml_find_all(officer::docx_body_xml(doc), "//w:outlineLvl", w_ns))
  doc <- lss_set_outline_level(doc, 1L)
  expect_length(
    xml2::xml_find_all(officer::docx_body_xml(doc), "//w:outlineLvl", w_ns), n
  )
})

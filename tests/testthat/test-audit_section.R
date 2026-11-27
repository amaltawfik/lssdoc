# The audit in render_documentation(): with `show_audit = TRUE`, the findings
# of audit_lss() are printed in one section near the top of the document,
# the same in both layouts. Nothing of the audit is printed on the cards of
# the questions: the heading of a card holds its number and its variable.

# `audit_demo.lss` rendered in one layout with the audit section and the
# card headings, read back with parity_read_docx(). Cached for the file.
as_render <- function(template) {
  key <- paste("audit_section", template)
  hit <- lss_fixture_cache[[key]]
  if (!is.null(hit)) return(hit)
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(suppressWarnings(render_documentation(
    flawed_lss(), out, template = template, chrome_lang = "en",
    show_audit = TRUE, show_item_heading = TRUE
  )))
  res <- parity_read_docx(out)
  lss_fixture_cache[[key]] <- res
  res
}

# The rows of the table of the audit section, its header first.
as_audit_rows <- function(doc) {
  ch <- lss_chrome_strings("en")
  head <- c(ch$audit_col_severity, ch$audit_col_location,
            ch$audit_col_language, ch$audit_col_message)
  hit <- Filter(function(t) identical(t[[1]], head), doc$tables)
  expect_length(hit, 1L)
  hit[[1]]
}

test_that("the audit section has one row per finding, the same in both layouts", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  f <- audit_lss(flawed_lss())$findings
  lang <- ifelse(is.na(f$language), "", f$language)
  expected <- lapply(seq_len(nrow(f)), function(i) {
    c(f$severity[i], f$location[i], lang[i], f$message[i])
  })
  cards <- as_audit_rows(as_render("cards"))
  table <- as_audit_rows(as_render("table"))
  expect_gt(length(expected), 0L)
  expect_identical(cards[-1L], expected)
  expect_identical(table, cards)
})

test_that("the heading of a card prints its number and variable, no audit note", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  doc <- as_render("cards")
  paras <- xml2::xml_find_all(doc$xml, "/w:document/w:body/w:p", parity_w)
  txt <- vapply(paras, parity_node_text, "")
  # The cards of the questions that carry a finding of audit_lss()
  # (`age` twice: its code is duplicated).
  headings <- c("1. age", "2. satisf", "3. rating", "4. income", "6. age",
                "7. blank_q")
  color <- sub("^#", "", lss_render_theme()$color_text)
  for (h in headings) {
    hit <- which(txt == h)
    expect_length(hit, 1L)
    if (length(hit) == 1L) {
      runs <- xml2::xml_find_all(paras[[hit]], ".//w:r/w:rPr/w:color",
                                 parity_w)
      expect_identical(unique(xml2::xml_attr(runs, "val")), color, info = h)
    }
    # No other paragraph starts with the heading and adds to it.
    expect_false(any(startsWith(txt, paste0(h, " "))), info = h)
  }
  expect_false(any(grepl("audit finding", doc$content, fixed = TRUE)))
})

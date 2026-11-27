# The Field column of the table layout never breaks a label inside a word:
# it is 0.62 in wide, wider when the widest word of a label of this table
# needs it, the extra width taken from the language columns (the rule of the
# No and the Filter columns). The measure depends on the fonts of the
# machine: a test that pins a number uses the estimate (measure = FALSE),
# and the exact 0.62 in of the English demo is expected only where Calibri
# is installed.

tfw_theme <- function(lang = "en") {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings(lang)
  th
}

# Is Calibri, the body font, installed on this machine?
tfw_has_calibri <- function() {
  requireNamespace("gdtools", quietly = TRUE) &&
    isTRUE(gdtools::font_family_exists("Calibri"))
}

# The grid column widths (twips) of the dense table of a rendered document.
tfw_grid <- function(doc) {
  tbls <- xml2::xml_find_all(doc$xml, "//w:tbl[w:tblGrid[count(w:gridCol) >= 8]]",
                             parity_w)
  expect_length(tbls, 1L)
  as.numeric(xml2::xml_attr(
    xml2::xml_find_all(tbls[[1]], "w:tblGrid/w:gridCol", parity_w), "w"
  ))
}

# The width a label needs on one line: bold, body font, at `size`, with the
# padding of its row and 1 pt of air, in inches.
tfw_need <- function(label, size, padding = 6) {
  th <- tfw_theme()
  (lss_q_text_width(label, th$font_body, size, bold = TRUE) + padding + 1) / 72
}

test_that("lss_filter_label_width() measures words, in the weight given", {
  w <- function(x, ...) {
    lss_filter_label_width(x, "Calibri", 8, 6, measure = FALSE, ...)
  }
  # A label may break between its words: the widest word counts.
  expect_identical(w("Welcome text"), w("Welcome"))
  expect_identical(w("Texte d'accueil"), w("d'accueil"))
  # A no-break space joins its words.
  expect_gt(w("Sous-question\u00A0:"), w("question"))
  # The weight reaches lss_q_text_width(): 72 pt in bold, 36 pt otherwise,
  # + 6 pt of padding + 1 pt, rounded up to 0.01 in.
  testthat::local_mocked_bindings(
    lss_q_text_width = function(x, font, size, bold = FALSE, code = FALSE,
                                measure = NULL) {
      if (isTRUE(bold)) 72 else 36
    }
  )
  expect_equal(lss_filter_label_width("Commentaire", "Calibri", 8, 6,
                                      bold = TRUE), 1.10)
  expect_equal(lss_filter_label_width("Commentaire", "Calibri", 8, 6), 0.60)
})

test_that("lss_table_field_width(): 0.62 in, wider for a long label", {
  th <- tfw_theme()
  fw <- function(labels, kinds, size = 8) {
    lss_table_field_width(labels, kinds, th, size, measure = FALSE)
  }
  lw <- function(label, size, padding) {
    lss_filter_label_width(label, th$font_body, size, padding, bold = TRUE,
                           measure = FALSE)
  }
  fit <- c("Question", "Comment", "Prefix", "Group", "")
  expect_identical(fw(fit, c("leaf", "leaf", "attr", "group", "value")), 0.62)
  long <- fw(c("Question", "Commentaire"), c("leaf", "leaf"))
  expect_gt(long, 0.62)
  expect_identical(long, lw("Commentaire", 8, 6))
  # The rows of the group, the description, the welcome and the end text
  # have 4 pt of padding on each side, the others 3.
  expect_identical(fw("Commentaire", "description"), lw("Commentaire", 8, 8))
  # At a smaller size the label needs less.
  expect_lt(fw("Commentaire", "leaf", size = 7), long)
})

test_that("table: a French Field label stays on one line", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- parity_lss()
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(render_documentation(
    lss, out, languages = c("en", "fr"), template = "table",
    chrome_lang = "fr", show_audit = FALSE
  ))
  fr <- parity_read_docx(out)
  g <- tfw_grid(fr)
  grid <- parity_table_grid(fr)
  expect_true("Commentaire" %in% grid[, "Field"])
  # Two languages: the labels are at theme$size_meta.
  size <- tfw_theme()$size_meta
  need <- max(vapply(setdiff(unique(grid[, "Field"]), ""), function(f) {
    max(vapply(strsplit(f, " ", fixed = TRUE)[[1]], tfw_need, 0, size = size))
  }, 0))
  expect_gt(need, 0.62)
  expect_gte(g[[1]], floor(need * 1440))
  expect_gte(g[[1]], floor(tfw_need("Commentaire", size) * 1440))
  # The extra width of the Field column comes from the language columns,
  # shared between them: the table keeps the content width of its page
  # (A4 landscape, the default of the table layout).
  expect_lte(abs(sum(g) - lss_content_width_in("A4-landscape") * 1440), 8)
  expect_length(g, 9L)
  expect_identical(g[[8]], g[[9]])
})

test_that("table: the English demo keeps a Field column of 0.62 in", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(render_documentation(
    demo_lss(), out, template = "table", chrome_lang = "en",
    page_format = "A4-landscape"
  ))
  g <- tfw_grid(parity_read_docx(out))
  expect_length(g, 11L)
  expect_gte(g[[1]], round(0.62 * 1440))
  # With Calibri every English label fits in the minimum; another font
  # (the substitute of a machine without Calibri) may need more.
  if (tfw_has_calibri()) {
    expect_equal(g[[1]], round(0.62 * 1440))
  }
})

test_that("render_documentation rejects objects that are not lss", {
  expect_error(
    render_documentation(list(), tempfile(fileext = ".docx")),
    class = "lssdoc_bad_input"
  )
})

test_that("render_documentation validates its output argument", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  expect_error(render_documentation(lss, 123), class = "lssdoc_bad_output")
  expect_error(render_documentation(lss, c("a", "b")), class = "lssdoc_bad_output")
})

test_that("render_documentation produces a non-empty .docx for hesav (2 langs)", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)

  res <- render_documentation(lss_cached(path), out, chrome_lang = "en")
  expect_identical(res, out)
  expect_true(file.exists(out))
  expect_true(file.size(out) > 10000)

  # Inspect the produced document. Section and group headings are the
  # package's own styled paragraphs (not Word's "heading 1" style, which
  # we dropped for a template-independent, uniform look), so we verify
  # structure by a section heading's text plus the item expansion via the
  # number of table cells (each item has its own meta table; arrays add a
  # shared answer scale on top).
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Variable index", txt, fixed = TRUE))
  expect_true(sum(s$content_type == "table cell") > 200L)
})

test_that("render_documentation with show_audit = FALSE drops the audit section", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)

  render_documentation(lss_cached(path), out, show_audit = FALSE)
  s <- officer::docx_summary(officer::read_docx(out))
  # No "Audit findings" heading is present in the document.
  txt <- s$text[!is.na(s$text)]
  expect_false(any(grepl("Audit findings", txt)))
})

test_that("the cover page carries the Survey ID and LimeSurvey last-save fields", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  # Force English chrome so the test pins exact English labels; the
  # default chrome_lang would follow the survey's primary content
  # language (e.g. German for this fixture) and translate the labels.
  render_documentation(lss_cached(path), out, chrome_lang = "en")
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Survey ID", txt))
  expect_true(grepl("LimeSurvey last save", txt))
  expect_true(grepl("354814", txt))  # demo_survey survey id
})

test_that("render_documentation validates the logo argument", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  expect_error(
    render_documentation(lss, tempfile(fileext = ".docx"), logo = c("a", "b")),
    class = "lssdoc_bad_logo"
  )
  expect_error(
    render_documentation(lss, tempfile(fileext = ".docx"), logo = "nope.png"),
    class = "lssdoc_logo_not_found"
  )
  bad <- tempfile(fileext = ".gif")
  writeLines("x", bad)
  on.exit(unlink(bad), add = TRUE)
  expect_error(
    render_documentation(lss, tempfile(fileext = ".docx"), logo = bad),
    class = "lssdoc_bad_logo_format"
  )
})

test_that("a valid logo is embedded in the rendered document", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  logo <- tempfile(fileext = ".png")
  on.exit(unlink(logo), add = TRUE)
  grDevices::png(logo, width = 200, height = 100, bg = "white")
  graphics::par(mar = c(0, 0, 0, 0)); graphics::plot.new()
  graphics::text(0.5, 0.5, "L", cex = 6)
  grDevices::dev.off()

  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  size_no_logo <- file.size({
    f <- tempfile(fileext = ".docx")
    render_documentation(lss_cached(path), f)
    f
  })
  render_documentation(lss_cached(path), out, logo = logo)
  expect_true(file.size(out) > size_no_logo)
})

test_that("render_documentation errors with the right class when officer/flextable are missing", {
  skip("requires the suggested packages to be intentionally unavailable")
})

test_that("lss_normalize_authors handles its three accepted input shapes", {
  expect_null(lss_normalize_authors(NULL))

  out_unnamed <- lss_normalize_authors(c("Amal Tawfik", "John Doe"))
  expect_length(out_unnamed, 2L)
  expect_identical(out_unnamed[[1L]]$name, "Amal Tawfik")
  expect_identical(out_unnamed[[1L]]$affiliation, "")
  expect_identical(out_unnamed[[1L]]$orcid, "")

  out_named <- lss_normalize_authors(c("Amal Tawfik" = "HES-SO", "John Doe" = ""))
  expect_identical(out_named[[1L]]$affiliation, "HES-SO")
  expect_identical(out_named[[2L]]$name, "John Doe")
  expect_identical(out_named[[2L]]$affiliation, "")

  out_list <- lss_normalize_authors(list(
    list(name = "Amal Tawfik", affiliation = "HES-SO", orcid = "0009-0006-2422-1555"),
    list(name = "John Doe")
  ))
  expect_identical(out_list[[1L]]$orcid, "0009-0006-2422-1555")
  expect_identical(out_list[[2L]]$affiliation, "")
  expect_identical(out_list[[2L]]$orcid, "")
})

test_that("lss_normalize_authors rejects malformed input", {
  expect_error(lss_normalize_authors(123), class = "lssdoc_bad_authors")
  expect_error(lss_normalize_authors(list(list(affiliation = "x"))),
               class = "lssdoc_bad_authors")
  expect_error(lss_normalize_authors(list(list(name = ""))),
               class = "lssdoc_bad_authors")
})

test_that("lss_normalize_description coerces empty/whitespace input to NULL", {
  expect_null(lss_normalize_description(NULL))
  expect_null(lss_normalize_description(""))
  expect_null(lss_normalize_description("   "))
  expect_identical(lss_normalize_description("hello"), "hello")
  expect_error(lss_normalize_description(123), class = "lssdoc_bad_description")
  expect_error(lss_normalize_description(c("a", "b")), class = "lssdoc_bad_description")
})

test_that("authors and description appear on the cover when supplied", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_documentation(
    lss_cached(path), out, languages = c("fr", "de"),
    authors = list(
      list(name = "Amal Tawfik", affiliation = "HES-SO Valais",
           orcid = "0009-0006-2422-1555")
    ),
    description = "Validated as part of project XYZ. See https://example.org/x."
  )
  s <- officer::docx_summary(officer::read_docx(out))
  txt <- paste(s$text[!is.na(s$text)], collapse = " | ")
  expect_true(grepl("Amal Tawfik", txt))
  expect_true(grepl("HES-SO Valais", txt))
  expect_true(grepl("0009-0006-2422-1555", txt))
  expect_true(grepl("project XYZ", txt))
  expect_true(grepl("example.org", txt))
})

test_that("lss_validate_colors accepts NULL, normalizes hex maps, rejects garbage", {
  expect_null(lss_validate_colors(NULL))

  out <- lss_validate_colors(list(primary = "#5C9F1A", accent = "#7FA82E"))
  expect_named(out, c("color_primary", "color_accent"))
  expect_identical(out$color_primary, "#5C9F1A")
  expect_identical(out$color_accent, "#7FA82E")

  # Three-digit hex form is accepted too.
  expect_identical(
    lss_validate_colors(list(primary = "#abc"))$color_primary,
    "#abc"
  )

  expect_error(
    lss_validate_colors(list(primay = "#000")),  # typo on purpose
    class = "lssdoc_bad_colors"
  )
  expect_error(
    lss_validate_colors(list(primary = "red")),
    class = "lssdoc_bad_colors"
  )
  expect_error(
    lss_validate_colors(list(primary = NA_character_)),
    class = "lssdoc_bad_colors"
  )
  expect_error(
    lss_validate_colors(list("#5C9F1A")),  # no names
    class = "lssdoc_bad_colors"
  )
})

test_that("render_documentation accepts a colors override and runs end-to-end", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_documentation(
    lss_cached(path), out,
    languages = c("fr", "de"),
    colors = list(primary = "#5C9F1A", accent = "#7FA82E")
  )
  expect_true(file.exists(out))
  expect_true(file.size(out) > 10000L)
})

# ---- the .pdf route ----------------------------------------------------------

test_that("lss_detect_output_format reads both extensions and refuses a third", {
  expect_identical(lss_detect_output_format("a.docx"), "docx")
  expect_identical(lss_detect_output_format("a.PDF"), "pdf")
  expect_error(lss_detect_output_format("a.odt"), class = "lssdoc_bad_output_ext")
})

test_that("the .pdf route renders a temporary .docx and converts it", {
  # The conversion itself needs LibreOffice, which CI does not have: what is
  # under test here is the ROUTE -- a temporary .docx, handed to the
  # converter with the caller's own output path.
  seen <- NULL
  testthat::local_mocked_bindings(
    .render_documentation_docx = function(lss, output, ...) {
      file.create(output)
      invisible(output)
    },
    .docx_to_pdf = function(docx, pdf) {
      seen <<- c(docx = docx, pdf = pdf)
      invisible(pdf)
    }
  )
  out <- tempfile(fileext = ".pdf")
  .render_documentation_pdf(structure(list(), class = "lss"), out)
  expect_match(seen[["docx"]], "[.]docx$")
  expect_identical(seen[["pdf"]], out)
})

test_that("the formula of a computed question: a row of the card and of the table", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  expect_true("equation" %in% eval(formals(render_documentation)$show_attrs))
  expect_true("equation" %in% eval(formals(.render_documentation_docx)$show_attrs))
  lss <- demo_lss()
  lss$questions <- lss$questions[lss$questions$title == "imc", , drop = FALSE]
  formula <- "round((weight / ((height/100)*(height/100))), 1)"
  w <- c(w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main")
  render_xml <- function(lss, ...) {
    out <- tempfile(fileext = ".docx")
    dir <- tempfile()
    on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
    suppressMessages(suppressWarnings(
      render_documentation(lss, out, languages = c("en", "fr"),
                           show_audit = FALSE, ...)
    ))
    utils::unzip(out, exdir = dir)
    xml2::read_xml(file.path(dir, "word", "document.xml"))
  }
  # The rows of the tables: each its w:tr and the texts of its cells.
  rows_of <- function(doc) {
    lapply(xml2::xml_find_all(doc, "//w:tbl/w:tr", w), function(tr) {
      tcs <- xml2::xml_find_all(tr, "w:tc", w)
      list(tr = tr, tcs = tcs, cells = vapply(tcs, xml2::xml_text, ""))
    })
  }
  with_formula <- function(rows) {
    Filter(function(r) formula %in% r$cells, rows)
  }
  span <- function(tc) {
    v <- xml2::xml_attr(xml2::xml_find_first(tc, "w:tcPr/w:gridSpan", w),
                        "val")
    if (is.na(v)) 1L else as.integer(v)
  }

  # Cards, chrome in French: the row carries the label of the chrome, and
  # the formula without its braces.
  doc <- render_xml(lss, chrome_lang = "fr")
  hit <- with_formula(rows_of(doc))
  expect_length(hit, 1L)
  expect_true("\u00C9quation" %in% hit[[1]]$cells)
  expect_false(grepl("{round(", xml2::xml_text(doc), fixed = TRUE))

  # Table: one row right under the Question row of imc, the label in the
  # Field column, the formula once across the two language columns in the
  # code font, every other cell empty.
  rows <- rows_of(render_xml(lss, template = "table", chrome_lang = "en"))
  k <- which(vapply(rows, function(r) formula %in% r$cells, logical(1)))
  expect_length(k, 1L)
  expect_true("imc" %in% rows[[k - 1L]]$cells)
  cells <- rows[[k]]$cells
  expect_identical(cells[[1]], "Equation")
  j <- which(cells == formula)
  expect_length(j, 1L)
  expect_identical(span(rows[[k]]$tcs[[j]]), 2L)
  expect_identical(length(cells), 8L)
  expect_true(all(cells[-c(1L, j)] == ""))
  font <- xml2::xml_attr(
    xml2::xml_find_first(rows[[k]]$tcs[[j]], ".//w:r/w:rPr/w:rFonts", w),
    "ascii"
  )
  expect_identical(font, "Consolas")

  # No row with show_attrs = character(0), in the cards or the table.
  for (tpl in c("cards", "table")) {
    none <- rows_of(render_xml(lss, template = tpl, chrome_lang = "en",
                               show_attrs = character(0)))
    expect_length(with_formula(none), 0L)
  }
  # No row for a computed question without the attribute.
  bare <- lss
  qa <- bare$question_attributes
  bare$question_attributes <- qa[qa$attribute != "equation", , drop = FALSE]
  rows <- rows_of(render_xml(bare, template = "table", chrome_lang = "en"))
  expect_false(any(vapply(rows, function(r) "Equation" %in% r$cells,
                          logical(1))))
})

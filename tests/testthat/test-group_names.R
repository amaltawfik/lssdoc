# The name of a group is printed as its author wrote it, with no number added
# and none removed: in the heading of the cards and the names block under it,
# in the group row of the table, and in the table of contents.

gn_w <- c(
  w = "http://schemas.openxmlformats.org/wordprocessingml/2006/main",
  r = "http://schemas.openxmlformats.org/officeDocument/2006/relationships"
)

# small_lss() in English and French, the names of its two groups set to
# `names` (a list of two c(en = , fr = ) vectors), the descriptions to
# `descriptions` when given.
gn_lss <- function(names = list(c(en = "", fr = "1. \u00C9tudes"),
                                c(en = "1 Studies", fr = "1 \u00C9tudes")),
                   descriptions = NULL, relevance = NULL) {
  lss <- small_lss(c("en", "fr"))
  gid <- lss$groups$gid[order(as.integer(lss$groups$group_order))]
  for (i in seq_along(gid)) {
    for (lg in c("en", "fr")) {
      hit <- lss$group_l10ns$gid == gid[i] & lss$group_l10ns$language == lg
      lss$group_l10ns$group_name[hit] <- names[[i]][[lg]]
      if (!is.null(descriptions)) {
        lss$group_l10ns$description[hit] <- descriptions[[i]][[lg]]
      }
    }
  }
  if (!is.null(relevance)) {
    lss$groups$grelevance[lss$groups$gid == gid[1]] <- relevance
  }
  lss
}

# Render and read back the body of the document, each internal link given a
# w:anchor (officer writes it as a relationship to "#<bookmark>").
gn_render <- function(lss, template, languages = c("en", "fr"), ...) {
  out <- tempfile(fileext = ".docx")
  dir <- tempfile()
  on.exit(unlink(c(out, dir), recursive = TRUE), add = TRUE)
  suppressMessages(suppressWarnings(render_documentation(
    lss, out, languages = languages, template = template,
    chrome_lang = "en", show_audit = FALSE, ...
  )))
  utils::unzip(out, exdir = dir)
  doc <- xml2::read_xml(file.path(dir, "word", "document.xml"))
  rels <- xml2::xml_ns_strip(xml2::read_xml(
    file.path(dir, "word", "_rels", "document.xml.rels")
  ))
  rel <- xml2::xml_find_all(rels, "//Relationship")
  target <- stats::setNames(xml2::xml_attr(rel, "Target"),
                            xml2::xml_attr(rel, "Id"))
  for (h in xml2::xml_find_all(doc, "//w:hyperlink[@r:id]", gn_w)) {
    t <- target[[xml2::xml_attr(h, "r:id", gn_w)]]
    if (startsWith(t, "#")) {
      xml2::xml_set_attr(h, "w:anchor", substring(t, 2), ns = gn_w)
    }
  }
  doc
}

# The text of each group entry of the table of contents, named by anchor.
gn_toc <- function(doc) {
  links <- xml2::xml_find_all(
    doc, "//w:hyperlink[starts-with(@w:anchor, 'lssdoc_group_')]", gn_w
  )
  stats::setNames(xml2::xml_text(links),
                  xml2::xml_attr(links, "w:anchor", gn_w))
}

# The body element (paragraph or table) that holds the bookmark `name`, and
# the `n` body elements after it.
gn_after <- function(doc, name, n = 2L) {
  kids <- xml2::xml_find_all(doc, "/w:document/w:body/*", gn_w)
  b <- xml2::xml_find_first(
    doc, sprintf("//w:bookmarkStart[@w:name='%s']", name), gn_w
  )
  top <- xml2::xml_find_first(b, "ancestor::*[parent::w:body][1]", gn_w)
  i <- which(vapply(kids, function(k) identical(k, top), logical(1)))
  list(at = kids[[i]], after = kids[seq(i + 1L, length.out = n)])
}

# The cells of the rows of a table: one character vector per w:tr.
gn_cells <- function(tbl) {
  lapply(xml2::xml_find_all(tbl, "w:tr", gn_w), function(tr) {
    vapply(xml2::xml_find_all(tr, "w:tc", gn_w), parity_node_text, "")
  })
}

# The properties of the first run of a node: bold, italic, colour, size in
# points.
gn_run <- function(node) {
  rpr <- xml2::xml_find_first(node, "descendant-or-self::w:r/w:rPr", gn_w)
  on <- function(tag) {
    x <- xml2::xml_find_first(rpr, tag, gn_w)
    !inherits(x, "xml_missing") &&
      xml2::xml_attr(x, "val") %in% c(NA, "true", "1", "on")
  }
  sz <- xml2::xml_attr(xml2::xml_find_first(rpr, "w:sz", gn_w), "val")
  list(
    bold = on("w:b"), italic = on("w:i"),
    color = toupper(xml2::xml_attr(xml2::xml_find_first(rpr, "w:color", gn_w),
                                   "val")),
    size = as.numeric(sz) / 2
  )
}

gn_hex <- function(x) toupper(sub("^#", "", x))

# The group row of the table that carries the bookmark of group `k`.
gn_table_row <- function(doc, k) {
  b <- xml2::xml_find_first(
    doc, sprintf("//w:tbl//w:bookmarkStart[@w:name='lssdoc_group_%d']", k),
    gn_w
  )
  xml2::xml_find_first(b, "ancestor::w:tr[1]", gn_w)
}

test_that("lss_group_title() takes the first name among the languages of the document", {
  g <- list(gid = "7", names = list(en = "Health", fr = "Sant\u00E9", de = ""))
  expect_identical(lss_group_title(g, c("en", "fr")), "Health")
  expect_identical(lss_group_title(g, c("fr", "en")), "Sant\u00E9")
  # An empty or blank name is passed over.
  expect_identical(lss_group_title(g, c("de", "fr")), "Sant\u00E9")
  g$names$de <- "  "
  expect_identical(lss_group_title(g, c("de", "en")), "Health")
  # Only the languages of the document count.
  expect_identical(lss_group_title(g, "de"), "Group 7")
  g$names <- list(en = "", fr = NA_character_)
  expect_identical(lss_group_title(g, c("en", "fr")), "Group 7")
  expect_identical(lss_group_title(list(gid = 3L, names = list()), "en"),
                   "Group 3")
  # As written: a number typed in front stays.
  g$names <- list(en = "1. Studies", fr = "1) \u00C9tudes")
  expect_identical(lss_group_title(g, c("en", "fr")), "1. Studies")
  expect_identical(lss_group_title(g, "fr"), "1) \u00C9tudes")
})

test_that("the group row of the table holds the names as written, no number", {
  theme <- ta_theme()
  rows_of <- function(names) {
    g <- list(gid = "7", names = names, descriptions = list(),
              relevance = "1", questions = list())
    lss_table_template_rows_for_group(
      g, TA_LANGS, theme, show_help = TRUE, state = lss_render_state(NULL)
    )
  }
  r <- rows_of(list(en = "1. Studies", fr = "1 \u00C9tudes"))[[1]]
  expect_identical(r$kind, "group")
  expect_identical(r$name_by_lang,
                   list(en = "1. Studies", fr = "1 \u00C9tudes"))
  expect_identical(r$group_index, 1L)
  # A language without a name, when another has one: NA, printed as the
  # empty marker.
  r <- rows_of(list(en = "", fr = "\u00C9tudes"))[[1]]
  expect_identical(r$name_by_lang, list(en = NA_character_, fr = "\u00C9tudes"))
  r <- rows_of(list(en = "Studies", fr = "  "))[[1]]
  expect_identical(r$name_by_lang, list(en = "Studies", fr = NA_character_))
  # No language has a name: "Group <gid>" in each column.
  r <- rows_of(list(en = "", fr = NA_character_))[[1]]
  expect_identical(r$name_by_lang, list(en = "Group 7", fr = "Group 7"))
})

test_that("table: the group row prints each name as written, the empty marker for a missing one", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  theme <- lss_render_theme()
  doc <- gn_render(gn_lss(), "table")
  row1 <- gn_table_row(doc, 1L)
  tcs <- xml2::xml_find_all(row1, "w:tc", gn_w)
  cells <- vapply(tcs, parity_node_text, "")
  expect_identical(cells[1], "Group")
  expect_identical(cells[8:9], c(theme$empty_marker, "1. \u00C9tudes"))
  expect_identical(gn_run(tcs[[8]])$color, gn_hex(theme$color_muted))
  expect_identical(gn_run(tcs[[9]])$color, gn_hex(theme$color_primary))
  expect_true(gn_run(tcs[[9]])$bold)
  cells <- vapply(xml2::xml_find_all(gn_table_row(doc, 2L), "w:tc", gn_w),
                  parity_node_text, "")
  expect_identical(cells[8:9], c("1 Studies", "1 \u00C9tudes"))
  # No language has a name.
  lss <- gn_lss(list(c(en = "A", fr = "B"), c(en = "", fr = "")))
  gid <- lss$groups$gid[order(as.integer(lss$groups$group_order))][2]
  doc <- gn_render(lss, "table")
  cells <- vapply(xml2::xml_find_all(gn_table_row(doc, 2L), "w:tc", gn_w),
                  parity_node_text, "")
  expect_identical(cells[8:9], rep(paste0("Group ", gid), 2L))
})

test_that("cards: the names block sits under the heading and the filter, above the description", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  theme <- lss_render_theme()
  desc <- list(c(en = "<p>About <strong>studies</strong>.</p>", fr = "<p>Sur les \u00E9tudes.</p>"),
               c(en = "", fr = ""))
  doc <- gn_render(gn_lss(descriptions = desc, relevance = "age > 17"),
                   "cards")
  # Group 1: heading, filter paragraph, then one table of two rows.
  a <- gn_after(doc, "lssdoc_group_1", 2L)
  expect_identical(parity_node_text(a$at), "1. \u00C9tudes")
  expect_identical(xml2::xml_name(a$after[[1]]), "p")
  expect_match(parity_node_text(a$after[[1]]), "^Filter: ")
  expect_identical(xml2::xml_name(a$after[[2]]), "tbl")
  rows <- gn_cells(a$after[[2]])
  expect_length(rows, 2L)
  expect_identical(rows[[1]], c(theme$empty_marker, "1. \u00C9tudes"))
  expect_identical(rows[[2]], c("About studies.", "Sur les \u00E9tudes."))
  # No header row.
  expect_length(xml2::xml_find_all(a$after[[2]], "w:tr[w:trPr/w:tblHeader]",
                                   gn_w), 0L)
  trs <- xml2::xml_find_all(a$after[[2]], "w:tr", gn_w)
  tcs <- xml2::xml_find_all(trs[[1]], "w:tc", gn_w)
  name_run <- gn_run(tcs[[2]])
  expect_identical(name_run,
                   list(bold = TRUE, italic = FALSE,
                        color = gn_hex(theme$color_text),
                        size = as.numeric(theme$size_subq)))
  marker_run <- gn_run(tcs[[1]])
  expect_identical(marker_run$color, gn_hex(theme$color_muted))
  expect_identical(marker_run$size, as.numeric(theme$size_subq))
  # The description row keeps its italic and the formatting of the author,
  # at the same size as the names row.
  desc_tc <- xml2::xml_find_all(trs[[2]], "w:tc", gn_w)[[1]]
  expect_true(gn_run(desc_tc)$italic)
  expect_identical(gn_run(desc_tc)$size, as.numeric(theme$size_subq))
  runs <- xml2::xml_find_all(desc_tc, ".//w:r[w:t]", gn_w)
  bold <- vapply(runs, function(r) gn_run(r)$bold, logical(1))
  expect_identical(xml2::xml_text(runs[bold]), "studies")
  # Group 2, without a description: the names row alone, right under the
  # heading.
  a <- gn_after(doc, "lssdoc_group_2", 1L)
  expect_identical(parity_node_text(a$at), "1 Studies")
  expect_identical(xml2::xml_name(a$after[[1]]), "tbl")
  expect_identical(gn_cells(a$after[[1]]), list(c("1 Studies", "1 \u00C9tudes")))
})

test_that("cards: show_group_description = FALSE keeps the names row alone", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- gn_lss()
  doc <- gn_render(lss, "cards", show_group_description = FALSE)
  a <- gn_after(doc, "lssdoc_group_2", 1L)
  expect_identical(gn_cells(a$after[[1]]), list(c("1 Studies", "1 \u00C9tudes")))
})

test_that("cards: one language or show_groups = FALSE adds no names block", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- gn_lss(list(c(en = "Studies", fr = "\u00C9tudes"),
                     c(en = "Work", fr = "Travail")))
  desc <- lss$group_l10ns$description[lss$group_l10ns$language == "en"]
  # One language: the description block of one row and one cell, as before.
  doc <- gn_render(lss, "cards", languages = "en")
  a <- gn_after(doc, "lssdoc_group_1", 1L)
  expect_identical(parity_node_text(a$at), "Studies")
  rows <- gn_cells(a$after[[1]])
  expect_length(rows, 1L)
  expect_length(rows[[1]], 1L)
  expect_true(gn_run(a$after[[1]])$italic)
  # Without the headings: the description alone, no names anywhere.
  doc <- gn_render(lss, "cards", show_groups = FALSE)
  tbls <- xml2::xml_find_all(doc, "/w:document/w:body/w:tbl", gn_w)
  all_rows <- unlist(lapply(tbls, gn_cells), recursive = FALSE)
  for (nm in list(c("Studies", "\u00C9tudes"), c("Work", "Travail"))) {
    expect_false(any(vapply(all_rows, identical, logical(1), nm)))
  }
  hit <- Filter(function(t) {
    any(grepl(substr(lss_html_to_text(desc[1]), 1, 20),
              unlist(gn_cells(t)), fixed = TRUE))
  }, tbls)
  expect_length(hit, 1L)
  expect_length(gn_cells(hit[[1]]), 1L)
})

test_that("an entry of the table of contents reads like the heading it points to", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  theme <- lss_render_theme()
  for (lss in list(gn_lss(), gn_lss(list(c(en = "A", fr = "B"),
                                         c(en = "", fr = ""))))) {
    # Cards: the text of the heading.
    doc <- gn_render(lss, "cards")
    toc <- gn_toc(doc)
    expect_length(toc, 2L)
    for (k in seq_along(toc)) {
      a <- gn_after(doc, names(toc)[k], 0L)
      expect_identical(unname(toc[k]), parity_node_text(a$at))
    }
    # Table: the first name of the row that is not the empty marker.
    doc <- gn_render(lss, "table")
    toc_t <- gn_toc(doc)
    expect_identical(toc_t, toc)
    for (k in seq_along(toc_t)) {
      cells <- vapply(xml2::xml_find_all(gn_table_row(doc, k), "w:tc", gn_w),
                      parity_node_text, "")[8:9]
      expect_identical(unname(toc_t[k]),
                       cells[cells != theme$empty_marker][1])
    }
  }
})

test_that("a number typed in front of a group name is printed whole in the three places", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- gn_lss(list(c(en = "1. Studies", fr = "1. \u00C9tudes"),
                     c(en = "1 Work", fr = "2) Travail")))
  cards <- gn_render(lss, "cards")
  table <- gn_render(lss, "table")
  expect_identical(unname(gn_toc(cards)), c("1. Studies", "1 Work"))
  expect_identical(unname(gn_toc(table)), c("1. Studies", "1 Work"))
  expect_identical(parity_node_text(gn_after(cards, "lssdoc_group_1", 0L)$at),
                   "1. Studies")
  expect_identical(gn_cells(gn_after(cards, "lssdoc_group_2", 1L)$after[[1]])[[1]],
                   c("1 Work", "2) Travail"))
  cells <- function(k) {
    vapply(xml2::xml_find_all(gn_table_row(table, k), "w:tc", gn_w),
           parity_node_text, "")[8:9]
  }
  expect_identical(cells(1L), c("1. Studies", "1. \u00C9tudes"))
  expect_identical(cells(2L), c("1 Work", "2) Travail"))
})

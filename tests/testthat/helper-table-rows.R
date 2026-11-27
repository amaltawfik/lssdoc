# Helpers of the tests that build the rows of the table layout
# (lss_table_template_rows_for_group()) from small questions, and read the
# rendered table back: shared by test-table_attributes.R and
# test-comment_variables.R.

ta_theme <- function() {
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings("en")
  th
}
TA_LANGS <- c("en", "fr")
TA_ATTRS <- c("prefix", "suffix", "other_replace_text", "validation",
              "equation")

ta_txt <- function(en, fr = en) {
  list(en = list(question = en, help = NULL),
       fr = list(question = fr, help = NULL))
}
ta_attrs <- function(...) {
  a <- list(...)
  if (length(a) == 0L) {
    return(data.frame(attribute = character(0), value = character(0),
                      language = character(0), stringsAsFactors = FALSE))
  }
  do.call(rbind, lapply(a, function(x) {
    data.frame(attribute = x[[1]], value = x[[2]], language = x[[3]],
               stringsAsFactors = FALSE)
  }))
}
ta_answer <- function(code, scale = "0") {
  list(code = code, aid = paste0("a", code), scale_id = scale,
       labels = list(en = paste("Label", code), fr = paste("Libell\u00E9", code)))
}
ta_sq <- function(code, scale = "0", attributes = NULL) {
  list(code = code, texts = ta_txt(paste("Row", code), paste("Ligne", code)),
       relevance = "1", scale_id = scale, attributes = attributes)
}
ta_q <- function(type, code, attributes = ta_attrs(), answers = list(),
                 subquestions = list(), scales = NULL, other = "N") {
  list(qid = code, code = code, type = type, mandatory = "N",
       relevance = "1", group_relevance = "1", other = other,
       texts = ta_txt(paste("Stem", code), paste("Libell\u00E9", code)),
       answers = answers, scales = scales, subquestions = subquestions,
       attributes = attributes)
}
# The rows of the table for the questions `qs`, without the group row.
ta_rows <- function(qs, show_attrs = TA_ATTRS) {
  state <- lss_render_state(NULL)
  state$show_attrs <- show_attrs
  lss_table_template_rows_for_group(
    list(questions = qs), TA_LANGS, ta_theme(), show_help = TRUE,
    state = state, show_groups = FALSE
  )
}
ta_kinds <- function(rows) vapply(rows, function(r) r$kind, "")
ta_attr <- function(rows) Filter(function(r) identical(r$kind, "attr"), rows)

# The body rows of the dense table, each its w:tr node and the texts of its
# cells (one per w:tc, a merged cell once).
ta_xml_rows <- function(doc) {
  tbls <- xml2::xml_find_all(doc$xml, "//w:tbl[w:tblGrid[count(w:gridCol) = 9]]",
                             parity_w)
  expect_length(tbls, 1L)
  lapply(xml2::xml_find_all(tbls[[1]], "w:tr", parity_w), function(tr) {
    tcs <- xml2::xml_find_all(tr, "w:tc", parity_w)
    list(tcs = tcs, cells = vapply(tcs, parity_node_text, ""))
  })
}
ta_span <- function(tc) {
  v <- xml2::xml_attr(xml2::xml_find_first(tc, "w:tcPr/w:gridSpan", parity_w),
                      "val")
  if (is.na(v)) 1L else as.integer(v)
}
# The run properties of the first run of a cell: italic (a w:i without a
# value, or with a true one), colour, font.
ta_run <- function(tc) {
  rpr <- xml2::xml_find_first(tc, ".//w:r/w:rPr", parity_w)
  i <- xml2::xml_find_first(rpr, "w:i", parity_w)
  list(
    italic = !inherits(i, "xml_missing") &&
      xml2::xml_attr(i, "val") %in% c(NA, "true", "1", "on"),
    color = xml2::xml_attr(xml2::xml_find_first(rpr, "w:color", parity_w), "val"),
    font = xml2::xml_attr(xml2::xml_find_first(rpr, "w:rFonts", parity_w), "ascii")
  )
}

ta_text_of <- function(rows) vapply(rows, function(r) {
  if (is.null(r$text)) "" else r$text
}, "")

# The texts and the italic flag of the chunks of a composed paragraph.
ta_chunks <- function(p) {
  d <- p[[1]]
  list(txt = d$txt, italic = d$italic, color = d$color)
}

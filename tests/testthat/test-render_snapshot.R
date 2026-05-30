# Content-snapshot tests for render_questionnaire().
#
# Each test renders a small docx and asserts that specific tokens are
# present (or absent) in the document text. These are not exact-byte
# snapshots -- they pin the *visible content* the reviewer should see,
# so a chrome string rename, a default flip or an alignment regression
# surfaces here rather than in a manual review of every preview.
#
# The helpers use officer::docx_summary() which extracts every text
# token from the .docx (paragraphs + table cells + headers + footers).

lss_render_text <- function(lss, ..., template = "cards") {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_questionnaire(lss, out, template = template, ...)
  s <- officer::docx_summary(officer::read_docx(out))
  paste(s$text[!is.na(s$text)], collapse = " | ")
}

# ---- Cover subtitle is the single-word localized noun --------------

test_that("the cover subtitle is the single localized noun in English", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "en")
  expect_true(grepl("\\bQuestionnaire\\b", txt))
  # The previous long subtitle must NOT be there.
  expect_false(grepl("LimeSurvey questionnaire review", txt))
})

test_that("the cover subtitle localizes to 'Fragebogen' in German chrome", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "de")
  expect_true(grepl("Fragebogen", txt))
})

# ---- Mandatory header: full word in cards, abbreviated in table ----

test_that("cards template uses the full 'Mandatory' header in English", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), template = "cards", chrome_lang = "en")
  expect_true(grepl("Mandatory", txt))
})

test_that("table template uses the abbreviated 'Mand.' header in English", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), template = "table", chrome_lang = "en")
  expect_true(grepl("Mand\\.", txt))
})

# ---- show_raw_filter default behaviour ------------------------------

test_that("show_raw_filter = FALSE (default) does not surface .NAOK in Filter cells", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "en")
  # The humanized form is the editorial default; the LimeSurvey
  # `.NAOK` token only appears when the user opts into the raw
  # expression. Cards header text doesn't naturally carry `.NAOK`,
  # so finding any occurrence here means the raw form leaked.
  expect_false(grepl("\\.NAOK", txt))
})

test_that("show_raw_filter = TRUE surfaces the raw LimeSurvey expression", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  # Find any question with a non-trivial relevance so the raw form
  # actually has content to show.
  has_rel <- !is.na(lss$questions$relevance) &
    nzchar(lss$questions$relevance) &
    lss$questions$relevance != "1"
  skip_if(!any(has_rel), "No non-trivial filters in the fixture")
  txt <- lss_render_text(lss, chrome_lang = "en", show_raw_filter = TRUE)
  expect_true(grepl("\\.NAOK", txt))
})

# ---- French chrome surfaces French labels --------------------------

test_that("chrome_lang = 'fr' surfaces French labels and not their EN counterparts", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "fr")
  expect_true(grepl("Filtre", txt))
  expect_true(grepl("Obligatoire", txt))
  # The EN-only label must NOT appear when chrome_lang is fr.
  expect_false(grepl("\\bMandatory\\b", txt))
})

# ---- Audit summary section --------------------------------------------

test_that("show_audit = TRUE places the audit findings heading near the top", {
  path <- system.file("extdata", "limesurvey_survey_751689.lss",
                      package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path), chrome_lang = "en", show_audit = TRUE)
  expect_true(grepl("Audit findings", txt))
})

# ---- Languages column header presence ------------------------------

test_that("requested languages each appear as a column label in cards", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  txt <- lss_render_text(read_lss(path),
                         languages = c("fr", "de"),
                         template = "cards", chrome_lang = "en")
  expect_true(grepl("Fran", txt))   # Francais (the localized name)
  expect_true(grepl("Deutsch", txt))
})

# ---- Variable codes appear in the variable index --------------------

test_that("every question code appears in the variable index", {
  path <- system.file("extdata", "hesav_2026.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- read_lss(path)
  txt <- lss_render_text(lss, chrome_lang = "en", show_index = TRUE)
  # Pick a few codes to spot-check (every question appearing would
  # be a stronger assertion but it would couple the test to the
  # fixture too tightly).
  codes <- lss$questions$title[1:min(5L, nrow(lss$questions))]
  for (code in codes) {
    expect_true(grepl(code, txt, fixed = TRUE),
                info = sprintf("expected code '%s' in the rendered text", code))
  }
})

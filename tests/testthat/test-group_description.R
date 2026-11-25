# show_group_description: the text LimeSurvey shows under a group title.

# small_lss() in English and French, every group given a description
# that names its group and language, so a test can look for it.
described_lss <- function() {
  lss <- small_lss(c("en", "fr"))
  gl <- lss$group_l10ns
  gl$description <- sprintf("Intro of group %s (%s)", gl$gid, gl$language)
  lss$group_l10ns <- gl
  lss
}

group_description_text <- function(lss, ...) {
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  suppressMessages(render_documentation(lss, out, ...))
  s <- officer::docx_summary(officer::read_docx(out))
  paste(s$text[!is.na(s$text)], collapse = " | ")
}

test_that("the fixture gives every group a description in both languages", {
  skip_on_cran()
  lss <- described_lss()
  expect_gte(length(unique(lss$group_l10ns$gid)), 2L)
  expect_setequal(lss$group_l10ns$language, c("en", "fr"))
})

for (template in c("cards", "table")) {
  test_that(sprintf("%s: the group description is printed by default", template), {
    skip_on_cran()
    skip_if_not_installed("officer")
    skip_if_not_installed("flextable")
    lss <- described_lss()
    by_default <- group_description_text(lss, template = template)
    explicit <- group_description_text(
      lss, template = template, show_group_description = TRUE
    )
    for (d in lss$group_l10ns$description) {
      expect_true(grepl(d, by_default, fixed = TRUE), info = d)
    }
    # TRUE is the default: the document is the same, generation date aside.
    strip_date <- function(x) gsub("[0-9]{4}-[0-9]{2}-[0-9]{2}[^|]*", "", x)
    expect_identical(strip_date(explicit), strip_date(by_default))
  })

  test_that(sprintf("%s: show_group_description = FALSE leaves it out", template), {
    skip_on_cran()
    skip_if_not_installed("officer")
    skip_if_not_installed("flextable")
    lss <- described_lss()
    txt <- group_description_text(
      lss, template = template, show_group_description = FALSE
    )
    expect_false(grepl("Intro of group", txt, fixed = TRUE))
    # The group headings themselves stay (in the first document language
    # at least: the cards banner carries that language only).
    gl <- lss$group_l10ns
    first <- lss_model(lss)$languages[1]
    for (g in gl$group_name[gl$language == first]) {
      expect_true(grepl(g, txt, fixed = TRUE), info = g)
    }
  })
}

test_that("with show_groups = FALSE, cards follow the flag and the table drops it", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- described_lss()
  # Cards without banners still print the description by default ...
  kept <- group_description_text(
    lss, template = "cards", show_groups = FALSE, show_group_description = TRUE
  )
  for (d in lss$group_l10ns$description) {
    expect_true(grepl(d, kept, fixed = TRUE), info = d)
  }
  # ... and FALSE removes it there too.
  dropped <- group_description_text(
    lss, template = "cards", show_groups = FALSE, show_group_description = FALSE
  )
  expect_false(grepl("Intro of group", dropped, fixed = TRUE))
  # The table description belongs to the group rows: gone with them.
  table <- lapply(c(TRUE, FALSE), function(flag) group_description_text(
    lss, template = "table", show_groups = FALSE,
    show_group_description = flag
  ))
  for (txt in table) {
    expect_false(grepl("Intro of group", txt, fixed = TRUE))
  }
})

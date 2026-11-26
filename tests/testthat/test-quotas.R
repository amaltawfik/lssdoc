# Quota parsing and the show_quotas back-matter section.

test_that("read_lss exposes the quota sections", {
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  lss <- lss_cached(path)
  expect_true(!is.null(lss$quotas) && nrow(lss$quotas) >= 1L)
  expect_true(!is.null(lss$quota_members))
  expect_true(!is.null(lss$quota_languagesettings))
})

test_that("show_quotas renders a quota section with resolved conditions", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  path <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(path))
  out <- tempfile(fileext = ".docx")
  on.exit(unlink(out), add = TRUE)
  render_documentation(out = out, input = path, chrome_lang = "en")
  txt <- paste(
    officer::docx_summary(officer::read_docx(out))$text, collapse = " | "
  )
  expect_true(grepl("Quotas", txt, fixed = TRUE))
  expect_true(grepl("fin2", txt, fixed = TRUE))
  expect_true(grepl("terminate survey", txt, fixed = TRUE))
  # The membership condition resolves the answer code to its label and
  # joins several members with the localized AND connector.
  expect_true(grepl("ST (Student)", txt, fixed = TRUE))
  expect_true(grepl(" AND ", txt, fixed = TRUE))
  # The localized "quota full" message is shown.
  expect_true(grepl("exceeded a quota", txt, fixed = TRUE))
})

test_that("show_quotas = FALSE and quota-less surveys produce no quota section", {
  skip_on_cran()
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  demo <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
  skip_if_not(file.exists(demo))

  out_off <- tempfile(fileext = ".docx")
  on.exit(unlink(out_off), add = TRUE)
  render_documentation(demo, out_off, chrome_lang = "en", show_quotas = FALSE)
  t_off <- paste(
    officer::docx_summary(officer::read_docx(out_off))$text, collapse = " | "
  )
  expect_false(grepl("exceeded a quota", t_off, fixed = TRUE))

  # A quota-less survey (quotas stripped in memory) renders no quota
  # full message even with the default show_quotas = TRUE.
  lss_noq <- lss_cached(demo)
  lss_noq$quotas <- lss_noq$quotas[0, , drop = FALSE]
  lss_noq$quota_members <- lss_noq$quota_members[0, , drop = FALSE]
  out_none <- tempfile(fileext = ".docx")
  on.exit(unlink(out_none), add = TRUE)
  render_documentation(lss_noq, out_none, chrome_lang = "en")
  t_none <- paste(
    officer::docx_summary(officer::read_docx(out_none))$text, collapse = " | "
  )
  expect_false(grepl("exceeded a quota", t_none, fixed = TRUE))
})

# A survey reduced to what lss_quota_condition() reads.
quota_lss <- function(qid, code) {
  list(
    quota_members = data.frame(quota_id = "1", qid = qid, code = code,
                               stringsAsFactors = FALSE),
    questions = data.frame(qid = c("10", "20"), title = c("gender", "role"),
                           stringsAsFactors = FALSE),
    answers = data.frame(qid = c("10", "10", "20"), code = c("F", "M", "ST"),
                         aid = c("a1", "a2", "a3"), stringsAsFactors = FALSE),
    answer_l10ns = data.frame(aid = c("a1", "a2", "a3"), language = "en",
                              answer = c("Female", "Male", ""),
                              stringsAsFactors = FALSE)
  )
}

test_that("the condition of a quota: questions joined by AND, answers as a set", {
  en <- list(chrome = lss_chrome_strings("en"))
  fr <- list(chrome = lss_chrome_strings("fr"))
  # One member.
  one <- quota_lss("10", "M")
  expect_identical(lss_quota_condition(one, "1", en), "gender = M")
  expect_identical(lss_quota_condition(one, "1", en, label_lang = "en"),
                   "gender = M (Male)")
  # Two questions, in the order of their first member.
  two <- quota_lss(c("20", "10"), c("ST", "F"))
  expect_identical(lss_quota_condition(two, "1", en),
                   "role = ST AND gender = F")
  expect_identical(lss_quota_condition(two, "1", fr, label_lang = "en"),
                   "role = ST ET gender = F (Female)")
  # Two answers of one question are alternatives: a set, as
  # lss_humanize_relevance() writes it.
  alt <- quota_lss(c("10", "20", "10"), c("F", "ST", "M"))
  expect_identical(lss_quota_condition(alt, "1", en),
                   "gender \u2208 {F, M} AND role = ST")
  expect_identical(
    lss_quota_condition(alt, "1", en, label_lang = "en"),
    "gender \u2208 {F (Female), M (Male)} AND role = ST"
  )
  expect_identical(
    sub(" AND role = ST", "", lss_quota_condition(alt, "1", en)),
    lss_humanize_relevance('gender.NAOK == "F" || gender.NAOK == "M"', en)
  )
  # A question or an answer that is not in the survey: its id, its code.
  odd <- quota_lss(c("99", "10"), c("X", "Z"))
  expect_identical(lss_quota_condition(odd, "1", en, label_lang = "en"),
                   "99 = X AND gender = Z")
  odd$answers <- NULL
  expect_identical(lss_quota_condition(odd, "1", en, label_lang = "en"),
                   "99 = X AND gender = Z")
  # No member, no member table; the English AND without a theme.
  expect_identical(lss_quota_condition(one, "2", en), "")
  expect_identical(lss_quota_condition(list(), "1", en), "")
  expect_identical(lss_quota_condition(two, "1"), "role = ST AND gender = F")
})

test_that("the quotas table of the documentation reads two answers as a set", {
  skip_if_not_installed("officer")
  skip_if_not_installed("flextable")
  lss <- lss_cached(system.file("extdata", "demo_survey.lss",
                                package = "lssdoc"))
  qm <- lss$quota_members
  extra <- qm[qm$quota_id == "52" & qm$code == "ST", , drop = FALSE]
  extra$code <- "EM"
  lss$quota_members <- rbind(qm, extra)
  th <- lss_render_theme()
  th$chrome <- lss_chrome_strings("en")
  doc <- lss_render_quotas(officer::read_docx(), lss, c("en", "fr"), th)
  txt <- officer::docx_summary(doc)$text
  cond <- txt[grepl("^respondentrole", txt)]
  expect_length(cond, 1L)
  expect_true(startsWith(
    cond, "respondentrole \u2208 {ST (Student), EM} AND education = 3 ("
  ))
  # One answer per question reads as before.
  expect_true("gender = M" %in% txt)
  # A quota without member: the empty marker.
  lss$quota_members <- qm[qm$quota_id != "53", , drop = FALSE]
  doc <- lss_render_quotas(officer::read_docx(), lss, "en", th)
  expect_true(th$empty_marker %in% officer::docx_summary(doc)$text)
})

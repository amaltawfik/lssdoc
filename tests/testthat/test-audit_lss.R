test_that("audit_lss rejects objects that are not lss", {
  expect_error(audit_lss(list()), class = "lssdoc_bad_lss")
})

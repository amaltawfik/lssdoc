# ==================================================
# 03_post_release.R
# Post-CRAN acceptance tasks for the lssdoc package
# ==================================================

library(usethis)
library(devtools)
library(sessioninfo)

run_cmd <- function(cmd, args = character()) {
  status <- system2(cmd, args = args)
  if (!identical(status, 0L)) {
    stop(sprintf("Command failed: %s %s", cmd, paste(args, collapse = " ")))
  }
  invisible(status)
}

# Commit only when something is staged: `git commit` exits non-zero on
# "nothing to commit", which would abort the whole script through run_cmd().
commit_if_changes <- function(msg) {
  if (identical(system2("git", c("diff", "--cached", "--quiet")), 0L)) {
    message("Nothing to commit: ", msg)
    return(invisible(FALSE))
  }
  run_cmd("git", c("commit", "-m", shQuote(msg)))
  invisible(TRUE)
}

# 01 GITHUB TAG & RELEASE -------
# use_github_release() reads CRAN-SUBMISSION (written by devtools::submit_cran())
# to tag exactly the submitted SHA, then deletes that file itself. Never remove
# the file before this step: the tag would land on the current HEAD instead.
usethis::use_github_release() # Create tag + publish GitHub release from NEWS.md

# 02 SAFETY NET: CRAN-SUBMISSION must be gone by now -------
if (file.exists("CRAN-SUBMISSION")) {
  file.remove("CRAN-SUBMISSION")
}

# 03 UPDATE README & DOCS -------
devtools::build_readme()
devtools::document()
source("dev/build_pkgdown_site.R") # Build site + clean internal pages

# `docs/` is git-ignored (the site is built and deployed to gh-pages by the
# pkgdown GitHub Action), so it must not be staged: `git add docs` would fail.
run_cmd("git", c("add", "-A", "--", "CRAN-SUBMISSION", "README.md", "man", "NAMESPACE"))
commit_if_changes("docs: refresh README and Rd for release")
run_cmd("git", c("push"))

# 04 BUMP TO DEVELOPMENT VERSION -------
usethis::use_dev_version()
run_cmd("git", c("add", "DESCRIPTION", "NEWS.md"))
run_cmd("git", c("commit", "-m", shQuote("chore: start next development version")))
run_cmd("git", c("push"))

# 05 SESSION INFO -------
sessioninfo::session_info()

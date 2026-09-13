## Submission

This is a feature release (0.2.0), about three months after 0.1.1.

* It adds an experimental authoring layer: `lss_spec()` builds a validated
  survey specification and `write_lss()` writes it as a LimeSurvey 6
  (DBVersion 700) `.lss` file. Both are flagged experimental with lifecycle
  badges, as their interface may evolve with LimeSurvey's file format.
* New dependency: \pkg{lifecycle} (Imports), for those badges.
* `read_lss()` now warns when a file comes from a newer LimeSurvey than
  the version the package targets.

## R CMD check results

<!-- Fill in from devtools::check() and win-builder before submitting. -->
(pending)

* If flagged, "LimeSurvey" (the survey software the package reads and
  writes) and "methodologists" (a correctly spelled English term) in the
  DESCRIPTION are intentional.

## Test environments

* Local: Windows 11, R 4.6.1
* GitHub Actions (r-lib/actions, R CMD check, `error-on = "warning"`):
  * macOS-latest (R release)
  * windows-latest (R release)
  * ubuntu-latest (R devel, release, oldrel-1)
<!-- Add win-builder (release, devel, oldrelease) and R-hub results after
     running dev/02_release_cran.R steps 04-05. -->

## Notes for the reviewer

* The rendering path (`render_questionnaire()`, `render_audit()`) relies
  on the suggested packages \pkg{officer} and \pkg{flextable}; every use
  is guarded with `requireNamespace()` and a classed, actionable error,
  and the parse, audit and authoring paths work without them. Those
  examples are wrapped in `\dontrun{}` because they write a Word file and
  the PDF variant additionally requires a local LibreOffice install.
* The new `write_lss()` example runs on check and writes only to
  `tempfile()`.
* The `.lss` output was validated against a real LimeSurvey instance
  (7.0.0-beta1): a generated 21-question survey covering every supported
  question kind imported without warnings and was re-exported with every
  question, attribute and display condition intact (DBVersion 700).
* All processing is local: the package makes no network calls and never
  uploads questionnaire content to a third-party service.

## Downstream dependencies

There are no reverse dependencies on CRAN.

## Resubmission

This is a resubmission. In response to the CRAN review:

* The software name 'LimeSurvey' is now single-quoted in the Title and
  Description fields of DESCRIPTION.

## R CMD check results

0 errors | 0 warnings | 1 note

* This is a new release, so there is a "New submission" NOTE.

The spell checker may still flag "LimeSurvey" (now single-quoted, the name
of the survey software the package reads) and "methodologists" (a correctly
spelled English term).

## Test environments

* Local: Windows 11, R 4.6.0
* GitHub Actions (r-lib/actions, R CMD check, `error-on = "warning"`):
  * macOS-latest (R release)
  * windows-latest (R release)
  * ubuntu-latest (R devel, release, oldrel-1)

## Notes for the reviewer

* The rendering path (`render_questionnaire()`, `render_audit()`) relies
  on the suggested packages \pkg{officer} and \pkg{flextable}; every use
  is guarded with `requireNamespace()` and a classed, actionable error,
  and the parse and audit paths work without them. The corresponding
  examples are wrapped in `\dontrun{}` because they write a Word file and
  the PDF variant additionally requires a local LibreOffice install.
* All processing is local: the package makes no network calls and never
  uploads questionnaire content to a third-party service.

## Downstream dependencies

There are no downstream dependencies (new package).

## Submission

This is a patch release (0.1.1) that fixes the test ERROR reported on
r-devel-linux-x86_64-fedora-gcc.

* On that platform's libxml2, passing non-XML input to `xml2::read_xml()`
  aborted the R process with an uncatchable C++ exception
  ("Start tag expected, '<' not found"), so the unit test exercising the
  invalid-XML path crashed. `read_lss()` now pre-validates that the file
  begins with an XML tag and fails cleanly with a classed
  `lssdoc_invalid_xml` error before reaching libxml2.

## R CMD check results

0 errors | 0 warnings | 1 note

* The spell checker may flag "LimeSurvey" (the survey software the package
  reads) and "methodologists" (a correctly spelled English term) in the
  DESCRIPTION.

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

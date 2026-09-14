## Submission

This release corrects the ERROR reported on
r-devel-linux-x86_64-fedora-gcc (CRAN e-mail of 2026-09-14, correction
requested by 2026-10-05), and adds a feature.

### The reported problem, and why no Internet resource is involved

The package makes **no network calls of any kind**: it reads a local
LimeSurvey `.lss` (XML) file and writes a Word or PDF document. Nothing
in it fetches a URL, and keeping questionnaire content local is a design
constraint, stated in the DESCRIPTION.

The failure came from a unit test of our own error handling. It feeds
deliberately malformed XML to `read_lss()` to check that the classed
`lssdoc_invalid_xml` error is raised. On that one toolchain, libxml2
raised a *fatal* parse error, which surfaced as an uncatchable C++
exception ("terminate called after throwing an instance of
'Rcpp::exception'") and terminated the R process instead of being
converted to an R condition, so `tryCatch()` never saw it. Every other
platform turned the same input into the expected R error.

### The fix

`read_lss()` no longer reaches the parser with input the parser can
refuse. It now validates in R, before calling `xml2::read_xml()`, that
the content is valid UTF-8 (transcoding a UTF-16 byte-order mark rather
than refusing it) and that it carries a complete `document` envelope.
Malformed, truncated or non-UTF-8 input therefore raises the classed
`lssdoc_invalid_xml` error identically on every platform, without the
parser being involved. The structure is validated again after parsing.

We did test `RECOVER`, `NOERROR` and `NOWARNING`: with xml2 1.6.0 they
do not stop `read_xml()` from signalling, so the fix deliberately does
not rest on them. They are kept, together with `NONET`, which forbids
libxml2 from resolving an external entity or DTD over the network — so
a crafted input file cannot make the package reach the Internet either.

### Title and Description

Both now say that the package also writes `.lss` files, which it did not
do when the current Title was written. The package name, the maintainer
and the scope are otherwise unchanged.

### Also in this release

An experimental Word authoring layer: a questionnaire can be written in a
Word form and turned into an importable LimeSurvey file
(`lss_template_docx()`, `write_form_docx()`, `read_form_docx()`,
`check_form_docx()`, `as_lss_spec()`), and `write_lss()` now writes all
declared languages. No new dependency: reading a form uses only
\pkg{xml2} and `utils::unzip()`; writing one uses the already suggested
\pkg{officer} and \pkg{flextable}, guarded as before.

## R CMD check results

0 errors | 0 warnings | 0 notes

Locally (`devtools::check()`).
<!-- Add the win-builder verdicts (release, devel, oldrelease) once received. -->

* If flagged, "LimeSurvey" (the survey software the package reads and
  writes) and "methodologists" (a correctly spelled English term) in the
  DESCRIPTION are intentional.

## Test environments

* Local: Windows 11, R 4.6.1
* GitHub Actions (r-lib/actions, R CMD check, `error-on = "warning"`):
  * macOS-latest (R release)
  * windows-latest (R release)
  * ubuntu-latest (R devel, release, oldrel-1)
<!-- Add the win-builder results (release, devel, oldrelease) once received. -->

## Notes for the reviewer

* The rendering path (`render_questionnaire()`, `render_audit()`) and the
  form writer rely on the suggested packages \pkg{officer} and
  \pkg{flextable}; every use is guarded with `requireNamespace()` and a
  classed, actionable error. Parsing, auditing, writing `.lss` files and
  reading a Word form all work without them. Those examples are wrapped
  in `\dontrun{}` because they write a Word file and the PDF variant
  additionally requires a local LibreOffice install.
* The `.lss` output is validated against a real LimeSurvey instance
  (7.0.0-beta1): a generated survey covering every supported question
  type imports without warnings and re-exports with every question,
  attribute and display condition intact (DBVersion 700).
* All processing is local: the package makes no network calls and never
  uploads questionnaire content to a third-party service.

## Downstream dependencies

There are no reverse dependencies on CRAN.

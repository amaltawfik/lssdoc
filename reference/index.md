# Package index

## Render

Render a LimeSurvey survey to a Word or PDF document. The output format
is inferred from the extension of `output` (`.docx` or `.pdf`). Each
function accepts either a path to a `.lss` file or a pre-parsed `lss`
object returned by
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md).

- [`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md)
  : Render a LimeSurvey questionnaire to a Word or PDF document
- [`render_audit()`](https://amaltawfik.github.io/lssdoc/reference/render_audit.md)
  : Render the audit as a focused Word or PDF document

## Read and audit

Parse a `.lss` file into a structured object, and audit a survey for
reviewable anomalies (missing translations, dangling references,
structural issues). Useful for inspecting a survey before rendering, or
for rendering several variants from a single parse.

- [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  :

  Read a LimeSurvey `.lss` file

- [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  : Audit a LimeSurvey survey for reviewable anomalies

## Author (experimental)

Build a survey specification programmatically and write it as an
importable LimeSurvey 6 `.lss` file. The specification is validated in
depth at construction time, because LimeSurvey imports silently and only
surfaces mistakes once respondents hit them. The written file
round-trips through
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
and
[`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md).

- [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  : Build and validate a survey specification

- [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
  :

  Write a survey specification to an importable `.lss` file

## Package overview

- [`lssdoc`](https://amaltawfik.github.io/lssdoc/reference/lssdoc-package.md)
  [`lssdoc-package`](https://amaltawfik.github.io/lssdoc/reference/lssdoc-package.md)
  : lssdoc: Render 'LimeSurvey' '.lss' Questionnaires as Word and PDF
  Documents

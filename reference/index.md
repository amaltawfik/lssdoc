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

## Package overview

- [`lssdoc`](https://amaltawfik.github.io/lssdoc/reference/lssdoc-package.md)
  [`lssdoc-package`](https://amaltawfik.github.io/lssdoc/reference/lssdoc-package.md)
  : lssdoc: Render Multilingual Questionnaires from LimeSurvey '.lss'
  Files

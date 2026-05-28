# Package index

## One-shot pipeline

Parse a `.lss` file and render the review document in a single call. The
most common entry points for everyday use.

- [`lss_to_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_to_docx.md)
  :

  Convert a `.lss` file to a Word review document

- [`lss_to_pdf()`](https://amaltawfik.github.io/lssdoc/reference/lss_to_pdf.md)
  :

  Convert a `.lss` file to a PDF review document

- [`lss_audit_to_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_audit_to_docx.md)
  :

  Convert a `.lss` file to a Word audit-only document

- [`lss_audit_to_pdf()`](https://amaltawfik.github.io/lssdoc/reference/lss_audit_to_pdf.md)
  :

  Convert a `.lss` file to a PDF audit-only document

## Parse

Read a LimeSurvey `.lss` (XML) export into a structured R object.

- [`parse_lss()`](https://amaltawfik.github.io/lssdoc/reference/parse_lss.md)
  :

  Parse a LimeSurvey `.lss` file

## Audit

Inspect the parsed survey for integrity issues a reviewer would
otherwise miss – missing translations, dangling references, malformed
relevance expressions, structural inconsistencies.

- [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  : Audit a parsed LimeSurvey structure for reviewable anomalies

## Render

Lower-level entry points if you want to control parsing and rendering
separately, or render the audit alone.

- [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md)
  : Render a parsed LimeSurvey structure to a Word document

- [`render_lss_audit_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_audit_docx.md)
  : Render the audit alone as a focused Word document

- [`lss_docx_to_pdf()`](https://amaltawfik.github.io/lssdoc/reference/lss_docx_to_pdf.md)
  :

  Convert a `.docx` to `.pdf` locally

## Package overview

- [`lssdoc`](https://amaltawfik.github.io/lssdoc/reference/lssdoc-package.md)
  [`lssdoc-package`](https://amaltawfik.github.io/lssdoc/reference/lssdoc-package.md)
  : lssdoc: Generate Word Review Documents from LimeSurvey '.lss' Files

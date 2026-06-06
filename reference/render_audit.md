# Render the audit as a focused Word or PDF document

Build a short, action-oriented document containing only the audit
findings: the same cover page as the full questionnaire document,
summary counts, then one table per severity (errors, warnings, notes)
listing every finding with its location and message. Use it for QA
follow-up or to share issues with a colleague without distributing the
full questionnaire.

## Usage

``` r
render_audit(
  input,
  output,
  languages = NULL,
  logo = NULL,
  logo_width = 1.5,
  logo_height = 0.75,
  font = NULL,
  font_code = NULL,
  colors = NULL,
  authors = NULL,
  description = NULL,
  chrome_lang = NULL
)
```

## Arguments

- input:

  Either a path to a `.lss` file (character string) or a pre-parsed
  `lss` object returned by
  [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md).
  Passing a path parses it on the fly; passing an `lss` object avoids
  re-parsing in a workflow that already inspected the audit.

- output:

  Character. Path to the file to create. The extension determines the
  output format: `.docx` writes a Word document directly, `.pdf` writes
  a Word document into a temporary location and converts it locally via
  LibreOffice (or Word, on Windows). Any other extension is rejected
  with `lssdoc_bad_output_ext`.

- languages:

  Character vector of language codes used on the cover page. `NULL`
  (default) keeps all languages of the survey in their declared order.

- logo:

  Optional path (character) to a PNG or JPEG image displayed at the top
  of the cover page. `NULL` (default) keeps the cover logo-free.

- logo_width, logo_height:

  Image dimensions in inches. Defaults `1.5` and `0.75`, tuned to a 2:1
  logo. Resize or pre-crop your image to fit a different aspect ratio.

- font:

  Optional body font name (character). `NULL` (default) keeps Calibri.
  See
  [`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md)
  for guidance on overrides.

- font_code:

  Optional monospace font (character) used for code-like content
  (variable codes, raw expressions). `NULL` (default) keeps Consolas.

- colors:

  Optional named list of hex color overrides for the editorial
  petrol-blue palette. `NULL` (default) keeps the package palette
  intact. Same shape and accepted names as in
  [`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md).

- authors, description:

  Optional cover-page credit block (`authors`) and free-form note
  (`description`). `NULL` (default) for both. Same shapes as in
  [`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md).

- chrome_lang:

  Language used for the document chrome (column headers, row labels,
  audit section). One of `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`. `NULL`
  (default) follows `languages[1]` when supported, otherwise falls back
  to `"en"`.

## Value

The `output` path, invisibly.

## See also

[`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
to inspect the same findings in the console;
[`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md)
for the full questionnaire document.

## Examples

``` r
if (FALSE) { # \dontrun{
# One-shot (path -> .docx)
render_audit(
  system.file("extdata", "demo_survey.lss",
              package = "lssdoc"),
  tempfile(fileext = ".docx")
)

# PDF output -- same call, just pass a .pdf path
render_audit("survey.lss", "qa.pdf")
} # }
```

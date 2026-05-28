# Render the audit alone as a focused Word document

Build a short, action-oriented `.docx` that contains only the audit
findings: the same cover page as the full review document, the summary
counts, then one table per severity (errors, warnings, notes) listing
every finding with its location and message. Use this for QA follow-up
or to share the issues with a colleague without distributing the full
questionnaire.

## Usage

``` r
render_lss_audit_docx(
  lss,
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

- lss:

  An `lss` object returned by
  [`parse_lss()`](https://amaltawfik.github.io/lssdoc/reference/parse_lss.md).

- output:

  Path to the `.docx` file to create.

- languages:

  Character vector of language codes for the cover page. Defaults to all
  languages of the survey.

- logo:

  Optional path to a PNG or JPEG image to display at the top of the
  cover page. `NULL` (default) keeps the cover logo-free.

- logo_width, logo_height:

  Image dimensions in inches. Defaults tuned to a 2:1 logo (1.5 x 0.75
  inches).

- font:

  Body font name. `NULL` (default) keeps Calibri. See
  [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md)
  for guidance on overrides.

- font_code:

  Monospace font used for code-like content (variable codes, raw
  expressions). `NULL` (default) keeps Consolas.

- colors:

  Optional named list of hex color overrides. Same shape and accepted
  names as in
  [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md).

- authors, description:

  Optional cover-page credit block and free-form note. Same shapes as in
  [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md).

- chrome_lang:

  Language used for the document chrome (column headers, row labels,
  audit section). Supported: `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`.
  `NULL` (default) follows `languages[1]` when supported, otherwise
  `"en"`.

## Value

The `output` path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
lss <- parse_lss(system.file("extdata", "limesurvey_survey_751689.lss",
  package = "lssdoc"
))
render_lss_audit_docx(lss, tempfile(fileext = ".docx"))
} # }
```

# Convert a `.lss` file to a Word audit-only document

Pipeline counterpart of
[`lss_to_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_to_docx.md)
for the focused audit report: parses the file, runs the audit, and
writes a `.docx` that contains only the audit findings. Use this for QA
follow-up.

## Usage

``` r
lss_audit_to_docx(input, output, ...)
```

## Arguments

- input:

  Path to a `.lss` file.

- output:

  Path to the `.docx` file to create.

- ...:

  Arguments passed on to
  [`render_lss_audit_docx`](https://amaltawfik.github.io/lssdoc/reference/render_lss_audit_docx.md)

  `languages`

  :   Character vector of language codes for the cover page. Defaults to
      all languages of the survey.

  `logo`

  :   Optional path to a PNG or JPEG image to display at the top of the
      cover page. `NULL` (default) keeps the cover logo-free.

  `logo_width,logo_height`

  :   Image dimensions in inches. Defaults tuned to a 2:1 logo (1.5 x
      0.75 inches).

  `font`

  :   Body font name. `NULL` (default) keeps Calibri. See
      [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md)
      for guidance on overrides.

  `font_code`

  :   Monospace font used for code-like content (variable codes, raw
      expressions). `NULL` (default) keeps Consolas.

  `colors`

  :   Optional named list of hex color overrides. Same shape and
      accepted names as in
      [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md).

  `authors,description`

  :   Optional cover-page credit block and free-form note. Same shapes
      as in
      [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md).

  `chrome_lang`

  :   Language used for the document chrome (column headers, row labels,
      audit section). Supported: `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`.
      `NULL` (default) follows `languages[1]` when supported, otherwise
      `"en"`.

## Value

The `output` path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
lss_audit_to_docx(
  system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc"),
  "audit.docx"
)
} # }
```

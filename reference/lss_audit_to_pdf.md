# Convert a `.lss` file to a PDF audit-only document

Same as
[`lss_audit_to_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_audit_to_docx.md)
but converts the resulting `.docx` to PDF via
[`lss_docx_to_pdf()`](https://amaltawfik.github.io/lssdoc/reference/lss_docx_to_pdf.md).

## Usage

``` r
lss_audit_to_pdf(input, output, ...)
```

## Arguments

- input:

  Path to a `.lss` file.

- output:

  Path to the `.docx` file to create.

- ...:

  Additional arguments forwarded to
  [`render_lss_audit_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_audit_docx.md).

## Value

The `output` path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
lss_audit_to_pdf(
  system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc"),
  "audit.pdf"
)
} # }
```

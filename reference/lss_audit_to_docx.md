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

  Additional arguments forwarded to
  [`render_lss_audit_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_audit_docx.md).

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

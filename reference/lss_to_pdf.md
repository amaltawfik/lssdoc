# Convert a `.lss` file to a PDF review document

Same pipeline as
[`lss_to_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_to_docx.md),
but the generated `.docx` is then converted to PDF locally via
[`lss_docx_to_pdf()`](https://amaltawfik.github.io/lssdoc/reference/lss_docx_to_pdf.md)
(LibreOffice or Word). Nothing leaves the user's machine.

## Usage

``` r
lss_to_pdf(input, output, ...)
```

## Arguments

- input:

  Path to a `.lss` file.

- output:

  Path to the `.pdf` file to create.

- ...:

  Additional arguments forwarded to
  [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md).

## Value

The `output` path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
lss_to_pdf(
  system.file("extdata", "hesav_2026.lss", package = "lssdoc"),
  "rapport.pdf"
)
} # }
```

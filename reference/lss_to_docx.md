# Convert a `.lss` file to a Word review document

One-shot pipeline: parse the LimeSurvey `.lss` file with
[`parse_lss()`](https://amaltawfik.github.io/lssdoc/reference/parse_lss.md)
and render it to a `.docx` review document with
[`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md).
Use the underlying functions directly when you need finer control over
the audit or the layout.

## Usage

``` r
lss_to_docx(input, output, ...)
```

## Arguments

- input:

  Path to a `.lss` file.

- output:

  Path to the `.docx` file to create.

- ...:

  Additional arguments forwarded to
  [`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md).

## Value

The `output` path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
lss_to_docx(
  system.file("extdata", "hesav_2026.lss", package = "lssdoc"),
  "rapport.docx"
)
} # }
```

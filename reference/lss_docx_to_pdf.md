# Convert a `.docx` to `.pdf` locally

Use LibreOffice (or Word, on Windows) in headless mode to convert a
generated `.docx` to a PDF. Conversion stays on the user's machine: no
upload, no network call. LibreOffice (the `soffice` executable) must be
available; the function reports an actionable error if it is not.

## Usage

``` r
lss_docx_to_pdf(docx, pdf)
```

## Arguments

- docx:

  Path to the source `.docx` file.

- pdf:

  Path to the `.pdf` file to produce.

## Value

The `pdf` path, invisibly.

## Details

LibreOffice headless does not refresh Word field values (TOC, page
counts) during conversion, even when the document is flagged to update
fields on open. As a result, the table-of-contents field produced by
[`render_lss_docx()`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md)
appears empty in the converted PDF. To obtain a PDF with a populated
TOC, open the generated `.docx` in Word – the TOC refreshes
automatically – and use `File > Save As > PDF`. Working directly from
the `.docx` is the simplest path.

## Examples

``` r
if (FALSE) { # \dontrun{
lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
  package = "lssdoc"
))
tmp_docx <- tempfile(fileext = ".docx")
render_lss_docx(lss, tmp_docx)
lss_docx_to_pdf(tmp_docx, "rapport.pdf")
} # }
```

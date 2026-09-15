# Report every problem of a Word authoring form at once

**\[experimental\]**

## Usage

``` r
check_form_docx(path)

# S3 method for class 'lss_form_check'
print(x, ..., n = 20L)
```

## Arguments

- path:

  Character. Path of the `.docx` form to check.

- x:

  An `lss_form_check` object.

- ...:

  Ignored.

- n:

  Maximum number of problems to print; `Inf` for all.

## Value

An object of class `lss_form_check`: a data frame with one row per
problem and the columns `severity` (`"error"` or `"warning"`), `class`
(the condition class), `block`, `code`, `field` and `message`. Zero rows
means the document reads:
[`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md)
will return a specification. A
[`print()`](https://rdrr.io/r/base/print.html) method summarizes it.

## Details

**Experimental.** Run
[`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md)'s
parser in dry-run mode: an author iterates on a form until it is clean,
and fixing one problem per round trip is not a workflow. The same rules,
the same messages; the first error simply does not stop the read.

A problem in one block does not hide the problems of the next: parsing
resumes at the following block. Three problems are still fatal, because
nothing can be read past them: an unreadable file, a missing or
unsupported contract version, and a document-wide structural refusal
(tracked changes, content controls). When one of those fires it is the
only row reported.

## See also

[`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md),
[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md),
[`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md).

## Examples

``` r
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {
  form <- tempfile(fileext = ".docx")
  lss_template_docx(form, lang = "fr", kinds = c("single", "text"))
  check_form_docx(form)
}
#> ✔ Wrote /tmp/Rtmp9SqjLn/file1a2a5c90bf73.docx (2 questions, 2 groups, 1 quota).
#> 
#> ── lssdoc form check ───────────────────────────────────────────────────────────
#> File: /tmp/Rtmp9SqjLn/file1a2a5c90bf73.docx
#> ✔ No problems found: the form reads.
```

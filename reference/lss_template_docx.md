# Write a blank Word template for authoring a questionnaire

**\[experimental\]**

## Usage

``` r
lss_template_docx(path, lang = "fr", kinds = lss_kinds$kind)
```

## Arguments

- path:

  Character. Path of the `.docx` file to write.

- lang:

  Language of the template's labels and example wording, one of `"en"`,
  `"fr"` (default), `"de"`, `"es"`, `"it"`.

- kinds:

  Character vector of kinds to illustrate, `lss_kinds$kind` (all 21) by
  default; the order of the kind table is kept.

## Value

Invisibly, the path to the written file.

## Details

**Experimental.** Generate the blank authoring form: one example
question per requested kind, every default pre-filled, and a muted hint
under each key telling the author the syntax the field expects. The
template is always **generated** from the kind table – the package ships
no static `.docx` – so it cannot describe a field
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
would refuse.

Delete the example questions you do not need, edit the others, then hand
the file to the reader (0.3.0) to obtain an
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
and, through
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md),
a `.lss` file LimeSurvey imports.

The template is the render of a generated example specification, so the
same object feeds the template, the tests and the vignette. The value of
a Type row is always the spec kind (`single`, `array5`, ...); the
localized type label is shown as the hint under the key, because it is
many-to-one and could not be read back. Reserved words are localized: an
option line reading `Autre` in a French template creates the native
other option, and an ordinary option that happens to read "Autre" is
written with an explicit code (`9 = Autre`) – which the hint under the
Options key spells out.

Requires the suggested packages officer and flextable.

## See also

[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md)
to render an existing specification,
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md),
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md).

## Examples

``` r
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {
  out <- tempfile(fileext = ".docx")
  lss_template_docx(out, lang = "fr", kinds = c("single", "multiple"))
  file.exists(out)
}
#> ✔ Wrote /tmp/RtmpIpXP1P/file194a34641dc6.docx (2 questions, 1 group, 1 quota).
#> [1] TRUE
```

# Render a survey specification as a Word authoring form

**\[experimental\]**

## Usage

``` r
write_form_docx(spec, path, lang = NULL, hints = FALSE, strict = TRUE)
```

## Arguments

- spec:

  An
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  object, or a plain list with the same structure (it is then validated
  through
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  first).

- path:

  Character. Path of the `.docx` file to write.

- lang:

  Language of the form's own labels (its "chrome"), one of `"en"`,
  `"fr"`, `"de"`, `"es"`, `"it"`. `NULL` (default) follows the survey's
  primary language when it is one of them, English otherwise. It is
  independent of the survey's content languages.

- hints:

  Logical. Add the muted syntax hint under each key
  (`"one per line, \"1 = Label\""`, ...). `FALSE` by default;
  [`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md)
  turns it on for the blank template.

- strict:

  Logical, used only when `spec` is an `lss` object read by
  [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md):
  it is converted with
  [`as_lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/as_lss_spec.md),
  and `strict` is passed to it. `TRUE` (default) refuses a survey
  carrying anything the specification cannot express; `FALSE` renders
  the rest of it and warns.

## Value

Invisibly, the path to the written file.

## Details

**Experimental.** Write an
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
as a `.docx` **form**: one two-column table per block (Survey, Group,
Question, Quota), keys on the left, values on the right. Unlike the
review documents produced by
[`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md),
this document is meant to be *edited*: an author fills or changes the
value cells in Word and the companion reader (0.3.0) turns the file back
into a specification.

The rows a Question block carries are decided by the question's kind, so
the form shows exactly the fields that kind accepts and never a field
the validator would refuse. Defaults are written as real values
(Mandatory `No`, an empty Filter, the other option at the End) rather
than as placeholders, because anything sitting in a value cell is
content.

Conventions the document obeys, and the companion reader relies on:

- Each block is a top-level two-column table whose first row is its
  title row; two empty paragraphs separate two blocks. No cell is
  merged.

- Multi-valued fields (Options, Rows, Columns, Exclusive) hold one value
  per line. A line break inside a single-line field (an option label, a
  title, a quota name) is refused with a classed error naming the
  question, the field and the offending lines.

- The Type row carries the spec kind (`single`, `array5`, ...) as its
  value; the localized type label ("Single choice") is deliberately
  many-to-one, so it appears only as the key's hint in a blank template,
  never as content.

- Options are always written with an explicit code, `1 = Label`; the
  native other option is written with the reserved word of the form
  language (`Other`, `Autre`, `Sonstiges`, `Otro`, `Altro`), alone when
  the option has no label and as `Other = Label` as soon as it has one.

- When the spec declares several languages, every localizable key is
  written once per language and suffixed with the language code –
  `Question [fr]`, `Question [en]` – primary language first, the primary
  one suffixed as well.

- The file carries the custom document property
  `lssdoc-template-version`, the version of this contract.

Requires the suggested packages officer and flextable.

## See also

[`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md)
for a blank template,
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md),
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md),
[`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md).

## Examples

``` r
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {
  spec <- lss_spec(
    title = "Demo",
    groups = list(list(title = "G", questions = list(
      list(code = "q1", kind = "single", text = "Oui ou non ?",
           options = list(list(text = "Oui"), list(text = "Non")))
    )))
  )
  out <- tempfile(fileext = ".docx")
  write_form_docx(spec, out, lang = "fr")
  file.exists(out)
}
#> ✔ Wrote /tmp/Rtmpk3zuc8/file1a0d46b5940.docx (1 question, 1 group, 0 quotas).
#> [1] TRUE
```

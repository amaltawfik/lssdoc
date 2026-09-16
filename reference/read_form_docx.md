# Read a Word authoring form back into a survey specification

**\[experimental\]**

## Usage

``` r
read_form_docx(path)
```

## Arguments

- path:

  Character. Path of the `.docx` form to read.

## Value

An
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
object.

## Details

**Experimental.** Parse a `.docx` **authoring form** – the document
[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md)
and
[`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md)
produce, filled in by an author in Word – back into an
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md),
ready for
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
and a LimeSurvey import. The round trip is exact: a specification
written as a form and read back describes the same survey.

Reading needs no suggested package: only xml2 and
[`utils::unzip()`](https://rdrr.io/r/utils/unzip.html), so an author's
file can be turned into a `.lss` on a bare installation. officer and
flextable are needed to WRITE a form, never to read one.

The contract the document must honor – all of it written by
[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md),
and spelled out in the blank template's hints:

- one top-level two-column table per block (Survey, Group, Question,
  Quota), the key on the left and the value on the right; the first row
  of a block names it, and a title row starts a new block even in the
  middle of a table (pasting a block makes Word merge two tables).

- keys are matched by their TEXT, ignoring case, accents, quotes and a
  trailing colon, against the labels of all five chrome languages, so a
  French author can fill an English template. Only the first line of a
  key cell is read: the muted hints under the key are ignored.

- a value cell holds one value per line, where a line is a paragraph or
  a soft return (Enter or Shift+Enter). Options, rows and columns are
  read as `code = label`, or as a bare label that gets auto-numbered;
  the reserved word (`Other`, `Autre`, `Sonstiges`, `Otro`, `Altro`)
  alone or on the left of `=` is the native other option.

- the Type cell carries the kind code (`single`, `array5`, ...); a
  localized type label is accepted only when it names exactly one kind.

- `Mandatory` takes the yes/no words of any language (and `y`/`n`,
  `true`/`false`, `1`/`0`); blank means no. The `Filter` cell takes the
  mini-language of
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  (`Q1 = 1`, `Q2 in [1, autre]`, `count(Q3) >= 2`); its keywords are
  matched whatever Word capitalized, and the case of a question or
  answer code is never changed.

- when the survey declares several languages, every localizable key is
  suffixed with a language code – `Wording [fr]`, `Wording [en]` – and
  every declared language must supply every text.

- the file must carry the custom document property
  `lssdoc-template-version`: it is the reader's proof that the document
  agreed to this contract. Tracked changes, content controls, merged
  cells, a third column, a nested table and Word's automatic list
  numbering are each refused with a classed error naming the block and
  the field.

Errors carry the class `lssdoc_bad_form` plus one leaf class
(`lssdoc_bad_form_file`, `_marker`, `_layout`, `_block`, `_key`,
`_value`, `_spec`), and every message names the block, the question code
and the field. Use
[`check_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/check_form_docx.md)
to list every problem of a document at once instead of stopping at the
first.

## See also

[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md)
and
[`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md)
to write the form,
[`check_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/check_form_docx.md)
for a dry run,
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md),
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md).

## Examples

``` r
if (requireNamespace("officer", quietly = TRUE) &&
    requireNamespace("flextable", quietly = TRUE)) {
  form <- tempfile(fileext = ".docx")
  lss_template_docx(form, lang = "fr", kinds = c("single", "text"))
  spec <- read_form_docx(form)
  spec
}
#> ✔ Wrote /tmp/Rtmp7B7HAr/file195117a1d8b1.docx (2 questions, 2 groups, 1 quota).
#> <lss_spec> "Questionnaire d'exemple lssdoc" (fr)
#> 2 groups, 2 questions, 1 quota
```

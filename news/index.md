# Changelog

## lssdoc (development version)

### New features

- Write a questionnaire in Word and turn it into a LimeSurvey file. New
  experimental authoring form, a third document template:
  [`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md)
  writes a blank form (one key/value table per survey, group, question
  and quota, every field a question type needs, defaults pre-filled and
  syntax hints in the key column);
  [`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md)
  renders any
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  as that form;
  [`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md)
  parses a filled form back into an `lss_spec`, ready for
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md);
  and
  [`check_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/check_form_docx.md)
  reports every problem in a form at once (block, question code and
  field named), so an author can fix them in one pass. The form
  round-trips losslessly and is read with `xml2` only (Word is not
  required to read it, only to fill it in).

- [`as_lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/as_lss_spec.md)
  converts a survey read with
  [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  into an `lss_spec`, so an existing LimeSurvey questionnaire can be
  rendered as the Word form
  ([`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md)),
  edited and re-imported. Question types and filters the specification
  cannot express are refused with a complete list (`strict = TRUE`) or
  dropped with a warning naming each one (`strict = FALSE`); HTML texts
  are flattened and missing translations filled from the primary
  language, both reported. Nothing is lost silently.

- [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  gains an optional localised group `description` and an optional quota
  `limit`, both written by
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md).

## lssdoc 0.2.0

CRAN release: 2026-09-13

### New features

- New experimental authoring layer.
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  describes a questionnaire in R – groups, 21 question kinds, answer
  options (including “other” and exclusive choices), display conditions
  and quotas – and validates it against what LimeSurvey accepts.
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
  then writes it as a LimeSurvey 6 `.lss` file ready to import. Both
  functions are experimental and their interface may change.

- [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  accepts texts in several languages (`languages = c("fr", "en")`, each
  text given as a named vector), but
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
  emits the primary language only for now. Multilingual output is
  planned for 0.3.0.

### Minor improvements and bug fixes

- [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  now warns (class `lssdoc_newer_dbversion`) when a file comes from a
  newer LimeSurvey than the package targets, instead of reading it
  silently.

- In the table layout, the “Type” column no longer wraps one-word labels
  such as “Computed” onto a second line.

## lssdoc 0.1.1

CRAN release: 2026-06-18

- [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  now returns a clear error on a non-XML or empty file, instead of, on
  some systems, crashing the R session.

## lssdoc 0.1.0

CRAN release: 2026-06-15

- Initial CRAN release.

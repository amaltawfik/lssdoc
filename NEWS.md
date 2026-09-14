# lssdoc 0.3.0

## New features

* Write a questionnaire in Word and turn it into a LimeSurvey file. New
  experimental authoring form, a third document template: `lss_template_docx()`
  writes a blank form (one key/value table per survey, group, question and
  quota, every field a question type needs, defaults pre-filled and syntax
  hints in the key column); `write_form_docx()` renders any `lss_spec()` as
  that form; `read_form_docx()` parses a filled form back into an `lss_spec`,
  ready for `write_lss()`; and `check_form_docx()` reports every problem in a
  form at once (block, question code and field named), so an author can fix
  them in one pass. The form round-trips without loss and is read with `xml2`
  only (Word is not required to read it, only to fill it in).

* `as_lss_spec()` converts a survey read with `read_lss()` into an `lss_spec`,
  so an existing LimeSurvey questionnaire can be rendered as the Word form
  (`write_form_docx()`), edited and re-imported. Question types and filters the
  specification cannot express are refused with a complete list
  (`strict = TRUE`) or dropped with a warning naming each one (`strict = FALSE`);
  HTML texts are flattened and missing translations filled from the primary
  language, both reported. Nothing is lost silently.

* `lss_spec()` gains an optional localized group `description` and an optional
  quota `limit`, both written by `write_lss()`.

## Bug fixes

* `read_lss()` now fails with a clear `lssdoc_invalid_xml` error on any
  malformed, truncated or non-UTF-8 file, on every platform. The encoding
  is validated in R and the file is checked for a complete `document`
  envelope *before* the XML parser is called at all, so a fatal parser
  error can no longer terminate the R session as it did on CRAN's
  r-devel-linux-x86_64-fedora-gcc build (the same family of crash 0.1.1
  had fixed for files that do not start with an XML tag). Files with a
  UTF-16 byte-order mark are transcoded instead of being refused, and the
  parser is called with network access disabled.

# lssdoc 0.2.0

## New features

* New experimental authoring layer. `lss_spec()` describes a questionnaire
  in R -- groups, 21 question kinds, answer options (including "other" and
  exclusive choices), display conditions and quotas -- and validates it
  against what LimeSurvey accepts. `write_lss()` then writes it as a
  LimeSurvey 6 `.lss` file ready to import. Both functions are experimental
  and their interface may change.

* `lss_spec()` accepts texts in several languages (`languages = c("fr", "en")`,
  each text given as a named vector), but `write_lss()` emits the primary
  language only for now. Multilingual output is planned for 0.3.0.

## Minor improvements and bug fixes

* `read_lss()` now warns (class `lssdoc_newer_dbversion`) when a file comes
  from a newer LimeSurvey than the package targets, instead of reading it
  silently.

* In the table layout, the "Type" column no longer wraps one-word labels
  such as "Computed" onto a second line.

# lssdoc 0.1.1

* `read_lss()` now returns a clear error on a non-XML or empty file, instead
  of, on some systems, crashing the R session.

# lssdoc 0.1.0

* Initial CRAN release.

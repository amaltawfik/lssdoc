# Changelog

## lssdoc (development version)

- New experimental authoring layer:
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  builds and deeply validates a survey specification (21 question kinds
  attested by a corpus of real exports, option codes, exclusives, caps,
  display conditions in a minimal syntax, end-of-survey quotas), and
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
  emits it as a LimeSurvey 6 (DBVersion 700) `.lss` file that imports
  directly. Validation covers what LimeSurvey silently accepts and
  breaks: localized attributes without a language code, conditions
  citing questions defined later or multiple-choice targets, caps at or
  above the option count, an “other” option marked exclusive, malformed
  option codes. The written file round-trips through
  [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  and
  [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md).

- The table codebook’s “Type” column no longer wraps one-word labels
  (such as “Computed”) onto a second line.

## lssdoc 0.1.1

CRAN release: 2026-06-18

- [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  now returns a clear error on a non-XML or empty file, instead of, on
  some systems, crashing the R session.

## lssdoc 0.1.0

CRAN release: 2026-06-15

- Initial CRAN release.

# lssdoc: Render 'LimeSurvey' '.lss' Questionnaires as Word and PDF Documents

Render 'LimeSurvey' '.lss' survey exports as questionnaire documents in
Word ('.docx') or PDF, displaying one to four languages side by side
with localized chrome in English, French, German, Spanish and Italian.
Includes a rule-based automated audit that flags missing translations,
forward filter references, duplicate codes, array-scale inconsistencies
and orphan structural references. Designed for anyone working with a
'LimeSurvey' survey: researchers, methodologists, ethics committees,
translators and reviewers. Processing is fully local: the source file is
the only input and no questionnaire content is uploaded to a third-party
service.

## Example surveys

Two example `.lss` files ship with the package and are reachable with
[`base::system.file()`](https://rdrr.io/r/base/system.file.html), so
every reader can reproduce the examples and the *Get started* vignette
without supplying their own LimeSurvey export:

- `demo_survey.lss` – a clean, synthetic four-language survey (English,
  French, German, Spanish) with quotas and a consent block:
  `system.file("extdata", "demo_survey.lss", package = "lssdoc")`.

- `audit_demo.lss` – a deliberately flawed survey seeded with every
  anomaly
  [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  detects:
  `system.file("extdata", "audit_demo.lss", package = "lssdoc")`.

## See also

Useful links:

- <https://amaltawfik.github.io/lssdoc/>

- <https://github.com/amaltawfik/lssdoc>

- Report bugs at <https://github.com/amaltawfik/lssdoc/issues>

## Author

**Maintainer**: Amal Tawfik <amal.tawfik@hesav.ch>
([ORCID](https://orcid.org/0009-0006-2422-1555))
([ROR](https://ror.org/04j47fz63)) \[copyright holder\]

Authors:

- Amal Tawfik <amal.tawfik@hesav.ch>
  ([ORCID](https://orcid.org/0009-0006-2422-1555))
  ([ROR](https://ror.org/04j47fz63)) \[copyright holder\]

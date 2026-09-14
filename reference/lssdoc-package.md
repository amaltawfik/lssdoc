# lssdoc: 'LimeSurvey' '.lss' Questionnaires to and from Word Documents

Turn a 'LimeSurvey' '.lss' survey export into a publication-quality
questionnaire document in Word ('.docx') or PDF, with up to four of the
survey's own languages side by side. Every label the package adds around
that content – column headers, type names, the audit section – is
written in English, French, German, Spanish or Italian, whatever the
survey languages are. A rule-based audit flags missing translations,
forward filter references, duplicate codes, array-scale inconsistencies
and orphan structural references. Questionnaires travel the other way
too: describe one in R, or fill in a Word form, and write a '.lss' file
ready to import. Meant for the people who work on questionnaires –
researchers, methodologists, ethics committees, translators and
reviewers – and fully local: the source file is the only input, and no
questionnaire content is uploaded to a third-party service.

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

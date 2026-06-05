# Read a LimeSurvey `.lss` file

Read a LimeSurvey survey structure export (`.lss`, an XML file) and turn
it into a structured `lss` object that the rest of the package can audit
([`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md))
and render
([`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md),
[`render_audit()`](https://amaltawfik.github.io/lssdoc/reference/render_audit.md)).
Parsing is fully local: the file is never uploaded anywhere.

## Usage

``` r
read_lss(file)
```

## Arguments

- file:

  Character. Path to a `.lss` file. Must be a single string pointing to
  an existing file, otherwise a classed error is raised
  (`lssdoc_bad_path`, `lssdoc_file_not_found`).

## Value

An object of class `lss`: a list with the survey languages, metadata,
and one data frame per `.lss` section. Structural sections (`surveys`,
`groups`, `questions`, `subquestions`, `answers`, `question_attributes`,
`conditions`) stay separate from the localized text sections
(`survey_language_settings`, `group_l10ns`, `question_l10ns`,
`answer_l10ns`), which carry the per-language titles, labels, and help
texts. All values are read verbatim as character.

## Details

The `.lss` format is a LimeSurvey XML export. Since DBVersion 4xx/7xx
the translatable text lives in dedicated localization sections
(`*_l10ns`), keyed by language, while the structural sections hold
identifiers and settings. `read_lss()` reads every section into a tidy
data frame without mutating any user-facing identifier or text. A field
that is present but empty (e.g. `<help/>`) is read as `""`; a field that
is absent from a row is read as `NA`.

## Examples

``` r
# A synthetic four-language demo survey ships with the package.
demo <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
lss <- read_lss(demo)
lss$languages
#> [1] "en" "de" "es" "fr"
```

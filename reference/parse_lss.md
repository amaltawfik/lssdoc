# Parse a LimeSurvey `.lss` file

Read a LimeSurvey survey structure export (`.lss`, an XML file) and turn
it into a structured `lss` object that the rest of the package can audit
and render. Parsing is fully local: the file is never uploaded anywhere.

## Usage

``` r
parse_lss(path)
```

## Arguments

- path:

  Path to a `.lss` file.

## Value

An object of class `lss`: a list holding the survey languages and
metadata plus one data frame per `.lss` section. Structural sections
(`surveys`, `groups`, `questions`, `subquestions`, `answers`,
`question_attributes`, `conditions`) are kept separate from the
localized text sections (`survey_language_settings`, `group_l10ns`,
`question_l10ns`, `answer_l10ns`), which carry the per-language titles,
labels, and help texts. All values are read verbatim as character.

## Details

The `.lss` format is a LimeSurvey XML export. Since DBVersion 4xx/7xx
the translatable text lives in dedicated localization sections
(`*_l10ns`), keyed by language, while the structural sections hold
identifiers and settings. `parse_lss()` reads every section into a tidy
data frame without mutating any user-facing identifier or text. A field
that is present but empty (e.g. `<help/>`) is read as `""`; a field that
is absent from a row is read as `NA`.

## Examples

``` r
lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
  package = "lssdoc"
))
lss$languages
#> [1] "de" "fr"
```

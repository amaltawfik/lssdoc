# Audit a parsed LimeSurvey structure for reviewable anomalies

Inspect an `lss` object and flag anomalies that can be detected without
any AI. The audit is meant to guide a human reviewer, not to silently
correct anything: every finding names a precise location and a severity.

## Usage

``` r
audit_lss(lss)
```

## Arguments

- lss:

  An `lss` object returned by
  [`parse_lss()`](https://amaltawfik.github.io/lssdoc/reference/parse_lss.md).

## Value

An object of class `lss_audit`: a list with `file`, `languages`, summary
counts, and a `findings` data frame (`severity`, `check`, `location`,
`language`, `message`). It has a
[`print()`](https://rdrr.io/r/base/print.html) method and an
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method.

## Details

Checks performed:

- **Missing translations** – a question, help, answer, or subquestion
  text that exists in at least one language but is empty in another.

- **Empty in all languages** – a translatable text that is empty in
  every language.

- **Duplicate codes** – a question variable code repeated in the survey,
  or an answer/subquestion code repeated within one question.

- **Missing options for the type** – a question whose type requires
  answer options or subquestions but has none (per the type taxonomy).

- **Orphan references** – a subquestion or answer that points to a
  question that does not exist.

## Examples

``` r
lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
  package = "lssdoc"
))
audit <- audit_lss(lss)
print(audit)
#> 
#> ── lssdoc audit ────────────────────────────────────────────────────────────────
#> File: /tmp/RtmphEng2K/temp_libpath1a1510460977/lssdoc/extdata/hesav_2026.lss
#> Languages: "de" and "fr"
#> ✔ No anomalies detected.
```

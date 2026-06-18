# Audit a LimeSurvey survey for reviewable anomalies

Inspect a LimeSurvey survey and flag anomalies that can be detected
without any AI. The audit guides a human reviewer; it does not silently
correct anything. Every finding names a precise location and a severity.

## Usage

``` r
audit_lss(input)
```

## Arguments

- input:

  Either a path to a `.lss` file (character string) or a pre-parsed
  `lss` object returned by
  [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md).
  Passing a path parses it on the fly; passing an `lss` object avoids
  re-parsing when the same survey is also rendered in the same session.

## Value

An object of class `lss_audit`: a list with `file`, `languages`, summary
counts, and a `findings` data frame (`severity`, `check`, `location`,
`language`, `message`). It has a
[`print()`](https://rdrr.io/r/base/print.html) method and an
[`as.data.frame()`](https://rdrr.io/r/base/as.data.frame.html) method.

## Details

Checks performed:

- **Missing translations** – a question, help, answer, or subquestion
  text present in at least one language but empty in another.

- **Empty in all languages** – a translatable text empty in every
  language.

- **Duplicate codes** – a question variable code repeated in the survey,
  or an answer/subquestion code repeated within one question.

- **Whitespace in codes** – a question, subquestion or answer code
  containing leading, trailing or interior whitespace (likely a typo;
  causes subtle bugs in the data export).

- **Missing options for the type** – a question whose type requires
  answer options or subquestions but has none (per the type taxonomy).

- **Forward filter references** – a relevance expression that names a
  variable appearing at or after the filtered question (the value is not
  yet collected when the filter is evaluated).

- **Array-scale inconsistencies** – an array (single or dual) whose
  subquestions reference a `scale_id` that has no answer options, or
  vice versa.

- **Orphan references** – a subquestion or answer pointing to a question
  that does not exist.

## See also

[`render_audit()`](https://amaltawfik.github.io/lssdoc/reference/render_audit.md)
to write the same findings to a Word or PDF document.

## Examples

``` r
# A deliberately flawed demo survey ships with the package, seeded
# with every anomaly the audit detects.
demo <- system.file("extdata", "audit_demo.lss", package = "lssdoc")
audit_lss(demo)
#> 
#> ── lssdoc audit ────────────────────────────────────────────────────────────────
#> File: /tmp/Rtmpa0Dfw1/temp_libpath1ad26fb464f8/lssdoc/extdata/audit_demo.lss
#> Languages: "en" and "fr"
#> 12 findings: 5 errors, 7 warnings, 0 notes.
#> ✖ Survey: Duplicate question code: 'age'.
#> ✖ Question 'blank_q': The question text is empty in every language.
#> ✖ Question 'age': Filter references variable 'income' (item 4), which is not
#>   asked before this question (item 1).
#> ✖ Answer 'X': Answer points to question id '99999', which does not exist.
#> ✖ Subquestion 'orphan_sq': Subquestion points to question id '99999', which
#>   does not exist.
#> Question 'arr': Subquestions reference scale_id '0' but no answer options are
#> defined for it.
#> Question 'arr': Answer options reference scale_id '1' but no subquestions are
#> defined for it.
#> Question 'comment ': The question code 'comment ' contains whitespace;
#> LimeSurvey will export it verbatim, which usually breaks downstream lookups.
#> Group: The group name is empty in every language.
#> Question 'satisf': Type 'Single choice' expects answer options, but none are
#> defined.
#> Question 'rating': Type 'Multiple choice' expects subquestions, but none are
#> defined.
#> Question 'income' [fr]: The question text is missing in 'fr' but present in
#> other languages.
```

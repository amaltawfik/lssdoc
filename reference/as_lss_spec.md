# Turn a parsed LimeSurvey survey into an authoring specification

**\[experimental\]**

## Usage

``` r
as_lss_spec(lss, strict = TRUE)
```

## Arguments

- lss:

  An `lss` object from
  [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md).

- strict:

  Logical. `TRUE` (default) refuses the whole survey with a single
  classed error (`lssdoc_unconvertible`) listing **every** unconvertible
  item. `FALSE` drops those items – and the filters and quotas that
  depend on them – and returns a valid specification, with one warning
  (`lssdoc_lossy_conversion`) listing what was dropped.

## Value

An
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
object.

## Details

**Experimental.** Convert an `lss` object – a real LimeSurvey export
read by
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
– into the
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
object the authoring side of the package works with. It is the entry
point for *modifying an existing questionnaire*: read the `.lss` a
colleague sends, render it as the Word authoring form with
[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md),
let the author edit the form, and hand the result back to
[`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md)
and
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md).

The conversion is deliberately narrower than the file it reads. An `lss`
object is whatever LimeSurvey exported; an `lss_spec` is what lssdoc can
author and re-emit. Everything in between is reported: refused outright
(`strict = TRUE`) or dropped with a warning (`strict = FALSE`). Nothing
is ever lost in silence.

## What is converted

- **Kinds**: the LimeSurvey type letter maps to the authoring kind
  through the kind table (`lss_kinds` in the sources). A question theme
  other than the authorable one is replaced by it and reported.

- **Texts**: survey title, welcome and end text, group titles and
  descriptions, question wordings and help, option, row and column
  labels, the "other" label and the quota name and message, each taken
  per language from the `*_l10ns` tables. HTML is flattened to plain
  text – paragraphs and `<br>` become line breaks, inline marks are
  dropped – and every field that really carried markup is named in the
  lossy warning.

- **Languages**: all the survey's languages, the base language
  (`surveys.language`) first, as
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  requires. A text missing in a language is filled from the base
  language and reported: real exports do have missing translations –
  that is what
  [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  flags – and the author fixes them in the form. A text missing in the
  BASE language but present in another is filled the other way round,
  and the report names the direction
  (`"help of question Q1 [fr, from en]"`), so the translations a survey
  drafted in another language already has are never thrown away.

- **Structure**: options, rows and columns from `answers` and
  `subquestions`; the native "other" option from `other = "Y"` and
  `other_replace_text`; exclusive options from `exclude_all_others`; the
  cap from `max_answers`; the "other" position from `other_position` and
  `other_position_code`. Every other question attribute passes through
  unchanged into the question's `attributes`.

- **Filters**: the three equations
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
  emits – `Q.NAOK == "1"`, `(Q.NAOK == "1" or Q.NAOK == "-oth-")` and
  `count(Q_1.NAOK, ...) >= n` – are translated back into `Q = 1`,
  `Q in [1, autre]` and `count(Q) >= n`, with or without `.NAOK`.

- **Quotas**: a single-member quota with the terminate action becomes a
  `code = value` condition on its question – on any kind holding a
  single coded answer, the implicit scales included, so a quota on the
  gender question (`M` / `F`) converts like a quota on a declared option
  code.

## What is refused

Each of the following is one row of the unconvertible report (question
code, item, reason), and all of them are listed at once:

- a question whose type is not authorable – the eight deferred
  LimeSurvey types (`P`, `H`, `1`, `;`, `:`, `*`, `|`, `I`) and any
  unknown one;

- a question code that is not a spec code, or a duplicate of an earlier
  one;

- a question or option with no wording in the base language, or a shape
  the
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  validator refuses (too few options, an array without rows or columns,
  a cap larger than the option list);

- a display condition in any other form than the three above – a
  hand-written equation is never guessed at;

- a quota combining several questions, built on a question the spec
  cannot target, or carrying an action other than "terminate";

- a group with no title in the base language (its questions go with it),
  and a question belonging to no group at all;

- a question storing rows on a second answer scale, or in a section its
  type does not carry: the shape on file is not the shape lssdoc emits,
  and half an option list is not a smaller option list;

- a structural column carrying something a specification has no field
  for and
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
  would silently replace by its own constant, where that changes what
  the respondent meets: a group display equation (`groups.grelevance`)
  or randomization, a per-subquestion relevance (an array filter), a
  validation regex (`preg`), per-question JavaScript
  (`question_l10ns.script`), encrypted storage (`encrypted`). The
  columns that change nothing a respondent sees – assessment values,
  `same_default`, `modulename`, `same_script`, a quota URL description –
  are noted in the lossy warning instead;

- a multilingual survey that does not declare its base language: a
  specification's first language IS its base language, and `<languages>`
  lists the base one last.

## See also

[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md),
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md),
[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md),
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md).

## Examples

``` r
demo <- system.file("extdata", "demo_survey.lss", package = "lssdoc")
# The demo survey uses LimeSurvey types lssdoc does not author yet, so it
# converts only in the permissive mode.
spec <- suppressWarnings(as_lss_spec(read_lss(demo), strict = FALSE))
spec
#> <lss_spec> "Questionnaire de démonstration lssdoc" (fr, en, de, and es)
#> 6 groups, 41 questions, 1 quota
```

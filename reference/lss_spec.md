# Build and validate a survey specification

**Experimental.** Assemble a survey specification – the authoring-side
counterpart of the `lss` object – that
[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
can turn into an importable LimeSurvey `.lss` file. The specification is
validated in depth at construction time, because LimeSurvey itself
imports silently: a mistyped attribute, a filter referencing a missing
answer code, or a cap larger than the option list are all accepted on
import and only surface once respondents hit them.

## Usage

``` r
lss_spec(
  title,
  groups,
  language = "fr",
  welcome = NULL,
  end_text = NULL,
  quotas = NULL
)
```

## Arguments

- title:

  Character. Survey title shown to respondents.

- groups:

  List of groups. Each group is a list with `title` (character) and
  `questions` (list of question specifications, see Details).

- language:

  Character. Single language code of the survey (e.g. `"fr"`).
  Multi-language authoring is not supported yet.

- welcome:

  Character vector of welcome-text paragraphs, or a single string
  starting with `<` used verbatim as HTML. Optional.

- end_text:

  Character vector of end-page paragraphs, or a single string starting
  with `<` used verbatim as HTML. Optional.

- quotas:

  List of end-of-survey quotas. Each element is a list with `question`
  (code of a single-choice question), `code` (the answer code that
  triggers the quota), `message` (text shown to the respondent) and
  optionally `name`. A quota emitted by
  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
  has limit zero and terminates the survey – the LimeSurvey mechanism
  for "if the person declines, end here".

## Value

An object of class `lss_spec`: the validated specification with
normalized questions (auto-numbered option codes filled in).

## Details

Each question is a list with fields:

- `code` – stable technical code (letters then letters/digits, at most
  20 characters), unique across the survey. Codes become variable names
  in the data and are pinned once fieldwork starts.

- `kind` – the question type. Choice kinds: `"single"` (radio list),
  `"dropdown"`, `"singlecomment"` (list with comment), `"multiple"`,
  `"ranking"`. Array kinds: `"array"` (rows and columns), `"array5"`,
  `"array10"`, `"arrayyesno"`, `"arraytrend"` (rows only, the scale is
  implicit). Item batteries: `"multitext"`, `"multinumeric"` (one field
  per option). Scalar kinds: `"text"`, `"shorttext"`, `"hugetext"`,
  `"numeric"`, `"date"`, `"yesno"` (implicit Y/N), `"gender"` (implicit
  M/F), `"fivepoint"` (implicit 1-5). Plus `"display"` (text shown
  without input). Every kind maps to a LimeSurvey type attested by real
  exports; types that would require an unverified mechanism (dual-scale
  arrays of texts or numbers, equations, file upload) are deliberately
  not supported yet.

- `text` – the question wording. `mandatory` – logical, default `FALSE`.
  `help` – optional help text shown under the wording.

- `options` – for `single`, `multiple` and `ranking`: list of options,
  each a list with `text` and optionally `code`, `other = TRUE` (native
  LimeSurvey "other" with a free-text field; `single` and `multiple`
  only) and `exclusive = TRUE` (`multiple` only; unchecks every other
  box). Options without a `code` are numbered `1..n` in order, skipping
  the `other` option, which LimeSurvey codes natively.

- `rows` / `columns` – for `array`: the subquestions and the answer
  scale, same shape as `options`.

- `relevance` – display condition in a minimal syntax: `code = 1`,
  `code in [1, 2, autre]`, `count(code) >= 2` (at least n boxes ticked
  in a multiple-choice question). The keyword `autre` designates the
  native "other" option. Conditions may only reference questions defined
  earlier in the survey.

- `max_answers` – cap for `multiple` (strictly below the number of
  options) and `ranking` (at most the number of items).

- `other_position` – where the "other" option is displayed: `"end"`
  (LimeSurvey default), `"beginning"`, or `"specific"` together with
  `other_position_code`, the code of the option AFTER which "other"
  appears. In practice "other" usually belongs before the "none of the
  above"-type exclusive options, which the default position puts it
  after.

- `attributes` – optional named list of extra global question attributes
  passed through verbatim (e.g. `display_columns`).

## See also

[`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
to emit the `.lss` file,
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
and
[`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
to read it back and check it.

## Examples

``` r
spec <- lss_spec(
  title = "Demo",
  language = "fr",
  groups = list(list(
    title = "Profil",
    questions = list(
      list(code = "consent", kind = "single", text = "Participez-vous ?",
           mandatory = TRUE,
           options = list(list(text = "Oui"), list(text = "Non"))),
      list(code = "raisons", kind = "multiple", text = "Pourquoi ?",
           relevance = "consent = 1", max_answers = 2,
           options = list(
             list(text = "Une raison"), list(text = "Une autre"),
             list(text = "Encore une"),
             list(text = "Aucune raison", exclusive = TRUE),
             list(text = "Autre raison", other = TRUE)))
    )
  ))
)
spec$groups[[1]]$questions[[2]]$options[[4]]$code
#> [1] "4"
```

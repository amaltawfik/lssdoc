# Write a survey specification to an importable `.lss` file

**\[experimental\]**

## Usage

``` r
write_lss(spec, file, sid = 100001L, settings = list())
```

## Arguments

- spec:

  An
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  object, or a plain list with the same structure (it is then validated
  through
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  first).

- file:

  Character. Path of the `.lss` file to write.

- sid:

  Integer. Survey id embedded in the file. LimeSurvey assigns a fresh id
  on import when this one is taken, so the value rarely matters.

- settings:

  Named list of survey fields overriding the built-in defaults (e.g.
  `list(anonymized = "Y", showprogress = "Y")`). A name belonging to the
  `surveys` table takes a single string; a name belonging to the
  per-language `surveys_languagesettings` table (`surveyls_dateformat`,
  `surveyls_numberformat`, `surveyls_description`, the e-mail templates,
  ...) takes either a single string applied to every language or a list
  or vector keyed by language code – see the *Languages* section. The
  defaults ship with the package and come from a real LimeSurvey 6
  export, scrubbed – see `lss_default_surveys_fields` in the sources for
  the rationale.

## Value

Invisibly, the path to the written file.

## Details

**Experimental.** Turn an
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
specification into a LimeSurvey structure file (`.lss`) that imports
directly through *Create survey -\> Import*. The output targets
LimeSurvey 6 (DBVersion 700). The emitted file can be read back with
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md),
checked with
[`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
and rendered with
[`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md)
– so the document reviewers read is produced from the very file
LimeSurvey receives.

Mapping choices, each validated against real LimeSurvey 6 imports:

- Each kind maps to a LimeSurvey type and theme attested by a corpus of
  real exports (see `lss_kinds` in the sources). Options of
  single-choice lists, rankings and array columns are emitted as
  `answers`; options of multiple-choice questions, item batteries and
  array rows as `subquestions`; scalar kinds and implicit scales
  (yes/no, gender, five-point, 5/10-point arrays) emit none.

- The native `other` option is emitted as `other = "Y"` plus the
  localized attribute `other_replace_text`. Localized attributes MUST
  carry the language code: emitted without one, LimeSurvey silently
  ignores them and shows its default wording. Global attributes
  (`exclude_all_others`, `max_answers`, ...) stay language-less.

- `other_position` / `other_position_code` control where the other
  option is displayed; `exclude_all_others` accepts several codes
  separated by `;`.

- Relevance equations are translated from the minimal syntax of
  [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
  into ExpressionScript (`code.NAOK == "1"`).

- Quotas are emitted with the terminate action and the quota's `limit`
  (zero unless the spec gives one).

- A group's optional `description` is emitted into
  `group_l10ns.description` (empty when the spec gives none).

- A mandatory or capped ranking also receives `min_answers = 1`,
  overridable through the question's `attributes`.

## Languages

Every language the spec declares is written. The `surveys` row carries
the base language – `languages[1]` – as `language` and the others,
space-separated, as `additional_languages`. Every localized section
(`surveys_languagesettings`, `group_l10ns`, `question_l10ns` for
questions and subquestions alike, `answer_l10ns`,
`quota_languagesettings`) receives one row per language, grouped by
entity as a real LimeSurvey export groups them.
[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
already requires every declared language for every text, so no
translation can go missing at emission.

The `<languages>` element lists the additional languages first and the
base language last, the order LimeSurvey itself writes. It is only a
membership set: read the base language from `read_lss()$base_language`,
never from `read_lss()$languages[1]`.

Localized question attributes – `other_replace_text`, `prefix`,
`suffix`, `choice_title`, `rank_title`, `printable_help`, ... – are
emitted once per language, each row carrying its language code; without
it LimeSurvey silently ignores the attribute and shows its own default
wording. An attribute passed through `question$attributes` under one of
those names is treated the same way: a plain string is repeated in every
language, a value keyed by language code is resolved language by
language (`attributes = list(prefix = "CHF")` or
`list(choice_title = c(fr = "Choix", en = "Choice"))`). Every other
attribute stays language-less, as LimeSurvey stores it.

Per-language survey settings work the same way: `settings` accepts a
single value repeated in every `surveys_languagesettings` row, or a list
keyed by language code for the fields LimeSurvey genuinely varies –
`list(surveyls_dateformat = c(fr = "5", en = "2"))` gives the French
respondent a `dd.mm.yyyy` date picker and the English one `mm/dd/yyyy`.
A declared language missing from such a value is an error, not a silent
fallback.

A quota is localized through `quotals_name` and `quotals_message`, one
row per language. The administration-side label `quota.name` has a
single column in LimeSurvey and therefore keeps the primary-language
wording.

## See also

[`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md),
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md),
[`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md),
[`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md).

## Examples

``` r
spec <- lss_spec(
  title = "Demo",
  groups = list(list(title = "G", questions = list(
    list(code = "q1", kind = "single", text = "Oui ou non ?",
         options = list(list(text = "Oui"), list(text = "Non")))
  )))
)
out <- tempfile(fileext = ".lss")
write_lss(spec, out)
#> ✔ Wrote /tmp/RtmpIpXP1P/file194a492aa8f3.lss (1 question, 1 group, 0 quotas).
audit_lss(out)
#> 
#> ── lssdoc audit ────────────────────────────────────────────────────────────────
#> File: /tmp/RtmpIpXP1P/file194a492aa8f3.lss
#> Languages: "fr"
#> ✔ No anomalies detected.
```

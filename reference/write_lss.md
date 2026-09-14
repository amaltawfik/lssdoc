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

  Named list of `surveys`-table fields overriding the built-in defaults
  (e.g. `list(anonymized = "Y", showprogress = "Y")`). The defaults ship
  with the package and come from a real LimeSurvey 6 export, scrubbed –
  see `lss_default_surveys_fields` in the sources for the rationale.

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

The spec model is multilingual
([`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md)
accepts `languages` and per-language texts); the emitter is not yet.
This version writes the primary language – `languages[1]` – only,
exactly as it did when a spec could hold a single language. A spec
declaring more than one language raises a classed error
(`lssdoc_unsupported_multilang`) rather than silently dropping the
translations; multi-language emission is planned for 0.3.0.

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
#> ✔ Wrote /tmp/RtmpxbRXCv/file1a0f27341ef4.lss (1 question, 1 group, 0 quotas).
audit_lss(out)
#> 
#> ── lssdoc audit ────────────────────────────────────────────────────────────────
#> File: /tmp/RtmpxbRXCv/file1a0f27341ef4.lss
#> Languages: "fr"
#> ✔ No anomalies detected.
```

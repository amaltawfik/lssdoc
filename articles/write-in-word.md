# Write a questionnaire in Word

``` r

library(lssdoc)
```

Many questionnaires are written by people who live in Word, not in R:
researchers, methodologists, translators, ethics committees. lssdoc lets
them write the questionnaire in a **Word form** and turns that form into
a LimeSurvey `.lss` file ready to import. Nothing is typed twice,
nothing is uploaded anywhere, and every mistake is reported before
LimeSurvey ever sees the file.

The workflow has four steps and four functions:

1.  [`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md)
    writes a blank form.
2.  The author fills it in, in Word.
3.  [`check_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/check_form_docx.md)
    reports every problem in the filled form;
    [`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md)
    turns it into a specification once it is clean.
4.  [`write_lss()`](https://amaltawfik.github.io/lssdoc/reference/write_lss.md)
    writes the `.lss` file, which you import into LimeSurvey.

An existing survey can enter the loop too:
[`as_lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/as_lss_spec.md)
converts a survey read with
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
into a specification, and
[`write_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/write_form_docx.md)
renders any specification as the form, so a questionnaire can be
exported from LimeSurvey, edited in Word and re-imported.

The form functions are **experimental**: their interface may still
change, and the form’s own contract is versioned (see *What the reader
refuses*). Reading a form needs only the packages lssdoc imports;
writing one needs the suggested packages officer and flextable.

## 1. Get a blank form

[`lss_template_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_template_docx.md)
writes a form with one example question per question type, so the author
sees every field a type can carry. The `lang` argument sets the language
of the labels and hints — English, French, German, Spanish or Italian —
independently of the questionnaire’s own languages.

``` r

template <- tempfile(fileext = ".docx")
lss_template_docx(template, lang = "en")
#> ✔ Wrote /tmp/Rtmp67uvIt/file1c3dad2a53.docx (21 questions, 2 groups, 1 quota).
```

The person who writes the questionnaire often does not use R at all. The
same blank forms can be downloaded from the [package
website](https://amaltawfik.github.io/lssdoc/): a short one to fill in,
in each of the five interface languages, and a longer one showing every
question type. They are regenerated whenever the site is built, so the
form offered there is always the one the current version reads.

Open the file in Word. It is a sequence of two-column tables, one per
block, in the order of the questionnaire:

| Block | Rows |
|----|----|
| **Survey** (once, first) | Title, Languages, Welcome text, End text |
| **Group** | Title, Description |
| **Question** | Type, Mandatory, Filter, Wording, Help, then the rows the type needs: Options, Exclusive, Rows, Columns, Min. answers, Max. answers, Position of “Other” |
| **Quota** | Name, Limit, Action, Condition, Message |

The first row of every table names the block; for a question it also
carries the **question code** — the variable name in the exported data.
The left column holds the field labels, the right column is yours. In
the blank form, a muted hint under each label recalls the syntax, and
defaults are already filled in (a question is optional, always shown,
without an “Other” option).

To add a question, copy a Question table and paste it after the empty
line that follows a block; to remove one, delete its table. Blocks can
also be reordered by moving tables.

## 2. Fill it in

The form is plain text: no HTML, no formatting. A handful of conventions
cover every questionnaire.

**One value per line.** Options, rows and columns take one entry per
line — press Enter (or Shift+Enter) between them. Each entry is either
`code = Label` or a bare `Label`, in which case codes are numbered
automatically (1, 2, 3, …). Codes are variable values in the data file,
so choose them deliberately when they matter.

A line is split at its **first** `=`, and only when what precedes it is
a single word: `1 = Fees = 0` is the code `1` with the label `Fees = 0`,
and a label may contain as many further `=` as it likes. A sentence like
`Salary = market rate` has spaces on the left, so it is not a code at
all: the whole line is the label, and it is numbered automatically. If
the left side has no spaces but is not a valid code — letters and digits
only, at most 20 characters — the line is reported rather than quietly
taken as a label.

    1 = Never
    2 = Sometimes
    3 = Often
    4 = Always

**The “Other” option.** The reserved word `Other` (or `Autre`,
`Sonstiges`, `Otro`, `Altro`) alone in the Options adds LimeSurvey’s own
free-text “Other” option; `Other = Please specify` gives it a label. An
ordinary option that happens to be called Other needs an explicit code:
`9 = Other`.

**Type.** The question type is one of the codes in the table below
(`single`, `multiple`, `array`, …). Each type decides which rows the
block carries; extra rows are ignored, missing required rows are
reported.

**Mandatory.** `yes` or `no` in any of the five label languages (also
`y`/`n`, `true`/`false`, `1`/`0`). Empty means no.

**Filter.** When the question should be shown only in some cases, write
the condition in the small language of section 4. Empty means always
shown.

**Several languages.** Declare them in the Survey block,
comma-separated, primary language first: `fr, en`. Every text field is
then labelled once per language — `Wording [fr]`, `Wording [en]` — and
every language is required for every text, because a questionnaire with
a missing translation is exactly what the audit exists to catch. With a
single language, labels carry no suffix.

## 3. Question types

Write the **Type** value of the first column in the Type row of a
question block. The other columns say which rows that type accepts:
whether it takes Options, Rows and Columns, how many options it needs at
least, whether it can carry an “Other” option or an exclusive one, and
the codes it already has built in (a yes/no question does not need
options: its codes are `Y` and `N`).

This table is generated from the package’s own table of types, so it
cannot disagree with what
[`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md)
accepts.

A blank cell means the type does not use that row: leave it out, and the
blank form will not show it in the first place.

| Type | Meaning | Options | Rows | Columns | Min. | Other | Exclusive | Built-in codes |
|:---|:---|:---|:---|:---|:---|:---|:---|:---|
| single | Single choice (radio) | yes |  |  | 2 | yes |  |  |
| dropdown | Single choice (dropdown) | yes |  |  | 2 | yes |  |  |
| singlecomment | Single choice with comment | yes |  |  | 2 |  |  |  |
| multiple | Multiple choice | yes |  |  | 2 | yes | yes |  |
| array | Array (rows and columns) |  | yes | yes |  |  |  |  |
| array5 | Array, 5-point scale |  | yes |  |  |  |  | 1, 2, 3, 4, 5 |
| array10 | Array, 10-point scale |  | yes |  |  |  |  | 1, 2, 3, 4, 5, 6, 7, 8, 9, 10 |
| arrayyesno | Array, yes/no/uncertain |  | yes |  |  |  |  | Y, N, U |
| arraytrend | Array, increase/same/decrease |  | yes |  |  |  |  | I, S, D |
| ranking | Ranking | yes |  |  | 2 |  |  |  |
| multitext | Multiple short texts | yes |  |  | 1 |  |  |  |
| multinumeric | Multiple numeric inputs | yes |  |  | 1 |  |  |  |
| text | Long free text |  |  |  |  |  |  |  |
| shorttext | Short free text |  |  |  |  |  |  |  |
| hugetext | Huge free text |  |  |  |  |  |  |  |
| numeric | Numeric input |  |  |  |  |  |  |  |
| date | Date |  |  |  |  |  |  |  |
| yesno | Yes/no |  |  |  |  |  |  | Y, N |
| gender | Gender |  |  |  |  |  |  | M, F |
| fivepoint | Five-point choice |  |  |  |  |  |  | 1, 2, 3, 4, 5 |
| display | Text display |  |  |  |  |  |  |  |

Internally, LimeSurvey identifies a question type by a **single
character** stored in the survey file, and several of those characters
are punctuation: `!` is the dropdown list, `1` a dual scale, `:` an
array of numbers, `;` an array of texts. You never type those in the
form — the package translates them — but they appear in the table
returned by `lssdoc:::lss_kinds_reference()`, alongside the LimeSurvey
theme name, for anyone comparing a form with a raw export.

Defaults shared by all types:

| setting          | default |
|:-----------------|:--------|
| mandatory        | FALSE   |
| relevance        | 1       |
| other            | FALSE   |
| exclusive        | FALSE   |
| option_code_from | 1       |
| language         | fr      |
| primary_language | 1       |

Some LimeSurvey types are deliberately not supported yet, each for a
concrete reason (usually a storage mechanism the package has not yet
seen in a real export). They are listed with their reason:

| type | label | reason |
|:---|:---|:---|
| P | Multiple choice with comments | Subquestion mechanism identical to `multiple`, but its LS6 theme name is not attested. |
| H | Array by column | Rows-and-columns shape identical to `array` (F), but its LS6 theme name is not attested. |
| 1 | Array (dual scale) | Needs two answer scales (scale_id 0 and 1), which the emitter does not write. |
| ; | Array (texts) | Needs dual-scale subquestions (the text columns live on scale_id 1): unproven mechanism. |
| : | Array (numbers) | Same unproven dual-scale subquestion mechanism as `;`. |
| \* | Equation | Collects no response and carries its formula in an attribute whose LS6 theme name is not attested. |
| \| | File upload | Needs the file-upload attribute family (max size, allowed types), none of it attested. |
| I | Language switch | Not a question: a language selector, with no attested LS6 theme name. |

## 4. Filters

A filter is a condition on an earlier question. Three forms are
understood, and only these three, because they are the ones LimeSurvey
handles without surprises:

| Form | Meaning |
|----|----|
| `Q1 = 2` | Q1 was answered with code 2 |
| `Q1 in [1, 3, autre]` | Q1 was answered with code 1, code 3 or the “Other” option |
| `count(Q4) >= 2` | at least two options of the multiple-choice question Q4 are ticked |

`autre` (in any of the five languages) is the only way to refer to the
“Other” option; codes are compared exactly as written. The target must
be defined **before** the question that uses it, and must be a
single-choice question for `=` and `in`, a multiple-choice question for
`count()` — anything else is reported. Quota conditions use the `=` form
only.

## 5. Check the form, then read it

[`check_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/check_form_docx.md)
is a dry run: it reads the whole form and reports **every** problem at
once — the block, the question code and the field are named each time —
so the author fixes them in one pass instead of discovering them one by
one.

``` r

check_form_docx(template)
#> 
#> ── lssdoc form check ───────────────────────────────────────────────────────────
#> File: /tmp/Rtmp67uvIt/file1c3dad2a53.docx
#> ✔ No problems found: the form reads.
```

Problems come in a few families, each with its own condition class so
they can be caught programmatically:

| Class | Typical cause |
|----|----|
| `lssdoc_bad_form_file` | not a Word file, or not a form |
| `lssdoc_bad_form_marker` | the form’s version marker is missing (the document was recreated, or the tables were pasted into another document) or belongs to another version of the package |
| `lssdoc_bad_form_layout` | merged cells, a table that is not two columns, a nested table, tracked changes, an automatic list |
| `lssdoc_bad_form_block` | a table that does not start with a block title, a question before any group, two Survey blocks |
| `lssdoc_bad_form_key` | an unknown or duplicated field label |
| `lssdoc_bad_form_value` | a value the field cannot take: unknown type, bad option line, missing language |
| `lssdoc_bad_form_spec` | the form is well-formed but the questionnaire is not (the same checks [`lss_spec()`](https://amaltawfik.github.io/lssdoc/reference/lss_spec.md) applies) |

[`read_form_docx()`](https://amaltawfik.github.io/lssdoc/reference/read_form_docx.md)
applies the same checks and stops at the first problem; when the form is
clean it returns the specification:

``` r

spec <- read_form_docx(template)
spec
#> <lss_spec> "lssdoc example questionnaire" (en)
#> 2 groups, 20 questions, 1 quota
```

## 6. Write the LimeSurvey file

``` r

lss_file <- tempfile(fileext = ".lss")
write_lss(spec, lss_file)
#> ✔ Wrote /tmp/Rtmp67uvIt/file1c3d13d774d0.lss (20 questions, 2 groups, 1 quota).
```

Import the file in LimeSurvey (*Surveys → Create → Import*). All
declared languages are written, including the labels of “Other” options
and every other localized attribute, which LimeSurvey silently drops
when the language is missing — the package emits them the way a real
export does.

Two habits make the loop safe. First, export the survey back from
LimeSurvey and read it with
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
and
[`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md):
what you get back is what respondents will see.

``` r

back <- read_lss(lss_file)
audit_lss(back)
#> 
#> ── lssdoc audit ────────────────────────────────────────────────────────────────
#> File: /tmp/Rtmp67uvIt/file1c3d13d774d0.lss
#> Languages: "en"
#> ✔ No anomalies detected.
```

Second, keep the filled form: it is the source of the questionnaire, and
re-running the four steps after an edit is cheaper than editing in
LimeSurvey.

## 7. Edit an existing survey

A survey already in LimeSurvey can be brought into the same loop. Export
it, read it, convert it, render the form:

``` r

lss <- read_lss(system.file("extdata", "demo_survey.lss", package = "lssdoc"))
form <- tempfile(fileext = ".docx")
write_form_docx(lss, form, lang = "en", strict = FALSE)
#> Warning: Dropped 5 items `lss_spec()` cannot express.
#> ✖ imc -- type: Equation (type "*") is not authorable: Collects no response and
#>   carries its formula in an attribute whose LS6 theme name is not attested.
#> ✖ adaptability -- type: Array (numbers) (type ":") is not authorable: Same
#>   unproven dual-scale subquestion mechanism as `;`.
#> ✖ trustinstitutions -- type: Array (dual scale) (type "1") is not authorable:
#>   Needs two answer scales (scale_id 0 and 1), which the emitter does not write.
#> ✖ mediaquality -- type: Array (texts) (type ";") is not authorable: Needs
#>   dual-scale subquestions (the text columns live on scale_id 1): unproven
#>   mechanism.
#> ✖ quota fin -- quota: it combines 2 question(s); the spec expresses a quota on
#>   exactly one
#> ℹ The specification describes the rest of the survey.
#> Warning: Converting this survey into an `lss_spec()` is lossy.
#> ! HTML flattened to plain text in 3 fields: "survey welcome text", "survey end
#>   text", and "text of question thankyou".
#> ! question respondentrole: the "other" option carries no label; the default
#>   wording is used
#> ! question nationality: the "other" option carries no label; the default
#>   wording is used
#> ! quota fin2: its end URL is dropped (the spec has no URL field)
#> ! survey settings datestamp, usecookie, allowsave, autoredirect, printanswers,
#>   showsurveypolicynotice and 41 more have no place in a specification;
#>   write_lss() re-emits its own defaults
#> ℹ Review the result, or fix the survey in the Word authoring form.
#> ✔ Wrote /tmp/Rtmp67uvIt/file1c3d1c1cd377.docx (43 questions, 6 groups, 1 quota).
```

The conversion is honest about its limits. Question types the form does
not support and filters written in LimeSurvey’s full expression language
cannot be expressed in the form: by default (`strict = TRUE`) the
conversion refuses and lists every such item; with `strict = FALSE` it
drops them and warns, naming each one, so nothing disappears silently.
HTML in texts is turned into plain text, and a translation missing in
the export is filled from the primary language — both are reported, and
both are things the author can then fix in Word.

## What the reader tolerates, and what it refuses

The reader is built for people who type in Word, not for people who know
XML. It accepts what Word does on its own: a capital letter at the start
of a cell (`Single`, `Oui`, `Count(Q4) >= 2`), curly quotes, a
non-breaking space, a label typed as `Type:` or with quotation marks,
labels in any of the five languages. It never changes the case of a
code: `A` stays `A`.

It refuses, with a precise message, what it cannot read safely: merged
cells, tables with more or fewer than two columns, a table inside a
cell, content controls, tracked changes, and Word’s automatic numbering
applied to an option list (type `1 = Label`, not `1. Label`, or turn
automatic lists off). And it checks a version marker stored in the
document’s properties: a form written by one version of the package is
read by that version’s rules, and a document recreated by hand is
refused rather than misread.

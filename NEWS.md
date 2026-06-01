# lssdoc 0.1.0

First release. lssdoc reads a LimeSurvey `.lss` export and renders a
multilingual questionnaire document (Word `.docx` or PDF) for review by
ethics committees, methodologists, researchers and translators, together
with a rule-based audit of the survey. The user-facing API is four
functions: `read_lss()`, `audit_lss()`, `render_questionnaire()` and
`render_audit()`. All processing stays on the user's machine. Tested
across two example surveys, both templates (`cards` and `table`), the
five chrome languages (English, French, German, Spanish, Italian) and
both output formats.

## Reading

* `read_lss()` reads a LimeSurvey `.lss` file into a structured `lss`
  object, preserving all user text verbatim. It validates the `.lss`
  `DBVersion` against the tested window (400 to 799): a version below
  400 (pre `*_l10ns` schema) raises a classed error, a version of 800
  or above warns but still parses.
* `audit_lss(input)` and the two renderers all accept either a path to
  a `.lss` file or a pre-parsed `lss` object as `input`.

## Audit

* `audit_lss()` inspects a parsed survey and reports reviewable
  anomalies as a classed `lss_audit` object with a print method:
  missing translations, texts empty in every language, duplicate
  question or answer codes, types missing their required options or
  subquestions, and orphan references.
* It also flags **forward filter references** -- a routing condition
  that depends on a variable appearing at or after the filtered
  question, almost always a survey-design bug (severity: error);
  **array-scale inconsistencies** -- an array whose subquestions and
  answer options use mismatched `scale_id` values (severity: warning);
  and **whitespace in identifier codes** -- a question, subquestion or
  answer code carrying leading, trailing or interior whitespace, which
  causes silent lookup failures at data export (severity: warning).
* `print(audit)` paginates output at 20 findings by default, with a
  hint to use `print(audit, n = Inf)` or `as.data.frame(audit)` to see
  them all.

## Rendering

* `render_questionnaire()` produces a Word or PDF questionnaire document
  from a path or a parsed `lss`: a cover page with the survey title in
  every language and a metadata table, a table of contents, an optional
  audit section near the top with inline markers on affected questions,
  group sections, and one compact block per question with a meta header
  (number, variable code, type, mandatory, filter) and one column per
  language for the question text, subquestions and answer options. The
  output format (`.docx` or `.pdf`) is inferred from the `output`
  extension; PDF is produced locally via LibreOffice. The page
  orientation auto-picks portrait (1-2 languages) or landscape (3+).
* `render_audit()` produces a focused audit-only document: the same
  cover page, then one section per severity (errors, warnings, notes)
  with a table of findings. Use it for QA follow-up, separate from the
  full review.
* Two output templates:
  * `"cards"` (default) -- a meta band plus item table per question,
    stacked vertically, reading as a questionnaire.
  * `"table"` -- one dense codebook table covering the whole document,
    with one tinted Question row per variable and one or more Value
    rows beneath.
* `chrome_lang` controls the document chrome (column headers, row
  labels, audit-section labels) independently of the survey content
  languages. Supported: `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`.

## Cover and metadata

* The cover shows the LimeSurvey **Survey ID** and the last-save
  timestamp for stable traceability.
* Optional `logo` places a PNG or JPEG image at the top of the cover
  page (default keeps the cover logo-free, matching the neutral style
  of survey-methodology references).
* Optional `authors` supports an unnamed character vector, a named
  character vector (name = affiliation), or a list of named lists
  (`name`, `affiliation`, `orcid`). ORCID iDs render as monospace
  hyperlinks.
* Optional `description` is shown below the authors block. Line breaks
  (`\n`) split into separate centered lines; `http://` and `https://`
  tokens render as clickable hyperlinks.
* Multilingual `welcome` text, `endtext` and `description` are
  toggleable via `show_welcome`, `show_endtext`, `show_description`
  (all `TRUE` by default).
* Optional `show_privacy_settings` and `show_admin_settings` surface
  LimeSurvey flags (`anonymized`, save partial, `datestamp`, `ipaddr`,
  `refurl`, `alias`, end URL, `active`) as additional cover metadata
  rows (both default `FALSE`).
* `show_toc`, `show_index` and `show_header_title` (all `TRUE`) add a
  group table of contents (skipped when there are fewer than two
  groups), an alphabetical variable index listing every item code with
  its number, and the survey title at the top of every page. The page
  footer always carries a compact `X/Y` page counter.
* `show_quotas` (default `TRUE`) appends a quotas section after the end
  text: one block per sampling quota with its localized name, status
  (active, limit, action when full), the membership condition resolved
  to question codes and answer labels, and the localized "quota full"
  message. Skipped when the survey defines no quotas.

## Filter rendering

* The `Filter` cell reads in the chosen chrome language -- `AND`, `OR`,
  `is answered`, `is empty` and `matches` localize alongside the rest
  of the chrome.
* The humanizer translates LimeSurvey's defensive
  `!is_empty(X.NAOK) && (X.NAOK OP value)` idiom to `X OP value`. It
  collapses several disjunctions on one variable to set notation
  (`Q1 = 1 OR Q1 = 2 OR Q1 = 3` reads as `Q1 in {1, 2, 3}`) and two
  bounds to an encased range (`age >= 18 AND age <= 65` reads as
  `18 <= age <= 65`, with Unicode math symbols in the output). It also
  unwraps `intval(X.NAOK)`, maps `strlen(X.NAOK)` predicates to "is
  answered" / "is empty", renders `regexMatch("pat", X.NAOK)` as
  `X matches "pat"`, and drops `that.X` group prefixes.
* `show_raw_filter` defaults to `FALSE`: the `Filter` cell shows only
  the human-readable form (e.g. `Q1 = 1`), matching ESS / MOSAiCH /
  GESIS conventions. Pass `show_raw_filter = TRUE` to also surface the
  raw LimeSurvey expression underneath.

## Typography and layout

* The cards body renders at one uniform type size with a two-tone meta
  band and cell-symmetric alignment per column (each column's header
  takes the same alignment as its body). The meta table, intra-item gap
  and item table carry Word's "keep with next" property so they stay on
  the same page in the typical case.
* In the table template the language columns get at least 50% of the
  page width, the `Mandatory` header abbreviates in the dense layout
  (`Mand.` / `Oblig.` / `Pflicht` / `Obblig.`), and the body font steps
  down (8 pt to 7 pt) when rendering 3 or 4 languages so the narrower
  columns stay readable.
* The package ships a hex logo (`man/figures/logo.png` and `.svg`) and
  a favicon set for the pkgdown site.

## Quality

* Continuous integration runs the full render suite; a Codecov coverage
  badge in the README is wired through
  `.github/workflows/test-coverage.yaml`.

# Changelog

## lssdoc 0.1.0

First release. The user-facing API is four functions and is now stable:
[`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md),
[`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md),
[`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md),
[`render_audit()`](https://amaltawfik.github.io/lssdoc/reference/render_audit.md).
Tested across two example surveys, both templates (`cards` and `table`),
the five chrome languages (English, French, German, Spanish, Italian)
and the two output formats (Word `.docx` and PDF).

### Breaking changes

- The user-facing API is now four functions:
  [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md),
  [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md),
  [`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md)
  and
  [`render_audit()`](https://amaltawfik.github.io/lssdoc/reference/render_audit.md).
  The previous nine-function surface (`parse_lss()`,
  `render_lss_docx()`, `render_lss_audit_docx()`, `lss_to_docx()`,
  `lss_to_pdf()`, `lss_audit_to_docx()`, `lss_audit_to_pdf()`,
  `lss_docx_to_pdf()`) is gone – no deprecation shims, since the package
  had not yet been released. The two renderers infer the output format
  (`.docx` or `.pdf`) from the extension of the `output` argument, and
  `input` accepts either a path or a pre-parsed `lss` object.

- `show_raw_filter` now defaults to `FALSE`. The `Filter` cell shows
  only the human-readable form (e.g. `Q1 = 1`) by default, matching ESS
  / MOSAiCH / GESIS conventions. Pass `show_raw_filter = TRUE` to also
  surface the raw LimeSurvey expression underneath.

- `show_attrs` no longer surfaces the two row-level exclusivity flags
  (`exclude_all_others`, `exclude_all_others_auto`) by default. Add them
  explicitly if you need them.

- The cover subtitle is now a single localized noun (`Questionnaire` /
  `Fragebogen` / `Cuestionario` / `Questionario`) instead of the
  previous long “Revue du questionnaire LimeSurvey” / “LimeSurvey
  questionnaire review” family.

### New features

#### Audit

- [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  now flags **forward filter references** – a routing condition that
  depends on a variable appearing at or after the filtered question,
  which is almost always a survey design bug (severity: error).

- [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  flags **array-scale inconsistencies** – an array question whose
  subquestions and answer options use mismatched `scale_id` values
  (severity: warning).

- [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  flags **whitespace in identifier codes** – a question, subquestion or
  answer code carrying leading, trailing or interior whitespace, which
  causes silent lookup failures at data export (severity: warning).

- `print(audit)` paginates output at 20 findings by default with a hint
  to use `print(audit, n = Inf)` or `as.data.frame(audit)` to see them
  all.

#### Filter rendering

- The `Filter` cell now reads in the chosen chrome language – `AND`,
  `OR`, `is answered`, `is empty` and `matches` localize alongside the
  rest of the chrome (English, French, German, Spanish, Italian).

- Several disjunctions on the same variable collapse to set notation:
  `Q1 = 1 OR Q1 = 2 OR Q1 = 3` reads as `Q1 in {1, 2, 3}`.

- Two bounds on the same variable collapse to an encased range:
  `age >= 18 AND age <= 65` reads as `18 <= age <= 65` (with Unicode
  math symbols in the actual output).

- More LimeSurvey constructs are humanized: `intval(X.NAOK)` is
  unwrapped, `strlen(X.NAOK)` predicates map to “is answered” / “is
  empty”, `regexMatch("pat", X.NAOK)` becomes `X matches "pat"`, and
  `that.X` (group references) lose the structural prefix.

#### Table template

- The language columns now get at least 50 % of the page width. The
  `Mandatory` header uses an abbreviation in the dense table layout
  (`Mand.` / `Oblig.` / `Pflicht` / `Obblig.`); the cards layout keeps
  the full localized word.

- Body font size reduces automatically (8 pt to 7 pt) when rendering 3
  or 4 languages so the narrower columns stay readable.

#### Cards template

- Headers and bodies use a cell-symmetric alignment per column (each
  column’s header takes the same alignment as its body content), so the
  dark meta band reads as a coherent ribbon of labels over its content.

- The meta table, intra-item gap and item table now carry Word’s “keep
  with next” property so they stay on the same page in the typical case.

### Minor improvements

- [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  validates the `.lss` `DBVersion` against the tested window (400 to
  799). A version below 400 (pre `*_l10ns` schema) raises a classed
  error; a version of 800 or above warns but parses.

- `audit_lss(input)` and the two renderers all accept either a path to a
  `.lss` file or a pre-parsed `lss` object as `input`.

- Large surveys are faster to model: `lss_model()` now uses an O(1)
  index for localized lookups instead of a full table scan per question.

- Codecov coverage badge in the README;
  `.github/workflows/test-coverage.yaml` wires the upload.

### Internal

- `R/render_lss_docx.R` (3110 lines) is split into three self-contained
  sub-modules: `R/render_filter.R` (humanizer), `R/render_theme.R`
  (theme and argument validators) and `R/render_cover.R` (cover
  orchestration). No functional change.

## lssdoc 0.0.0.9000

### Initial development scaffolding

- [`read_lss()`](https://amaltawfik.github.io/lssdoc/reference/read_lss.md)
  reads a LimeSurvey `.lss` file into a structured `lss` object,
  preserving all user text verbatim.

- [`audit_lss()`](https://amaltawfik.github.io/lssdoc/reference/audit_lss.md)
  inspects a parsed survey and reports reviewable anomalies (missing
  translations, texts empty in every language, duplicate question or
  answer codes, types missing their required options or subquestions,
  and orphan references) as a classed `lss_audit` object with a print
  method.

- [`render_questionnaire()`](https://amaltawfik.github.io/lssdoc/reference/render_questionnaire.md)
  produces a professional Word questionnaire document from a parsed
  `lss` object: cover page with the survey title in every language and a
  metadata table, table of contents, optional audit section near the top
  with inline markers on affected questions, group sections, one compact
  `flextable` per question with a single meta header (variable code,
  QID, type, mandatory, filter) and one column per language for the
  question text, subquestions and answer options. Auto-picks portrait
  (1-2 languages) or landscape (3+).

- [`render_audit()`](https://amaltawfik.github.io/lssdoc/reference/render_audit.md)
  produces a focused audit-only Word document: the same cover page, then
  one section per severity (errors, warnings, notes) with a table of
  findings. Use it for QA follow-up, separate from the full review.

- Cover page shows the LimeSurvey **Survey ID** and the LimeSurvey
  last-save timestamp for stable traceability.

- Optional `logo` argument places a PNG or JPEG image at the top of the
  cover page (default keeps the cover logo-free, matching the neutral
  style of survey-methodology references).

- Optional `authors` argument supports an unnamed character vector, a
  named character vector (name = affiliation), or a list of named lists
  (`name`, `affiliation`, `orcid`). ORCID iDs render as monospace
  hyperlinks.

- Optional `description` argument shown on the cover below the authors
  block. Line breaks (`\n`) split into separate centered lines;
  `http://` and `https://` tokens render as clickable hyperlinks.

- Multilingual `welcome` text, `endtext` and `description` toggleable
  via `show_welcome`, `show_endtext`, `show_description` (all `TRUE` by
  default).

- Optional `show_privacy_settings` and `show_admin_settings` surface
  LimeSurvey flags (`anonymized`, `save partial`, `datestamp`, `ipaddr`,
  `refurl`, `alias`, end URL, `active`) as additional cover metadata
  rows. Both default to `FALSE`.

- `show_toc` (default `TRUE`) includes a table of contents listing the
  groups (skipped automatically when there are fewer than two groups).

- `show_index` (default `TRUE`) appends a variable index listing every
  item code with its number, sorted alphabetically.

- `show_header_title` (default `TRUE`) shows the survey title on every
  page top-right. The page footer carries a compact `X/Y` page counter
  on the right.

- Two output templates:

  - `"cards"` (default) – one detached meta table + item table pair per
    question, stacked vertically.
  - `"table"` – one dense codebook table covering the whole document,
    with one tinted Question row per variable and one or more Value rows
    beneath.

- `chrome_lang` controls the document chrome (column headers, row
  labels, audit section labels) independently of the survey content
  languages. Supported: `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`.

- The filter humanizer translates LimeSurvey’s defensive
  `!is_empty(X.NAOK) && (X.NAOK OP value)` idiom to `X OP value` for
  human review.

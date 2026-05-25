# lssdoc 0.0.0.9000

* New navigation layer for the rendered document:
  - **Table of contents** now lists groups only, not items: a 95-item
    survey produces a clean 5-line TOC instead of a 95-line list.
    Controlled by `show_toc` (default `TRUE`), skipped automatically when
    the survey has fewer than two groups.
  - **Variable index** appended at the end of the document: every item
    code with its sequential number, sorted alphabetically, so the
    reader can look up a specific variable. Controlled by `show_index`
    (default `TRUE`).
  - **Page footer** redesigned: survey title (per displayed language,
    joined by ` | `) on the left, `X / Y` page number on the right.
    Controlled by `show_footer_title` (default `TRUE`); the page number
    is always shown.
* Items are now rendered as styled paragraphs with a manual sequential
  prefix (`12. variable`) instead of Heading 1 paragraphs. Groups
  become Heading 1 so the TOC reads as a list of sections. Word's
  auto-numbering of Heading 1 thus increments per group, not per item;
  the manual prefix on items preserves the flat per-item numbering.
* The filter humanizer now collapses LimeSurvey's defensive
  `!is_empty(X.NAOK) && (X.NAOK OP value)` idiom into the equivalent
  human-readable form `X OP value`. The LimeSurvey conditional designer
  emits this guard automatically for every comparison; for a human
  reviewer it is redundant boilerplate. The collapse only applies when
  both clauses reference the **same** variable (so a real two-variable
  filter like `!is_empty(a.NAOK) && (b.NAOK == 1)` is preserved as
  `a is answered AND (b = 1)`).
* The meta description of every question is now a **structured 5-column
  table** (`No` / `Variable` / `Type` / `Oblig.` / `Filter`) instead of
  the previous colored band string. The `Variable` header replaces the
  former `Code` label, matching the SPSS / Stata convention. The Filter
  cell shows a human-readable form of the LimeSurvey relevance
  expression on top (best-effort: `is_empty(X.NAOK)` -> `X is empty`,
  `!is_empty(X.NAOK)` -> `X is answered`, `X.NAOK == N` -> `X = N`, `&&`
  / `||` -> `AND` / `OR`); when the new `show_raw_filter` argument is
  `TRUE` (the default), the raw expression is shown underneath in small
  italic gray for verification. Parent stems of compound questions show
  a 4-column variant (no `No`, since their subquestions below carry the
  numbering).
* Answer scale tables now label the code column **`Value`** (rather than
  `Code`), in line with the variable/value convention.
* The rendered document is now organized **item-by-item** (ESS / MOSAiCH
  style) rather than question-by-question. Each leaf question becomes one
  numbered item with its own answer scale; for compound questions
  (multiple choice, arrays, multiple numerical, etc.), the parent stem is
  shown once in a colored band, the shared answer scale once below it,
  and each subquestion becomes its **own numbered item** with the full
  LimeSurvey response variable code (`parent_subq`). The Word table of
  contents now lists items sequentially across the whole document; groups
  remain visible as section banners but are not numbered. The redundant
  "Francais / Deutsch" header that previously repeated above every small
  table is dropped; the language column header is kept only where it adds
  information (the shared answer scale, leaf-item answer tables).
* Every page now carries a centered "Page X of Y" footer (Word fields, so
  the totals update on open).
* All flextables in the rendered document are horizontally centered
  rather than left-aligned.
* The Word table-of-contents entry for each question now combines the
  variable code with the question text in the **first requested
  language**, making the TOC readable in that language while keeping the
  variable code as a stable cross-reference anchor. The `languages`
  argument is documented as both a selector and an ordering: the first
  language is treated as the primary language for headings and the TOC.
* `render_lss_docx()` and `render_lss_audit_docx()` accept an optional
  `logo` argument (path to a PNG or JPEG) that places an image at the top
  of the cover page; `logo_width` and `logo_height` control its size. The
  default keeps the cover logo-free, matching the neutral style of
  survey-methodology references.
* The cover page now shows the LimeSurvey **Survey ID** and **Last
  modified** timestamp from the `.lss`, giving reviewers stable
  traceability for the source questionnaire.
* `lss_audit_to_docx()` and `lss_audit_to_pdf()` pipeline wrappers run the
  audit and produce a focused report in one call.
* `lss_docx_to_pdf()` converts a generated `.docx` to PDF locally via
  LibreOffice (or Word) in headless mode. Nothing leaves the user's
  machine. `lss_to_pdf()` ties the full pipeline together.
* `lss_to_docx()` runs the full pipeline (`parse_lss()` then
  `render_lss_docx()`) in one call.
* `render_lss_audit_docx()` produces a focused audit-only Word document:
  the same cover page, then one section per severity (errors, warnings,
  notes) with a table of findings. Use it for QA follow-up, separate from
  the full review.
* `render_lss_docx()` produces a professional Word review document from a
  parsed `lss` object: cover page with the survey title in every language
  and a metadata table, table of contents, optional audit section near the
  top with inline markers on affected questions, group sections (Word
  Heading 1/2 styles for navigation), one compact `flextable` per question
  with a single meta header (variable code, QID, type, mandatory, filter)
  and one column per language for the question text, subquestions, and
  answer options. Auto-picks portrait (1-2 languages) or landscape (3+).
* `audit_lss()` inspects a parsed survey and reports reviewable anomalies
  (missing translations, texts empty in every language, duplicate question
  or answer codes, types missing their required options or subquestions, and
  orphan references) as a classed `lss_audit` object with a print method.
* `parse_lss()` reads a LimeSurvey `.lss` file into a structured `lss`
  object, preserving all user text verbatim.
* Initial package scaffolding: the user-facing API (`parse_lss()`,
  `audit_lss()`, `render_lss_docx()`, and the `lss_to_docx()` wrapper) is
  defined and documented, with `render_lss_docx()` still to come.

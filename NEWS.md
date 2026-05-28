# lssdoc 0.0.0.9000

* **Public API simplified to four functions.** The package now
  exposes only `read_lss()`, `audit_lss()`, `render_questionnaire()`,
  and `render_audit()`. The previous nine-function surface
  (`parse_lss()`, `render_lss_docx()`, `render_lss_audit_docx()`,
  `lss_to_docx()`, `lss_to_pdf()`, `lss_audit_to_docx()`,
  `lss_audit_to_pdf()`, `lss_docx_to_pdf()`) has been collapsed:
  * `parse_lss()` -> `read_lss()` (readr-style verb).
  * `audit_lss()` now accepts either a `.lss` path or an `lss`
    object, so a one-line audit is `audit_lss("survey.lss")`.
  * `render_questionnaire(input, output, ...)` replaces
    `render_lss_docx()`, `lss_to_docx()`, and `lss_to_pdf()`. The
    output format is inferred from the extension of `output`
    (`.docx` or `.pdf`). `input` accepts a path or a pre-parsed
    `lss` object.
  * `render_audit(input, output, ...)` replaces
    `render_lss_audit_docx()`, `lss_audit_to_docx()`, and
    `lss_audit_to_pdf()` with the same polymorphism and format
    detection.
  * `lss_docx_to_pdf()` is now an internal helper invoked
    automatically by the `.pdf` branch of the renderers.
  Pre-release breaking change (the package was at `0.0.0.9000`),
  no deprecation shims are provided.
* The survey title now appears at the **top-right header** of every
  page (one line per displayed language, truncated to 80 characters
  with a trailing ellipsis when longer), replacing the previous
  footer-left position. The footer now holds only the compact `X/Y`
  page counter (no spaces) right-aligned. Controlled by the renamed
  `show_header_title` argument (was `show_footer_title`).
* New `title` argument to `render_questionnaire()`. Pass a single string
  to override every language with the same title, or a named
  character vector like `c(fr = "Mon titre", de = "Mein Titel")` for
  per-language overrides. `NULL` (the default) keeps the per-language
  titles from the survey settings. Drives both the cover page and the
  header.
* Each item now renders as a **unified mini-table** with a left label
  column and one row per content element
  (`Language | Question | Help | Value 1 | Value 2 | ...`). Each row
  is self-describing in line with ESS / MOSAiCH convention.
* When a question has `other = "Y"`, the LimeSurvey free-text
  variable `<parent>_other` is now surfaced as its own numbered item
  with the customized `other_replace_text` prompt, instead of being
  buried in the attributes section.
* Table-of-contents entries are now **clickable hyperlinks** that jump
  to the corresponding group heading. Each group heading is anchored
  with a bookmark and the TOC entry is an internal hyperlink to that
  anchor. Works in Word, LibreOffice and PDF.
* Group headings are rendered as a **uniformly styled paragraph**
  (manual `"N. group name"` prefix in Calibri 14pt blue bold) rather
  than a Word Heading 1 paragraph. Word's Heading 1 style ships with a
  linked list definition whose number style does not match the body
  font; using a styled paragraph keeps the whole heading
  typographically uniform. Trade-off: groups no longer appear in
  Word's left-side navigation pane, but the clickable TOC at the
  start of the document provides the same navigation in every viewer.
* The table of contents is now a **manual list** of group names with
  sequential numbering, rather than a Word `TOC` field. The previous
  field-based implementation had two practical limitations: Word's
  auto-refresh of `PAGEREF` entries can fire before pagination is
  complete, producing page numbers of `1` everywhere; and LibreOffice
  does not refresh field values on open or during headless PDF
  conversion. The manual list is always visible immediately in every
  viewer (Word, LibreOffice, PDF) with no refresh interaction. Trade-
  off: no page numbers in the TOC.
* The page footer's `X / Y` page counter now inherits the same muted
  Calibri 8pt gray as the title on its left; `officer::run_word_field`
  needs an explicit `prop` argument to override the paragraph default.
* The field-dirty marker injection now patches `word/footer*.xml` and
  `word/header*.xml` in addition to `word/document.xml`, so Word
  refreshes PAGE/NUMPAGES fields wherever they appear.
* `zip` declared in `Suggests` (used by `lss_inject_update_fields()`
  to repack the .docx after modifying its parts). The helper now
  guards behind `requireNamespace("zip")` and silently no-ops if the
  package is unavailable.
* Generated `.docx` files now embed the `<w:updateFields w:val="true"/>`
  setting, so Word refreshes the table-of-contents and page-number
  fields automatically when the document is opened -- no F9 needed.
  The instruction line "Press F9 in Word to refresh page numbers"
  above the TOC has been removed, and the marketing line "Processed
  locally with lssdoc. Nothing is uploaded." has been dropped from the
  cover page. (LibreOffice headless PDF conversion still does not
  refresh fields, so `render_questionnaire()` produces a PDF with an empty TOC;
  open the .docx in Word and save as PDF to obtain a populated TOC.)
* New `show_source` argument (default `TRUE`). When `FALSE`, the
  `Source file` and `Survey ID` rows are hidden from the cover
  metadata table -- useful when sharing a review document without
  exposing the internal LimeSurvey survey id or the original filename.
* `show_item_heading` default flipped to `FALSE`: the meta table now
  starts each item directly for a more compact layout. The item number
  is still visible in the meta table's `No` column and the variable
  index, so the bold "N. variable" heading is redundant for cross-
  reference. Pass `show_item_heading = TRUE` to restore it.
* Each **subquestion** now carries its own structured meta table (with its
  own No, composite variable code `parent_subq`, and the type / mandatory /
  filter inherited from the parent question). Every numbered item is now
  fully self-documented.
* **Compound parent banners** use the same 5-column meta layout as leaf
  items (No, Variable, Type, Mand., Filter) instead of a 4-column variant,
  with the No cell left empty since the parent itself is not a numbered
  item (its subquestions below carry the numbers). Visual structure is
  now consistent across leaf and compound questions.
* Group names with a leading author-written numeric prefix (`"1. Vos
  etudes"`, `"Section A - Demographics"`) are now stripped before being
  passed to Heading 1, so Word's auto-numbering is the only visible one
  ("1. Vos etudes" rather than "1. 1. Vos etudes").
* New `show_item_heading` argument (default `TRUE`). When `FALSE`, the
  bold "N. variable" line above each item is suppressed and the meta
  table starts the item directly. Use it for a more compact layout.
* The mandatory column is now labelled **`Mand.`** (was `Oblig.`),
  widened to 0.7", and recognizes LimeSurvey's `S` value as `soft`
  (soft-mandatory questions). Headers stay short while accommodating the
  longer value.
* A page break now follows the table of contents so the welcome text and
  groups start on a fresh page.
* New navigation layer for the rendered document:
  * **Table of contents** now lists groups only, not items: a 95-item
    survey produces a clean 5-line TOC instead of a 95-line list.
    Controlled by `show_toc` (default `TRUE`), skipped automatically when
    the survey has fewer than two groups.
  * **Variable index** appended at the end of the document: every item
    code with its sequential number, sorted alphabetically, so the
    reader can look up a specific variable. Controlled by `show_index`
    (default `TRUE`).
  * **Page footer** redesigned: survey title (per displayed language,
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
* `render_questionnaire()` and `render_audit()` accept an optional
  `logo` argument (path to a PNG or JPEG) that places an image at the top
  of the cover page; `logo_width` and `logo_height` control its size. The
  default keeps the cover logo-free, matching the neutral style of
  survey-methodology references.
* The cover page now shows the LimeSurvey **Survey ID** and **Last
  modified** timestamp from the `.lss`, giving reviewers stable
  traceability for the source questionnaire.
* `render_audit()` and `render_audit()` pipeline wrappers run the
  audit and produce a focused report in one call.
* `.docx_to_pdf()` converts a generated `.docx` to PDF locally via
  LibreOffice (or Word) in headless mode. Nothing leaves the user's
  machine. `render_questionnaire()` ties the full pipeline together.
* `render_questionnaire()` runs the full pipeline (`read_lss()` then
  `render_questionnaire()`) in one call.
* `render_audit()` produces a focused audit-only Word document:
  the same cover page, then one section per severity (errors, warnings,
  notes) with a table of findings. Use it for QA follow-up, separate from
  the full review.
* `render_questionnaire()` produces a professional Word review document from a
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
* `read_lss()` reads a LimeSurvey `.lss` file into a structured `lss`
  object, preserving all user text verbatim.
* Initial package scaffolding: the user-facing API (`read_lss()`,
  `audit_lss()`, `render_questionnaire()`, and the `render_questionnaire()` wrapper) is
  defined and documented, with `render_questionnaire()` still to come.

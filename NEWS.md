# lssdoc 0.0.0.9000

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

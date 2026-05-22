# lssdoc 0.0.0.9000

* `audit_lss()` inspects a parsed survey and reports reviewable anomalies
  (missing translations, texts empty in every language, duplicate question
  or answer codes, types missing their required options or subquestions, and
  orphan references) as a classed `lss_audit` object with a print method.
* `parse_lss()` reads a LimeSurvey `.lss` file into a structured `lss`
  object, preserving all user text verbatim.
* Initial package scaffolding: the user-facing API (`parse_lss()`,
  `audit_lss()`, `render_lss_docx()`, and the `lss_to_docx()` wrapper) is
  defined and documented, with `render_lss_docx()` still to come.

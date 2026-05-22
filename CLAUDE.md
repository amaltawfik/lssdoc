# lssdoc — Claude-specific notes

For design principles, coding conventions, testing rules,
documentation rules, and the full release workflow, see
[`AGENTS.md`](AGENTS.md). The notes below are Claude-specific
overrides that take precedence within Claude Code sessions.

The functional specification for the package lives in
[`SPEC.md`](SPEC.md): goal, target user API, and architecture.

## Package architecture

- `R/parse_lss.R` - read a LimeSurvey `.lss` (XML) export into a
  structured `lss` object (languages, groups, questions,
  subquestions, answers, attributes).
- `R/audit_lss.R` - detect reviewable anomalies in an `lss` object
  and return an `lss_audit` object with a `print()` method.
- `R/render_lss_docx.R` - render an `lss` object to a `.docx`
  review document, up to four languages side by side.
- `R/lss_to_docx.R` - convenience wrapper: parse then render.
- `R/conditions.R` - classed error and warning helpers
  (`lssdoc_abort()`, `lssdoc_warn()`).
- `R/lssdoc-package.R` - package-level documentation and imports.

Imports: `cli`, `rlang`, `xml2`.
Optional dependencies (Suggests): `officer`, `flextable` (used only
by the rendering path). Guard all usage with `requireNamespace()`
and a clear, actionable error (`lssdoc_missing_suggest`).

Example data ships in `inst/extdata/` (`hesav_2026.lss`,
`limesurvey_survey_751689.lss`).

## Working style

- For any change touching more than one file or affecting user-facing
  behavior, describe the plan before writing code.
- Prefer minimal, focused changes. Do not refactor surrounding code
  unless asked.
- Confidentiality is a design constraint: never add a code path that
  uploads a questionnaire or its content to any external service.

## Git

- Do not commit unless explicitly asked.
- Do not push unless explicitly asked.

## Key commands

```sh
# Load package and run code
Rscript -e "devtools::load_all(); code"

# Run all tests
Rscript -e "devtools::test()"

# Run tests matching a filter
Rscript -e "devtools::test(filter = '^parse_lss')"

# Run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-parse_lss.R')"

# Redocument the package
Rscript -e "devtools::document()"
```

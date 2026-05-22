# lssdoc — package development

lssdoc generates a Word (`.docx`) review document from a LimeSurvey
`.lss` export, displaying up to four languages side by side. See
[`SPEC.md`](SPEC.md) for the goal, the target user API, and the
architecture. Processing is fully local: confidentiality is a design
constraint, never add a code path that uploads a questionnaire or its
content anywhere.

## Design principles

These two principles override convenience when they conflict.
Reach for them first when reviewing any change to user-facing
input or output.

* **Verbatim user content.** Never silently mutate user-supplied
  text from the questionnaire: question titles, labels, help texts,
  answer options, or filenames are preserved exactly as they appear
  in the `.lss` file. The whole point of the review document is to
  let a human see the questionnaire as it really is. The only
  sanctioned exception is the encoding/escaping strictly required to
  produce valid `.docx` output.
* **Loud signals over silent mutations.** The user must always be
  able to see what the package did to their input. The visible signal
  scales with the response, in this order of preference:

    1. **Succeed silently** when the request is fulfilled exactly as
       stated, with no compromise.
    2. **Succeed with a classed warning** (`lssdoc_warn()`) when there
       is a small, unambiguous, reversible adjustment that lets the
       function continue (e.g. more than four languages are requested
       and the extra ones are dropped from a side-by-side layout; warn
       with a leaf class so the user sees exactly which were kept).
       Prefer this over option 3 whenever the adjustment is mechanical.
    3. **Fail with `lssdoc_abort()`** when there is no clean adjustment
       and the user must correct the input (e.g. the file is not valid
       `.lss` XML, or a requested language is absent). Use a classed
       condition (the `lssdoc_error` parent plus a leaf class) so
       downstream code can dispatch on class.

  Anomalies found during review are surfaced through `audit_lss()`,
  not through warnings or errors: a malformed questionnaire is data to
  report, not a failure of the package.

  Tests that observe these signals must dispatch on **class**, not on
  regex over the message string. `cli` formatting and locale settings
  vary across platforms; a test using `class = "lssdoc_file_not_found"`
  is robust where a message-string match is not.

## Key commands

```sh
# To run code
Rscript -e "devtools::load_all(); code"

# To run all tests
Rscript -e "devtools::test()"

# To run tests matching a function or file prefix
Rscript -e "devtools::test(filter = '^parse_lss')"

# To run a single test file
Rscript -e "testthat::test_file('tests/testthat/test-parse_lss.R')"

# To redocument the package
Rscript -e "devtools::document()"

# To rebuild the README after editing README.Rmd
Rscript -e "devtools::build_readme()"

# To check URLs in docs
Rscript -e "urlchecker::url_check()"

# To spell-check docs
Rscript -e "devtools::spell_check()"

# To check the package with R CMD check
Rscript -e "devtools::check()"

# To format code
air format .
```

## Coding

* Always run `air format .` after generating or editing R code.
* Use the base pipe operator (`|>`) not the magrittr pipe (`%>%`).
* Do not use `_$x` or `_[["x"]]` because this package must work on R 4.1.
* Use `\(...) ...` for single-line anonymous functions. For all other
  cases, use `function(...) {}`.
* Parse `.lss` XML with `xml2`. Be defensive about optional sections and
  missing translations; a real questionnaire is rarely complete.
* Prefer minimal dependencies. `officer` and `flextable` are Suggests
  used only by the rendering path; guard them with `requireNamespace()`
  and fail with `lssdoc_abort(class = "lssdoc_missing_suggest")`.
* Follow existing file boundaries: one user-facing function per
  `R/*.R` file, classed-condition helpers in `R/conditions.R`.

## Testing

* Tests for `R/{name}.R` go in `tests/testthat/test-{name}.R`.
* All new code should have an accompanying test.
* Keep tests minimal and focused, with few comments.
* Prefer testing returned objects, their classes, and their fields over
  asserting printed output; only assert printed/rendered output when the
  formatting itself is the feature under test.
* Cover optional-dependency paths (rendering) with either guarded tests
  or explicit expectations for the `lssdoc_missing_suggest` error when
  `officer`/`flextable` are unavailable.
* When fixing a bug, add a regression test that would have failed before
  the fix. Use the bundled `inst/extdata/*.lss` files as fixtures.

## Documentation

* Every user-facing function should be exported and have roxygen2
  documentation.
* Internal functions should not have standalone roxygen2 documentation
  topics; mark them `@keywords internal` and `@noRd`.
* Wrap roxygen comments at 80 characters.
* Always run `devtools::document()` after changing a roxygen comment.
* If you edit `README.Rmd`, rebuild `README.md` with
  `devtools::build_readme()`.
* Examples that need to write files, the rendering path, or optional
  packages should be guarded with `\dontrun{}` / `\donttest{}` and
  `requireNamespace()` checks.

## NEWS.md

* Every user-facing change should get a bullet in `NEWS.md`. Do not add
  bullets for small documentation changes or internal refactors.
* Each bullet should describe the end-user impact and mention the
  affected function early when relevant.
* Keep each bullet to a single paragraph with no manual line wrapping.
* Order bullets alphabetically by function name within a release block.
  Put general bullets that do not name a function first.

## Writing

* Use sentence case for headings.
* Use US English.
* Keep error messages, warnings, and documentation direct and practical.

## Proofreading

If the user asks you to proofread a file, act as an expert proofreader and
editor with a deep understanding of clear, engaging, and well-structured
writing.

Work paragraph by paragraph, always starting by making a TODO list that
includes individual items for each top-level heading.

Fix spelling, grammar, and other minor problems without asking the user.
Label any unclear, confusing, or ambiguous sentences with a FIXME comment.

Only report what you have changed.

# Review notes — problems spotted in passing

Running log of bugs, typos, dead code, gaps and smells found while
working. Not all are necessarily worth acting on now; flagged for your
call.

## Dead code (zero call-sites, zero tests, all `@noRd` internal)

Found while chasing test coverage in `R/render_item.R`. These functions
are never called anywhere in `R/` and never referenced by tests — they
are the residue of superseded render paths:

1. **`lss_render_shared_scale()`** (~702–725) and its helper
   **`lss_render_scale_table()`** (~730–757). The old "Shared answer
   scale" block for arrays, replaced by the per-subquestion path
   (`lss_render_subq_item`). Bonus bug had it ever run: the title
   `"Shared answer scale %d"` is **hard-coded English**, not pulled from
   `theme$chrome`, so it would not localize.
2. **`lss_render_parent_stem()`** (~523–573). The old grouped
   multiple-choice "parent stem" path. Its only consumer of
   **`lss_coding_row()`** (~1259–1277) — so `lss_coding_row` is dead too.
   `lss_coding_row` also carries **hard-coded English** coding strings
   (`"Y = selected, blank = not selected"`, `"M = Male, F = Female"`,
   `"1, 2, 3, 4, 5 ..."`) that would not localize in fr/de chrome.
3. **`lss_render_attrs()`** (~1740–1755). Self-documented as "legacy …
   no longer called from the main render path."
4. **`lss_render_optional_lang_block()`** (~1597–1619). Zero call-sites.

→ Recommend deleting all six (done as part of the coverage work, since
testing unreachable code is artificial). ~125 lines removed.

5. **`lss_question_meta()`** (render_meta_table.R ~5–16) — zero
   call-sites (the meta line is built by `lss_render_question_meta_table`
   now). Its only consumer of **`lss_relevance_label()`**
   (render_filter.R ~16–22), so that one is dead too. Both removed.

## i18n smells

- See (1)/(2) above: hard-coded English in `lss_render_shared_scale`
  and `lss_coding_row`. Dead today, but if either is ever revived the
  English strings must move into `chrome_strings`.

## RESOLVED — table template now renders group descriptions

`lss_table_template_rows_for_group()` used to emit a **group-name** row
but never the group's **description** text, whereas cards rendered it.
Fixed: a `group_description` row is now emitted under the group banner,
composed per language like the welcome/end-text rows and banded with the
group tint. Asserted for both templates in `test-coverage-boost3.R`;
roxygen + README updated.

## RESOLVED — stale man page referenced a deleted fixture

`man/render_audit.Rd` still had `@examples` calling
`system.file("extdata", "limesurvey_survey_751689.lss", ...)` — a file
removed earlier this session. The roxygen source had been updated but
`document()` was never re-run, leaving the Rd stale (a CRAN
`\dontrun` example pointing at a non-existent file). Re-running
`devtools::document()` regenerated it to `demo_survey.lss`. No other man
page references the removed fixtures.

## Test-suite migration (this session)

- Several tests were coupled to the removed `hesav_2026.lss` fixture's
  *content*. Migrated to `demo_survey.lss` and re-pinned to its real
  values. Notable: `demo_survey` is **not** audit-clean — it carries one
  informational `note` (`empty_in_all_languages` on `imc`, a type-`*`
  equation question with no display text). Tests that assumed a
  perfectly clean fixture were reframed accordingly.
- `test-quotas.R` still had a leftover `hesav <- system.file(... )`
  pointing at `demo_survey.lss`; the "quota-less survey" half was a
  no-op (demo_survey *has* quotas). Rewrote it to strip quotas in memory
  so the no-quota-section path is actually exercised.

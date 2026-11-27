# lssdoc (development version)

## Breaking changes

* `render_questionnaire()` is renamed `render_documentation()` (#9). A call
  only needs the new name. The cover of the document now says
  "Questionnaire documentation". The name `render_questionnaire()` goes to
  the new document described below. A call written for an earlier version
  that passes `languages`, `template` or another of the former arguments
  stops with an error that names `render_documentation()`.

* `render_documentation()` now numbers the questions, not the cards (#10).
  A question has one number, whatever the number of cards or rows it takes.
  The rows of an array are numbered `28.1`, `28.2` and so on, as are the
  options of a multiple choice and the positions of a ranking in the table
  layout. An "Other" or comment field carries the number of its question.
  A text display, an equation and a question hidden from respondents get
  no number: their `No` cell shows an en dash, and the cover counts them
  apart. The last number is the number of questions asked, which is the
  count the cover prints under "Questions". A document rendered with an
  earlier version carries other numbers.

## New features

* `render_questionnaire()` now renders the questionnaire as it reads, in
  one language (#9). Each question has its number and its text, then its
  subquestions and its answer options, one per line. The codes of
  LimeSurvey and the conditions written with them are printed in small
  gray type, and `show_codes = FALSE` removes both. The numbers are those
  of `render_documentation()`. The document declares its language to Word
  (`proofing_lang`), so the spelling checker and the hyphenation follow
  the questionnaire. The quotas of the survey come at the end
  (`show_quotas`).

## Minor improvements and bug fixes

* `render_documentation()` gains `show_group_description`. `FALSE` leaves
  out the description of a group, the text LimeSurvey shows respondents
  under the group title.

* `render_documentation()` now prints the conditions set on a group and on
  a subquestion (the row of an array, the option of a multiple choice). It
  printed the condition of the question only, so a question whose group
  alone carried a condition showed `All`. When a group or a subquestion has
  a condition, the Filter cell gives each one on its own line after the
  name of its level (`Group: Q1 = 1`), and the condition of a group follows
  its heading.

* A condition typed by hand, without the `.NAOK` suffix, is now simplified
  like one written by LimeSurvey's condition editor: `workstatus == "1"`
  reads `workstatus = 1`, where it kept its quotes. The operators `and`
  and `or` are read like `&&` and `||`.

* The demo survey `demo_survey.lss` now has a condition on a group and on
  two subquestions, besides the one it had on a question, so a render of
  it shows the three levels.

* `render_documentation()` now prints the formula of a computed question
  by default: `"equation"` joins the default of `show_attrs`.
  `render_questionnaire()` prints it after the code of the question.

* The table layout of `render_documentation()` now prints the attributes
  named in `show_attrs`, such as a prefix or a suffix, as the cards layout
  does. Each one has its own row under the row of the question or of the
  subquestion. The table printed the formula of a computed question only.

* `render_documentation()` now reads the exclusive options of a question
  (`exclude_all_others`) with the separator LimeSurvey uses, a semicolon.
  With two exclusive options, it read their codes as one. The note of a
  multiple choice now names the options, and the table layout prints the
  note of an exclusive subquestion of an array, a multiple numeric or a
  multiple text question, as the cards layout does.

* In the table layout of `render_documentation()`, the rows of an "Other"
  field and of the comment field of a list with comment now show `[text]`
  in the Value cell, which was empty. The comment field is labelled
  "Comment", and its row no longer repeats the help of the question.

* In the table layout, a question text, a subquestion or a help missing in
  one language of the document now shows the empty marker in its place.
  The marker used to appear only when the whole cell was empty.

* The quotas table of `render_documentation()` joined two answers of the
  same question with `AND`. They are alternatives and now read
  `Q1 ∈ {1, 2}`.

* The table of contents of `render_documentation()` now gives the numbers
  of the questions of each group (`questions 1 to 10`). In the table layout
  its group entries lead to the group rows, where they led nowhere. With
  `show_groups = FALSE` it lists no group.

* A gender question now lists Female before Male, the order LimeSurvey
  shows.

* `render_documentation()` now prints the labels LimeSurvey builds in (Yes,
  No, Uncertain, Increase, Same, Decrease, Female, Male) and the default
  prompt of an "Other" field in the language of each column. Every column
  had them in the language of the document (`chrome_lang`): in an English
  document, the French column read "Yes" and "Other:". A column in a
  language lssdoc has no strings for keeps the language of the document.

* A question hidden from respondents (LimeSurvey attribute `hidden` at 1)
  now says so on its card or row. It used to be rendered like any other
  question.

* The document rendered by `render_documentation()` now gives its section
  and group headings an outline level: Word lists them in its navigation
  pane and the PDF carries bookmarks (#8). In the table layout the groups
  are table rows and stay out of the pane.

# lssdoc 0.3.0

## New features

* Write a questionnaire in Word and turn it into a LimeSurvey file, with a
  new experimental set of functions: `lss_template_docx()` writes a blank
  form to fill in, `read_form_docx()` reads a filled one back, and
  `check_form_docx()` reports everything wrong with it in one pass. Blank
  forms can also be downloaded from the package website, for authors who do
  not use R.

* `write_form_docx()` renders a specification as that same form, and
  `as_lss_spec()` turns an existing survey into a specification, so a
  questionnaire can leave LimeSurvey, be edited in Word and come back.

* `write_lss()` now writes every declared language, not only the first.

* `lss_template_docx()` can write a blank form for a multilingual
  questionnaire, independently of the language of the form's own labels.

* `lss_spec()` gains an optional group `description` and quota `limit`.

## Minor improvements and bug fixes

* `read_lss()` is about ten times faster on a large survey.

* `read_lss()` now fails with a clear error on a malformed, truncated or
  non-UTF-8 file, instead of terminating the R session on some platforms.

# lssdoc 0.2.0

## New features

* New experimental authoring layer. `lss_spec()` describes a questionnaire
  in R -- groups, 21 question kinds, answer options (including "other" and
  exclusive choices), display conditions and quotas -- and validates it
  against what LimeSurvey accepts. `write_lss()` then writes it as a
  LimeSurvey 6 `.lss` file ready to import. Both functions are experimental
  and their interface may change.

* `lss_spec()` accepts texts in several languages (`languages = c("fr", "en")`,
  each text given as a named vector), but `write_lss()` emits the primary
  language only for now. Multilingual output is planned for 0.3.0.

## Minor improvements and bug fixes

* `read_lss()` now warns (class `lssdoc_newer_dbversion`) when a file comes
  from a newer LimeSurvey than the package targets, instead of reading it
  silently.

* In the table layout, the "Type" column no longer wraps one-word labels
  such as "Computed" onto a second line.

# lssdoc 0.1.1

* `read_lss()` now returns a clear error on a non-XML or empty file, instead
  of, on some systems, crashing the R session.

# lssdoc 0.1.0

* Initial CRAN release.

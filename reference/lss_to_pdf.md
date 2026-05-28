# Convert a `.lss` file to a PDF review document

Same pipeline as
[`lss_to_docx()`](https://amaltawfik.github.io/lssdoc/reference/lss_to_docx.md),
but the generated `.docx` is then converted to PDF locally via
[`lss_docx_to_pdf()`](https://amaltawfik.github.io/lssdoc/reference/lss_docx_to_pdf.md)
(LibreOffice or Word). Nothing leaves the user's machine.

## Usage

``` r
lss_to_pdf(input, output, ...)
```

## Arguments

- input:

  Path to a `.lss` file.

- output:

  Path to the `.pdf` file to create.

- ...:

  Arguments passed on to
  [`render_lss_docx`](https://amaltawfik.github.io/lssdoc/reference/render_lss_docx.md)

  `languages`

  :   Character vector of language codes to display, **in the order they
      will appear as columns**. Use this both to subset (display only
      the languages you want) and to order them; for example
      `c("fr", "de")` puts French first, while `c("de", "fr")` puts
      German first. The first language is treated as the primary
      language: the question heading shown in the table of contents
      includes the question text in that language, and group headings
      fall back to it. Requesting a language absent from the survey is
      an error (`lssdoc_unknown_language`). Defaults to all languages
      found in the `.lss` file, in the order of the `<languages>`
      section.

  `template`

  :   Output style. `"cards"` (default) renders one detached pair of
      tables per item (a meta table + an item table) stacked vertically,
      with the survey's content languages displayed side-by-side in the
      item table. `"table"` renders a single dense table covering the
      whole document in codebook style: every variable is one row, the
      meta columns (`No | Variable | Type | Mandatory | Filter`) come
      first, then one column per content language carrying the question
      stem, the subquestion label (when applicable), and the response
      modalities stacked underneath. Group banners become merged section
      rows inside the table; the column header repeats on every page
      automatically. The `table` template auto-promotes the page format
      to A4 landscape for 2+ languages (overridable through
      `page_format`).

  `layout`

  :   Reserved for future use. Currently `"side-by-side"` only.

  `show_audit`

  :   Logical; include an audit summary section near the top and inline
      markers on questions that carry findings.

  `show_help`

  :   Logical; include question help texts under the question text.

  `show_attrs`

  :   Character vector of question attributes to display under the
      question text when present.

  `show_technical_attrs`

  :   Logical; include technical attributes such as `answer_order` and
      `location_*`.

  `page_format`

  :   Page format. `"auto"` picks portrait for one or two languages and
      landscape from three. Use `"A4-portrait"`, `"A4-landscape"`, or
      `"A3"` to force a layout.

  `show_toc`

  :   Logical; include a table of contents listing the groups of the
      survey. Skipped automatically when the survey has fewer than two
      groups (a single-group survey makes the TOC redundant). Items
      themselves are not in the TOC; use `show_index` for a navigable
      variable index.

  `show_index`

  :   Logical; append a variable index at the end of the document
      listing every item code with its item number, sorted
      alphabetically. Useful for cross-referencing a specific variable.

  `show_header_title`

  :   Logical; show the survey title at the top right of every page, one
      line per displayed language. Long titles are truncated to 80
      characters with a trailing ellipsis. Default `TRUE`. When `FALSE`,
      only the `X/Y` page counter shows at the bottom right.

  `show_source`

  :   Logical; show the **Source file** name and the **Survey ID** rows
      in the cover metadata table. Default `TRUE` keeps them for
      traceability; pass `FALSE` to hide both (some reviewers prefer not
      to expose the LimeSurvey survey id or the internal filename).

  `show_item_heading`

  :   Logical; show a bold heading `"N. variable"` above each item.
      Default `FALSE`: the meta table starts each item directly, for a
      compact layout. Set to `TRUE` to add the heading line for
      scroll-time navigation; the item number is already present in the
      meta table's `No` column and in the variable index so the heading
      is redundant for cross-reference purposes.

  `show_raw_filter`

  :   Logical; when `TRUE` (the default) the Filter cell of each meta
      table shows the human-readable form on top and the raw LimeSurvey
      relevance expression in smaller italic gray underneath. Set to
      `FALSE` for a cleaner cell that shows only the plain form (the raw
      expression is still shown when it could not be simplified).

  `show_groups`

  :   Logical; show the group banners (cards layout) or group rows
      (table layout). Default `TRUE`. Surveys sometimes use groups only
      as internal organization tools without showing them to
      respondents; pass `FALSE` to flatten the document into a single
      sequence of items with no section breaks.

  `show_welcome`

  :   Logical; include the survey's welcome text
      (`surveyls_welcometext`) in the document. Default `TRUE`. The
      welcome text is multilingual and appears as a side-by-side block
      in the cards layout or as an embedded row in the codebook table.

  `show_endtext`

  :   Logical; include the survey's end text (`surveyls_endtext`).
      Default `TRUE`. Same multilingual treatment as `show_welcome`.

  `show_description`

  :   Logical; include the survey's description
      (`surveyls_description`). Default `TRUE`. The description is the
      short "what this survey is about" multilingual intro that
      LimeSurvey shows above the welcome text on the landing page; it
      appears here before the welcome block in the cards layout and as a
      Description row in the codebook table.

  `show_privacy_settings`

  :   Logical; surface the survey-level privacy / tracking settings
      (`anonymized`, `save` partial, `datestamp`, `ipaddr`, `refurl`) as
      additional rows on the cover metadata table. Default `FALSE`
      because most reviewers do not need them; set to `TRUE` for an
      ethics committee submission where these flags are part of the
      methodological assessment.

  `show_admin_settings`

  :   Logical; surface the survey-level administrative settings
      (`alias`, end URL and its description, `active` flag) as
      additional rows on the cover metadata table. Default `FALSE`. The
      end URL is where the respondent is redirected after submission;
      reviewers may want to check it for third-party redirects.

  `title`

  :   Optional override of the survey title shown on the cover page and
      the top-right header. `NULL` (default) uses the per- language
      titles from the `.lss` survey settings. Pass a single string to
      use the same title in every displayed language, or a named
      character vector keyed by language code (e.g.
      `c(fr = "Mon titre", de = "Mein Titel")`) for per-language
      overrides.

  `logo`

  :   Optional path to an image (PNG or JPEG) to display at the top of
      the cover page. The `.lss` file does not embed a logo, so this
      image must be supplied by the caller. `NULL` (default) keeps the
      cover logo-free, matching the neutral style of survey-methodology
      references (ESS, MOSAiCH, Panel).

  `logo_width,logo_height`

  :   Image dimensions in inches. Defaults are tuned to a 2:1 logo (1.5
      x 0.75 inches). Resize or pre-crop your image to fit if it has a
      different aspect ratio.

  `font`

  :   Body font name. `NULL` (default) keeps Calibri, which is
      pre-installed on every recent Windows Office and
      metric-substituted with Carlito (OFL) on Mac and Linux
      LibreOffice, so column widths stay stable across platforms. Pass a
      string to override (e.g. `"Source Sans 3"`, `"IBM Plex Sans"`, or
      a corporate brand font). The string is passed through to Word;
      install the font on the machine that opens the document, otherwise
      the reader's application will substitute its own fallback face.

  `font_code`

  :   Monospace font used for code-like content: the variable column in
      each meta table, the raw relevance expression under each filter
      cell, and the variable index entries. `NULL` (default) keeps
      Consolas; pass `"JetBrains Mono"` or `"IBM Plex Mono"` for sharper
      code style if installed on the reader's machine.

  `colors`

  :   Optional named list of hex color overrides for the editorial
      petrol-blue palette. `NULL` (default) keeps the package palette
      intact. Accepted names: `"primary"` (group filets, headers text
      and item top borders, 1.0 pt strokes), `"accent"` (hyperlinks,
      ORCID iD, URL auto-links, group under-line in the cards layout),
      `"band"` (light header backgrounds in the meta table headers and
      the codebook's Question rows), `"band_dark"` (the meta-table dark
      header in the cards layout), `"zebra"` (the very-light tint on the
      codebook Question rows in the table template), `"grid"` (the 0.5
      pt border color used for every internal table line), `"text"` and
      `"muted"`. Each value must be a hex string (`"#XXXXXX"` or
      `"#XXX"`). Unknown keys are rejected with a classed condition so a
      typo never silently turns into a no-op. Useful for honoring an
      institutional brand: e.g.
      `colors = list(primary = "#5C9F1A", accent = "#7FA82E")` produces
      a LimeSurvey-green codebook instead of the default petrol blue.

  `chrome_lang`

  :   Language used for the **chrome** of the document (column headers,
      row labels, navigation titles, MOSAiCH-style type labels, Value
      descriptors, audit section). Independent of `languages`, which
      controls the survey's content columns. Supported values: `"en"`,
      `"fr"`, `"de"`, `"es"`, `"it"`. `NULL` (default) follows
      `languages[1]` when it is a supported chrome language, otherwise
      falls back to `"en"`. Pass the value explicitly to force a
      specific chrome language regardless of the content – e.g.
      `chrome_lang = "en"` with `languages = c("fr", "de")` produces an
      English-labeled document with French and German survey content.
      Spanish and Italian translations should be reviewed by a native
      speaker before publishing an official document.

  `description`

  :   Optional free-form text shown on the cover page below the authors
      block. Useful for a citation hint, a funding acknowledgement, a
      methodology note, or a link to a related publication. Pass a
      single string; line breaks (`\n`) split the block into separate
      centered lines. Any `http://` or `https://` token is rendered as a
      clickable hyperlink, so DOI URLs or article permalinks become
      navigable. Plain text otherwise.

  `authors`

  :   Optional credit block for the questionnaire's designers, displayed
      on the cover page below the subtitle. Each author is shown
      centered on its own line as `Name \u2014 Affiliation`; when an
      ORCID iD is provided, a smaller monospace line below shows
      `ORCID 0000-0000-0000-0000` as a hyperlink to
      `https://orcid.org/<id>`. Accepts:

      - `NULL` (default): no authorship block.

      - An **unnamed character vector**
        (`c("Amal Tawfik", "John Doe")`): each entry becomes a line with
        no affiliation.

      - A **named character vector**
        (`c("Amal Tawfik" = "HES-SO Valais")`): names are authors,
        values are affiliations. Use the empty string `""` to render an
        author without affiliation.

      - A **list of named lists** for the full form, e.g.
        `list(list(name = "Amal Tawfik", affiliation = "HES-SO Valais", orcid = "0009-0006-2422-1555"), list(name = "John Doe", affiliation = "UNIL"))`.
        The `name` field is required; the `affiliation` and `orcid`
        fields are optional.

## Value

The `output` path, invisibly.

## Examples

``` r
if (FALSE) { # \dontrun{
lss_to_pdf(
  system.file("extdata", "hesav_2026.lss", package = "lssdoc"),
  "rapport.pdf"
)
} # }
```

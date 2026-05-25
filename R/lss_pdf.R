#' Convert a `.docx` to `.pdf` locally
#'
#' Use LibreOffice (or Word, on Windows) in headless mode to convert a
#' generated `.docx` to a PDF. Conversion stays on the user's machine: no
#' upload, no network call. LibreOffice (the `soffice` executable) must be
#' available; the function reports an actionable error if it is not.
#'
#' @param docx Path to the source `.docx` file.
#' @param pdf  Path to the `.pdf` file to produce.
#'
#' @return The `pdf` path, invisibly.
#'
#' @details
#' LibreOffice headless does not refresh Word field values (TOC, page
#' counts) during conversion, even when the document is flagged to
#' update fields on open. As a result, the table-of-contents field
#' produced by [render_lss_docx()] appears empty in the converted PDF.
#' To obtain a PDF with a populated TOC, open the generated `.docx` in
#' Word -- the TOC refreshes automatically -- and use `File > Save As >
#' PDF`. Working directly from the `.docx` is the simplest path.
#'
#' @examples
#' \dontrun{
#' lss <- parse_lss(system.file("extdata", "hesav_2026.lss",
#'   package = "lssdoc"
#' ))
#' tmp_docx <- tempfile(fileext = ".docx")
#' render_lss_docx(lss, tmp_docx)
#' lss_docx_to_pdf(tmp_docx, "rapport.pdf")
#' }
#' @export
lss_docx_to_pdf <- function(docx, pdf) {
  if (!is.character(docx) || length(docx) != 1L || is.na(docx) || !file.exists(docx)) {
    lssdoc_abort(
      "{.arg docx} must point to an existing {.file .docx} file.",
      class = "lssdoc_bad_input"
    )
  }
  if (!is.character(pdf) || length(pdf) != 1L || is.na(pdf)) {
    lssdoc_abort(
      "{.arg pdf} must be a single file path.",
      class = "lssdoc_bad_output"
    )
  }
  soffice <- lss_find_soffice()
  if (is.null(soffice)) {
    lssdoc_abort(
      c(
        "Converting to PDF requires LibreOffice (or Word) installed locally.",
        "i" = "Install LibreOffice from {.url https://www.libreoffice.org/}, then retry.",
        "i" = "All processing stays on your machine."
      ),
      class = "lssdoc_missing_soffice"
    )
  }

  outdir <- normalizePath(dirname(pdf), mustWork = FALSE)
  if (!dir.exists(outdir)) {
    dir.create(outdir, recursive = TRUE, showWarnings = FALSE)
  }
  status <- suppressWarnings(system2(
    soffice,
    c("--headless", "--convert-to", "pdf", "--outdir", outdir,
      normalizePath(docx)),
    stdout = FALSE, stderr = FALSE
  ))
  produced <- file.path(
    outdir,
    paste0(tools::file_path_sans_ext(basename(docx)), ".pdf")
  )
  if (!file.exists(produced)) {
    lssdoc_abort(
      c(
        "LibreOffice did not produce a PDF (exit status {status}).",
        "i" = "Try opening the document in LibreOffice and exporting to PDF manually."
      ),
      class = "lssdoc_pdf_conversion_failed"
    )
  }
  if (!identical(normalizePath(produced, mustWork = FALSE),
                 normalizePath(pdf, mustWork = FALSE))) {
    if (file.exists(pdf)) file.remove(pdf)
    file.rename(produced, pdf)
  }
  invisible(pdf)
}

#' Locate the LibreOffice `soffice` executable
#' @keywords internal
#' @noRd
lss_find_soffice <- function() {
  on_path <- Sys.which("soffice")
  candidates <- c(
    if (nzchar(on_path)) on_path,
    "C:/Program Files/LibreOffice/program/soffice.exe",
    "C:/Program Files (x86)/LibreOffice/program/soffice.exe",
    "/usr/bin/soffice",
    "/usr/local/bin/soffice",
    "/Applications/LibreOffice.app/Contents/MacOS/soffice"
  )
  ok <- candidates[file.exists(candidates)]
  if (length(ok) == 0L) NULL else ok[1]
}

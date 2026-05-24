# -----------------------------------------------------------------
# Test complet du package lssdoc
# -----------------------------------------------------------------

# 1. Installation (si pas déjà fait)
# install.packages("pak")
# pak::pak("amaltawfik/lssdoc")
#
# Ou en dev local :
# devtools::load_all("C:/Users/at/Documents/R/Packages/lssdoc")

library(lssdoc)

out_dir <- tempdir()
cat("Outputs dans:", out_dir, "\n\n")


# -----------------------------------------------------------------
# 2. Parsing
# -----------------------------------------------------------------
lss <- parse_lss(
  system.file("extdata", "hesav_2026.lss", package = "lssdoc")
)
print(lss) # résumé compact
str(lss, max.level = 1) # structure de haut niveau

# Aperçu des tables internes
head(lss$questions[, c("qid", "title", "type", "gid")])
head(lss$question_l10ns[, c("qid", "language", "question")])


# -----------------------------------------------------------------
# 3. Audit
# -----------------------------------------------------------------
audit <- audit_lss(lss)
print(audit) # cli formaté
head(as.data.frame(audit)) # tableau exploitable en R

# Sur le second fichier (qui a 1 erreur boilerplate)
lss2 <- parse_lss(
  system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc")
)
print(audit_lss(lss2))


# -----------------------------------------------------------------
# 4. Rendu Word complet (avec audit incrusté par défaut)
# -----------------------------------------------------------------
review_docx <- file.path(out_dir, "review.docx")
render_lss_docx(lss, review_docx)

# Version finale propre (sans audit)
final_docx <- file.path(out_dir, "final.docx")
render_lss_docx(lss, final_docx, show_audit = FALSE)


# -----------------------------------------------------------------
# 5. Rendu audit-only
# -----------------------------------------------------------------
audit_docx <- file.path(out_dir, "audit.docx")
render_lss_audit_docx(lss2, audit_docx) # le fichier qui a 1 finding


# -----------------------------------------------------------------
# 6. Pipelines en un appel (parse + render)
# -----------------------------------------------------------------
lss_to_docx(
  system.file("extdata", "hesav_2026.lss", package = "lssdoc"),
  file.path(out_dir, "pipe_review.docx")
)
lss_audit_to_docx(
  system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc"),
  file.path(out_dir, "pipe_audit.docx")
)


# -----------------------------------------------------------------
# 7. Sortie PDF (nécessite LibreOffice installé)
# -----------------------------------------------------------------
lss_to_pdf(
  system.file("extdata", "hesav_2026.lss", package = "lssdoc"),
  file.path(out_dir, "review.pdf")
)
lss_audit_to_pdf(
  system.file("extdata", "limesurvey_survey_751689.lss", package = "lssdoc"),
  file.path(out_dir, "audit.pdf")
)


# -----------------------------------------------------------------
# 8. Avec un logo sur la couverture (PNG ou JPEG)
# -----------------------------------------------------------------
# Crée un logo de test (ou remplace par le chemin de ton propre PNG)
logo_path <- file.path(out_dir, "test_logo.png")
grDevices::png(logo_path, width = 600, height = 300, bg = "white")
par(mar = c(0, 0, 0, 0))
plot.new()
rect(0, 0, 1, 1, col = "#1F4E79", border = NA)
text(0.5, 0.5, "MON LOGO", col = "white", cex = 4, font = 2)
grDevices::dev.off()

render_lss_docx(
  lss,
  file.path(out_dir, "review_with_logo.docx"),
  logo = logo_path
)


# -----------------------------------------------------------------
# 9. Restriction à un sous-ensemble de langues
# -----------------------------------------------------------------
render_lss_docx(
  lss,
  file.path(out_dir, "review_fr_only.docx"),
  languages = "fr"
)


# -----------------------------------------------------------------
# 10. Forcer un format de page (utile pour 3-4 langues)
# -----------------------------------------------------------------
render_lss_docx(
  lss,
  file.path(out_dir, "review_landscape.docx"),
  page_format = "A4-landscape"
)


# -----------------------------------------------------------------
# 11. Ouvrir les fichiers générés
# -----------------------------------------------------------------
list.files(out_dir, pattern = "\\.(docx|pdf)$", full.names = TRUE)

# Windows : ouvrir le rendu principal dans Word
shell.exec(review_docx)

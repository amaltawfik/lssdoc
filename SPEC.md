# lssdoc — Spécification du package R

## Objectif

Générer un document Word de relecture professionnel à partir d'un fichier
LimeSurvey `.lss`, prenant en charge jusqu'à 4 langues affichées côte à côte.
Le document doit faciliter la relecture humaine de questionnaires multilingues
(comités d'éthique, traducteurs, méthodologistes) en restituant fidèlement
l'ensemble du contenu et en signalant automatiquement les anomalies
détectables sans IA.

Le package cible un usage local (confidentialité préservée), distribué via
GitHub (`remotes::install_github("amaltawfik/lssdoc")`). Aucun service en
ligne, aucun upload de questionnaire vers un serveur tiers.

## API utilisateur cible

```r
library(lssdoc)

# Pipeline simple : .lss -> .docx
render_questionnaire("monquestionnaire.lss", "rapport.docx")

# Avec contrôle fin
lss <- read_lss("monquestionnaire.lss")
audit <- audit_lss(lss)
print(audit)  # affiche les anomalies détectées

render_questionnaire(
  lss,
  output      = "rapport.docx",
  languages   = c("fr", "de", "en", "it"),  # défaut : toutes celles du .lss
  layout      = "auto",                      # auto | side-by-side | stacked
  show_audit  = TRUE,                        # section audit en début de doc
  show_help   = TRUE,                        # affiche les textes d'aide
  show_attrs  = c("prefix", "suffix", "other_replace_text", "validation"),
  show_technical_attrs = FALSE,              # exclut answer_order, location_*, etc.
  page_format = "auto"                       # auto | A4-portrait | A4-landscape | A3
)
```

## Architecture du package
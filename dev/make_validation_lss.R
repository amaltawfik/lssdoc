# Kitchen-sink survey exercising every `spec_kinds` entry, for a manual import
# test in a real LimeSurvey 6 before releasing lssdoc 0.2.0.
# Run from the package root: Rscript dev/make_validation_lss.R
# Output under dev/validation/ is git-ignored regenerable output: commit only this script.

pkgload::load_all(".", quiet = TRUE)

out_dir <- file.path("dev", "validation")
dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
out <- file.path(out_dir, "lssdoc_0.2.0_validation.lss")

# Question codes are LimeSurvey variable names: letters and digits only (no
# underscore), so each code carries its number and its kind in camelCase.
# The wording is deliberately accented: the import must round-trip UTF-8.
spec <- lss_spec(
  title = "lssdoc 0.2.0 - validation d'import (toutes les familles de questions)",
  language = "fr",
  welcome = c(
    "Questionnaire de validation technique généré par lssdoc.",
    "Chaque question porte son type dans son code (Q01SingleOther = question 1, liste simple avec option Autre)."
  ),
  end_text = "Fin du questionnaire de validation.",
  groups = list(

    # ---- Groupe 1 : choix ------------------------------------------------
    list(title = "1. Choix (listes, cases à cocher, classement)", questions = list(

      list(code = "Q01SingleOther", kind = "single", mandatory = TRUE,
           text = "Q01 - Liste à choix unique avec option Autre. Acceptez-vous de participer ?",
           help = "Répondre Non déclenche le quota de fin de questionnaire.",
           options = list(
             list(text = "Oui, je participe"),
             list(text = "Non, je refuse"),
             list(text = "Autre situation, merci de préciser", other = TRUE))),

      list(code = "Q02DropdownOther", kind = "dropdown",
           text = "Q02 - Liste déroulante avec option Autre. Dans quel canton travaillez-vous ?",
           options = list(
             list(text = "Vaud"), list(text = "Genève"), list(text = "Valais"),
             list(text = "Autre canton, merci de préciser", other = TRUE))),

      list(code = "Q03SingleComment", kind = "singlecomment",
           text = "Q03 - Liste avec commentaire libre. Quel est votre statut ?",
           options = list(
             list(text = "Employé·e"), list(text = "Indépendant·e"),
             list(text = "Sans activité"))),

      list(code = "Q04MultiOtherExcl", kind = "multiple", mandatory = TRUE,
           text = "Q04 - Cases à cocher : option Autre, option exclusive et maximum 3 réponses.",
           help = "\"Aucun de ces soutiens\" doit décocher toutes les autres cases.",
           relevance = "Q01SingleOther = 1",
           max_answers = 3,
           other_position = "specific", other_position_code = "4",
           options = list(
             list(text = "Du temps"),
             list(text = "De l'argent"),
             list(text = "Un conseil"),
             list(text = "Une formation"),
             list(text = "Aucun de ces soutiens", exclusive = TRUE),
             list(text = "Autre soutien, merci de préciser", other = TRUE))),

      list(code = "Q05RankingMin", kind = "ranking", mandatory = TRUE,
           text = "Q05 - Classement : exactement 3 éléments sur 5 (min_answers et max_answers).",
           max_answers = 3,
           attributes = list(min_answers = "3"),
           options = list(
             list(text = "La sécurité de l'emploi"), list(text = "Le salaire"),
             list(text = "L'ambiance d'équipe"), list(text = "L'autonomie"),
             list(text = "La formation continue"))),

      list(code = "Q06YesNo", kind = "yesno",
           text = "Q06 - Oui / Non (échelle implicite Y/N). Avez-vous suivi une formation cette année ?")
    )),

    # ---- Groupe 2 : tableaux ---------------------------------------------
    list(title = "2. Tableaux (array et variantes à échelle implicite)", questions = list(

      list(code = "Q07Array", kind = "array",
           text = "Q07 - Tableau générique : lignes et colonnes définies dans la spécification.",
           relevance = "Q02DropdownOther in [1, autre]",
           rows = list(list(text = "La charge de travail"),
                       list(text = "Le soutien de la hiérarchie"),
                       list(text = "Les horaires")),
           columns = list(list(text = "Jamais"), list(text = "Parfois"),
                          list(text = "Souvent"), list(text = "Toujours"))),

      list(code = "Q08Array5", kind = "array5",
           text = "Q08 - Tableau à échelle implicite 1 à 5.",
           rows = list(list(text = "Ma satisfaction globale"),
                       list(text = "Mon équilibre vie privée / travail"))),

      list(code = "Q09Array10", kind = "array10",
           text = "Q09 - Tableau à échelle implicite 1 à 10.",
           rows = list(list(text = "Qualité des outils informatiques"),
                       list(text = "Qualité des locaux"))),

      list(code = "Q10ArrayYesNo", kind = "arrayyesno",
           text = "Q10 - Tableau Oui / Non / Incertain.",
           rows = list(list(text = "J'ai reçu un entretien annuel"),
                       list(text = "J'ai reçu un plan de formation"))),

      list(code = "Q11ArrayTrend", kind = "arraytrend",
           text = "Q11 - Tableau Augmente / Identique / Diminue.",
           rows = list(list(text = "Ma charge de travail depuis un an"),
                       list(text = "Mon intérêt pour le poste depuis un an")))
    )),

    # ---- Groupe 3 : saisie libre, échelles, affichage ---------------------
    list(title = "3. Saisie libre, échelles simples et texte affiché", questions = list(

      list(code = "Q12MultiText", kind = "multitext",
           text = "Q12 - Plusieurs champs texte courts.",
           options = list(list(text = "Fonction"), list(text = "Service"),
                          list(text = "Lieu de travail"))),

      list(code = "Q13MultiNumeric", kind = "multinumeric",
           text = "Q13 - Plusieurs champs numériques.",
           options = list(list(text = "Années dans l'institution"),
                          list(text = "Heures hebdomadaires"),
                          list(text = "Nombre de collaborateurs encadrés"))),

      list(code = "Q14Text", kind = "text",
           text = "Q14 - Texte libre long.",
           help = "Ne s'affiche que si au moins deux cases sont cochées en Q04.",
           relevance = "count(Q04MultiOtherExcl) >= 2"),

      list(code = "Q15ShortText", kind = "shorttext",
           text = "Q15 - Texte libre court."),

      list(code = "Q16HugeText", kind = "hugetext",
           text = "Q16 - Texte libre très long."),

      list(code = "Q17Numeric", kind = "numeric",
           text = "Q17 - Valeur numérique. Combien d'années d'expérience avez-vous ?"),

      list(code = "Q18Date", kind = "date",
           text = "Q18 - Date. Date de votre entrée en fonction ?"),

      list(code = "Q19Gender", kind = "gender",
           text = "Q19 - Genre (échelle implicite M/F)."),

      list(code = "Q20FivePoint", kind = "fivepoint",
           text = "Q20 - Échelle implicite 1 à 5.",
           relevance = "Q06YesNo = Y"),

      list(code = "Q21Display", kind = "display",
           text = "Q21 - Texte affiché sans saisie : vérifier qu'aucune variable n'est créée.")
    ))
  ),
  quotas = list(
    list(name = "Refus de participer", question = "Q01SingleOther", code = "2",
         message = "Vous avez indiqué ne pas souhaiter participer. Le questionnaire s'arrête ici. Merci.")
  )
)

write_lss(spec, out)

# Round-trip control: a warning here (newer DBVersion, unreadable section) is a bug.
warned <- character()
lss <- withCallingHandlers(
  read_lss(out),
  warning = function(w) {
    warned <<- c(warned, class(w)[1L])
    invokeRestart("muffleWarning")
  }
)
if (length(warned)) {
  stop("read_lss() warned on the generated file: ", paste(warned, collapse = ", "))
}

kinds <- unlist(lapply(spec$groups, function(g) {
  vapply(g$questions, function(q) q$kind, character(1))
}))
missing <- setdiff(spec_kinds, kinds)
if (length(missing)) {
  stop("kinds not exercised: ", paste(missing, collapse = ", "))
}

cat(sprintf(
  "OK  %s\n    %d bytes | %d questions | %d groups | %d kinds | %d quota | DBVersion %s\n",
  normalizePath(out, winslash = "/"), file.size(out), length(kinds),
  length(spec$groups), length(unique(kinds)), length(spec$quotas), lss$db_version
))

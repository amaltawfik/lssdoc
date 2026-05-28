#' Localized "chrome" strings for the rendered documents
#'
#' The chrome of a rendered review document -- column headers (No,
#' Variable, Type, Mandatory, Filter), row labels (Question,
#' Subquestion, Help, Value), navigation titles (Table of contents,
#' Variable index, Welcome text, End text), MOSAiCH-style type labels
#' (Single choice, Multiple choice, Text, Number, ...), Value
#' descriptors for implicit codings (Y = selected, Numeric input, ...),
#' and audit section labels -- is independent from the survey's
#' content languages. This function returns the full set of chrome
#' strings for a given user-facing language so the renderer can swap
#' them without touching the survey content.
#'
#' Supported languages: `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`.
#' Unknown languages fall back to English with no warning -- the
#' caller is expected to validate the value upstream via
#' [lss_resolve_chrome_lang()].
#'
#' Translation notes:
#' - English is the source language.
#' - French and German are reviewed by the package author.
#' - Spanish and Italian are best-effort and should be reviewed by a
#'   native speaker before publishing an official document.
#'
#' @param lang Length-one character: `"en"`, `"fr"`, `"de"`, `"es"`,
#'   or `"it"`.
#' @return A named list of strings.
#' @keywords internal
#' @noRd
lss_chrome_strings <- function(lang = "en") {
  base <- list(
    # ---- Cover page ----
    cover_subtitle_review = "LimeSurvey questionnaire review",
    cover_subtitle_audit  = "Questionnaire audit report",
    cover_source_file     = "Source file",
    cover_survey_id       = "Survey ID",
    cover_languages       = "Languages",
    cover_groups          = "Groups",
    cover_questions       = "Questions",
    cover_subquestions    = "Subquestions",
    cover_answer_options  = "Answer options",
    cover_last_modified   = "Last modified",
    cover_generated       = "Generated",
    # ---- Navigation ----
    toc_title             = "Table of contents",
    welcome_text_title    = "Welcome text",
    end_text_title        = "End text",
    variable_index_title  = "Variable index",
    # ---- Meta table headers ----
    meta_no               = "No",
    meta_variable         = "Variable",
    meta_type             = "Type",
    meta_mandatory        = "Mandatory",
    meta_filter           = "Filter",
    # ---- Item table row labels ----
    item_language         = "Language",
    item_question         = "Question",
    item_subquestion      = "Subquestion",
    item_help             = "Help",
    item_value            = "Value",
    item_value_scale_fmt  = "Value (scale %d)",
    item_exclusive        = "Exclusive",
    # ---- Mandatory values ----
    # Sentence case to match the row label style used everywhere else
    # in the document ("Question", "Value", "Help", ...). The previous
    # lowercase form clashed with that convention.
    mandatory_yes         = "Yes",
    mandatory_no          = "No",
    mandatory_soft        = "Soft",
    # ---- Filter values ----
    filter_all            = "All",
    # ---- Type labels (MOSAiCH-style) ----
    type_single_choice              = "Single choice",
    type_single_choice_with_comment = "Single choice with comment",
    type_multiple_choice            = "Multiple choice",
    type_multiple_choice_with_comment = "Multiple choice with comment",
    type_text             = "Text",
    type_text_short       = "Text (short)",
    type_text_long        = "Text (long)",
    type_text_other       = "Text (other)",
    type_number           = "Number",
    type_date             = "Date",
    type_ranking          = "Ranking",
    type_file_upload      = "File upload",
    type_computed         = "Computed",
    type_display          = "Display",
    # ---- Value implicit descriptors ----
    value_multi_y_blank               = "Y = selected, blank = not selected",
    value_multi_y_blank_with_comment  = "Y = selected, blank = not selected (plus a `<subq>comment` text variable)",
    value_yes_no                      = "Y = Yes, N = No",
    value_gender                      = "M = Male, F = Female",
    value_5point                      = "1, 2, 3, 4, 5 (1 = lowest, 5 = highest)",
    value_numeric_input               = "Numeric input",
    value_free_text_short             = "Free text (single line, short)",
    value_free_text                   = "Free text (multi-line)",
    value_free_text_long              = "Free text (multi-line, long)",
    value_date_input                  = "Date input",
    value_computed                    = "Computed expression (server-side)",
    value_ranking                     = "Ranking (positions assigned to subquestions)",
    value_file_upload                 = "File upload",
    # ---- Exclusive row template ----
    exclusive_text_fmt    = "When checked, clears all other selections of %s",
    # ---- Audit section ----
    audit_findings_title  = "Audit findings",
    audit_no_anomalies    = "No anomalies detected.",
    audit_summary_fmt     = "%d finding(s): %d error(s), %d warning(s), %d note(s).",
    audit_col_severity    = "Severity",
    audit_col_check       = "Check",
    audit_col_location    = "Location",
    audit_col_language    = "Language",
    audit_col_message     = "Message",
    audit_severity_error  = "error",
    audit_severity_warning = "warning",
    audit_severity_note   = "note",
    # ---- ORCID prefix in author block ----
    orcid_label           = "ORCID"
  )

  fr <- list(
    cover_subtitle_review = "Revue du questionnaire LimeSurvey",
    cover_subtitle_audit  = "Rapport d'audit du questionnaire",
    cover_source_file     = "Fichier source",
    cover_survey_id       = "Identifiant de l'enquête",
    cover_languages       = "Langues",
    cover_groups          = "Groupes",
    cover_questions       = "Questions",
    cover_subquestions    = "Sous-questions",
    cover_answer_options  = "Modalités de réponse",
    cover_last_modified   = "Dernière modification",
    cover_generated       = "Généré",
    toc_title             = "Table des matières",
    welcome_text_title    = "Texte d'accueil",
    end_text_title        = "Texte de fin",
    variable_index_title  = "Index des variables",
    meta_no               = "N°",
    meta_variable         = "Variable",
    meta_type             = "Type",
    meta_mandatory        = "Obligatoire",
    meta_filter           = "Filtre",
    item_language         = "Langue",
    item_question         = "Question",
    item_subquestion      = "Sous-question",
    item_help             = "Aide",
    item_value            = "Valeur",
    item_value_scale_fmt  = "Valeur (échelle %d)",
    item_exclusive        = "Exclusif",
    mandatory_yes         = "Oui",
    mandatory_no          = "Non",
    # "Souple" plutôt que "Doux" : LimeSurvey "soft mandatory" =
    # obligation assouplie (le formulaire avertit mais autorise la
    # soumission), pas "gentil".
    mandatory_soft        = "Souple",
    filter_all            = "Toutes",
    type_single_choice              = "Choix unique",
    type_single_choice_with_comment = "Choix unique avec commentaire",
    type_multiple_choice            = "Choix multiple",
    type_multiple_choice_with_comment = "Choix multiple avec commentaire",
    type_text             = "Texte",
    type_text_short       = "Texte (court)",
    type_text_long        = "Texte (long)",
    type_text_other       = "Texte (autre)",
    type_number           = "Nombre",
    type_date             = "Date",
    type_ranking          = "Classement",
    type_file_upload      = "Téléversement de fichier",
    type_computed         = "Calcul",
    type_display          = "Affichage",
    value_multi_y_blank               = "Y = sélectionné, vide = non sélectionné",
    value_multi_y_blank_with_comment  = "Y = sélectionné, vide = non sélectionné (plus une variable texte `<subq>comment`)",
    value_yes_no                      = "Y = Oui, N = Non",
    value_gender                      = "M = Homme, F = Femme",
    value_5point                      = "1, 2, 3, 4, 5 (1 = plus bas, 5 = plus haut)",
    value_numeric_input               = "Saisie numérique",
    value_free_text_short             = "Texte libre (une ligne, court)",
    value_free_text                   = "Texte libre (multi-ligne)",
    value_free_text_long              = "Texte libre (multi-ligne, long)",
    value_date_input                  = "Saisie de date",
    value_computed                    = "Expression calculée (côté serveur)",
    value_ranking                     = "Classement (positions attribuées aux sous-questions)",
    value_file_upload                 = "Téléversement de fichier",
    exclusive_text_fmt    = "Si coché, efface toutes les autres sélections de %s",
    audit_findings_title  = "Constats d'audit",
    audit_no_anomalies    = "Aucune anomalie détectée.",
    audit_summary_fmt     = "%d constat(s) : %d erreur(s), %d avertissement(s), %d note(s).",
    audit_col_severity    = "Gravité",
    audit_col_check       = "Contrôle",
    audit_col_location    = "Localisation",
    audit_col_language    = "Langue",
    audit_col_message     = "Message",
    audit_severity_error  = "erreur",
    audit_severity_warning = "avertissement",
    audit_severity_note   = "note",
    orcid_label           = "ORCID"
  )

  de <- list(
    cover_subtitle_review = "Prüfung des LimeSurvey-Fragebogens",
    cover_subtitle_audit  = "Audit-Bericht zum Fragebogen",
    cover_source_file     = "Quelldatei",
    cover_survey_id       = "Umfrage-ID",
    cover_languages       = "Sprachen",
    cover_groups          = "Gruppen",
    cover_questions       = "Fragen",
    cover_subquestions    = "Teilfragen",
    cover_answer_options  = "Antwortoptionen",
    cover_last_modified   = "Zuletzt geändert",
    cover_generated       = "Erstellt",
    toc_title             = "Inhaltsverzeichnis",
    welcome_text_title    = "Begrüßungstext",
    end_text_title        = "Abschlusstext",
    variable_index_title  = "Variablenverzeichnis",
    meta_no               = "Nr.",
    meta_variable         = "Variable",
    meta_type             = "Typ",
    meta_mandatory        = "Pflichtfeld",
    meta_filter           = "Filter",
    item_language         = "Sprache",
    item_question         = "Frage",
    item_subquestion      = "Teilfrage",
    item_help             = "Hilfe",
    item_value            = "Wert",
    item_value_scale_fmt  = "Wert (Skala %d)",
    item_exclusive        = "Exklusiv",
    mandatory_yes         = "Ja",
    mandatory_no          = "Nein",
    mandatory_soft        = "Weich",
    filter_all            = "Alle",
    type_single_choice              = "Einfachauswahl",
    type_single_choice_with_comment = "Einfachauswahl mit Kommentar",
    type_multiple_choice            = "Mehrfachauswahl",
    type_multiple_choice_with_comment = "Mehrfachauswahl mit Kommentar",
    type_text             = "Text",
    type_text_short       = "Text (kurz)",
    type_text_long        = "Text (lang)",
    type_text_other       = "Text (Sonstige)",
    type_number           = "Zahl",
    type_date             = "Datum",
    type_ranking          = "Rangordnung",
    type_file_upload      = "Datei-Upload",
    type_computed         = "Berechnet",
    type_display          = "Anzeige",
    value_multi_y_blank               = "Y = ausgewählt, leer = nicht ausgewählt",
    value_multi_y_blank_with_comment  = "Y = ausgewählt, leer = nicht ausgewählt (plus eine Textvariable `<subq>comment`)",
    value_yes_no                      = "Y = Ja, N = Nein",
    value_gender                      = "M = Männlich, F = Weiblich",
    value_5point                      = "1, 2, 3, 4, 5 (1 = niedrigster, 5 = höchster)",
    value_numeric_input               = "Numerische Eingabe",
    value_free_text_short             = "Freitext (einzeilig, kurz)",
    value_free_text                   = "Freitext (mehrzeilig)",
    value_free_text_long              = "Freitext (mehrzeilig, lang)",
    value_date_input                  = "Datumseingabe",
    value_computed                    = "Berechneter Ausdruck (serverseitig)",
    value_ranking                     = "Rangordnung (Positionen den Teilfragen zugewiesen)",
    value_file_upload                 = "Datei-Upload",
    exclusive_text_fmt    = "Wenn aktiviert, werden alle anderen Auswahlen von %s gelöscht",
    audit_findings_title  = "Audit-Befunde",
    audit_no_anomalies    = "Keine Anomalien festgestellt.",
    audit_summary_fmt     = "%d Befund(e): %d Fehler, %d Warnung(en), %d Hinweis(e).",
    audit_col_severity    = "Schweregrad",
    audit_col_check       = "Prüfung",
    audit_col_location    = "Ort",
    audit_col_language    = "Sprache",
    audit_col_message     = "Meldung",
    audit_severity_error  = "Fehler",
    audit_severity_warning = "Warnung",
    audit_severity_note   = "Hinweis",
    orcid_label           = "ORCID"
  )

  # Spanish and Italian: best-effort translations. Reviewers should
  # validate before using in an official publication.
  es <- list(
    cover_subtitle_review = "Revisión del cuestionario LimeSurvey",
    cover_subtitle_audit  = "Informe de auditoría del cuestionario",
    cover_source_file     = "Archivo de origen",
    cover_survey_id       = "Identificador de la encuesta",
    cover_languages       = "Idiomas",
    cover_groups          = "Grupos",
    cover_questions       = "Preguntas",
    cover_subquestions    = "Subpreguntas",
    cover_answer_options  = "Opciones de respuesta",
    cover_last_modified   = "Última modificación",
    cover_generated       = "Generado",
    toc_title             = "Índice",
    welcome_text_title    = "Texto de bienvenida",
    end_text_title        = "Texto de cierre",
    variable_index_title  = "Índice de variables",
    meta_no               = "N.º",
    meta_variable         = "Variable",
    meta_type             = "Tipo",
    meta_mandatory        = "Obligatorio",
    meta_filter           = "Filtro",
    item_language         = "Idioma",
    item_question         = "Pregunta",
    item_subquestion      = "Subpregunta",
    item_help             = "Ayuda",
    item_value            = "Valor",
    item_value_scale_fmt  = "Valor (escala %d)",
    item_exclusive        = "Exclusivo",
    mandatory_yes         = "Sí",
    mandatory_no          = "No",
    mandatory_soft        = "Suave",
    filter_all            = "Todas",
    type_single_choice              = "Opción única",
    type_single_choice_with_comment = "Opción única con comentario",
    type_multiple_choice            = "Opción múltiple",
    type_multiple_choice_with_comment = "Opción múltiple con comentario",
    type_text             = "Texto",
    type_text_short       = "Texto (corto)",
    type_text_long        = "Texto (largo)",
    type_text_other       = "Texto (otro)",
    type_number           = "Número",
    type_date             = "Fecha",
    type_ranking          = "Ordenación",
    type_file_upload      = "Subida de archivo",
    type_computed         = "Calculado",
    type_display          = "Visualización",
    value_multi_y_blank               = "Y = seleccionado, vacío = no seleccionado",
    value_multi_y_blank_with_comment  = "Y = seleccionado, vacío = no seleccionado (más una variable de texto `<subq>comment`)",
    value_yes_no                      = "Y = Sí, N = No",
    value_gender                      = "M = Hombre, F = Mujer",
    value_5point                      = "1, 2, 3, 4, 5 (1 = más bajo, 5 = más alto)",
    value_numeric_input               = "Entrada numérica",
    value_free_text_short             = "Texto libre (una línea, corto)",
    value_free_text                   = "Texto libre (varias líneas)",
    value_free_text_long              = "Texto libre (varias líneas, largo)",
    value_date_input                  = "Entrada de fecha",
    value_computed                    = "Expresión calculada (lado servidor)",
    value_ranking                     = "Ordenación (posiciones asignadas a subpreguntas)",
    value_file_upload                 = "Subida de archivo",
    exclusive_text_fmt    = "Si se marca, borra todas las demás selecciones de %s",
    audit_findings_title  = "Hallazgos de auditoría",
    audit_no_anomalies    = "No se detectaron anomalías.",
    audit_summary_fmt     = "%d hallazgo(s): %d error(es), %d advertencia(s), %d nota(s).",
    audit_col_severity    = "Gravedad",
    audit_col_check       = "Comprobación",
    audit_col_location    = "Ubicación",
    audit_col_language    = "Idioma",
    audit_col_message     = "Mensaje",
    audit_severity_error  = "error",
    audit_severity_warning = "advertencia",
    audit_severity_note   = "nota",
    orcid_label           = "ORCID"
  )

  it <- list(
    cover_subtitle_review = "Revisione del questionario LimeSurvey",
    cover_subtitle_audit  = "Rapporto di audit del questionario",
    cover_source_file     = "File sorgente",
    cover_survey_id       = "Identificativo dell'indagine",
    cover_languages       = "Lingue",
    cover_groups          = "Gruppi",
    cover_questions       = "Domande",
    cover_subquestions    = "Sotto-domande",
    cover_answer_options  = "Opzioni di risposta",
    cover_last_modified   = "Ultima modifica",
    cover_generated       = "Generato",
    toc_title             = "Indice",
    welcome_text_title    = "Testo di benvenuto",
    end_text_title        = "Testo di chiusura",
    variable_index_title  = "Indice delle variabili",
    meta_no               = "N.",
    meta_variable         = "Variabile",
    meta_type             = "Tipo",
    meta_mandatory        = "Obbligatoria",
    meta_filter           = "Filtro",
    item_language         = "Lingua",
    item_question         = "Domanda",
    item_subquestion      = "Sotto-domanda",
    item_help             = "Aiuto",
    item_value            = "Valore",
    item_value_scale_fmt  = "Valore (scala %d)",
    item_exclusive        = "Esclusiva",
    mandatory_yes         = "Sì",
    mandatory_no          = "No",
    mandatory_soft        = "Morbido",
    filter_all            = "Tutte",
    type_single_choice              = "Scelta singola",
    type_single_choice_with_comment = "Scelta singola con commento",
    type_multiple_choice            = "Scelta multipla",
    type_multiple_choice_with_comment = "Scelta multipla con commento",
    type_text             = "Testo",
    type_text_short       = "Testo (breve)",
    type_text_long        = "Testo (lungo)",
    type_text_other       = "Testo (altro)",
    type_number           = "Numero",
    type_date             = "Data",
    type_ranking          = "Classifica",
    type_file_upload      = "Caricamento file",
    type_computed         = "Calcolato",
    type_display          = "Visualizzazione",
    value_multi_y_blank               = "Y = selezionato, vuoto = non selezionato",
    value_multi_y_blank_with_comment  = "Y = selezionato, vuoto = non selezionato (più una variabile di testo `<subq>comment`)",
    value_yes_no                      = "Y = Sì, N = No",
    value_gender                      = "M = Uomo, F = Donna",
    value_5point                      = "1, 2, 3, 4, 5 (1 = più basso, 5 = più alto)",
    value_numeric_input               = "Inserimento numerico",
    value_free_text_short             = "Testo libero (una riga, breve)",
    value_free_text                   = "Testo libero (multi-riga)",
    value_free_text_long              = "Testo libero (multi-riga, lungo)",
    value_date_input                  = "Inserimento data",
    value_computed                    = "Espressione calcolata (lato server)",
    value_ranking                     = "Classifica (posizioni assegnate alle sotto-domande)",
    value_file_upload                 = "Caricamento file",
    exclusive_text_fmt    = "Se selezionata, cancella tutte le altre selezioni di %s",
    audit_findings_title  = "Risultati dell'audit",
    audit_no_anomalies    = "Nessuna anomalia rilevata.",
    audit_summary_fmt     = "%d risultato(i): %d errore(i), %d avvertimento(i), %d nota(e).",
    audit_col_severity    = "Gravità",
    audit_col_check       = "Controllo",
    audit_col_location    = "Posizione",
    audit_col_language    = "Lingua",
    audit_col_message     = "Messaggio",
    audit_severity_error  = "errore",
    audit_severity_warning = "avvertimento",
    audit_severity_note   = "nota",
    orcid_label           = "ORCID"
  )

  pack <- switch(
    lang,
    "en" = base, "fr" = fr, "de" = de, "es" = es, "it" = it,
    base
  )
  pack
}

#' Localized methodological type label for a question
#'
#' Maps the question's LimeSurvey type code (and optional theme name
#' fallback) to the appropriate `theme$chrome$type_*` string. The
#' methodological collapse is the same as [lss_methodological_label()]
#' (List/Array/Y/G/5 -> Single choice, M/P -> Multiple choice, ...);
#' the localized string replaces the English one at the call site so
#' the meta-table reads in the chrome language.
#'
#' @keywords internal
#' @noRd
lss_localized_type_label <- function(q, theme) {
  chrome <- theme$chrome
  key <- switch(
    as.character(q$type),
    "L" = "type_single_choice", "!" = "type_single_choice",
    "Y" = "type_single_choice", "G" = "type_single_choice",
    "5" = "type_single_choice", "F" = "type_single_choice",
    "1" = "type_single_choice", "A" = "type_single_choice",
    "B" = "type_single_choice", "C" = "type_single_choice",
    "E" = "type_single_choice", "H" = "type_single_choice",
    ":" = "type_single_choice",
    "O" = "type_single_choice_with_comment",
    "M" = "type_multiple_choice",
    "P" = "type_multiple_choice_with_comment",
    "S" = "type_text_short",
    "T" = "type_text",
    "U" = "type_text_long",
    "Q" = "type_text",
    ";" = "type_text",
    "N" = "type_number",
    "K" = "type_number",
    "D" = "type_date",
    "R" = "type_ranking",
    "|" = "type_file_upload",
    "*" = "type_computed",
    "X" = "type_display",
    "I" = "type_display",
    NA_character_
  )
  if (!is.na(key)) return(chrome[[key]])
  # Fall back to the type label baked at model-build time (English
  # methodological label) so unknown plugin types still appear with
  # their best-effort name.
  q$type_label
}

#' Resolve and validate the `chrome_lang` argument
#'
#' Returns one of `"en"`, `"fr"`, `"de"`, `"es"`, `"it"`. When `NULL`,
#' falls back to the first content language if it is supported,
#' otherwise to English. Unknown explicit values are rejected with a
#' classed condition so callers can catch them programmatically.
#'
#' @keywords internal
#' @noRd
lss_resolve_chrome_lang <- function(chrome_lang, content_languages) {
  supported <- c("en", "fr", "de", "es", "it")
  if (is.null(chrome_lang)) {
    primary <- content_languages[1L]
    if (!is.na(primary) && primary %in% supported) return(primary)
    return("en")
  }
  if (!is.character(chrome_lang) || length(chrome_lang) != 1L ||
      is.na(chrome_lang)) {
    lssdoc_abort(
      c(
        "{.arg chrome_lang} must be {.code NULL} or one of {.val {supported}}.",
        "i" = "Got {.val {chrome_lang}}."
      ),
      class = "lssdoc_bad_chrome_lang"
    )
  }
  if (!chrome_lang %in% supported) {
    lssdoc_abort(
      c(
        "{.arg chrome_lang} {.val {chrome_lang}} is not supported.",
        "i" = "Supported values: {.val {supported}}."
      ),
      class = "lssdoc_bad_chrome_lang"
    )
  }
  chrome_lang
}

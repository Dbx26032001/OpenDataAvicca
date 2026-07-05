#' code pour ouvrir et faire les premiers traitements sur le fichier trajectoire d'Orange.
#' Notamment les fusions entre les arrondissements de Paris, Marseille, Lyon
#' @export

process_orange <- function(file_path) {


  sheets <- readxl::excel_sheets(file_path)

  sheets_lower <- tolower(sheets)

  if ("communes" %in% sheets_lower) {
    feuille_utiliser <- sheets[which(sheets_lower == "communes")[1]]
  } else {
    stop("Aucune feuille 'Communes' trouvée dans le fichier d'Orange")
  }

  orange_data <- readxl::read_excel(file_path, sheet = feuille_utiliser)

  cols <- c(
    "code_insee","lot","report_FC",
    "annonce_ferm_commerciale","annonce_ferm_technique",
    "fermeture_technique_initiale","fermeture_commerciale",
    "fermeture_technique",
    "prevision_completude_des_zapms_premierpm",
    "prevision_completude_des_zapms_dernierpm",
    "eligibilite_ftth",
    "nbr_log_refus_tiers",
    "nb_log_blocage_eligibilite"
  )

  orange_data <- orange_data[, intersect(cols, names(orange_data))] # Verifier si les colonnes dans la liste sont dans le fichier trajectoire d'Orange

  # sécurisation colonnes manquantes

  for (c in cols) if (!c %in% names(orange_data)) orange_data[[c]] <- NA




  # Fusion des arrondissements de paris pour avoir une ligne pour paris comme dans les bases ARCEP et Open data Avicca

  paris_cuivre <- orange_data %>%
    dplyr::filter(as.integer(code_insee) >= 75101 & as.integer(code_insee) <=75120) %>%
    dplyr::summarise(
      code_insee = "75056",
      lot = paste(
        sort(unique(lot[!is.na(lot)])),
        collapse = " / "
      ),
      report_FC=ifelse("Oui"%in% unique(report_FC), "Oui",ifelse(unique(report_FC)==c(NA),NA, "Non")),
      annonce_ferm_commerciale=max(annonce_ferm_commerciale, na.rm = TRUE),
      annonce_ferm_technique=max(annonce_ferm_technique, na.rm = TRUE),
      fermeture_technique_initiale=max(fermeture_technique_initiale, na.rm = TRUE),
      fermeture_commerciale=max(fermeture_commerciale, na.rm = TRUE),
      fermeture_technique=max(fermeture_technique, na.rm = TRUE),
      prevision_completude_des_zapms_premierpm=max(prevision_completude_des_zapms_premierpm, na.rm = TRUE),
      prevision_completude_des_zapms_dernierpm=max(prevision_completude_des_zapms_dernierpm, na.rm = TRUE),
      eligibilite_ftth=ifelse("Oui"%in% unique(eligibilite_ftth), "Oui", ifelse(unique(eligibilite_ftth)==c(NA),NA, "Non")),
      nbr_log_refus_tiers = sum(nbr_log_refus_tiers, na.rm=TRUE),
      nb_log_blocage_eligibilite = sum(nb_log_blocage_eligibilite, na.rm=TRUE),
      .groups = "drop"
    )


  # Fusion des arrondissements de Lyon  pour avoir une ligne pour Lyon comme dans les bases ARCEP et Open data Avicca


  lyon_cuivre <- orange_data %>%
    dplyr::filter(as.integer(code_insee) >= 69381 & as.integer(code_insee) <=69389) %>%
    dplyr::summarise(
      code_insee = "69123",
      lot = paste(
        sort(unique(lot[!is.na(lot)])),
        collapse = " / "
      ),
      report_FC=ifelse("Oui"%in% unique(report_FC), "Oui",ifelse(unique(report_FC)==c(NA),NA, "Non")),
      annonce_ferm_commerciale=max(annonce_ferm_commerciale, na.rm = TRUE),
      annonce_ferm_technique=max(annonce_ferm_technique, na.rm = TRUE),
      fermeture_technique_initiale=max(fermeture_technique_initiale, na.rm = TRUE),
      fermeture_commerciale=max(fermeture_commerciale, na.rm = TRUE),
      fermeture_technique=max(fermeture_technique, na.rm = TRUE),
      prevision_completude_des_zapms_premierpm=max(prevision_completude_des_zapms_premierpm, na.rm = TRUE),
      prevision_completude_des_zapms_dernierpm=max(prevision_completude_des_zapms_dernierpm, na.rm = TRUE),
      eligibilite_ftth=ifelse("Oui"%in% unique(eligibilite_ftth), "Oui", ifelse(unique(eligibilite_ftth)==c(NA),NA, "Non")),
      nbr_log_refus_tiers = sum(nbr_log_refus_tiers, na.rm=TRUE),
      nb_log_blocage_eligibilite = sum(nb_log_blocage_eligibilite, na.rm=TRUE),
      .groups = "drop"
    )


  # Fusion des arrondissements de Marseille  pour avoir une ligne pour Marseille comme dans les bases ARCEP et Open data Avicca


  marseille_cuivre <- orange_data %>%
    dplyr::filter(as.integer(code_insee) >= 13201 & as.integer(code_insee) <= 13216) %>%
    dplyr::summarise(
      code_insee = "13055",
      lot = paste(
        sort(unique(lot[!is.na(lot)])),
        collapse = " / "
      ),
      report_FC=ifelse("Oui"%in% unique(report_FC), "Oui",ifelse(unique(report_FC)==c(NA),NA, "Non")),
      annonce_ferm_commerciale=max(annonce_ferm_commerciale, na.rm = TRUE),
      annonce_ferm_technique=max(annonce_ferm_technique, na.rm = TRUE),
      fermeture_technique_initiale=max(fermeture_technique_initiale, na.rm = TRUE),
      fermeture_commerciale=max(fermeture_commerciale, na.rm = TRUE),
      fermeture_technique=max(fermeture_technique, na.rm = TRUE),
      prevision_completude_des_zapms_premierpm=max(prevision_completude_des_zapms_premierpm, na.rm = TRUE),
      prevision_completude_des_zapms_dernierpm=max(prevision_completude_des_zapms_dernierpm, na.rm = TRUE),
      eligibilite_ftth=ifelse("Oui"%in% unique(eligibilite_ftth), "Oui", ifelse(unique(eligibilite_ftth)==c(NA),NA, "Non")),
      nbr_log_refus_tiers = sum(nbr_log_refus_tiers, na.rm=TRUE),
      nb_log_blocage_eligibilite = sum(nb_log_blocage_eligibilite, na.rm=TRUE),
      .groups = "drop"
    )


  # Ajouter les nouvelles lignes Paris, Lyon et Marseille dans la base orange

  orange_data=rbind(orange_data, paris_cuivre, lyon_cuivre, marseille_cuivre)


  # Renommer le nom des variables pour différentier avec les colonnes potenitiellement déjà existantes dans l'open data Avicca

  orange_data <- orange_data%>%
    dplyr::rename(source_orange_lot = lot,
           source_orange_report_FC=report_FC,
           source_orange_annonce_ferm_commerciale=annonce_ferm_commerciale,
           source_orange_annonce_ferm_technique=annonce_ferm_technique,
           source_orange_fermeture_technique_initiale=fermeture_technique_initiale,
           source_orange_fermeture_commerciale=fermeture_commerciale,
           source_orange_fermeture_technique=fermeture_technique,
           source_orange_prevision_completude_des_zapms_premierpm=prevision_completude_des_zapms_premierpm,
           source_orange_prevision_completude_des_zapms_dernierpm=prevision_completude_des_zapms_dernierpm,
           source_orange_eligibilite_ftth=eligibilite_ftth,
           source_orange_nbr_log_refus_tiers = nbr_log_refus_tiers,
           source_orange_nb_log_blocage_eligibilite = nb_log_blocage_eligibilite)%>%
    dplyr::mutate(
      source_orange_annonce_ferm_commerciale = as.Date(source_orange_annonce_ferm_commerciale, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      source_orange_annonce_ferm_technique = as.Date(source_orange_annonce_ferm_technique, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      source_orange_fermeture_technique_initiale = as.Date(source_orange_fermeture_technique_initiale, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      source_orange_fermeture_commerciale = as.Date(source_orange_fermeture_commerciale, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      source_orange_fermeture_technique = as.Date(source_orange_fermeture_technique, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      source_orange_prevision_completude_des_zapms_premierpm = as.Date(source_orange_prevision_completude_des_zapms_premierpm, tryFormats = c("%d/%m/%Y", "%Y-%m-%d")),
      source_orange_prevision_completude_des_zapms_dernierpm = as.Date(source_orange_prevision_completude_des_zapms_dernierpm, tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
    )

  # Retourne le fichier trajectoire d'Orange avec tous les pre-traitements effectués
  orange_data
}

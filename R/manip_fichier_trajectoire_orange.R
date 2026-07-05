#' code pour ouvrir et faire les premiers traitements sur le fichier trajectoire d'Orange.
#' Notamment les fusions entre les arrondissements de Paris, Marseille, Lyon
#' @export



process_orange <- function(file_path) {

  sheets <- readxl::excel_sheets(file_path)
  sheets_lower <- tolower(sheets)

  feuille_utiliser <- sheets[match("communes", sheets_lower)]

  orange_data <- readxl::read_excel(file_path, sheet = feuille_utiliser)

  # ===================================
  # Colonnes à prendre pour la jointure
  # ===================================

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

  orange_data <- orange_data[, intersect(cols, names(orange_data))]

  # sécurisation colonnes manquantes (vectorisé)
  missing_cols <- setdiff(cols, names(orange_data))
  for (c in missing_cols) orange_data[[c]] <- NA

  # =========================
  # CODE INSEE NUMÉRIQUE UNE SEULE FOIS
  # =========================

  code <- as.integer(orange_data$code_insee)

  # =================================================================================================================
  # FONCTION DE FUSION GENERIQUE pour mettre ensemble les arrondissements de paris, de même pour marseille et lyon
  # ==================================================================================================================

  fusion_ville <- function(df, idx, new_code) {

    sub <- df[idx, , drop = FALSE]

    sub$code_insee <- new_code

    sub$lot <- paste(sort(unique(sub$lot[!is.na(sub$lot)])), collapse = " / ")

    sub$report_FC <- if ("Oui" %in% sub$report_FC) {
      "Oui"
    } else if (all(is.na(sub$report_FC))) {
      NA
    } else {
      "Non"
    }

    num_cols <- c(
      "annonce_ferm_commerciale","annonce_ferm_technique",
      "fermeture_technique_initiale","fermeture_commerciale",
      "fermeture_technique",
      "prevision_completude_des_zapms_premierpm",
      "prevision_completude_des_zapms_dernierpm"
    )

    for (col in num_cols) {
      sub[[col]] <- max(sub[[col]], na.rm = TRUE)
    }

    sub$eligibilite_ftth <- if ("Oui" %in% sub$eligibilite_ftth) {
      "Oui"
    } else if (all(is.na(sub$eligibilite_ftth))) {
      NA
    } else {
      "Non"
    }

    sub$nbr_log_refus_tiers <- sum(sub$nbr_log_refus_tiers, na.rm = TRUE)
    sub$nb_log_blocage_eligibilite <- sum(sub$nb_log_blocage_eligibilite, na.rm = TRUE)

    sub
  }

  # =========================
  # INDEX VECTORISÉS
  # =========================

  paris_idx    <- code >= 75101 & code <= 75120
  lyon_idx     <- code >= 69381 & code <= 69389
  marseille_idx <- code >= 13201 & code <= 13216

  paris_cuivre <- fusion_ville(orange_data, paris_idx, "75056")
  lyon_cuivre  <- fusion_ville(orange_data, lyon_idx, "69123")
  marseille_cuivre <- fusion_ville(orange_data, marseille_idx, "13055")

  # =======================================
  # AJOUT LIGNES : paris, marseille, lyon
  # =======================================

  orange_data <- rbind(orange_data, paris_cuivre, lyon_cuivre, marseille_cuivre)

  # =====================================================
  # RENAME pour eviter la confusion avec open data avicca
  # =====================================================

  names(orange_data) <- sub("^lot$", "source_orange_lot", names(orange_data))
  names(orange_data) <- sub("^report_FC$", "source_orange_report_FC", names(orange_data))
  names(orange_data) <- sub("^annonce_ferm_", "source_orange_annonce_ferm_", names(orange_data))
  names(orange_data) <- sub("^fermeture_", "source_orange_fermeture_", names(orange_data))
  names(orange_data) <- sub("^prevision_", "source_orange_prevision_", names(orange_data))
  names(orange_data) <- sub("^eligibilite_ftth$", "source_orange_eligibilite_ftth", names(orange_data))
  names(orange_data) <- sub("^nbr_log_refus_tiers$", "source_orange_nbr_log_refus_tiers", names(orange_data))
  names(orange_data) <- sub("^nb_log_blocage_eligibilite$", "source_orange_nb_log_blocage_eligibilite", names(orange_data))

  # =========================
  # DATES : format dates
  # =========================

  date_cols <- grep("annonce|fermeture|prevision", names(orange_data), value = TRUE)

  orange_data[date_cols] <- lapply(orange_data[date_cols], function(x) {
    as.Date(x, tryFormats = c("%d/%m/%Y", "%Y-%m-%d"))
  })

  orange_data
}

#' Ce code permet de produire les resumés statistiques pour faire les graphs sur flourish ainsi que certaines cartes notamment les cartes niveau départemental
#' @export


prod_stats <- function(avicca, departements, orange_data=NULL){

  # S'assurer que les codes de département soient codés sur deux caractères afin de faciliter les jointures

  avicca <- avicca %>%
    dplyr::mutate(`Code département` = stringr::str_pad(as.character(`Code département`), width = 2, pad = "0"))


  departements <- departements%>%
    dplyr::mutate(`Code département` = stringr::str_pad(as.character(`Code département`), width = 2, pad = "0"))


  avicca <- dplyr::left_join(avicca, departements, by = "Code département")



  trimestres <- grep("^T[1-4] \\d{4}$", names(avicca), value = TRUE) # liste des Colonnes des locaux raccordables (T4 2017,...T1 2024, ...)



  locaux_racc_nouveau_trimestre <- trimestres[length(trimestres)] # colonne des locaux raccordables du nouveau trimestre publié



  base_sans_en_cours_construction <- paste0("Base sans locaux en cours de construction ", locaux_racc_nouveau_trimestre) # nom de la colonne base deslocaux sans construction


  nouveau_taux_raccordable <- paste0("%", locaux_racc_nouveau_trimestre)



  # =========================================================
  # Début de calcul des statistiques : feuille par feuille
  # =========================================================


  # Feuille 1-  Stats DEP - ZIPU : Statistiques du dernier trimestre sur les locaux raccordables et la base sans construction niveau départemental pour les ZIPU


  data_niveau_departement_zipu <- avicca %>%
    dplyr::filter(`Répartition public/privé` != "ZIPRI") %>%
    dplyr::group_by(`Code département`) %>%
    dplyr::summarise(
      !!base_sans_en_cours_construction := sum(.data[[base_sans_en_cours_construction]], na.rm = TRUE),
      !!locaux_racc_nouveau_trimestre := sum(.data[[locaux_racc_nouveau_trimestre]], na.rm = TRUE),
      `Nom département` = unique(`Nom département`)) %>%
    dplyr::select(`Code département`,`Nom département`, base_sans_en_cours_construction, all_of(locaux_racc_nouveau_trimestre))





  # Feuille 2- Stats DEP - ZIPRI : Statistiques du dernier trimestre sur les locaux raccordables et la base sans construction niveau départemental pour les ZIPRI


  data_niveau_departement_zipri <- avicca %>%
    dplyr::filter(`Répartition public/privé` == "ZIPRI") %>%
    dplyr::group_by(`Code département`) %>%
    dplyr::summarise(
      !!base_sans_en_cours_construction := sum(.data[[base_sans_en_cours_construction]], na.rm = TRUE),
      !!locaux_racc_nouveau_trimestre := sum(.data[[locaux_racc_nouveau_trimestre]], na.rm = TRUE),
      `Nom département` = unique(`Nom département`)) %>%
    dplyr::select(`Code département`,`Nom département`, base_sans_en_cours_construction, all_of(locaux_racc_nouveau_trimestre))



  # Feuille 3- Evol couv FTTH - DEP ZIPRI : Evolution couverture FttH dans les départements ZIPRI


  cols_locaux_raccordables <- names(avicca)[grepl("^T[1-4] \\d{4}$", names(avicca))]

  cols_taux_couverture <- paste0("%", cols_locaux_raccordables)



  vars_name <- names(avicca)

  # Pour conserver une cohérence avec les cartes au niveau ZIPU réalisées depuis le T4 2025.

  if ("Base sans locaux en cours de construction T4 2025" %in% vars_name) {
    base_locaux <- "Base sans locaux en cours de construction T4 2025"
  } else {
    base_locaux <- base_sans_en_cours_construction # Sinon on prend pour le nouveau trimestre
  }



  # Pour conserver une cohérence avec les cartes ZIPU publiées depuis le T4 2025,
  # les pourcentages sont calculés à partir de la base de locaux utilisée lors de
  # ces publications (si elle est disponible). En revanche, l'indicateur global de
  # locaux raccordables (locaux_racc_pourcentage) est ensuite recalculé avec la
  # base de locaux du trimestre courant afin de refléter les données actualisées



  data_evol_couverture_niv_depart_zipri <- avicca %>%
    dplyr::filter(`Répartition public/privé` == "ZIPRI") %>%
    dplyr::group_by(`Code département`) %>%
    dplyr::summarise(
      across(all_of(cols_locaux_raccordables), ~ sum(.x, na.rm = TRUE)),
      !!base_locaux := sum(.data[[base_locaux]], na.rm = TRUE),
      base_locaux_nouveau_trimestre = sum(.data[[base_sans_en_cours_construction]], na.rm = TRUE),
      `Nom département` = unique(`Nom département`),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      across(
        all_of(cols_locaux_raccordables),
        ~ ifelse(
          is.na(.data[[base_locaux]]) | .data[[base_locaux]] == 0,
          NA_real_,
          .x / .data[[base_locaux]]
        ),
        .names = "%{.col}"
      ),
      !!nouveau_taux_raccordable := ifelse(
        is.na(base_locaux_nouveau_trimestre) |
          base_locaux_nouveau_trimestre == 0,
        NA_real_,
        .data[[cols_locaux_raccordables[length(cols_locaux_raccordables)]]] /
          base_locaux_nouveau_trimestre
      )
    )%>%
    dplyr::select(`Code département`, `Nom département`, all_of(cols_taux_couverture))







  # Feuille 4- Evol couv FTTH - DEP ZIPU : Evolution couverture FttH dans les départements ZIPU


  data_evol_couverture_niv_depart_zipu <- avicca %>%
    dplyr::filter(`Répartition public/privé` != "ZIPRI") %>%
    dplyr::group_by(`Code département`) %>%
    dplyr::summarise(
      across(all_of(cols_locaux_raccordables), ~ sum(.x, na.rm = TRUE)),
      !!base_locaux := sum(.data[[base_locaux]], na.rm = TRUE),
      base_locaux_nouveau_trimestre = sum(.data[[base_sans_en_cours_construction]], na.rm = TRUE),
      `Nom département` = unique(`Nom département`),
      .groups = "drop"
    ) %>%
    dplyr::mutate(
      across(
        all_of(cols_locaux_raccordables),
        ~ ifelse(
          is.na(.data[[base_locaux]]) | .data[[base_locaux]] == 0,
          NA_real_,
          .x / .data[[base_locaux]]
        ),
        .names = "%{.col}"
      ),
      !!nouveau_taux_raccordable := ifelse(
        is.na(base_locaux_nouveau_trimestre) |
          base_locaux_nouveau_trimestre == 0,
        NA_real_,
        .data[[cols_locaux_raccordables[length(cols_locaux_raccordables)]]] /
          base_locaux_nouveau_trimestre
      )
    )%>%
    dplyr::select(`Code département`, `Nom département`, all_of(cols_taux_couverture))




  # Feuille 5 - Evol Locaux rac - Zonage : Evolution des prises (locaux) raccordables dans le temps par zonage (RIP, ZTD, ZMD privée)
  # Feuille 6 - Evol couv FTTH - Zonage : Evolution des  taux de locaux raccordables dans le temps par zonage (RIP, ZTD, ZMD privée)

  zone_AMII_AMEL_CPSD <- c("Zone CPSD hors L,33-13 ex RIP", "Zone AMII L,33-13 ex RIP", "Zone AMII L,33-13",
                           "Zone AMEL ex-RIP", "Zone AMII hors L,33-13", "Zone CPSD L,33-13 ex RIP",
                           "Zone AMEL", "Zone AMII hors L,33-13 ex RIP", "Zone CPSD hors L,33-13 ex RIP")


  zone_rip <- c("RIP", "Zone RIP ex AMII",
                "Zone mixte RIP et AMEL", "Zone mixte RIP et AMII L,33-13",
                "Zone mixte RIP et AMII hors L,33-13", "Zone mixte RIP et CPSD")

  zTD <- c("Zone très dense")



  data <- avicca%>%
    dplyr::mutate(zonage = ifelse(`Zonage identifié Avicca` %in% zone_rip, "RIP",
                           ifelse(`Zonage identifié Avicca` %in% zone_AMII_AMEL_CPSD, "ZMD privée",
                                  ifelse(`Zonage identifié Avicca` %in% zTD, "ZTD",""))))


  data_nbre_raccordable_par_zonage <- data%>%
    dplyr::group_by(zonage)%>%
    dplyr::summarise(across(all_of(cols_locaux_raccordables), ~ sum(.x, na.rm = TRUE)))


  data_taux_raccordable_par_zonage <- data%>%
    dplyr::group_by(zonage)%>%
    dplyr::summarise(across(all_of(cols_taux_couverture), ~ mean(.x, na.rm = TRUE)))



  data_taux_raccordable_par_zonage <- data_taux_raccordable_par_zonage %>%
    tidyr::pivot_longer(cols = -zonage, names_to = "trimestre", values_to = "taux_couverture_FttH")


  data_taux_raccordable_par_zonage <- data_taux_raccordable_par_zonage %>%
    dplyr::mutate(
      trimestre = gsub("%T", "", trimestre),
      trimestre = as.yearqtr(trimestre, format = "%q %Y")
    )

  data_taux_raccordable_par_zonage <- data_taux_raccordable_par_zonage %>%
    dplyr::mutate(trimestre = factor(trimestre, levels = unique(trimestre)))


  data_taux_raccordable_par_zonage <- data_taux_raccordable_par_zonage %>%
    tidyr::pivot_wider(
      names_from = zonage,
      values_from = taux_couverture_FttH,
      names_prefix = "couverture_"
    )




  # Feuille 7- Locaux raccordables par OI filtre sur les lots


  # Classification des OI à partir de l'identification faite par Avicca en T4 2023 dans l'Open data

  orange <- c("Orange", "Orange RIP hors Orange Concessions")

  orange_concession <- c("Orange Concessions","Orange concessions")

  axione <- c("Axione")

  altitude_infra <- c("Altitude Infra")

  SFR_xp_fibre <- c("SFR/XP Fibre", "SFR/XP Fibre RIP")

  lumière <- c("Lumière")

  Regies_SPL_SIEA <- c('SPL Orne THD', 'Régie du Pays de Bitche', 'Régie FIBRAGGLO',
                       'Régie FIBRESO', 'Régie du Warndt', "SIeA - Energie & e-Communication de l'Ain")

  plusieurs_opérateurs <- c('Bouygues Telecom + Free + Orange + SFR','Orange + Axione',
                            'Orange + Altitude Infra','Zeop - Reunicable + SFR + Orange')




  data <- data %>%
    dplyr::mutate(operateurs = ifelse(`Opérateur identifié Avicca T4 2023` %in% orange, "Orange",
                               ifelse(`Opérateur identifié Avicca T4 2023` %in% orange_concession, "Orange Concessions",
                                      ifelse(`Opérateur identifié Avicca T4 2023` %in% axione, "Axione",
                                             ifelse(`Opérateur identifié Avicca T4 2023` %in% altitude_infra, "Altitude Infra",
                                                    ifelse(`Opérateur identifié Avicca T4 2023` %in% SFR_xp_fibre, "SFR/XP Fibre",
                                                           ifelse(`Opérateur identifié Avicca T4 2023` %in% lumière, "Lumière",
                                                                  ifelse(`Opérateur identifié Avicca T4 2023` %in% Regies_SPL_SIEA, "Régies/SPL/SIEA",
                                                                         ifelse(`Opérateur identifié Avicca T4 2023` %in% plusieurs_opérateurs, "ZTD", "Autres Opérateurs")))))))))



  if (!is.null(orange_data)) {
    var_lot <- "source_orange_lot"
    var_report <- "source_orange_report_FC"
  } else {
    var_lot <- "Lot de fermeture du réseau cuivre"
    var_report <- "Report FC cuivre"
  }



  data <- data %>%
    dplyr::mutate(
      `Lot FC cuivre` = case_when(

        `Code commune` %in% c("69123", "75056", "13055") ~ case_when(
          `Code commune` == "69123" ~ "Lyon",
          `Code commune` == "75056" ~ "Paris",
          `Code commune` == "13055" ~ "Marseille"
        ),

        .data[[var_lot]] == "ExpeZTD" ~ "ExpeZTD",
        str_detect(.data[[var_lot]], "^6_.*\\d$") ~ "Lot 6",
        str_detect(.data[[var_lot]], "^7_.*\\d$") ~ "Lot 7",
        str_detect(.data[[var_lot]], "^5_.*\\d$") ~ "Lot 5",
        str_detect(.data[[var_lot]], "^4_.*\\d$") ~ "Lot 4",
        .data[[var_lot]] == "3" ~ "Lot 3",
        .data[[var_lot]] %in% c("2", "Expe2") ~ "Lot 2",
        .data[[var_lot]] %in% c("1", "Expe1") ~ "Lot 1",
        .data[[var_lot]] == "PreselectionLot6" ~ "Lot 6",
        .data[[var_lot]] == "PreselectionLot7" ~ "Lot 7",

        TRUE ~ ""
      )
    )





  data_oi_lot_locaux <- data %>%
    dplyr::group_by(operateurs, `Lot FC cuivre`) %>%
    dplyr::summarise(
      !!paste0("locaux raccordables ", locaux_racc_nouveau_trimestre) :=
        sum(.data[[locaux_racc_nouveau_trimestre]], na.rm = TRUE),

      !!paste0("locaux raccordables avec report ", locaux_racc_nouveau_trimestre) :=
        sum(.data[[locaux_racc_nouveau_trimestre]][.data[[var_report]] == "Oui"], na.rm = TRUE),

      !!paste0("locaux raccordables ayant au moins 99% couverture FttH ", locaux_racc_nouveau_trimestre) :=
        sum(.data[[locaux_racc_nouveau_trimestre]][.data[[nouveau_taux_raccordable]] >= 0.99], na.rm = TRUE)
    )





  # Feuille 8 – Locaux raccordables par zonage, filtre par lot (statistiques similaires aux précédentes)


  data_zonage_lot_locaux <- data %>%
    dplyr::group_by(zonage, `Lot FC cuivre`) %>%
    dplyr::summarise(
      !!paste0("locaux raccordables ", locaux_racc_nouveau_trimestre) :=
        sum(.data[[locaux_racc_nouveau_trimestre]], na.rm = TRUE),

      !!paste0("locaux raccordables avec report fc cuivre ", locaux_racc_nouveau_trimestre) :=
        sum(.data[[locaux_racc_nouveau_trimestre]][.data[[var_report]] == "Oui"], na.rm = TRUE),

      !!paste0("locaux raccordables ayant au moins 99% couverture FttH ", locaux_racc_nouveau_trimestre) :=
        sum(.data[[locaux_racc_nouveau_trimestre]][.data[[nouveau_taux_raccordable]] >= 0.99], na.rm = TRUE)
    )




  # Feuille 9 - Locaux rac DEP - report : Locaux raccordables en zones reportées vs zones non reportées (filtre par département)


  nom_col <- paste0("locaux raccordables", " ", locaux_racc_nouveau_trimestre)

  stats_sur_report_niv_dep_1 <- data%>%
    dplyr::group_by(`Code département`, .data[[var_report]])%>%
    dplyr::summarise(!!nom_col := sum(.data[[locaux_racc_nouveau_trimestre]], na.rm=TRUE),
              `Nom département` = unique(`Nom département`))%>%
    dplyr::rename(`Report FC cuivre` = !!sym(var_report)) %>%
    dplyr::select(`Code département`, `Nom département`, `Report FC cuivre`, all_of(nom_col))



  # Feuille 10- Locaux REPORT DEP - zonage : Locaux en zones reportées par zonage (RIP, ZMD privée, ZTD) dans chaque département


  nom_col <- paste0("locaux raccordables en report fc cuivre", " ", locaux_racc_nouveau_trimestre)

  stats_sur_report_niv_dep_2 <- data%>%
    dplyr::filter(.data[[var_report]]=="Oui")%>%
    dplyr::group_by(`Code département`, zonage)%>%
    dplyr::summarise(!!nom_col := sum(.data[[locaux_racc_nouveau_trimestre]], na.rm=TRUE),
              `Nom département` = unique(`Nom département`))%>%
    dplyr::select(`Code département`,`Nom département`, zonage, all_of(nom_col))




  # Classification couverture FttH

  data <- data %>%
    dplyr::mutate(niveau_couverture_ftth = ifelse(.data[[nouveau_taux_raccordable]] < 0.9, "Moins de 90%",
                                           ifelse(.data[[nouveau_taux_raccordable]] < 0.95, "90-95%",
                                                  ifelse(.data[[nouveau_taux_raccordable]] < 0.99, "95-99%",
                                                         "99% et plus"))))



  if(!is.null(orange_data)){

    # Feuille 11- Refus tiers bloc - DEP : Statistiques sur les refus tiers et blocages éligibilité FttH niveau départemental

    stats_bloc_elig_refus_tiers <- data%>%
      dplyr::group_by(`Code département`)%>%
      dplyr::summarise(!!base_sans_en_cours_construction := sum(.data[[base_sans_en_cours_construction]], na.rm = TRUE),
                nbr_log_refus_tiers = sum(source_orange_nbr_log_refus_tiers, na.rm=TRUE),
                nb_log_blocage_eligibilite = sum(source_orange_nb_log_blocage_eligibilite, na.rm=TRUE),
                `Nom département` = unique(`Nom département`))%>%
      dplyr::ungroup()%>%
      dplyr::mutate(`taux de blocage eligibilite ftth` = ifelse(.data[[base_sans_en_cours_construction]]==0, NA_real_,
                                                         `nb_log_blocage_eligibilite`/.data[[base_sans_en_cours_construction]]),
             `taux de refus tiers` = ifelse(.data[[base_sans_en_cours_construction]]==0, NA_real_,
                                            `nbr_log_refus_tiers`/.data[[base_sans_en_cours_construction]]))%>%
      dplyr::select(`Code département`,`Nom département`, base_sans_en_cours_construction,
             `nbr_log_refus_tiers`, `taux de refus tiers`,
             `nb_log_blocage_eligibilite`, `taux de blocage eligibilite ftth`)



    # Feuille 12- Refus tiers bloc - OI : refus tiers et blocages éligibilité FttH par opérateur avec filtre niveau de couverture FttH


    stats_bloc_elig_refus_tiers_OI <- data %>%
      dplyr::group_by(operateurs, niveau_couverture_ftth)%>%
      dplyr::summarise(nbr_log_refus_tiers = sum(source_orange_nbr_log_refus_tiers, na.rm = TRUE),
                nb_log_blocage_eligibilite = sum(source_orange_nb_log_blocage_eligibilite, na.rm = TRUE))


    total <- data %>%
      dplyr::group_by(operateurs)%>%
      dplyr::summarise(nbr_log_refus_tiers = sum(source_orange_nbr_log_refus_tiers, na.rm = TRUE),
                nb_log_blocage_eligibilite = sum(source_orange_nb_log_blocage_eligibilite, na.rm = TRUE),
                niveau_couverture_ftth = "ALL")


    stats_bloc_elig_refus_tiers_OI <- rbind(stats_bloc_elig_refus_tiers_OI, total)


    # Feuille 13- Refus tiers bloc - zonage : refus tiers et blocages éligibilité FttH par zonage avec filtre niveau de couverture FttH


    stats_bloc_elig_refus_tiers_zonage <- data %>%
      dplyr::group_by(zonage, niveau_couverture_ftth)%>%
      dplyr::summarise(nbr_log_refus_tiers = sum(source_orange_nbr_log_refus_tiers, na.rm = TRUE),
                nb_log_blocage_eligibilite = sum(source_orange_nb_log_blocage_eligibilite, na.rm = TRUE))


    total <- data %>%
      dplyr::group_by(zonage)%>%
      dplyr::summarise(nbr_log_refus_tiers = sum(source_orange_nbr_log_refus_tiers, na.rm = TRUE),
                nb_log_blocage_eligibilite = sum(source_orange_nb_log_blocage_eligibilite, na.rm = TRUE),
                niveau_couverture_ftth = "ALL")


    stats_bloc_elig_refus_tiers_zonage <- rbind(stats_bloc_elig_refus_tiers_zonage, total)

  }else{
    stats_bloc_elig_refus_tiers <- data.frame()

    stats_bloc_elig_refus_tiers_zonage <- data.frame()

    stats_bloc_elig_refus_tiers_OI <- data.frame()
  }







  # La fonction retourne les différentes statistiques qui seront exportées dans des feuilles sur Excel



  return(
    list(

      data_niveau_departement_zipu = data_niveau_departement_zipu,

      data_niveau_departement_zipri = data_niveau_departement_zipri,

      evolut_couverture_dep_zipri = data_evol_couverture_niv_depart_zipri,

      evolut_couverture_dep_zipu = data_evol_couverture_niv_depart_zipu,

      data_nbre_raccordable_par_zonage = data_nbre_raccordable_par_zonage,

      data_taux_raccordable_par_zonage = data_taux_raccordable_par_zonage,

      data_oi_lot_locaux = data_oi_lot_locaux,

      data_zonage_lot_locaux = data_zonage_lot_locaux,

      stats_sur_report_niv_dep_1 = stats_sur_report_niv_dep_1,

      stats_sur_report_niv_dep_2 = stats_sur_report_niv_dep_2,

      stats_bloc_elig_refus_tiers = stats_bloc_elig_refus_tiers,

      stats_bloc_elig_refus_tiers_OI = stats_bloc_elig_refus_tiers_OI,

      stats_bloc_elig_refus_tiers_zonage = stats_bloc_elig_refus_tiers_zonage
    ))

}

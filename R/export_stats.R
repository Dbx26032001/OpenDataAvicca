#' Ce code sert à exporter les tableaux statistiques produites à partir de l'open data / fichier ZAPM
#' @export

export_stats <- function(file, mode_traitement, data_export, orange_file=NULL){

  wb_stats <- createWorkbook()

  if(mode_traitement == "Open data - déploiement & fermeture du cuivre"){

    # Données à exporter
    data_niveau_departement_zipu <- data_export$stats$data_niveau_departement_zipu
    data_niveau_departement_zipri <- data_export$stats$data_niveau_departement_zipri
    data_evol_couverture_niv_depart_zipu <- data_export$stats$evolut_couverture_dep_zipu
    data_evol_couverture_niv_depart_zipri <- data_export$stats$evolut_couverture_dep_zipri
    data_nbre_raccordable_par_zonage <- data_export$stats$data_nbre_raccordable_par_zonage
    data_taux_raccordable_par_zonage <- data_export$stats$data_taux_raccordable_par_zonage
    data_oi_lot_locaux <- data_export$stats$data_oi_lot_locaux
    data_zonage_lot_locaux <- data_export$stats$data_zonage_lot_locaux
    stats_sur_report_niv_dep_1 <- data_export$stats$stats_sur_report_niv_dep_1
    stats_sur_report_niv_dep_2 <- data_export$stats$stats_sur_report_niv_dep_2
    stats_bloc_elig_refus_tiers <- data_export$stats$stats_bloc_elig_refus_tiers
    stats_bloc_elig_refus_tiers_OI <- data_export$stats$stats_bloc_elig_refus_tiers_OI
    stats_bloc_elig_refus_tiers_zonage <- data_export$stats$stats_bloc_elig_refus_tiers_zonage


    # Création des feuilles
    addWorksheet(wb_stats, "Stats DEP - ZIPU")

    addWorksheet(wb_stats, "Stats DEP - ZIPRI")

    addWorksheet(wb_stats, "Evol couv FTTH - DEP ZIPRI")

    addWorksheet(wb_stats, "Evol couv FTTH - DEP ZIPU")

    addWorksheet(wb_stats, "Evol Locaux rac - Zonage")

    addWorksheet(wb_stats, "Evol couv FTTH - Zonage")

    addWorksheet(wb_stats, "Locaux raccordables par OI")

    addWorksheet(wb_stats, "Locaux raccordables - zonage")

    addWorksheet(wb_stats, "Locaux rac DEP - report")

    addWorksheet(wb_stats, "Locaux REPORT DEP - zonage")


    if(!is.null(orange_file)){

      addWorksheet(wb_stats, "Refus tiers bloc - DEP")

      addWorksheet(wb_stats, "Refus tiers bloc - OI")

      addWorksheet(wb_stats, "Refus tiers bloc - zonage")
    }




    # Export des données dans les feuilles

    writeData(wb_stats, "Stats DEP - ZIPU", data_niveau_departement_zipu)

    writeData(wb_stats, "Stats DEP - ZIPRI", data_niveau_departement_zipri)

    writeData(wb_stats, "Evol couv FTTH - DEP ZIPRI", data_evol_couverture_niv_depart_zipri)

    writeData(wb_stats, "Evol couv FTTH - DEP ZIPU", data_evol_couverture_niv_depart_zipu)

    writeData(wb_stats, "Evol Locaux rac - Zonage", data_nbre_raccordable_par_zonage)

    writeData(wb_stats, "Evol couv FTTH - Zonage", data_taux_raccordable_par_zonage)

    writeData(wb_stats, "Locaux raccordables par OI", data_oi_lot_locaux)

    writeData(wb_stats, "Locaux raccordables - zonage", data_zonage_lot_locaux)

    writeData(wb_stats, "Locaux rac DEP - report", stats_sur_report_niv_dep_1)

    writeData(wb_stats, "Locaux REPORT DEP - zonage", stats_sur_report_niv_dep_2)





    if(!is.null(orange_file)){

      writeData(wb_stats, "Refus tiers bloc - DEP", stats_bloc_elig_refus_tiers)

      writeData(wb_stats, "Refus tiers bloc - OI", stats_bloc_elig_refus_tiers_OI)

      writeData(wb_stats, "Refus tiers bloc - zonage", stats_bloc_elig_refus_tiers_zonage)
    }



  }else{

    # Données à exporter

    stock_zapm = data_export$stock_zapm
    stats_zapm_dep = data_export$stats_zapm_dep


    # Création des feuiles


    addWorksheet(wb_stats, "Taille ZAPM par DEP")

    addWorksheet(wb_stats, "Stock zapm")




    # Export des données dans les feuilles créées

    writeData(wb_stats, "Taille ZAPM par DEP", stats_zapm_dep)

    writeData(wb_stats, "Stock zapm", stock_zapm)


  }


  saveWorkbook(wb_stats, file, overwrite = TRUE)


}




#' Ce code permet d'ouvrir dans R les données de l'ARCEP sur le déploiement et extrait les colonnes que l'on recherche pour le nouveau trimestre
#' Colonnes: TX XXX (locaux raccordable), Estimation locaux, Source retenue, Base locaux sans construction, locaux en construction, %Tx xxx (taux de couverture FttH)
#' @export

process_arcep <- function(file_path) {

  sheets <- readxl::excel_sheets(file_path)


  feuille_commune <- sheets[which(tolower(sheets) == "communes")[1]] # Cherche la feuille communes



  tmp <- readxl::read_excel(file_path, sheet = feuille_commune, col_names = FALSE, n_max = 10)



  ligne_sauter <- which(apply(tmp, 1, function(x) any(x == "Code commune"))) - 1



  arcep_data <- readxl::read_excel(file_path, sheet = feuille_commune , skip = ligne_sauter)



  source_retenue_col <- names(arcep_data)[grepl("^Source retenue",names(arcep_data))] # Source retenue pour l'estimation des locaux



  locaux_col <- names(arcep_data)[grepl("^Meilleure estimation des locaux", names(arcep_data))][1] # colonne  Meilleure estimation des locaux



  locaux_en_cours_col <- names(arcep_data)[grepl("^Estimation locaux en cours", names(arcep_data))][1] # colonne  Estimation des locaux en cours de construction



  trimestres <- grep("^T[1-4] \\d{4}$", names(arcep_data), value = TRUE) # liste des Colonnes des locaux raccordables (T4 2017,...T1 2024, ...)



  locaux_racc_nouveau_trimestre <- trimestres[length(trimestres)] # colonne des locaux raccordables du nouveau trimestre publié


  locaux_en_cours_col_rename <- paste0(locaux_en_cours_col, " ", locaux_racc_nouveau_trimestre)



  base_sans_en_cours_construction <- paste0("Base sans locaux en cours de construction ", locaux_racc_nouveau_trimestre) # nom de la colonne base deslocaux sans construction


  nouveau_taux_raccordable <- paste0("%", locaux_racc_nouveau_trimestre)


  arcep_data[[base_sans_en_cours_construction]] <- arcep_data[[locaux_col]] - arcep_data[[locaux_en_cours_col]]


  arcep_data[[nouveau_taux_raccordable]] <- arcep_data[[locaux_racc_nouveau_trimestre]] / arcep_data[[base_sans_en_cours_construction]] # calcul des taux de couverture par commune à partir du nbr locaux raccordables et la base deslocaux sans construction



  # la fonction retourne la data ARCEP avec les informations pertinentes du nouveau trimestre publié

  arcep_data %>%
    dplyr::rename(locaux_en_cours_col_rename = locaux_en_cours_col)%>%
    dplyr::select(`Code commune`, source_retenue_col, locaux_col, locaux_en_cours_col_rename, base_sans_en_cours_construction, all_of(locaux_racc_nouveau_trimestre), nouveau_taux_raccordable)


}



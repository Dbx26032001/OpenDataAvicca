#' Export open data Avicca Maj
#' @export


export_open_data <- function(file, avicca_file, data_export){

  wb <- openxlsx::loadWorkbook(avicca_file)

  ancienne_version_avicca_data <- data_export$ancienne_version_avicca_data

  new_open_data  <- data_export$open_data

  fiche_controle <- data_export$fiche_erreurs

  nbre_col_initial_avicca <- ncol(ancienne_version_avicca_data)

  # Les nouvelles colonnes à ajouter dans la fichier Excel de l'ancienne version open data Avicca

  new_cols <- names(new_open_data)[(nbre_col_initial_avicca + 1):ncol(new_open_data)]

  if(length(new_cols) > 0){

    openxlsx::writeData(
      wb,
      sheet = 1,
      x = new_open_data[, new_cols],
      startCol = nbre_col_initial_avicca + 1,
      startRow = 1,
      colNames = TRUE
    )

  }

  if("Fiche d'erreurs" %in% names(wb)){

    openxlsx::writeData(
      wb,
      "Fiche d'erreurs",
      fiche_controle,
      withFilter = TRUE
    )

  } else {

    openxlsx::addWorksheet(wb, "Fiche d'erreurs")

    openxlsx::writeData(
      wb,
      "Fiche d'erreurs",
      fiche_controle,
      withFilter = TRUE
    )

  }

  openxlsx::saveWorkbook(wb, file, overwrite = TRUE)



}

#' cette fonction programme la jointure entre les différentes sources de données pour obtenir l'open data Avicca
#' @export


jointure <- function(avicca, arcep = NULL, orange = NULL) {

  df <- avicca

  if (!is.null(arcep)) {
    df <- dplyr::left_join(df, arcep, by = "Code commune")
  }

  if (!is.null(orange)) {
    df <- dplyr::left_join(df, orange, by = c("Code commune" = "code_insee"))
  }

  df
}


#' cette fonction programme la jointure entre les différentes sources de données pour obtenir l'open data Avicca
#' @export


jointure <- function(avicca, arcep = NULL, orange = NULL) {

  if (!is.null(arcep)) {
    avicca <- merge(df, arcep, by = "Code commune", all.x = TRUE)
  }

  if (!is.null(orange)) {
    avicca <- merge(df, orange, by.x = "Code commune", by.y = "code_insee", all.x = TRUE)
  }

  avicca
}


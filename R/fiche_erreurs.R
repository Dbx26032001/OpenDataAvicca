#' code pour obtenir la fiche d'erreurs des jointures effectuées entre data (dernière version avicca, open data ARCEP, fichier trajectoire Orange)
#' @export

fiche_erreurs <- function(avicca, arcep=NULL, orange = NULL){


  communes_avicca <- as.character(unique(avicca$`Code commune`))

  if(!is.null(arcep)){
    communes_arcep  <- as.character(unique(arcep$`Code commune`))
    absentes_dans_avicca_depuis_arcep <- setdiff(communes_arcep, communes_avicca)
    absentes_dans_arcep               <- setdiff(communes_avicca, communes_arcep)
  }

  if (!is.null(orange)) {
    communes_orange <- as.character(unique(orange$code_insee))
    absentes_dans_avicca_depuis_orange <- setdiff(communes_orange, communes_avicca)
    absentes_dans_orange               <- setdiff(communes_avicca, communes_orange)
  }



  create_df_ecart <- function(source, commentaire, codes) {
    if (length(codes) == 0) return(NULL)

    data.frame(
      source = source,
      Commentaire = commentaire,
      code_commune = codes,
      stringsAsFactors = FALSE
    )
  }



  liste_ecarts <- list()

  # Si jointure avec data ARCP
  if(!is.null(arcep)){

    # ARCEP
    liste_ecarts[[length(liste_ecarts) + 1]] <-
      create_df_ecart("ARCEP",
                      "Présente dans ARCEP mais absente dans Avicca",
                      absentes_dans_avicca_depuis_arcep)

    # AVICCA vs ARCEP
    liste_ecarts[[length(liste_ecarts) + 1]] <-
      create_df_ecart("AVICCA",
                      "Présente dans AVICCA mais absente dans ARCEP",
                      absentes_dans_arcep)
  }


  # si jointure avec fichier ORANGE
  if (!is.null(orange)) {

    liste_ecarts[[length(liste_ecarts) + 1]] <-
      create_df_ecart("ORANGE",
                      "Présente dans ORANGE mais absente dans Avicca",
                      absentes_dans_avicca_depuis_orange)

    liste_ecarts[[length(liste_ecarts) + 1]] <-
      create_df_ecart("AVICCA",
                      "Présente dans AVICCA mais absente dans ORANGE",
                      absentes_dans_orange)
  }


  fiche_controle <- dplyr::bind_rows(liste_ecarts)

  # ... Sécurité si aucun écart ....

  if (is.null(fiche_controle) || nrow(fiche_controle) == 0) {
    fiche_controle <- data.frame(
      source = character(0),
      Commentaire = character(0),
      code_commune = character(0),
      stringsAsFactors = FALSE
    )

  }

  fiche_controle

}

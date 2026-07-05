#' Ce code concerne les manipulations sur le fichier ZAPM
#' (1) Transformer les données en format Excel avec quelques filtres selon le type de ZAPM (>=10 ans <=50% couverture, etc)
#' (2) Produire quelques stats : Taille des ZAPM par département selon certains type de ZAPM; Stock de ZAPM (Stock ZAPM >= 5 ans et couverture <= 95%  ou 80%)
#' @export


manip_fichier_zapm <- function(avicca, chemin_fichier_zapm, name_fichier_zapm){

  # ... Charger fichier ZAPM dbf ...

  zapm_dbf <- foreign::read.dbf(chemin_fichier_zapm)

  # .... DEBUT DES ANALYSES ....


  zone_AMII_AMEL_CPSD <- c("Zone CPSD hors L,33-13 ex RIP", "Zone AMII L,33-13 ex RIP", "Zone AMII L,33-13",
                           "Zone AMEL ex-RIP", "Zone AMII hors L,33-13", "Zone CPSD L,33-13 ex RIP",
                           "Zone AMEL", "Zone AMII hors L,33-13 ex RIP", "Zone CPSD hors L,33-13 ex RIP")


  zone_rip <- c("RIP", "Zone RIP ex AMII",
                "Zone mixte RIP et AMEL", "Zone mixte RIP et AMII L,33-13",
                "Zone mixte RIP et AMII hors L,33-13", "Zone mixte RIP et CPSD")

  zTD <- c("Zone très dense")



  avicca <- avicca%>%
    dplyr::mutate(type_zonage = ifelse(`Zonage identifié Avicca` %in% zone_rip, "RIP",
                                ifelse(`Zonage identifié Avicca` %in% zone_AMII_AMEL_CPSD, "ZMD privée",
                                       ifelse(`Zonage identifié Avicca` %in% zTD, "ZTD",""))))




  zapm_dbf <- zapm_dbf %>%
    dplyr::mutate(date_debut = as.Date(date_debut))%>%
    dplyr::left_join(
      avicca_last_version %>%
        select(`Code commune`, `Zonage identifié Avicca`, type_zonage),
      by = c("INSEE_COM" = "Code commune"))%>%
    dplyr::mutate(
      type_zonage = ifelse(
        grepl("^(Paris|Marseille|Lyon)", NOM_COM),
        "ZTD",
        type_zonage
      )
    )





  annee_reference <- substr(name_fichier_zapm, 1, 4)

  date_reference <- as.Date(paste0(annee_reference, "-12-31"))






  # ZAPM avec complétude (>= 99% de couverture FttH) sous 5 ans


  zapm_dbf <- zapm_dbf %>%
    dplyr::mutate(`complétude sous 5 ans`=ifelse(taux >= 0.99 & (date_debut >= (date_reference - years(5))), "Oui", "Non"))




  # ZAPM de 10 ans et plus avec taux <= 50%


  zapm_inquietantes_1 <- zapm_dbf %>%
    dplyr::filter(taux < 0.51 & (date_debut <= (date_reference - years(10))))%>%
    dplyr::select(CodeOI, INSEE_COM, NOM_COM, INSEE_DEP, NOM_DEP, date_debut, `Zonage identifié Avicca`, type_zonage, lgtZAPM, taux)


  # ZAPM de 15 ans et plus avec taux <= 80%


  zapm_inquietantes_2 <- zapm_dbf %>%
    dplyr::filter(taux < 0.81 & (date_debut <= (date_reference - years(15))))%>%
    dplyr::select(CodeOI, INSEE_COM, NOM_COM, INSEE_DEP, NOM_DEP, date_debut, `Zonage identifié Avicca`, type_zonage, lgtZAPM, taux)



  # Tableaux statistiques



  # Stock ZAPM >= 5 ans et couverture <= 95%  ou 80%

  stock_zapm <- zapm_dbf %>%
    dplyr::mutate(`Stock zapm <= 95%` = ifelse(taux < 0.96 & (date_debut <= (date_reference - years(5))), 1, 0),
           `Stock zapm <= 80%` = ifelse(taux < 0.81 & (date_debut <= (date_reference - years(5))), 1, 0),
           Periode = paste0("Fin"," ", annee_reference))%>%
    dplyr::group_by(Periode)%>%
    dplyr::summarise(`Stock zapm <= 95%` = sum(`Stock zapm <= 95%`, na.rm = TRUE),
              `Stock zapm <= 80%` = sum(`Stock zapm <= 80%`, na.rm = TRUE))




  # Stats niveau departemental

  zapm_dbf_mod <- zapm_dbf %>%
    dplyr::mutate(
      `complétude sous 5 ans` =
        ifelse(`complétude sous 5 ans` == "Oui", 1, 0),

      `ZAPM >= 5 ans et couverture <=80%` =
        ifelse(
          taux < 0.81 &
            date_debut <= (date_reference - years(5)),
          1, 0
        ),

      `ZAPM >= 5 ans et couverture <=95%` =
        ifelse(
          taux < 0.96 &
            date_debut <= (date_reference - years(5)),
          1, 0
        ),

      `ZAPM >= 10 ans et couverture <=50%` =
        ifelse(
          taux < 0.51 &
            date_debut <= (date_reference - years(10)),
          1, 0
        ),

      `ZAPM >= 15 ans et couverture <=80%` =
        ifelse(
          taux < 0.81 &
            date_debut <= (date_reference - years(15)),
          1, 0
        )
    )

  stats_zapm_dep <- zapm_dbf_mod %>%
    dplyr::rename(zonage = type_zonage)%>%
    dplyr::group_by(INSEE_DEP, zonage) %>%
    dplyr::summarise( NOM_DEP = first(NOM_DEP),

               `complétude sous 5 ans` =
                 sum(`complétude sous 5 ans` * lgtZAPM, na.rm = TRUE),

               `ZAPM >= 5 ans et couverture <=80%` =
                 sum(`ZAPM >= 5 ans et couverture <=80%` * lgtZAPM, na.rm = TRUE),

               `ZAPM >= 5 ans et couverture <=95%` =
                 sum(`ZAPM >= 5 ans et couverture <=95%` * lgtZAPM, na.rm = TRUE),

               `ZAPM >= 10 ans et couverture <=50%` =
                 sum(`ZAPM >= 10 ans et couverture <=50%` * lgtZAPM, na.rm = TRUE),

               `ZAPM >= 15 ans et couverture <=80%` =
                 sum(`ZAPM >= 15 ans et couverture <=80%` * lgtZAPM, na.rm = TRUE)


    )












  # Sauvegarde les outputs

  return(list(zapm_dbf = zapm_dbf,
              nom_fichier_zapm = nom_fichier_zapm,
              zapm_inquietantes_1 = zapm_inquietantes_1,
              zapm_inquietantes_2 = zapm_inquietantes_2,
              stock_zapm = stock_zapm,
              stats_zapm_dep = stats_zapm_dep))


}





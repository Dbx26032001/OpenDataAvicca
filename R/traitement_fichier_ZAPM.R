#' Ce code concerne les manipulations sur le fichier ZAPM
#' (1) Transformer les données en format Excel avec quelques filtres selon le type de ZAPM (>=10 ans <=50% couverture, etc)
#' (2) Produire quelques stats : Taille des ZAPM par département selon certains type de ZAPM; Stock de ZAPM (Stock ZAPM >= 5 ans et couverture <= 95%  ou 80%)
#' @export


manip_fichier_zapm <- function(avicca, chemin_fichier_zapm, name_fichier_zapm) {

  zapm_dbf <- foreign::read.dbf(chemin_fichier_zapm)

  # =========================
  # PRE-CALCUL CONSTANTS
  # =========================

  zone_AMII_AMEL_CPSD <- c(
    "Zone CPSD hors L,33-13 ex RIP", "Zone AMII L,33-13 ex RIP",
    "Zone AMII L,33-13", "Zone AMEL ex-RIP",
    "Zone AMII hors L,33-13", "Zone CPSD L,33-13 ex RIP",
    "Zone AMEL", "Zone AMII hors L,33-13 ex RIP",
    "Zone CPSD hors L,33-13 ex RIP"
  )

  zone_rip <- c(
    "RIP", "Zone RIP ex AMII",
    "Zone mixte RIP et AMEL",
    "Zone mixte RIP et AMII L,33-13",
    "Zone mixte RIP et AMII hors L,33-13",
    "Zone mixte RIP et CPSD"
  )

  # =========================
  # DATE (évite lubridate)
  # =========================

  annee_reference <- as.integer(substr(name_fichier_zapm, 1, 4))
  date_reference <- as.Date(paste0(annee_reference, "-12-31"))

  seuil_5  <- date_reference - 365 * 5
  seuil_10 <- date_reference - 365 * 10
  seuil_15 <- date_reference - 365 * 15

  # =========================
  # AVICCA PREP (1 seule fois)
  # =========================

  avicca$type_zonage <- "ZMD privée"

  avicca$type_zonage[
    avicca$`Zonage identifié Avicca` %in% zone_rip
  ] <- "RIP"

  avicca$type_zonage[
    avicca$`Zonage identifié Avicca` %in% "Zone très dense"
  ] <- "ZTD"

  # =========================
  # JOIN + TRANSFORMATION UNIQUE
  # =========================

  zapm_dbf$date_debut <- as.Date(zapm_dbf$date_debut)

  zapm_dbf <- merge(
    zapm_dbf,
    avicca[, c("Code commune", "Zonage identifié Avicca", "type_zonage")],
    by.x = "INSEE_COM",
    by.y = "Code commune",
    all.x = TRUE
  )

  # correction ZTD Paris/Lyon/Marseille
  zapm_dbf$type_zonage[
    grepl("^(Paris|Marseille|Lyon)", zapm_dbf$NOM_COM)
  ] <- "ZTD"

  # =========================
  # VARIABLES UNE SEULE FOIS
  # =========================

  zapm_dbf$compl_5 <- as.integer(
    zapm_dbf$taux >= 0.99 & zapm_dbf$date_debut >= seuil_5
  )

  zapm_dbf$inq1 <- as.integer(
    zapm_dbf$taux < 0.51 & zapm_dbf$date_debut <= seuil_10
  )

  zapm_dbf$inq2 <- as.integer(
    zapm_dbf$taux < 0.81 & zapm_dbf$date_debut <= seuil_15
  )

  zapm_dbf$stock95 <- as.integer(
    zapm_dbf$taux < 0.96 & zapm_dbf$date_debut <= seuil_5
  )

  zapm_dbf$stock80 <- as.integer(
    zapm_dbf$taux < 0.81 & zapm_dbf$date_debut <= seuil_5
  )

  # =========================
  # OUTPUTS FILTRES (rapide)
  # =========================

  zapm_inquietantes_1 <- zapm_dbf[zapm_dbf$inq1 == 1,
                                  c("CodeOI","INSEE_COM","NOM_COM","INSEE_DEP","NOM_DEP",
                                    "date_debut","Zonage identifié Avicca","type_zonage",
                                    "lgtZAPM","taux")]

  zapm_inquietantes_2 <- zapm_dbf[zapm_dbf$inq2 == 1,
                                  c("CodeOI","INSEE_COM","NOM_COM","INSEE_DEP","NOM_DEP",
                                    "date_debut","Zonage identifié Avicca","type_zonage",
                                    "lgtZAPM","taux")]

  # =========================
  # STOCK
  # =========================

  stock_zapm <- aggregate(
    cbind(stock95, stock80) ~ 1,
    data = zapm_dbf,
    sum,
    na.rm = TRUE
  )

  stock_zapm$Periode <- paste0("Fin ", annee_reference)

  # =========================
  # STATS DEPARTEMENT
  # =========================

  zapm_dbf$compl_5_w <- zapm_dbf$compl_5 * zapm_dbf$lgtZAPM
  zapm_dbf$inq1_w <- zapm_dbf$inq1 * zapm_dbf$lgtZAPM
  zapm_dbf$inq2_w <- zapm_dbf$inq2 * zapm_dbf$lgtZAPM

  stats_zapm_dep <- aggregate(
    cbind(compl_5_w, inq1_w, inq2_w) ~ INSEE_DEP + type_zonage + NOM_DEP,
    data = zapm_dbf,
    sum,
    na.rm = TRUE
  )

  names(stats_zapm_dep) <- c(
    "INSEE_DEP", "zonage", "NOM_DEP",
    "complétude sous 5 ans",
    "ZAPM >= 5 ans et couverture <=80%",
    "ZAPM >= 10 ans et couverture <=50%"
  )

  # =========================
  # RETURN
  # =========================

  return(list(
    zapm_dbf = zapm_dbf,
    nom_fichier_zapm = name_fichier_zapm,
    zapm_inquietantes_1 = zapm_inquietantes_1,
    zapm_inquietantes_2 = zapm_inquietantes_2,
    stock_zapm = stock_zapm,
    stats_zapm_dep = stats_zapm_dep
  ))
}

#' Export open data ZAPM en format Excel
#' @export


export_data_zapm <- function(file, data_export){

  wb <- openxlsx::createWorkbook()

  openxlsx::addWorksheet(wb,"ZAPM data")
  openxlsx::addWorksheet(wb,">= 10 ans & <= 50%")
  openxlsx::addWorksheet(wb,">= 15 ans & <= 80%")

  openxlsx::writeData(wb,"ZAPM data",data_export$zapm_dbf)
  openxlsx::writeData(wb,">= 10 ans & <= 50%",data_export$zapm_inquietantes_1)
  openxlsx::writeData(wb,">= 15 ans & <= 80%",data_export$zapm_inquietantes_2)

  openxlsx::saveWorkbook(wb,file,overwrite=TRUE)



}

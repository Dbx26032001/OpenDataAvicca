#' Ce code permet d'ouvrir dans R l'open data Avicca
#' @export
lecture_avicca <- function(path) {

  preview <- readxl::read_excel(path, n_max = 2)
  n <- ncol(preview)

  readxl::read_excel(
    path,
    col_types = c(rep("text", 4), rep("guess", n - 4))
  )

}

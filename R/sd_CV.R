#' Standard Deviation e Coefficiente di Variazione variabili quantitative
#'
#' Preso un dataframe di variabili di solo tipo numeric ne calcola la Standard Deviation e il Coefficiente di Variazione
#' @usage sd_cv(data)
#' @param data Un dataset di sole variabili quantitative (di tipo numeric)
#' @return Tabella Standard Deviation e Coefficiente di Variazione di tutte le variabili
#' @examples
#' ## Calcoliamo la standard deviation e il coefficiente di variazioni delle variabili del dataset
#' ## USArrests e li salviamo in una variabile
#'
#' result <- sd_cv(datasets::USArrests)
#'
#' ## Verranno stampati i risultati. Per richiamarli basta scrivere:
#'
#' result
#' @export

#' @importFrom "stats" "sd"

sd_cv <- function(data){

  check_IS.NOT.NULL.DATA(data)
  data <- as.matrix(data)
  check_IS.NUMERIC(data)

  sd_CV_tot <- matrix(1, 2, dim(data)[2])

  for(i in 1:dim(data)[2]) {
    sd_CV_tot[1, i] <- round(sd(data[ , i], 4))
    sd_CV_tot[2, i] <- round(sd(data[ , i])/mean(data[ , i]), 4)
  }
  rownames(sd_CV_tot) <- c("SD","CV")
  colnames(sd_CV_tot) <- colnames(data)
  writeLines(c("SD E CV TOTALI",""))
  print(sd_CV_tot)

  return(sd_CV_tot)
}

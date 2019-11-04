#' Indici di Confronto variabili quantitative
#'
#' Preso un dataframe di variabili di solo tipo numeric ne calcola gli indici di confronto
#' @usage indici_conf(data)
#' @param data Un dataset di sole variabili quantitative (di tipo numeric)
#' @return Tabella indici di confronto della media/mediana e quartili di tutte le variabili
#' @examples
#' ## Calcoliamo gli indici di confronto delle variabili del dataset USArrests
#' ## e li salviamo in una variabile
#'
#' result <- indici_conf(datasets::USArrests)
#'
#' ## Verranno stampati i risultati. Per richiamarli basta scrivere:
#'
#' result
#' @export

#' @importFrom "stats" "quantile" "sd"

indici_conf <- function(data){

  check_IS.NOT.NULL.DATA(data)
  data <- as.matrix(data)
  check_IS.NUMERIC(data)

  indici_confronto_tot <- matrix(1, 2, dim(data)[2])

  for(i in 1:dim(data)[2]) {
    quantile_temp <- quantile(data[ , i], seq(0,1,0.25))
    indici_confronto_tot[1,i] <- round((mean(data[ , i])-quantile_temp[3])/sd(data[ , i]), 4)
    indici_confronto_tot[2,i] <- round((quantile_temp[4]-2*quantile_temp[3]+quantile_temp[2])/(quantile_temp[4]-quantile_temp[2]),4)
  }
  rownames(indici_confronto_tot) <- c("MEDIA/MEDIANA","QUARTILI")
  colnames(indici_confronto_tot) <- colnames(data)
  writeLines(c("MEDIA/MEDIANA E QUARTILI TOTALI",""))
  print(indici_confronto_tot)

  return(indici_confronto_tot)
}

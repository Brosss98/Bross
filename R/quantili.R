#' Quantili variabili quantitative
#'
#' Preso un dataframe di variabili di solo tipo numeric ne calcola i quantili principali (MIN, Q1, Q2, Q3, MAX)
#' @usage quantili(data)
#' @param data Un dataset di sole variabili quantitative (di tipo numeric)
#' @return Tabella dei quantili di tutte le variabili
#' #' @examples
#' ## Calcoliamo i quantili di tutte le variabili del dataset USArrests
#' ## e li salviamo in una variabile
#'
#' result <- quantili(datasets::USArrests)
#'
#' ## Verranno stampati i risultati. Per richiamarli basta scrivere:
#'
#' result
#' @export

#' @importFrom "stats" "quantile"

quantili <- function(data){

  check_IS.NOT.NULL.DATA(data)
  data <- as.matrix(data)
  check_IS.NUMERIC(data)

  quantile_tot <- matrix(1, dim(data)[2], 5)
  CONT <- 1

  for(i in colnames(data)){
    quantile_temp <- quantile(data[ , i], seq(0,1,0.25), type=1)
    quantile_tot[CONT,1] <- quantile_temp[1]
    quantile_tot[CONT,2] <- quantile_temp[2]
    quantile_tot[CONT,3] <- quantile_temp[3]
    quantile_tot[CONT,4] <- quantile_temp[4]
    quantile_tot[CONT,5] <- quantile_temp[5]
    CONT <- CONT + 1
  }

  rownames(quantile_tot) <- colnames(data)
  colnames(quantile_tot) <- c("MIN","Q1","Q2","Q3","MAX")

  writeLines(c("TABELLA QUANTILI",""))
  print(round(quantile_tot, 4))

  return(quantile_tot)
}

#' Covarianza e Correlazione variabili quantitative
#'
#' Preso un dataframe di variabili di solo tipo numeric ne calcola le tabelle di covarianza e correlazione
#' @usage cov_cor(data)
#' @param data Un dataset di sole variabili quantitative (di tipo numeric)
#' @return Tabella covarianza di tutte le variabili
#' @return Tabella correlazione di tutte le variabili
#' @examples
#' ## Calcoliamo la covarianza e correlazione tra le variabili del dataset USArrests
#' ## e le salviamo in una variabile
#'
#' result <- cov_cor(datasets::USArrests)
#'
#' ## Verranno stampati i risultati. Per richiamarli basta scrivere:
#'
#' result$COV_TOT
#' result$COR_TOT
#' @export

#' @importFrom "stats" "cov"

cov_cor <- function(data){

  check_IS.NOT.NULL.DATA(data)
  data <- as.matrix(data)
  check_IS.NUMERIC(data)

  cov_tot <- cov(data)
  cor_tot <- cor(data)

  for(i in 1:dim(data)[2]) {
    for(j in 1:dim(data)[2]) {
      if (i==j) {
        cov_tot[i,j] <- "=="
        cor_tot[i,j] <- "=="
      } else {
        cov_tot[i,j] <- round(as.numeric(cov_tot[i,j]), 4)
        cor_tot[i,j] <- round(as.numeric(cor_tot[i,j]), 4)
      }
    }
  }
  writeLines("COVARIANZA")
  print(cov_tot)
  writeLines(c("","","CORRELAZIONE"))
  print(cor_tot)
  writeLines("","")

  return_data <- list(cov_tot, cor_tot)
  names(return_data) <- c("COV_TOT", "COR_TOT")
  return(return_data)
}

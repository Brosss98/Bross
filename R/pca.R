#' PCA Analysis
#'
#' Prende un dataframe e ne esegue l'Analisi in Componenti Principali
#' @usage pca(data)
#' @param data Un dataframe non vuoto
#' @return Stampa dell'istogramma delle varianze delle singole componenti
#' @return Una lista con i valori dei Loadings delle varie componendi ($Loadings)
#' @return La tabella di fedelta' di rappresentazione sulle componenti della PCA ($Fedelta_PCA)
#' @return Matrice R della PCA ($R_PCA)
#' @examples
#' ## Eseguiamo una PCA delle variabili quantitative del dataset USArrests
#' ##e salviamo i risultati in una variabile
#'
#' result <- pca(datasets::USArrests)
#'
#' ## Verrà stampato un barplot rappresentante la varianza delle singole componenti e la tabella
#' ## della fedelta' della PCA. Per richiamare i risultati basta scrivere:
#'
#' result$Loadings
#' result$Fedelta_PCA
#' result$R_PCA
#' @export

#' @importFrom "graphics" "abline"
#' @importFrom "stats" "princomp"

pca <- function(data){

  #checking input
  check_IS.NOT.NULL.DATA(data)
  check_IS.NUM.DATAFRAME(data)
  if(check_NA.ELEMENTS(data))
    data <- na.omit(data)

  #calculate components of PCA
  pca_data <- princomp(data, cor=T)

  #print("LOADINGS", quote = FALSE)
  #print(round(pca_data$loadings,3))

  #calculate fedelty of PCA's components
  var_pca_data <- pca_data$sdev^2
  perc_var_pca_data <- var_pca_data/sum(var_pca_data)
  fedelta_pca <- rbind(pca_data$sdev, var_pca_data, perc_var_pca_data, cumsum(perc_var_pca_data))
  rownames(fedelta_pca) <- c("Standard Deviation", "Variance", "Proportion of Variance", "Cumulative Proportion")

  writeLines(c("FEDELTA' PCA",""))
  print(round(fedelta_pca, 4))

  #plot of variance of PCA's components
  plot(pca_data, ylim = c(0, max(fedelta_pca[2])+0.5))
  abline(h=0)

  #R matrix of PCA
  R_pca <- pca_data$loadings%*%diag(pca_data$sdev)
  #print("MATRICE R PCA", quote = FALSE)
  #print(round(R_pca,4))

  #return data of function
  return_data <- list(pca_data$loadings, fedelta_pca, R_pca)
  names(return_data) <- c("Loadings", "Fedelta_PCA", "R_PCA")
  return(return_data)
}

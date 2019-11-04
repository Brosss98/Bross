#' PCA Plot N-Componenti
#'
#' Prende un dataframe e ne esegue la PCA per poi stampare i grafici delle componenti scelte e le tabelle di fedelta' dei grafici
#' @usage pca_plot_comp(data, R, n_comp)
#' @param data Un dataframe non vuoto
#' @param R Una matrice R corretta per il dataframe dato
#' @param n_comp Numero di componenti da prendere in considerazione
#' @return Stampa dei singoli grafici delle coppie di componenti
#' @return Le tabelle di fedelta' dei singoli grafici
#' @examples
#' ## Stampiamo i plot della PCA delle prime n_comp componenti selezionate, lavorando sul dataset
#' ## USArrests e prendendo la matrice R calcolata con la funzione pca()
#'
#' R <- pca(datasets::USArrests)$R_PCA
#' pca_plot_comp(datasets::USArrests, R, 2)
#'
#' ## Verrà stampata la tabella di fedelta' della PCA, i plot riguardo le componenti d'interesse
#' ## e le relative tabelle di fedelta' di rappresentazione dei singoli grafici
#' @export

#' @importFrom "graphics" "symbols" "text"
#' @importFrom "stats" "na.omit"
pca_plot_comp <- function(data, R, n_comp){

  #checking input
  check_IS.NOT.NULL.DATA(data)
  check_IS.NUM.DATAFRAME(data)
  check_IS.NOT.NULL.DATA(R)
  check_IS.NUM.DATAFRAME(R)
  if(check_NA.ELEMENTS(data))
    data <- na.omit(data)
  if(sum(is.na(R)))
    stop("La matrice R non puo' contenere elementi NA!")

  if(dim(data)[2] != dim(R)[1])
    stop("La matrice R fornita non corrisponde al dataframe dato!")
  if(dim(R)[1] != dim(R)[2])
    stop("La matrice R deve essere quadrata!")
  if(!equal_vectors(rownames(R), colnames(data)))
    stop("Informazioni non valide nella matrice R rispetto al dataframe dato!")

  if(n_comp <= 1 | n_comp > dim(R)[2])
    stop("Inserire un numero di componenti valido!")

  #print the number of selected components
  writeLines(c(paste("Numero componenti scelte:",n_comp), ""))

  #print the plot for each couple of selected components and its matrix of fedelty
  for(i in 1:n_comp){
    j=i+1
    while(j <= n_comp){
      plot(R[,i],R[,j],xlim=c(-1,1),ylim=c(-1,1),asp=1,xlab=paste("Asse ", i),ylab=paste("Asse ", j),main=paste("Grafico delle componenti ", i, " e ", j))
      abline(h=0,v=0)
      symbols(0,0,circles=1,inches=F,add=T)
      text(R[,i],R[,j],pos=3,labels=colnames(data),cex=0.8)
      symbols(0,0,circles=0.8,inches=F,fg="red",add=T)

      qual <- R[,i]^2+R[,j]^2
      fedelta <- round(cbind(R[,i]^2,R[,j]^2,qual),4)
      colnames(fedelta)=c(paste("R[,",i,"]^2"),paste("R[,",j,"]^2"),paste("qual_",i,"-",j))
      print(fedelta)
      writeLines("")

      j=j+1
    }
  }
}



#' PCA Plot
#'
#' Prende un dataframe e ne esegue la PCA per poi stampare i grafici delle componenti piu' rilevanti
#' @usage pca_plot(data)
#' @param data Un dataframe non vuoto
#' @return Stampa dei singoli grafici delle coppie di componenti
#' @return Le tabelle di fedelta' dei singoli grafici
#' @seealso \code{\link{pca}} Metodo utilizzato per l'analisi in componenti principali
#' @seealso \code{\link{pca_plot_comp}} Metodo utilizzato per la stampa dei grafici delle componenti e la creazione delle tabelle di fedelta' dei grafici
#' @examples
#' ## Stampiamo i plot della PCA delle sole componenti ritenute le piu' significative,
#' ## ovvero sulle quali sono meglio rappresentate le variabili (si sceglie il minor numero
#' ## possibile), lavorando sul dataset USArrests.
#'
#' pca_plot(datasets::USArrests)
#'
#' ## Verrà stampata la tabella di fedelta' della PCA, i plot riguardo le componenti d'interesse
#' ## e le relative tabelle di fedelta' di rappresentazione dei singoli grafici
#' @export

pca_plot <- function(data){

  #checking input
  check_IS.NOT.NULL.DATA(data)
  check_IS.NUM.DATAFRAME(data)
  if(check_NA.ELEMENTS(data))
    data <- na.omit(data)

  #result of pca() function
  pca_result <- pca(data)

  #calculate number of useful components
  i <- 4
  while(i <= 4*dim(pca_result$R_PCA)[2]){
    if(pca_result$Fedelta_PCA[i] > 0.85){
      n_comp <- i/4-1
      j <- i
      i <- (4*dim(pca_result$R_PCA)[2])+1
    }
    i <- i+4
  }
  if(j %in% c(0,4,8)){
    writeLines(c("", "Viene selezionato il minimo numero di componenti necessarie per la rappresentazione grafica: 2",
                 paste("L'algoritmo consiglierebbe di selezionare", j/4-1, "componenti"),
                 ""))
    n_comp <- 2
  }

  #call pca_plot_comp to show result and PCA analysis
  pca_plot_comp(data, pca_result$R_PCA, n_comp)
}

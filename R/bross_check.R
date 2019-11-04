### BROSS CHECK FILE ###

equal_vectors <- function(x, y){
  for(i in 1:length(x))
    if(x[i]!=y[i])
      return(FALSE)
  return(TRUE)
}

is.num.dataframe <- function(data){

  for(i in colnames(data))
    if(!is.numeric(data[,i]))
      return(FALSE)
  return(TRUE)
}

check_IS.NOT.NULL.DATA <- function(data){

  if(is.null(data))
    stop("Inserire un dataframe non vuoto!")
}

check_IS.NUM.DATAFRAME <- function(data){

  if(!is.num.dataframe(data))
    stop("Inserire un dataframe di soli valori numerici!")
}

check_cluster_METHODS <- function(using_method){

  METHODS <- c("ward.D", "single", "complete", "average",
               "mcquitty", "median", "centroid", "ward.D2")

  if(!(using_method %in% METHODS))
    stop("E' stato inserito un metodo non valido!")
}

check_cluster_DISTANCES <- function(using_distance){

  DISTANCES <- c("euclidean", "maximum", "manhattan", "canberra",
                 "binary", "minkowski")

  if(!(using_distance %in% DISTANCES))
    stop("E' stata inserita una distanza non valida!")
}

check_NA.ELEMENTS <- function(data){

  if(sum(is.na(data) > 0)){
    writeLines(c("Ci sono degli elementi NA all'interno del dataframe!",
                 "Non verranno considerate tutte quelle osservazioni che contengono un NA per almeno una variabile",
                 ""))
    return(TRUE)
  }
  return(FALSE)
}

check_IS.FACTOR <- function(data){

    if(!is.factor(data))
      stop("L'argomento fornito deve essere di tipo factor!")
}

check_IS.NUMERIC <- function(data){

  for(i in 1:dim(data)[2])
    if(!is.numeric(data[ , i]))
      stop("L'argomento fornito deve essere di tipo numeric!")
}

check_LEGEND.POSITION <- function(position){

  POSITION <- c("bottomright", "bottom", "bottomleft", "left", "topleft", "top", "topright", "right", "center")
  if(!(position %in% POSITION))
    stop("Inserire una posizione valida per la legenda!")
}

#' Divisione variabili factor e numeric
#'
#' Preso un dataframe, viene restituito un oggetto di due componenti avente un dataframe di sole variabili factor ed uno di sole variabili numeric
#' @usage divide_FACTOR.NUMERIC(data)
#' @param data Un dataframe non vuoto
#' @return Un oggetto contenente due dataframe: uno di sole variabili di tipo factor e uno di sole numeric
#' @examples
#' ## Dividiamo il dataset esoph in variabili di tipo factor e di tipo numeric
#' ## Salviamo in una variabile i risultati ottenuti
#'
#' result <- divide_FACTOR.NUMERIC(datasets::esoph)
#'
#' ## Richiamiamo i risultati ottenuti:
#'
#' result$factor
#' result$numeric
#' @export

divide_FACTOR.NUMERIC <- function(data){

  factor <- NULL
  numeric <- NULL

  for(i in colnames(data)){
    if(is.factor(data[ , i]))
      factor <- c(factor, i)
    if(is.numeric(data[ , i]))
      numeric <- c(numeric, i)
    if(!is.factor(data[ , i]) & !is.numeric(data[ , i]))
      stop("Le colonne della matrice devono essere di tipo factor o numeric. In caso di colonne senza un ordine, si predilige il factor con ordine alfabetico.")
  }

  factor <- data[ , factor]
  numeric <- data[ , numeric]
  return_data <- list(factor, numeric)
  names(return_data) <- c("factor", "numeric")
  return(return_data)
}

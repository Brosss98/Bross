#' Cluster Analysis delle variabili
#'
#' Prende un dataframe e ne esegue la clusterizzazione delle variabili in base alle direttive date
#' @usage cluster_var(data, corr, using_method, n_cluster)
#' @param data Un dataframe non vuoto
#' @param corr Un metodo valido per il calcolo della distanza (1 --> 1-cor, 2 --> 1-cor^2)
#' @param using_method Un metodo valido per la clusterizzazione dei dati
#' @param n_cluster Un numero positivo per la creazione dei cluster
#' @return Stampa del dendogramma
#' @return Una lista con i dati originali con aggiunta una colonna di classe ($data)
#' @return La numerosita' dei singoli cluster ($group_var)
#' @examples
#' ## Eseguiamo una cluster analysis delle variabili del dataset USArrests
#' ## e salviamo i risultati in una variabile. Impostiamo come distanza quella 1-cor^2 (2),
#' ## utilizziamo il metodo di Ward e vogliamo creare 2 cluster
#'
#' result <- cluster_var(datasets::USArrests, 2, "ward.D", 2)
#'
#' ## Verrà stampata la numerosita' dei singoli gruppi. Per richiamare i risultati basta scrivere:
#'
#' result$data
#' result$group
#' @export

#' @importFrom "graphics" "par" "plot"
#' @importFrom "stats" "as.dist" "cor" "cutree" "dist" "hclust" "rect.hclust"

cluster_var <- function(data, corr, using_method, n_cluster){

  #checking input
  check_IS.NOT.NULL.DATA(data)
  check_IS.NUM.DATAFRAME(data)
  check_cluster_METHODS(using_method)
  if(check_NA.ELEMENTS(data))
    data <- na.omit(data)

  data_st <- scale(data)
  data_st_matrix <- (as.matrix(data_st))

  if(corr %in% c(1,2))
      dist <- 1-cor(data_st_matrix)^corr
  if(!(corr %in% c(1,2)))
    stop("Tipo di distanza scelta non valida!")

  if(n_cluster <= 0 | n_cluster >= dim(data)[2])
    stop("Numero di cluster richiesti non valido!")

  #prepare data to cluster of variables
  aggrega <- hclust(as.dist(dist), method = using_method)

  #plot dendogram and grouping observations
  par(mar=c(2.5,4.5,2.5,2)+0.1)
    plot(aggrega,sub="",main="Dendogramma Cluster Variabili",ylab="Indice di aggregazione",xlab="",
         hang=-1, frame.plot=T)

  group_var <- cutree(aggrega, k=n_cluster)
  rect.hclust(aggrega, k=n_cluster, group_var)

  print(table(group_var))

  #return data of function

  return_data <- list(data, table(group_var))
  names(return_data) <- c("data", "group_var")
  return(return_data)
}

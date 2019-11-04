#' Cluster Analysis
#'
#' Prende un dataframe e ne esegue la clusterizzazione in base alle direttive date
#' @usage cluster(data, using_distance, using_method, n_cluster)
#' @param data Un dataframe non vuoto
#' @param using_distance Un metodo valido per il calcolo della distanza
#' @param using_method Un metodo valido per la clusterizzazione dei dati
#' @param n_cluster Un numero positivo per la creazione dei cluster
#' @return Stampa del dendogramma
#' @return Una lista con i dati originali con aggiunta una colonna di classe ($data)
#' @return La numerosita' dei singoli cluster ($group)
#' @return I baricentri dei singoli cluster (dati standardizzati) ($baricentri_cluster)
#' @return Informazioni sulla compattezza dei singoli cluster ($compattezza_cluster)
#' @return Informazioni relative all'inerzia dei cluster (interna, totale e fra classi) ($inerzia)
#' @examples
#' ## Eseguiamo una cluster analysis delle unita' sperimentali del dataset USArrests
#' ## e salviamo i risultati in una variabile. Impostiamo come distanza quella euclidea,
#' ## utilizziamo il metodo di Ward e vogliamo creare 4 cluster
#'
#' result <- cluster(datasets::USArrests, "euclidean", "ward.D", 4)
#'
#' ## Verrà stampata la numerosita' dei singoli gruppi. Per richiamare i risultati basta scrivere:
#'
#' result$data
#' result$group
#' result$baricentri_cluster
#' result$compattezza_cluster
#' result$inerzia
#' @export

#' @importFrom "graphics" "par" "plot"
#' @importFrom "stats" "cutree" "dist" "hclust" "rect.hclust"

cluster <- function(data, using_distance, using_method, n_cluster){

  #checking input
  check_IS.NOT.NULL.DATA(data)
  check_IS.NUM.DATAFRAME(data)
  check_cluster_DISTANCES(using_distance)
  check_cluster_METHODS(using_method)
  if(check_NA.ELEMENTS(data))
    data <- na.omit(data)

  if(n_cluster <= 0 | n_cluster >= dim(data)[1])
    stop("Numero di cluster richiesti non valido!")

  #prepare data to cluster
  data_st <- scale(data)
  dist_st <- dist(data_st, method = using_distance)
  aggrega <- hclust(dist_st, method = using_method)

  #plot dendogram and grouping observations
  par(mar=c(2.5,4.5,2.5,2)+0.1)
    plot(aggrega,sub="",main="Dendogramma Cluster Unita' Sperimentali",ylab="Indice di aggregazione",
         xlab="",hang=-0.1,frame.plot=TRUE,label=FALSE)

  group <- cutree(aggrega, k=n_cluster)
  rect.hclust(aggrega,k=n_cluster,border="red")

  print(table(group))

  #barycenter of cluster
  baricentri_cluster <- colMeans(data_st[group==1, ])
  for(i in 2:n_cluster)
    baricentri_cluster <- rbind(baricentri_cluster, colMeans(data_st[group==i, ]))

  rownames(baricentri_cluster) <- rownames(table(group))

  #compactness of cluster
  dist_int_medie <- c(1:n_cluster)
  dist_int_max <- c(1:n_cluster)
  iner_int_medie <- (1:n_cluster)
  for(i in 1:n_cluster) {
    a <- rowSums(scale(data_st[group==i, ],scale=F)^2)
    dist_int_medie[i] <- mean(sqrt(a))
    dist_int_max[i] <- max(sqrt(a))
    iner_int_medie[i] <- mean(a)
  }
  compattezza_cluster <- cbind(table(group),dist_int_medie,dist_int_max,iner_int_medie)
  colnames(compattezza_cluster)[1] <- "Numerosita"

  #inertia cluster
  inerzia_tot <- (dim(data_st)[1]-1)*dim(data_st)[2]
  inerzia_int <- sum(iner_int_medie*table(group))
  percent_inerzia_fra <- round((inerzia_tot-inerzia_int)/inerzia_tot*100,2)
  inerzia <- matrix(1,1,3)
  inerzia[1] <- inerzia_int
  inerzia[2] <- inerzia_tot
  inerzia[3] <- percent_inerzia_fra
  colnames(inerzia) <- c("Inerzia Interna","Inerzia Totale","Percentuale Inerzia fra Classi")

  #return data of function
  data <- cbind(data,group)

  return_data <- list(data, table(group), round(baricentri_cluster, 4), round(compattezza_cluster,4), inerzia)
  names(return_data) <- c("data", "group", "baricentri_cluster", "compattezza_cluster", "inerzia")
  return(return_data)
}

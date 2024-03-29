% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/cluster_var.R
\name{cluster_var}
\alias{cluster_var}
\title{Cluster Analysis delle variabili}
\usage{
cluster_var(data, corr, using_method, n_cluster)
}
\arguments{
\item{data}{Un dataframe non vuoto}

\item{corr}{Un metodo valido per il calcolo della distanza (1 --> 1-cor, 2 --> 1-cor^2)}

\item{using_method}{Un metodo valido per la clusterizzazione dei dati}

\item{n_cluster}{Un numero positivo per la creazione dei cluster}
}
\value{
Stampa del dendogramma

Una lista con i dati originali con aggiunta una colonna di classe ($data)

La numerosita' dei singoli cluster ($group_var)
}
\description{
Prende un dataframe e ne esegue la clusterizzazione delle variabili in base alle direttive date
}
\examples{
## Eseguiamo una cluster analysis delle variabili del dataset USArrests
## e salviamo i risultati in una variabile. Impostiamo come distanza quella 1-cor^2 (2),
## utilizziamo il metodo di Ward e vogliamo creare 2 cluster

result <- cluster_var(datasets::USArrests, 2, "ward.D", 2)

## Verrà stampata la numerosita' dei singoli gruppi. Per richiamare i risultati basta scrivere:

result$data
result$group
}

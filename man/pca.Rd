% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/pca.R
\name{pca}
\alias{pca}
\title{PCA Analysis}
\usage{
pca(data)
}
\arguments{
\item{data}{Un dataframe non vuoto}
}
\value{
Stampa dell'istogramma delle varianze delle singole componenti

Una lista con i valori dei Loadings delle varie componendi ($Loadings)

La tabella di fedelta' di rappresentazione sulle componenti della PCA ($Fedelta_PCA)

Matrice R della PCA ($R_PCA)
}
\description{
Prende un dataframe e ne esegue l'Analisi in Componenti Principali
}
\examples{
## Eseguiamo una PCA delle variabili quantitative del dataset USArrests
##e salviamo i risultati in una variabile

result <- pca(datasets::USArrests)

## Verrà stampato un barplot rappresentante la varianza delle singole componenti e la tabella
## della fedelta' della PCA. Per richiamare i risultati basta scrivere:

result$Loadings
result$Fedelta_PCA
result$R_PCA
}

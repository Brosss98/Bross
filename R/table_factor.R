#' Analisi di due variabili factor
#'
#' Prende due variabili factor e ne calcola le tabelle assolute e relative prende in considerazione i marginali,
#' calcola le condizionate riga e colonna e ne disegna i barplot, infine calcola le deviazioni riga e colonna
#' @usage table_factor(factor1, factor2, name, position)
#' @param factor1 prima variabile di tipo factor
#' @param factor2 seconda variabile di tipo factor
#' @param name un vettore di due posizioni contenente i nomi con i quali verranno rappresentati le due variabili factor
#' @param position la posizione della legenda nella rappresentazione dei barplot
#' @return tutte le informazioni relative ai due fattori
#' @return la stampa dei barplot delle condizionate
#' @examples
#' ## Prendiamo due variabili qualitative di tipo fattore, un vettore di due posizioni contenente
#' ## i nomi di tali variabili (utile per la stampa) e la posizione della legenda nel grafico
#' ## (in alto a destra). Utilizziamo le variabili "agegp" (Gruppo di eta') e "alcgp"
#' ## (Quantitativo di consumo di alcool) provenienti dal dataset esoph
#'
#' dataset <- datasets::esoph
#' table_factor(dataset$agegp, dataset$alcgp, c("Fascia_eta", "Alcool"), "topright")
#'
#' ## Vengono stampate le tabelle relative per i due fattori separati, le tabelle assolute e relative
#' ## con i marginali, profili riga e colonna e le deviazioni riga e colonna. Vengono inoltre stampati
#' ## i barplot relativi alle condizionate riga e colonna
#' @export

#' @importFrom "graphics" "barplot" "legend" "title"
#' @importFrom "colorspace" "rainbow_hcl"

table_factor <- function(factor1, factor2, name, position){

  #checking input
  check_IS.NOT.NULL.DATA(factor1)
  check_IS.NOT.NULL.DATA(factor2)
  check_IS.FACTOR(factor1)
  check_IS.FACTOR(factor2)
  check_LEGEND.POSITION(position)
  if(length(name)!=2)
    stop("Inserire un vettore composto da due elementi che verranno utilizzati per identificare i due fattori!")

  #write information on factor
  writeLines(c("INFORMAZIONI:", paste("factor1 =", name[1]), paste("factor2 =", name[2]),"",""))

  #creating absoute and relative table and print relative table
  f1_ass <- table(factor1)
  f1_rel <- prop.table(f1_ass)
  f2_ass <- table(factor2)
  f2_rel <- prop.table(f2_ass)

  writeLines(c("TABELLE RELATIVE PER I DUE FATTORI SEPARATI",""))
  print(round(f1_rel*100, 2), quote = FALSE)
  writeLines("")
  print(round(f2_rel*100, 2), quote = FALSE)
  writeLines(c("","",""))

  #analyzing f1 and f2, print table
  f1_f2_ass <- table(factor1, factor2)
  f1_f2_ass_marg <- cbind(f1_f2_ass, margin.table(f1_f2_ass, 1))
  f1_f2_ass_marg <- rbind(f1_f2_ass_marg, margin.table(f1_f2_ass_marg, 2))

  f1_f2_rel <- prop.table(f1_f2_ass)
  f1_f2_rel_marg <- cbind(f1_f2_rel, margin.table(f1_f2_rel, 1))
  f1_f2_rel_marg <- rbind(f1_f2_rel_marg, margin.table(f1_f2_rel_marg, 2))

  writeLines(c("TABELLE ASSOLUTA E RELATIVA CON MARGINALI",""))
  print(f1_f2_ass_marg, quote = FALSE)
  writeLines("")
  print(round(f1_f2_rel_marg*100, 2), quote = FALSE)
  writeLines(c("","",""))

  #analyzing conditioned f1 and f2, print table
  prof_riga_f1_f2 <- prop.table(f1_f2_ass, 1)
  prof_colonna_f1_f2 <- prop.table(f1_f2_ass, 2)
  prof_riga_f1_f2_marg <- rbind(cbind(prof_riga_f1_f2, margin.table(prof_riga_f1_f2, 1)), f1_f2_rel_marg[dim(f1_f2_rel_marg)[1],])*100
  prof_colonna_f1_f2_marg <- cbind(rbind(prof_colonna_f1_f2, margin.table(prof_colonna_f1_f2, 2)), f1_f2_rel_marg[,dim(f1_f2_rel_marg)[2]])*100

  writeLines(c("PPROFILI RIGA E COLONNA",""))
  print(round(prof_riga_f1_f2_marg, 2), quote = FALSE)
  writeLines("")
  print(round(prof_colonna_f1_f2_marg, 2), quote = FALSE)
  writeLines(c("","",""))

  #draw barplot of conditioned factor
  colors <- factor(factor2, levels=names(table(factor2)), labels=rainbow_hcl(dim(table(factor2))))

  barplot(t(prof_riga_f1_f2*100), beside=T, ylim=c(0, round(max(t(prof_riga_f1_f2*100))+5,-1)), col=levels(colors))
    title(paste("Condizionata di", name[2], "a", name[1],"% (Prof. Riga)"))
    legend(position, legend=names(table(factor2)), pch=rep(15,dim(table(factor2))), col=levels(colors))
    abline(h=0)

  colors <- factor(factor1, levels=names(table(factor1)), labels=rainbow_hcl(dim(table(factor1))))

  barplot(prof_colonna_f1_f2*100, beside=T, ylim=c(0, round(max(prof_colonna_f1_f2*100)+5,-1)), col=levels(colors))
    title(paste("Condizionata di", name[1], "a", name[2], "% (Prof. Colonna)"))
    legend(position, legend=names(table(factor1)), pch=rep(15,dim(table(factor1))), col=levels(colors))
    abline(h=0)

  #row deviations
  deviaz_riga <- matrix(rep(round(margin.table(f1_f2_rel,2)*100, 2)), dim(table(factor1)), byrow=T, dim(table(factor2)))
  deviaz_riga <- round(prof_riga_f1_f2*100, 2) - deviaz_riga

  #column deviations
  deviaz_colonna <- matrix(rep(round(margin.table(f1_f2_rel, 1)*100, 2)), dim(table(factor1)), byrow=F, dim(table(factor2)))
  deviaz_colonna <- round(prof_colonna_f1_f2*100, 2) - deviaz_colonna

  #print deviations
  writeLines(c("DEVIAZIONI RIGA E COLONNA",""))
  print(deviaz_riga, quote = FALSE)
  writeLines("")
  print(deviaz_colonna, quote = FALSE)

  #return data
  return_data <- list(f1_rel*100, f2_rel*100, f1_f2_ass_marg, f1_f2_rel_marg*100, prof_riga_f1_f2_marg,
                      prof_colonna_f1_f2_marg, deviaz_riga, deviaz_colonna)
  names(return_data) <- c("f1_rel", "f2_rel", "f1_f2_ass_marg", "f1_f2_rel_marg", "prof_riga", "prof_colonna",
                          "deviaz_riga", "deviaz_colonna")
  return(return_data)
}

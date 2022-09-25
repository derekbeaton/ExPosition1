# File beersBlindSorting
# Document the data from Lelièvre et al.
# to be included in DistatisR 1.2.0
#
#' @title  Novices and Experts sorted 3 types of beers from 
#' 3 different brewers without and without seeing the beers.
#' 
#' @description 
#' \code{beersBlindSorting}:  several different groups of
#' Novices and Beer-Experts sorted 9 beers with (Vision)
#' or without (Blind) visual information.
#' The 9 beers were
#' 3 types of beers 
#' (blond, amber, and  dark) obtained from 
#' 3 different brewers (Pelforth, Chti, & Leffe).
#' 
#' 
#' @details 
#' Nine different commercial beers (denoted
#'  \code{PelfBL, PelfA, PelfBR, ChtiBL, ChtiA, ChtiBR, LeffBL,
#'  LeffA, and LeffBR}) were evaluated. 
#' These beers came from three different breweries:
#' Pelforth (noted \code{Pelf}), Chti, (\code{Chti}), 
#' and Leffe (\code{Leff}), 
#' and each brewery provided three types of beer:
#'      blond (\code{BL}), amber (\code{A}), and dark (\code{BR}). 
#'
#' For each sorting task the data file gives 
#' the sorting distance matrix: A 9-beers by 9-beers distance matrix
#' in which at the intersection of a row (representing one beer)
#' and a column (representing another beer) a value 
#' of 0 indicates that these two beers were sorted
#' in the same group and
#' a value 
#' of 1 indicates that these two beers were sorted 
#' in different groups.
#'  
#' Multiple groups of novices and experts participated to 
#' the experiments. In the blind condition, the group of experts
#' and one group of novices repeated four times  the sorting
#' taks (replication 1 to 4).
#' 
#' @format A list with 11 lists
#' each storing a 9*9*\eqn{N_k}  \code{cubeOfDistance} and
#' one 9*9 distance table. Specifically:
#' \describe{
#' \item{$EV}{9*9* 17 Experts, Vision}
#' \item{$EBr1}{9*9* 13 Experts, Blind, rep 1}
#' \item{$EBr2}{9*9* 13 Experts, Blind, rep 2}
#' \item{$EBr3}{9*9* 13 Experts, Blind, rep 3}
#' \item{$EBr4}{9*9* 13 Experts, Blind, rep 4}
#' \item{$NV}{9*9* 21 Novices, Vision}
#' \item{$NBr1}{9*9* 18 Novices, Blind, rep 1}
#' \item{$NBr2}{9*9* 18 Novices, Blind, rep 2}
#' \item{$NBr3}{9*9* 18 Novices, Blind, rep 3}
#' \item{$NBr4}{9*9* 18 Novices, Blind, rep 4}
#' \item{$N2B}{9*9* 37 Novices, Blind. (Group 2)}
#' }
#' @author  Maud Lelièvre, 
#'    Sylvie Chollet , Hervé Abdi,  and 
#'    Dominique Valentin.
#' @source A longer description of the data, 
#' story, first analysis, etc. can be found in:
#' Lelièvre M., Chollet, S., Abdi, H., & Valentin, B. (2009). 
#' Beer trained and untrained assessors rely more on vision 
#' than on taste when they categorize beers. 
#' \emph{Chemosensory Perception, 2}, 143-153.
#' available from
#' \url{http://www.utdallas.edu/~herve/abdi-lcav09-inpress.pdf}
"beersBlindSorting"


#_____________________________________________________________________
#
# Print function for the dataset beersBlindSorting
#_____________________________________________________________________

#_____________________________________________________________________
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for cubeSort
#'
#' Change the print function for cubeSort
#'
#' @param x an object of the class cubeSort
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.cubeSort <- function (x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A cube of distance and a distance matrix (class: cubeSort)  \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$cubeOfDistance ", "An I-Products*I-Products*K-Judges 0/1 cube of distance")
  cat("\n$distanceTable  ", "An I-Products**I-Products distance")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.cubeSort
#
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for class 'beersBlind'
#'
#' Change the print function for class 'beersBlind'
#'
#' @param x an object of the class 'beersBlind'
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.beersBlind <- function (x, ...){
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\nA list with the data from Lelievre et al.'s 2009 experiment.  \n")
  cat(" Experts (E) and Novices (N) sorted with Vision (V) or Blind (B)  \n")
  cat(" three types of beers: Blond (BL), Amber (A) or Dark (BR)  \n")
  cat(" from three brewers: PELForth,  CHTI, and LEFF. \n")
  cat(" Blind condition was repeated 4 times (r1-4) for Experts and Novices Group 1.\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$EV   ", "9*9* 17 Experts, Vision:           cubeOfDistance & distanceTable")
  cat("\n$EBr1 ", "9*9* 13 Experts, Blind, rep 1:     cubeOfDistance & distanceTable")
  cat("\n$EBr2 ", "9*9* 13 Experts, Blind, rep 2:     cubeOfDistance & distanceTable")
  cat("\n$EBr3 ", "9*9* 13 Experts, Blind, rep 3:     cubeOfDistance & distanceTable")
  cat("\n$EBr4 ", "9*9* 13 Experts, Blind, rep 4:     cubeOfDistance & distanceTable")
  cat("\n$NV   ", "9*9* 21 Novices, Vision:           cubeOfDistance & distanceTable")
  cat("\n$NBr1 ", "9*9* 18 Novices, Blind, rep 1:     cubeOfDistance & distanceTable")
  cat("\n$NBr2 ", "9*9* 18 Novices, Blind, rep 2:     cubeOfDistance & distanceTable")
  cat("\n$NBr3 ", "9*9* 18 Novices, Blind, rep 3:     cubeOfDistance & distanceTable")
  cat("\n$NBr4 ", "9*9* 18 Novices, Blind, rep 4:     cubeOfDistance & distanceTable")
  cat("\n$N2B  ", "9*9* 37 Novices, Blind. (Group 2): cubeOfDistance & distanceTable")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.cubeSort
#
#_____________________________________________________________________
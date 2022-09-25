# Description for file BeersProjectiveMapping.rda
# Herve Abdi: March 18, 2018.
#
#' @title 
#' 7 (fictitious) assessors sort and verbally describe 7 Beers using
#' \emph{Projective Mapping}.
#' 
#' @description 
#' \code{BeersProjectiveMapping}:
#' 7 (fictitious) assessors evaluated 7 Beers using
#' \emph{Projective Mapping} (with verbal description).
#' 
#' @details 
#' First, Each assessor positioned the 7 beers on a sheet
#' of paper according to the perceived similarity between the beers.
#' For each assessor, the position of the beers was recorded
#' from the \eqn{X} and \eqn{Y} coordinates. 
#' Second, the assessors were asked if they could describe each
#' beer with some freely chosen descriptors. 
#' These descriptors
#' are stored in a dataframe with 7 elements (one per assessor)
#' where each element of the dataframe
#' is a 7 component vector (one per beer) where each element stores
#' the words used to describe a beer 
#' (words are separated with spaces).
#' 
#' @name BeersProjectiveMapping
#' @aliases BeersProjectiveMapping BeersProjectiveNapping
#' @usage data("BeersProjectiveMapping")
#' @docType data
#' @format a list with 3 elements:
#' 1) \code{ProjectiveMapping}: 
#' a matrix of dimensions 7 beers by  7*2 
#' assessors-dimensions of the coordinates of the beers on the sheet
#' of paper;
#' 2) \code{Vocabulary}: a Beers (rows) by Assessors (columns)
#'  data.frame 
#'  where each element of \code{Vocabulary}
#' stores
#' the words used by one assessor to describe a beer
#' (words are separated with spaces);
#' and 3) \code{CT.vocabulary} a matrix storing
#' the  \eqn{I} Products by \eqn{N} 
#'  words (from the Vocabulary) contingency table,
#'  in  \code{CT.vocabulary} the number at the intersection
#'  of a row (beer) and a column (word) is the number
#'  of assessors who used this word to describe that beer.
#' @references  Abdi, H., & Valentin, D., (2007). 
#' Some new and easy ways to describe, compare, 
#'and evaluate products and assessors. 
#'In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds) 
#'\emph{New trends in sensory evaluation 
#'of food and non-food products}.
#' Ho Chi Minh (Vietnam): 
#' Vietnam National University & Ho Chi Minh City Publishing House. 
#' pp. 5-18.
#' @source Abdi, H,  & Valentin, D. (2007). 
#' \url{www.utdallas.edu/~herve}
#' @keywords datasets DistatisR
#' @author Hervé Abdi
NULL

#---------------------------------------------------------------------
# Print function for BeersProjectiveMapping
#*********************************************************************
#---------------------------------------------------------------------
#' Change the print function for the data set: 
#' \code{str_BeersProjectiveMapping}
#'
#' Change the print function for the data set: 
#' \code{str_BeersProjectiveMapping}
#'
#' @param x a list: the data set: {str_BeersProjectiveMapping}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.str_BeersProjectiveMapping <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 7 Assessors Evaluate 7 Beers with Projective Mapping + Description \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$ProjectiveMapping ", "A matrix. Rows   : Beers")
  cat("\n                   ", "          Columns: X&Y coordinates for 7 Assessors.")
  cat("\n$Vocabulary        ", "A dataframe. Vocabulary for 7 Beers by 7 Assessors.")
  cat("\n                   ", "             Each element of the list is a") 
  cat("\n                   ", "             7 (one per beer) component vector ")
  cat("\n                   ", "             (the vocabulary of one assessor).")     
  cat("\n$CT.vocabulary     ","The Products by Vocabulary contingency table ")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.str_BeersProjectiveMapping
#---------------------------------------------------------------------
# Description for file amariSorting.rda
# Herve Abdi: March 18, 2018.
#
#' @title 
#' 25 assessors twice sort and describe 12 amaris (i.e., bitter) 
#' 
#' @description 
#' \code{sortingAmari}: 25 assessors
#' twice sort and describe  12 amaris 
#' (i.e., bitter).
#' The data consist in
#' a list containing 3 objects:
#'  1) \code{Sorting} a data frame with the 12 by 25*2 sorting data,
#' 2) \code{cubeOfVocabulary}: an array of dimensions
#' a 12 products * 41 words  (vocabulary) * 50 assessors-repetition
#' (i.e., 25 assessors *2 repetitions), and 3)
#' a data frame:
#' \code{information4Amaris} storing the description of the amaris.
#' 
#' @details
#' The assessors are described by their 5 character
#' names with the following code: the first letter
#' {m/f} give the gender of the assessors (male versus female),
#' the second and third characters go from 01 to 25 and 
#'  uniquely identify the assessor, 
#'  the fourth and fifth characters
#'  identify the repetition (r1 vs. r2).
#'
#' Some words of the vocabulary have been shortened for convenience;
#' here is the list of 
#' the short and long versions of the descriptors that have shortened:
#' 'coffee_chocolate' <- 'coffee',
#' fortified wine' <- 'wine',
#' 'red fruit' <- 'red',
#' 'soy sauce' <- 'soy',
#' spirit (green)'  <- 'spirit',
#' 'vegetal_green'  <- 'vegetal', and
#' 'warm spice' <- 'spice'.
#' 
#' In the data sets, the amaris are identified with shortened
#' names, the whole names can be found in the data frame
#' \code{information4Amaris}.
#' 
#' @name amariSorting
#' @usage data("amariSorting")
#' @docType data
#' @format
#' a list containing 3 objects:
#'  1) \code{Sorting} a data frame with the 12 by 25*2 sorting data,
#' 2) \code{cubeOfVocabulary}: and array of dimensions
#' a 12 products * 41 words  (vocabulary) * 50 assessors-repetition
#' (i.e., 25 assessors *2 repetitions), and 3)
#' a data frame:
#' \code{information4Amaris} storing the description of the amaris.
#' @references  
#' Lahne, J., Abdi, H., & Heymann, H. (2018). 
#' Rapid sensory profiles with DISTATIS 
#' and barycentric text projection: 
#' An example with amari, bitter herbal liqueurs. 
#' \emph{Food Quality and Preference}, 66, 36-43. 
#' @source Lahne, J., Abdi, H., & Heymann, H. (2018). 
#' #' Rapid sensory profiles with DISTATIS 
#' and barycentric text projection: 
#' An example with amari, bitter herbal liqueurs. 
#' \emph{Food Quality and Preference}, 66, 36-43. 
#' (available from \url{www.utdallas.edu/~herve}).
#' @keywords datasets DistatisR
#' @author Jacob Lahne, Hervé Abdi, & Hildegarde Heymann
NULL


#---------------------------------------------------------------------
# Print function for dataAmari
#*********************************************************************
#---------------------------------------------------------------------
#' Change the print function for the data set: 
#' \code{dataAmari}
#'
#' Change the print function for the data set: 
#' \code{dataAmari}
#'
#' @param x a list: the data set from  {amariSorting}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.dataAmari <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 25 Assessors Twice Sort and Describe 12 Amaris \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$Sorting            ","A data frame storing the sorting matrix of dimensions")
  cat("\n                    ","    12 products * (25 assessors * 2 repetitions). ")
  cat("\n$cubeOfVocubulary   ","An array of dimensions:")
   cat("\n                   ","    12 (products) *41 (words) * (25*2 assessors).")          
   cat("\n                   ","    where a slice gives the description of one assessor.") 
  cat("\n$information4Amaris ","A data frame describing the amaris.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.str_BeersProjectiveMapping
#---------------------------------------------------------------------


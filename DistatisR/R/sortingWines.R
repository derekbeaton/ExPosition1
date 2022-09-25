# Description for file sortingWine.rda
# Hervé Abdi: April 10, 2018.
#
#  sortingWines Preambule ----
#' @title Novices and wines experts sort red, rosé, and white wines
#' 
#' @description 
#' \code{sortingWines}: 26 novices participants
#'  and 19 wine experts sort (by smell alone, 
#'  without visual information) 18 wines (6 red, 6 rosé, and 6 whites)
#' into three categories. The experts also performed a free
#' sorting task on the wines (i.e. with as many groups as the wished).
#' 
#' The data consist in
#' a list containing 4 objects:
#'  1) \code{freeSortExperts}:
#'  a data frame with the 18 wines by 19 experts free sorting data
#'   (the number at the intersection of a row and a colum
#'   indicates the number of the pile in which the wine was sorted);
#' 2) \code{ternarySortExperts}:
#'  a data frame with the 18 wines by 19 experts ternary
#'  (i.e., in three piles) sorting data
#'   (the number at the intersection of a row and a colum
#'   indicates the number of the pile in which the wine was sorted);
#'  3) \code{$ternarySortNovices}:
#'  a data frame with the 18 wines by 19 novices ternary
#'  (i.e., in three piles) sorting data
#'   (the number at the intersection of a row and a colum
#'   indicates the number of the pile in which the wine was sorted);
#'   and 4)
#' \code{vinesDescription} a data frame
#' storing the description of the 18 wines.
#' 
#' @details 
#' The wines were served in dark glasses and the sorting task
#' was performed with red light (this way 
#' all wines look black). In the experiment, 
#' the wines were labeled with three-digit codes, for more details 
#' see Ballester \emph{et al.} (2009).
#' Only the experts performed the free sorting task.
#' 
#' In the data sets, the wines are identified with shortened
#' names, the whole names can be found in the data frame.
#' All the wines were from the 2005 vintage 
#' (see Ballester \emph{et al.}, 2009 for details)
#' 
#' Compared to the original data, some missing data were added
#' to the set after imputation of the missing data 
#' (a total of 4 entries). The current data include 
#' only 19 experts out the original 27 experts.
#' \code{vinesDescription}.
#' @name sortingWines
#' @usage data("sortingWines")
#' @docType data
#' @format
#' a list containing 4 objects:
#'  1) \code{freeSortExperts}:
#'  a data frame with the 18 wines by 19 experts free sorting data
#'   (the number at the intersection of a row and a colum
#'   indicates the number of the pile in which the wine was sorted);
#' 2) \code{ternarySortExperts}:
#'  a data frame with the 18 wines by 19 experts ternary
#'  (i.e., in three piles) sorting data
#'   (the number at the intersection of a row and a colum
#'   indicates the number of the pile in which the wine was sorted);
#'  3) \code{$ternarySortNovices}:
#'  a data frame with the 18 wines by 19 nivices ternary
#'  (i.e., in three piles) sorting data
#'   (the number at the intersection of a row and a colum
#'   indicates the number of the pile in which the wine was sorted);
#'   and 4)
#' \code{vinesDescription} a data frame
#' storing the description of the 18 wines.
#' @references 
#' For more details see:
#'  
#' Ballester, J., Abdi, H., Langlois, J., Peyron, D., & Valentin, D.
#' (2009). {The odor of colors: Can wine experts and novices
#' distinguish the odors of white, red, and rosé wines?}
#' \emph{Chemosensory Perception, 2}, 203-213.
#' 
#' @keywords datasets DistatisR
#' @author Ballester, J., Abdi, H., Langlois, J., 
#' Peyron, D., & Valentin, D.
NULL
# End of sortingWines ----

#_____________________________________________________________________
# Print function sortingWines ----
#_____________________________________________________________________
#_____________________________________________________________________
#' Change the print function for the data set: 
#' \code{dataSortingWines}
#'
#' Change the print function for the data set: 
#' \code{dataSortingWines}
#'
#' @param x a list: the data set: {sortingWines}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.dataSortingWines <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list: 26 Novices and 19 Assessors Sort 18 Wines (6 red/rose/white)  \n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat(rep("-", ndash), sep = "")
  cat("\n$freeSortExperts    ","A data frame storing the free sorting matrix")
  cat("\n                    ","    of dimensions 18 wines * 19 wine experts.")
  cat("\n$ternarySortExperts ","A data frame storing the ternary sorting matrix")
  cat("\n                    ","    of dimensions 18 wines * 19 wine experts. ")      
  cat("\n$ternarySortNovices ","A data frame storing the ternary sorting matrix")
  cat("\n                    ","    of dimensions 18 wines * 26 novices.")
  cat("\n$winesDescription   ","A data frame describing the wines.")
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.dataSortingWines
# end print.dataSorting ----
#_____________________________________________________________________



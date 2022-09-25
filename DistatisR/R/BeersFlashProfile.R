# Description for file BeersFlashProfile
# an excel file storing the Flash Profile data
#
#' @title 
#' An example of an excel file
#' storing the Flash Profile of 6 (fictitious) assessors
#' evaluating 7 (imaginary) beers.
#' This excel file can be read by
#' \code{read.df.excel}.
#' 
#' @description 
#' \code{BeersFlashProfile}:
#' An example of an excel file
#' storing the Flash Profile of 6 (fictitious) assessors
#' evaluating 7 (imaginary) beers.
#' This excel file can be read by
#' \code{read.df.excel}.
#' 
#' @details 
#' In this example of Flash Profiling
#' 6 (fictitious) assessors evaluated 7 (imaginery) beers. 
#' First, Each assessor chose a set of descriptors
#' suited to describe these beers and then ranked 
#' (or rated in variations of the technique) 
#' the beers for each dimension. Note that the descriptors 
#' as well as the number of descriptors vary with the judges.
#' 
#' Note: 
#' The names of the variables starts with the Judges ID (J1- to J6-).
#' 
#' Note: the data are stored in the 
#' Excel Sheet called \code{Rankings} of the excel 
#' file  \code{BeersFlashProfile.xlsx}.
#' @seealso BeersProjectiveMapping BeersProjectiveMapping_xlsx
#' @name BeersFlashProfile
#' @section FileName: BeersFlashProfile.xlsx
#' @docType data
#' @references  Abdi, H., & Valentin, D. (2007). 
#' Some new and easy ways to describe, compare, 
#' and evaluate products and assessors. 
#' In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds) 
#' \emph{New trends in sensory evaluation 
#' of food and non-food products}.
#' Ho Chi Minh (Vietnam): 
#' Vietnam National University & Ho Chi Minh City Publishing House. 
#' pp. 5-18.
#' @source Abdi, H,  & Valentin, D. (2007). 
#' \url{www.utdallas.edu/~herve}
#' @keywords datasets DistatisR
#' @author Hervé Abdi
#' @examples
#' # get the path and file name
#' path2file <- system.file("extdata",
#'                   "BeersFlashProfile.xlsx", package = 'DistatisR')
#' # read the data in excel file with read.df.excel
#' beerDataFlash  <- read.df.excel(path = path2file,
#'                            sheet = 'Rankings')$df.data
#'  # the Flash Profiling data are now in the data.frame beerDataFlash                        
NULL

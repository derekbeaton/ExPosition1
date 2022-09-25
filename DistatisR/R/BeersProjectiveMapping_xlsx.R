# Description for file BeersProjectiveMapping_xlsx
# an excel file storing the Projective Mapping data
#
#' @title 
#' An example of an excel file
#' with Projective Mapping data and vocabulary. 
#' This excel file can be read by
#' \code{read.df.excel}.
#' 
#' @description 
#' \code{BeersProjectiveMapping_xlsx}:
#' an example of an excel file
#' with Projective Mapping and vocabulary. 
#' This excel file can be read by
#' \code{read.df.excel}. In this example
#' 7 (fictitious) assessors evaluated 7 Beers.
#' 
#' @details 
#' In this example of projective mapping with vocabulary,
#' 7 (fictitious) assessors evaluated 7 Beers 
#' First, each assessor positionned the 7 beers on a sheet
#' of paper according  to the perceived similarity between the beers.
#' For each assessor, the position of the beers is recorded
#' from the \eqn{X} and \eqn{Y} coordinates. 
#' Second, the assessors are asked if they can describe the
#' beers with some freely chosen descriptors. These descriptors
#' are stored in a list with 7 elements (one per assessor)
#' where each element of the list
#' is a 7 component vector (one per beer) where each element stores
#' the words used to describe a beer 
#' (words are separated with spaces).
#' The coordinates of the beers on the sheet of paper
#' are stored in the sheet \code{Maps}, the Vocabulary generated
#' by the assessors is stored in the sheet
#' \code{Vocabulary}
#' @seealso BeersProjectiveMapping
#' @name BeersProjectiveMapping_xlsx
#' @section FileName: BeersProjectiveMapping_xlsx.xlsx
#' @aliases BeersProjectiveMapping_xlsx BeersProjectiveNapping_xlsx
#' @docType data
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
#' @examples
#' # get the path and file name
#' path2file <- system.file("extdata",
#'        "BeersProjectiveMapping_xlsx.xlsx", package = 'DistatisR')
#' # read the data in excel file with read.df.excel
#' beerDataPM  <- read.df.excel(path = path2file,
#'                            sheet = 'Maps',
#'                        voc.sheet = 'Vocabulary')
NULL

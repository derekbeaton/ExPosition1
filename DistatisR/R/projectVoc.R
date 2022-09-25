# function projectVoc
# plain barycentric and CA like barycentric projection
#  functions in this file
# projectVoc
# printF4voc


#_____________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(projectVoc)
#
#_____________________________________________________________________


#' @title Compute barycentric projections for count-like
#' description of the items of a \code{distatis}-type of  analysis.
#' 
#' @description \code{projectVoc}
#' Compute barycentric projection for count-like
#' description of the items of a \code{distatis}-type of  analysis.
#' The data need to be non-negative and typically represent
#' the vocabulary (i.e., words) used to describe the items
#' in a sorting/ranking/projective-mapping task.
#'  
#' @details two types of projection are computed: 1)
#' a plain barycentric (words are positioned at the 
#' barycenter--a.k.a. center of mass--of the items it describes) and
#' 2) a correspondence analysis barycentric where the variance
#' of the projected words is equal to the variance
#' of the items (as for correspondence analysis when using the
#' "symmetric" representation).
#' 
#' @param CT.voc a matrix or data.frame
#' storing a
#'  \eqn{I} items by \eqn{J} descriptors
#'  contingency table where the \eqn{i,j}-th cell 
#'  gives the number of times
#'  the \eqn{j}-th descriptor (in the column) 
#'  was used to describe the \eqn{i}-th item
#'  (in the row). \code{CT.voc} needs to containt only non-negative 
#'  numbers.
#' 
#' @param Fi a matrix or data.frame
#' storing the
#'  \eqn{I} items by \eqn{L} factor scores obtained 
#'  from the compromise of a distatis analysis or equivalent.
#' @param namesOfFactors  (Default: NULL), if \code{NULL},
#' \code{projectVoc} uses the names of the columns of 
#' \code{Fi} for the names of the projected factors;
#' if \code{namesOfFactors} is one word then this word is used
#' to name the factors of the projections; if \code{namesOfFactors}
#' is a character vector, it is used to name the factors of
#' the projection.
#' @return a list with 
#' 1) \code{Fvoca.bary}: the barycentric projections of
#' the words,
#' and 2) \code{Fvoca.normed}: the CA normalized 
#' (i.e., variance of projections equals eigenvalue)
#' barycentric projections of
#' the words.
#' @examples 
#' # use the data from the BeersProjectiveMapping dataset
#' data("BeersProjectiveMapping")
#' # Create the I*J*K brick of data
#' zeBrickOfData <- projMap2Cube(
#'                           BeersProjectiveMapping$ProjectiveMapping, 
#'                           shape = 'flat',  nVars = 2)
#' # create the cube of covariance matrices between beers
#' cubeOfCov <- createCubeOfCovDis(zeBrickOfData$cubeOfData)
#' # Call distatis
#' testDistatis <- distatis(cubeOfCov$cubeOfCovariance, Distance = FALSE)
#' # Project the vocabulary onto the factor space
#' F4Voc <- projectVoc(BeersProjectiveMapping$CT.vocabulary, 
#'                     testDistatis$res4Splus$F)
#' @references  Abdi, H., & Valentin, D., (2007). 
#' Some new and easy ways to describe, compare, 
#'and evaluate products and assessors. 
#'In D., Valentin, D.Z. Nguyen, L. Pelletier (Eds) 
#'\emph{New trends in sensory evaluation 
#'of food and non-food products}.
#' Ho Chi Minh (Vietnam): 
#' Vietnam National University & Ho Chi Minh City Publishing House. 
#' pp. 5-18.
#' 
#'  and
#'  
#'  Lahne, J., Abdi, H., & Heymann, H. (2018). 
#'  Rapid sensory profiles with DISTATIS and 
#'  barycentric text projection: An example with amari,
#'   bitter herbal liqueurs. 
#'   \emph{Food Quality and Preference, 66}, 36-43. 
#' @source Abdi, H,  & Valentin, D. (2007). 
#' \url{www.utdallas.edu/~herve}
#' @author Herve Abdi
#' @rdname projectVoc
#' @export 
#' 
projectVoc <- function(CT.voc , # I * N  The CT for vocabulary
                       Fi,      # I * L The factor scores
                       namesOfFactors = NULL
                      ){
     Fi <- as.matrix(Fi)
     if (is.null(namesOfFactors)){
       namesOfFactors <- colnames(Fi)
                         } # end if
     if (length(namesOfFactors) == 1) { 
       namesOfFactors <- paste(namesOfFactors, 1:ncol(Fi)) 
                        } # end if
     CT.voc <- as.matrix(CT.voc)
     if (nrow(Fi) != nrow(CT.voc)){
       stop('CT.voc & Fi should same number of rows')
         } # end if
     if ( any(CT.voc < 0 )){
       stop('All elements of CT.voc should be non-negative')
           } # end if
     #  3.1.1. Compute the columns profiles
     Rvoca <- as.matrix(apply(CT.voc,2, function(x) x / sum(x)))
     # rownames(Rvoca) <- df.name$ID
     #  3.1.2. Pure Barycentric
     Fvoca.bary <- t(Rvoca) %*% Fi 
     # CA- barycentric both sets have the same inertia
     v.bary <- colSums(Fvoca.bary^2)^(-1/2)
     F.eig <- colSums(Fi^2) # get the eigen values with same Dim as F
     Fvoca.normed <- Fvoca.bary %*% diag(F.eig^(1/2) * v.bary ) 
     colnames(Fvoca.normed) <- namesOfFactors
     colnames(Fvoca.bary) <- namesOfFactors
     return.list <- structure(
       list(Fvoca.bary   = Fvoca.bary,
            Fvoca.normed = Fvoca.normed),
            class = 'F4voc')
     
     return(return.list)
   } # End of function projectVoc


#_____________________________________________________________________
# Print function for 'F4voc'
#*********************************************************************
#_____________________________________________________________________
#' Change the print function for objects of the class 
#' \code{'F4voc'} (e.g., output from function 
#' \code{projectVoc}).
#'
#' Change the print function for objects of the class 
#' \code{'F4voc'} (e.g., ooutput from function 
#' \code{projectVoc}).
#'
#' @param x a list: the data set: {str_BeersProjectiveMapping}
#' @param ... the rest
#' @author Herve Abdi
#' @keywords internal
#' @export
print.F4voc <- function (x, ...) {
  ndash = 78 # How many dashes for separation lines
  cat(rep("-", ndash), sep = "")
  cat("\n A list. Barycentric Projections of a Contingency Tables on Factor\n")
  # cat("\n List name: ",deparse(eval(substitute(substitute(x)))),"\n")
  cat("\n", rep("-", ndash), sep = "")
  cat("\n$Fvoca.bary   ", "A matrix: ")
  cat("\n$             ", "  Plain   barycentric projection of items onto the factor space")
  cat("\n$Fvoca.normed ", "A matrix:")
  cat("\n$             ", "  CA-like barycentric projection of items onto the factor space")
  
  cat("\n",rep("-", ndash), sep = "")
  cat("\n")
  invisible(x)
} # end of function print.F4voc
#_____________________________________________________________________

# File for list2cubeOfCovDis
# creating a cube of Covariance/Distance from a list of
# rectangular data table such as e.g., the ones in a flash profile
#
# Created March 21, 2018.

#_____________________________________________________________________
# Function starts here
#
#_____________________________________________________________________
# Helper for roxygen2
#  install.packages('sinew')
#  sinew::makeOxygen(list2CubeOfCovDis)
#
#_____________________________________________________________________

#_____________________________________________________________________
# function list2CubeOfCovDis 
#
#' @title compute a cube of covariance and a cube of distance
#' between the items (rows) of a matrix of measurements 
#' comprising \eqn{K} different blocks of possibly different
#' number of variables.
#' 
#' @description \code{list2CubeOfCovDis}
#' compute a cube of covariance and a cube of 
#' (squared) Euclidean distance
#' between the items (rows) a matrix of measurements 
#' comprising \eqn{K} different blocks of possibly different
#' number of variables.
#' The variables describing the items can scaled to norm 1
#' and centered. The whole matrix for a block 
#' can  be scaled by its first eigenvalue
#' (a la DISTATIS). Blocks can have different number of variables and
#' when all blocks have same number
#' of variables \code{list2CubeOfCovDis} 
#' is a more efficient alternative
#' 
#' @details  
#' The input of  \code{list2CubeOfCovDis} is a 
#' \eqn{I} items by \eqn{J} quantitative variables
#' that are organized in \eqn{K} blocks (i.e., submatrices)
#' each comprising \eqn{J_k} variables (with sum \eqn{J_k = J}).
#' 
#' By default  \code{list2CubeOfCovDis}
#' centers and normalizes each column for each block
#' and then normalize each covariance matrix such that
#' the first eigenvalue of each covariance matrix 
#' (for a given block) is equal to 1.
#'  
#'A \code{distatis} analysis of the Distance matrices with
#' the option \code{Distance = TRUE} will give the same results
#' as the \code{distatis} analysis of the Covariance matrices with
#' the option \code{Distance = FALSE}.
#'    
#' @param Data a matrix of dimensions
#' \eqn{I} items by \eqn{J} quantitative variables
#'  (structured in \eqn{K} blocks of \eqn{J_k} variables
#'  each). No Default.
#' @param Judges a \eqn{J} components character 
#'  vector identifiying the
#'  variables corresponding to each block of variables.
#'  No Default.
#' @param scale (Default: \code{TRUE}), when \code{TRUE}
#' scale to norm 1 each column for each slice.
#' @param center (Default: \code{TRUE}), when \code{TRUE}
#' centers each column.
#' @param ev.scale (Default: \code{TRUE}), when \code{TRUE}
#' normalizes each slice 
#' (i.e., each \eqn{I} items by \eqn{J}  matrix) so that its first 
#' eigenvalue is equal to 1.
#' @return a list with 1) \code{cubeOfCovariance}
#' a cube of \eqn{K}  \eqn{I} by \eqn{I} covariance matrices;
#' and 2) code{cubeOfDistance}
#' a cube of \eqn{K}  \eqn{I} by \eqn{I} 
#' (squared) Euclidean distance
#'   matrices. 
#' @examples 
# # get the path and file name to the excel Flash profile example
#' path2file <- system.file("extdata",
#'                         "BeersFlashProfile.xlsx", 
#'                         package = 'DistatisR')
#' # read the data in the excel file with read.df.excel
#' beerDataFlash  <- read.df.excel(path = path2file,
#'                                sheet = 'Rankings')$df.data
#' # Extract the namers of the judges (first 2 characters)
#' JudgesVars <- colnames(beerDataFlash)
#' zeJudges <- substr(JudgesVars,1,2)
#'  # call list2CubeOfCovDis
#'test.list2 <- list2CubeOfCovDis(Data = beerDataFlash ,
#'                                Judges =  zeJudges)
#' @seealso list2CubeOfCov
#' @author Herve Abdi
#' @rdname list2CubeOfCovDis
#' @export 

list2CubeOfCovDis <- function(Data,   # beerDataFlash 
                              Judges, #  zeJudges
                              scale    = TRUE,
                              center   = TRUE,
                              ev.scale = TRUE){
nVarPerJudge <- as.vector(table(Judges))
namesOfJudges <-  unique(Judges)
namesOfProducts <- rownames(Data)
nI = nrow(Data)
nK = length(nVarPerJudge)

# Get the blocks in a list
kBlocks <- list()
ind = 0
for (k in 1:nK){
  index <- (ind + 1) : (ind + nVarPerJudge[k])
  kBlocks[[k]] <- Data[,index]
  ind <- ind + nVarPerJudge[k]
         } # end loop in k
names(kBlocks) <- namesOfJudges
# Now create the matrices of coVariance and Distance

cubeOfCov <- array(NA, dim = c(nI,nI,nK))
cubeOfDis <- array(NA, dim = c(nI,nI,nK))
# First compute the covariance matrices
for (k in 1:nK){
  # step one normalize per column
  norm_Xk <- apply( kBlocks[[k]],2,scale1,scale,center)
  cov_k   <- norm_Xk %*% t(norm_Xk)
  if (ev.scale) {
    cov_k <- cov_k / eigen(cov_k, 
                           symmetric = TRUE,
                           only.values = TRUE)$values[1]
  } # end if
  cubeOfCov[,,k] <- cov_k
  d_k <- matrix(diag(cov_k), nrow = nI, ncol = nI, byrow = TRUE) +
    matrix(diag(cov_k), nrow = nI, ncol = nI, byrow = FALSE) -
    2*cov_k
  cubeOfDis[,,k] <- d_k 
  # test here
} # end loop on k
dimnames(cubeOfCov)[[1]]  <- namesOfProducts
dimnames(cubeOfCov)[[2]]  <- namesOfProducts
dimnames(cubeOfCov)[[3]]  <- namesOfJudges
dimnames(cubeOfDis)[[1]]  <- namesOfProducts
dimnames(cubeOfDis)[[2]]  <- namesOfProducts
dimnames(cubeOfDis)[[3]]  <- namesOfJudges
return.list <- structure(list(cubeOfCovariance = cubeOfCov,
                              cubeOfDistance   = cubeOfDis),
                         class = 'cubeOfCovDis')
return(return.list)
} # End of funciton list2CubeOfCov
#_____________________________________________________________________
# Note that the print function is the same as createCubeOfCovDis
#_____________________________________________________________________

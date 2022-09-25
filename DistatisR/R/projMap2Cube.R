# A function to format Projective Mapping  or PM like data
# With or without vocabulary
# data will typically be read by read.df.excel
# HA March 18, 2018.
# functions in this file
#  projMap2Cube

#_____________________________________________________________________
#_____________________________________________________________________
# Helper for roxygen2
# sinew::makeOxygen(projMap2Cube)

#' @title \
#' reshape a data matrix from projective mapping
#' into a brick of data for a \code{distatis} analysis.
#' 
#' @description 
#' \code{projMap2Cube} 
#' reshapes a data matrix from projective mapping
#' into a brick of data for a \code{distatis} analysis.
#' With \eqn{I} products, \eqn{J} variables, and
#' \eqn{K} blocks (assessors),
#' the original data can be 1) "flat" 
#' (e.g., \eqn{I} rows as products,
#' columns as \eqn{K} blocks of \eqn{J} Variables) 
#' or 2) "long"
#' "flat" (e.g., \eqn{K} blocks of 
#' \eqn{I} rows as products by assessors,
#' columns as \eqn{J} Variables).
#' 
#' @details the output \code{projMap2Cube} (i.e., the brick of data)
#' is used as input to the function \code{cubeOfCov} that will
#' create the cubeOfDistance (or covariance) that will be used
#' as input of \code{distatis}. 
#' \code{projMap2Cube} guesses the 
#' names of the products and variables from the
#' rownames and columns of the data, but this guess
#' needs to be verified. 
#'  
#' @param Data a data matrix that can be 
#' \eqn{I} rows by \eqn{J*K} columns (when \code{"flat"}) or
#' \eqn{I*K} rows by \eqn{J} columns when \code{"long"}. 
#' @param shape (Default: \code{flat} when \code{"flat"} the data 
#'  matrix has dimensions \eqn{I} rows by \eqn{J*K} columns;
#'  when \code{"long"} the data  matrix has dimensions
#'  \eqn{I*K} rows by \eqn{J} columns.
#' @param nVars Number of variables (default = 2),
#' relevant only when \code{shape = "flat"}.
#' @param nBlocks (Default = \code{NULL}) number
#' of Blocks (i.e., \eqn{K}) of \eqn{I} products.
#' Relevant only when    \code{shape = "long"}.
#' @return An \eqn{I} by \eqn{J} by \eqn{K} array (i.e., a brick)
#' to be used to create a cube of distance or covariance.
#' @examples 
#' # Use the data from the BeersProjectiveMapping dataset
#' data("BeersProjectiveMapping")
#' # Create the I*J_k*K brick of data
#' dataBrick <- projMap2Cube(BeersProjectiveMapping$ProjectiveMapping, 
#'                         shape = 'flat',  nVars = 2)
#' @author Herve Abdi 
#' @rdname projMap2Cube
#' @export 

projMap2Cube <- function(Data, 
                  shape = 'flat', # or 'long'
                  nVars = 2 , # default for Proj Map
                  nBlocks = NULL
){ Data <- as.matrix(Data)
   if (shape == 'long'){# shape is long
    if (is.null(nBlocks)){stop('Missing value for parameter nBlocks')} 
    nIK <- nrow(Data)
    nI  <- nIK / nBlocks
    nJ  <- ncol(Data)
    Z <- matrix(NA, nrow = nI, ncol = nJ*nBlocks)
    for (k in 1:nBlocks){
      i4row <- ((k - 1)*nI + 1):(nI*k)
      j4col <- ((k - 1)*nJ + 1):(nJ*k)
      Z[,j4col] <- Data[i4row,] 
                        } # end loop in k
    if (!is.null(rownames(Data))) {
      rownames(Z) <- rownames(Data)[1:nI]
    }
    if (!is.null(colnames(Data))) {
      colnames(Z) <- rep(colnames(Data), nBlocks)
    }
      Data <- Z
      nVars <- nJ
   } # end if
    nI  <- nrow(Data)
    nJK <- ncol(Data)
    nK  <- nJK / nVars
    Xc  <- as.matrix(Data)
    dim(Xc) <- c(nI,nVars,nK)
    if (!is.null(rownames(Data))) {
    dimnames(Xc)[[1]] <- rownames(Data)
    }
    dimnames(Xc)[[3]] <- paste0('J_',1:nK)
    if (!is.null(colnames(Data))) {
    dimnames(Xc)[[2]] <- colnames(Data)[1:nVars]
    }
    # Voc here
  return( list(cubeOfData = Xc ))
}

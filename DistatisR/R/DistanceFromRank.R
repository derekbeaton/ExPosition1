# functions in this file:
# DistanceFromRank
# Rank2Dist
#_____________________________________________________________________
# A function to create a distance matrix between elements
# that have been ranked.
# The distance is the plain Euclidean/Spearman rank
# HA October 2015
# Input is a rectangular I stimuli by J participants  matrix
# gives back a I*I*J "cube of distance: to be used by
# DistatisR
# Current Version is March 14, 2018.
#_____________________________________________________________________
#' \code{DistanceFromRank}:
#' Creates a 3-dimensional distance array from the results 
#' of a ranking task.
#' 
#' \code{DistanceFromRank}:  
#' Takes the results from a (plain) ranking task 
#' where \eqn{K} assessors rank (with possible ties)
#' \eqn{I} observations on 
#' one dimension and transform it into a brick of data
#' to be used by \code{distatis}.
#' 
#' @details    
#' \code{DistanceFromRank} creates an 
#'  \eqn{I \times I \times K}{I*I*K} 
#' array of distance 
#' in which each of the \eqn{K} "slices"
#' stores the (squared Euclidean ranking) 
#' distance matrix of the \eqn{k}th assessor.  
#' In one of
#' these distance matrices, the distance 
#' between two objects is computed
#' from he Pythagorean theorem. 
#' The ouput ot the function \code{DistanceFromRank} 
#' is used as input for the
#' function \code{\link{distatis}}.
#'
#' The input should have assessors as columns and observations 
#' as rows (see
#' example below).
#' 
#' @param X gives the results of a ranking task 
#' (see example below) as an
#' objects (rows) by assessors (columns) matrix.
#' @return \code{DistanceFromRank} returns an
#'  \eqn{I\times I \times K}{I*I*K}
#' array of distance.
#' @author Herve Abdi
#' @seealso \code{\link{distatis}} \code{\link{DistanceFromSort}} 
#' @export
#' @examples
#' # Use the data set WinesRankingRawData stored in an excel file.
#' path2file <- system.file("extdata",
#'            "WinesRankingRawData.xlsx", package = 'DistatisR')
#' ranking6Wines <- read.df.excel(path = path2file, sheet = 'Ranking')
#' aCubeOfDistance <- DistanceFromRank(ranking6Wines$df.data)
#' 
DistanceFromRank <- function(X){
	# Private functions first
	# Rank2Dist: 
  # Create a sorting distance matrix from the ranking vector
    Rank2Dist <- function(LeRank){# Start LeRank
	   nObj = length(LeRank)
     truc = matrix(LeRank,nObj,nObj)
     # D = (v - v')^2
     # DistMat = abs(truc - t(truc)) # Euclidean
     DistMat =  (truc - t(truc))^2 # squared Euclidean
	 return(DistMat)
     }# End LeRank
	# X is a matrix of sort 
	# Each column is a participant
  # Objects are rank-ordered or rated
	# send back a Object*Object*Participant
	# distance array
	nI = nrow(X);nJ = ncol(X)
	# initialize
	LeCube2Distance = array(0, c(nI,nI,nJ))
	# An Ugly loop
	for(j in 1:nJ){LeCube2Distance[,,j]<-Rank2Dist(X[,j]) }
	dimnames(LeCube2Distance) <-list(rownames(X),rownames(X),colnames(X))
	return(LeCube2Distance)  
} # End of function DistanceFromRank
#_____________________________________________________________________
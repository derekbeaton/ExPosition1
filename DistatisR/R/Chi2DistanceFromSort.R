# functions in this file:
# Chi2DistanceFromSort
# Chi2Dist (as a private function)

#' \code{Chi2DistanceFromSort}:
#' Creates a 3-dimensional \eqn{\chi^2}{chi2}
#' distance array from the results
#' of a sorting task.
#'
#'  \code{Chi2DistanceFromSort}:
#'  Takes the results from a (plain) sorting task where \eqn{K} assessors sort
#' \eqn{I} observations into (mutually exclusive) groups (i.e., one object is
#' in one an only one group).
#'  \code{Chi2DistanceFromSort} creates an \eqn{I \times
#' I \times K}{I*I*K} array of distance in which each of the \eqn{k} "slices"
#' stores the (sorting) distance matrix of the \eqn{k}th assessor.  In one of
#' these distance matrices, the distance between rows is the \eqn{\chi^2}{Chi2}
#' distance between rows when the results of the task are coded as 0/1 group
#' coding (i.e., the "complete disjunctive coding" as used in multiple
#' correspondence analysis, see Abdi & Valentin, 2007, for more)
#'
#' The ouput ot the function \code{Chi2DistanceFromSort} is used as input for the
#' function \code{\link{distatis}}.
#'
#' The input should have assessors
#' as columns and observations as rows (see
#' example below)
#'
#' @param X gives the results of a sorting task
#' (see example below) as a
#' objects (row) by assessors (columns) matrix.
#' @return \code{Chi2DistanceFromSort} returns an
#' \eqn{I\times I \times K}{I*I*K}
#' array of \eqn{K} distances matrices
#' (between the \eqn{I} observations)
#' @author Herve Abdi
#' @seealso \code{\link{distatis}}
#' @references See examples in
#'
#' Abdi, H., Valentin, D., Chollet, S., & Chrea, C. (2007).  Analyzing
#' assessors and products in sorting tasks: DISTATIS, theory and applications.
#' \emph{Food Quality and Preference}, \bold{18}, 627--640.
#'
#' Abdi, H., & Valentin, D., (2007).  Some new and easy ways to describe,
#' compare, and evaluate products and assessors.  In D., Valentin, D.Z. Nguyen,
#' L. Pelletier (Eds) \emph{New trends in sensory evaluation of food and
#' non-food products.} Ho Chi Minh (Vietnam): Vietnam National University-Ho
#' chi Minh City Publishing House. pp. 5--18.
#'
#' Abdi, H., & Valentin, D. (2007). Multiple correspondence analysis.  In N.J.
#' Salkind (Ed.): \emph{Encyclopedia of Measurement and Statistics.} Thousand
#' Oaks (CA): Sage. pp. 651-657.
#'
#' These papers are available from \url{www.utdallas.edu/~herve}
#' @keywords DistatisR
#' @examples
#'
#' #  1. Get the data from the 2007 sorting example
#' #      this is the eay they look from Table 1 of
#' #      Abdi et al. (2007).
#' #                       Assessors
#' #                  1 2 3 4 5 6 7 8 9 10
#' # Beer        Sex  f m f f m m m m f m
#' #            -----------------------------
#' #Affligen          1 4 3 4 1 1 2 2 1 3
#' #Budweiser         4 5 2 5 2 3 1 1 4 3
#' #Buckler_Blonde    3 1 2 3 2 4 3 1 1 2
#' #Killian           4 2 3 3 1 1 1 2 1 4
#' #St. Landelin      1 5 3 5 2 1 1 2 1 3
#' #Buckler_Highland  2 3 1 1 3 5 4 4 3 1
#' #Fruit Defendu     1 4 3 4 1 1 2 2 2 4
#' #EKU28             5 2 4 2 4 2 5 3 4 5
#'
#' #
#' # 1.1. Create the
#' #     Name of the Beers
#' BeerName <- c('Affligen', 'Budweiser','Buckler Blonde',
#'               'Killian','St.Landelin','Buckler Highland',
#'               'Fruit Defendu','EKU28')
#' # 1.2. Create the name of the Assessors
#' #      (F are females, M are males)
#' Juges <- c('F1','M2', 'F3', 'F4', 'M5', 'M6', 'M7', 'M8', 'F9', 'M10')
#'
#' # 1.3. Get the sorting data
#' SortData <- c(1, 4, 3, 4, 1, 1, 2, 2, 1, 3,
#'               4, 5, 2, 5, 2, 3, 1, 1, 4, 3,
#'               3, 1, 2, 3, 2, 4, 3, 1, 1, 2,
#'               4, 2, 3, 3, 1, 1, 1, 2, 1, 4,
#'               1, 5, 3, 5, 2, 1, 1, 2, 1, 3,
#'               2, 3, 1, 1, 3, 5, 4, 4, 3, 1,
#'               1, 4, 3, 4, 1, 1, 2, 2, 2, 4,
#'               5, 2, 4, 2, 4, 2, 5, 3, 4, 5)
#' # 1.4 Create a data frame
#' Sort <- matrix(SortData,ncol = 10, byrow= TRUE, dimnames = list(BeerName, Juges))
#' #
#' #-----------------------------------------------------------------------------
#' # 2. Create the set of distance matrices (one distance matrix per assessor)
#' #    (use the function DistanceFromSort)
#' DistanceCube <- Chi2DistanceFromSort(Sort)
#' #-----------------------------------------------------------------------------
#' # 3. Call the DISTATIS routine with the cube of distance
#' #       obtained from DistanceFromSort as a parameter for the distatis function
#' testDistatis <- distatis(DistanceCube)
#'
#' @importFrom stats model.matrix
#' @export
Chi2DistanceFromSort <-
function(X){
	# Create a Cube of Chi2Distance from a Sorting Task
	# X is a matrix of sort
	# Each column is a participant
	# 2 objects in a column with the same number
	# were in the same group
	# send back a Object*Object*Participant
	# distance array

	# First Chi2Dist as a private function
	Chi2Dist    <- function(X){#compute the chi2 distance
	# between the rows of a matrix
    # send back the distance and the vector of mass

    # 1. transform X into Row profiles
    xip = rowSums(X)
    R <- X / xip
    # 2. Masses, weights
    xpp = sum(xip)             # grand total
    m <- xip / xpp             # masses
    c <- colSums(X) / xpp      # row barycenter
    w = 1/c                    # columns weights
    # Preprocess R
    Rc = t(t(R) - c)           # deviations to barycenter
    Rtilde = t(t(Rc)*sqrt(w))  # weighted R
    S =  Rtilde %*% t(Rtilde)     # covariance
    s = diag(S) # diag of
    D = (s - S) + t(s-S)       # Chi2 distance matrix
    return(list(Distance = D, masses = m))
} # end of private Chi2Dist

	nI = nrow(X);nJ = ncol(X)
	# initialize
	LeCube2Distance = array(0, c(nI,nI,nJ))
	# Horrible  Loop!
	for(j in 1:nJ){
		# get Chi2Distance of jth assessor expressed as disjunctive coding
		dist =  Chi2Dist(stats::model.matrix(~ as.factor(as.matrix(X[,j])) - 1))
		LeCube2Distance[,,j] <- dist$Distance
		 } # done ugly loop
	dimnames(LeCube2Distance) <-list(rownames(X),rownames(X),colnames(X))
	return(LeCube2Distance)
}

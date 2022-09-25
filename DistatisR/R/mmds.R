# functions in this file
# mmds

#'  Metric (classical) Multidimensional Scaling
#' (a.k.a Principal Coordinate
#' Analysis) of a (Euclidean) Distance Matrix.
#'
#' \code{mmds}: Perform a Metric Multidimensional Scaling
#' (MMDS) of an (Euclidean) distance matrix measured between a set of
#' objects (with or without masses).
#'
#'@details
#' \code{mmds} gives factor scores that make it possible
#' to draw a map of the objects
#' such that the distances between objects
#' on the map best approximate the
#' original distances between objects.
#'
#' @section Method: MMDS transform the distance matrix
#' into a (double centered)
#' covariance-like matrix which is then analyzed
#' via its eigen-decomposition.  The
#' factor scores of each dimension are scaled
#' such that their variance (i.e.,
#' the sum ot their weighted squared factor scores)
#' is equal to the eigen-value
#' of the corresponding dimension.
#'  Note that if the \code{masses} vector is
#' absent, equal masses
#' (i.e., 1 divided by number of objects) are used.
#' @section Technicalities:
#' the distance matrix to be analyzed is supposed to be a
#' \eqn{squared} euclidean distance matrix.
#' Note also that a non Euclidean distance matrix
#' will have negative eigenvalues that will be ignored
#' by \code{mmds} which, therefore, gives the best Euclidean
#' approximation to this non-Euclidean distance matrix
#' (note that, nonmetric MDS maybe a better technique in these cases).
#'
#' @param DistanceMatrix A squared (assumed to be Euclidean)
#' distance matrix
#' @param masses A vector of masses
#' (i.e., a set of non-negative numbers with a sum of
#' 1) of same dimensionality as the number
#' of rows of \code{DistanceMatrix}.
#' @return Sends back a list \item{LeF}{factor scores for the objects.}
#' \item{eigenvalues}{the eigenvalues for the factor scores
#'  (i.e., a variance).}
#' \item{tau}{the percentage of explained variance by each dimension.}
#' \item{Contributions}{give the proporion of explained
#' variance by an object
#' for a dimension.}
#' @author Herve Abdi
#' @seealso \code{\link{GraphDistatisCompromise}} \code{\link{distatis}}
#' @references The procedure and references are detailled in:
#' Abdi, H. (2007).
#' Metric multidimensional scaling.
#' In N.J. Salkind (Ed.): \emph{Encyclopedia
#' of Measurement and Statistics}.
#' Thousand Oaks (CA): Sage. pp. 598--605.
#'
#' (Paper available from \url{www.utdallas.edu/~herve}).
#' @keywords DistatisR MMDS mmds distatis
#' @examples
#'
#' # An example of MDS from Abdi (2007)
#' # Discriminability of Brain States
#' # Table 1.
#' # 1. Get the distance matrix
#' D <- matrix(c(
#' 0.00, 3.47, 1.79, 3.00, 2.67, 2.58, 2.22, 3.08,
#' 3.47, 0.00, 3.39, 2.18, 2.86, 2.69, 2.89, 2.62,
#' 1.79, 3.39, 0.00, 2.18, 2.34, 2.09, 2.31, 2.88,
#' 3.00, 2.18, 2.18, 0.00, 1.73, 1.55, 1.23, 2.07,
#' 2.67, 2.86, 2.34, 1.73, 0.00, 1.44, 1.29, 2.38,
#' 2.58, 2.69, 2.09, 1.55, 1.44, 0.00, 1.19, 2.15,
#' 2.22, 2.89, 2.31, 1.23, 1.29, 1.19, 0.00, 2.07,
#' 3.08, 2.62, 2.88, 2.07, 2.38, 2.15, 2.07, 0.00),
#' ncol = 8, byrow=TRUE)
#' rownames(D) <- c('Face','House','Cat','Chair','Shoe','Scissors','Bottle','Scramble')
#' colnames(D) <- rownames(D)
#' # 2. Call mmds
#' BrainRes <- mmds(D)
#' # Note that compared to Abdi (2007)
#' # the factor scores of mmds are equal to F / sqrt(nrow(D))
#' # the eigenvalues of mmds are equal to \Lambda *{1/nrow(D)}
#' # (ie., the normalization differs but the results are proportional)
#' # 3. Now a pretty plot with the prettyPlot function from prettyGraphs
#' prettyGraphs::prettyPlot(BrainRes$FactorScore,
#'            display_names = TRUE,
#'            display_points = TRUE,
#'            contributionCircles = TRUE,
#'            contributions = BrainRes$Contributions)
#' # 4. et Voila!
#' @rdname mmds
#' @export 
mmds <- function(DistanceMatrix, masses = NULL){
	# mmds (metric or classical mds)
	# Quick and dirty mds with or without masses
	# here to accompagny distatis and allow chi2 distance analyses
	# WARNING mmds assumes that the distance matrix
	# is (generalized) Euclidean
	# and so it keeps only the positive eigenvalues
	D <- DistanceMatrix   # Being lazy!
	nI <- nrow(D)
	if (is.null(masses)) {masses <- rep(1/nI,nI)} # create masses if needed
	m <- masses # lazy again
	LeM = t(kronecker(m,t(rep(1,nI))))  # PM repmat
	Xhi <- diag(1,nI) -  LeM # centering matrix
	#print(Xhi)
	S <- -.5*sqrt(LeM)*Xhi %*% D %*% t(Xhi) * sqrt(t(LeM)) 
	# Double Centered SCP matrix
	eig <- eigen(S, symmetric = TRUE) # Eigen-decomposition of S

	# clean to keep positive eigenvalues only
	Cleaner =  which(eig$value > 0)
	U <- eig$vector[,Cleaner]
    L <- eig$values[Cleaner]
     Nom2Dim = paste('dim',1:length(L))
         names(L) <- Nom2Dim
    Nom2Row = rownames(D)
    # Factor scores
	LeF <- kronecker(1/sqrt(m),t(rep(1,length(L))))*t(t(U) *  sqrt(L) )

	rownames(LeF) <- Nom2Row
	colnames(LeF) <- Nom2Dim
	# Percentage of inertia
	tau <- round(100*(L/sum(L)),digits=2)
	names(tau) <- Nom2Dim
	# Contributions
	Ctr <- kronecker(m,t(rep(1,length(L))))*t(t(LeF^2) /L )
	rownames(Ctr) <- Nom2Row
	colnames(Ctr) <- Nom2Dim
	#	return(list(S=S,fi=LeF,pdq=eig,M=LeM))
	return(list(FactorScores = LeF, eigenvalues = L,
	            Contributions = Ctr,
	            percentage = tau,
	            M = LeM))
}

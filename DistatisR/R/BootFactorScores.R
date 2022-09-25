# functions in this file:
# BootFactorScores
# HA.



#' Computes observation
#' factor scores
#' Bootstrap replicates from partial factor scores.
#'
#'@description
#' \code{BootFactorScores}: Computes Bootstrap replicates
#' of the factor scores of
#' the observations from the partial factor scores.
#' \code{BootFactorScores} is typically
#' used to create confidence intervals and to compute
#' Bootstrap ratios.
#'
#' @section Technicalities:
#' The input of \code{BootFactorScores} is obtained from the
#' \code{distatis} function, the output is a 3-way array
#' of dimensions number
#' of observations by number of factors by number of replicates.
#' The output is
#' typically used to plot confidence intervals
#' (i.e., ellipsoids or convex
#' hulls) or to compute \eqn{t}-like statistic
#' called \emph{bootstrap ratios}.
#' To compute a bootstrapped sample a set of
#' \eqn{K} distance matrices is
#' selected with replacement from the original set of \eqn{K} distance
#' matrices.
#' The partial factors scores of the selected distance matrices are
#' then averaged to produce the bootstrapped estimate
#' of the factor scores of
#' the observations.
#' This approach is also called \emph{partial boostrap} by
#' Lebart (2007, see also Chateau & Lebart 1996).
#' It has the advantage of
#' being very fast even for very large data sets.
#' Recent work (Cadoret & Husson,
#' 2012), however, suggests that partial boostrap could lead
#' to optimistic
#' bootstrap estimates when the number of distance matrices
#' is large and that
#' it is preferable to use instead a \emph{total boostrap}
#' approach (i.e.,
#' creating new compromises by
#' resampling and then projecting them on the
#' common solution see function
#' \code{BootFromCompromise}, and Cadoret &
#' Husson, 2012 see also Abdi \emph{et al}., 2009 for an example).
#'
#' @param PartialFS The partial factor scores (e.g., as obtained from
#' \code{distatis}).
#' @param niter number of boostrap iterations (default = 1000)
#' @return the output is a 3-way array of dimensions
#' "number of observations by
#' number of factors by number of replicates."
#' @author Herve Abdi
#' @seealso \code{\link{BootFromCompromise}}
#' \code{\link{GraphDistatisBoot}}
#' @references Abdi, H., & Valentin, D., (2007).
#' Some new and easy ways to
#' describe, compare, and evaluate products and assessors.
#' In D., Valentin,
#' D.Z. Nguyen, L. Pelletier (Eds)
#' \emph{New trends in sensory evaluation of
#' food and non-food products}.
#'  Ho Chi Minh (Vietnam): Vietnam National
#' University-Ho chi Minh City Publishing House. pp. 5-18.
#'
#' Abdi, H., Dunlop, J.P., & Williams, L.J. (2009).
#' How to compute reliability
#' estimates and display confidence and tolerance intervals
#' for pattern
#' classiffers using the Bootstrap and
#' 3-way multidimensional scaling
#' (DISTATIS). \emph{NeuroImage}, \bold{45}, 89--95.
#'
#' Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012).
#' STATIS
#' and DISTATIS: Optimum multi-table principal component
#' analysis and three way
#' metric multidimensional scaling.
#' \emph{Wiley Interdisciplinary Reviews:
#' Computational Statistics}, \bold{4}, 124--167.
#'
#' These papers are available from \url{www.utdallas.edu/~herve}
#'
#' Additional references:
#'
#' Cadoret, M., Husson, F. (2012) Construction and evaluation
#' of confidence
#' ellipses applied at sensory data.
#' \emph{Food Quality and Preference},
#' \bold{28}, 106--115.
#'
#' Chateau, F., & Lebart, L. (1996). Assessing sample variability
#' in the
#' visualization techniques related to principal component
#' analysis: Bootstrap
#' and alternative simulation methods. In A. Prats (Ed.),
#' \emph{Proceedings of
#' COMPSTAT 2006.} Heidelberg: Physica Verlag.
#'
#' Lebart, L. (2007). Which bootstrap for principal
#' axes methods?  In
#' \emph{Selected contributions in data analysis and classification,
#' COMPSTAT
#' 2006}.  Heidelberg: Springer Verlag.
#'
#' @keywords sample bootstrap
#' @examples
#' # 1. Load the Sort data set from the SortingBeer example
#' #    (available from the DistatisR package)
#' data(SortingBeer)
#' # Provide an 8 beers by 10 assessors set of
#' # results of a sorting task
#' #-----------------------------------------------------------------------------
#' # 2. Create the set of distance matrices (one distance matrix per assessor)
#' #    (ues the function DistanceFromSort)
#' DistanceCube <- DistanceFromSort(Sort)
#'
#' #-----------------------------------------------------------------------------
#' # 3. Call the DISTATIS routine with the cube of distance as parameter
#' testDistatis <- distatis(DistanceCube)
#' # The factor scores for the beers are in
#' # testDistatis$res4Splus$F
#' # the partial factor score for the beers for the assessors are in
#' #  testDistatis$res4Splus$PartialF
#' #
#' # 4. Get the bootstraped factor scores (with default 1000 iterations)
#' BootF <- BootFactorScores(testDistatis$res4Splus$PartialF)
#'
#' @export
BootFactorScores <- function(PartialFS,niter = 1000){
# Bootstrap Confidence interval for DISTATIS
# computed on the partial factor scores
# PartialFS is nI,nF,nK array of the partial factor scores
# with nI # of objects, nF # of factors, nK # of observations
# (obtained from DISTATIS program)
# niter: how many iterations? default =1000
print(c('Bootstrap On Factor Scores. Iterations #: ',niter),quote=FALSE)


# Initialize the Bootstrap Table
nI <- dim(PartialFS)[1]
nK <- dim(PartialFS)[3]
nF <- dim(PartialFS)[2]
BootF <- array(0,dim = c(nI,nF,niter))
# Iterate Bootstrap
for (n in 1:niter){
	BootF[,,n] <- apply(PartialFS[,,sample(nK,nK,TRUE)],c(1,2),mean)
    }
rownames(BootF) <- rownames(PartialFS)
colnames(BootF) <- colnames(PartialFS)
return(BootF)
}

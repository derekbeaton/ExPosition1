

#' InPosition: Inference Tests for \emph{Ex}ploratory Analysis with the
#' Singular Value Decom\emph{Position} (\code{ExPosition}).
#' 
#' InPosition provides multiple forms of inference tests for the
#' \code{\link[ExPosition]{ExPosition}} package.
#' 
#' @name InPosition-package
#' @aliases InPosition-package InPosition
#' @author Questions, comments, compliments, and complaints go to Derek Beaton
#' \email{exposition.software@@gmail.com}. Also see the bug-tracking and live
#' update website for ExPosition:
#' https://github.com/derekbeaton/ExPosition1\cr\cr Primary authors and
#' contributors are: Derek Beaton, Joseph Dunlop, and Hervé Abdi
#' @seealso \code{\link{epPCA.inference.battery}},
#' \code{\link{epCA.inference.battery}}, \code{\link{epMCA.inference.battery}}.
#' There are no inference tests for MDS at this time. We recommend PCA for
#' inference instead of MDS (some MDS inference tests require the rectangluar
#' table, not the distances, so it is easier to just use PCA).\cr\cr
#' 
#' See also \code{\link{inGraphs}} for graphing and \code{\link{caChiTest}} for
#' an alternate to resampling methods for Correspondence Analysis.
#' @references Permutation:\cr Berry, K. J., Johnston, J. E., & Mielke, P. W.
#' (2011). Permutation methods. \emph{Wiley Interdisciplinary Reviews:
#' Computational Statistics},\emph{3}, 527–542. \cr Peres-Neto, P. R., Jackson,
#' D. A., & Somers, K. M. (2005). How many principal components? Stopping rules
#' for determining the number of non-trivial axes revisited.
#' \emph{Computational Statistics & Data Analysis}, \emph{49}(4), 974-997. \cr
#' \cr Bootstrap:\cr Chernick, M. R. (2008). \emph{Bootstrap methods: A guide
#' for practitioners and researchers} (Vol. 619). Wiley-Interscience.\cr
#' Hesterberg, T. (2011). Bootstrap. \emph{Wiley Interdisciplinary Reviews:
#' Computational Statistics}, \emph{3}, 497–526. \cr
#' @keywords package multivariate
#' @import prettyGraphs ExPosition
#' @importFrom stats pchisq pnorm
#' @importFrom utils setTxtProgressBar txtProgressBar
#' 
#' 
"_PACKAGE"




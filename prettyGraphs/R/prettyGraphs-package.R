

#' prettyGraphs: for publication-quality graphics.
#' 
#' prettyGraphs is a package that produces simple, crisp, publication-quality
#' graphics for multivariate analyses.
#' 
#' 
#' @name prettyGraphs
#' @author Derek Beaton <exposition.software@@gmail.com>
#' @seealso \code{\link{prettyPlot}} \code{\link{contributionBars}}
#' \code{\link{correlationPlotter}} \code{\link{peeledHull}}
#' \code{\link{minmaxHelper}} \code{\link{repmat}}
#' @references Three functions were copied/derived for use in prettyGraphs:
#' peeledHull, add.alpha, and repmat.\cr \cr
#' 
#' For peeledHull see:\cr http://carme-n.org/?sec=code2\cr Greenacre, M. J.
#' (2007). Correspondence Analysis in Practice. \emph{Chapman and Hall}.\cr \cr
#' 
#' For repmat see:\cr http://cran.r-project.org/doc/contrib/R-and-octave.txt
#' 
#' For add.alpha see:\cr
#' https://magesblog.com/post/2013-04-30-how-to-change-alpha-value-of-colours-in/
#' @importFrom grDevices chull col2rgb colors dev.new hsv rgb 
#' @importFrom graphics abline axis barplot box lines mtext par plot points text 
#' @importFrom stats cor 
#' @importFrom utils head tail
#' @keywords package graphs multivariate
#' @aliases prettyGraphs-package prettyGraphs
NULL
"_PACKAGE"



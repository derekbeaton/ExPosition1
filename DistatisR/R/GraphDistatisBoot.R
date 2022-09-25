#'  \code{GraphDistatisBoot} Plot maps of the factor scores
#'  of the observations and their bootstrapped
#' confidence intervals (as confidence ellipsoids or peeled hulls)
#' for a
#' \acronym{DISTATIS} analysis.
#'
#' \code{GraphDistatisBoot} plots maps of the factor scores of the observations
#' from a \code{\link{distatis}} analysis.
#' \code{GraphDistatisBoot} gives a
#' map of the factors scores of the observations plus the boostrapped
#' confidence intervals drawn as "Confidence Ellipsoids" at
#' the \code{percentage} level (see parameter \code{percentage}).
#'
#' The ellipses are plotted using the function \code{dataEllipse()} from the
#' package \code{car}. The peeled hulls are plotted using the function
#' \code{peeledHulls()} from the package \code{prettyGraphs}.
#'
#' Note that, in the current version, the graphs are plotted as R-plots and are
#' \emph{not} passed back by the function.  So the graphs need to be saved "by
#' hand" from the R graphic windows.  We plan to improve this in a future
#' version.
#' See also package \code{PTCA4CATA} for \code{ggplot2} based graphs.
#'
#' @param FS The factor scores of the observations (\code{$res4Splus$F} from
#' \code{\link{distatis}})
#' @param FBoot is the bootstrapped factor scores array (\code{FBoot} obtained
#' from \code{\link{BootFactorScores}} or \code{\link{BootFromCompromise}})
#' @param axis1 The dimension for the horizontal axis of the plots.
#' (default = 1).
#' @param axis2 The dimension for the vertical axis of the plots
#' (default = 2).
#' @param item.colors When present, 
#' should be a column matrix (dimensions of
#' observations and 1).  
#' Gives the color-names to be used to color the plots.
#' Can be obtained as the output of this or the other graph routine. 
#' If \code{NULL},
#' \code{prettyGraphs} chooses.
#' @param ZeTitle General title for the plots (deefault is
#' 'Distatis-Bootstrap').
#' @param constraints constraints for the axes
#' @param nude When \code{TRUE} do not plot
#' the names of the observations (default is \code{FALSE}).
#' @param Ctr Contributions of each observation.
#' If \code{NULL} (default), these are
#' computed from \code{FS}.
#' @param lwd Thickness of the line plotting the ellipse or hull
#' (default = 3.5).
#' @param ellipses a boolean. When \code{TRUE} (default)
#' will plot ellipses (from the
#' \code{car} package).
#' When \code{FALSE} will plot peeled hulls (from
#' \code{prettyGraphs} package).
#' @param fill when \code{TRUE}, fill in the ellipse with color.
#' Relevant for
#' ellipses only.
#' @param fill.alpha transparency index
#' (a number between 0 and 1) when filling in the ellipses.
#' Relevant for
#' ellipses only (default = .27).
#' @param percentage A value to determine the percent
#' coverage of the bootstrap
#' partial factor scores to provide ellipse
#' or hull confidence intervals (default = .95).
#' @return \item{constraints}{A set of plot constraints that are returned.}
#' \item{item.colors}{A set of colors for the observations are returned.}
#' @author Derek Beaton and Herve Abdi
#' @seealso \code{\link{GraphDistatisAll}}
#' \code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}}
#' \code{\link{GraphDistatisBoot}} \code{\link{GraphDistatisRv}}
#' \code{\link{distatis}}
#' @references The plots are similar to the graphs described in:
#'
#' Abdi, H., Williams, L.J., Valentin, D., & Bennani-Dosse, M. (2012). STATIS
#' and DISTATIS: Optimum multi-table principal component analysis and three way
#' metric multidimensional scaling. \emph{Wiley Interdisciplinary Reviews:
#' Computational Statistics}, \bold{4}, 124--167.
#'
#' Abdi, H., Dunlop, J.P., & Williams, L.J. (2009). How to compute reliability
#' estimates and display confidence and tolerance intervals for pattern
#' classiffers using the Bootstrap and 3-way multidimensional scaling
#' (DISTATIS). \emph{NeuroImage}, \bold{45}, 89--95.
#'
#' Abdi, H., & Valentin, D., (2007). Some new and easy ways to describe,
#' compare, and evaluate products and assessors. In D., Valentin, D.Z. Nguyen,
#' L. Pelletier (Eds) \emph{New trends in sensory evaluation of food and
#' non-food products}.  Ho Chi Minh (Vietnam): Vietnam National University-Ho
#' chi Minh City Publishing House. pp. 5--18.
#'
#' These papers are available from \url{www.utdallas.edu/~herve}
#' @keywords DistatisR
#' @examples
#'
#' # 1. Load the Sort data set from the SortingBeer example
#' # (available from the DistatisR package)
#' data(SortingBeer)
#' # Provide an 8 beers by 10 assessors results of a sorting task
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
#' #-----------------------------------------------------------------------------
#' # 5. Create the Graphics with GraphDistatisBoot
#' #
#' GraphDistatisBoot(testDistatis$res4Splus$F,BootF)
#'
#'
#' @importFrom car dataEllipse
#' @importFrom prettyGraphs peeledHull
#' @export
GraphDistatisBoot <-
function(FS,FBoot, axis1=1, axis2=2, item.colors=NULL,
         ZeTitle= 'Distatis-Bootstrap', constraints = NULL,
         nude = FALSE, Ctr=NULL, lwd = 3.5, ellipses = TRUE,
         fill = TRUE, fill.alpha = .27, percentage = 0.95){

	if (is.null(item.colors)){
		item.design <- diag(dim(FS)[1])
		item.colors <- as.matrix(createColorVectorsByDesign(item.design)$oc)
	}

	if(is.null(constraints)){
		real.minimum <- min(c(FS,FBoot))
    	real.maximum <- max(c(FS,FBoot))
	    real.value <- max(c(abs(real.minimum),abs(real.maximum)))
   		#set up a constraints list
		constraints <- list(minx=-real.value, maxx=real.value,
		                    miny=-real.value,maxy=real.value)
	}

	Compromise.Out <- GraphDistatisCompromise(FS,
	                               axis1=axis1,axis2=axis2,
	                               constraints=constraints,
	                               item.colors=item.colors,
	                               ZeTitle=ZeTitle,
	                               nude=nude,Ctr=Ctr)
    for(i in 1:dim(FS)[1]){
    	if(ellipses){
	        car::dataEllipse(x=FBoot[i,axis1,],y=FBoot[i,axis2,],
	                    add=TRUE,levels=percentage,
	                    plot.points=FALSE,
	                    center.pch = FALSE,
	                    col = item.colors[i,], lwd = lwd,
	                    fill=fill,fill.alpha=fill.alpha)
		}else{
			##this intentionally overlays a black background hull.
		  prettyGraphs::peeledHull(t(FBoot[i,,]),
			           x_axis=axis1,
			           y_axis=axis2,
			           percentage=percentage,
			           lwd=lwd)
			if(lwd<3){
				color.lwd <- 1
			}else{
				color.lwd <- lwd-2
			}
		  prettyGraphs::peeledHull(t(FBoot[i,,]),x_axis=axis1,y_axis=axis2,
			           percentage=percentage,
			           col=item.colors[i,],lwd=color.lwd)
		}
	}
    return(Compromise.Out)
#return(LeRetour)
}

#' Plot maps of the factor scores of the observations for a \acronym{DISTATIS}
#' analysis
#'
#' Plot maps of the factor scores of the observations for a \acronym{distatis}
#' analysis. \code{GraphDistatis} gives a map of the factor scores for the
#' observations. The labels of the observations are plotted by defaults but can
#' be omitted (see the \code{nude=TRUE} option).
#'
#' Note that, in the current version, the graphs are plotted as R-plots and are
#' \emph{not} passed back by the routine.  So the graphs need to be saved "by
#' hand" from the R graphic windows.  We plan to improve this in a future
#' version.
#'
#' @param FS The factor scores of the observations (\code{$res4Splus$F}from
#' \code{distatis}).
#' @param axis1 The dimension for the horizontal axis of the plots.
#' @param axis2 The dimension for the vertical axis of the plots.
#' @param constraints constraints for the axes
#' @param item.colors A \eqn{I\times 1}{I*1} matrix (with \eqn{I} = #
#' observations) of color names for the observations. If \code{NULL} (default),
#' \code{prettyGraphs} chooses.
#' @param ZeTitle General title for the plots.
#' @param nude (default \code{FALSE}) 
#' When \code{nude} is \code{TRUE} the labels for the observations
#' are not plotted (useful when editing the graphs for publication).
#' @param Ctr Contributions of each observation. If NULL (default), these are
#' computed from FS
#' @return \item{constraints}{A set of plot constraints that are returned.}
#' \item{item.colors}{A set of colors for the observations are returned.}
#' @author Derek Beaton and Herve Abdi
#' @seealso \code{\link{GraphDistatisAll}}
#' \code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}}
#' \code{\link{GraphDistatisBoot}} \code{\link{GraphDistatisRv}}
#' \code{\link{distatis}}
#' @references The plots are similar to the graphs from
#'
#' Abdi, H., Valentin, D., O'Toole, A.J., & Edelman, B. (2005).  DISTATIS: The
#' analysis of multiple distance matrices.  \emph{Proceedings of the IEEE
#' Computer Society: International Conference on Computer Vision and Pattern
#' Recognition}.  (San Diego, CA, USA). pp. 42-47.
#'
#' see \url{www.utdallas.edu/~herve}
#' @keywords DistatisR mds
#' @examples
#'
#' # 1. Load the DistAlgo data set (available from the DistatisR package)
#' data(DistAlgo)
#' # DistAlgo is a 6*6*4 Array (face*face*Algorithm)
#' #-----------------------------------------------------------------------------
#' # 2. Call the DISTATIS routine with the array of distance (DistAlgo) as parameter
#' DistatisAlgo <- distatis(DistAlgo)
#' # 3. Plot the compromise map with the labels for the first 2 dimensions
#' # DistatisAlgo$res4Splus$F are the factors scores for the 6 observations (i.e., faces)
#' # DistatisAlgo$res4Splus$PartialF are the partial factors scores
#' 	##(i.e., one set of factor scores per algorithm)
#'  GraphDistatisCompromise(DistatisAlgo$res4Splus$F)
#'
#' @importFrom prettyGraphs prettyPlot
#' @export
GraphDistatisCompromise <-
function(FS,axis1=1,axis2=2,constraints=NULL,
         item.colors=NULL,ZeTitle= 'Distatis-Compromise',nude=FALSE,Ctr=NULL){

    if(is.null(item.colors)){
		item.design <- diag(dim(FS)[1])
		item.colors <- as.matrix(createColorVectorsByDesign(item.design)$oc)
	}

	if(is.null(constraints) || sum(names(constraints) %in% c('minx','maxx','miny','maxy')) != 4){
		print('Making constraints')
    	#First, compute the constraints
	    real.minimum <- min(FS)
    	real.maximum <- max(FS)
	    real.value <- max(c(abs(real.minimum),abs(real.maximum)))
   		#set up a constraints list
		constraints <- list(minx=-real.value, maxx=real.value,miny=-real.value,maxy=real.value)
	}

	if(is.null(Ctr)){
		F2 <- FS**2
		SF2 <- apply(F2,2,sum)
		Ctr <- t( t(F2) / SF2)
	}
  # Now Compute the contributions for the plane of the dimensions to be plotted
  #Ctr4Plot =  apply(Ctr[,c(axis1,axis2)],1,sum)

  plot.out <- prettyGraphs::prettyPlot(FS,constraints=constraints,col=item.colors,main=ZeTitle,x_axis=axis1,y_axis=axis2,contributionCircles=TRUE,contributions=Ctr,display_names=!nude)

  return(list(constraints=constraints,item.colors=item.colors))
}

#' Plot maps of the factor scores and partial factor scores of the observations
#' for a \acronym{DISTATIS} analysis.
#'
#' \code{GraphDistatisPartial} plots maps of the factor scores of the
#' observations from a \code{\link{distatis}} analysis.
#' \code{GraphDistatisPartial} gives a map of the factors scores of the
#' observations plus partial factor scores, 
#' as "seen" by each of the matrices.
#'
#' Note that, in the current version, the graphs are plotted as R-plots and are
#' \emph{not} passed back by the routine.  So the graphs need to be saved "by
#' hand" from the R graphic windows.  We plan to improve this in a future
#' version.
#'
#' @param FS The factor scores of the observations
#' (\code{$res4Splus$F} from the output of
#' \code{distatis}).
#' @param PartialFS The partial factor scores of the observations
#' (\code{$res4Splus$PartialF} from \code{distatis})
#' @param axis1 The dimension for the horizontal axis of the plots.
#' @param axis2 The dimension for the vertical axis of the plots.
#' @param constraints constraints for the axes
#' @param item.colors A \eqn{I\times 1}{I*1} matrix (with \eqn{I} = #
#' observations) of color names for the observations.
#' If \code{NULL} (default),
#' \code{prettyGraphs} chooses.
#' @param participant.colors A \eqn{I\times 1}{I*1} matrix (with \eqn{I} = #
#' participants) of color names for the observations.
#' If \code{NULL} (default),
#' \code{prettyGraphs} chooses (with function \code{prettyGraphs::}).
#' @param ZeTitle General title for the plots.
#' @param Ctr Contributions of each observation. If NULL (default), these are
#' computed from FS
#' @param color.by.observations if \code{TRUE} (default), the partial factor
#' scores are colored by \code{item.colors}. When \code{FALSE},
#' \code{participant.colors} are used.
#' @param nude When \code{nude} is \code{TRUE} the labels for the observations
#' are not plotted (useful when editing the graphs for publication).
#' @param lines If \code{TRUE} (default) then lines are drawn between the
#' partial factor score of an observation and the compromise factor score of
#' the observation.
#' @return \item{constraints}{A set of plot constraints that are returned.}
#' \item{item.colors}{A set of colors for the observations are returned.}
#' \item{participant.colors}{A set of colors for the participants are
#' returned.}
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
#'  GraphDistatisPartial(DistatisAlgo$res4Splus$F,DistatisAlgo$res4Splus$PartialF)
#' @importFrom prettyGraphs createColorVectorsByDesign prettyPlot
#' @importFrom graphics points
#' @export GraphDistatisPartial
GraphDistatisPartial <-
function(FS, PartialFS,
         axis1 = 1, axis2 = 2,
         constraints = NULL,
         item.colors = NULL,
         participant.colors = NULL,
         ZeTitle = 'Distatis-Partial',
         Ctr = NULL,
         color.by.observations = TRUE,
         nude = FALSE,
         lines = TRUE){
	if(is.null(participant.colors)){
	   	part.design <- diag(dim(PartialFS)[3])
		participant.colors <- as.matrix(
		        prettyGraphs::createColorVectorsByDesign(part.design)$oc)
	}
	if(is.null(item.colors)){
		item.design <- diag(dim(FS)[1])
		item.colors <- as.matrix(
		  prettyGraphs::createColorVectorsByDesign(item.design)$oc)
	}
	if(is.null(Ctr)){
		F2 <- FS**2
		SF2 <- apply(F2,2,sum)
		Ctr <- t( t(F2) / SF2)
	}
	if(is.null(constraints)){
	    real.minimum <- min(c(FS,PartialFS))
    	real.maximum <- max(c(FS,PartialFS))
	    real.value <- max(c(abs(real.minimum),abs(real.maximum)))
   		#set up a constraints list
		constraints <- list(minx = -real.value, maxx = real.value,
		                     miny = -real.value,maxy = real.value)
	}

	if(!color.by.observations){
		title.pass <- paste(ZeTitle,"Colored By Participants", sep = " ")
		Compromise.Out <- GraphDistatisCompromise(FS,
		                         axis1 = axis1, axis2 = axis2,
		                         constraints = constraints,
		                         item.colors = item.colors,
		                         ZeTitle = title.pass,
		                         nude = nude,Ctr=Ctr)
		if(lines){
			for(i in 1:dim(PartialFS)[1]){
				to.plot <- t(PartialFS[i,,])
				center.point <- FS[i,c(axis1,axis2)]
				center.rep <- matrix(center.point,
				                         dim(PartialFS)[3],2,byrow=TRUE)
				bound.mat <- rbind(center.rep,to.plot[,c(axis1,axis2)])
				bound.mat <- bound.mat[
				       as.vector(t(matrix(seq(1,nrow(bound.mat)),ncol=2))), ]
				graphics::points(bound.mat,type="l", lty = 2, lwd = 2,
				                 col="grey80")
			}
		}
		for(i in 1:dim(PartialFS)[3]){
			to.plot <- PartialFS[,,i]
			rownames(to.plot) <- rep(
			           unlist(dimnames(PartialFS)[3])[i],dim(PartialFS)[1])
			prettyGraphs::prettyPlot(to.plot,col=participant.colors[i,],
			         dev.new = FALSE, axes = FALSE, new.plot = FALSE,
			         x_axis = axis1, y_axis = axis2, display_names = !nude)
		}
	}else{
		title.pass <- paste(ZeTitle,"Colored By Observations", sep = " ")
		Compromise.Out <- GraphDistatisCompromise(FS,
		                      axis1 = axis1,axis2=axis2,
		                      constraints = constraints,
		                      item.colors = item.colors,
		                      ZeTitle = title.pass,
		                      nude = nude, Ctr = Ctr)
		if(lines){
			for(i in 1:dim(PartialFS)[1]){
				to.plot <- t(PartialFS[i,,])
				center.point <- FS[i,c(axis1,axis2)]
				center.rep <- matrix(center.point,
				                     dim(PartialFS)[3],2,byrow = TRUE)
				bound.mat <- rbind(center.rep,to.plot[,c(axis1,axis2)])
				bound.mat <- bound.mat[ as.vector(
				                t(matrix(seq(1,nrow(bound.mat)),ncol=2))), ]
			  	graphics::points(bound.mat,type="l",lty=2,lwd=2,col="grey80")
			}
		}
		for(i in 1:dim(PartialFS)[1]){
			to.plot <- t(PartialFS[i,,])
			prettyGraphs::prettyPlot(to.plot,col=item.colors[i,],
			           dev.new=FALSE,axes=FALSE,new.plot=FALSE,
			           x_axis=axis1,y_axis=axis2,
			           display_names=!nude)
		}
	}

	retour <- Compromise.Out
	retour$participant.colors = participant.colors
	return(retour)
}

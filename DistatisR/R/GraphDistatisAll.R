#' This function combines the functionality of
#' \code{\link{GraphDistatisCompromise}}, \code{\link{GraphDistatisPartial}},
#' \code{\link{GraphDistatisBoot}}, and \code{\link{GraphDistatisRv}}.
#'
#' This function produces 4 plots: (1) a compromise plot, (2) a partial factor
#' scores plot, (3) a bootstrap confidence intervals plot, and (4) an \eqn{Rv} map.
#'
#'
#' @param FS The factor scores of the observations
#'  (\code{$res4Splus$F}from
#' \code{distatis})
#' @param PartialFS The partial factor scores of the observations
#' (\code{$res4Splus$PartialF} from \code{distatis})
#' @param FBoot is the bootstrapped factor scores array 
#' (\code{FBoot} obtained
#' from \code{\link{BootFactorScores}} or 
#' \code{\link{BootFromCompromise}})
#' @param RvFS The factor scores
#'  of the distance matrices (\code{$res4Cmat$G}
#' from \code{distatis})
#' @param axis1 The dimension for the horizontal axis of the plots.
#' @param axis2 The dimension for the vertical axis of the plots.
#' @param constraints constraints for the axes
#' @param item.colors A \eqn{I\times 1}{I*1} matrix (with \eqn{I} = #
#' observations) of color names for the observations. 
#' If \code{NULL} (default),
#' \code{prettyGraphs} chooses.
#' @param participant.colors A \eqn{I\times 1}{I*1} matrix (with \eqn{I} = #
#' participants) of color names for the observations. If NULL (default),
#' \code{prettyGraphs} chooses.
#' @param ZeTitleBase General title for the plots.
#' @param nude When \code{nude} is \code{TRUE} the labels for the observations
#' are not plotted (useful when editing the graphs for publication).
#' @param Ctr Contributions of each observation. If NULL (default), these are
#' computed from \code{FS}
#' @param RvCtr Contributions of each participant. If NULL (default), these are
#' computed from RvFS
#' @param color.by.observations if \code{TRUE} (default), the partial factor
#' scores are colored by \code{item.colors}. When \code{FALSE},
#' \code{participant.colors} are used.
#' @param lines If \code{TRUE} (default) then lines are drawn between the
#' partial factor score of an observation and the compromise factor score of
#' the observation.
#' @param lwd Thickness of the line plotting the ellipse or hull.
#' @param ellipses a boolean. When \code{TRUE} will plot ellipses (from
#' \code{car} package). When \code{FALSE} (default) 
#' will plot peeled hulls (from
#' \code{prettyGraphs} package).
#' @param fill when \code{TRUE}, fill in the ellipse with color. 
#' Relevant for
#' ellipses only.
#' @param fill.alpha transparency index when filling in the ellipses.  
#' Relevant
#' to ellipses only.
#' @param percentage A value to determine the percent coverage of the bootstrap
#' partial factor scores to provide ellipse or hull confidence intervals.
#' @return \item{constraints}{A set of plot constraints that are returned.}
#' \item{item.colors}{A set of colors for the observations are returned.}
#' \item{participant.colors}{A set of colors for the participants are
#' returned.}
#' @author Derek Beaton and Herve Abdi
#' @seealso \code{\link{GraphDistatisAll}}
#' \code{\link{GraphDistatisCompromise}} \code{\link{GraphDistatisPartial}}
#' \code{\link{GraphDistatisBoot}} \code{\link{GraphDistatisRv}}
#' \code{\link{distatis}}
#' @keywords distatis mds
#' @importFrom prettyGraphs createColorVectorsByDesign
#' @examples
#'
#' # 1. Load the Sort data set from the SortingBeer example  (available from the DistatisR package)
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
#' # 5. Create the Graphics with GraphDistatisAll
#' #
#' GraphDistatisAll(testDistatis$res4Splus$F,testDistatis$res4Splus$PartialF,
#' 	BootF,testDistatis$res4Cmat$G)
#'
#' @export
GraphDistatisAll <-
function(FS,PartialFS,FBoot,
         RvFS,
         axis1 = 1, axis2 = 2,
         constraints=NULL,
         item.colors=NULL,
         participant.colors=NULL,
         ZeTitleBase=NULL,
         nude=FALSE, Ctr=NULL,
         RvCtr=NULL,
         color.by.observations=TRUE,
         lines=TRUE,lwd=3.5,
         ellipses=TRUE,fill=TRUE,
         fill.alpha = .27,
         percentage=0.95){

	if(is.null(participant.colors)){
	   	part.design <- diag(dim(PartialFS)[3])
		participant.colors <- as.matrix(prettyGraphs::createColorVectorsByDesign(part.design)$oc)
	}
	if(is.null(item.colors)){
		item.design <- diag(dim(FS)[1])
		item.colors <- as.matrix(prettyGraphs::createColorVectorsByDesign(item.design)$oc)
	}
	if(is.null(ZeTitleBase)){
		ZeTitleBase <- 'Distatis'
	}
	if(is.null(Ctr)){
		F2 <- FS**2
		SF2 <- apply(F2,2,sum)
		Ctr <- t( t(F2) / SF2)
	}
	if(is.null(RvCtr)){
		RvF2 <- RvFS**2
		RvSF2 <- apply(RvF2,2,sum)
		RvCtr <- t( t(RvF2) / RvSF2)
	}
	if(is.null(constraints)){
    	#First, compute the constraints
	    real.minimum <- min(c(PartialFS,FS,FBoot))
    	real.maximum <- max(c(PartialFS,FS,FBoot))
	    real.value <- max(c(abs(real.minimum),abs(real.maximum)))
   		#set up a constraints list
		constraints <- list(minx=-real.value, maxx=real.value,miny=-real.value,maxy=real.value)
   }

	compromise.out <- GraphDistatisCompromise(FS=FS,axis1=axis1,axis2=axis2,constraints=constraints,item.colors=item.colors,ZeTitle=paste(ZeTitleBase,"Compromise",sep=" "),nude=nude,Ctr=Ctr)

	partial.out <- GraphDistatisPartial(FS=FS,PartialFS=PartialFS,axis1=axis1,axis2=axis2,constraints=constraints,item.colors=item.colors,participant.colors=participant.colors,ZeTitle=paste(ZeTitleBase,"Partial",sep=" "), color.by.observations=color.by.observations,lines=lines)

	boot.out <- GraphDistatisBoot(FS=FS,FBoot=FBoot,axis1=axis1, axis2=axis2, item.colors=item.colors, ZeTitle=paste(ZeTitleBase,"Bootstrap",sep=" "), constraints=constraints, nude = nude, Ctr=Ctr, lwd = lwd, ellipses=ellipses, fill = fill, fill.alpha = fill.alpha, percentage=percentage)

	rv.map.out <- GraphDistatisRv(RvFS,axis1=axis1,axis2=axis2,ZeTitle=paste(ZeTitleBase,"Rv Map",sep=" "), participant.colors = participant.colors, nude=nude,RvCtr=RvCtr)

	return(list(constraints=constraints,item.colors=item.colors,participant.colors=participant.colors))

}
